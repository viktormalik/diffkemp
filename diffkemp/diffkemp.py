from diffkemp.building.cc_wrapper import get_cc_wrapper_path, wrapper_env_vars
from diffkemp.cli import make_argument_parser
from diffkemp.config import Config
from diffkemp.snapshot import Snapshot
from diffkemp.llvm_ir.kernel_source_tree import KernelSourceTree
from diffkemp.llvm_ir.kernel_llvm_source_builder import KernelLlvmSourceBuilder
from diffkemp.llvm_ir.source_tree import SourceTree, SourceNotFoundException
from diffkemp.llvm_ir.llvm_module import LlvmParam, LlvmModule
from diffkemp.llvm_ir.single_llvm_finder import SingleLlvmFinder
from diffkemp.llvm_ir.wrapper_build_finder import WrapperBuildFinder
from diffkemp.simpll.library import run_generate_pattern
from diffkemp.semdiff.caching import SimpLLCache
from diffkemp.semdiff.pattern_config import PatternConfig
from diffkemp.semdiff.function_diff import functions_diff
from diffkemp.semdiff.result import Result
from subprocess import check_call, CalledProcessError
from diffkemp.utils import get_llvm_version
from tempfile import mkdtemp
from timeit import default_timer
import errno
import os
import re
import sys


def run_from_cli():
    """Main method to run the tool."""
    ap = make_argument_parser()
    args = ap.parse_args()
    args.func(args)


def generate(args):
    if args.pattern_config:
        run_generate_pattern(args.pattern_config)


def build(args):
    # Generate wrapper for C/C++ compiler
    cc_wrapper = get_cc_wrapper_path(args.no_native_cc_wrapper)

    # Create temp directory and environment
    tmpdir = mkdtemp()
    db_filename = os.path.join(tmpdir, "diffkemp-wdb")
    environment = {
        wrapper_env_vars["db_filename"]: db_filename,
        wrapper_env_vars["clang"]: args.clang,
        wrapper_env_vars["clang_append"]: ",".join(args.clang_append)
        if args.clang_append is not None
        else "",
        wrapper_env_vars["clang_drop"]: ",".join(args.clang_drop)
        if args.clang_drop is not None
        else "",
        wrapper_env_vars["llvm_link"]: args.llvm_link,
    }
    environment.update(os.environ)

    # Determine make args
    make_cc_setting = 'CC="{}"'.format(cc_wrapper)
    make_args = [args.build_program, "-C", args.source_dir, make_cc_setting]
    if args.build_file is not None:
        make_args.extend(["-c", args.build_file])
    make_target_args = make_args[:]
    if args.target is not None:
        make_target_args.extend(args.target)

    # Clean the project
    config_log_filename = os.path.join(args.source_dir, "config.log")
    if os.path.exists(config_log_filename):
        # Backup config.log
        os.rename(config_log_filename, config_log_filename + ".bak")
    try:
        check_call(make_args + ["clean"], env=environment)
    except CalledProcessError:
        pass
    if os.path.exists(config_log_filename + ".bak"):
        # Restore config.log
        os.rename(config_log_filename + ".bak", config_log_filename)

    # If config.log is present, reconfigure using wrapper
    # Note: this is done to support building with nested configure
    if args.reconfigure and os.path.exists(config_log_filename):
        with open(config_log_filename, "r") as config_log:
            # Try to get line with configure command from config.log
            # This line is identified by being the first line containing $
            configure_cmd = None
            for line in config_log.readlines():
                if "$" in line:
                    configure_cmd = line.strip()
                    break
            if configure_cmd and make_cc_setting not in configure_cmd:
                # Remove beginning of line containing spaces and $
                configure_cmd = configure_cmd.split("$ ", 1)[1]
                configure_cmd += " " + make_cc_setting
                # Delete all config.cache files
                for root, dirs, _ in os.walk(args.source_dir):
                    for dirname in dirs:
                        try:
                            os.remove(os.path.join(root, dirname,
                                                   "config.cache"))
                        except (FileNotFoundError, PermissionError):
                            pass
                # Reconfigure with CC wrapper
                check_call(configure_cmd, cwd=args.source_dir,
                           env=environment, shell=True)

    # Build the project using generated wrapper
    check_call(make_target_args, env=environment)

    # Create a new snapshot from the source directory.
    source_finder = WrapperBuildFinder(args.source_dir, db_filename)
    source = SourceTree(args.source_dir, source_finder)
    snapshot = Snapshot.create_from_source(source, args.output_dir,
                                           "function")

    # Build sources for symbols from the list into LLVM IR
    if args.symbol_list is None:
        args.symbol_list = os.path.join(args.source_dir, "function_list")
    symbol_list = read_symbol_list(args.symbol_list)
    generate_from_function_list(snapshot, symbol_list)

    # Create the snapshot directory containing the YAML description file
    snapshot.generate_snapshot_dir()
    snapshot.finalize()


def build_kernel(args):
    """
    Create snapshot of a Linux kernel source tree. Kernel sources are
    compiled into LLVM IR on-the-fly as necessary.
    Supports two kinds of symbol lists to generate the snapshot from:
      - list of functions (default)
      - list of sysctl options
    """
    # Create a new snapshot from the kernel source directory.
    source_finder = KernelLlvmSourceBuilder(args.source_dir)
    source = KernelSourceTree(args.source_dir, source_finder)
    list_kind = "sysctl" if args.sysctl else "function"
    snapshot = Snapshot.create_from_source(source,
                                           args.output_dir,
                                           list_kind,
                                           not args.no_source_dir)

    # Read the symbol list
    symbol_list = read_symbol_list(args.symbol_list)
    if not symbol_list:
        sys.stderr.write("ERROR: symbol list is empty or could not be read\n")
        return

    # Generate snapshot contents
    if args.sysctl:
        generate_from_sysctl_list(snapshot, symbol_list)
    else:
        generate_from_function_list(snapshot, symbol_list)

    # Create the snapshot directory containing the YAML description file
    snapshot.generate_snapshot_dir()
    snapshot.finalize()


def llvm_to_snapshot(args):
    """
    Create snapshot from a project pre-compiled into a single LLVM IR file.
    :param args: CLI arguments of the "diffkemp llvm-to-snapshot" command.
    """
    source_finder = SingleLlvmFinder(args.source_dir, args.llvm_file)
    source = SourceTree(args.source_dir, source_finder)
    snapshot = Snapshot.create_from_source(source, args.output_dir, "function")

    function_list = read_symbol_list(args.function_list)
    if not function_list:
        sys.stderr.write("ERROR: symbol list is empty or could not be read\n")
        return

    generate_from_function_list(snapshot, function_list)
    snapshot.generate_snapshot_dir()
    snapshot.finalize()


def read_symbol_list(list_path):
    """
    Read and parse the symbol list file. Filters out entries which are not
    valid symbols (do not start with letter or underscore).
    :param list_path: Path to the list.
    :return: List of symbols (strings).
    """
    with open(list_path, "r") as list_file:
        return [symbol for line in list_file.readlines() if
                (symbol := line.strip()) and
                (symbol[0].isalpha() or symbol[0] == "_")]


def generate_from_function_list(snapshot, fun_list):
    """
    Generate a snapshot from a list of functions.
    For each function, find the source with its definition, compile it into
    LLVM IR, and add the appropriate entry to the snapshot.
    :param snapshot: Existing Snapshot object to fill.
    :param fun_list: List of functions to add. If non-function symbols are
                     present, these are added into the snapshot with empty
                     module entry.
    """
    for symbol in fun_list:
        try:
            sys.stdout.write("{}: ".format(symbol))
            sys.stdout.flush()

            # Find the source for function definition and add it to
            # the snapshot
            llvm_mod = snapshot.source_tree.get_module_for_symbol(symbol)
            if llvm_mod.has_function(symbol):
                snapshot.add_fun(symbol, llvm_mod)
                print(os.path.relpath(llvm_mod.llvm,
                                      snapshot.source_tree.source_dir))
            else:
                snapshot.add_fun(symbol, None)
                print("not a function")
        except SourceNotFoundException:
            print("source not found")
            snapshot.add_fun(symbol, None)


def generate_from_sysctl_list(snapshot, sysctl_list):
    """
    Generate a snapshot from a list of sysctl options.
    For each sysctl option:
      - get LLVM IR of the file which defines the sysctl option
      - find and compile the proc handler function and add it to the snapshot
      - find the sysctl data variable
      - find, compile, and add to the snapshot all functions that use
        the data variable
    :param snapshot: Existing Snapshot object to fill
    :param sysctl_list: List of sysctl options.
                        May contain patterns such as "kernel.*".
    """
    for symbol in sysctl_list:
        # Get module with sysctl definitions
        try:
            sysctl_mod = snapshot.source_tree.get_sysctl_module(symbol)
        except SourceNotFoundException:
            print("{}: sysctl not supported".format(symbol))
            continue

        # Iterate all sysctls represented by the symbol (it can be a pattern)
        sysctl_list = sysctl_mod.parse_sysctls(symbol)
        if not sysctl_list:
            print("{}: no sysctl found".format(symbol))
            continue
        for sysctl in sysctl_list:
            print("{}:".format(sysctl))

            # Proc handler function for sysctl
            proc_fun = sysctl_mod.get_proc_fun(sysctl)
            if proc_fun:
                try:
                    proc_fun_mod = snapshot.source_tree.get_module_for_symbol(
                        proc_fun)
                    snapshot.add_fun(name=proc_fun,
                                     llvm_mod=proc_fun_mod,
                                     glob_var=None,
                                     tag="proc handler function",
                                     group=sysctl)
                    print("  {}: {} (proc handler)".format(
                        proc_fun,
                        os.path.relpath(proc_fun_mod.llvm,
                                        snapshot.source_tree.source_dir)))
                except SourceNotFoundException:
                    print("  could not build proc handler")

            # Functions using the sysctl data variable
            data = sysctl_mod.get_data(sysctl)
            if not data:
                continue
            for data_mod in \
                    snapshot.source_tree.get_modules_using_symbol(data.name):
                for data_fun in data_mod.get_functions_using_param(data):
                    if data_fun == proc_fun:
                        continue
                    snapshot.add_fun(
                        name=data_fun,
                        llvm_mod=data_mod,
                        glob_var=data.name,
                        tag="using data variable \"{}\"".format(data.name),
                        group=sysctl)
                    print("  {}: {} (using data variable \"{}\")".format(
                        data_fun,
                        os.path.relpath(data_mod.llvm,
                                        snapshot.source_tree.source_dir),
                        data.name))


def _get_modules_to_cache(functions, group_name, other_snapshot,
                          min_frequency):
    """
    Generates a list of frequently used modules. These will be loaded into
    cache if DiffKemp is running with module caching enable.
    :param functions: List of pairs of functions to be compared along
    with their description objects
    :param group_name: Name of the group the functions are in
    :param other_snapshot: Snapshot object for looking up the functions
    in the other snapshot
    :param min_frequency: Minimal frequency for a module to be included into
    the cache
    :return: Set of modules that should be loaded into module cache
    """
    module_frequency_map = dict()
    for fun, old_fun_desc in functions:
        # Check if the function exists in the other snapshot
        new_fun_desc = other_snapshot.get_by_name(fun, group_name)
        if not new_fun_desc:
            continue
        for fun_desc in [old_fun_desc, new_fun_desc]:
            if not fun_desc.mod:
                continue
            if fun_desc.mod.llvm not in module_frequency_map:
                module_frequency_map[fun_desc.mod.llvm] = 0
            module_frequency_map[fun_desc.mod.llvm] += 1
    return {mod for mod, frequency in module_frequency_map.items()
            if frequency >= min_frequency}


def compare(args):
    """
    Compare the generated snapshots. Runs the semantic comparison and shows
    information about the compared functions that are semantically different.
    """
    # Parse both the new and the old snapshot.
    old_snapshot = Snapshot.load_from_dir(args.snapshot_dir_old)
    new_snapshot = Snapshot.load_from_dir(args.snapshot_dir_new)

    # Check if snapshot LLVM versions are compatible with the current version
    llvm_version = get_llvm_version()

    if llvm_version != old_snapshot.llvm_version:
        sys.stderr.write(
            "Error: old snapshot was built with LLVM {}, "
            "current version is LLVM {}.\n".format(
                old_snapshot.llvm_version, llvm_version))
        sys.exit(errno.EINVAL)

    if llvm_version != new_snapshot.llvm_version:
        sys.stderr.write(
            "Error: new snapshot was built with LLVM {}, "
            "current version is LLVM {}.\n".format(
                new_snapshot.llvm_version, llvm_version))
        sys.exit(errno.EINVAL)

    # Set the output directory
    if not args.stdout:
        if args.output_dir:
            output_dir = args.output_dir
            if os.path.isdir(output_dir):
                sys.stderr.write("Error: output directory exists\n")
                sys.exit(errno.EEXIST)
        else:
            output_dir = default_output_dir(args.snapshot_dir_old,
                                            args.snapshot_dir_new)
    else:
        output_dir = None

    if args.function:
        old_snapshot.filter([args.function])
        new_snapshot.filter([args.function])

    # Transform difference pattern files into an LLVM IR based configuration.
    if args.patterns:
        pattern_config = PatternConfig.create_from_file(args.patterns)
    else:
        pattern_config = None

    config = Config(old_snapshot, new_snapshot, args.show_diff,
                    args.output_llvm_ir, pattern_config,
                    args.control_flow_only, args.print_asm_diffs,
                    args.verbose, not args.disable_simpll_ffi,
                    args.semdiff_tool)
    result = Result(Result.Kind.NONE, args.snapshot_dir_old,
                    args.snapshot_dir_old, start_time=default_timer())

    for group_name, group in sorted(old_snapshot.fun_groups.items()):
        group_printed = False

        # Set the group directory
        if output_dir is not None and group_name is not None:
            group_dir = os.path.join(output_dir, group_name)
        else:
            group_dir = None

        result_graph = None
        cache = SimpLLCache(mkdtemp())
        module_cache = {}

        if args.enable_module_cache:
            modules_to_cache = _get_modules_to_cache(group.functions.items(),
                                                     group_name,
                                                     new_snapshot,
                                                     2)
        else:
            modules_to_cache = set()

        for fun, old_fun_desc in sorted(group.functions.items()):
            # Check if the function exists in the other snapshot
            new_fun_desc = new_snapshot.get_by_name(fun, group_name)
            if not new_fun_desc:
                continue

            # Check if the module exists in both snapshots
            if old_fun_desc.mod is None or new_fun_desc.mod is None:
                result.add_inner(Result(Result.Kind.UNKNOWN, fun, fun))
                if group_name is not None and not group_printed:
                    print("{}:".format(group_name))
                    group_printed = True
                print("{}: unknown".format(fun))
                continue

            # If function has a global variable, set it
            glob_var = LlvmParam(old_fun_desc.glob_var) \
                if old_fun_desc.glob_var else None

            # Run the semantic diff
            fun_result = functions_diff(
                mod_first=old_fun_desc.mod, mod_second=new_fun_desc.mod,
                fun_first=fun, fun_second=fun,
                glob_var=glob_var, config=config,
                prev_result_graph=result_graph, function_cache=cache,
                module_cache=module_cache, modules_to_cache=modules_to_cache)
            result_graph = fun_result.graph

            if fun_result is not None:
                if args.regex_filter is not None:
                    # Filter results by regex
                    pattern = re.compile(args.regex_filter)
                    for called_res in fun_result.inner.values():
                        if pattern.search(called_res.diff):
                            break
                    else:
                        fun_result.kind = Result.Kind.EQUAL

                result.add_inner(fun_result)

                # Printing information about failures and non-equal functions.
                if fun_result.kind in [Result.Kind.NOT_EQUAL,
                                       Result.Kind.ERROR, Result.Kind.UNKNOWN]:
                    if fun_result.kind == Result.Kind.NOT_EQUAL:
                        # Create the output directory if needed
                        if output_dir is not None:
                            if not os.path.isdir(output_dir):
                                os.mkdir(output_dir)
                        # Create the group directory or print the group name
                        # if needed
                        if group_dir is not None:
                            if not os.path.isdir(group_dir):
                                os.mkdir(group_dir)
                        elif group_name is not None and not group_printed:
                            print("{}:".format(group_name))
                            group_printed = True
                        print_syntax_diff(
                            snapshot_dir_old=args.snapshot_dir_old,
                            snapshot_dir_new=args.snapshot_dir_new,
                            fun=fun,
                            fun_result=fun_result,
                            fun_tag=old_fun_desc.tag,
                            output_dir=group_dir if group_dir else output_dir,
                            show_diff=args.show_diff,
                            initial_indent=2 if (group_name is not None and
                                                 group_dir is None) else 0)
                    else:
                        # Print the group name if needed
                        if group_name is not None and not group_printed:
                            print("{}:".format(group_name))
                            group_printed = True
                        print("{}: {}".format(fun, str(fun_result.kind)))

            # Clean LLVM modules (allow GC to collect the occupied memory)
            old_fun_desc.mod.clean_module()
            new_fun_desc.mod.clean_module()
            LlvmModule.clean_all()

    old_snapshot.finalize()
    new_snapshot.finalize()

    if output_dir is not None and os.path.isdir(output_dir):
        print("Differences stored in {}/".format(output_dir))

    if args.report_stat:
        print("")
        print("Statistics")
        print("----------")
        result.stop_time = default_timer()
        result.report_stat(args.show_errors)
    return 0


def default_output_dir(src_snapshot, dest_snapshot):
    """Name of the directory to put log files into."""
    base_dirname = "diff-{}-{}".format(
        os.path.basename(os.path.normpath(src_snapshot)),
        os.path.basename(os.path.normpath(dest_snapshot)))
    if os.path.isdir(base_dirname):
        suffix = 0
        dirname = base_dirname
        while os.path.isdir(dirname):
            dirname = "{}-{}".format(base_dirname, suffix)
            suffix += 1
        return dirname
    return base_dirname


def print_syntax_diff(snapshot_dir_old, snapshot_dir_new, fun, fun_result,
                      fun_tag, output_dir, show_diff, initial_indent):
    """
    Log syntax diff of 2 functions. If log_files is set, the output is printed
    into a separate file, otherwise it goes to stdout.
    :param snapshot_dir_old: Old snapshot directory.
    :param snapshot_dir_new: New snapshot directory.
    :param fun: Name of the analysed function
    :param fun_tag: Analysed function tag
    :param fun_result: Result of the analysis
    :param output_dir: True if the output is to be written into a file
    :param show_diff: Print syntax diffs.
    :param initial_indent: Initial indentation of printed messages
    """
    def text_indent(text, width):
        """Indent each line in the text by a number of spaces given by width"""
        return ''.join(" " * width + line for line in text.splitlines(True))

    if fun_result.kind == Result.Kind.NOT_EQUAL:
        if output_dir:
            output = open(os.path.join(output_dir, "{}.diff".format(fun)), "w")
            output.write(
                "Found differences in functions called by {}".format(fun))
            if fun_tag is not None:
                output.write(" ({})".format(fun_tag))
            output.write("\n\n")
            indent = initial_indent + 2
        else:
            output = sys.stdout
            if fun_tag is not None:
                output.write(text_indent("{} ({}):\n".format(fun, fun_tag),
                                         initial_indent))
            else:
                output.write(text_indent("{}:\n".format(fun), initial_indent))
            indent = initial_indent + 4

        for called_res in sorted(fun_result.inner.values(),
                                 key=lambda r: r.first.name):
            if called_res.diff == "" and called_res.first.covered:
                # Do not print empty diffs
                continue

            output.write(
                text_indent("{} differs:\n".format(called_res.first.name),
                            indent - 2))
            if not output_dir:
                output.write(text_indent("{{{\n", indent - 2))

            if called_res.first.callstack:
                output.write(
                    text_indent("Callstack ({}):\n".format(snapshot_dir_old),
                                indent))
                output.write(text_indent(
                    called_res.first.callstack.replace(
                        os.path.join(os.path.abspath(snapshot_dir_old), ""),
                        ""), indent))
                output.write("\n\n")
            if called_res.second.callstack:
                output.write(
                    text_indent("Callstack ({}):\n".format(snapshot_dir_new),
                                indent))
                output.write(text_indent(
                    called_res.second.callstack.replace(
                        os.path.join(os.path.abspath(snapshot_dir_new), ""),
                        ""),
                    indent))
                output.write("\n\n")

            if show_diff:
                if (called_res.diff.strip() == "" and
                        called_res.macro_diff is not None):
                    output.write(text_indent(
                        "\n".join(map(str, called_res.macro_diff)), indent))
                else:
                    output.write(text_indent("Diff:\n", indent))
                    output.write(text_indent(
                        called_res.diff, indent))

            if not output_dir:
                output.write(text_indent("}}}\n", indent - 2))
            output.write("\n")
