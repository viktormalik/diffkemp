from diffkemp.building.cc_wrapper import get_cc_wrapper_path, wrapper_env_vars
from diffkemp.config import Config
from diffkemp.snapshot import Snapshot
from diffkemp.llvm_ir.optimiser import opt_llvm, BuildException
from diffkemp.llvm_ir.kernel_source_tree import KernelSourceTree
from diffkemp.llvm_ir.kernel_llvm_source_builder import KernelLlvmSourceBuilder
from diffkemp.llvm_ir.source_tree import SourceTree, SourceNotFoundException
from diffkemp.llvm_ir.llvm_module import LlvmParam, LlvmModule
from diffkemp.llvm_ir.single_llvm_finder import SingleLlvmFinder
from diffkemp.llvm_ir.wrapper_build_finder import WrapperBuildFinder
from diffkemp.llvm_ir.single_c_builder import SingleCBuilder
from diffkemp.semdiff.caching import SimpLLCache
from diffkemp.semdiff.function_diff import functions_diff
from diffkemp.semdiff.result import Result
from diffkemp.output import YamlOutput
from diffkemp.syndiff.function_syntax_diff import unified_syntax_diff
from http.server import HTTPServer, SimpleHTTPRequestHandler
from subprocess import check_call, CalledProcessError
from tempfile import mkdtemp
from timeit import default_timer
import errno
import os
import re
import sys
import shutil
import yaml

VIEW_INSTALL_DIR = "/var/lib/diffkemp/view"


def build(args):
    # build snapshot from make-based c project
    if os.path.isdir(args.source_dir):
        build_c_project(args)
    # make snapshot from single c file
    elif os.path.isfile(args.source_dir) and args.source_dir.endswith(".c"):
        build_c_file(args)
    else:
        sys.stderr.write(
            "Error: the specified source_dir is not a directory nor a C file\n"
        )
        sys.exit(errno.EINVAL)


def build_c_project(args):
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
        wrapper_env_vars["no_opt_override"]: ("1" if args.no_opt_override
                                              else "0")
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

    # Run LLVM IR simplification passes if the user did not specified
    # to not override project optimization.
    if not args.no_opt_override:
        # Run llvm passes on created LLVM IR files.
        with open(db_filename, "r") as db_file:
            for line in [r for r in db_file if r.startswith("o:")]:
                llvm_file = line.split(":")[1].rstrip()
                try:
                    opt_llvm(llvm_file)
                except BuildException:
                    # Unsuccessful optimization, leaving as it is.
                    pass

    # Create a new snapshot from the source directory.
    source_finder = WrapperBuildFinder(args.source_dir, db_filename)
    source = SourceTree(args.source_dir, source_finder)
    snapshot = Snapshot.create_from_source(source, args.output_dir,
                                           "function")

    # Copy the database file into the snapshot directory
    shutil.copyfile(db_filename, os.path.join(args.output_dir, "diffkemp-wdb"))

    # Build sources for symbols from the list into LLVM IR
    if args.symbol_list is None:
        args.symbol_list = os.path.join(args.source_dir, "function_list")
    symbol_list = read_symbol_list(args.symbol_list)
    generate_from_function_list(snapshot, symbol_list)

    # Create the snapshot directory containing the YAML description file
    snapshot.generate_snapshot_dir()
    snapshot.finalize()


def build_c_file(args):
    # It ignores following args: build-program, build-file, clang-drop,
    #   llvm-link, llvm-dis, target, reconfigure, no-native-cc-wrapper.
    source_file_name = os.path.basename(args.source_dir)
    source_dir = os.path.dirname(args.source_dir)

    clang_append = args.clang_append if args.clang_append is not None else []
    # Create a new snapshot and generate its content.
    source_finder = SingleCBuilder(source_dir, source_file_name,
                                   clang=args.clang,
                                   clang_append=clang_append,
                                   default_optim=not args.no_opt_override)
    source = SourceTree(source_dir, source_finder)
    snapshot = Snapshot.create_from_source(source, args.output_dir, "function")
    if args.symbol_list is None:
        function_list = source_finder.get_function_list()
    else:
        function_list = read_symbol_list(args.symbol_list)
    generate_from_function_list(snapshot, function_list)

    # Create the snapshot directory containing the YAML description file.
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

    config = Config.from_args(args)
    result = Result(Result.Kind.NONE, args.snapshot_dir_old,
                    args.snapshot_dir_old, start_time=default_timer())

    for group_name, group in sorted(config.snapshot_first.fun_groups.items()):
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
                                                     config.snapshot_second,
                                                     2)
        else:
            modules_to_cache = set()

        for fun, old_fun_desc in sorted(group.functions.items()):
            # Check if the function exists in the other snapshot
            new_fun_desc = config.snapshot_second.get_by_name(fun, group_name)
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
                                       Result.Kind.UNKNOWN,
                                       Result.Kind.ERROR] or config.full_diff:
                    if fun_result.kind == Result.Kind.NOT_EQUAL or \
                       config.full_diff:
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
                            show_diff=config.show_diff,
                            full_diff=config.full_diff,
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
    # Create yaml output
    if output_dir is not None and os.path.isdir(output_dir):
        old_dir_abs = os.path.join(os.path.abspath(args.snapshot_dir_old), "")
        new_dir_abs = os.path.join(os.path.abspath(args.snapshot_dir_new), "")
        yaml_output = YamlOutput(snapshot_dir_old=old_dir_abs,
                                 snapshot_dir_new=new_dir_abs, result=result)
        yaml_output.save(output_dir=output_dir, file_name="diffkemp-out.yaml")
    config.snapshot_first.finalize()
    config.snapshot_second.finalize()

    if output_dir is not None and os.path.isdir(output_dir):
        print("Differences stored in {}/".format(output_dir))

    if args.report_stat or args.extended_stat:
        print("")
        print("Statistics")
        print("----------")
        result.stop_time = default_timer()
        result.report_stat(args.show_errors, args.extended_stat)
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
                      fun_tag, output_dir, show_diff, full_diff,
                      initial_indent):
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
    :param full_diff: Print semantics-preserving syntax diffs too.
    :param initial_indent: Initial indentation of printed messages
    """
    def text_indent(text, width):
        """Indent each line in the text by a number of spaces given by width"""
        return ''.join(" " * width + line for line in text.splitlines(True))

    old_dir_abs_path = os.path.join(os.path.abspath(snapshot_dir_old), "")
    new_dir_abs_path = os.path.join(os.path.abspath(snapshot_dir_new), "")

    if fun_result.kind == Result.Kind.NOT_EQUAL or (
            full_diff and any([x.diff for x in fun_result.inner.values()])):
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
                    called_res.first.callstack.as_str_with_rel_paths(
                        old_dir_abs_path), indent))
                output.write("\n\n")
            if called_res.second.callstack:
                output.write(
                    text_indent("Callstack ({}):\n".format(snapshot_dir_new),
                                indent))
                output.write(text_indent(
                    called_res.second.callstack.as_str_with_rel_paths(
                        new_dir_abs_path), indent))
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


def view(args):
    """
    View the compare differences. Prepares files for the visualisation
    and runs viewer.
    """
    # Load yaml describing results
    YAML_FILE_NAME = "diffkemp-out.yaml"
    yaml_path = os.path.join(args.compare_output_dir, YAML_FILE_NAME)
    if not os.path.exists(yaml_path):
        sys.stderr.write(
            "ERROR: compare output is missing file " + f"'{YAML_FILE_NAME}'\n")
        sys.exit(errno.EINVAL)
    with open(yaml_path, "r") as file:
        yaml_result = yaml.safe_load(file)

    # Determine the view_directory.
    # The manually built one has priority over the installed one.
    # public_directory: Path to folder which viewer can access.
    view_directory = os.path.join(os.path.dirname(__file__), "../view")
    # Manually build one
    if os.path.exists(view_directory):
        if args.devel:
            public_directory = os.path.join(view_directory, "public")
        else:
            public_directory = os.path.join(view_directory, "build")
            if not os.path.isdir(public_directory):
                sys.stderr.write(
                    "Could not find production build of the viewer.\n" +
                    "Use --devel to run a development server " +
                    "or execute CMake with -DBUILD_VIEWER=On.\n")
                sys.exit(errno.ENOENT)
    # Installed one
    elif os.path.exists(VIEW_INSTALL_DIR):
        view_directory = VIEW_INSTALL_DIR
        public_directory = VIEW_INSTALL_DIR
        if args.devel:
            sys.stderr.write(
                "Error: it is not possible to run the development server " +
                "for installed DiffKemp\n")
            sys.exit(errno.EINVAL)
    # View directory does not exist
    else:
        sys.stderr.write(
            "Error: the viewer was not found.\n" +
            "The viewer was probably not installed.\n")
        sys.exit(errno.ENOENT)

    # Dir for storing necessary data for a visualisation of a compared project.
    data_directory = os.path.join(public_directory, "data")
    if not os.path.exists(data_directory):
        os.mkdir(data_directory)

    # Preparing source directory
    source_directory = os.path.join(data_directory, "src")
    if not os.path.exists(source_directory):
        os.mkdir(source_directory)
    # Preparing diff directory
    diff_directory = os.path.join(data_directory, "diffs")
    if not os.path.exists(diff_directory):
        os.mkdir(diff_directory)

    # Prepare source and diff files to view directory
    old_snapshot_dir = yaml_result["old-snapshot"]
    new_snapshot_dir = yaml_result["new-snapshot"]
    # Check if snapshot dirs exist
    if not os.path.isdir(old_snapshot_dir):
        sys.stderr.write(
            f"Error: expecting to find old snapshot in {old_snapshot_dir}\n")
        sys.exit(errno.EINVAL)
    if not os.path.isdir(new_snapshot_dir):
        sys.stderr.write(
            f"Error: expecting to find new snapshot in {new_snapshot_dir}\n")
        sys.exit(errno.EINVAL)

    processed_files = set()
    for name, definition in yaml_result["definitions"].items():
        # Relatives paths to source files
        old_file = definition["old"]["file"]
        new_file = definition["new"]["file"]

        old_file_abs_path = os.path.join(old_snapshot_dir, old_file)
        new_file_abs_path = os.path.join(new_snapshot_dir, new_file)

        # Copy source file
        if old_file not in processed_files:
            processed_files.add(old_file)

            output_file_path = os.path.join(source_directory, old_file)
            output_dir_path = os.path.dirname(output_file_path)
            if not os.path.isdir(output_dir_path):
                os.makedirs(output_dir_path)

            shutil.copy(old_file_abs_path, output_dir_path)

        # Make diff of function, add diff info to YAML
        if "end-line" in definition["old"] and "end-line" in definition["new"]:
            diff = unified_syntax_diff(first_file=old_file_abs_path,
                                       second_file=new_file_abs_path,
                                       first_line=definition["old"]["line"],
                                       second_line=definition["new"]["line"],
                                       first_end=definition["old"]["end-line"],
                                       second_end=definition["new"]["end-line"]
                                       )
            if diff.isspace() or diff == "":
                definition["diff"] = False
            else:
                definition["diff"] = True
                diff_path = os.path.join(diff_directory, name + ".diff")
                with open(diff_path, "w") as file:
                    file.write(diff)
        else:
            definition["diff"] = False

    # save YAML
    with open(os.path.join(data_directory, YAML_FILE_NAME), "w") as file:
        yaml.dump(yaml_result, file, sort_keys=False)

    if args.devel:
        os.chdir(view_directory)
        os.system("npm install")
        os.system("npm start")
    else:
        os.chdir(public_directory)
        handler = SimpleHTTPRequestHandler
        handler.log_message = lambda *_, **__: None
        with HTTPServer(("localhost", 3000), handler) as httpd:
            print("Result viewer is available at http://localhost:3000")
            print("Press Ctrl+C to exit")
            try:
                httpd.serve_forever()
            except KeyboardInterrupt:
                httpd.shutdown()
    # Cleaning
    shutil.rmtree(source_directory)
    shutil.rmtree(diff_directory)
    os.remove(os.path.join(data_directory, YAML_FILE_NAME))
