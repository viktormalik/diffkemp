from __future__ import absolute_import

from argparse import ArgumentParser
from diffkemp.llvm_ir.build_llvm import LlvmKernelBuilder, LlvmKernelModule
from diffkemp.llvm_ir.kernel_module import KernelParam
from diffkemp.semdiff.module_diff import diff_all_modules_using_global, \
    functions_diff
from diffkemp.semdiff.result import Result
import os
import shutil
import sys


def __make_argument_parser():
    """Parsing CLI arguments."""
    ap = ArgumentParser(description="Check semantic equivalence of KABI \
                        whitelist functions.")
    ap.add_argument("src_version")
    ap.add_argument("dest_version")
    ap.add_argument("-f", "--function", help="specify a function to compare")
    ap.add_argument("-v", "--verbose", help="increase output verbosity",
                    action="store_true")
    ap.add_argument("-t", "--timeout", help="timeout in seconds for a single \
                    function comparison")
    ap.add_argument("--report-stat", help="report statistics of the analysis",
                    action="store_true")
    ap.add_argument("--syntax-diff", help="for functions that are \
                    semantically different, show the result of diff",
                    action="store_true")
    ap.add_argument("--control-flow-only", help="see only control-flow \
                    differences", action="store_true")
    ap.add_argument("--include-globals", help="include also whitelists that \
                    are global variables", action="store_true")
    ap.add_argument("--log-files", help="log diff results into files",
                    action="store_true")
    return ap


def run_from_cli():
    """Main method to run the tool"""
    ap = __make_argument_parser()
    args = ap.parse_args()

    try:
        # Prepare kernels
        first_builder = LlvmKernelBuilder(args.src_version, None, debug=True,
                                          verbose=False)
        if args.function:
            kabi_funs_first = [args.function]
        else:
            kabi_funs_first = first_builder.get_kabi_whitelist()

        second_builder = LlvmKernelBuilder(args.dest_version, None, debug=True,
                                           verbose=False)
        if args.function:
            kabi_funs_second = [args.function]
        else:
            kabi_funs_second = second_builder.get_kabi_whitelist()

        timeout = int(args.timeout) if args.timeout else 40

        if args.log_files:
            dirname = logs_dirname(args.src_version, args.dest_version)
            if os.path.exists(dirname):
                shutil.rmtree(dirname)
            os.mkdir(dirname)

        print "Computing semantic difference of functions on kabi whitelist"
        print "------------------------------------------------------------"
        result = Result(Result.Kind.NONE, args.src_version, args.dest_version)
        for f in kabi_funs_first:
            if f not in kabi_funs_second:
                continue
            try:
                # Find source files with function definitions and build them
                mod_first = first_builder.build_file_for_symbol(f)
                mod_second = second_builder.build_file_for_symbol(f)

                fun_result = None
                if mod_first.has_function(f):
                    if not args.syntax_diff:
                        print f
                    # Compare functions semantics
                    fun_result = functions_diff(
                        first=mod_first.llvm, second=mod_second.llvm,
                        funFirst=f, funSecond=f,
                        glob_var=None,
                        timeout=timeout,
                        syntax_only=args.syntax_diff,
                        control_flow_only=args.control_flow_only,
                        verbose=args.verbose)
                    fun_result.first.name = f
                    fun_result.second.name = f
                elif args.include_globals:
                    # f is a global variable: compare semantics of all
                    # functions using the variable
                    if not args.syntax_diff:
                        print "{} (global variable)".format(f)
                        print "Comparing all functions using {}".format(f)
                    globvar = KernelParam(f)
                    fun_result = diff_all_modules_using_global(
                        first_builder=first_builder,
                        second_builder=second_builder,
                        glob_first=globvar,
                        glob_second=globvar,
                        timeout=timeout,
                        syntax_only=args.syntax_diff,
                        control_flow_only=args.control_flow_only,
                        verbose=args.verbose)

                if fun_result is not None:
                    result.add_inner(fun_result)
                    if args.syntax_diff:
                        print_sytax_diff(args.src_version, args.dest_version,
                                         first_builder.kernel_path,
                                         second_builder.kernel_path,
                                         f, fun_result, args.log_files)
                    else:
                        print "  {}".format(str(fun_result.kind).upper())

            except Exception as e:
                if not args.syntax_diff:
                    sys.stderr.write("  ERROR: {}\n".format(str(e)))
                result.add_inner(Result(Result.Kind.ERROR, f, f))
            finally:
                # Clean LLVM modules (allow GC to collect the occupied memory)
                try:
                    mod_first.clean_module()
                    mod_second.clean_module()
                except NameError:
                    pass
                LlvmKernelModule.clean_all()

        print ""
        print "Statistics"
        print "----------"
        result.report_stat()
        return 0

    except Exception as e:
        sys.stderr.write("Error: {}\n".format(str(e)))
        return -1


def logs_dirname(src_version, dest_version):
    """Name of the directory to put log files into."""
    return "kabi-diff-{}_{}".format(src_version, dest_version)


def print_sytax_diff(src_version, dest_version, src_path, dest_path, fun,
                     fun_result, log_files):
    """
    Log syntax diff of 2 functions. If log_files is set, the output is printed
    into a separate file, otherwise it goes to stdout.
    :param src_version: Source version number
    :param dest_version: Destination version number
    :param src_path: Path to the source kernel root directory
    :param dest_path: Path to the destination kernel root directory
    :param fun: Name of the analysed function
    :param fun_result: Result of the analysis
    :param log_files: True if the output is to be written into a file
    """
    def text_indent(text, width):
        """Indent each line in the text by a number of spaces given by width"""
        return ''.join(" " * width + line for line in text.splitlines(True))

    if fun_result.kind == Result.Kind.NOT_EQUAL:
        if log_files:
            output = open(os.path.join(logs_dirname(src_version, dest_version),
                                       "{}.diff".format(fun)), 'w')
            indent = 2
        else:
            output = sys.stdout
            indent = 4
            print "{}:".format(fun)

        for called_res in fun_result.inner.itervalues():
            output.write(
                text_indent("{} differs:\n".format(called_res.first.name),
                            indent - 2))
            if not log_files:
                print "  {{{"
            output.write(text_indent("Callstack ({}):\n".format(src_version),
                                     indent))
            output.write(text_indent(
                called_res.first.callstack.replace(src_path + "/", ""),
                indent))
            output.write("\n\n")
            output.write(text_indent("Callstack ({}):\n".format(dest_version),
                                     indent))
            output.write(text_indent(
                called_res.second.callstack.replace(dest_path + "/", ""),
                indent))
            output.write("\n\n")
            output.write(text_indent("Diff:\n", indent))
            output.write(text_indent(
                called_res.diff, indent))
            if not log_files:
                print "  }}}"
            output.write("\n")
