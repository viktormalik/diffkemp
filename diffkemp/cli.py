import argparse
from argparse import ArgumentParser, SUPPRESS
import diffkemp.diffkemp


def not_negative(value):
    int_value = int(value)
    if int_value < 0:
        raise argparse.ArgumentTypeError(
            f"{value} is an invalid non-negative number")
    return int_value


def make_argument_parser():
    """Parsing CLI arguments."""
    ap = ArgumentParser(description="Checking equivalence of semantics of "
                                    "functions in large C projects.")
    ap.add_argument("-v", "--verbose",
                    help="increase output verbosity",
                    action="count", default=0)
    ap.add_argument("-d", "--debug",
                    help="increase debug output verbosity",
                    action="count", default=0)
    sub_ap = ap.add_subparsers(dest="command", metavar="command")
    sub_ap.required = True

    # "build" sub-command
    build_ap = sub_ap.add_parser("build",
                                 help="build snapshot from Makefile project "
                                 "or from a single C file")
    build_ap.add_argument("source_dir",
                          help="project's root directory "
                          "or a path to a single C file")
    build_ap.add_argument("output_dir",
                          help="output directory of the snapshot")
    build_ap.add_argument("symbol_list", nargs='?',
                          help="list of symbols to compare")
    build_ap.add_argument("--build-program", help="make tool used to be used\
                          for build", default="make")
    build_ap.add_argument("--build-file", help="filename of Makefile to be\
                          used for build")
    build_ap.add_argument("--clang", help="clang compiler to be used for\
                          building", default="clang")
    build_ap.add_argument("--clang-append", help="option to append to clang",
                          action='append')
    build_ap.add_argument("--clang-drop", help="option to drop from clang",
                          action='append')
    build_ap.add_argument("--llvm-link", help="llvm-link to be used for ll\
                          file linking", default="llvm-link")
    build_ap.add_argument("--llvm-dis", help="llvm-dis to be used for bc file\
                          disassembly", default="llvm-dis")
    build_ap.add_argument("--target", help="Makefile target to build",
                          action='append')
    build_ap.add_argument("--reconfigure", help="reconfigure autotools-based\
                          project with CC=<wrapper>", action="store_true")
    build_ap.add_argument("--no-native-cc-wrapper",
                          help="do not use a native compiler wrapper even if\
                          present", action="store_true")
    build_ap.set_defaults(func=diffkemp.diffkemp.build)

    # "build-kernel" sub-command
    build_kernel_ap = sub_ap.add_parser(
        "build-kernel",
        help="generate snapshot from Linux kernel")
    build_kernel_ap.add_argument("source_dir",
                                 help="kernel's root directory")
    build_kernel_ap.add_argument("output_dir",
                                 help="output directory of the snapshot")
    build_kernel_ap.add_argument("symbol_list",
                                 help="list of symbols (functions) to compare")
    build_kernel_ap.add_argument(
        "--sysctl",
        action="store_true",
        help="interpret symbol list as a list of sysctl parameters")
    build_kernel_ap.add_argument(
        "--no-source-dir",
        action="store_true",
        help="do not store path to the source kernel directory in snapshot")
    build_kernel_ap.set_defaults(func=diffkemp.diffkemp.build_kernel)

    # "llvm-to-snapshot" sub-command
    llvm_snapshot_ap = sub_ap.add_parser(
        "llvm-to-snapshot",
        help="generate snapshot from a single LLVM IR file")
    llvm_snapshot_ap.add_argument("source_dir",
                                  help="project's root directory")
    llvm_snapshot_ap.add_argument("llvm_file", help="name of the LLVM IR file")
    llvm_snapshot_ap.add_argument("output_dir",
                                  help="output directory of the snapshot")
    llvm_snapshot_ap.add_argument("function_list",
                                  help="list of functions to compare")
    llvm_snapshot_ap.set_defaults(func=diffkemp.diffkemp.llvm_to_snapshot)

    # "compare" sub-command
    compare_ap = sub_ap.add_parser("compare",
                                   help="compare generated snapshots for "
                                        "semantic equality")
    compare_ap.add_argument("snapshot_dir_old",
                            help="directory with the old snapshot")
    compare_ap.add_argument("snapshot_dir_new",
                            help="directory with the new snapshot")
    compare_ap.add_argument("--show-diff",
                            help="show diff for non-equal functions",
                            action="store_true")
    compare_ap.add_argument("--no-show-diff",
                            help="do not show diff for non-equal functions",
                            action="store_true")
    compare_ap.add_argument("--regex-filter",
                            help="filter function diffs by given regex")
    compare_ap.add_argument("--output-dir", "-o",
                            help="name of the output directory")
    compare_ap.add_argument("--stdout", help="print results to stdout",
                            action="store_true")
    compare_ap.add_argument("--report-stat",
                            help="report statistics of the analysis",
                            action="store_true")
    compare_ap.add_argument("--extended-stat",
                            help="report detailed statistics about the "
                                 "analysis (may be more expensive to compute)",
                            action="store_true")
    compare_ap.add_argument("--source-dirs",
                            nargs=2,
                            help="specify root dirs for the compared projects")
    compare_ap.add_argument("--function", "-f",
                            help="compare only selected function")
    compare_ap.add_argument("--custom-patterns", "-p",
                            help="custom pattern file or configuration")
    compare_ap.add_argument("--use-smt",
                            help="Use SMT-based checking of short snippets",
                            action="store_true")
    compare_ap.add_argument("--smt-timeout",
                            help="Set timeout for SMT checking. Set to 0 for \
                            no timeout.",
                            type=not_negative,
                            default=30)
    compare_ap.add_argument("--output-llvm-ir",
                            help="output each simplified module to a file",
                            action="store_true")
    compare_ap.add_argument("--print-asm-diffs",
                            help="print raw inline assembly differences (does \
                            not apply to macros)",
                            action="store_true")
    compare_ap.add_argument("--semdiff-tool",
                            help=SUPPRESS,
                            choices=["llreve"])
    compare_ap.add_argument("--show-errors",
                            help="show functions that are either unknown or \
                            ended with an error in statistics",
                            action="store_true")
    compare_ap.add_argument("--disable-simpll-ffi",
                            help="call SimpLL through binary (for debugging)",
                            action="store_true")
    compare_ap.add_argument("--enable-module-cache",
                            help="loads frequently used modules to memory and \
                            uses them in SimpLL",
                            action="store_true")
    compare_ap.add_argument("--full-diff",
                            help="show diff for all functions \
                            (even semantically equivalent ones)",
                            action="store_true")

    BUILTIN_PATTERNS = ["struct-alignment",
                        "function-splits",
                        "unused-returns",
                        "kernel-prints",
                        "dead-code",
                        "numerical-macros",
                        "relocations",
                        "type-casts",
                        "control-flow-only",
                        "inverse-conditions",
                        "reordered-bin-ops",
                        "group-vars"]

    # Semantic patterns options.
    compare_ap.add_argument("--enable-pattern",
                            action="append", default=[],
                            choices=BUILTIN_PATTERNS,
                            help="choose which built-in patterns should be "
                                 "explicitly enabled")
    compare_ap.add_argument("--disable-pattern",
                            action="append", default=[],
                            choices=BUILTIN_PATTERNS,
                            help="choose which built-in patterns should be "
                                 "explicitly disabled")
    compare_ap.add_argument("--enable-all-patterns", action="append_const",
                            dest="enable_pattern", const="all",
                            help="enable all supported built-in patterns")
    compare_ap.add_argument("--disable-all-patterns", action="append_const",
                            dest="disable_pattern", const="all",
                            help="disable all built-in patterns")

    compare_ap.set_defaults(func=diffkemp.diffkemp.compare)

    # "view" sub-command
    view_ap = sub_ap.add_parser("view",
                                help="view differences found by compare")
    view_ap.add_argument("compare_output_dir",
                         help="directory with the compare output")
    view_ap.add_argument("--devel",
                         action="store_true",
                         help="runs development server instead of production \
                         server")
    view_ap.set_defaults(func=diffkemp.diffkemp.view)
    return ap


def run_from_cli():
    """Main method to run the tool."""
    ap = make_argument_parser()
    args = ap.parse_args()
    if args.verbose or args.debug:
        args.verbose = 1 + args.debug
    args.func(args)
