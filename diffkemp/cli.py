from argparse import ArgumentParser, SUPPRESS
import diffkemp.diffkemp


def make_argument_parser():
    """Parsing CLI arguments."""
    ap = ArgumentParser(description="Checking equivalence of semantics of "
                                    "functions in large C projects.")
    ap.add_argument("-v", "--verbose",
                    help="increase output verbosity",
                    action="count", default=0)
    sub_ap = ap.add_subparsers(dest="command", metavar="command")
    sub_ap.required = True

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
    compare_ap.add_argument("--regex-filter",
                            help="filter function diffs by given regex")
    compare_ap.add_argument("--output-dir", "-o",
                            help="name of the output directory")
    compare_ap.add_argument("--stdout", help="print results to stdout",
                            action="store_true")
    compare_ap.add_argument("--report-stat",
                            help="report statistics of the analysis",
                            action="store_true")
    compare_ap.add_argument("--source-dirs",
                            nargs=2,
                            help="specify root dirs for the compared projects")
    compare_ap.add_argument("--function", "-f",
                            help="compare only selected function")
    compare_ap.add_argument("--patterns", "-p",
                            help="difference pattern file or configuration")
    compare_ap.add_argument("--output-llvm-ir",
                            help="output each simplified module to a file",
                            action="store_true")
    compare_ap.add_argument("--control-flow-only",
                            help=SUPPRESS,
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
    compare_ap.add_argument("--enable-simpll-ffi",
                            help="calls SimpLL through FFI",
                            action="store_true")
    compare_ap.add_argument("--enable-module-cache",
                            help="loads frequently used modules to memory and \
                            uses them in SimpLL (requires SimpLL FFI to be \
                            enabled)",
                            action="store_true")
    compare_ap.set_defaults(func=diffkemp.diffkemp.compare)
    return ap
