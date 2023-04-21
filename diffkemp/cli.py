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

    # "generate-pattern" sub-command
    generate_ap = sub_ap.add_parser("generate",
                                    help="Generate pattern for diffkemp\
                                    compare")
    generate_ap.add_argument(
        "--pattern-config",
        "-c",
        required=False,
        help="function for which we are generating pattern")
    # generate_ap.add_argument(
    #     "--function",
    #     "-f",
    #     required=False,
    #     help="function for which we are generating pattern")
    # generate_ap.add_argument("diff_chunks",
    #                          metavar="DIFF CHUNKS",
    #                          help="diff chunks from which new pattern should\
    #                          be infered",
    #                          nargs="*")
    generate_ap.set_defaults(func=diffkemp.diffkemp.generate)

    # "build" sub-command
    build_ap = sub_ap.add_parser("build",
                                 help="build snapshot from Makefile project")
    build_ap.add_argument("source_dir",
                          help="project's root directory")
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
    compare_ap.add_argument("--disable-simpll-ffi",
                            help="call SimpLL through binary (for debugging)",
                            action="store_true")
    compare_ap.add_argument("--enable-module-cache",
                            help="loads frequently used modules to memory and \
                            uses them in SimpLL",
                            action="store_true")
    compare_ap.set_defaults(func=diffkemp.diffkemp.compare)
    return ap
