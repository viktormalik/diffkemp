from argparse import ArgumentParser, SUPPRESS
import diffkemp.diffkemp


def make_argument_parser():
    """Parsing CLI arguments."""
    ap = ArgumentParser(description="Checking equivalence of semantics of "
                                    "kernel functions.")
    ap.add_argument("-v", "--verbose",
                    help="increase output verbosity",
                    action="store_true")
    sub_ap = ap.add_subparsers(dest="command", metavar="command")
    sub_ap.required = True

    # "generate" sub-command
    generate_ap = sub_ap.add_parser("generate",
                                    help="generate snapshot of kernel "
                                         "functions")
    generate_ap.add_argument("kernel_dir",
                             help="kernel root directory")
    generate_ap.add_argument("output_dir",
                             help="output directory of the snapshot")
    generate_ap.add_argument("functions_list",
                             help="list of functions to compare")
    generate_ap.add_argument("--sysctl", action="store_true",
                             help="function list is a list of function "
                                  "parameters")
    generate_ap.set_defaults(func=diffkemp.diffkemp.generate)

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
    compare_ap.add_argument("--kernel-dirs",
                            nargs=2,
                            help="specify root dirs for the compared kernels")
    compare_ap.add_argument("--function", "-f",
                            help="compare only selected function")
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

    # Semantic patterns options.
    compare_ap.add_argument("--enable-pattern",
                            action="append", default=[],
                            choices=["struct-alignment", "function-splits",
                                     "unused-returns", "kernel-prints",
                                     "dead-code", "numerical-macros",
                                     "type-casts", "control-flow-only"],
                            help="choose which semantic patterns should be"
                                 "explicitly enabled")
    compare_ap.add_argument("--disable-pattern",
                            action="append", default=[],
                            choices=["struct-alignment", "function-splits",
                                     "unused-returns", "kernel-prints",
                                     "dead-code", "numerical-macros",
                                     "type-casts", "control-flow-only"],
                            help="choose which semantic patterns should be"
                                 "explicitly disabled")
    compare_ap.add_argument("--enable-all-patterns", action="append_const",
                            dest="enable_pattern", const="all",
                            help="enable all supported semantic patterns")
    compare_ap.add_argument("--disable-all-patterns", action="append_const",
                            dest="disable_pattern", const="all",
                            help="disable all semantic patterns (only do pure"
                                 "syntax diff)")

    compare_ap.set_defaults(func=diffkemp.diffkemp.compare)
    return ap
