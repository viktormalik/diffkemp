"""Semantic difference of two functions using llreve and Z3 SMT solver."""
from diffkemp.llvm_ir.kernel_source import SourceNotFoundException
from diffkemp.simpll.simpll import simplify_modules_diff, SimpLLException
from diffkemp.semdiff.result import Result
from diffkemp.syndiff.function_syntax_diff import syntax_diff
from subprocess import Popen, PIPE
from threading import Timer
import sys


def _kill(processes):
    """ Kill each process of the list. """
    for p in processes:
        p.kill()


def _run_llreve_z3(first, second, funFirst, funSecond, coupled, timeout,
                   verbose):
    """
    Run the comparison of semantics of two functions using the llreve tool and
    the Z3 SMT solver. The llreve tool takes compared functions in LLVM IR and
    generates a formula in first-order predicate logic. The formula is then
    solved using the Z3 solver. If it is unsatisfiable, the compared functions
    are semantically the same, otherwise, they are different.

    The generated formula is in the theory of bitvectors.

    :param first: File with the first LLVM module
    :param second: File with the second LLVM module
    :param funFirst: Function from the first module to be compared
    :param funSecond: Function from the second module to be compared
    :param coupled: List of coupled functions (functions that are supposed to
                    correspond to each other in both modules). These are needed
                    for functions not having definintions.
    :param timeout: Timeout for the analysis in seconds
    :param verbose: Verbosity option
    """

    stderr = None
    if not verbose:
        stderr = open('/dev/null', 'w')

    # Commands for running llreve and Z3 (output of llreve is piped into Z3)
    command = ["build/llreve/reve/reve/llreve",
               first, second,
               "--fun=" + funFirst + "," + funSecond,
               "-muz", "--ir-input", "--bitvect", "--infer-marks",
               "--disable-auto-coupling"]
    for c in coupled:
        command.append("--couple-functions={},{}".format(c[0], c[1]))

    if verbose:
        sys.stderr.write(" ".join(command) + "\n")

    llreve_process = Popen(command, stdout=PIPE, stderr=stderr)

    z3_process = Popen(["z3", "fixedpoint.engine=duality", "-in"],
                       stdin=llreve_process.stdout,
                       stdout=PIPE, stderr=stderr)

    # Set timeout for both tools
    timer = Timer(timeout, _kill, [[llreve_process, z3_process]])
    try:
        timer.start()

        z3_process.wait()
        result_kind = Result.Kind.ERROR
        # Processing the output
        for line in z3_process.stdout:
            line = line.strip()
            if line == b"sat":
                result_kind = Result.Kind.NOT_EQUAL
            elif line == b"unsat":
                result_kind = Result.Kind.EQUAL
            elif line == b"unknown":
                result_kind = Result.Kind.UNKNOWN

        if z3_process.returncode != 0:
            result_kind = Result.Kind.ERROR
    finally:
        if not timer.is_alive():
            result_kind = Result.Kind.TIMEOUT
        timer.cancel()

    return Result(result_kind, first, second)


def functions_semdiff(first, second, fun_first, fun_second, config):
    """
    Compare two functions for semantic equality.

    Functions are compared under various assumptions, each having some
    'assumption level'. The higher the level is, the more strong assumption has
    been made. Level 0 indicates no assumption. These levels are determined
    from differences of coupled functions that are set as a parameter of the
    analysis. The analysis tries all assumption levels in increasing manner
    until functions are proven to be equal or no assumptions remain.

    :param first: File with the first LLVM module
    :param second: File with the second LLVM module
    :param fun_first: Function from the first module to be compared
    :param fun_second: Function from the second module to be compared
    :param config: Configuration.
    """
    if fun_first == fun_second:
        fun_str = fun_first
    else:
        fun_str = fun_second
    sys.stdout.write("      Semantic diff of {}".format(fun_str))
    sys.stdout.write("...")
    sys.stdout.flush()

    # Run the actual analysis
    if config.semdiff_tool == "llreve":
        called_first = first.get_functions_called_by(fun_first)
        called_second = second.get_functions_called_by(fun_second)
        called_couplings = [(f, s) for f in called_first for s in called_second
                            if f == s]
        result = _run_llreve_z3(first.llvm, second.llvm, fun_first, fun_second,
                                called_couplings, config.timeout,
                                config.verbosity)
        first.clean_module()
        second.clean_module()
        return result


def functions_diff(mod_first, mod_second,
                   fun_first, fun_second,
                   glob_var, config):
    """
    Compare two functions for equality.

    First, functions are simplified and compared for syntactic equality using
    the SimpLL tool. If they are not syntactically equal, SimpLL prints a list
    of functions that the syntactic equality depends on. These are then
    compared for semantic equality.
    :param mod_first: First LLVM module
    :param mod_second: Second LLVM module
    :param fun_first: Function from the first module to be compared
    :param fun_second: Function from the second module to be compared
    :param glob_var: Global variable whose effect on the functions to compare
    :param config: Configuration
    """
    result = Result(Result.Kind.NONE, fun_first, fun_second)
    try:
        if config.verbosity:
            if fun_first == fun_second:
                fun_str = fun_first
            else:
                fun_str = "{} and {}".format(fun_first, fun_second)
            print("Syntactic diff of {} (in {})".format(fun_str,
                                                        mod_first.llvm))

        simplify = True
        while simplify:
            simplify = False
            # Simplify modules
            first_simpl, second_simpl, objects_to_compare, missing_defs, \
                syndiff_bodies, analysed_loc, analysed_funs = \
                simplify_modules_diff(mod_first.llvm, mod_second.llvm,
                                      fun_first, fun_second,
                                      glob_var.name if glob_var else None,
                                      config,
                                      glob_var.name if glob_var else "simpl")
            funs_to_compare = list([o for o in objects_to_compare
                                    if o[0].diff_kind == "function"])
            if funs_to_compare and missing_defs:
                # If there are missing function definitions, try to find
                # implementing them, link those to the current modules, and
                # rerun the simplification.
                for fun_pair in missing_defs:
                    if "first" in fun_pair:
                        try:
                            new_mod = config.source_first \
                                .get_module_for_symbol(fun_pair["first"])
                            if mod_first.link_modules([new_mod]):
                                simplify = True
                            new_mod.clean_module()
                        except SourceNotFoundException:
                            pass
                    if "second" in fun_pair:
                        try:
                            new_mod = config.source_second \
                                .get_module_for_symbol(fun_pair["second"])
                            if mod_second.link_modules([new_mod]):
                                simplify = True
                            new_mod.clean_module()
                        except SourceNotFoundException:
                            pass
        mod_first.restore_unlinked_llvm()
        mod_second.restore_unlinked_llvm()

        if analysed_loc:
            result.analysed_loc = analysed_loc
        if analysed_funs:
            result.analysed_funs = analysed_funs

        if not objects_to_compare:
            result.kind = Result.Kind.EQUAL_SYNTAX
        else:
            # If the functions are not syntactically equal, objects_to_compare
            # contains a list of functions and macros that are different.
            for fun_pair in objects_to_compare:
                if (not fun_pair[0].diff_kind == "function" and
                        config.semdiff_tool is not None):
                    # If a semantic diff tool is set, use it for further
                    # comparison of non-equal functions
                    fun_result = functions_semdiff(first_simpl, second_simpl,
                                                   fun_pair[0].name,
                                                   fun_pair[1].name,
                                                   config)
                else:
                    fun_result = Result(Result.Kind.NOT_EQUAL, "", "")
                fun_result.first = fun_pair[0]
                fun_result.second = fun_pair[1]
                if fun_result.kind == Result.Kind.NOT_EQUAL:
                    if fun_result.first.diff_kind in ["function", "type"]:
                        # Get the syntactic diff of functions or types
                        fun_result.diff = syntax_diff(
                            fun_result.first.filename,
                            fun_result.second.filename,
                            fun_result.first.name,
                            fun_result.first.diff_kind,
                            fun_pair[0].line,
                            fun_pair[1].line)
                    elif fun_result.first.diff_kind == "syntactic":
                        # Find the syntax differences and append the left and
                        # right value to create the resulting diff
                        found = None
                        for sd in syndiff_bodies:
                            if sd["name"] == fun_result.first.name:
                                found = sd
                                break
                        if found is not None:
                            fun_result.diff = "  {}\n\n  {}\n".format(
                                sd["left-value"], sd["right-value"])
                    else:
                        sys.stderr.write(
                            "warning: unknown diff kind: {}\n".format(
                                fun_result.first.diff_kind))
                        fun_result.diff = "unknown\n"
                result.add_inner(fun_result)
        if config.verbosity:
            print("  {}".format(result))
    except SimpLLException as e:
        if config.verbosity:
            print(e)
        result.kind = Result.Kind.ERROR
    return result
