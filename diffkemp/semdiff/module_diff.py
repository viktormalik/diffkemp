"""
Comparing two modules in LLVM IR for semantic equivalence w.r.t. some
global variable (parameter of modules). Each pair of corresponding functions
using the given parameter is compared individually.
"""

from diffkemp.llvm_ir.kernel_llvm_source_builder import BuildException
from diffkemp.semdiff.function_diff import functions_diff
from diffkemp.semdiff.result import Result


def diff_all_modules_using_global(glob_first, glob_second, config):
    """
    Compare semantics of all modules using given global variable.
    Finds all source files that use the given globals and compare all of them.
    :param glob_first: First global to compare
    :param glob_second: Second global to compare
    :param config: Configuration
    """
    result = Result(Result.Kind.NONE, glob_first.name, glob_second.name)
    if glob_first.name != glob_second.name:
        # Variables with different names are treated as unequal
        result.kind = Result.Kind.NOT_EQUAL
        return result

    srcs_first = config.source_first.find_srcs_using_symbol(glob_first.name)
    srcs_second = config.source_second.find_srcs_using_symbol(glob_second.name)
    # Compare all sources containing functions using the variable
    for src in srcs_first:
        if src not in srcs_second:
            result.add_inner(Result(Result.Kind.NOT_EQUAL, src, src))
        else:
            try:
                mod_first = config.source_first.get_module_from_source(src)
                mod_second = config.source_second.get_module_from_source(src)
                mod_first.parse_module()
                mod_second.parse_module()
                if (mod_first.has_global(glob_first.name) and
                        mod_second.has_global(glob_second.name)):
                    src_result = modules_diff(
                        mod_first=mod_first, mod_second=mod_second,
                        glob_var=glob_first, fun=None,
                        config=config)
                    for res in src_result.inner.values():
                        result.add_inner(res)
            except BuildException as e:
                if config.verbosity:
                    print(e)
                result.add_inner(Result(Result.Kind.ERROR, src, src))
    return result


def modules_diff(mod_first, mod_second, glob_var, fun, config):
    """
    Analyse semantic difference of two LLVM IR modules w.r.t. some parameter
    :param mod_first: First LLVM module
    :param mod_second: Second LLVM module
    :param glob_var: Parameter (global variable) to compare (if specified, all
                  functions using this variable are compared).
    :param fun: Function to be compared.
    :param config: Configuration.
    """
    result = Result(Result.Kind.NONE, mod_first, mod_second)

    if fun:
        funs_first = {fun}
        funs_second = {fun}
    elif glob_var:
        funs_first = mod_first.get_functions_using_param(glob_var)
        funs_second = mod_second.get_functions_using_param(glob_var)
    else:
        funs_first = []
        funs_second = []

    for fun in funs_first:
        if fun not in funs_second:
            continue
        if (not mod_first.has_function(fun) or not
                mod_second.has_function(fun)):
            print("    Given function not found in module")
            result.kind = Result.Kind.ERROR
            return result

        fun_result = functions_diff(mod_first=mod_first, mod_second=mod_second,
                                    fun_first=fun, fun_second=fun,
                                    glob_var=glob_var, config=config)
        result.add_inner(fun_result)

    return result
