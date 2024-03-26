"""
Functions for compilation of c files to LLVM IR.

This file must be RPython compatible because it is used from cc_wrapper.
"""


def get_clang_default_options(default_optim=True):
    """Returns clang options for compiling c files to LLVM IR.
    :param default_optim: By default adds also optimization flags."""
    opts = ["-S", "-emit-llvm", "-g", "-fdebug-macro", "-Wno-format-security"]
    if default_optim:
        opts.extend(["-O1", "-Xclang", "-disable-llvm-passes"])
    return opts
