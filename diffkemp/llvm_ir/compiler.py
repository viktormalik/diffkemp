"""
Functions for compilation of c files to LLVM IR.
"""


def get_clang_default_options():
    """Returns clang options for compiling c files to LLVM IR."""
    return ["-S", "-emit-llvm", "-O1", "-Xclang",
            "-disable-llvm-passes", "-g", "-fdebug-macro",
            "-Wno-format-security"]
