#!/usr/bin/env python3
from cffi import FFI
from subprocess import check_output
from diffkemp.utils import get_simpll_build_dir


def get_c_declarations(header_filename):
    """
    Extracts C declarations important for the cFFI module from the SimpLL FFI
    header file.
    :param filename Name of the SimpLL header file
    """
    cdef_start = "// CFFI_DECLARATIONS_START\n"
    cdef_end = "// CFFI_DECLARATIONS_END\n"

    with open(header_filename, "r") as header_file:
        lines = header_file.readlines()
        start = lines.index(cdef_start) + 1
        end = lines.index(cdef_end)
        return "".join(lines[start:end])


ffibuilder = FFI()

ffibuilder.cdef(get_c_declarations("diffkemp/simpll/library/FFI.h"))

llvm_components = ["irreader", "passes", "support"]
llvm_cflags = check_output(["llvm-config", "--cflags"])
llvm_ldflags = check_output(["llvm-config", "--ldflags"])
llvm_libs = check_output(["llvm-config", "--libs"] + llvm_components +
                         ["--system-libs"])

llvm_cflags = list(filter(lambda x: x != "",
                          llvm_cflags.decode("ascii").strip().split()))
llvm_ldflags = list(filter(lambda x: x != "",
                           llvm_ldflags.decode("ascii").strip().split()))
llvm_libs = list(filter(lambda x: x != "",
                        llvm_libs.decode("ascii").strip().split()))

simpll_link_arg = "-L{}/diffkemp/simpll".format(get_simpll_build_dir())

ffibuilder.set_source(
    "diffkemp.simpll._simpll", '#include <library/FFI.h>',
    language="c++",
    libraries=['simpll-lib'],
    extra_compile_args=["-Idiffkemp/simpll"] + llvm_cflags,
    extra_link_args=[simpll_link_arg, "-lstdc++"] + llvm_ldflags +
    llvm_libs + ["-lz3"])

if __name__ == "__main__":
    ffibuilder.compile()
