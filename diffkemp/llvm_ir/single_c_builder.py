"""
LLVM source builder for single C file.
"""
from diffkemp.llvm_ir.compiler import get_clang_default_options
from diffkemp.llvm_ir.optimiser import opt_llvm
from diffkemp.llvm_ir.single_llvm_finder import SingleLlvmFinder
from diffkemp.utils import get_functions_from_llvm
import os
from subprocess import check_call


class SingleCBuilder(SingleLlvmFinder):
    """
    LLVM source builder for single C file.
    Extends the SingleLlvmFinder class by compiling C file to LLVM IR file.
    """
    def __init__(self, source_dir, c_file_name, clang="clang",
                 clang_append=[], default_optim=True):
        """
        :param clang: clang compiler to be used
        :param clang_append: list of args to add when compiling
        :param default_optim: use default optimalisations flags
            and run LLVM IR simplification passes
        """
        llvm_file_name = os.path.splitext(c_file_name)[0] + ".ll"
        SingleLlvmFinder.__init__(self, source_dir, llvm_file_name)

        self.c_file_name = c_file_name
        self.clang = clang
        self.clang_append = clang_append
        self.default_optim = default_optim
        self.initialize()

    def str(self):
        return "single_c_file"

    def initialize(self):
        """Compiles the C file to LLVM IR file and runs passes"""
        # Using clang to compile c file to llvm file.
        command = [self.clang, self.c_file_name, "-o", self.llvm_file_name]
        # Note: extending with the default options must be before
        # extending with the clang_append option
        # so the optimisations flags can be redefined
        # (it looks like clang uses last specified optimisaton flag).
        command.extend(get_clang_default_options(self.default_optim))
        command.extend(self.clang_append)
        check_call(command, cwd=self.source_dir)

        # Running llvm passes
        if self.default_optim:
            opt_llvm(self.llvm_file_path)

    def get_function_list(self):
        """Returns a list of functions that are found in the source file."""
        return list(get_functions_from_llvm(
            [os.path.join(self.source_dir, self.llvm_file_name)]).keys())
