"""
LLVM source finder for projects that are entirely compiled into a single
LLVM IR file.
"""
from diffkemp.llvm_ir.llvm_source_finder import LlvmSourceFinder
import os


class SingleLlvmFinder(LlvmSourceFinder):
    """
    LLVM source finder for projects that are entirely compiled into a single
    LLVM IR file.
    Extends the LlvmSourceFinder class by always returning the single IR file.
    """
    def __init__(self, source_dir, llvm_file_name):
        LlvmSourceFinder.__init__(self, source_dir)
        self.llvm_file_name = llvm_file_name
        self.llvm_file_path = os.path.join(self.source_dir,
                                           self.llvm_file_name)

    def str(self):
        return "single_llvm_file"

    def clone_to_dir(self, new_source_dir):
        return SingleLlvmFinder(new_source_dir, self.llvm_file_name)

    def initialize(self):
        pass

    def finalize(self):
        pass

    def find_llvm_with_symbol_def(self, symbol):
        return self.llvm_file_path

    def find_llvm_with_symbol_use(self, symbol):
        return self.llvm_file_path
