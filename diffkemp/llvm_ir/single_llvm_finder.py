"""
LLVM source finder for projects that are entirely compiled into a single
LLVM IR file.
"""
from diffkemp.llvm_ir.llvm_source_finder import LlvmSourceFinder


class SingleLlvmFinder(LlvmSourceFinder):
    """
    LLVM source finder for projects that are entirely compiled into a single
    LLVM IR file.
    Extends the LlvmSourceFinder class by always returning the single IR file.
    """
    def __init__(self, source_dir, llvm_file):
        LlvmSourceFinder.__init__(self, source_dir, llvm_file)

    def str(self):
        return "single_llvm_file"

    def initialize(self):
        pass

    def finalize(self):
        pass

    def find_llvm_with_symbol_def(self, symbol):
        return self.path

    def find_llvm_with_symbol_use(self, symbol):
        return self.path
