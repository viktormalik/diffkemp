"""
LLVM sources finding in a project source tree.
"""
from abc import ABC, abstractmethod
import os


class SourceNotFoundException(Exception):
    def __init__(self, fun):
        self.fun = fun

    def __str__(self):
        return "Source for {} not found".format(self.fun)


class LlvmSourceFinder(ABC):
    """
    Abstract class for finding LLVM files in a source tree.
    Defines methods that each LLVM source finder must implement.
    """
    def __init__(self, source_dir, path):
        self.source_dir = os.path.abspath(source_dir)
        if path is not None:
            self.path = os.path.relpath(path, source_dir)
        else:
            self.path = None

    @abstractmethod
    def str(self):
        pass

    @abstractmethod
    def initialize(self):
        """Initialize the source finder."""
        pass

    @abstractmethod
    def finalize(self):
        """Finalize the source finder."""
        pass

    @abstractmethod
    def find_llvm_with_symbol_def(self, symbol):
        """Get LLVM IR file containing definition of the given symbol"""
        pass

    @abstractmethod
    def find_llvm_with_symbol_use(self, symbol):
        """
        Get a list of LLVM IR files containing functions using the given symbol
        """
        pass
