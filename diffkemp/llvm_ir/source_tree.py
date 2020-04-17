"""
Class representing source tree of the analysed project.
Contains functions for getting LLVM modules from the project.
"""
from diffkemp.llvm_ir.kernel_module import LlvmKernelModule
from diffkemp.llvm_ir.llvm_source_finder import SourceNotFoundException
import errno
import os
import shutil


class SourceTree:
    """
    Class representing source tree of the analysed project.
    Contains functions for getting LLVM modules from the project.
    Requires a source finder that extends the LlvmSourceFinder abstract class.
    """
    def __init__(self, source_dir, source_finder_cls=None,
                 source_finder_path=None):
        self.source_dir = os.path.abspath(source_dir)
        self.source_finder = None
        if source_finder_cls is not None:
            self.init_source_finder(source_finder_cls, source_finder_path)
        self.modules = dict()

    def initialize(self):
        if self.source_finder is not None:
            self.source_finder.initialize()

    def finalize(self):
        if self.source_finder is not None:
            self.source_finder.finalize()

    def init_source_finder(self, finder_cls, path):
        self.source_finder = finder_cls(self.source_dir, path)

    def _get_module_from_source(self, source):
        """
        Create instance of KernelModule from an LLVM IR source file.
        Caches created modules to reuse them for the same sources.
        """
        if source in self.modules:
            return self.modules[source]

        module = LlvmKernelModule(source)
        self.modules[source] = module
        return module

    def get_module_for_symbol(self, symbol, created_before=None):
        """
        Get LLVM module containing definition of a symbol.
        :param symbol: Symbol (function or global) to search for.
        :param created_before: If specified, the LLVM module is returned only
                               if the LLVM file was modified before this time.
        :return: Instance of LlvmKernelModule.
        """
        if self.source_finder is None:
            raise SourceNotFoundException(symbol)

        source = self.source_finder.find_llvm_with_symbol_def(symbol)
        if source is None or not os.path.isfile(source):
            raise SourceNotFoundException(symbol)

        # If the LLVM IR file exits but was modified after the given timestamp,
        # do not return the module.
        if created_before:
            try:
                if os.path.getmtime(source) > created_before:
                    raise SourceNotFoundException(symbol)
            except OSError:
                pass

        return self._get_module_from_source(source)

    def get_modules_using_symbol(self, symbol):
        """
        Get all LLVM modules that contain functions using the given symbol.
        :param symbol: Symbol (function or global) to search for.
        :return: List of instances of LlvmKernelModule.
        """
        if self.source_finder is None:
            raise SourceNotFoundException(symbol)

        sources = self.source_finder.find_llvm_with_symbol_use(symbol)
        modules = []
        for src in sources:
            if os.path.isfile(src):
                modules.append(self._get_module_from_source(src))
        return modules

    @staticmethod
    def _create_dir_with_parents(directory):
        """
        Create a directory with all parent directories.
        Implements bash `mkdir -p`.
        :param directory: Path to the directory to create.
        """
        if not os.path.isdir(directory):
            try:
                os.makedirs(directory)
            except OSError as e:
                if e.errno == errno.EEXIST and os.path.isdir(directory):
                    pass
                else:
                    raise

    def copy_source_files(self, modules, target_source_tree):
        """
        Copy C and LLVM source files of given modules from this kernel into
        a different source tree.
        Preserves the directory structure.
        Also copies all headers included by the modules.
        :param modules: List of modules to copy.
        :param target_source_tree: Destination source tree (sub-folders will be
                                   created corresponding to the sources
                                   structure).
        """
        for mod in modules:
            module_dir = os.path.dirname(
                os.path.relpath(mod.llvm, self.source_dir))
            target_module_dir = os.path.join(target_source_tree.source_dir,
                                             module_dir)
            self._create_dir_with_parents(target_module_dir)

            # Copy linked sources and headers.
            for source in mod.get_included_sources():
                src_sourcefile = source
                if not src_sourcefile.startswith(self.source_dir):
                    continue
                dest_sourcefile = os.path.join(
                    target_source_tree.source_dir,
                    os.path.relpath(source, self.source_dir))
                if not os.path.isfile(dest_sourcefile):
                    self._create_dir_with_parents(
                        os.path.dirname(dest_sourcefile))
                    shutil.copyfile(src_sourcefile, dest_sourcefile)

            mod.move_to_other_root_dir(self.source_dir,
                                       target_source_tree.source_dir)
