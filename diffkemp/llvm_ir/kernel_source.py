"""
Browsing kernel sources.
Functions for searching function definitions, kernel modules, etc.
"""
from diffkemp.llvm_ir.build_llvm import LlvmKernelBuilder, BuildException
from diffkemp.llvm_ir.kernel_module import LlvmKernelModule
from diffkemp.llvm_ir.llvm_sysctl_module import LlvmSysctlModule
import errno
import os
import shutil
from subprocess import CalledProcessError, check_call, check_output


class SourceNotFoundException(Exception):
    def __init__(self, fun):
        self.fun = fun

    def __str__(self):
        return "Source for {} not found".format(self.fun)


class KernelSource:
    """
    Source code of a single kernel.
    Provides functions to search source files for function definitions, kernel
    modules, and others.
    """

    def __init__(self, kernel_dir, with_builder=False, single_llvm_lib=None):
        self.kernel_dir = os.path.abspath(kernel_dir)
        self.builder = LlvmKernelBuilder(kernel_dir) if with_builder else None
        self.llvm_lib_module = \
            LlvmKernelModule(os.path.abspath(single_llvm_lib)) \
            if single_llvm_lib else None
        self.modules = dict()
        self.cscope_cache = dict()

    def initialize(self):
        """
        Prepare the kernel builder.
        This is done automatically on in LlvmKernelBuilder constructor but it
        may be useful to re-initialize the builder after finalize was called.
        """
        if self.builder:
            self.builder.initialize()

    def finalize(self):
        """Restore the kernel builder state."""
        if self.builder:
            self.builder.finalize()

    def get_sources_with_params(self, directory):
        """
        Get list of .c files in the given directory and all its subdirectories
        that contain definitions of module parameters (contain call to
        module_param macro).
        """
        path = os.path.join(self.kernel_dir, directory)
        result = list()
        for f in os.listdir(path):
            file = os.path.join(path, f)
            if os.path.isfile(file) and file.endswith(".c"):
                for line in open(file, "r"):
                    if "module_param" in line:
                        result.append(file)
                        break
            elif os.path.isdir(file):
                dir_files = self.get_sources_with_params(file)
                result.extend(dir_files)

        return result

    def build_cscope_database(self):
        """
        Build a database for the cscope tool. It will be later used to find
        source files with symbol definitions.
        """
        # If the database exists, do not rebuild it
        if "cscope.files" in os.listdir(self.kernel_dir):
            return

        # Write all files that need to be scanned into cscope.files
        with open(os.path.join(self.kernel_dir, "cscope.files"), "w") \
                as cscope_file:
            for root, dirs, files in os.walk(self.kernel_dir):
                if ("/Documentation/" in root or
                        "/scripts/" in root or
                        "/tmp" in root):
                    continue

                for f in files:
                    if os.path.islink(os.path.join(root, f)):
                        continue
                    if f.endswith((".c", ".h", ".x", ".s", ".S")):
                        path = os.path.relpath(os.path.join(root, f),
                                               self.kernel_dir)
                        cscope_file.write("{}\n".format(path))

        # Build cscope database
        cwd = os.getcwd()
        os.chdir(self.kernel_dir)
        check_call(["cscope", "-b", "-q", "-k"])
        os.chdir(cwd)

    def _cscope_run(self, symbol, definition):
        """
        Run cscope search for a symbol.
        :param symbol: Symbol to search for
        :param definition: If true, search definitions, otherwise search all
                           usage.
        :return: List of found cscope entries.
        """
        if (symbol, definition) in self.cscope_cache:
            return self.cscope_cache[(symbol, definition)]

        self.build_cscope_database()
        try:
            command = ["cscope", "-d", "-L"]
            if definition:
                command.append("-1")
            else:
                command.append("-0")
            command.append(symbol)
            with open(os.devnull, "w") as devnull:
                cscope_output = check_output(command, stderr=devnull).decode(
                    'utf-8')
            result = [l for l in cscope_output.splitlines() if
                      l.split()[0].endswith("c")]
            self.cscope_cache[(symbol, definition)] = result
            return result
        except CalledProcessError:
            return []

    def _find_tracepoint_macro_use(self, symbol):
        """
        Find usages of tracepoint macro creating a tracepoint symbol.
        :param symbol: Symbol generated using the macro.
        :return: List of found cscope entries.
        """
        macro_argument = symbol[len("__tracepoint_"):]
        candidates = self._cscope_run("EXPORT_TRACEPOINT_SYMBOL", False)
        return list(filter(lambda c: c.endswith("(" + macro_argument + ");"),
                           candidates))

    def find_srcs_with_symbol_def(self, symbol):
        """
        Use cscope to find a definition of the given symbol.
        :param symbol: Symbol to find.
        :return List of source files potentially containing the definition.
        """
        cwd = os.getcwd()
        os.chdir(self.kernel_dir)
        try:
            cscope_defs = self._cscope_run(symbol, True)

            # It may not be enough to get the definitions from the cscope.
            # There are multiple possible reasons:
            #   - the symbol is only defined in headers
            #   - there is a bug in cscope - it cannot find definitions
            #     containing function pointers as parameters
            cscope_uses = self._cscope_run(symbol, False)

            # Look whether this is one of the special cases when cscope does
            # not find a correct source because of the exact symbol being
            # created by the preprocessor
            if any([symbol.startswith(s) for s in
                    ["param_get_", "param_set_", "param_ops_"]]):
                # Symbol param_* are created in kernel/params.c using a macro
                cscope_defs = ["kernel/params.c"] + cscope_defs
            elif symbol.startswith("__tracepoint_"):
                # Functions starting with __tracepoint_ are created by a macro
                # in include/kernel/tracepoint.h; the corresponding usage of
                # the macro has to be found to get the source file
                cscope_defs = \
                    self._find_tracepoint_macro_use(symbol) + cscope_defs
            elif symbol == "rcu_barrier":
                cscope_defs = ["kernel/rcutree.c"] + cscope_defs

            if len(cscope_defs) == 0 and len(cscope_uses) == 0:
                raise SourceNotFoundException(symbol)
        except SourceNotFoundException:
            if symbol == "vfree":
                cscope_uses = []
                cscope_defs = ["mm/vmalloc.c"]
            else:
                raise
        finally:
            os.chdir(cwd)

        # We now create a list of files potentially containing the file
        # definition. The list is sorted by priority:
        #   1. Files marked by cscope as containing the symbol definition.
        #   2. Files marked by cscope as using the symbol in <global> scope.
        #   3. Files marked by cscope as using the symbol in other scope.
        # Each group is also partially sorted - sources from the drivers/ and
        # the arch/ directories occur later than the others (using prio_key).
        # Moreover, each file occurs in the list just once (in place of its
        # highest priority).
        seen = set()

        def prio_key(item):
            if item.startswith("drivers/"):
                return "}" + item
            if item.startswith("arch/x86"):
                # x86 has priority over other architectures
                return "}}" + item
            if item.startswith("arch/"):
                return "}}}" + item
            else:
                return item

        files = sorted(
            [f for f in [line.split()[0] for line in cscope_defs]
             if not (f in seen or seen.add(f))],
            key=prio_key)
        files.extend(sorted(
            [f for (f, scope) in [(line.split()[0], line.split()[1])
                                  for line in cscope_uses]
             if (scope == "<global>" and not (f in seen or seen.add(f)))],
            key=prio_key))
        files.extend(sorted(
            [f for (f, scope) in [(line.split()[0], line.split()[1])
                                  for line in cscope_uses]
             if (scope != "<global>" and not (f in seen or seen.add(f)))],
            key=prio_key))
        return files

    def find_srcs_using_symbol(self, symbol):
        """
        Use cscope to find sources using a symbol.
        :param symbol: Symbol to find.
        :return List of source files containing functions that use the symbol.
        """
        cwd = os.getcwd()
        os.chdir(self.kernel_dir)
        try:
            cscope_out = self._cscope_run(symbol, False)
            if len(cscope_out) == 0:
                raise SourceNotFoundException
            files = set()
            for line in cscope_out:
                if line.split()[0].endswith(".h"):
                    continue
                if line.split()[1] == "<global>":
                    continue
                files.add(os.path.relpath(line.split()[0], self.kernel_dir))
            return files
        except SourceNotFoundException:
            raise
        finally:
            os.chdir(cwd)

    def get_module_from_source(self, source_path, created_before=None):
        """
        Create an LLVM module from a source file.
        Builds the source into LLVM IR if needed. No module is returned if the
        module is already present but its LLVM IR was generated or its source
        file modified after the given time constraint.
        :param source_path: Relative path to the file
        :param created_before: File creation time constraint.
        :returns Instance of LlvmKernelModule
        """
        name = source_path[:-2] if source_path.endswith(".c") else source_path
        llvm_file = os.path.join(self.kernel_dir, "{}.ll".format(name))
        source_file = os.path.join(self.kernel_dir, source_path)

        # If the LLVM IR file exits but was modified after the given timestamp,
        # do not return the module.
        if created_before:
            try:
                if (os.path.getmtime(source_file) > created_before or
                        os.path.getmtime(llvm_file) > created_before):
                    return None
            except OSError:
                pass

        # If the module has already been created, return it
        if name in self.modules:
            return self.modules[name]

        if self.builder:
            try:
                self.builder.build_source_to_llvm(source_file, llvm_file)
            except BuildException:
                pass

        if not os.path.isfile(llvm_file):
            return None

        mod = LlvmKernelModule(llvm_file, source_file)
        self.modules[name] = mod
        return mod

    def get_module_for_symbol(self, symbol, created_before=None):
        """
        Looks up files containing definition of a symbol using CScope, then
        transforms them into LLVM modules and looks whether the symbol is
        actually defined in the created module.
        In case there are multiple files containing the definition, the first
        module containing the function definition is returned.
        :param symbol: Name of the function to look up.
        :param created_before: LLVM module creation time constraint.
        :returns LLVM module containing the specified function.
        """
        if self.llvm_lib_module:
            return self.llvm_lib_module

        mod = None

        srcs = self.find_srcs_with_symbol_def(symbol)
        for src in srcs:
            mod = self.get_module_from_source(src, created_before)
            if mod:
                if not (mod.has_function(symbol) or mod.has_global(symbol)):
                    mod = None
                else:
                    break
        if not mod:
            raise SourceNotFoundException(symbol)

        return mod

    def get_sysctl_module(self, sysctl):
        """
        Get the LLVM module containing the definition of a sysctl option.
        :param sysctl: sysctl option to search for
        :return: Instance of LlvmSysctlModule.
        """
        # The sysctl is composed of entries separated by dots. Entries form
        # a hierarchy - each entry is a child of its predecessor (i.e. all
        # entries except the last one point to sysctl tables). We follow
        # the hierarchy and build the source containing the parent table of
        # the last entry.
        entries = sysctl.split(".")
        if entries[0] in ["kernel", "vm", "fs", "debug", "dev"]:
            src = "kernel/sysctl.c"
            table = "sysctl_base_table"
        elif entries[0] == "net":
            if entries[1] == "ipv4":
                if entries[2] == "conf":
                    src = "net/ipv4/devinet.c"
                    table = "devinet_sysctl.1"
                    entries = entries[4:]
                else:
                    src = "net/ipv4/sysctl_net_ipv4.c"
                    table = "ipv4_table"
                    entries = entries[2:]
            elif entries[1] == "core":
                src = "net/core/sysctl_net_core.c"
                table = "net_core_table"
                entries = entries[2:]
            else:
                raise SourceNotFoundException(sysctl)
        else:
            raise SourceNotFoundException(sysctl)

        for (i, entry) in enumerate(entries):
            # Build the file normally and then create a corresponding
            # LlvmSysctlModule with the obtained sysctl table.
            kernel_mod = self.get_module_from_source(src)
            sysctl_mod = LlvmSysctlModule(kernel_mod, table)

            if i == len(entries) - 1:
                return sysctl_mod
            table = sysctl_mod.get_child(entry).name
            src = self.find_srcs_with_symbol_def(table)[0]
        raise SourceNotFoundException(sysctl)

    def get_module_for_kernel_mod(self, mod_dir, mod_name):
        """
        Get LLVM module for a kernel module.
        :param mod_dir: Kernel module directory.
        :param mod_name: Kernel module name.
        :return: LlvmKernelModule containing the built LLVM file.
        """
        llvm_file = self.builder.build_kernel_mod_to_llvm(mod_dir, mod_name)
        return LlvmKernelModule(os.path.join(self.kernel_dir, llvm_file))

    @staticmethod
    def create_dir_with_parents(directory):
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

    def copy_source_files(self, modules, target_dir):
        """
        Copy C and LLVM source files of given modules from this kernel into
        a different directory.
        Preserves the directory structure.
        Also copies all headers included by the modules.
        :param modules: List of modules to copy.
        :param target_dir: Destination directory (subfolders will be created
                           corresponding to the sources structure).
        """
        for mod in modules:
            src_dir = os.path.dirname(
                os.path.relpath(mod.llvm, self.kernel_dir))
            target_src_dir = os.path.join(target_dir, src_dir)
            self.create_dir_with_parents(target_src_dir)

            # Copy linked sources and headers.
            for source in mod.get_included_sources():
                src_source = source
                if not src_source.startswith(self.kernel_dir):
                    continue
                dest_source = os.path.join(
                    target_dir, os.path.relpath(source, self.kernel_dir))
                if not os.path.isfile(dest_source):
                    self.create_dir_with_parents(os.path.dirname(dest_source))
                    shutil.copyfile(src_source, dest_source)

            mod.move_to_other_root_dir(self.kernel_dir, target_dir)

    def copy_cscope_files(self, target_dir):
        """
        Copy CScope database into a different directory. Since CScope files
        contain paths relative to the kernel root, it can be used in the
        target directory in case it contains the same directory structure
        as this kernel does.
        :param target_dir: Target directory.
        """
        shutil.copy(os.path.join(self.kernel_dir, "cscope.files"), target_dir)
        shutil.copy(os.path.join(self.kernel_dir, "cscope.in.out"), target_dir)
        shutil.copy(os.path.join(self.kernel_dir, "cscope.out"), target_dir)
        shutil.copy(os.path.join(self.kernel_dir, "cscope.po.out"), target_dir)
