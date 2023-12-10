"""
Building kernel sources into LLVM IR.
"""
from diffkemp.llvm_ir.compiler import get_clang_default_options
from diffkemp.llvm_ir.llvm_module import LlvmModule
from diffkemp.llvm_ir.llvm_source_finder import LlvmSourceFinder, \
    SourceNotFoundException
from diffkemp.llvm_ir.optimiser import opt_llvm, BuildException
import os
from subprocess import check_call, check_output, CalledProcessError


class KernelLlvmSourceBuilder(LlvmSourceFinder):
    """
    Class for finding LLVM modules containing definitions and declarations of
    symbols. Uses CScope to find the correct C source file and then compiles
    it into LLVM IR.
    Extends the SourceFinder abstract class.
    """
    def __init__(self, source_dir):
        LlvmSourceFinder.__init__(self, source_dir)
        self.cscope_cache = dict()
        # Compiler headers (containing 'asm goto' constructions)
        self.compiler_headers = [os.path.join(self.source_dir, h) for h in
                                 ["include/linux/compiler-gcc.h",
                                  "include/linux/compiler_types.h"]]
        # Prepare kernel so that it can be built with Clang
        self.initialize()
        # Caching built modules to reuse them
        self.built_modules = dict()

    ######################################################################
    # Implementation of methods from the LlvmSourceFinder abstract class #
    ######################################################################

    def str(self):
        return "kernel_with_builder"

    def clone_to_dir(self, new_source_dir):
        return KernelLlvmSourceBuilder(new_source_dir)

    def initialize(self):
        """
        Prepare kernel so that it can be compiled into LLVM IR.
        """
        self._disable_asm_features()

    def finalize(self):
        """Restore the kernel state."""
        self._enable_asm_features()

    def find_llvm_with_symbol_def(self, symbol):
        """
        Looks up files containing definition of a symbol using CScope, then
        transforms them into LLVM IR files and looks whether the symbol is
        actually defined in the created file.
        In case there are multiple files containing the definition, the first
        module containing the function definition is returned.
        :param symbol: Name of the symbol (function or global var) to look up.
        :returns LLVM IR file containing the specified symbol.
        """
        llvm_filename = None

        srcs = self._find_srcs_with_symbol_def(symbol)
        for src in srcs:
            try:
                source_path = os.path.join(self.source_dir, src)
                llvm_filename = self._build_source_to_llvm(source_path)
                if os.path.isfile(llvm_filename):
                    mod = LlvmModule(llvm_filename)
                    if mod.has_function(symbol) or mod.has_global(symbol):
                        break
            except BuildException:
                pass
            llvm_filename = None

        return llvm_filename

    def find_llvm_with_symbol_use(self, symbol):
        """
        Use CScope to find sources using a symbol and build them into LLVM IR.
        :param symbol: Symbol to find.
        :return Set of source files containing functions that use the symbol.
        """
        cwd = os.getcwd()
        os.chdir(self.source_dir)
        try:
            cscope_out = self._cscope_run(symbol, definition=False)
            if len(cscope_out) == 0:
                raise SourceNotFoundException
            files = set()
            for line in cscope_out:
                if line.split()[0].endswith(".h"):
                    continue
                if line.split()[1] == "<global>":
                    continue
                try:
                    llvm_filename = self._build_source_to_llvm(line.split()[0])
                    if os.path.isfile(llvm_filename):
                        files.add(os.path.join(self.source_dir, llvm_filename))
                except BuildException:
                    pass
            return files
        except SourceNotFoundException:
            raise
        finally:
            os.chdir(cwd)

    def find_llvm_for_kernel_module(self, mod_dir, mod_name):
        """
        Build kernel module into LLVM IR and return the produced file.
        The build must be run every time since the target file name is
        determined during the build.
        :param mod_dir:  Directory of the module.
        :param mod_name: Name of the module.
        :return: LLVM IR file containing the whole module.
        """
        llvm_file = self._build_kernel_mod_to_llvm(mod_dir, mod_name)
        return os.path.join(self.source_dir, llvm_file)

    def _disable_asm_features(self):
        """
        Disable asm features that are not supported by older versions of LLVM.
        - transform 'asm goto(x)' command into 'asm("goto(x)")'
        - disable usage of 'asm inline'
        """
        for header in self.compiler_headers:
            if not os.path.isfile(header):
                continue
            commands = [
                ["sed", "-i", "s/asm goto(x)/asm (\"goto(\" #x \")\")/g",
                 header],
                ["sed", "-i",
                 "/#ifdef CONFIG_CC_HAS_ASM_INLINE/i "
                 "#undef CONFIG_CC_HAS_ASM_INLINE \\/\\/ DiffKemp generated",
                 header],
            ]
            try:
                with open(os.devnull, "w") as devnull:
                    for command in commands:
                        check_call(command, stderr=devnull)
            except CalledProcessError:
                pass

    def _enable_asm_features(self):
        """Restore the original 'asm goto' and 'asm inline' semantics."""
        for header in self.compiler_headers:
            if not os.path.isfile(header):
                continue
            commands = [
                ["sed", "-i", "s/asm (\"goto(\" #x \")\")/asm goto(x)/g",
                 header],
                ["sed", "-i",
                 "/#undef CONFIG_CC_HAS_ASM_INLINE "
                 "\\/\\/ DiffKemp generated/d",
                 header],
            ]
            try:
                with open(os.devnull, "w") as devnull:
                    for command in commands:
                        check_call(command, stderr=devnull)
            except CalledProcessError:
                pass

    ###################################################
    # Methods for finding C source files using CScope #
    ###################################################
    def _build_cscope_database(self):
        """
        Build a database for the cscope tool. It will be later used to find
        source files with symbol definitions.
        """
        cscope_path = os.path.join(self.source_dir, "cscope.files")

        # If the database exists, do not rebuild it
        if os.path.isfile(cscope_path):
            return

        # Write all files that need to be scanned into cscope.files
        with open(cscope_path, "w") as cscope_file:
            for root, dirs, files in os.walk(self.source_dir):
                if ("/Documentation/" in root or
                        "/scripts/" in root or
                        "/tmp" in root):
                    continue

                for f in files:
                    if os.path.islink(os.path.join(root, f)):
                        continue
                    if f.endswith((".c", ".h", ".x", ".s", ".S")):
                        path = os.path.relpath(os.path.join(root, f),
                                               self.source_dir)
                        cscope_file.write("{}\n".format(path))

        # Build cscope database
        try:
            check_call(["cscope", "-b", "-q", "-k"], cwd=self.source_dir)
        except CalledProcessError:
            os.remove(cscope_path)
            raise BuildException("Error building cscope database")

    def _cscope_run(self, symbol, definition):
        """
        Run cscope search for a symbol.
        :param symbol: Symbol to search for
        :param definition: If true, search definitions, otherwise search all
                           usage.
        :return: List of found cscope entries.
        """
        cached = self.cscope_cache.get((symbol, definition))
        if cached is not None:
            return cached

        self._build_cscope_database()
        try:
            command = ["cscope", "-d", "-L", "-1" if definition else "-0",
                       symbol]
            with open(os.devnull, "w") as devnull:
                cscope_output = check_output(command, stderr=devnull).decode(
                    'utf-8')
            result = [line for line in cscope_output.splitlines() if
                      line.split()[0].endswith("c")]
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
        return [c for c in candidates if
                c.endswith("(" + macro_argument + ");")]

    def _find_srcs_with_symbol_def(self, symbol):
        """
        Use cscope to find a definition of the given symbol.
        :param symbol: Symbol to find.
        :return List of source files potentially containing the definition.
        """
        cwd = os.getcwd()
        os.chdir(self.source_dir)
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

    #################################################
    # Methods for building C source files into LLVM #
    #################################################
    @staticmethod
    def _strip_bash_quotes(gcc_param):
        """
        Remove quotes from gcc_param that represents a part of a shell command.
        """
        if "\'" in gcc_param:
            return gcc_param.replace("\'", "")
        else:
            return gcc_param.replace("\"", "")

    @staticmethod
    def _gcc_to_llvm(gcc_command):
        """
        Convert GCC command to corresponding Clang command for compiling source
        into LLVM IR.
        :param gcc_command: GCC command to convert.
        :return Corresponding Clang command.
        """
        output_file = None
        command = ["clang"]
        command.extend(get_clang_default_options())
        for param in gcc_command.split():
            if (param == "gcc" or
                    (param.startswith("-W") and "-MD" not in param) or
                    param.startswith("-f") or
                    param.startswith("-m") or
                    param.startswith("-O") or
                    param == "-DCC_HAVE_ASM_GOTO" or
                    param == "-g" or
                    param == "-o" or
                    param.endswith(".o")):
                continue

            # Do not use generated debug hashes.
            # Note: they are used for debugging purposes only and cause false
            # positives in SimpLL.
            if param.startswith('-D"DEBUG_HASH='):
                param = '-D"DEBUG_HASH=1"'
            if param.startswith('-D"DEBUG_HASH2='):
                param = '-D"DEBUG_HASH2=1"'

            # Output name is given by replacing .c by .ll in source name
            if param.endswith(".c"):
                output_file = "{}.ll".format(param[:-2])

            command.append(KernelLlvmSourceBuilder._strip_bash_quotes(param))
        if output_file is None:
            raise BuildException("Build error: gcc command not compiling C \
                                 source found")
        command.extend(["-o", output_file])
        return command

    @staticmethod
    def _ld_to_llvm(ld_command):
        """
        Convert ld command into llvm-link command to link multiple LLVM IR
        files into one file.
        :param ld_command: Command to convert
        :return Corresponding llvm-link command.
        """
        command = ["llvm-link", "-S"]
        for param in ld_command.split():
            if param.endswith(".o"):
                command.append("{}.ll".format(param[:-2]))
            elif param == "-o":
                command.append(param)
        return command

    @staticmethod
    def _kbuild_to_llvm_commands(commands, module_name):
        llvm_commands = []
        for c in commands:
            command = c.lstrip()
            if (command.startswith("gcc") and
                    "{}.mod".format(module_name) not in command):
                llvm_commands.append(
                    KernelLlvmSourceBuilder._gcc_to_llvm(command))
            elif (command.startswith("ld") and
                  "{}.ko".format(module_name) not in command):
                llvm_commands.append(
                    KernelLlvmSourceBuilder._ld_to_llvm(command))
        return llvm_commands

    @staticmethod
    def _clean_object(obj):
        """Clean an object file"""
        if os.path.isfile(obj):
            os.remove(obj)

    @staticmethod
    def _extract_command(command, programs):
        """
        Extract a single command running one of the specified programs from
        a list of commands separated by ;.
        """
        for c in command.split(";"):
            if any([c.lstrip().startswith(prog) for prog in programs]):
                return c.lstrip()
        return None

    @staticmethod
    def _extract_gcc_command(command):
        """
        Extract a single gcc command from a list of commands separated by ;.
        """
        return KernelLlvmSourceBuilder._extract_command(command, ["gcc"])

    @staticmethod
    def _extract_gcc_or_ld_command(command):
        """
        Extract a single gcc or ld command from a list of commands separated
        by ;.
        """
        return KernelLlvmSourceBuilder._extract_command(command, ["gcc", "ld"])

    @staticmethod
    def _extract_gcc_or_ld_command_list(commands):
        """Extract all gcc and ld commands from a list of commands."""
        result = []
        for c in commands:
            command = KernelLlvmSourceBuilder._extract_gcc_or_ld_command(c)
            if command:
                result.append(command)
        return result

    @staticmethod
    def _get_build_object(command):
        """Get name of the object file built by the command."""
        return command[command.index("-o") + 1]

    @staticmethod
    def _get_build_source(command):
        """Get name of the object file built by the command."""
        return command[command.index("-c") + 1]

    def _kbuild_object_command(self, object_file):
        """
        Check which command would be run by KBuild to build the given object
        file.
        The command used is `make V=1 --just-print path/to/object.o`
        :returns GCC command used for the compilation. This is the last
                 command starting with 'gcc' that was run by make
        """
        cwd = os.getcwd()
        os.chdir(self.source_dir)
        self._clean_object(object_file)
        with open(os.devnull, "w") as stderr:
            try:
                output = check_output(
                    ["make", "V=1",
                     "CFLAGS=-w", "EXTRA_CFLAGS=-w",
                     object_file,
                     "--just-print"], stderr=stderr).decode("utf-8")
            except CalledProcessError:
                raise BuildException("Error compiling {}".format(object_file))
            finally:
                os.chdir(cwd)

        for c in reversed(output.splitlines()):
            command = self._extract_gcc_command(c)
            if command:
                return command
        raise BuildException("Compiling {} did not run a gcc command".format(
            object_file))

    def _kbuild_module_commands(self, mod_dir, mod_name):
        """
        Build a kernel module using Kbuild.
        The command used is `make V=1 M=/path/to/mod module.ko`
        First, tries to build module with the given name, next tries to replace
        '_' by '-'.
        :returns Name used in the .ko file (possibly with underscores replaced
                 by dashes).
                 List of commands that were used to compile and link files in
                 the module.
        """
        cwd = os.getcwd()
        os.chdir(self.source_dir)

        file_name = mod_name
        command = ["make", "--just-print", "V=1", "M={}".format(mod_dir),
                   "{}.ko".format(mod_name)]

        try:
            output = check_output(command).decode("utf-8")
            return file_name, \
                self._extract_gcc_or_ld_command_list(output.splitlines())
        except CalledProcessError as e:
            if e.returncode == 2:
                # If the target does not exist, replace "_" by "-" and try
                # again.
                file_name = file_name.replace("_", "-")
                command[4] = "{}.ko".format(file_name)
                try:
                    output = check_output(command).decode("utf-8")
                    return file_name, self._extract_gcc_or_ld_command_list(
                        output.splitlines())
                except CalledProcessError:
                    raise BuildException(
                        "Could not build module {}".format(mod_name))
                finally:
                    os.chdir(cwd)
        finally:
            os.chdir(cwd)
        raise BuildException("Could not build module {}".format(mod_name))

    def _build_source_to_llvm(self, source_file):
        """
        Build C source file into LLVM IR.
        Gets the Kbuild command that is used for building an object file,
        transforms it into the corresponding Clang command, and runs it.
        :param source_file: C source to build
        :return: Created LLVM IR file
        """
        llvm_file = "{}.ll".format(source_file[:-2])
        if (not os.path.isfile(llvm_file) or os.path.getmtime(llvm_file) <
                os.path.getmtime(source_file)):
            cwd = os.getcwd()
            os.chdir(self.source_dir)

            name = os.path.relpath(source_file, self.source_dir)[:-2]
            try:
                # Get GCC command for building the .o file
                command = self._kbuild_object_command("{}.o".format(name))
                # Convert the GCC command to a corresponding Clang command
                command = self._gcc_to_llvm(command)
                # Run the Clang command
                with open(os.devnull, "w") as stderr:
                    try:
                        check_call(command, stderr=stderr)
                    except CalledProcessError:
                        os.chdir(cwd)
                        raise BuildException(
                            "Could not build {}".format(llvm_file))
                # Run opt with selected optimisations
                opt_llvm(llvm_file)
            except BuildException:
                raise
            finally:
                os.chdir(cwd)
        return llvm_file

    def _build_kernel_mod_to_llvm(self, mod_dir, mod_name):
        """
        Build a kernel module into LLVM IR.
        This is done by retrieving commands that would be run by KBuild to
        build the given module, transforming them into corresponding clang/LLVM
        commands, and running them.
        :param mod_dir: Kernel module directory.
        :param mod_name: Kernel module name.
        :return: Name of the LLVM IR file built.
        """
        cwd = os.getcwd()
        os.chdir(self.source_dir)
        try:
            file_name, gcc_commands = self._kbuild_module_commands(mod_dir,
                                                                   mod_name)
            llvm_commands = self._kbuild_to_llvm_commands(gcc_commands,
                                                          file_name)
            with open(os.devnull, "w") as stderr:
                built = False
                for c in llvm_commands:
                    if c[0] == "clang":
                        src = self._get_build_source(c)
                        obj = self._get_build_object(c)
                        if (not os.path.isfile(obj) or
                                os.path.getmtime(obj) < os.path.getmtime(src)):
                            built = True
                            check_call(c, stderr=stderr)
                    elif c[0] == "llvm-link":
                        obj = self._get_build_object(c)
                        if not os.path.isfile(obj) or built:
                            check_call(c, stderr=stderr)
            llvm_file = os.path.join(mod_dir, "{}.ll".format(file_name))
            opt_llvm(llvm_file)
            return llvm_file
        except CalledProcessError:
            raise BuildException("Could not build {}".format(mod_name))
        except BuildException:
            raise
        finally:
            os.chdir(cwd)
