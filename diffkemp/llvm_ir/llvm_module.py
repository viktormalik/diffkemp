"""
LLVM IR modules.
Functions for working with modules and parameters in them.
"""

from llvmcpy.llvm import *
import os
from subprocess import check_call, CalledProcessError

# List of standard functions that are supported, so they should not be
# included in function collecting.
# Some functions have multiple variants so we need to check for prefix
supported_names = ["llvm.dbg.value", "llvm.dbg.declare"]
supported_prefixes = ["llvm.lifetime", "kmalloc", "kzalloc", "malloc",
                      "calloc", "__kmalloc", "devm_kzalloc"]


def supported_fun(llvm_fun):
    """Check whether the function is supported."""
    name = llvm_fun.get_name().decode("utf-8")
    if name:
        return (name in supported_names or
                any([name.startswith(p) for p in supported_prefixes]))


class LlvmParam:
    """
    A parameter represented by a (global) variable and an optional list of
    field indices in case the parameter is a member of a composite data type.
    The indices correspond to the indices used in LLVM GEP instruction to get
    the address of the particular element within the given variable.
    """
    def __init__(self, name, indices=None):
        self.name = name
        self.indices = indices

    def __str__(self):
        return self.name


class LlvmModule:
    """
    Representation of a module in LLVM IR
    """
    def __init__(self, llvm_file, source_file=None):
        self.llvm = llvm_file
        self.source = source_file
        self.llvm_module = None
        self.unlinked_llvm = None
        self.linked_modules = set()

    def parse_module(self, force=False):
        """Parse module file into LLVM module using llvmcpy library"""
        if force or self.llvm_module is None:
            buffer = create_memory_buffer_with_contents_of_file(self.llvm)
            context = get_global_context()
            self.llvm_module = context.parse_ir(buffer)

    def clean_module(self):
        """Free the parsed LLVM module."""
        if self.llvm_module is not None:
            self.llvm_module.dispose()
            self.llvm_module = None

    @staticmethod
    def clean_all():
        """Clean all statically managed LLVM memory."""
        shutdown()

    def link_modules(self, modules):
        """Link module against a list of other modules."""
        link_llvm_modules = [m for m in modules if not self.links_mod(m)]

        if not link_llvm_modules:
            return False

        if "-linked" not in self.llvm:
            new_llvm = "{}-linked.ll".format(self.llvm[:-3])
        else:
            new_llvm = self.llvm
        link_command = ["llvm-link", "-S", self.llvm]
        link_command.extend([m.llvm for m in link_llvm_modules])
        link_command.extend(["-o", new_llvm])
        opt_command = ["opt", "-S", "-constmerge", new_llvm, "-o", new_llvm]
        with open(os.devnull, "w") as devnull:
            try:
                check_call(link_command, stdout=devnull, stderr=devnull)
                check_call(opt_command, stdout=devnull, stderr=devnull)
                if self.unlinked_llvm is None:
                    self.unlinked_llvm = self.llvm
                self.llvm = new_llvm
                self.linked_modules.update(link_llvm_modules)
                self.parse_module(True)
            except CalledProcessError:
                return False
            finally:
                return any([m for m in link_llvm_modules if self.links_mod(m)])

    def _extract_param_name(self, param_var):
        """
        Extract name of the global variable representing a module parameter
        from the structure describing it.
        :param param_var: LLVM expression containing the variable
        :return Name of the variable
        """
        if param_var.get_kind() == GlobalVariableValueKind:
            param_name = param_var.get_name().decode("utf-8")
            if ("__param_arr" in param_name or
                    "__param_string" in param_name):
                # For array and string parameters, the actual variable is
                # inside another structure as its last element
                param_var_value = param_var.get_initializer()
                # Get the last element
                var_expr = param_var_value.get_operand(
                    param_var_value.get_num_operands() - 1)
                return self._extract_param_name(var_expr)
            else:
                return param_name

        if (param_var.get_kind() == ConstantExprValueKind and
                param_var.get_num_operands() >= 1):
            # Variable can be inside bitcast or getelementptr, in both cases
            # it is inside the first operand of the expression
            return self._extract_param_name(param_var.get_operand(0))

        return None

    def find_param_var(self, param):
        """
        Find global variable in the module that corresponds to the given param.
        In case the param is defined by module_param_named, this can be
        different from the param name.
        Information about the variable is stored inside the last element of the
        structure assigned to the '__param_#name' variable (#name is the name
        of param).
        :param param Parameter name
        :return Name of the global variable corresponding to the parameter
        """
        self.parse_module()
        glob = self.llvm_module.get_named_global("__param_{}".format(param))
        if not glob:
            return None
        # Get value of __param_#name variable
        glob_value = glob.get_initializer()
        # Get the last element
        var_union = glob_value.get_operand(glob_value.get_num_operands() - 1)
        if var_union.get_num_operands() == 1:
            # Last element should be a struct with single element, get it
            var = var_union.get_operand(0)
            return LlvmParam(self._extract_param_name(var), [])
        return None

    def has_function(self, fun):
        """Check if module contains a function definition."""
        pattern = re.compile(r"^define.*@{}\(".format(fun), flags=re.MULTILINE)
        with open(self.llvm, "r") as llvm_file:
            return pattern.search(llvm_file.read()) is not None

    def has_global(self, glob):
        """Check if module contains a global variable with the given name."""
        pattern = re.compile(r"^@{}\s*=".format(glob), flags=re.MULTILINE)
        with open(self.llvm, "r") as llvm_file:
            return pattern.search(llvm_file.read()) is not None

    def is_declaration(self, fun):
        """
        Check if the given function is a declaration (does not have body).
        """
        self.parse_module()
        llvm_fun = self.llvm_module.get_named_function(fun)
        return (llvm_fun.get_kind() == FunctionValueKind and
                llvm_fun.is_declaration())

    def links_mod(self, mod):
        """
        Check if the given module has been linked to this module
        """
        return mod in self.linked_modules

    def restore_unlinked_llvm(self):
        """Restore the module to the state before any linking was done."""
        if self.unlinked_llvm is not None:
            self.llvm = self.unlinked_llvm
            self.unlinked_llvm = None
            self.linked_modules = set()
            self.parse_module(True)

    def move_to_other_root_dir(self, old_root, new_root):
        """
        Move this LLVM module into a different project root directory.
        :param old_root: Project root directory to move from.
        :param new_root: Project root directory to move to.
        :return:
        """
        if self.llvm.startswith(old_root):
            dest_llvm = os.path.join(new_root,
                                     os.path.relpath(self.llvm, old_root))
            # Copy the .ll file and replace all occurrences of the old root by
            # the new root. There are usually in debug info.
            with open(self.llvm, "r") as llvm:
                with open(dest_llvm, "w") as llvm_new:
                    for line in llvm.readlines():
                        if "constant" not in line:
                            llvm_new.write(line.replace(old_root.strip("/"),
                                                        new_root.strip("/")))
                        else:
                            llvm_new.write(line)
            self.llvm = dest_llvm

        if self.source and self.source.startswith(old_root):
            # Copy the C source file.
            dest_source = os.path.join(new_root,
                                       os.path.relpath(self.source, old_root))
            shutil.copyfile(self.source, dest_source)
            self.source = dest_source

    def get_included_sources(self):
        """
        Get the list of source files that this module includes.
        Requires debugging information.
        """
        # Search for all .h files mentioned in the debug info.
        pattern = re.compile(r"filename:\s*\"([^\"]*)\", "
                             r"directory:\s*\"([^\"]*)\"")
        result = set()
        with open(self.llvm, "r") as llvm:
            for line in llvm.readlines():
                s = pattern.search(line)
                if (s and (s.group(1).endswith(".h") or
                           s.group(1).endswith(".c"))):
                    result.add(os.path.join(s.group(2), s.group(1)))
        return result

    @staticmethod
    def _check_gep_indices_correspond(gep, indices):
        """
        Checks whether the indices in the GEP correspond to the indices in
        the list. When one list is longer than the other, the function behaves
        like one is cut to the size of the other and compares the rest.
        :param gep: The GEP operator. Both the instruction and the constant
        expression are supported.
        :param indices: A list of integers representing indices to compare the
        GEP operator with.
        :return: True or false based on whether the indices correspond.
        """
        for i in range(1, gep.get_num_operands()):
            if (i - 1) >= len(indices):
                break
            op = gep.get_operand(i)
            if (op.is_constant() and
                    op.const_int_get_z_ext() != indices[i - 1]):
                return False
        return True

    def get_functions_using_param(self, param):
        """
        Find names of all functions using the given parameter (global variable)
        """
        self.parse_module()
        glob = self.llvm_module.get_named_global(param.name)
        if not glob:
            return set()
        result = set()
        for use in glob.iter_uses():
            if use.user.get_kind() == InstructionValueKind:
                # Use is an Instruction
                bb = use.user.instruction_parent
                func = bb.parent
                if (use.user.is_a_get_element_ptr_inst() and
                        param.indices is not None):
                    # Look whether the GEP references the desired index or not
                    if not self._check_gep_indices_correspond(use.user,
                                                              param.indices):
                        continue
                if ".old" not in func.name.decode("utf-8"):
                    result.add(func.name.decode("utf-8"))
            elif use.user.get_kind() == ConstantExprValueKind:
                # Use is an Operator (typically GEP)
                for inner_use in use.user.iter_uses():
                    if inner_use.user.get_kind() == InstructionValueKind:
                        bb = inner_use.user.instruction_parent
                        func = bb.parent
                        if (use.user.get_const_opcode() ==
                                Opcode['GetElementPtr'] and
                                param.indices is not None):
                            # Look whether the GEP references the desired
                            # index or not
                            if not self._check_gep_indices_correspond(
                                    use.user, param.indices):
                                continue
                        if ".old" not in func.name.decode("utf-8"):
                            result.add(func.name.decode("utf-8"))
        return result

    @staticmethod
    def _get_functions_called_by_rec(llvm_fun, result):
        """
        Find names of all functions that are (recursively) called by the given
        function. Found names are stored in the 'result' list.
        """
        for bb in llvm_fun.iter_basic_blocks():
            for instr in bb.iter_instructions():
                if instr.get_instruction_opcode() == Call:
                    called = instr.get_called()
                    if called.get_kind() == ConstantExprValueKind:
                        # Called function may be inside a bitcast
                        called = called.get_operand(0)
                    called_name = called.get_name().decode("utf-8")
                    # Collect all unsupported functions that are called
                    if (called.get_kind() == FunctionValueKind and
                            called_name and
                            not supported_fun(called) and
                            called_name not in result):
                        result.add(called_name)
                        # Recursively call the method on the called function
                        LlvmModule._get_functions_called_by_rec(
                            called, result)

                # Collect also functions that are passed as parameters to
                # instructions. For these, do not descend recursively since
                # their body will not be analysed.
                for opIndex in range(0, instr.get_num_operands()):
                    op = instr.get_operand(opIndex)

                    op_name = op.get_name().decode("utf-8")
                    if (op.get_kind() == FunctionValueKind and
                            op_name and
                            not supported_fun(op) and
                            op_name not in result):
                        result.add(op_name)

    def get_functions_called_by(self, fun_name):
        """
        Find names of all functions (recursively) called by one of functions
        in the given set.
        """
        result = set()
        self.parse_module()
        llvm_fun = self.llvm_module.get_named_function(fun_name)
        if llvm_fun:
            self._get_functions_called_by_rec(llvm_fun, result)
        return result
