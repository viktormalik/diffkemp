"""
LLVM module containing definitions of kernel sysctl options compiled into
LLVM IR. Contains reference to LlvmModule and additional sysctl-specific fields
and methods.
"""
from llvmcpy.llvm import *
from diffkemp.llvm_ir.llvm_module import LlvmParam


def matches(name, pattern):
    if pattern == "*":
        return True
    if pattern.startswith("{") and pattern.endswith("}"):
        match_list = pattern[1:-1].split("|")
        return name in match_list
    return name == pattern


class LlvmSysctlModule:
    # List of standard proc handler functions.
    standard_proc_funs = [
        "proc_dostring", "proc_dointvec", "proc_douintvec",
        "proc_dointvec_minmax", "proc_douintvec_minmax",
        "proc_dopipe_max_size", "proc_dointvec_jiffies",
        "proc_dointvec_userhz_jiffies", "proc_dointvec_ms_jiffies",
        "proc_dointvec_ms_jiffies", "proc_doulongvec_ms_jiffies_minmax",
        "proc_do_large_bitmap"
    ]

    @staticmethod
    def is_standard_proc_fun(proc_fun):
        """Check if the proc handler function is a standard one."""
        return proc_fun in LlvmSysctlModule.standard_proc_funs

    def __init__(self, kernel_mod, ctl_table):
        """
        :param kernel_mod: Reference to LlvmKernelModule with the compiled mod
        :param ctl_table: Name of global variable holding table with sysctl
                          option definitions.
        """
        self.mod = kernel_mod
        self.ctl_table = ctl_table
        self.sysctls = dict()

    def _get_sysctl(self, sysctl_name):
        """
        Get LLVM object (of type struct ctl_table) with the definition of
        the given sysctl option.
        """
        if sysctl_name not in self.sysctls:
            self.parse_sysctls(sysctl_name)
        self.mod.parse_module()
        return self.sysctls[sysctl_name]

    def parse_sysctls(self, sysctl_pattern):
        """
        Parse all sysctls entries that match the given pattern. Parsed entries
        are LLVM objects of type "struct ctl_table" containing the sysctl
        definition.
        They are stored in the dictionary self.sysctls.
        :return: List of names of parsed sysctls.
        """
        self.mod.parse_module()
        ctl_table = self.ctl_table.split(".")
        # sysctl table is a global variable
        table = self.mod.llvm_module.get_named_global(ctl_table[0])
        if not table:
            return []

        # Get global variable initializer. If sysctl_name contains some indices
        # follow them to get the actual table.
        sysctl_list = table.get_initializer()
        for i in ctl_table[1:]:
            sysctl_list = sysctl_list.get_operand(int(i))
        if not sysctl_list:
            return []

        names = []
        # Iterate all entries in the table
        for i in range(0, sysctl_list.get_num_operands()):
            sysctl = sysctl_list.get_operand(i)
            if sysctl.get_num_operands() == 0:
                continue
            # Sysctl option name is the first element of the entry
            # We need one more .get_operand(0) because of gep operator
            name = sysctl.get_operand(0).get_operand(0).get_initializer() \
                .get_as_string()
            if name.endswith("\x00"):
                name = name[:-1]

            pattern = sysctl_pattern.split(".")[-1]
            if matches(name, pattern):
                sysctl_name = sysctl_pattern.replace(pattern, name)
                self.sysctls[sysctl_name] = sysctl
                names.append(sysctl_name)
        return names

    def _get_global_variable_at_index(self, sysctl_name, index):
        """
        Find sysctl with given name and get its element at the given index.
        The element is expected to be a global variable.
        :param sysctl_name: Name of the sysctl to be found.
        :param index: Index to look for.
        :return: KernelParam object with the given global variable description.
        """
        # Get the sysctl entry
        sysctl = self._get_sysctl(sysctl_name)
        if not sysctl or sysctl.get_num_operands() <= index:
            return None
        # Get operand at the given index
        data = sysctl.get_operand(index)
        if data.is_null():
            return None
        indices = None

        # Address is a GEP, we have to extract the actual variable.
        if (data.get_kind() == ConstantExprValueKind and
                data.get_const_opcode() == Opcode['GetElementPtr']):
            all_constant = True
            indices = list()
            # Look whether are all indices constant.
            for i in range(1, data.get_num_operands()):
                indices.append(data.get_operand(i).const_int_get_z_ext())
                if not data.get_operand(i).is_constant():
                    all_constant = False
            if all_constant:
                data = data.get_operand(0)

        # Address is typed to "void *", we need to get rid of the bitcast
        # operator.
        if (data.get_kind() == ConstantExprValueKind and
                data.get_const_opcode() == Opcode['BitCast']):
            data = data.get_operand(0)

        if data.get_kind() == GlobalVariableValueKind:
            data = data.get_name().decode("utf-8")
            return LlvmParam(data, indices)
        return None

    def get_proc_fun(self, sysctl_name):
        """
        Get name of the proc handler function for the given sysctl option.
        """
        sysctl = self._get_sysctl(sysctl_name)
        if not sysctl or sysctl.get_num_operands() < 6:
            return None
        # Proc handler function is the 6th element of the
        # "struct ctl_table" type.
        proc_handler = sysctl.get_operand(5)
        return proc_handler.get_name().decode("utf-8") \
            if not proc_handler.is_null() else None

    def get_child(self, sysctl_name):
        """Get name of the child node of the given sysctl table entry."""
        return self._get_global_variable_at_index(sysctl_name, 4)

    def get_data(self, sysctl_name):
        """Get name of the data variable for the given sysctl option."""
        return self._get_global_variable_at_index(sysctl_name, 1)
