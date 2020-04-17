"""
Source tree of the Linux kernel.
"""
from diffkemp.llvm_ir.source_tree import SourceTree
from diffkemp.llvm_ir.llvm_source_finder import SourceNotFoundException
from diffkemp.llvm_ir.llvm_sysctl_module import LlvmSysctlModule
import os


class KernelSourceTree(SourceTree):
    """
    Class representing a kernel source tree. Extends the general source tree
    by implementing methods to get LLVM modules containing definitions of
    sysctl options or kernel modules.
    """
    def __init__(self, source_dir, source_finder_cls=None,
                 source_finder_path=None):
        SourceTree.__init__(self, source_dir, source_finder_cls,
                            source_finder_path)

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
            table = "sysctl_base_table"
        elif entries[0] == "net":
            if entries[1] == "ipv4":
                if entries[2] == "conf":
                    table = "devinet_sysctl.1"
                    entries = entries[4:]
                else:
                    table = "ipv4_table"
                    entries = entries[2:]
            elif entries[1] == "core":
                table = "net_core_table"
                entries = entries[2:]
            else:
                raise SourceNotFoundException(sysctl)
        else:
            raise SourceNotFoundException(sysctl)

        for (i, entry) in enumerate(entries):
            # Build the file normally and then create a corresponding
            # LlvmSysctlModule with the obtained sysctl table.
            table_var = table.split(".")[0]
            kernel_mod = self.get_module_for_symbol(table_var)
            sysctl_mod = LlvmSysctlModule(kernel_mod, table)

            if i == len(entries) - 1:
                return sysctl_mod
            table = sysctl_mod.get_child(entry).name
        raise SourceNotFoundException(sysctl)

    def get_kernel_module(self, mod_dir, mod_name):
        if self.source_finder is None:
            raise SourceNotFoundException(mod_name)

        source = self.source_finder.find_llvm_for_kernel_module(mod_dir,
                                                                mod_name)
        if source is None or not os.path.isfile(source):
            raise SourceNotFoundException(mod_name)

        return self._get_module_from_source(source)
