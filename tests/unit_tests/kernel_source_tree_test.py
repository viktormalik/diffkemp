"""
Unit tests for a Linux kernel source tree representation.
Testing the KernelSourceTree class.
"""
from diffkemp.llvm_ir.kernel_llvm_source_builder import KernelLlvmSourceBuilder
from diffkemp.llvm_ir.kernel_source_tree import KernelSourceTree
import os
import pytest


@pytest.fixture
def source():
    s = KernelSourceTree("kernel/linux-3.10.0-957.el7",
                         KernelLlvmSourceBuilder)
    yield s
    s.finalize()


@pytest.mark.parametrize("name, llvm_file, table", [
    ("net.core.message_burst",
     "net/core/sysctl_net_core.ll",
     "net_core_table"),
    ("kernel.usermodehelper.bset",
     "kernel/kmod.ll",
     "usermodehelper_table")
])
def test_get_sysctl_module(source, name, llvm_file, table):
    """
    Test building source files containing definitions of a sysctl option.
    """
    m = source.get_sysctl_module(name)
    assert m is not None
    assert m.mod is not None
    assert m.mod.llvm == os.path.join(source.source_dir, llvm_file)
    assert m.ctl_table == table
