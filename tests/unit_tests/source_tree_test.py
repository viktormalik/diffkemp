"""
Tests for representation of a project source tree.
Contains tests for the SourceTree class, mostly methods for getting LLVM IR
modules from a source tree.
"""

from diffkemp.llvm_ir.source_tree import SourceTree, \
    SourceNotFoundException
from diffkemp.llvm_ir.kernel_llvm_source_builder import KernelLlvmSourceBuilder
import datetime
import os
import pytest
import shutil
import tempfile


@pytest.fixture
def source():
    # If a new class extending LlvmSourceFinder is implemented, it should be
    # added here for testing that it provides correct LLVM IR files.
    s = SourceTree("kernel/linux-3.10.0-957.el7", KernelLlvmSourceBuilder)
    yield s
    s.finalize()


def test_get_module_for_symbol(source):
    """
    Test getting LLVM module with a function definition.
    """
    mod = source.get_module_for_symbol("__alloc_workqueue_key")
    assert mod is not None
    assert mod.llvm == os.path.join(source.source_dir, "kernel/workqueue.ll")
    assert mod.has_function("__alloc_workqueue_key")


def test_get_module_for_symbol_built_after(source):
    """
    Test getting LLVM module with a function definition when the module was
    modified after some specified time.
    The LLVM file should not be retrieved.
    """
    before_time = datetime.datetime.now() - datetime.timedelta(minutes=1)
    llvm_file = os.path.join(source.source_dir, "kernel/workqueue.ll")

    # Temporarily change mtime of the LLVM IR file to now
    stat = os.stat(llvm_file)
    prev_mtime = stat.st_mtime
    os.utime(llvm_file,
             times=(stat.st_atime, datetime.datetime.now().timestamp()))

    with pytest.raises(SourceNotFoundException):
        source.get_module_for_symbol("__alloc_workqueue_key",
                                     before_time.timestamp())

    # Restore the original mtime of the file
    os.utime(llvm_file, times=(stat.st_atime, prev_mtime))


def test_get_module_for_symbol_fail(source):
    """Test getting LLVM module for a non-existing function definition."""
    with pytest.raises(SourceNotFoundException):
        source.get_module_for_symbol("__get_user_2")


def test_get_modules_using_symbol(source):
    """
    Test getting LLVM modules containing functions using a global variable.
    """
    mods = source.get_modules_using_symbol("sysctl_wmem_max")
    assert len(mods) == 2
    assert set([m.llvm for m in mods]) == \
        set([os.path.join(source.source_dir, f)
             for f in ["net/core/sock.ll",
                       "net/netfilter/ipvs/ip_vs_sync.ll"]])


def test_copy_source_files(source):
    """Test copying source files into other root kernel directory."""
    m1 = source.get_module_for_symbol("net_ratelimit")
    m2 = source.get_module_for_symbol("__alloc_workqueue_key")
    tmp_source = SourceTree(tempfile.mkdtemp())
    source.copy_source_files([m1, m2], tmp_source)

    # Check that necessary directories were created.
    for d in ["net", "net/core", "kernel", "include", "include/linux"]:
        assert os.path.isdir(os.path.join(tmp_source.source_dir, d))

    # Check that files were successfully copied.
    for f in ["net/core/utils.c", "net/core/utils.ll", "kernel/workqueue.c",
              "kernel/workqueue.ll", "include/linux/module.h",
              "include/linux/kernel.h"]:
        assert os.path.isfile(os.path.join(tmp_source.source_dir, f))

    shutil.rmtree(tmp_source.source_dir)
