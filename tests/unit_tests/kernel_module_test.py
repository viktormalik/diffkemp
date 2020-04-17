"""
Unit tests for a kernel module in LLVM IR.
Testing LlvmKernelModule class located in llvm_ir/kernel_module.py.
"""

from diffkemp.llvm_ir.kernel_module import KernelParam, LlvmKernelModule
from diffkemp.llvm_ir.kernel_llvm_source_builder import KernelLlvmSourceBuilder
from diffkemp.llvm_ir.source_tree import SourceTree
import os
import pytest
import shutil
import tempfile


@pytest.fixture
def source():
    """Create KernelSource shared among multiple tests."""
    s = SourceTree("kernel/linux-3.10.0-957.el7", KernelLlvmSourceBuilder)
    yield s
    s.finalize()


@pytest.fixture
def mod(source):
    """Create kernel module shared among multiple tests."""
    return source.get_module_for_symbol("snd_request_card")


def test_has_function(mod):
    """Test checking if module contains function definition."""
    for f in ["snd_request_card", "kmalloc"]:
        assert mod.has_function(f)


def test_has_global(mod):
    """Test checking if module contains a global variable."""
    for g in ["param_ops_int", "major"]:
        assert mod.has_global(g)


def test_is_declaration(mod):
    """Test checking if module has a function declaration."""
    for f in ["snd_card_locked", "mutex_lock"]:
        assert mod.is_declaration(f)


def test_link_modules(source, mod):
    """
    Test linking modules.
    """
    # Get module for init.c
    init = source.get_module_for_symbol("snd_card_locked")

    # Check that a function defined in init.c is properly linked into core.c.
    assert not mod.has_function("snd_card_locked")
    mod.link_modules([init])
    assert mod.has_function("snd_card_locked")

    # Check links_mod and restore_unlinked_llvm methods.
    assert mod.links_mod(init)
    mod.restore_unlinked_llvm()
    assert not mod.links_mod(init)


def test_find_param_var():
    """
    Test finding the name of a variable corresponding to a module parameter.
    This is necessary since for parameters defined with module_param_named,
    names of the parameter and of the variable differ.
    """
    source = SourceTree("kernel/linux-3.10", KernelLlvmSourceBuilder)
    mod = source.get_module_for_symbol("rfkill_init")
    assert mod.find_param_var("default_state").name == "rfkill_default_state"


def test_move_to_other_root_dir(source):
    """Test moving the module into another root directory."""
    mod = source.get_module_for_symbol("snd_card_new")
    # Prepare target directory
    tmp = tempfile.mkdtemp()
    os.mkdir(os.path.join(tmp, "sound"))
    os.mkdir(os.path.join(tmp, "sound/core"))

    # Check that source (C and LLVM) files have been moved.
    mod.move_to_other_root_dir(os.path.abspath("kernel/linux-3.10.0-957.el7"),
                               tmp)
    assert os.path.isfile(os.path.join(tmp, "sound/core/init.ll"))

    # Check that the llvm file does not contain the original directory.
    assert mod.llvm == os.path.join(tmp, "sound/core/init.ll")
    with open(mod.llvm, "r") as llvm:
        for line in llvm.readlines():
            assert ("constant" in line or
                    "kernel/linux-3.10.0-957.el7" not in line)

    shutil.rmtree(tmp)


def test_get_included_headers(source):
    """Test finding the list of included source and header files."""
    llvm_file = source.source_finder._build_source_to_llvm(
        "arch/cris/arch-v10/lib/memset.c")
    mod = LlvmKernelModule(os.path.join(source.source_dir, llvm_file))
    sources = mod.get_included_sources()
    assert sources == set([os.path.join(source.source_dir, f) for f in [
        "arch/cris/arch-v10/lib/memset.c",
        "././include/linux/kconfig.h",
        "include/generated/autoconf.h"
    ]])


def test_get_functions_using_param(mod):
    """Test finding functions using a parameter (global variable)."""
    param = KernelParam("major")
    funs = mod.get_functions_using_param(param)
    assert funs == {"snd_register_device", "alsa_sound_init",
                    "alsa_sound_exit"}


def test_get_functions_using_param_with_index(source):
    """Test finding functions using a component of a parameter."""
    mod = source.get_module_for_symbol("net_core_table")
    param = KernelParam("netdev_rss_key", [0, 0])
    funs = mod.get_functions_using_param(param)
    assert funs == {"proc_do_rss_key"}


def test_get_functions_called_by(mod):
    """Test finding functions recursively called by a function."""
    funs = mod.get_functions_called_by("alsa_sound_exit")
    assert funs == {"snd_info_done", "unregister_chrdev",
                    "__unregister_chrdev"}
