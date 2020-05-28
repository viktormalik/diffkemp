"""
Unit tests for building kernel sources into LLVM IR.
Mostly tests for the KernelLlvmSourceBuilder class.
"""

from diffkemp.llvm_ir.kernel_llvm_source_builder import \
    KernelLlvmSourceBuilder, BuildException
from diffkemp.llvm_ir.source_tree import SourceNotFoundException
import pytest
import os


versions = ("kernel/linux-3.10", "kernel/linux-3.10.0-957.el7")


@pytest.fixture
def builder(request):
    """
    Create kernel source builder that is shared among tests.
    Parametrized by kernel directory.
    """
    b = KernelLlvmSourceBuilder(request.param)
    yield b
    b.finalize()


@pytest.mark.parametrize("kernel_dir", versions)
def test_create_kernel(kernel_dir):
    """Creating kernel source builder."""
    builder = KernelLlvmSourceBuilder(kernel_dir)
    assert builder.source_dir == os.path.join(os.getcwd(), kernel_dir)
    assert builder.built_modules == dict()
    # Check that "asm goto" has been disabled
    with open(os.path.join(kernel_dir, "include/linux/compiler-gcc.h"),
              "r") as gcc_header:
        assert "asm goto(x)" not in gcc_header.read()


@pytest.mark.parametrize("builder", versions, indirect=True)
def test_find_llvm_with_symbol_def(builder):
    """
    Test building LLVM module from a source containing a function definition.
    """
    llvm_file = builder.find_llvm_with_symbol_def("__alloc_workqueue_key")
    assert llvm_file == os.path.join(builder.source_dir, "kernel/workqueue.ll")


@pytest.mark.parametrize("builder", versions, indirect=True)
def test_find_llvm_with_symbol_use(builder):
    """Test finding sources using a global variable."""
    srcs = builder.find_llvm_with_symbol_use("net_ratelimit_state")
    assert srcs == {os.path.join(builder.source_dir, "net/core/utils.ll")}


@pytest.mark.parametrize("builder", versions, indirect=True)
def test_build_cscope_database(builder):
    """Test building CScope database."""
    builder._build_cscope_database()
    for file in ["cscope.files", "cscope.in.out", "cscope.out",
                 "cscope.po.out"]:
        assert os.path.isfile(os.path.join(builder.source_dir, file))


@pytest.mark.parametrize("builder", versions, indirect=True)
def test_find_srcs_with_symbol_def(builder):
    """Test finding sources with function definition."""
    srcs = builder._find_srcs_with_symbol_def("ipmi_set_gets_events")
    assert sorted(srcs) == sorted([
        "drivers/char/ipmi/ipmi_msghandler.c",
        "drivers/char/ipmi/ipmi_devintf.c"
    ])


@pytest.mark.parametrize("builder", versions, indirect=True)
def test_find_srcs_with_symbol_def_fail(builder):
    """Test finding sources with definition of a non-existing function."""
    with pytest.raises(SourceNotFoundException):
        builder._find_srcs_with_symbol_def("nonexisting_function")


@pytest.mark.parametrize("builder", versions, indirect=True)
def test_kbuild_object_command(builder):
    """Finding which command is used to build an object file."""
    command = builder._kbuild_object_command("sound/core/sound.o")
    assert command.startswith("gcc")


@pytest.mark.parametrize("builder", versions, indirect=True)
def test_kbuild_module_commands(builder):
    """Test finding name and commands for building a kernel module."""
    file_name, commands = builder._kbuild_module_commands("drivers/firewire",
                                                          "firewire-sbp2")
    assert file_name == "firewire-sbp2"
    assert commands
    for c in commands:
        assert c.startswith("gcc") or c.startswith("ld")


@pytest.mark.parametrize("builder", versions, indirect=True)
def test_build_src_to_llvm(builder):
    """Building single object into LLVM."""
    llvm_file = builder._build_source_to_llvm("sound/core/init.c")
    assert (llvm_file == "sound/core/init.ll")
    assert os.path.isfile(os.path.join(builder.source_dir, llvm_file))


@pytest.mark.parametrize("builder", versions, indirect=True)
def test_build_src_to_llvm_fail(builder):
    """Try to build a file that has no source."""
    with pytest.raises(BuildException):
        builder._build_source_to_llvm("sound/core/snd.c")


@pytest.mark.parametrize("builder", versions, indirect=True)
def test_build_mod_to_llvm(builder):
    """Test building kernel module into LLVM"""
    mod_file = os.path.join(builder.source_dir,
                            "drivers/firewire/firewire-sbp2.ll")
    if os.path.isfile(mod_file):
        os.unlink(mod_file)
    builder._build_kernel_mod_to_llvm("drivers/firewire", "firewire-sbp2")
    assert os.path.isfile(mod_file)


@pytest.mark.parametrize("builder", versions, indirect=True)
def test_build_mod_fail(builder):
    """Test building non-existing kernel module into LLVM"""
    with pytest.raises(BuildException):
        builder._build_kernel_mod_to_llvm("drivers/firewire", "firewire")


def test_finalize():
    """Testing destructor of LlvmKernelBuilder."""
    builder = KernelLlvmSourceBuilder("kernel/linux-3.10.0-957.el7")
    gcc_header_path = os.path.join(builder.source_dir,
                                   "include/linux/compiler-gcc.h")
    builder.finalize()
    # Check that "asm goto" has been re-enabled.
    with open(gcc_header_path, "r") as gcc_header:
        assert "asm goto(x)" in gcc_header.read()
