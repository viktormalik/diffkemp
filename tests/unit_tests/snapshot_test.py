"""Unit tests for the Snapshot class."""

from diffkemp.snapshot import Snapshot
from diffkemp.llvm_ir.kernel_module import LlvmKernelModule
from diffkemp.llvm_ir.kernel_source import KernelSource
from tempfile import NamedTemporaryFile
from tempfile import TemporaryDirectory
import datetime
import os
import yaml


def test_create_snapshot_from_source():
    """Create a new kernel directory snapshot."""
    kernel_dir = "kernel/linux-3.10.0-957.el7"
    output_dir = "snapshots/linux-3.10.0-957.el7"
    snap = Snapshot.create_from_source(kernel_dir, output_dir,
                                       None, False)

    assert snap.kernel_source is not None
    assert kernel_dir in snap.kernel_source.kernel_dir
    assert snap.snapshot_source is not None
    assert output_dir in snap.snapshot_source.kernel_dir
    assert snap.fun_kind is None
    assert len(snap.fun_groups) == 1
    assert len(snap.fun_groups[None].functions) == 0

    snap = Snapshot.create_from_source(kernel_dir, output_dir,
                                       "sysctl", False)

    assert snap.fun_kind == "sysctl"
    assert len(snap.fun_groups) == 0


def test_load_snapshot_from_dir_functions():
    """
    Create a temporary snapshot directory and try to parse it. Use a YAML
    configuration file that contains only a list of functions. Expect that
    the function list inside the parsed snapshot contains a single "None"
    group which contains the list of loaded functions. All parsed LLVM paths
    should contain the list root dir.
    """
    with TemporaryDirectory(prefix="test_snapshots_") as snap_dir, \
            NamedTemporaryFile(mode="w+t", prefix="snapshot_", suffix=".yaml",
                               dir=snap_dir) as config_file:
        # Populate the temporary snapshot configuration file.
        config_file.writelines("""
        - created_time: 2020-01-01 00:00:00.000001+00:00
          diffkemp_version: '0.1'
          kind: function_list
          list:
          - glob_var: null
            glob_var_value: null
            llvm: net/core/skbuff.ll
            name: ___pskb_trim
            tag: null
          - glob_var: null
            glob_var_value: null
            llvm: mm/page_alloc.ll
            name: __alloc_pages_nodemask
            tag: null
          source_kernel_dir: /diffkemp/kernel/linux-3.10.0-957.el7
        """)

        # Load the temporary snapshot configuration file.
        config_file.seek(0)
        config_filename = os.path.basename(config_file.name)
        snap = Snapshot.load_from_dir(snap_dir, config_filename)

        assert str(snap.created_time) == "2020-01-01 00:00:00.000001+00:00"
        assert isinstance(snap.snapshot_source, KernelSource)
        assert snap.snapshot_source.kernel_dir == snap_dir
        assert len(snap.fun_groups) == 1
        assert None in snap.fun_groups
        assert len(snap.fun_groups[None].functions) == 2
        assert set(snap.fun_groups[None].functions.keys()) == \
            {"___pskb_trim",
             "__alloc_pages_nodemask"}

        for name, f in snap.fun_groups[None].functions.items():
            assert f.glob_var is None
            assert f.tag is None
            if name == "___pskb_trim":
                assert os.path.abspath(f.mod.llvm) == snap_dir + \
                       "/net/core/skbuff.ll"
            elif name == "__alloc_pages_nodemask":
                assert os.path.abspath(f.mod.llvm) == snap_dir + \
                       "/mm/page_alloc.ll"


def test_load_snapshot_from_dir_sysctls():
    """
    Create a temporary snapshot directory and try to parse it. Use a YAML
    configuration file that contains a list of sysctl groups, each group
    containing a single function. All parsed LLVM paths should contain the list
    root dir.
    """
    with TemporaryDirectory(prefix="test_snapshots_sysctl_") as snap_dir, \
            NamedTemporaryFile(mode="w+t", prefix="snapshot_", suffix=".yaml",
                               dir=snap_dir) as config_file:
        # Populate the temporary sysctl snapshot configuration file.
        config_file.writelines("""
        - created_time: 2020-01-01 00:00:00.000001+00:00
          diffkemp_version: '0.1'
          kind: function_list
          list:
          - functions:
            - glob_var: null
              glob_var_value: null
              llvm: kernel/sched/fair.ll
              name: sched_proc_update_handler
              tag: proc handler
            sysctl: kernel.sched_latency_ns
          - functions:
            - glob_var: null
              glob_var_value: null
              llvm: kernel/sysctl.ll
              name: proc_dointvec_minmax
              tag: proc handler
            sysctl: kernel.timer_migration
          source_kernel_dir: /diffkemp/kernel/linux-3.10.0-957.el7
        """)

        # Load the temporary sysctl snapshot configuration file.
        config_file.seek(0)
        config_filename = os.path.basename(config_file.name)
        snap = Snapshot.load_from_dir(snap_dir, config_filename)

        assert str(snap.created_time) == "2020-01-01 00:00:00.000001+00:00"
        assert len(snap.fun_groups) == 2
        assert set(snap.fun_groups.keys()) == {"kernel.sched_latency_ns",
                                               "kernel.timer_migration"}

        for name, g in snap.fun_groups.items():
            f = None
            assert len(g.functions) == 1
            if name == "kernel.sched_latency_ns":
                assert g.functions.keys() == {"sched_proc_update_handler"}
                f = g.functions["sched_proc_update_handler"]
                assert os.path.abspath(f.mod.llvm) == snap_dir + \
                    "/kernel/sched/fair.ll"
            elif name == "kernel.timer_migration":
                assert g.functions.keys() == {"proc_dointvec_minmax"}
                f = g.functions["proc_dointvec_minmax"]
                assert os.path.abspath(f.mod.llvm) == snap_dir + \
                    "/kernel/sysctl.ll"
            assert f.tag == "proc handler"
            assert f.glob_var is None


def test_add_sysctl_fun_group():
    """Create a snapshot and check the creation of a sysctl function group."""

    kernel_dir = "kernel/linux-3.10.0-957.el7"
    output_dir = "snapshots/linux-3.10.0-957.el7"
    snap = Snapshot.create_from_source(kernel_dir, output_dir,
                                       "sysctl", False)

    snap.add_fun_group("kernel.sched_latency_ns")

    assert len(snap.fun_groups) == 1
    assert "kernel.sched_latency_ns" in snap.fun_groups


def test_add_fun_none_group():
    """Create a snapshot and try to add functions into a None group."""
    kernel_dir = "kernel/linux-3.10.0-957.el7"
    output_dir = "snapshots/linux-3.10.0-957.el7"
    snap = Snapshot.create_from_source(kernel_dir, output_dir,
                                       None, False)

    mod = LlvmKernelModule("net/core/skbuff.ll")
    snap.add_fun("___pskb_trim", mod)

    assert "___pskb_trim" in snap.fun_groups[None].functions
    fun_desc = snap.fun_groups[None].functions["___pskb_trim"]
    assert fun_desc.mod is mod
    assert fun_desc.glob_var is None
    assert fun_desc.tag is None


def test_add_fun_sysctl_group():
    """Create a snapshot and try to add functions into sysctl groups."""
    kernel_dir = "kernel/linux-3.10.0-957.el7"
    output_dir = "snapshots/linux-3.10.0-957.el7"
    snap = Snapshot.create_from_source(kernel_dir, output_dir,
                                       "sysctl", False)

    snap.add_fun_group("kernel.sched_latency_ns")
    mod = LlvmKernelModule("kernel/sched/debug.ll")
    snap.add_fun(
        "sched_debug_header",
        mod,
        glob_var="sysctl_sched_latency",
        tag="using_data_variable \"sysctl_sched_latency\"",
        group="kernel.sched_latency_ns"
    )

    assert "sched_debug_header" in snap.fun_groups[
        "kernel.sched_latency_ns"].functions
    fun_desc = snap.fun_groups["kernel.sched_latency_ns"].functions[
        "sched_debug_header"]
    assert fun_desc.mod is mod
    assert fun_desc.glob_var == "sysctl_sched_latency"
    assert fun_desc.tag == "using_data_variable \"sysctl_sched_latency\""


def test_get_modules():
    """
    Test getting all modules in the snapshot function lists.
    Check if the snapshot returns a list of all modules of all groups in case
    that multiple groups are present.
    """
    kernel_dir = "kernel/linux-3.10.0-957.el7"
    output_dir = "snapshots/linux-3.10.0-957.el7"
    snap = Snapshot.create_from_source(kernel_dir, output_dir,
                                       "sysctl", False)

    snap.add_fun_group("kernel.sched_latency_ns")
    snap.add_fun_group("kernel.timer_migration")
    snap.add_fun(
        "sched_proc_update_handler",
        LlvmKernelModule("kernel/sched/fair.ll"),
        glob_var=None,
        tag="proc_handler",
        group="kernel.sched_latency_ns"
    )
    snap.add_fun(
        "proc_dointvec_minmax",
        LlvmKernelModule("kernel/sysctl.ll"),
        glob_var=None,
        tag="proc_handler",
        group="kernel.timer_migration"
    )

    modules = snap.modules()
    assert len(modules) == 2
    assert set([m.llvm for m in modules]) == {"kernel/sched/fair.ll",
                                              "kernel/sysctl.ll"}


def test_get_by_name_functions():
    """Get the module of inserted function by its name."""
    kernel_dir = "kernel/linux-3.10.0-957.el7"
    output_dir = "snapshots/linux-3.10.0-957.el7"
    snap = Snapshot.create_from_source(kernel_dir, output_dir,
                                       None, False)

    mod_buff = LlvmKernelModule("net/core/skbuff.ll")
    mod_alloc = LlvmKernelModule("mm/page_alloc.ll")
    snap.add_fun("___pskb_trim", mod_buff)
    snap.add_fun("__alloc_pages_nodemask", mod_alloc)

    fun = snap.get_by_name("___pskb_trim")
    assert fun.mod is mod_buff
    fun = snap.get_by_name("__alloc_pages_nodemask")
    assert fun.mod is mod_alloc


def test_get_by_name_sysctls():
    """Get the module of inserted function by its name and sysctl group."""
    kernel_dir = "kernel/linux-3.10.0-957.el7"
    output_dir = "snapshots/linux-3.10.0-957.el7"
    snap = Snapshot.create_from_source(kernel_dir, output_dir,
                                       "sysctl", False)

    snap.add_fun_group("kernel.sched_latency_ns")
    snap.add_fun_group("kernel.timer_migration")
    mod_fair = LlvmKernelModule(
        "snapshots-sysctl/linux-3.10.0-957.el7/kernel/sched/fair.ll")
    mod_sysctl = LlvmKernelModule(
        "snapshots-sysctl/linux-3.10.0-957.el7/kernel/sysctl.ll")
    snap.add_fun(
        "sched_proc_update_handler",
        mod_fair,
        glob_var=None,
        tag="proc handler",
        group="kernel.sched_latency_ns"
    )
    snap.add_fun(
        "proc_dointvec_minmax",
        mod_sysctl,
        glob_var=None,
        tag="proc handler",
        group="kernel.timer_migration"
    )

    # Test that the function
    fun = snap.get_by_name("proc_dointvec_minmax", "kernel.sched_latency_ns")
    assert fun is None
    fun = snap.get_by_name("proc_dointvec_minmax", "kernel.timer_migration")
    assert fun.mod is mod_sysctl


def test_filter():
    """Filter snapshot functions."""
    kernel_dir = "kernel/linux-3.10.0-957.el7"
    output_dir = "snapshots/linux-3.10.0-957.el7"
    snap = Snapshot.create_from_source(kernel_dir, output_dir,
                                       None, False)

    snap.add_fun("___pskb_trim", LlvmKernelModule("net/core/skbuff.ll"))
    snap.add_fun("__alloc_pages_nodemask",
                 LlvmKernelModule("mm/page_alloc.ll"))

    snap.filter(["__alloc_pages_nodemask"])
    assert len(snap.fun_groups[None].functions) == 1
    assert "___pskb_trim" not in snap.fun_groups[None].functions
    assert "__alloc_pages_nodemask" in snap.fun_groups[None].functions


def test_to_yaml_functions():
    """
    Dump a snapshot with a single "None" group into YAML.
    YAML string should contain the version of Diffkemp, source kernel
    directory, a simple list of functions, each one having the "name",
    "llvm", "glob_var" and "tag" fields set, and the kind of this list,
    which should be a function list.
    The LLVM paths in the YAML should be relative to the snapshot directory.
    """
    kernel_dir = "kernel/linux-3.10.0-957.el7"
    output_dir = "snapshots/linux-3.10.0-957.el7"
    snap = Snapshot.create_from_source(kernel_dir, output_dir,
                                       None, False)

    snap.add_fun("___pskb_trim", LlvmKernelModule(
        "snapshots/linux-3.10.0-957.el7/net/core/skbuff.ll"))
    snap.add_fun("__alloc_pages_nodemask", LlvmKernelModule(
        "snapshots/linux-3.10.0-957.el7/mm/page_alloc.ll"))

    yaml_str = snap.to_yaml()
    yaml_snap = yaml.safe_load(yaml_str)

    assert len(yaml_snap) == 1
    yaml_dict = yaml_snap[0]
    assert len(yaml_dict) == 5
    assert isinstance(yaml_dict["created_time"], datetime.datetime)
    assert len(yaml_dict["list"]) == 2
    assert set([f["name"] for f in yaml_dict["list"]]) ==\
        {"___pskb_trim",
         "__alloc_pages_nodemask"}

    for f in yaml_dict["list"]:
        if f["name"] == "___pskb_trim":
            assert f["llvm"] == "net/core/skbuff.ll"
        elif f["name"] == "__alloc_pages_nodemask":
            assert f["llvm"] == "mm/page_alloc.ll"


def test_to_yaml_sysctls():
    """
    Dump a snapshot with multiple sysctl groups into YAML.
    YAML string should contain the version of Diffkemp, source kernel
    directory, a simple list of function groups, each one containing a
    function list with the "name", "llvm", "glob_var" and "tag" fields set,
    and the kind of this list, which should be a group list.
    The LLVM paths in the YAML should be relative to the snapshot directory.
    """
    kernel_dir = "kernel/linux-3.10.0-957.el7"
    output_dir = "snapshots-sysctl/linux-3.10.0-957.el7"
    snap = Snapshot.create_from_source(kernel_dir, output_dir,
                                       "sysctl", False)

    snap.add_fun_group("kernel.sched_latency_ns")
    snap.add_fun_group("kernel.timer_migration")
    snap.add_fun(
        "sched_proc_update_handler",
        LlvmKernelModule(
            "snapshots-sysctl/linux-3.10.0-957.el7/kernel/sched/fair.ll"),
        glob_var=None,
        tag="proc handler",
        group="kernel.sched_latency_ns"
    )
    snap.add_fun(
        "proc_dointvec_minmax",
        LlvmKernelModule(
            "snapshots-sysctl/linux-3.10.0-957.el7/kernel/sysctl.ll"),
        glob_var=None,
        tag="proc handler",
        group="kernel.timer_migration"
    )

    yaml_str = snap.to_yaml()
    yaml_snap = yaml.safe_load(yaml_str)
    assert len(yaml_snap) == 1
    yaml_dict = yaml_snap[0]
    assert len(yaml_dict) == 5
    assert isinstance(yaml_dict["created_time"], datetime.datetime)
    assert len(yaml_dict["list"]) == 2
    assert set([g["sysctl"] for g in yaml_dict["list"]]) == {
        "kernel.sched_latency_ns",
        "kernel.timer_migration"}

    for g in yaml_dict["list"]:
        assert len(g["functions"]) == 1
        if g["sysctl"] == "kernel.sched_latency_ns":
            assert g["functions"][0] == {
                "name": "sched_proc_update_handler",
                "llvm": "kernel/sched/fair.ll",
                "glob_var": None,
                "glob_var_value": None,
                "tag": "proc handler"
            }
        elif g["sysctl"] == "kernel.timer_migration":
            assert g["functions"][0] == {
                "name": "proc_dointvec_minmax",
                "llvm": "kernel/sysctl.ll",
                "glob_var": None,
                "glob_var_value": None,
                "tag": "proc handler"
            }
