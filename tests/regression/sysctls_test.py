"""
Regression test using pytest.
Tests are specified using YAML files in the tests/test_specs directory.
This module runs tests for kernel sysctl options specified using the "sysctls"
key in the YAML spec file.
"""
from diffkemp.semdiff.function_diff import functions_diff
from diffkemp.semdiff.result import Result
from .task_spec import SysctlTaskSpec, patterns_path, specs_path, tasks_path
import glob
import os
import pytest
import yaml


def collect_task_specs():
    """Collecting and parsing YAML files with test specifications."""
    result = list()
    if not os.path.isdir(tasks_path):
        os.mkdir(tasks_path)
    cwd = os.getcwd()
    os.chdir(specs_path)
    for spec_file_path in glob.glob("*.yaml"):
        with open(spec_file_path, "r") as spec_file:
            try:
                spec_yaml = yaml.safe_load(spec_file)
                spec_file_name = os.path.splitext(spec_file_path)[0]
                if "disabled" in spec_yaml and spec_yaml["disabled"] is True:
                    continue

                # Parse only the "sysctls" key. Each sysctl option gets
                # a single SysctlTaskSpec object.
                if "sysctls" in spec_yaml:
                    for sysctl in spec_yaml["sysctls"]:
                        spec = SysctlTaskSpec(
                            spec=spec_yaml,
                            task_name=sysctl["sysctl"],
                            tasks_path=tasks_path,
                            kernel_path=cwd,
                            patterns_path=patterns_path,
                            data_var=sysctl["data_variable"]["name"])

                        (proc_h, proc_h_res), = sysctl["proc_handler"].items()
                        spec.add_proc_handler(proc_h, proc_h_res)

                        for data_fun, res in \
                                sysctl["data_variable"]["functions"].items():
                            spec.add_data_var_function(data_fun, res)

                        spec_id = spec_file_name + "_" + sysctl["sysctl"]
                        result.append((spec_id, spec))
            except yaml.YAMLError:
                pass
    os.chdir(cwd)
    return result


specs = collect_task_specs()


@pytest.fixture(params=[x[1] for x in specs],
                ids=[x[0] for x in specs])
def task_spec(request):
    """pytest fixture to prepare tasks"""
    spec = request.param
    spec.build_sysctl_module()
    for fun in spec.functions.values():
        if fun.result == Result.Kind.NONE:
            continue
        mod_old, mod_new = spec.build_modules_for_function(fun.name)
        spec.prepare_dir(
            old_module=mod_old,
            old_src="{}.c".format(mod_old.llvm[:-3]),
            new_module=mod_new,
            new_src="{}.c".format(mod_new.llvm[:-3]),
            name=fun.name)
    yield spec
    spec.finalize()


class TestClassSysctl(object):
    """
    Tests for semantic comparison of sysctl options.
    For each sysctl option, three tests are run:
     - finding the correct proc handler and data variable
     - semantic diff of proc handler function
     - semantic diff of function using the data variable
    """
    def test_llvm_sysctl_module(self, task_spec):
        """
        Tests whether the data variable and the proc handler function are
        determined correctly.
        """
        data = task_spec.old_sysctl_module.get_data(
            task_spec.name)
        proc_handler = task_spec.old_sysctl_module.get_proc_fun(
            task_spec.name)

        # Data variable function
        if "." in task_spec.data_var:
            # Structure attribute
            assert task_spec.data_var.split(".")[0] == data.name
        else:
            # Standard variable
            assert task_spec.data_var == data.name
        # Proc handler function
        assert task_spec.proc_handler == proc_handler

    def test_proc_handler(self, task_spec):
        """
        Test comparison of semantic difference of the proc_handler function.
        """
        proc_handler = task_spec.get_proc_handler_spec()
        if proc_handler.result not in [Result.Kind.TIMEOUT, Result.Kind.NONE]:
            result = functions_diff(
                mod_first=proc_handler.old_module,
                mod_second=proc_handler.new_module,
                fun_first=proc_handler.name,
                fun_second=proc_handler.name,
                glob_var=None, config=task_spec.config)

            assert result.kind == proc_handler.result

    def test_data_functions(self, task_spec):
        """
        Test comparison of semantic difference of functions using the data
        variable associated with the sysctl parameter.
        """
        # Get the data variable KernelParam object
        data_kernel_param = task_spec.old_sysctl_module.get_data(
            task_spec.name)

        for fun, fun_spec in task_spec.functions.items():
            if fun == task_spec.proc_handler:
                continue
            if fun_spec.result != Result.Kind.TIMEOUT:
                result = functions_diff(mod_first=fun_spec.old_module,
                                        mod_second=fun_spec.new_module,
                                        fun_first=fun_spec.name,
                                        fun_second=fun_spec.name,
                                        glob_var=data_kernel_param,
                                        config=task_spec.config)
                assert result.kind == fun_spec.result
