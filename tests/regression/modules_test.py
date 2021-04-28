"""
Regression test using pytest.
Tests are specified using YAML files in the tests/test_specs directory.
This module runs tests for kernel module parameters specified using
the "modules" key in the YAML spec file.
"""
from diffkemp.semdiff.function_diff import functions_diff
from diffkemp.semdiff.result import Result
from .task_spec import ModuleParamSpec, patterns_path, specs_path, tasks_path
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
                # Parse only the "modules" key. Each parameter of each module
                # gets a single ModuleParamSpec object.
                if "modules" in spec_yaml:
                    for module in spec_yaml["modules"]:
                        for param in module["params"]:
                            spec = ModuleParamSpec(
                                spec=spec_yaml,
                                dir=module["dir"],
                                mod=module["mod"],
                                param=param["name"],
                                tasks_path=tasks_path,
                                kernel_path=cwd,
                                patterns_path=patterns_path
                            )

                            for fun, res in param["functions"].items():
                                spec.add_function_spec(fun, res)
                            spec_id = "{}_{}_{}".format(spec_file_name,
                                                        module["mod"],
                                                        param["name"])
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
    spec.build_module()
    spec.prepare_dir(
        old_module=spec.old_module,
        old_src=None,
        new_module=spec.new_module,
        new_src=None)
    for fun in spec.functions.values():
        if fun.result == Result.Kind.NONE:
            continue
    yield spec
    spec.finalize()


class TestClassModule(object):
    """
    Tests for semantic comparison of kernel module parameters.
    For each parameter, two tests are run.
    """
    def test_functions(self, task_spec):
        """Test getting the list of functions using the paramter."""
        old_funs = task_spec.old_module.get_functions_using_param(
            task_spec.get_param())
        new_funs = task_spec.new_module.get_functions_using_param(
            task_spec.get_param())
        assert old_funs == set(task_spec.functions.keys())
        assert new_funs == set(task_spec.functions.keys())

    def test_functions_diff(self, task_spec):
        """Test comparison of semantic difference of individual functions."""
        for fun_spec in task_spec.functions.values():
            if fun_spec.result not in [Result.Kind.TIMEOUT, Result.Kind.NONE]:
                result = functions_diff(
                    mod_first=task_spec.old_module,
                    mod_second=task_spec.new_module,
                    fun_first=fun_spec.name, fun_second=fun_spec.name,
                    glob_var=task_spec.get_param(), config=task_spec.config)
                assert result.kind == fun_spec.result
