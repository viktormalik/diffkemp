"""
Regression test using pytest.
Tests are specified using YAML files in the tests/test_specs directory.
This module runs tests for single functions specified using the "functions"
key in the YAML spec file.
"""
from diffkemp.semdiff.function_diff import functions_diff
from diffkemp.semdiff.result import Result
from .task_spec import TaskSpec, patterns_path, specs_path, tasks_path
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
                # Parse only the "functions" key. Each function gets a single
                # TaskSpec object.
                if "functions" in spec_yaml:
                    for fun, res in spec_yaml["functions"].items():
                        spec = TaskSpec(
                            spec=spec_yaml,
                            task_name=fun,
                            tasks_path=tasks_path,
                            kernel_path=cwd,
                            patterns_path=patterns_path)

                        spec.add_function_spec(fun, res)

                        spec_id = "{}_{}".format(spec_file_name, fun)
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
    for fun in spec.functions:
        mod_old, mod_new = spec.build_modules_for_function(fun)
        spec.prepare_dir(
            old_module=mod_old,
            old_src="{}.c".format(mod_old.llvm[:-3]),
            new_module=mod_new,
            new_src="{}.c".format(mod_new.llvm[:-3]))
    yield spec
    spec.finalize()


def test_function_diff(task_spec):
    """Test comparison of semantic difference of functions."""
    for fun_spec in task_spec.functions.values():
        if fun_spec.result != Result.Kind.TIMEOUT:
            result = functions_diff(
                mod_first=fun_spec.old_module,
                mod_second=fun_spec.new_module,
                fun_first=fun_spec.name, fun_second=fun_spec.name,
                glob_var=None, config=task_spec.config)
            assert result.kind == fun_spec.result
