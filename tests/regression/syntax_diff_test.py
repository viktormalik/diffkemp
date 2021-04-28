"""
Regression test using pytest.
Tests are specified using YAML files in the tests/test_specs directory.
This module runs tests for syntax differences using the "syntax_diffs" key in
the YAML spec file.
"""
from diffkemp.semdiff.function_diff import functions_diff
from diffkemp.semdiff.result import Result
from .task_spec import SyntaxDiffSpec, patterns_path, specs_path, tasks_path
import glob
import os
import pytest
import re
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
                # Parse only the "syntax_diffs" key. Each syntax_diffs entry
                # gets a single TaskSpec object.
                if "syntax_diffs" in spec_yaml:
                    for syntax_diff in spec_yaml["syntax_diffs"]:
                        spec = SyntaxDiffSpec(
                            spec=spec_yaml,
                            task_name=syntax_diff["diff_symbol"],
                            tasks_path=tasks_path,
                            kernel_path=cwd,
                            patterns_path=patterns_path)

                        spec.add_function_spec(syntax_diff["function"],
                                               "not_equal")

                        if ("result" in syntax_diff
                                and Result.Kind[syntax_diff["result"].upper()]
                                == Result.Kind.EQUAL_SYNTAX):
                            spec.add_equal_diff_spec(
                                syntax_diff["diff_symbol"]
                            )
                        else:
                            spec.add_syntax_diff_spec(
                                syntax_diff["diff_symbol"],
                                syntax_diff["def_old"],
                                syntax_diff["def_new"]
                            )

                        spec_id = "{}_{}".format(spec_file_name,
                                                 syntax_diff["diff_symbol"])
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


def test_syntax_diff(task_spec):
    """
    Test correctness of the obtained syntax diff.
    The expected difference is obtained by concatenation of symbol
    definitions in the compared versions, which is the way that DiffKemp uses
    for displaying diffs of macros and inline assemblies.
    Function differences are displayed using the diff utility and this test
    cannot be currently used to verify them.
    """
    for fun_spec in task_spec.functions.values():
        result = functions_diff(
            mod_first=fun_spec.old_module,
            mod_second=fun_spec.new_module,
            fun_first=fun_spec.name, fun_second=fun_spec.name,
            glob_var=None, config=task_spec.config)
        assert result.kind == fun_spec.result

        # Ensure that equal diffs are not present
        for symbol in task_spec.equal_diffs:
            assert symbol not in result.inner.keys()

        for symbol, symbol_result in result.inner.items():
            if symbol in task_spec.syntax_diffs:
                diff_spec = task_spec.syntax_diffs[symbol]
                # Compare the obtained diff with the expected one, omitting
                # all whitespace
                actual_diff = re.sub(r"\s+", "", symbol_result.diff)
                expected_diff = re.sub(r"\s+", "",
                                       diff_spec.def_old + diff_spec.def_new)
                assert actual_diff == expected_diff
