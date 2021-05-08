"""
Regression test using pytest.
Tests are specified using YAML files in the tests/test_specs directory.
This module runs tests for differences in slicing of functions using the
"slices" key in the YAML spec file.
"""
from diffkemp.semdiff.function_diff import functions_diff
from .task_spec import SliceSpec, specs_path, tasks_path
import difflib
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
                # Parse only the "slices" key. Each slices entry gets a single
                # TaskSpec object.
                if "slices" in spec_yaml:
                    for s in spec_yaml["slices"]:
                        spec = SliceSpec(
                            spec=spec_yaml,
                            task_name=s["function"],
                            tasks_path=tasks_path,
                            kernel_path=cwd)

                        spec.add_function_spec(s["function"], "not_equal")
                        spec.add_slice_spec(s["function"], s["slice_old"],
                                            s["slice_new"])

                        spec_id = "{}_{}".format(spec_file_name, s["function"])
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


def test_slice_diff(task_spec):
    """
    Test correctness of the produced slices.
    The model slices are stored in the sliced_functions folder. After running
    SimpLL we extract the specified function where we need to process the
    LLVM IR due to LLVM version-specific differences. Then we are comparing the
    model slice with the produced slice.
    """
    task_spec.config.output_llvm_ir = True
    task_spec.config.equivalence_slicer = True
    for fun_spec in task_spec.functions.values():
        result = functions_diff(
            mod_first=fun_spec.old_module,
            mod_second=fun_spec.new_module,
            fun_first=fun_spec.name, fun_second=fun_spec.name,
            glob_var=None, config=task_spec.config)
        assert result.kind == fun_spec.result

        for fun, fun_result in result.inner.items():
            if fun in task_spec.functions:
                slice_spec = task_spec.slicing_functions[fun]
                first_simpl = result.first.filename
                second_simpl = result.second.filename

                # Assign for each produced slice the corresponding model one
                slices = {first_simpl.llvm: slice_spec.slice_old,
                          second_simpl.llvm: slice_spec.slice_new}
                for llvm in slices:
                    # Temporary file for storing the produced slice
                    produced_fun = open(llvm[:-8] + "sliced.ll", "w+")
                    # Removing debug symbol info
                    os.system("opt -S -strip-debug -o {} {}".format(llvm, llvm)
                              )
                    found_fun = False
                    for line in open(llvm).readlines():
                        if "define" in line and "@" + fun + "(" in line:
                            # Found the required function
                            # Removing version-specific differences from header
                            print(line, end='')
                            line = re.sub(r'dso_local ', '', line)
                            line = re.sub(r' %\d', '', line)
                            found_fun = True
                        if found_fun:
                            print(line, end='')
                            # Removing version-specific differences
                            if "; <label>:" in line:
                                line = re.sub(r'; <label>:', '', line)
                                line = re.sub(r'(\d:)(\s+)', r"\1          \2",
                                              line)
                            line = re.sub(r', !tbaa !\d+', '', line)
                            line = re.sub(r', !prof !\d+', '', line)
                            line = re.sub(r', !misexpect !\d+', '', line)
                            line = re.sub(r', !llvm.loop !\d+', '', line)
                            line = re.sub(r', !idx_align_1 !\d+', '', line)
                            produced_fun.write(line)
                            if "}" in line:
                                # End of the function
                                break
                    produced_fun.close()
                    produced_fun = open(llvm[:-8] + "sliced.ll", "r"). \
                        readlines()
                    model_fun = open(slices[llvm]).readlines()
                    diff = False
                    for line in difflib.unified_diff(model_fun, produced_fun):
                        if line:
                            print(line)
                            diff = True
                    if diff:
                        assert False
                    # Remove *-simpl.ll and *-sliced.ll files
                    os.remove(llvm[:-8] + "sliced.ll")
                    os.remove(llvm)
