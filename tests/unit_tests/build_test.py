"""Unit tests for 'build' sub-command."""
from diffkemp.cli import run_from_cli
from diffkemp.utils import get_functions_from_llvm
from tempfile import mkdtemp
import os
import pytest
import shutil
import yaml

SINGLE_C_FILE = os.path.abspath("tests/testing_projects/make_based/file.c")
MAKE_BASED_PROJECT_DIR = os.path.abspath("tests/testing_projects/make_based")


@pytest.mark.parametrize("source",
                         [SINGLE_C_FILE, MAKE_BASED_PROJECT_DIR],
                         ids=["single c file", "make-based project"])
def test_build_command(monkeypatch, source):
    """Testing 'build' sub-command for single c file and make-based project."""
    output_dir = mkdtemp()
    # Running diffkemp build command
    args = ["bin/diffkemp", "build", source, output_dir]
    monkeypatch.setattr("sys.argv", args)
    run_from_cli()

    # Snapshot should contain source file, LLVM file and file with metadata.
    output_files = os.listdir(output_dir)
    assert "file.c" in output_files
    assert "file.ll" in output_files
    assert "snapshot.yaml" in output_files

    # Functions from the source file should be also in the llvm file.
    llvm_file_path = os.path.join(output_dir, "file.ll")
    llvm_fun_list = get_functions_from_llvm([llvm_file_path])
    assert "add" in llvm_fun_list
    assert "mul" in llvm_fun_list

    # Function names should be in snapshot.yaml
    snapshot_yaml_path = os.path.join(output_dir, "snapshot.yaml")
    with open(snapshot_yaml_path, "r") as file:
        snapshot_yaml = yaml.safe_load(file)
    function_list = [function["name"] for function in snapshot_yaml[0]["list"]]
    assert "add" in function_list
    assert "mul" in function_list

    # Removing the temporary directory.
    shutil.rmtree(output_dir)
