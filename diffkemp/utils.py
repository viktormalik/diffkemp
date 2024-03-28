import os
import subprocess
import re

LLVM_FUNCTION_REGEX = re.compile(r"^define.*@(\w+)\(", flags=re.MULTILINE)


def get_simpll_build_dir():
    """
    Return the current SimpLL build directory as specified
    in the `SIMPLL_BUILD_DIR` environment variable.
    """
    build_dir_var = "SIMPLL_BUILD_DIR"
    if build_dir_var in os.environ:
        return os.environ[build_dir_var]
    return "build"


def get_llvm_version():
    """
    Return the current LLVM major version number.
    """
    return int(subprocess.check_output(
        ["llvm-config", "--version"]).decode().rstrip().split(".")[0])


def get_opt_command(passes, llvm_file, overwrite=True):
    """
    Return a command for running the LLVM optimizer with the given passes.
    The `passes` argument is a list of tuples `(pass_name, IR_unit)`.
    """
    opt_command = ["opt", llvm_file]
    if get_llvm_version() >= 16:
        # The new PM expects passes as "-passes=function(pass1),module(pass2)"
        passes_formatted = map(lambda p: f"{p[1]}({p[0]})", passes)
        opt_command.append("-passes=" + ",".join(passes_formatted))
    else:
        # The legacy PM expects passes as "-pass1 -pass2"
        pass_names = map(lambda p: p[0], passes)
        opt_command.extend(map(lambda pass_name: f"-{pass_name}", pass_names))
    if overwrite:
        opt_command.extend(["-S", "-o", llvm_file])
    return opt_command


class EndLineNotFound(Exception):
    """
    Error to inform that end line of function / type was not found.
    """
    pass


def get_end_line(filename, start, kind):
    """
    Get number of line where function/type/macro ends.
    Can raise UnicodeDecodeError, EndLineError.
    :param kind: One of "type", "function", "macro".
    """
    is_end_map = {
        "function": lambda line: line in ["}", ");"],
        "type": lambda line: line in ["};"],
        "macro": lambda line: line[-1] != "\\"
    }
    if kind not in is_end_map:
        raise ValueError("Error: get_end_line expects kind to be" +
                         f"one of type/function/macro, received {kind}")
    is_end = is_end_map[kind]
    with open(filename, "r", encoding='utf-8') as file:
        lines = file.readlines()

        line_number = start
        line = lines[line_number - 1]
        while not is_end(line.rstrip()):
            line_number += 1
            if line_number > len(lines):
                raise EndLineNotFound
            line = lines[line_number - 1]
        return line_number


def get_functions_from_llvm(llvm_files):
    """
    Reads LLVM IR files, gets all LLVM functions that satisfy C naming from
    there.
    Returns dict with keys being names of found functions and values
    files there were found in.
    :param llvm_files: List of LLVM IR files.
    """
    functions = {}
    for llvm_filename in llvm_files:
        if not os.path.exists(llvm_filename):
            os.stderr.write(
                f"Warning: llvm file '{llvm_filename}' does not exist\n")
            continue
        with open(llvm_filename, 'r') as llvm_file:
            llvm_file_content = llvm_file.read()
            matches = LLVM_FUNCTION_REGEX.findall(llvm_file_content)
            for match in matches:
                functions[match] = llvm_filename
    return functions
