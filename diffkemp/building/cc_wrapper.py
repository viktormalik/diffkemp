#!/usr/bin/env python
"""
Template for C compiler wrapper used in build stage of DiffKemp

It's written to be either run with RPython to generate a native code
executable, or with Python to modify itself into a Python script.
Because of that it has to run under both Python 2 and Python 3 and some of
Python library functions are redefined here to be RPython-compatible.
"""
# TODO: Decompose simultaneous compiling and linking to get individual .ll
#       files out of it; support other GCC languages than C (Fortran, C++)
from diffkemp.llvm_ir.compiler import get_clang_default_options
import os
import shutil
import sys


wrapper_env_vars = {
    "db_filename": "DIFFKEMP_WRAPPER_DB_FILENAME",
    "clang": "DIFFKEMP_WRAPPER_CLANG",
    "clang_append": "DIFFKEMP_WRAPPER_CLANG_APPEND",
    "clang_drop": "DIFFKEMP_WRAPPER_CLANG_DROP",
    "debug": "DIFFKEMP_WRAPPER_DEBUG",
    "llvm_link": "DIFFKEMP_WRAPPER_LLVM_LINK",
    "llvm_dis": "DIFFKEMP_WRAPPER_LLVM_DIS",
    "no_opt_override": "DIFFKEMP_WRAPPER_NO_OPT_OVERRIDE",
}


def get_cc_wrapper_path(no_native_cc_wrapper=False):
    """
    Find C compiler wrapper. The priority is as follows:
    1. cc_wrapper-c inside build directory
    2. diffkemp-cc-wrapper from PATH
    3. wrapper source file (executable) = this file (if executable)
    4. diffkemp-cc-wrapper.py from PATH (usually executable copy of this file)
    :param no_native_cc_wrapper: Always use wrapper source file
    """
    # Note: this function is ignored by RPython
    from diffkemp.utils import get_simpll_build_dir
    wrapper_exe = os.path.join(os.path.abspath(get_simpll_build_dir()),
                               "cc_wrapper-c")
    if os.path.exists(wrapper_exe) and not no_native_cc_wrapper:
        return wrapper_exe
    elif shutil.which("diffkemp-cc-wrapper") is not None \
            and not no_native_cc_wrapper:
        return "diffkemp-cc-wrapper"
    elif os.access(__file__, os.X_OK):
        # Use wrapper source, i.e. this file
        return __file__
    else:
        return "diffkemp-cc-wrapper.py"


def execl(file, args):
    # Note: execl from os is not RPython-compatible
    if os.path.sep in file:
        os.execv(file, args)
        return
    if "PATH" in os.environ.keys():
        envpath = os.environ["PATH"]
    else:
        envpath = os.defpath
    path = envpath.split(os.pathsep)
    saved_exc = None
    last_exc = None
    for directory in path:
        fullname = os.path.join(directory, file)
        try:
            os.execv(fullname, args)
        except OSError as e:
            last_exc = e
            if saved_exc is None:
                saved_exc = e
    if saved_exc is not None:
        raise saved_exc
    raise last_exc


class CalledProcessError(OSError):
    pass


def check_call(file, args):
    pid = os.fork()
    if pid == 0:
        execl(file, args)
    _, status = os.waitpid(pid, 0)
    if status != 0:
        raise CalledProcessError()


def wrapper(argv):
    # Get arguments from environmental variables
    db_filename = os.environ.get(wrapper_env_vars["db_filename"])
    clang = os.environ.get(wrapper_env_vars["clang"])
    append = os.environ.get(wrapper_env_vars["clang_append"])
    drop = os.environ.get(wrapper_env_vars["clang_drop"])
    debug = os.environ.get(wrapper_env_vars["debug"])
    llvm_link = os.environ.get(wrapper_env_vars["llvm_link"])
    if os.environ.get(wrapper_env_vars["no_opt_override"]) == "1":
        no_opt_override = True
    else:
        no_opt_override = False

    if (db_filename is None or clang is None or append is None or
            drop is None or argv is None or llvm_link is None):
        print("cc_wrapper: fatal error: missing environmental variable")
        return 1
    append = append.split(",")
    drop = drop.split(",")

    db = []
    # Run GCC
    argv[0] = "gcc"
    try:
        check_call("gcc", argv)
    except CalledProcessError:
        print("cc_wrapper: warning: original build command failed")
        return 1

    # Analyze and modify parameters for clang (phase 1)
    clang_argv = []
    old_clang = clang
    linking_with_sources = False
    output_file = None
    linking = "-c" not in argv
    for index, arg in enumerate(argv):
        if arg in drop:
            continue
        is_object_file = (arg.endswith(".o") or arg.endswith(".lo") or
                          arg.endswith(".ko"))
        is_source_file = arg.endswith(".c")
        if index > 1 and argv[index - 1] == "-o":
            if is_object_file and not linking:
                # Compiling to object file: swap .o with .ll
                arg = arg.rsplit(".", 1)[0] + ".ll"
            if not is_object_file and linking:
                # Linking: add a .llw suffix (LLVM IR whole)
                arg = arg + ".llw"
            output_file = arg
        elif is_object_file and linking:
            # Input to linking phase: change suffix to .ll
            arg = arg.rsplit(".", 1)[0] + ".ll"
            clang = llvm_link
        elif is_source_file and linking:
            # Mark as linking with sources to detect hybrid mode
            linking_with_sources = True
        clang_argv.append(arg)

    if linking_with_sources and clang == llvm_link:
        # Compile/link mode with object files detected
        # Drop object files and revert to normal compile/link mode
        clang = old_clang
        clang_argv = [arg for arg in clang_argv if not arg.endswith(".ll")]

    # Do not continue if output is not .ll or .llw
    # Note: this means that this is neither compilation nor linking
    if (output_file is not None and not output_file.endswith(".ll") and
            not output_file.endswith(".llw")):
        return 0

    # Do not run clang on conftest files
    if output_file in ["conftest.ll", "conftest.llw"] or "conftest.c" in argv:
        return 0

    # Record file in database
    if output_file is not None:
        prefix = "o:" if clang != llvm_link else "f:"
        db.append(prefix + os.path.join(os.getcwd(), output_file))
    elif not linking:
        # Compiling to default output file
        db.extend(["o:" + os.path.join(os.getcwd(),
                                       arg.rsplit(".", 1)[0] + ".ll")
                   for arg in clang_argv if not arg.endswith(".c")])

    # Analyze and modify parameters for clang (phase 2)
    clang_argv[0] = clang
    if clang != llvm_link:
        # Note: extending with the default options must be after
        # adding the original arguments and before
        # extending with the append (clang_append) option
        # so the optimizations flags can be redefined
        # (it looks like clang uses last specified optimization flag).
        clang_argv.extend(get_clang_default_options(
            default_optim=not no_opt_override))
        clang_argv.extend(append)
        # TODO: allow compiling into binary IR
    else:
        # Keep only arguments with input files (and llvm-link itself)
        clang_argv = [arg for arg in clang_argv if arg == clang or
                      arg.endswith(".ll") or arg.endswith(".llw") or
                      arg == "-o"]
        # Remove non-existent files
        # Note: these might have been e.g. generated from assembly
        new_clang_argv = [clang_argv[0], "-S"]
        output_file_flag = False
        for arg in clang_argv[1:]:
            if output_file_flag or os.path.exists(arg) or arg == "-o":
                new_clang_argv.append(arg)
                if output_file_flag:
                    output_file_flag = False
            if arg == "-o":
                output_file_flag = True
        clang_argv = new_clang_argv

    # Run clang
    try:
        if debug:
            print("Wrapper calling: " + " ".join(clang_argv))
        check_call(clang, clang_argv)
    except CalledProcessError:
        print("cc_wrapper: warning: clang failed")
        print(clang_argv)
        # clang error is non-fatal
        return 0

    # Update database
    with open(db_filename, "a") as db_file:
        # TODO: RPython-compatible file locking
        for entry in db:
            if not os.path.exists(entry.split(":")[1]):
                continue
            db_file.write(entry + "\n")

    return 0


def target(driver, args):
    return wrapper


# Enable running directly under Python interpreter
if __name__ == "__main__":
    wrapper(sys.argv)
