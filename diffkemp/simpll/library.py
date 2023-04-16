"""
Python interface for the SimpLL library.
"""
from diffkemp.simpll.simpll_lib import ffi, lib


def run_generate_pattern(function, files):
    func = ffi.new("char []", function.encode("ascii"))
    for file in files:
        file = ffi.new("char []", file.encode("ascii"))
        lib.runGeneratePattern(func, file)
    lib.reportPattern()


def run_pattern_info(path):
    lib.readPatternConfig(ffi.new("char []", path.encode("ascii")))


def _ptrarray_to_list(ptrarray):
    """Converts a ptr_array structure from SimpLL into a Python list."""
    result = []
    for i in range(0, ptrarray.len):
        result.append(ptrarray.arr[i])
    lib.freePointerArray(ptrarray)
    return result


def _stringarray_to_list(ptrarray):
    """
    Converts a ptr_array structure representing strings that are owned by the
    array into a Python list.
    Note: for ptr_arrays containing converted StringRefs owned by
    a LLVMContext use _ptrarray_to_list.
    """
    result = []
    for i in range(0, ptrarray.len):
        result.append(ffi.string(ffi.cast("char *", ptrarray.arr[i])).
                      decode("ascii"))
    lib.freeStringArray(ptrarray)
    return result


class SimpLLModule:
    """Represents a Module class in LLVM."""

    def __init__(self, path):
        self.pointer = lib.loadModule(ffi.new("char []", path.encode("ascii")))
        if self.pointer == ffi.NULL:
            raise ValueError("Cannot import module " + path)

    def __eq__(self, other):
        return self.pointer == other.pointer

    def __hash__(self):
        return hash(self.pointer)

    def __del__(self):
        lib.freeModule(self.pointer)

    def get_function(self, fun_name):
        pointer = lib.getFunction(self.pointer,
                                  ffi.new("char []", fun_name.encode("ascii")))
        if pointer != ffi.NULL:
            return SimpLLFunction(self, pointer)
        else:
            return None

    def find_param_var(self, param):
        result = lib.findParamVarC(ffi.new("char []", param.encode("ascii")),
                                   self.pointer)
        if result != ffi.NULL:
            return ffi.string(result).decode("ascii")
        else:
            return None

    def get_functions_using_param(self, param, indices):
        # Convert indices into a C array.
        if indices is not None:
            indices_c = ffi.new("int [{}]".format(len(indices)))
            for i in range(0, len(indices)):
                indices_c[i] = indices[i]
            indices_n = len(indices)
        else:
            indices_c = ffi.NULL
            indices_n = 0

        # Run the function.
        param_c = ffi.new("char []", param.encode("ascii"))
        carray = lib.getFunctionsUsingParamC(param_c, indices_c, indices_n,
                                             self.pointer)

        # Convert the results back to Python.
        return {ffi.string(ffi.cast("char *", cstr)).decode("ascii")
                for cstr in _ptrarray_to_list(carray)}

    def preprocess(self, control_flow_only):
        lib.preprocessModuleC(self.pointer, control_flow_only)


class SimpLLFunction:
    """Represents a Function class in LLVM."""

    def __init__(self, module, pointer):
        self.module = module
        self.pointer = pointer

    def __eq__(self, other):
        return self.pointer == other.pointer

    def __hash__(self):
        return hash(self.pointer)

    def get_name(self):
        cstring = lib.getFunctionName(self.pointer)
        if cstring != ffi.NULL:
            return ffi.string(cstring).decode("ascii")
        else:
            return None

    def is_declaration(self):
        return bool(lib.isDeclaration(self.pointer))

    def get_called_functions(self):
        carray = lib.getCalledFunctions(self.pointer)
        return {SimpLLFunction(self.module, pointer)
                for pointer in _ptrarray_to_list(carray)}


class SimpLLSysctlTable:
    def __init__(self, module, ctl_table):
        self.module = module
        ctl_table_c = ffi.new("char []", ctl_table.encode("ascii"))
        self.pointer = lib.getSysctlTable(module.pointer, ctl_table_c)

    def __del__(self):
        lib.freeSysctlTable(self.pointer)

    def parse_sysctls(self, sysctl_pattern):
        sysctl_pattern_c = ffi.new("char []", sysctl_pattern.encode("ascii"))
        result = lib.parseSysctls(sysctl_pattern_c, self.pointer)

        return _stringarray_to_list(result)

    def get_proc_fun(self, sysctl_name):
        sysctl_name_c = ffi.new("char []", sysctl_name.encode("ascii"))
        result = lib.getProcFun(sysctl_name_c, self.pointer)

        return (SimpLLFunction(self.module, result)
                if result != ffi.NULL else None)

    def _get_global_variable(self, sysctl_name, function):
        """
        Code common for both get_child and get_data.
        :return: Pair of the global variable name and a list of indices.
        """
        sysctl_name_c = ffi.new("char []", sysctl_name.encode("ascii"))
        result = function(sysctl_name_c, self.pointer)
        if result.name == ffi.NULL:
            return None

        # Convert indices into a Python array.
        indices = []
        for i in range(result.indices_n):
            indices.append(result.indices[i])

        return ffi.string(result.name).decode("ascii"), indices

    def get_child(self, sysctl_name):
        return self._get_global_variable(sysctl_name, lib.getChild)

    def get_data(self, sysctl_name):
        return self._get_global_variable(sysctl_name, lib.getData)
