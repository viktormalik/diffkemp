"""Result of difference analysis."""

from enum import IntEnum


class Result:
    """
    Result of a difference analysis.
    Contains the compared entities (functions, params, etc.) and optionally
    contains a list of results of underlying entities (e.g. called functions).
    """
    class Kind(IntEnum):
        """
        Enumeration type for possible kinds of analysis results.
        Sorted by priority for result aggregation.
        """
        NONE = 0
        EQUAL = 1
        ASSUMED_EQUAL = 2
        NOT_EQUAL = 3
        UNKNOWN = 4
        TIMEOUT = 5
        ERROR = 6

        @staticmethod
        def from_string(string):
            dictionary = {
                "none": Result.Kind.NONE,
                "equal": Result.Kind.EQUAL,
                "assumed-equal": Result.Kind.ASSUMED_EQUAL,
                "not-equal": Result.Kind.NOT_EQUAL,
                "unknown": Result.Kind.UNKNOWN,
                "timeout": Result.Kind.TIMEOUT,
                "error": Result.Kind.ERROR
            }
            return dictionary[string]

        def __str__(self):
            return self.name.lower().replace("_", " ")

    class Callstack:
        """
        Callstack of called functions.
        Calls is a list of dictionaries with keys: name, file, line.
        """

        def __init__(self, calls):
            self.calls = calls

        def __add__(self, other):
            return Result.Callstack(self.calls + other.calls)

        @classmethod
        def from_edge_objects(cls, callstack):
            """Creates a Callstack from callstack consisting
            of Edge objects."""
            return cls([{"name": call.target_name, "file": call.filename,
                         "line": call.line}
                        for call in callstack])

        @classmethod
        def from_simpll_yaml(cls, callstack):
            """Creates a Callstack from a YAML representation of
            a callstack (used in non-fun diffs)."""
            return cls([{"name": call["function"], "file": call["file"],
                         "line": call["line"]}
                        for call in callstack])

        def as_str_with_rel_paths(self, prefix):
            """Returns callstack as string with relative paths
            to prefix."""
            return str(self).replace(prefix, "")

        def to_output_yaml_with_rel_path(self, prefix):
            """Returns callstack as output YAML representation with
            relative file paths to prefix"""
            if self.calls is None:
                return []
            return [{"name": call["name"],
                     "file": call["file"].replace(prefix, ""),
                     "line": call["line"]}
                    for call in self.calls]

        @staticmethod
        def get_name_and_kind(call):
            """Returns tuple (name, kind) for a call."""
            name = call["name"]
            kind = "function"
            if " " in name:
                name, kind = name.split(" ")
                # kind looks like this "(kind)", extracting it from brackets
                kind = kind[1:-1]
            return (name, kind)

        def get_parent_call_name(self, index, compared_fun):
            """Returns name of the parrent call (calling function/macro)-
            :param index: Index to callstack containing current call.
            :param compared_fun: Name of compared function.
            """
            parent_name = (self.calls[index - 1]["name"]
                           if index >= 1
                           else compared_fun)
            return parent_name

        def get_symbol_names(self, compared_fun):
            """
            Returns a tuple containing three sets of symbol names which appear
            in callstack:
            - function names (set of strings)
            - macro names (set of strings)
            - type names (set of string pairs)
                (type name, name of function in which the type is used)
            :param compared_fun: Name of compared function.
            """
            function_names = set()
            macro_names = set()
            type_names = set()
            if not self.calls:
                return function_names, macro_names, type_names
            for index, call in enumerate(self.calls):
                name, kind = self.get_name_and_kind(call)
                if kind == "function":
                    function_names.add(name)
                elif kind == "macro":
                    macro_names.add(name)
                elif kind == "type":
                    # name of type and name of function in which it is used
                    parent_name = self.get_parent_call_name(index,
                                                            compared_fun)
                    type_names.add((name, parent_name))
            return function_names, macro_names, type_names

        def get_macro_defs(self, compared_fun):
            """
            Returns a tuple:
            - list of macro definitions (except last macro in the callstack)
              = list of dicts (`file`, `name`, `line`),
            - (vertex name, last macro name) for retrieving info
              about last macro defintion from the ComparisonGraph.
            In case the call stack does not includes macros, the list
            with definitions is empty and instead of the (vertex, macro) tuple
            it contains None.
            :param compared_fun: Name of compared function.
            """
            macro_defs = []
            # For getting last macro deffinition
            last_macro_name = None
            last_function_name = compared_fun
            if self.calls:
                for index, call in enumerate(self.calls):
                    name, kind = self.get_name_and_kind(call)
                    if kind != "macro":
                        last_function_name = name
                        continue
                    # last call in callstack
                    if index + 1 == len(self.calls):
                        last_macro_name = name
                    else:
                        # The call contains a macro name, a file from which
                        # the macro was 'called' and a line where starts
                        # the macro body/def in which the call was called from.
                        # To get information about a current macro file and
                        # line, we have to look on a call below.
                        macro_defs.append({
                            "name": call["name"],
                            "file": self.calls[index + 1]["file"],
                            "line": self.calls[index + 1]["line"]
                        })
            last_macro_info = (last_function_name, last_macro_name) \
                if last_macro_name is not None else None
            return macro_defs, last_macro_info

        def __str__(self):
            """Converts a callstack to a string representation."""
            if self.calls is None:
                return ""
            return "\n".join(["{} at {}:{}".format(call["name"],
                                                   call["file"],
                                                   call["line"])
                             for call in self.calls])

    class Entity:
        """
        Compared entity information. This can be e.g. a function, a module,
        or a parameter.
        If it is a function, it contains the file of the function.
        """
        def __init__(self, name, filename=None, line=None, callstack=None,
                     diff_kind="function", covered=False):
            self.name = name
            self.filename = filename
            self.line = line
            self.callstack = callstack
            self.diff_kind = diff_kind
            self.covered = covered

    def __init__(self, kind, first_name, second_name, start_time=None,
                 stop_time=None):
        self.kind = kind
        self.first = Result.Entity(first_name)
        self.second = Result.Entity(second_name)
        self.diff = None
        self.macro_diff = None
        self.graph = None
        self.inner = dict()
        self.start_time = start_time
        self.stop_time = stop_time

    def __str__(self):
        return str(self.kind)

    def add_inner(self, result):
        """
        Add result of an inner entity.
        The overall current result is updated based on the entity result.
        """
        self.inner[result.first.name] = result
        # The current result is joined with the inner result (the result with
        # a higher priority is chosen from the two).
        self.kind = Result.Kind(max(int(self.kind), int(result.kind)))
        # The graph of the latest inner result is the graph of the outer one.
        # Note: this is true because the graph is built incrementally, reusing
        # the already known results from the previous comparison.
        self.graph = result.graph

    def report_symbol_stat(self, show_errors=False):
        """
        Report symbol statistics.
        Print numbers of equal, non-equal, unknown, and error results with
        percentage that each has from the total results.
        """
        total = len(self.inner)
        eq = len([r for r in iter(self.inner.values())
                  if r.kind == Result.Kind.EQUAL])
        neq = len([r for r in iter(self.inner.values())
                   if r.kind == Result.Kind.NOT_EQUAL])
        unkwn = len([r for r in iter(self.inner.values())
                     if r.kind == Result.Kind.UNKNOWN])
        errs = len([r for r in iter(self.inner.values())
                    if r.kind in [Result.Kind.ERROR, Result.Kind.TIMEOUT]])
        empty_diff = len([r for r in iter(self.inner.values()) if all(map(
            lambda x: x.diff == "", r.inner.values())) and
            r.kind == Result.Kind.NOT_EQUAL])
        if total > 0:
            print("Total symbols: {}".format(total))
            print("Equal:         {0} ({1:.0f}%)".format(eq, eq / total * 100))
            print("Not equal:     {0} ({1:.0f}%)".format(
                  neq, neq / total * 100))
            print("(empty diff):  {0} ({1:.0f}%)".format(
                  empty_diff, empty_diff / total * 100))
            print("Unknown:       {0} ({1:.0f}%)".format(unkwn,
                                                         unkwn / total * 100))
            print("Errors:        {0} ({1:.0f}%)".format(errs,
                                                         errs / total * 100))
        if show_errors:
            if unkwn > 0:
                print("\nFunctions that are unknown: ")
                for f, r in sorted(self.inner.items()):
                    if r.kind == Result.Kind.UNKNOWN:
                        print(f)
                print()
            if errs > 0:
                print("\nFunctions whose comparison ended with an error: ")
                for f, r in sorted(self.inner.items()):
                    if r.kind == Result.Kind.ERROR:
                        print(f)
                print()

    def report_object_stat(self):
        """
        Report detailed statistics about compared objects.
        Prints the total count of unique non-equal objects (inner diffs)
        excluding those covered by syntax diffs.
        Also prints the count and percentage of those which are function diffs,
        macro diffs, inline asm diffs and empty diffs.
        """
        # Wrapper class to store in set
        class UniqueDiff:
            def __init__(self, res):
                self.res = res

            def __eq__(self, other):
                return self.res.first.name == other.res.first.name

            def __hash__(self):
                return hash(self.res.first.name)

        # Convert inner result to set of unique diffs
        unique_diffs = set()
        for _, inner_res_out in self.inner.items():
            for _, inner_res in inner_res_out.inner.items():
                if (inner_res.diff == "" and
                        inner_res.first.covered):
                    continue
                unique_diffs.add(UniqueDiff(inner_res))

        # Generate counts
        compared = len(self.graph.vertices)
        compared_instructions = sum([v.stats.compared_inst_cnt() for v in
                                     self.graph.vertices.values()])
        compared_lines = sum([v.stats.compared_lines_cnt() for v in
                              self.graph.vertices.values()])
        equal_instructions = sum([v.stats.compared_inst_equal_cnt() for v
                                  in self.graph.vertices.values()])
        if compared_instructions > 0:
            equal_percent = equal_instructions / compared_instructions * 100
        else:
            equal_percent = float('nan')
        total = len(unique_diffs)
        functions = len([r for r in unique_diffs
                         if r.res.first.diff_kind == "function"])
        types = len([r for r in unique_diffs
                     if r.res.first.diff_kind == "type"])
        macros = len([r for r in unique_diffs
                      if (r.res.first.diff_kind == "syntactic" and
                          not r.res.first.name.startswith("assembly code"))])
        asm = len([r for r in unique_diffs
                  if (r.res.first.diff_kind == "syntactic" and
                      r.res.first.name.startswith("assembly code"))])
        empty = len([r for r in unique_diffs if r.res.diff == ""])

        # Print statistics
        if self.start_time and self.stop_time:
            print("Elapsed time:            {:.2f} s".format(
                self.stop_time - self.start_time))
        print("Functions compared:      {}".format(compared))
        print("Lines compared:          {}".format(compared_lines))
        print("Instructions compared:   {}".format(compared_instructions))
        print("1:1 equal instructions:  {0} ({1:.0f}%)".format(
            equal_instructions, equal_percent))
        print("")
        print("Total differences:       {}".format(total))
        if total == 0:
            return
        print("In functions:            {0} ({1:.0f}%)".format(functions,
              functions / total * 100))
        print("In types:                {0} ({1:.0f}%)".format(types,
              types / total * 100))
        print("In macros:               {0} ({1:.0f}%)".format(macros,
              macros / total * 100))
        print("In inline assembly code: {0} ({1:.0f}%)".format(asm,
              asm / total * 100))
        print("Empty diffs:             {0} ({1:.0f}%)".format(empty,
              empty / total * 100))

    def report_stat(self, show_errors=False, extended_stat=False):
        """Reports all statistics."""
        self.report_symbol_stat(show_errors)
        if extended_stat:
            print("")
            self.report_object_stat()
