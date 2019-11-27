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
        EQUAL_SYNTAX = 1
        EQUAL = 2
        NOT_EQUAL = 3
        UNKNOWN = 4
        TIMEOUT = 5
        ERROR = 6

        def __str__(self):
            return self.name.lower().replace("_", " ")

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

    def __init__(self, kind, first_name, second_name):
        self.kind = kind
        self.first = Result.Entity(first_name)
        self.second = Result.Entity(second_name)
        self.diff = None
        self.macro_diff = None
        self.analysed_loc = 0
        self.inner = dict()

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
        self.analysed_loc = self.analysed_loc + result.analysed_loc

    def report_symbol_stat(self, show_errors=False):
        """
        Report symbol statistics.
        Print numbers of equal, non-equal, unknown, and error results with
        percentage that each has from the total results.
        """
        total = len(self.inner)
        eq = len([r for r in iter(self.inner.values())
                  if r.kind in [Result.Kind.EQUAL, Result.Kind.EQUAL_SYNTAX]])
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

    def report_loc(self):
        """Report number of analysed lines of code."""
        print("Total LOC analysed: {}".format(self.analysed_loc))

    def report_stat(self, show_errors=False):
        """Reports all statistics."""
        self.report_symbol_stat(show_errors)
        print("")
        self.report_object_stat()
        print("")
        self.report_loc()
