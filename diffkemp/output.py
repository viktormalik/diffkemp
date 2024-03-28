from diffkemp.semdiff.caching import ComparisonGraph
from diffkemp.utils import get_end_line, EndLineNotFound
from diffkemp.semdiff.result import Result
import os
import yaml


class YamlOutput:
    """Yaml representation of compare result."""

    def __init__(self, snapshot_dir_old, snapshot_dir_new, result):
        self.old_dir = snapshot_dir_old
        self.new_dir = snapshot_dir_new
        self.result = result
        self.output = {}
        # Sets with symbol names
        self.function_names = set()
        # Note: type_names contains tuples (type name, parent name)
        #   parent name - name of function in which type is used
        self.type_names = set()

        # Macro definitions extracted from call stacks
        self.old_macro_defs = []
        self.new_macro_defs = []
        # Information from call stacks for extracting defs for last macros
        # - list of tuples (vertex name, last macro name).
        self.last_macro_infos = set()

        self._create_output()

    def save(self, output_dir, file_name):
        with open(os.path.join(output_dir, file_name), "w") as file:
            yaml.dump(self.output, file, sort_keys=False)

    def _create_output(self):
        """Creates yaml representation of compare result."""
        self.output["old-snapshot"] = self.old_dir
        self.output["new-snapshot"] = self.new_dir

        self._create_results()
        self._create_definitions()

    def _create_results(self):
        """Creates call stacks of not-equal functions."""
        # list of all not-equal functions callstacks
        results = []
        # Going over compared functions (fun_name) and its result.
        for fun_name, fun_result in self.result.inner.items():
            if fun_result.kind != Result.Kind.NOT_EQUAL:
                continue
            self.function_names.add(fun_name)
            # list of not-equal functions callstacks for a function
            diffs = []
            for called_res in sorted(fun_result.inner.values(),
                                     key=lambda r: r.first.name):
                if called_res.diff == "" and called_res.first.covered:
                    # Do not add empty diffs
                    continue
                # information about a function which is different
                diff = {}
                diff["function"] = called_res.first.name
                # getting callstacks
                diff["old-callstack"] = called_res.first \
                    .callstack.to_output_yaml_with_rel_path(self.old_dir) \
                    if called_res.first.callstack else []
                diff["new-callstack"] = called_res.second \
                    .callstack.to_output_yaml_with_rel_path(self.new_dir) \
                    if called_res.second.callstack else []
                # updating symbol names
                if called_res.first.callstack:
                    # Note: Macro names are not currently used.
                    function_names, macro_names, type_names = called_res \
                        .first.callstack.get_symbol_names(fun_name)
                    self.function_names.update(function_names)
                    self.type_names.update(type_names)

                    # Getting macro defs and info for retreiving info
                    # about last macros definitions.
                    macro_defs, last_macro_info = called_res.first.callstack \
                        .get_macro_defs(fun_name)
                    self.old_macro_defs.extend(macro_defs)
                    if last_macro_info is not None:
                        self.last_macro_infos.add(last_macro_info)
                if called_res.second.callstack:
                    macro_defs, last_macro_info = called_res.second.callstack \
                        .get_macro_defs(fun_name)
                    self.new_macro_defs.extend(macro_defs)
                    if last_macro_info is not None:
                        self.last_macro_infos.add(last_macro_info)
                diffs.append(diff)
            results.append({
                "function": fun_name,
                "diffs": diffs
            })
        self.output["results"] = results

    def _create_definitions(self):
        self.output["definitions"] = {}
        self._create_function_defs()
        self._create_type_defs()
        self._create_macro_defs()

    def _create_function_defs(self):
        if self.result.graph is None:
            return
        definitions = {}
        for name in self.function_names:
            if name not in self.result.graph.vertices:
                continue
            vertex = self.result.graph[name]
            definition = {
                "kind": "function",
                "old": self._create_def_info(vertex.lines[0],
                                             vertex.files[0],
                                             self.old_dir,
                                             "function"),
                "new": self._create_def_info(vertex.lines[1],
                                             vertex.files[1],
                                             self.new_dir,
                                             "function"),
            }
            # function name differs
            if vertex.names[1] != vertex.names[0]:
                definition["new"]["name"] = vertex.names[1]
            definitions[name] = definition
        self.output["definitions"].update(definitions)

    def _create_type_defs(self):
        """Creates definitions for types."""
        if self.result.graph is None:
            return
        definitions = {}
        for type_name, parent_name in self.type_names:
            if parent_name not in self.result.graph.vertices:
                continue
            vertex = self.result.graph[parent_name]
            for non_fun in vertex.nonfun_diffs:
                if non_fun.name == type_name and \
                        isinstance(non_fun, ComparisonGraph.TypeDiff):
                    definition = {
                        "kind": "type",
                        "old": self._create_def_info(non_fun.line[0],
                                                     non_fun.file[0],
                                                     self.old_dir,
                                                     "type"),
                        "new": self._create_def_info(non_fun.line[1],
                                                     non_fun.file[1],
                                                     self.new_dir,
                                                     "type")
                    }
                    definitions[type_name] = definition
                    break
        self.output["definitions"].update(definitions)

    def _create_macro_defs(self):
        """Creates definitions for macros."""
        # Adding macro defs extracted from call stacks.
        self._yaml_defs_from_macros(oldVersion=True)
        self._yaml_defs_from_macros(oldVersion=False)

        # Finding definitions for the last macros.
        definitions = {}
        if self.result.graph is None:
            return
        for vertex_name, last_macro_name in self.last_macro_infos:
            if vertex_name not in self.result.graph.vertices:
                continue
            vertex = self.result.graph[vertex_name]
            for non_fun in vertex.nonfun_diffs:
                if non_fun.name == last_macro_name and \
                        isinstance(non_fun, ComparisonGraph.SyntaxDiff):
                    if non_fun.last_def:
                        self._yaml_def_from_macro(non_fun.last_def[0],
                                                  self.old_dir, "old",
                                                  non_fun.kind)
                        self._yaml_def_from_macro(non_fun.last_def[1],
                                                  self.new_dir, "new",
                                                  non_fun.kind)
                    break
        self.output["definitions"].update(definitions)

    def _yaml_defs_from_macros(self, oldVersion):
        """Updates yaml definitions with macro defs retrieved from call stacks.
        :param version: If true updates definitions for old version of program,
            otherwise for new version.
        """
        version = "old" if oldVersion else "new"
        macro_defs = self.old_macro_defs if oldVersion else self.new_macro_defs
        base_dir = self.old_dir if oldVersion else self.new_dir
        for macro_def in macro_defs:
            self._yaml_def_from_macro(macro_def, base_dir, version)

    def _yaml_def_from_macro(self, macro_def, base_dir, version, kind="macro"):
        """
        :param macro_def: Dict with keys name, line, file.
        :param base_dir: Path to snapshot directory.
        :param version: Definition for "old" x "new" version of program. """
        name = macro_def["name"].split()[0]
        file = os.path.join(base_dir, macro_def["file"])
        line = macro_def["line"]
        version_kind = kind
        # Note: kind can be macro-function, function-macro.
        if "-" in kind:
            version_kind = kind.split("-")
            version_kind = version_kind[0] if version == "old" \
                else version_kind[1]
        definition = {
            "kind": kind,
            version: self._create_def_info(line, file, base_dir, version_kind)
        }
        if name not in self.output["definitions"]:
            self.output["definitions"][name] = definition
        else:
            self.output["definitions"][name].update(definition)

    def _create_def_info(self, line, file, snapshot_dir, kind):
        info = {
            "line": line,
            "file": os.path.relpath(file, snapshot_dir),
        }
        try:
            info["end-line"] = get_end_line(file, line, kind)
        except (UnicodeDecodeError, EndLineNotFound, ValueError):
            pass
        return info
