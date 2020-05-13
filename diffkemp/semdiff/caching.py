"""
A mechanism for caching the results of individual SimpLL runs.

The function comparison results that have been collected so far are stored in
a graph whose vertices are the individual results (i.e. whether two functions
are equal or not) and the edges are function calls. This graph is represented
by the ComparisonGraph class.
"""

from collections import deque
from diffkemp.semdiff.result import Result
from enum import IntEnum
import os


class ComparisonGraph:
    """
    Representation of a (partial or complete) result of a DiffKemp run
    in comparison mode.
    """
    class Side(IntEnum):
        """Either the left module or the right module."""
        LEFT = 0
        RIGHT = 1

    class DependencyKind(IntEnum):
        """
        A strong dependency means that the equality of the target affects
        the equality of the source. A weak dependency means that the edge (or
        edges to the vertex, if used with one) is/are important for lookups in
        the graph, but the non-equality of the target should not generate
        a diff.

        Note: A normalized graph is a graph where all vertices are strong (i.e.
        they do not contain dotted functions).
        """
        WEAK = 0
        STRONG = 1

        @classmethod
        def from_yaml(cls, yaml):
            if yaml:
                return ComparisonGraph.DependencyKind.WEAK
            else:
                return ComparisonGraph.DependencyKind.STRONG

    class Vertex:
        """
        Vertex in the comparison graph.

        Represents a single function difference and contains both function
        names, the comparison result, a list of attached non-function
        differences and a list of its edges (i.e. the direct callees).

        Note: names, files and lines are tuples containing the values for both
        modules.
        """
        def __init__(self, names, result, files=None, lines=None):
            self.names = names
            self.result = result
            self.nonfun_diffs = []
            self.successors = ([], [])
            self.files = files
            self.lines = lines
            # Vertices are by default cachable, but there are some cases when
            # it is necessary to run the comparison again.
            self.cachable = True
            # The result of some vertices prevents the caching of other ones.
            # This list is used in situations when the result changes in graph
            # merging to reset their cachable flags to True.
            self.prevents_caching_of = []
            # Used only for the detection of uncachable vertices in header
            # files.
            self.predecessors = ([], [])

        def __repr__(self):
            return "Vertex({0}, {1}, {2}, {3}, {4})".format(
                self.names, self.result, self.nonfun_diffs, self.files,
                self.lines
            )

        @classmethod
        def from_yaml(cls, fun_result, parent_graph=None):
            """
            Generates a Vertex object including all edges from a YAML
            representation returned by SimpLL.
            """
            res_left = fun_result["first"]
            res_right = fun_result["second"]
            vertex = cls(
                (res_left["function"], res_right["function"]),
                Result.Kind.from_string(fun_result["result"]),
                (res_left["file"] if "file" in res_left else None,
                 res_right["file"] if "file" in res_right else None),
                (res_left["line"] if "line" in res_left else None,
                 res_right["line"] if "line" in res_right else None)
            )
            if "calls" in res_left:
                for res, side in [(res_left,
                                   ComparisonGraph.Side.LEFT),
                                  (res_right,
                                   ComparisonGraph.Side.RIGHT)]:
                    for callee in res["calls"]:
                        edge = ComparisonGraph.Edge.from_yaml(callee)
                        if parent_graph:
                            parent_graph.add_edge(vertex, side, edge)
                        else:
                            vertex.add_successor(side, edge)
            # Add non-function differences.
            if "differing-objects" in fun_result:
                for nonfun_diff in fun_result["differing-objects"]:
                    if "body-first" in nonfun_diff:
                        # Syntax difference
                        diff_obj = ComparisonGraph.SyntaxDiff.from_yaml(
                            nonfun_diff)
                    elif "file-first" in nonfun_diff:
                        # Type difference
                        diff_obj = ComparisonGraph.TypeDiff.from_yaml(
                            nonfun_diff)
                    vertex.add_nonfun_diff(diff_obj)
            return vertex

        def add_successor(self, side, edge):
            """Adds a direct callee."""
            self.successors[side].append(edge)
            edge.parent_vertex = self

        def add_nonfun_diff(self, diff):
            """Adds a non-function difference."""
            self.nonfun_diffs.append(diff)

        def compare_vertex_priority(self, other):
            """
            Compares the priority of the two vertices. If the first should be
            prefered when merging graphs, returns False, else returns True.
            """
            if self.result in [Result.Kind.ASSUMED_EQUAL, Result.Kind.UNKNOWN]:
                return True
            # Check lengths of successor lists (this is important for cases
            # where linking happened).
            for side in ComparisonGraph.Side:
                if len(other.successors[side]) > len(self.successors[side]):
                    return True
            return False

    class Edge:
        """
        Edges in the comparison graph.
        Note: the object contains the target name instead of the Vertex object
        (unlike in all other cases) because it doesn't have to be present when
        the edge object is generated.
        """
        def __init__(self, target_name, filename, line):
            self.parent_vertex = None
            self.target_name = target_name
            self.filename = filename
            self.line = line
            self.kind = ComparisonGraph.DependencyKind.STRONG

        def __repr__(self):
            return "Edge({0}, {1}, {2})".format(
                self.target_name, self.filename, self.line
            )

        @classmethod
        def from_yaml(cls, callee):
            """Generates an Edge object from YAML returned by SimpLL."""
            res = cls(callee["function"], callee["file"], int(callee["line"]))
            res.kind = ComparisonGraph.DependencyKind.from_yaml(callee["weak"])
            return res

    class NonFunDiff:
        """
        A non-function difference.

        Note: callstack is a tuple containing the values for both modules.
        """
        def __init__(self, name, parent_fun, callstack):
            self.name = name
            self.parent_fun = parent_fun
            self.callstack = callstack

    class SyntaxDiff(NonFunDiff):
        """
        A syntax difference.

        Note: body is a tuple containing the values for both modules.
        """
        def __init__(self, name, parent_fun, callstack, body):
            self.name = name
            self.parent_fun = parent_fun
            self.callstack = callstack
            self.body = body

        @classmethod
        def from_yaml(cls, nonfun_diff):
            """Generates a SyntaxDiff object from YAML returned by SimpLL."""
            return cls(
                nonfun_diff["name"],
                nonfun_diff["function"],
                (nonfun_diff["stack-first"], nonfun_diff["stack-second"]),
                (nonfun_diff["body-first"], nonfun_diff["body-second"])
            )

    class TypeDiff(NonFunDiff):
        """
        A syntax difference.

        Note: file and line are tuples containing the values for both modules.
        """
        def __init__(self, name, parent_fun, callstack, file, line):
            self.name = name
            self.parent_fun = parent_fun
            self.callstack = callstack
            self.file = file
            self.line = line

        @classmethod
        def from_yaml(cls, nonfun_diff):
            """Generates a TypeDiff object from YAML returned by SimpLL."""
            return cls(
                nonfun_diff["name"],
                nonfun_diff["function"],
                (nonfun_diff["stack-first"], nonfun_diff["stack-second"]),
                (nonfun_diff["file-first"], nonfun_diff["file-second"]),
                (nonfun_diff["line-first"], nonfun_diff["line-second"])
            )

    def __init__(self):
        # Vertices are stored as a dictionary, the keys being the function
        # names in the left module and the values objects containing the result
        self.vertices = dict()
        self.equal_funs = set()
        # Cache containing edges that should be processed in the normalization
        # process.
        self._normalize_edge_cache = []
        # Cache containing weak vertices (i.e. vertices that should be
        # processed in the normalization process; there should be no weak
        # vertices in the graph after it).
        self._weak_vertex_cache = []

    def __getitem__(self, function_name):
        return self.vertices[function_name]

    def __setitem__(self, function_name, value):
        self.vertices[function_name] = value
        if value.result == Result.Kind.EQUAL:
            self.equal_funs.add(value.names[ComparisonGraph.Side.LEFT])
        if function_name.endswith(".void"):
            self._weak_vertex_cache.append(value)

    def __len__(self):
        return len(self.vertices)

    def __repr__(self):
        return "Graph(vertices: {0}, equal_funs: {1})".format(
            str(self.vertices), str(self.equal_funs)
        )

    def add_edge(self, vertex, side, edge):
        """
        Adds an edge to the graph.
        Note: Unlike direct addition through Vertex.add_successor, this
        generates an entry in the normalization cache, therefore this should be
        the preferred way to add a new edge to the graph.
        """
        vertex.add_successor(side, edge)
        if edge.target_name.endswith(".void"):
            # Edge's target is a variant function (e.g. a void-returning one).
            # This is possibly a weak dependency (this depends on whether the
            # target vertex, which doesn't have to be present yet, has equal as
            # its result).
            self._normalize_edge_cache.append(edge)

    def reachable_from(self, side, start_fun_name):
        """
        Generates a list of all functions depending on the function
        in the argument (e.g. all vertices reachable from the one specified by
        the function name including the base vertex).

        The function returns two objects. The first one is the function list,
        the second one is a backtracking map for callstacks - for each function
        the edge used to get to its vertex in the BFS algorithm is recorded, so
        the parent vertex and the call site can be retrieved.
        """
        start_fun = self[start_fun_name]
        original_source_files = start_fun.files
        visited = set()
        queue = deque()
        result = []
        backtracking_map = {}
        queue.appendleft(start_fun)
        result.append(start_fun)
        while queue:
            current = queue.pop()
            visited.add(current)
            for edge in current.successors[side]:
                if edge.target_name not in self.vertices:
                    # Invalid successor.
                    continue
                target = self[edge.target_name]
                if target in visited:
                    continue
                if (target.files[side].endswith(".c") and
                        original_source_files[side] != target.files[side]):
                    # Do not walk on edges to a C source file from a different
                    # file.
                    # Note: this is an (in context of this class) arbitrary
                    # comparison boundary beyond which we don't consider the
                    # results interesting.
                    continue
                queue.appendleft(target)
                if target not in backtracking_map:
                    backtracking_map[target] = edge
                if edge.kind == ComparisonGraph.DependencyKind.WEAK:
                    # Do not include targets of weak edges to the result.
                    continue
                result.append(target)
        return result, backtracking_map

    def absorb_graph(self, graph):
        """
        Merges another graph into this one. New vertices (i.e. vertices from
        graph that do not appear in self) are added and the equal funs set is
        updated respectively.
        Note: the graph in the argument is expected to reference (in edges)
        only vertices that are already present in one graph or the other.
        """
        for name, vertex in graph.vertices.items():
            if (name not in self.vertices or
                    self[name].compare_vertex_priority(vertex)):
                if (name in self.vertices and
                        self[name].result == Result.Kind.ASSUMED_EQUAL and
                        vertex.result != Result.Kind.ASSUMED_EQUAL):
                    # If changing away from assumed equal, reset the cachable
                    # mark for all vertices affected by the previous result.
                    for vertex_to_reset in self[name].prevents_caching_of:
                        vertex_to_reset.cachable = True
                # Note: the entry to equal_funs is added automatically.
                self[name] = vertex

    def normalize(self):
        """
        Normalizes the graph. A normalized graph satisfies these conditions:
        1. It contains no weak vertices. A weak vertex is a vertex where at
        least one of the functions (either left or right) is a variant. Such a
        vertex can be recognized by a point suffix in its key.
        2. Weak edges point to vertices generated from weak vertices that had
        equal as their comparison result. This result is set to assumed-equal
        to enable it to be replaced with the actual result if needed; the
        weakness of the edge ensures that revisiting the original path does not
        generate an additional diff.
        """
        for edge in self._normalize_edge_cache:
            unpointed_name = edge.target_name[:-len(".void")]
            # Now the vertex the edge is pointing to should be in place.
            # Note: the key is always the name of the variant function.
            vertex = self[edge.target_name]
            if vertex.result == Result.Kind.EQUAL:
                edge.kind = ComparisonGraph.DependencyKind.WEAK
                # Try to find the corresponding edge in the other side of the
                # graph and also set it to weak.
                if edge.parent_vertex:
                    # Look for the undotted name.
                    for side in ComparisonGraph.Side:
                        for e in edge.parent_vertex.successors[side]:
                            if e.target_name == unpointed_name:
                                # Set the corresponding edge to weak.
                                e.kind = ComparisonGraph.DependencyKind.WEAK
            # Redirect the edge to the strong vertex (since all weak ones will
            # be removed).
            edge.target_name = unpointed_name
        # Deal with weak vertices. If there is no corresponding strong vertex,
        # strengthen the weak one, if there is one, delete the weak one.
        for weak_vertex in self._weak_vertex_cache:
            pointed_name = (weak_vertex.names[ComparisonGraph.Side.LEFT] if
                            weak_vertex.names[ComparisonGraph.Side.LEFT].
                            endswith(".void")
                            else weak_vertex.names[ComparisonGraph.Side.RIGHT])
            non_pointed_name = pointed_name[:-len(".void")]
            del self.vertices[pointed_name]
            if non_pointed_name not in self.vertices:
                # Corresponding strong vertex does not exist. Strengthen this
                # one.
                weak_vertex.names = (non_pointed_name, non_pointed_name)
                if weak_vertex.result == Result.Kind.EQUAL:
                    # Enable "upgrading" with the result of the actual strong
                    # vertex in the future.
                    weak_vertex.result = Result.Kind.ASSUMED_EQUAL
                self[non_pointed_name] = weak_vertex

    def populate_predecessor_lists(self):
        """For every vertex a list of its predecessors is generated."""
        for vertex in self.vertices.values():
            for side in ComparisonGraph.Side:
                for edge in vertex.successors[side]:
                    if edge.target_name not in self.vertices:
                        # Invalid edge target.
                        continue
                    successor = self[edge.target_name]
                    successor.predecessors[side].append(vertex)

    def mark_uncachable_from_assumed_equal(self):
        """
        This method marks some specific vertices as "uncachable", preventing
        them from being included into the cache passed to SimpLL, therefore
        forcing them to be re-compared.
        This applies to functions defined in headers that call a function in a
        non-header file with the "assumed equal" result. The reason for this is
        that the assumed equal result may be caused by the implementation not
        being available and the possibility of it becoming available when
        the header is included from another source file (e.g. from the same one
        that the implementation is in).
        """
        # Do a reversed BFS on the graph.
        side = ComparisonGraph.Side.LEFT
        for vertex in self.vertices.values():
            if (vertex.result != Result.Kind.ASSUMED_EQUAL or
                    vertex.files[side].endswith(".h")):
                # Process only vertices with assumed equal results that are not
                # in headers.
                continue
            queue = deque()
            visited = set()
            queue.appendleft(vertex)
            while queue:
                current = queue.pop()
                visited.add(current)
                for predecessor in current.predecessors[side]:
                    if predecessor in visited:
                        continue
                    if not predecessor.files[side].endswith(".h"):
                        # The problem affect only header files.
                        continue
                    queue.appendleft(predecessor)
                    predecessor.cachable = False
                    vertex.prevents_caching_of.append(predecessor)

    def _edge_callstack_to_string(self, callstack):
        """Converts a callstack consisting of Edge objects to a string
        representation."""
        return "\n".join(["{} at {}:{}".format(call.target_name,
                                               call.filename,
                                               call.line)
                          for call in callstack])

    def _yaml_callstack_to_string(self, callstack):
        """Converts a YAML representation of a callstack (used in non-fun)
        diffs to a string representation."""
        return "\n".join(["{} at {}:{}".format(call["function"],
                                               call["file"],
                                               call["line"])
                          for call in callstack])

    def graph_to_fun_pair_list(self, fun_first, fun_second):
        # Extract the functions that should be compared from the graph in
        # the form of Vertex objects.
        called_funs_left, backtracking_map_left = self.reachable_from(
            ComparisonGraph.Side.LEFT, fun_first)
        called_funs_right, backtracking_map_right = self.reachable_from(
            ComparisonGraph.Side.RIGHT, fun_second)
        # Use the intersection of called functions in the left and right
        # module (i.e. differences in functions called in one module only
        # are not processed).
        vertices_to_compare = list(set(called_funs_left).intersection(
            set(called_funs_right)))
        # Use methods from ComparisonGraph (on the graph variable) and
        # vertices_to_compare to generate objects_to_compare.
        objects_to_compare = []
        syndiff_bodies_left = dict()
        syndiff_bodies_right = dict()
        for vertex in vertices_to_compare:
            if vertex.result in [Result.Kind.EQUAL,
                                 Result.Kind.ASSUMED_EQUAL]:
                # Do not include equal functions into the result.
                continue
            # Generate and add the function difference.
            fun_pair = []
            for side in ComparisonGraph.Side:
                fun = fun_first if side == ComparisonGraph.Side.LEFT \
                    else fun_second
                backtracking_map = (backtracking_map_left
                                    if side == ComparisonGraph.Side.LEFT
                                    else backtracking_map_right)
                if fun == vertex.names[side]:
                    # There is no callstack from the base function.
                    calls = None
                else:
                    # Transform the Edge objects returned by
                    # get_shortest_path to a readable callstack.
                    edges = _get_callstack(backtracking_map, self[fun], vertex)
                    calls = self._edge_callstack_to_string(edges)
                # Note: a function diff is covered (i.e. hidden when empty)
                # if and only if there is a non-function difference
                # referencing it.
                fun_pair.append(Result.Entity(
                    vertex.names[side],
                    vertex.files[side],
                    vertex.lines[side],
                    calls,
                    "function",
                    covered=len(vertex.nonfun_diffs) != 0
                ))
            fun_pair.append(vertex.result)
            objects_to_compare.append(tuple(fun_pair))

            # Process non-function differences.
            for nonfun_diff in vertex.nonfun_diffs:
                nonfun_pair = []
                for side in ComparisonGraph.Side:
                    syndiff_bodies = (syndiff_bodies_left
                                      if side == ComparisonGraph.Side.LEFT
                                      else syndiff_bodies_right)
                    backtracking_map = (backtracking_map_left
                                        if side == ComparisonGraph.Side.LEFT
                                        else backtracking_map_right)
                    # Convert the YAML callstack format to string.
                    calls = self._yaml_callstack_to_string(
                        nonfun_diff.callstack[side])
                    # Append the parent function's callstack.
                    # (unless it is the base function)
                    fun = fun_first if side == ComparisonGraph.Side.LEFT \
                        else fun_second
                    if nonfun_diff.parent_fun != fun:
                        parent_calls = self._edge_callstack_to_string(
                            _get_callstack(backtracking_map, self[fun],
                                           vertex))
                        calls = parent_calls + "\n" + calls

                    if isinstance(nonfun_diff, ComparisonGraph.SyntaxDiff):
                        nonfun_pair.append(Result.Entity(
                            nonfun_diff.name,
                            None,
                            None,
                            calls,
                            "syntactic",
                            False
                        ))
                        syndiff_bodies[nonfun_diff.name] = \
                            nonfun_diff.body[side]
                    elif isinstance(nonfun_diff, ComparisonGraph.TypeDiff):
                        nonfun_pair.append(Result.Entity(
                            nonfun_diff.name,
                            nonfun_diff.file[side],
                            nonfun_diff.line[side],
                            calls,
                            "type",
                            False
                        ))
                # Non-function differences are always of the non-equal type
                nonfun_pair.append(Result.Kind.NOT_EQUAL)
                objects_to_compare.append(tuple(nonfun_pair))
        return objects_to_compare, syndiff_bodies_left, syndiff_bodies_right


def _get_callstack(backtracking_map, start_vertex, end_vertex):
    """Generates an edge callstack based on the backtracking map."""
    if start_vertex == end_vertex:
        return []
    edges = [backtracking_map[end_vertex]]
    while edges[-1].parent_vertex is not start_vertex:
        edges.append(backtracking_map[edges[-1].parent_vertex])
    return list(reversed(edges))


class SimpLLCache:
    """
    A class that handles the providing of function pairs contained in the
    comparison graph to SimpLL so it doesn't have to re-compare the functions.
    This is done in the form of a directory containing one cache file for each
    source file pair.
    """
    class CacheFile:
        def __init__(self, directory, left_module, right_module):
            self.left_module = left_module
            self.right_module = right_module
            self.filename = os.path.join(directory,
                                         left_module.replace("/", "$") + ":" +
                                         right_module.replace("/", "$"))

        def add_function_pairs(self, pairs):
            with open(self.filename, "a") as file:
                for pair in pairs:
                    text = "{0}:{1}\n".format(pair[0], pair[1])
                    file.write(text)

        def clear(self):
            os.remove(self.filename)

    def __init__(self, directory):
        self.directory = directory
        # Map from C source file pairs (left and right module) to cache files.
        # (There is one cache file for each such pair).
        self.cache_map = {}

    def update(self, vertices):
        """Update the cache to include vertices passed in the vertices
        argument."""
        # Sort vertices into a map based on source file pairs.
        vertex_map = {}
        for vertex in vertices:
            if not vertex.cachable:
                continue
            if vertex.files not in vertex_map:
                vertex_map[vertex.files] = []
            vertex_map[vertex.files].append(vertex)
        # Update each cache file at once using the generated map to avoid
        # unneccessary overhead from opening and closing files.
        for files, vertices_in_file in vertex_map.items():
            if files not in self.cache_map:
                self.cache_map[files] = SimpLLCache.CacheFile(self.directory,
                                                              files[0],
                                                              files[1])
            cache_file = self.cache_map[files]
            cache_file.add_function_pairs([v.names for v in vertices_in_file])

    def clear(self):
        for cache_file in self.cache_map.values():
            cache_file.clear()
        os.rmdir(self.directory)
