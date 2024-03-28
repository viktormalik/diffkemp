"""Unit tests for the ComparisonGraph and SimpLLCache class."""

from diffkemp.semdiff.caching import ComparisonGraph, SimpLLCache
from diffkemp.semdiff.result import Result
from tempfile import mkdtemp
import os
import pytest
from conftest import dup


def test_add_vertex_strong(graph):
    """Tests adding a strong vertex to the graph."""
    graph["test"] = ComparisonGraph.Vertex(
        dup("test"), Result.Kind.EQUAL, dup("app/main.c"), (81, 82)
    )
    assert "test" in graph.vertices
    assert graph["test"].names == dup("test")
    assert graph["test"].result == Result.Kind.EQUAL
    assert graph["test"].files == dup("app/main.c")
    assert graph["test"].lines == (81, 82)
    assert graph["test"] not in graph._weak_vertex_cache


def test_add_vertex_weak(graph):
    """Tests adding a weak vertex to the graph."""
    graph["test.void"] = ComparisonGraph.Vertex(
        ("test", "test.void"), Result.Kind.EQUAL, dup("app/main.c"), (81, 82)
    )
    assert "test.void" in graph.vertices
    assert graph["test.void"].names == ("test", "test.void")
    assert graph["test.void"].result == Result.Kind.EQUAL
    assert graph["test.void"].files == dup("app/main.c")
    assert graph["test.void"].lines == (81, 82)
    assert graph["test.void"] in graph._weak_vertex_cache


def test_add_edge_strong(graph):
    """Tests adding a strong edge to a graph."""
    for side in ComparisonGraph.Side:
        graph.add_edge(graph["main_function"], side,
                       ComparisonGraph.Edge("missing", "app/main.c", 61))
        assert "missing" in [edge.target_name for edge in
                             graph["main_function"].successors[side]]
        assert "missing" not in [edge.target_name for edge in
                                 graph._normalize_edge_cache]


def test_add_edge_weak(graph):
    """Tests adding a weak edge to a graph."""
    graph.add_edge(graph["main_function"], ComparisonGraph.Side.LEFT,
                   ComparisonGraph.Edge("strength", "app/main.c", 61))
    graph.add_edge(graph["main_function"], ComparisonGraph.Side.RIGHT,
                   ComparisonGraph.Edge("strength.void", "app/main.c", 61))
    left_succesor_names = [edge.target_name for edge in
                           graph["main_function"].successors[
                                                  ComparisonGraph.Side.LEFT]]
    right_successor_names = [edge.target_name for edge in
                             graph["main_function"].successors[
                                            ComparisonGraph.Side.RIGHT]]
    assert "strength" in left_succesor_names
    assert "strength.void" in right_successor_names
    assert "strength" not in right_successor_names
    assert "strength.void" not in left_succesor_names
    assert "strength.void" in [edge.target_name
                               for edge in graph._normalize_edge_cache]


def test_reachable_from_basic(graph):
    """Tests called function list generation (without normalization)."""
    reachable_l, map_l = graph.reachable_from(ComparisonGraph.Side.LEFT,
                                              "do_check")
    reachable_r, map_r = graph.reachable_from(ComparisonGraph.Side.RIGHT,
                                              "do_check")
    assert (set([v.names[ComparisonGraph.Side.LEFT] for v in reachable_l]) ==
            {"do_check", "looping", "strength", "main_function",
            "side_function"})
    # Note: in the right module the void-returning variant "strength.void" is
    # used alongside the original one.
    # This is solved by normalization (and tested later).
    assert (set([v.names[ComparisonGraph.Side.RIGHT] for v in reachable_r]) ==
            {"do_check", "looping", "strength", "main_function",
             "side_function", "strength.void"})
    # Test backtracking graphs for callstack generation.
    assert (set([v.names for v in map_l.keys()]) ==
            {dup("looping"), dup("main_function"), dup("strength"),
             dup("side_function")})
    assert (set([v.names for v in map_r.keys()]) ==
            {dup("looping"), dup("main_function"), dup("side_function"),
             dup("strength"), ("strength", "strength.void")})
    for backtracking_map in [map_l, map_r]:
        assert (backtracking_map[graph["looping"]].parent_vertex.names ==
                dup("do_check"))
        assert (backtracking_map[graph["main_function"]].parent_vertex.names ==
                dup("looping"))
        assert (backtracking_map[graph["strength"]].parent_vertex.names ==
                dup("looping"))
        assert (backtracking_map[graph["side_function"]].parent_vertex.names ==
                dup("main_function"))


def test_absort_graph(graph):
    """Tests the absorb graph function, especially whether vertex replacing
    works properly."""
    new_graph = ComparisonGraph()
    # This vertex should replace the old one (which is assumed equal).
    new_graph["missing"] = ComparisonGraph.Vertex(
        dup("missing"), Result.Kind.NOT_EQUAL, dup("app/mod.c"), dup(665)
    )
    # This vertex should not replace the old one.
    new_graph["do_check"] = ComparisonGraph.Vertex(
        dup("do_check"), Result.Kind.EQUAL, dup("app/mod.c"), dup(665)
    )
    # This vertex should replace the old one (it has more successors).
    new_graph["strength"] = ComparisonGraph.Vertex(
        dup("strength"), Result.Kind.NOT_EQUAL, dup("app/test.h"), (5, 5)
    )
    for side in ComparisonGraph.Side:
        new_graph.add_edge(new_graph["strength"], side,
                           ComparisonGraph.Edge("missing", "app/w.c", 6))
        new_graph.add_edge(new_graph["strength"], side,
                           ComparisonGraph.Edge("main_function", "app/w.c", 7))
    graph.absorb_graph(new_graph)
    assert graph["missing"].result == Result.Kind.NOT_EQUAL
    assert graph["do_check"].result == Result.Kind.NOT_EQUAL
    assert graph["strength"].result == Result.Kind.NOT_EQUAL


def test_normalize(graph):
    """Tests whether the conditions for a normalized graph hold after the
    normalization."""
    graph.normalize()
    # Check whether all vertices are strong.
    assert all(["." not in v.names[side] for v in graph.vertices.values()
                for side in ComparisonGraph.Side])
    # Check whether all edges point to existing vertices.
    assert all([e.target_name in graph.vertices
               for side in ComparisonGraph.Side
               for vertex in graph.vertices.values()
               for e in vertex.successors[side]])
    # Check kind of the weak and strong calls.
    assert any([e.kind == ComparisonGraph.DependencyKind.STRONG
                for side in ComparisonGraph.Side
                for e in graph["looping"].successors[side]
                if e.target_name == "strength"])
    assert any([e.kind == ComparisonGraph.DependencyKind.WEAK
                for side in ComparisonGraph.Side
                for e in graph["side_function"].successors[side]
                if e.target_name == "strength"])


def test_reachable_from_extended(graph):
    """
    Tests called function list generation with weak edges.
    Note: to generate weak edges the normalize method has to be used before,
    therefore this test doesn't make sense when the one for normalize fails.
    """
    graph.normalize()
    for side in ComparisonGraph.Side:
        reachable, _ = graph.reachable_from(side, "side_function")
        # The weakly dependent "strength" function should not be in the set.
        assert (set([v.names[side] for v in reachable]) ==
                {"side_function"})


def test_graph_to_fun_pair_list(graph):
    """Tests the conversion of a graph to a structure representing the output
    of DiffKemp."""
    objects_to_compare, syndiff_bodies_left, syndiff_bodies_right = \
        graph.graph_to_fun_pair_list("main_function", "main_function", False)
    for side in [0, 1]:
        assert {obj[side].name for obj in objects_to_compare} == {
            "do_check", "___MACRO", "struct_file"}
        do_check = [obj[side] for obj in objects_to_compare
                    if obj[side].name == "do_check"][0]
        macro = [obj[side] for obj in objects_to_compare
                 if obj[side].name == "___MACRO"][0]
        struct_file = [obj[side] for obj in objects_to_compare
                       if obj[side].name == "struct_file"][0]
        assert do_check.filename == "app/main.c"
        assert do_check.line == 105
        assert str(do_check.callstack) == "do_check at app/main.c:58"
        assert do_check.diff_kind == "function"
        assert do_check.covered
        assert macro.filename is None
        assert macro.line is None
        assert str(macro.callstack) == ("do_check at app/main.c:58\n"
                                        "_MACRO (macro) at test.c:1\n"
                                        "__MACRO (macro) at test.c:2\n"
                                        "___MACRO (macro) at test.c:3")
        assert macro.diff_kind == "syntactic"
        assert not macro.covered
        assert struct_file.filename == "include/file.h"
        assert struct_file.line == 121
        assert str(struct_file.callstack) == (
            "do_check at app/main.c:58\n"
            "struct_file (type) at include/file.h:121")
        assert struct_file.diff_kind == "type"
        assert not struct_file.covered
    # All results should be not equal.
    assert {obj[2] for obj in objects_to_compare} == {Result.Kind.NOT_EQUAL}
    assert syndiff_bodies_left == {"___MACRO": "5"}
    assert syndiff_bodies_right == {"___MACRO": "5L"}


def test_populate_predecessor_lists(graph):
    """Tests whether all predecessors are recorded."""
    graph.normalize()
    graph.populate_predecessor_lists()
    for side in ComparisonGraph.Side:
        assert ({v.names for v in graph["main_function"].predecessors[side]}
                == {dup("looping")})
        assert ({v.names for v in graph["do_check"].predecessors[side]} ==
                {dup("main_function")})
        assert ({v.names for v in graph["missing"].predecessors[side]} ==
                {dup("do_check"), dup("strength")})
        assert ({v.names for v in graph["looping"].predecessors[side]} ==
                {dup("do_check")})
        assert ({v.names for v in graph["strength"].predecessors[side]} ==
                {dup("looping"), dup("side_function")})


@pytest.fixture
def graph_uncachable():
    """Graph used to test the marking of uncachable vertices."""
    graph = ComparisonGraph()
    graph["f1"] = ComparisonGraph.Vertex(
        dup("f1"), Result.Kind.EQUAL, dup("app/f1.c"), dup(10)
    )
    graph["f2"] = ComparisonGraph.Vertex(
        dup("f2"), Result.Kind.EQUAL, dup("include/h1.h"), dup(20)
    )
    graph["f3"] = ComparisonGraph.Vertex(
        dup("f3"), Result.Kind.ASSUMED_EQUAL, dup("app/f2.c"), dup(20)
    )
    for side in ComparisonGraph.Side:
        graph.add_edge(graph["f1"], side, ComparisonGraph.Edge("f2",
                       "app/f1.c", 11))
        graph.add_edge(graph["f2"], side, ComparisonGraph.Edge("f3",
                       "include/h1.c", 21))
    yield graph


def test_mark_uncachable_from_assumed_equal(graph_uncachable):
    """Tests the marking of function in headers followed by an assumed equal
    function in a cache file as uncachable."""
    graph_uncachable.normalize()
    graph_uncachable.populate_predecessor_lists()
    assert graph_uncachable["f2"].cachable
    graph_uncachable.mark_uncachable_from_assumed_equal()
    assert graph_uncachable["f1"].cachable
    assert graph_uncachable["f3"].cachable
    assert not graph_uncachable["f2"].cachable


def test_cachability_reset_after_absorb(graph_uncachable):
    """Tests whether the cachable attribute is reset to true after replacing
    the assumed equal vertex causing the uncachability."""
    graph_uncachable.normalize()
    graph_uncachable.populate_predecessor_lists()
    graph_uncachable.mark_uncachable_from_assumed_equal()
    assert not graph_uncachable["f2"].cachable
    graph_to_merge = ComparisonGraph()
    graph_to_merge["f3"] = ComparisonGraph.Vertex(
        dup("f3"), Result.Kind.NOT_EQUAL, dup("app/f2.c"), dup(20)
    )
    graph_uncachable.absorb_graph(graph_to_merge)
    assert graph_uncachable["f2"].cachable


@pytest.fixture
def cache_file():
    yield SimpLLCache.CacheFile(mkdtemp(), "/test/f1/1.ll", "/test/f2/2.ll")


def test_cache_file_init(cache_file):
    """Tests the constructor of the SimpLLCache.CacheFile class."""
    assert cache_file.left_module == "/test/f1/1.ll"
    assert cache_file.right_module == "/test/f2/2.ll"
    assert cache_file.filename.endswith("..$f1$1.ll:..$f2$2.ll")


def test_cache_file_add_function_pairs(cache_file):
    """Tests adding function pairs to a single cache file."""
    cache_file.add_function_pairs([("f1", "f2"), ("g1", "g2")])
    with open(cache_file.filename, "r") as file:
        assert file.readlines() == ["f1:f2\n", "g1:g2\n"]


def test_cache_file_clear(cache_file):
    """Tests the clear method which deletes the cache file."""
    cache_file.add_function_pairs([("f1", "f2")])
    cache_file.clear()
    assert not os.path.exists(cache_file.filename)


@pytest.fixture
def simpll_cache():
    yield SimpLLCache(mkdtemp())


@pytest.fixture
def vertices():
    yield [ComparisonGraph.Vertex(dup("f"),
                                  Result.Kind.EQUAL,
                                  ("/test/f1/1.ll", "/test/f2/2.ll")),
           ComparisonGraph.Vertex(dup("h"),
                                  Result.Kind.NOT_EQUAL,
                                  ("/test/f1/1.ll", "/test/f2/3.ll")),
           ComparisonGraph.Vertex(dup("g"),
                                  Result.Kind.NOT_EQUAL,
                                  ("/test/f1/1.ll", "/test/f2/2.ll"))]


def test_simpll_cache_update(simpll_cache, vertices):
    """Tests updating of a SimpLL cache with vertices from a graph."""
    simpll_cache.update(vertices)
    assert os.path.exists(os.path.join(simpll_cache.directory,
                                       "..$f1$1.ll:..$f2$2.ll"))
    assert os.path.exists(os.path.join(simpll_cache.directory,
                                       "..$f1$1.ll:..$f2$3.ll"))
    with open(os.path.join(simpll_cache.directory,
                           "..$f1$1.ll:..$f2$2.ll"), "r") as file:
        assert file.readlines() == ["f:f\n", "g:g\n"]
    with open(os.path.join(simpll_cache.directory,
                           "..$f1$1.ll:..$f2$3.ll"), "r") as file:
        assert file.readlines() == ["h:h\n"]


def test_simpll_cache_clear(simpll_cache, vertices):
    """Tests the clear method which deletes the entire cache directory."""
    simpll_cache.update(vertices)
    simpll_cache.clear()
    assert not os.path.exists(simpll_cache.directory)
