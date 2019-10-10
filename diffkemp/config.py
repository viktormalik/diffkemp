"""Configuration of the tool."""
import os


class ConfigException(Exception):
    pass


class Config:
    def __init__(self, source_first, source_second, show_diff,
                 enabled_patterns, disabled_patterns, print_asm_diffs,
                 verbosity, semdiff_tool):
        """
        Store configuration of DiffKemp
        :param source_first: Sources for the first kernel (instance of
                             KernelSource).
        :param source_second: Sources for the second kernel (instance of
                              KernelSource).
        :param show_diff: Only perform the syntax diff.
        :param control_flow_only: Check only for control-flow differences.
        :param verbosity: Verbosity level (currently boolean).
        :param semdiff_tool: Tool to use for semantic diff
        """
        self.source_first = source_first
        self.source_second = source_second
        self.show_diff = show_diff
        self.enabled_patterns = enabled_patterns
        self.disabled_patterns = disabled_patterns
        self.print_asm_diffs = print_asm_diffs
        self.verbosity = verbosity

        # Semantic diff tool configuration
        self.semdiff_tool = semdiff_tool
        if semdiff_tool == "llreve":
            self.timeout = 10
            if not os.path.isfile("build/llreve/reve/reve/llreve"):
                raise ConfigException("LLReve not built, try to re-run CMake \
                                       with -DBUILD_LLREVE=ON")
        elif semdiff_tool is not None:
            raise ConfigException("Unsupported semantic diff tool")
