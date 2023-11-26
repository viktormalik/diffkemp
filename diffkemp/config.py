"""Configuration of the tool."""
from diffkemp.semdiff.custom_pattern_config import CustomPatternConfig
from diffkemp.snapshot import Snapshot
from diffkemp.simpll.simpll_lib import ffi
import os


class ConfigException(Exception):
    pass


class BuiltinPatterns:
    def __init__(
        self,
        struct_alignment=True,
        function_splits=True,
        unused_return_types=True,
        kernel_prints=True,
        dead_code=True,
        numerical_macros=True,
        relocations=True,
        type_casts=False,
        control_flow_only=False,
        inverse_conditions=True,
        reordered_bin_ops=True,
        group_vars=True,
    ):
        """
        Create a configuration of built-in patterns.
        :param struct_alignment: Changes in structure alignment.
        :param function_splits: Splitting code into functions.
        :param unused_return_types: Changing unused return values to void.
        :param kernel_prints: Changes in kernel-specific printing functions:
            - changes in strings printed by kernel print functions
            - changes in arguments of kernel functions that are related to the
              call location (file name and line number)
            - changes in counter, date, time, file name, and line macros
        :param dead_code: Changes in dead code.
        :param numerical_macros: Changed numerical value of a macro.
        :param relocations: Relocated instructions.
        :param type_casts: Changes in type casts.
        :param control_flow_only: Consider control-flow changes only.
        :param inverse_conditions: Inverted branch conditions.
        :param reordered_bin_ops: Match reordered binary operations.
        :param group_vars: Grouping of local variables.
        """
        self.settings = {
            "struct-alignment": struct_alignment,
            "function-splits": function_splits,
            "unused-return-types": unused_return_types,
            "kernel-prints": kernel_prints,
            "dead-code": dead_code,
            "numerical-macros": numerical_macros,
            "relocations": relocations,
            "type-casts": type_casts,
            "control-flow-only": control_flow_only,
            "inverse-conditions": inverse_conditions,
            "reordered-bin-ops": reordered_bin_ops,
            "group-vars": group_vars,
        }
        self.resolve_dependencies()

    def update_from_args(self, args):
        """
        Update the configuration based on Diffkemp command line arguments.
        :param args: Diffkemp command line arguments.
        """
        if "all" in args.enable_pattern:
            for pattern in self.settings.keys():
                self.settings[pattern] = True

        if "all" in args.disable_pattern:
            for pattern in self.settings.keys():
                self.settings[pattern] = False

        for pattern in self.settings.keys():
            if pattern in args.enable_pattern:
                self.settings[pattern] = True
            if pattern in args.disable_pattern:
                self.settings[pattern] = False

        self.resolve_dependencies()

    def resolve_dependencies(self):
        """
        Resolve dependencies between built-in patterns.
        """
        if self.settings["control-flow-only"]:
            self.settings["type-casts"] = True

    def as_ffi_struct(self):
        """
        Return the FFI representation of the built-in pattern configuration.
        """
        ffi_struct = ffi.new("struct builtin_patterns *")
        ffi_struct.StructAlignment = self.settings["struct-alignment"]
        ffi_struct.FunctionSplits = self.settings["function-splits"]
        ffi_struct.UnusedReturnTypes = self.settings["unused-return-types"]
        ffi_struct.KernelPrints = self.settings["kernel-prints"]
        ffi_struct.DeadCode = self.settings["dead-code"]
        ffi_struct.NumericalMacros = self.settings["numerical-macros"]
        ffi_struct.Relocations = self.settings["relocations"]
        ffi_struct.TypeCasts = self.settings["type-casts"]
        ffi_struct.ControlFlowOnly = self.settings["control-flow-only"]
        ffi_struct.InverseConditions = self.settings["inverse-conditions"]
        ffi_struct.ReorderedBinOps = self.settings["reordered-bin-ops"]
        ffi_struct.GroupVars = self.settings["group-vars"]
        return ffi_struct


class Config:
    def __init__(
        self,
        snapshot_first=None,
        snapshot_second=None,
        show_diff=True,
        full_diff=False,
        output_llvm_ir=False,
        custom_pattern_config=None,
        builtin_patterns=BuiltinPatterns(),
        use_smt=False,
        print_asm_diffs=False,
        extended_stat=False,
        verbosity=0,
        use_ffi=False,
        semdiff_tool=None,
    ):
        """
        Store configuration of DiffKemp
        :param snapshot_first: First snapshot representation.
        :param snapshot_second: Second snapshot representation.
        :param show_diff: Evaluate syntax differences.
        :param full_diff: Evaluate semantics-preserving syntax differences too.
        :param output_llvm_ir: Output each simplified module into a file.
        :param custom_pattern_config: Valid custom pattern configuration.
        :param use_smt: Enable SMT-based checking of short snippets.
        :param builtin_patterns: Configuration of built-in patterns.
        :param print_asm_diffs: Print assembly differences.
        :param extended_stat: Gather extended statistics.
        :param verbosity: Verbosity level (currently boolean).
        :param use_ffi: Use Python FFI to call SimpLL.
        :param semdiff_tool: Tool to use for semantic diff.
        """

        self.snapshot_first = snapshot_first
        self.snapshot_second = snapshot_second
        self.show_diff = show_diff
        self.full_diff = full_diff
        self.output_llvm_ir = output_llvm_ir
        self.custom_pattern_config = custom_pattern_config
        self.builtin_patterns = builtin_patterns
        self.use_smt = use_smt
        self.print_asm_diffs = print_asm_diffs
        self.extended_stat = extended_stat
        self.verbosity = verbosity
        self.use_ffi = use_ffi

        # Semantic diff tool configuration
        self.semdiff_tool = semdiff_tool
        if semdiff_tool == "llreve":
            self.timeout = 10
            if not os.path.isfile("build/llreve/reve/reve/llreve"):
                raise ConfigException("LLReve not built, try to re-run CMake \
                                       with -DBUILD_LLREVE=ON")
        elif semdiff_tool is not None:
            raise ConfigException("Unsupported semantic diff tool")

    @classmethod
    def from_args(cls, args):
        """
        Create the configuration from command line arguments.
        :param args: Command line arguments
        """
        # Parse both the new and the old snapshot.
        snapshot_first = Snapshot.load_from_dir(args.snapshot_dir_old)
        snapshot_second = Snapshot.load_from_dir(args.snapshot_dir_new)

        if args.function:
            snapshot_first.filter([args.function])
            snapshot_second.filter([args.function])

        # Create the configuration of built-in patterns.
        builtin_patterns = BuiltinPatterns()
        builtin_patterns.update_from_args(args)

        # Transform difference pattern files into an LLVM IR
        # based configuration.
        if args.custom_patterns:
            custom_pattern_config = CustomPatternConfig.create_from_file(
                args.custom_patterns
            )
        else:
            custom_pattern_config = None

        return cls(
            snapshot_first=snapshot_first,
            snapshot_second=snapshot_second,
            show_diff=not args.no_show_diff or args.show_diff,
            full_diff=args.full_diff,
            custom_pattern_config=custom_pattern_config,
            builtin_patterns=builtin_patterns,
            use_smt=args.use_smt,
            output_llvm_ir=args.output_llvm_ir,
            print_asm_diffs=args.print_asm_diffs,
            extended_stat=args.extended_stat,
            verbosity=args.verbose,
            use_ffi=not args.disable_simpll_ffi,
            semdiff_tool=args.semdiff_tool,
        )
