//===------------------ SimpLL.cpp - SimpLL entry point -------------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the main function of the SimpLL tool.
///
//===----------------------------------------------------------------------===//

#include "Config.h"
#include "ModuleAnalysis.h"
#include "ModuleComparator.h"
#include "Output.h"
#include "Utils.h"

using namespace llvm;

// Command line options
cl::OptionCategory SimpLLCategory("SimpLL options",
                                  "Options for controlling the SimpLL tool");
cl::opt<std::string> FirstFileOpt(cl::Positional,
                                  cl::Required,
                                  cl::desc("<first file>"),
                                  cl::cat(SimpLLCategory));
cl::opt<std::string> SecondFileOpt(cl::Positional,
                                   cl::Required,
                                   cl::desc("<second file>"),
                                   cl::cat(SimpLLCategory));
cl::opt<std::string> FunctionOpt("fun",
                                 cl::value_desc("function"),
                                 cl::desc("Specify function to be analysed"),
                                 cl::cat(SimpLLCategory));
cl::opt<std::string> VariableOpt(
        "var",
        cl::value_desc("variable"),
        cl::desc("Do analysis w.r.t. the value of the given variable"),
        cl::cat(SimpLLCategory));
cl::opt<bool>
        OutputLlvmIROpt("output-llvm-ir",
                        cl::desc("Output each simplified module to a file."),
                        cl::cat(SimpLLCategory));
cl::opt<std::string>
        SuffixOpt("suffix",
                  cl::value_desc("suffix"),
                  cl::desc("Add suffix to names of simplified files."),
                  cl::cat(SimpLLCategory));
cl::opt<std::string> CacheDirOpt(
        "cache-dir",
        cl::value_desc("cache-dir"),
        cl::desc("Directory containing a SimpLL cache generated by DiffKemp."),
        cl::cat(SimpLLCategory));
cl::opt<std::string> CustomPatternConfigOpt(
        "custom-pattern-config",
        cl::value_desc("custom-pattern-config"),
        cl::desc("Configuration file for custom LLVM IR difference patterns."),
        cl::cat(SimpLLCategory));
cl::opt<bool> UseSMTOpt("use-smt",
                        cl::desc("Use SMT-based checking of code snippets."),
                        cl::cat(SimpLLCategory));
cl::opt<unsigned> SMTTimeoutOpt("smt-timeout",
                                cl::desc("Set timeout for --use-smt option. "
                                         "Set to 0 to prevent timing out."),
                                cl::cat(SimpLLCategory));
cl::opt<bool> PrintCallstacksOpt(
        "print-callstacks",
        cl::desc("Print call stacks for non-equal functions."),
        cl::cat(SimpLLCategory));
cl::opt<int> VerbosityOpt(
        "verbosity",
        cl::desc("Level of verbose output (debugging information)."),
        cl::cat(SimpLLCategory));
cl::opt<bool> PrintAsmDiffsOpt(
        "print-asm-diffs",
        cl::desc("Print raw differences in inline assembly code "
                 "(does not apply to macros)."),
        cl::cat(SimpLLCategory));
cl::opt<bool> ExtendedStats("extended-stat",
                            cl::desc("Track extended statistics "
                                     "(may be more expensive to compute)."),
                            cl::cat(SimpLLCategory));

cl::OptionCategory BuiltinPatternsCategory("SimpLL pattern options",
                                           "Options for configuring built-in "
                                           "semantics-preserving patterns");
cl::opt<bool> StructAlignmentOpt("struct-alignment",
                                 cl::desc("Enable struct alignment pattern."),
                                 cl::cat(BuiltinPatternsCategory));
cl::opt<bool> FunctionSplitsOpt("function-splits",
                                cl::desc("Enable function splits pattern."),
                                cl::cat(BuiltinPatternsCategory));
cl::opt<bool>
        UnusedReturnTypesOpt("unused-return-types",
                             cl::desc("Enable unused return types pattern."),
                             cl::cat(BuiltinPatternsCategory));
cl::opt<bool> KernelPrintsOpt("kernel-prints",
                              cl::desc("Enable kernel prints pattern."),
                              cl::cat(BuiltinPatternsCategory));
cl::opt<bool> DeadCodeOpt("dead-code",
                          cl::desc("Enable dead code pattern."),
                          cl::cat(BuiltinPatternsCategory));
cl::opt<bool> NumericalMacrosOpt("numerical-macros",
                                 cl::desc("Enable numerical macros pattern."),
                                 cl::cat(BuiltinPatternsCategory));
cl::opt<bool> RelocationsOpt("relocations",
                             cl::desc("Enable relocations pattern."),
                             cl::cat(BuiltinPatternsCategory));
cl::opt<bool> TypeCastsOpt("type-casts",
                           cl::desc("Enable type casts pattern."),
                           cl::cat(BuiltinPatternsCategory));
cl::opt<bool> ControlFlowOnlyOpt("control-flow-only",
                                 cl::desc("Enable control flow only pattern."),
                                 cl::cat(BuiltinPatternsCategory));
cl::opt<bool>
        InverseConditionsOpt("inverse-conditions",
                             cl::desc("Enable inverse conditions pattern."),
                             cl::cat(BuiltinPatternsCategory));
cl::opt<bool> ReorderedBinOpsOpt(
        "reordered-bin-ops",
        cl::desc("Enable reordered binary operations pattern."),
        cl::cat(BuiltinPatternsCategory));
cl::opt<bool> GroupVarsOpt("group-vars",
                           cl::desc("Enable variable grouping pattern."),
                           cl::cat(BuiltinPatternsCategory));

/// Add suffix to the file name.
/// \param File Original file name.
/// \param Suffix Suffix to add.
/// \return File name with added suffix.
std::string addSuffix(std::string File, std::string Suffix) {
    unsigned long dotPos = File.find_last_of(".");
    return File.substr(0, dotPos) + "-" + Suffix + File.substr(dotPos);
}

/// Parse --fun option - find functions with given names.
/// The option can be either single function name (same for both modules)
/// or two function names separated by a comma.
/// \return Name of function in first and second module.
std::pair<std::string, std::string> parseFunOption() {
    std::string first_name;
    std::string second_name;

    if (!FunctionOpt.empty()) {
        auto commaPos = FunctionOpt.find(',');
        if (commaPos == std::string::npos) {
            first_name = FunctionOpt;
            second_name = FunctionOpt;
        } else {
            first_name = FunctionOpt.substr(0, commaPos);
            second_name = FunctionOpt.substr(commaPos + 1);
        }
    }

    return {first_name, second_name};
}

int main(int argc, const char **argv) {
    // Parse CLI options
    cl::HideUnrelatedOptions({&SimpLLCategory, &BuiltinPatternsCategory});
    cl::ParseCommandLineOptions(argc, argv);

    BuiltinPatterns Patterns{.StructAlignment = StructAlignmentOpt,
                             .FunctionSplits = FunctionSplitsOpt,
                             .UnusedReturnTypes = UnusedReturnTypesOpt,
                             .KernelPrints = KernelPrintsOpt,
                             .DeadCode = DeadCodeOpt,
                             .NumericalMacros = NumericalMacrosOpt,
                             .Relocations = RelocationsOpt,
                             .TypeCasts = TypeCastsOpt,
                             .ControlFlowOnly = ControlFlowOnlyOpt,
                             .InverseConditions = InverseConditionsOpt,
                             .ReorderedBinOps = ReorderedBinOpsOpt,
                             .GroupVars = GroupVarsOpt};

    // Parse --fun option
    auto FunName = parseFunOption();

    // Load modules and create Config based on given CLI options
    LLVMContext context_first, context_second;
    SMDiagnostic err;
    std::unique_ptr<Module> FirstModule(
            parseIRFile(FirstFileOpt, err, context_first));
    std::unique_ptr<Module> SecondModule(
            parseIRFile(SecondFileOpt, err, context_second));
    Config config(FunName.first,
                  FunName.second,
                  FirstModule.get(),
                  SecondModule.get(),
                  !SuffixOpt.empty() ? addSuffix(FirstFileOpt, SuffixOpt) : "",
                  !SuffixOpt.empty() ? addSuffix(SecondFileOpt, SuffixOpt) : "",
                  CacheDirOpt,
                  CustomPatternConfigOpt,
                  Patterns,
                  UseSMTOpt,
                  SMTTimeoutOpt,
                  VariableOpt,
                  OutputLlvmIROpt,
                  PrintAsmDiffsOpt,
                  PrintCallstacksOpt,
                  ExtendedStats,
                  VerbosityOpt);

    // Run transformations and the comparison.
    OverallResult Result;
    processAndCompare(config, Result);

    // Report the result to standard output.
    reportOutput(Result);

    llvm_shutdown();
    return 0;
}
