//===----------------- Config.cpp - Parsing CLI options -------------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains definition of command line options, their parsing, and
/// setting the tool configuration.
///
//===----------------------------------------------------------------------===//

#include "Config.h"
#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>

// Command line options
cl::opt<std::string>
        FirstFileOpt(cl::Positional, cl::Required, cl::desc("<first file>"));
cl::opt<std::string>
        SecondFileOpt(cl::Positional, cl::Required, cl::desc("<second file>"));
cl::opt<std::string> FunctionOpt("fun",
                                 cl::value_desc("function"),
                                 cl::desc("Specify function to be analysed"));
cl::opt<std::string> VariableOpt(
        "var",
        cl::value_desc("variable"),
        cl::desc("Do analysis w.r.t. the value of the given variable"));
cl::opt<std::string>
        SuffixOpt("suffix",
                  cl::value_desc("suffix"),
                  cl::desc("Add suffix to names of simplified files."));
cl::opt<bool> PrintCallstacksOpt(
        "print-callstacks",
        cl::desc("Print call stacks for non-equal functions."));
cl::opt<bool>
        VerboseOpt("verbose",
                   cl::desc("Show verbose output (debugging information)."));
cl::opt<bool> PrintAsmDiffsOpt(
        "print-asm-diffs",
        cl::desc("Print raw differences in inline assembly code "
                 "(does not apply to macros)."));
// Syntax patterns options
cl::opt<bool> DisableAllPatterns(
        "disable-all-patterns",
        cl::desc(
                "Do not treat any syntactic difference as semantically equal"));
cl::opt<bool> EnableAllPatterns(
        "enable-all-patterns",
        cl::desc("Treat all supported patterns of syntactic differences as "
                 "semantically equal"));
cl::opt<bool> EnablePatternStructAlignment(
        "enable-pattern-struct-alignment",
        cl::desc("Treat differences in alignments of structures that do not "
                 "affect the accessed fields as semantically equal"));
cl::opt<bool> DisablePatternStructAlignment(
        "disable-pattern-struct-alignment",
        cl::desc("Always treat differences in alignments of structures as "
                 "semantically different"));
cl::opt<bool> EnablePatternFunctionSplits(
        "enable-pattern-function-splits",
        cl::desc("Treat differences caused by splitting code into functions as"
                 " semantically equal"));
cl::opt<bool> DisablePatternFunctionSplits(
        "disable-pattern-function-splits",
        cl::desc("Treat differences caused by splitting code into functions as"
                 " semantically different"));
cl::opt<bool> EnablePatternUnusedReturnTypes(
        "enable-pattern-unused-returns",
        cl::desc("Treat differences caused by changing an unused return type "
                 "to void as semantically equal"));
cl::opt<bool> DisablePatternUnusedReturnTypes(
        "disable-pattern-unused-returns",
        cl::desc("Treat differences caused by changing an unused return type "
                 "to void as semantically different"));
cl::opt<bool> EnablePatternKernelPrints(
        "enable-pattern-kernel-prints",
        cl::desc("Treat differences caused by changes in calls to kernel print"
                 " functions as semantically equal"));
cl::opt<bool> DisablePatternKernelPrints(
        "disable-pattern-kernel-prints",
        cl::desc("Treat differences caused by changes in calls to kernel print"
                 " functions as semantically different"));
cl::opt<bool> EnablePatternDeadCode(
        "enable-pattern-dead-code",
        cl::desc("Treat differences located in dead code as semantically "
                 "equal"));
cl::opt<bool> DisablePatternDeadCode(
        "disable-pattern-dead-code",
        cl::desc("Treat differences located in dead code as semantically "
                 "different"));
cl::opt<bool> EnablePatternNumericalMacros(
        "enable-pattern-numerical-macros",
        cl::desc("Treat differences caused by change of a value of a numerical"
                 " macro as semantically equal"));
cl::opt<bool> DisablePatternNumericalMacros(
        "disable-pattern-numerical-macros",
        cl::desc("Treat differences caused by change of a value of a numerical"
                 " macro as semantically different"));
cl::opt<bool> EnablePatternTypeCasts(
        "enable-pattern-type-casts",
        cl::desc("Treat differences in type casts as semantically equal"));
cl::opt<bool> DisablePatternTypeCasts(
        "disable-pattern-type-casts",
        cl::desc("Treat differences in type casts as semantically different"));
cl::opt<bool> EnablePatternControlFlowOnly(
        "enable-pattern-control-flow-only",
        cl::desc("Treat differences not caused by control flow related "
                 "operations as semantically equal"));
cl::opt<bool> DisablePatternControlFlowOnly(
        "disable-pattern-control-flow-only",
        cl::desc("Treat differences not caused by control flow related "
                 "operations as semantically different"));

/// Add suffix to the file name.
/// \param File Original file name.
/// \param Suffix Suffix to add.
/// \return File name with added suffix.
std::string addSuffix(std::string File, std::string Suffix) {
    unsigned long dotPos = File.find_last_of(".");
    return File.substr(0, dotPos) + "-" + Suffix + File.substr(dotPos);
}

/// Parsing command line options.
Config::Config()
        : First(parseIRFile(FirstFileOpt, err, context_first)),
          Second(parseIRFile(SecondFileOpt, err, context_second)),
          FirstOutFile(FirstFileOpt), SecondOutFile(SecondFileOpt),
          PrintAsmDiffs(PrintAsmDiffsOpt), PrintCallStacks(PrintCallstacksOpt) {
    if (!FunctionOpt.empty()) {
        // Parse --fun option - find functions with given names.
        // The option can be either single function name (same for both modules)
        // or two function names separated by a comma.
        auto commaPos = FunctionOpt.find(',');
        std::string first_name;
        std::string second_name;
        if (commaPos == std::string::npos) {
            first_name = FunctionOpt;
            second_name = FunctionOpt;
        } else {
            first_name = FunctionOpt.substr(0, commaPos);
            second_name = FunctionOpt.substr(commaPos + 1);
        }
        FirstFunName = first_name;
        SecondFunName = second_name;
        refreshFunctions();
    }
    if (!VariableOpt.empty()) {
        // Parse --var option - find global variables with given name.
        FirstVar = First->getGlobalVariable(VariableOpt, true);
        SecondVar = Second->getGlobalVariable(VariableOpt, true);
    }
    if (!SuffixOpt.empty()) {
        // Parse --suffix option - add suffix to the names of output files.
        FirstOutFile = addSuffix(FirstOutFile, SuffixOpt);
        SecondOutFile = addSuffix(SecondOutFile, SuffixOpt);
    }
    if (VerboseOpt) {
        // Enable debugging output in passes
        DebugFlag = true;
        setCurrentDebugType(DEBUG_SIMPLL);
    }

    // Set semantic patterns. Priorities are as follows:
    // 1. Explicit disablement of a single pattern.
    // 2. Explicit enablement of a single pattern.
    // 3. Explicit disablement of all patterns.
    // 4. Explicit enablement of all patterns.
    // 5. Default settings (see Config.h)
    if (EnableAllPatterns) {
        PatternStructAlignment = true;
        PatternFunctionSplits = true;
        PatternUnusedReturnTypes = true;
        PatternKernelPrints = true;
        PatternDeadCode = true;
        PatternNumericalMacros = true;
        PatternTypeCasts = true;
        PatternControlFlowOnly = true;
    }
    if (DisableAllPatterns) {
        PatternStructAlignment = false;
        PatternFunctionSplits = false;
        PatternUnusedReturnTypes = false;
        PatternKernelPrints = false;
        PatternDeadCode = false;
        PatternNumericalMacros = false;
        PatternTypeCasts = false;
        PatternControlFlowOnly = false;
    }
    if (EnablePatternStructAlignment)
        PatternStructAlignment = true;
    if (DisablePatternStructAlignment)
        PatternStructAlignment = false;
    if (EnablePatternFunctionSplits)
        PatternFunctionSplits = true;
    if (DisablePatternFunctionSplits)
        PatternFunctionSplits = false;
    if (EnablePatternUnusedReturnTypes)
        PatternUnusedReturnTypes = true;
    if (DisablePatternUnusedReturnTypes)
        PatternUnusedReturnTypes = false;
    if (EnablePatternKernelPrints)
        PatternKernelPrints = true;
    if (DisablePatternKernelPrints)
        PatternKernelPrints = false;
    if (EnablePatternDeadCode)
        PatternDeadCode = true;
    if (DisablePatternDeadCode)
        PatternDeadCode = false;
    if (EnablePatternNumericalMacros)
        PatternNumericalMacros = true;
    if (DisablePatternNumericalMacros)
        PatternNumericalMacros = false;
    if (EnablePatternTypeCasts)
        PatternTypeCasts = true;
    if (DisablePatternTypeCasts)
        PatternTypeCasts = false;
    if (EnablePatternControlFlowOnly) {
        PatternControlFlowOnly = true;
        // Turning on control-flow-only automatically turns on type casts
        PatternTypeCasts = true;
    }
    if (DisablePatternControlFlowOnly)
        PatternControlFlowOnly = false;
}

void Config::refreshFunctions() {
    FirstFun = First->getFunction(FirstFunName);
    SecondFun = Second->getFunction(SecondFunName);
}
