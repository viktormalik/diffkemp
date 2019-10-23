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
cl::opt<bool> ControlFlowOpt(
        "control-flow",
        cl::desc("Only keep instructions related to the control-flow."));
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
          PatternControlFlowOnly(ControlFlowOpt),
          PrintAsmDiffs(PrintAsmDiffsOpt),
          PrintCallStacks(PrintCallstacksOpt) {
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
}

void Config::refreshFunctions() {
    FirstFun = First->getFunction(FirstFunName);
    SecondFun = Second->getFunction(SecondFunName);
}
