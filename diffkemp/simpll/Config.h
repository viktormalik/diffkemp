//===------------------ Config.h - Parsing CLI options --------------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of the Config class that stores parsed
/// command line options.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_CONFIG_H
#define DIFFKEMP_SIMPLL_CONFIG_H

#include "llvm/Support/CommandLine.h"
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>

#define DEBUG_SIMPLL "debug-simpll"

using namespace llvm;

// Declaration of command line options
extern cl::opt<std::string> FirstFileOpt;
extern cl::opt<std::string> SecondFileOpt;
extern cl::opt<std::string> FunctionOpt;
extern cl::opt<std::string> VariableOpt;
extern cl::opt<std::string> SuffixOpt;
extern cl::opt<bool> ControlFlowOpt;
extern cl::opt<bool> PrintCallstacksOpt;
extern cl::opt<bool> VerboseOpt;

/// Tool configuration parsed from CLI options.
class Config {
  private:
    SMDiagnostic err;
    LLVMContext context_first;
    LLVMContext context_second;

    std::string FirstFunName;
    std::string SecondFunName;

  public:
    // Parsed LLVM modules
    std::unique_ptr<Module> First;
    std::unique_ptr<Module> Second;
    // Compared functions
    Function *FirstFun = nullptr;
    Function *SecondFun = nullptr;
    // Compared global variables
    GlobalVariable *FirstVar = nullptr;
    GlobalVariable *SecondVar = nullptr;
    // Output files
    std::string FirstOutFile;
    std::string SecondOutFile;

    // Print raw differences in inline assembly.
    bool PrintAsmDiffs;
    // Show call stacks for non-equal functions
    bool PrintCallStacks;

    // The following boolean variables specify which patterns of syntactic
    // changes should be treated as semantically equal.

    // Patterns that are known to be semantically equal (turned on by default):

    // Changes in structure alignment
    bool PatternStructAlignment = true;
    // Splitting code into functions
    bool PatternFunctionSplits = true;
    // Changing unused return values to void
    bool PatternUnusedReturnTypes = true;
    // Changes in kernel-specific printing functions calls. These include:
    // - changes in strings printed by kernel print functions
    // - changes in arguments of kernel functions that are related to the call
    //   call location (file name and line number)
    // - changes in counter, date, time, file name, and line macros
    bool PatternKernelPrints = true;
    // Changes in dead code
    bool PatternDeadCode = true;
    // Changed numerical value of a macro
    bool PatternNumericalMacros = true;

    // Patterns that are not semantically equal (turned off by default):

    // Changes in type casts
    bool PatternTypeCasts = false;
    // Ignore all changes except those in control-flow
    bool PatternControlFlowOnly = false;

    Config();

    void refreshFunctions();
};

#endif // DIFFKEMP_SIMPLL_CONFIG_H
