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

#include <vector>

#include "llvm/Support/CommandLine.h"
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>

#define DEBUG_SIMPLL "debug-simpll"
#define DEBUG_SIMPLL_MACROS "debug-simpll-macros"

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
extern cl::opt<bool> VerboseMacrosOpt;
extern cl::opt<bool> NoMissingDefsInPreprocessOpt;
extern cl::list<int> IndicesOpt;

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
    // Cache file directory.
    std::string CacheDir;

    // Save the simplified IR of the module to a file.
    bool OutputLlvmIR;
    // Keep only control-flow related instructions
    bool ControlFlowOnly;
    // Print raw differences in inline assembly.
    bool PrintAsmDiffs;
    // Show call stacks for non-equal functions
    bool PrintCallStacks;
    // Don't print missingDefs in preprocess phase
    bool NoMissingDefsInPreprocess;

    // Indices for GEP instuction dependency check
    std::vector<int> Indices;

    // Constructor for command-line use.
    Config();
    // Constructor for other use than from the command line.
    Config(std::string FirstFunName,
           std::string SecondFunName,
           std::string FirstModule,
           std::string SecondModule,
           std::string FirstOutFile,
           std::string SecondOutFile,
           std::string CacheDir,
           std::string Variable = "",
           bool OutputLlvmIR = false,
           bool ControlFlowOnly = false,
           bool PrintAsmDiffs = true,
           bool PrintCallStacks = true,
           bool Verbose = false,
           bool VerboseMacros = false);
    // Constructor without module loading (for tests).
    Config(std::string FirstFunName,
           std::string SecondFunName,
           std::string CacheDir,
           bool ControlFlowOnly = false,
           bool PrintAsmDiffs = true,
           bool PrintCallStacks = true)
            : First(nullptr), Second(nullptr), FirstFunName(FirstFunName),
              SecondFunName(SecondFunName), FirstOutFile("/dev/null"),
              SecondOutFile("/dev/null"), CacheDir(CacheDir),
              ControlFlowOnly(ControlFlowOnly), PrintAsmDiffs(PrintAsmDiffs),
              PrintCallStacks(PrintCallStacks) {}

    /// Sets debug types specified in the vector.
    void setDebugTypes(std::vector<std::string> &debugTypes);

    void refreshFunctions();
};

#endif // DIFFKEMP_SIMPLL_CONFIG_H
