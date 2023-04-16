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
#define DEBUG_SIMPLL_VERBOSE "debug-simpll-verbose"
#define DEBUG_SIMPLL_VERBOSE_EXTRA "debug-simpll-verbose-extra"

using namespace llvm;

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
    Module *First;
    Module *Second;
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
    // Path to LLVM IR pattern configuration.
    std::string PatternConfigPath;

    // Save the simplified IR of the module to a file.
    bool OutputLlvmIR;
    // Keep only control-flow related instructions
    bool ControlFlowOnly;
    // Print raw differences in inline assembly.
    bool PrintAsmDiffs;
    // Show call stacks for non-equal functions
    bool PrintCallStacks;

    Config(std::string FirstFunName,
           std::string SecondFunName,
           Module *FirstModule,
           Module *SecondModule,
           std::string FirstOutFile = "",
           std::string SecondOutFile = "",
           std::string CacheDir = "",
           std::string PatternConfigPath = "",
           std::string Variable = "",
           bool OutputLlvmIR = false,
           bool ControlFlowOnly = false,
           bool PrintAsmDiffs = true,
           bool PrintCallStacks = true,
           int Verbosity = 0);
    // Constructor without module loading (for tests).
    Config(std::string FirstFunName,
           std::string SecondFunName,
           std::string CacheDir,
           std::string PatternConfigPath,
           bool ControlFlowOnly = false,
           bool PrintAsmDiffs = true,
           bool PrintCallStacks = true)
            : FirstFunName(FirstFunName), SecondFunName(SecondFunName),
              First(nullptr), Second(nullptr), FirstOutFile("/dev/null"),
              SecondOutFile("/dev/null"), CacheDir(CacheDir),
              PatternConfigPath(PatternConfigPath),
              ControlFlowOnly(ControlFlowOnly), PrintAsmDiffs(PrintAsmDiffs),
              PrintCallStacks(PrintCallStacks) {}

    /// Sets debug types specified in the vector.
    void setDebugTypes(std::vector<std::string> &debugTypes);

    void refreshFunctions();
};

#endif // DIFFKEMP_SIMPLL_CONFIG_H
