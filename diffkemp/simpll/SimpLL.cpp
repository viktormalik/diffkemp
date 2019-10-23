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
#include "ModuleComparator.h"
#include "Output.h"
#include "Transforms.h"
#include "Utils.h"
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

/// Write LLVM IR of a module into a file.
/// \param Mod LLVM module to write.
/// \param FileName Path to the file to write to.
void writeIRToFile(Module &Mod, StringRef FileName) {
    std::error_code errorCode;
    raw_fd_ostream stream(FileName, errorCode, sys::fs::F_None);
    Mod.print(stream, nullptr);
    stream.close();
}

int main(int argc, const char **argv) {
    // Parse CLI options
    cl::ParseCommandLineOptions(argc, argv);
    Config config;

    // Run transformations
    preprocessModule(*config.First, config.FirstFun, config.FirstVar, config);
    preprocessModule(
            *config.Second, config.SecondFun, config.SecondVar, config);
    config.refreshFunctions();

    ComparisonResult Result;
    simplifyModulesDiff(config, Result);

    reportOutput(config, Result);

    // Split list of non-equal function pairs into two sets.
    std::set<Function *> MainFunsFirst;
    std::transform(Result.nonequalFuns.begin(),
                   Result.nonequalFuns.end(),
                   std::inserter(MainFunsFirst, MainFunsFirst.begin()),
                   [](FunPair &p) { return p.first; });
    std::set<Function *> MainFunsSecond;
    std::transform(Result.nonequalFuns.begin(),
                   Result.nonequalFuns.end(),
                   std::inserter(MainFunsSecond, MainFunsSecond.begin()),
                   [](FunPair &p) { return p.second; });

    // Write LLVM IR to output files
    writeIRToFile(*config.First, config.FirstOutFile);
    writeIRToFile(*config.Second, config.SecondOutFile);

    llvm_shutdown();
    return 0;
}
