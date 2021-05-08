//===------------- ModuleComparator.h - Comparing LLVM modules ------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of the ModuleComparator class that can be
/// used for syntactical comparison of two LLVM modules.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_MODULECOMPARATOR_H
#define DIFFKEMP_SIMPLL_MODULECOMPARATOR_H

#include "Config.h"
#include "DebugInfo.h"
#include "Result.h"
#include "ResultsCache.h"
#include "SourceCodeUtils.h"
#include "Utils.h"
#include "passes/StructureDebugInfoAnalysis.h"
#include "passes/StructureSizeAnalysis.h"
#include <llvm/IR/Module.h>
#include <set>

using namespace llvm;

class ModuleComparator {
    Module &First;
    Module &Second;
    const Config &config;

  public:
    bool EqSlicerEnabled = true;
    /// Storing results of function comparisons.
    std::map<ConstFunPair, Result> ComparedFuns;
    // Structure size to structure name map.
    StructureSizeAnalysis::Result &StructSizeMapL;
    StructureSizeAnalysis::Result &StructSizeMapR;
    // Structure name to structure debug info map.
    StructureDebugInfoAnalysis::Result &StructDIMapL;
    StructureDebugInfoAnalysis::Result &StructDIMapR;
    // Counter of assembly diffs
    int asmDifferenceCounter = 0;

    std::vector<GlobalValuePair> MissingDefs;

    /// DebugInfo class storing results from analysing debug information
    const DebugInfo *DI;

    /// Cache used for dynamic lookup of already compared functions using
    /// data passed from DiffKemp.
    ResultsCache ResCache;

    /// Analysis of differences in macros
    MacroDiffAnalysis MacroDiffs;

    ModuleComparator(Module &First,
                     Module &Second,
                     const Config &config,
                     const DebugInfo *DI,
                     StructureSizeAnalysis::Result &StructSizeMapL,
                     StructureSizeAnalysis::Result &StructSizeMapR,
                     StructureDebugInfoAnalysis::Result &StructDIMapL,
                     StructureDebugInfoAnalysis::Result &StructDIMapR)
            : First(First), Second(Second), config(config), DI(DI),
              ResCache(config.CacheDir), MacroDiffs(),
              StructSizeMapL(StructSizeMapL), StructSizeMapR(StructSizeMapR),
              StructDIMapL(StructDIMapL), StructDIMapR(StructDIMapR) {}

    /// Syntactically compare two functions.
    /// The result of the comparison is stored into the ComparedFuns map.
    void compareFunctions(Function *FirstFun, Function *SecondFun);

    /// Pointer to a function that is called just by one of the compared
    /// functions and needs to be inlined.
    std::pair<const CallInst *, const CallInst *> tryInline = {nullptr,
                                                               nullptr};
};

#endif // DIFFKEMP_SIMPLL_MODULECOMPARATOR_H
