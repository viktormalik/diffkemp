//===------------- Transforms.h - Simplifications of modules --------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of functions that do the actual
/// simplification of programs so that they are later easier to be compared for
/// semantic difference.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_INDEPENDENTPASSES_H
#define DIFFKEMP_SIMPLL_INDEPENDENTPASSES_H

#include "Config.h"
#include "ModuleComparator.h"
#include "Utils.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <set>

using namespace llvm;

/// Preprocessing transformations - run independently on each module at the
/// beginning.
/// \param Mod Module to simplify.
/// \param Main Function that is to be compared in the module. Can be set to
///             NULL, but specifying this optimizes the transformations.
/// \param Var Global variable w.r.t. to whose value the semantic diff will be
///            done. Can be set to NULL, but specifying this enables more
///            aggresive simplification.
void preprocessModule(Module &Mod,
                      Function *Main,
                      GlobalVariable *Var,
                      const Config &Config);

/// Structure to represent the output value of simplifyModulesDiff containing
/// several vectors that are all outputs of ModuleComparator.
struct ComparisonResult {
    std::vector<FunPair> nonequalFuns;
    std::vector<ConstFunPair> missingDefs;
    std::vector<std::unique_ptr<NonFunctionDifference>> differingObjects;
    std::set<std::string> coveredFuns;
};

/// Simplify two corresponding modules for the purpose of their subsequent
/// semantic difference analysis. Tries to remove all the code that is
/// syntactically equal between the modules which should decrease the complexity
/// of the semantic diff.
void simplifyModulesDiff(Config &config, ComparisonResult &Result);

/// Preprocessing transformations - run independently on each module at the
/// end.
/// \param Mod Module to simplify.
/// \param Main Function that is to be compared in the module. Can be set to
///             NULL, but specifying this optimizes the transformations.
void postprocessModule(Module &Mod, const std::set<Function *> &MainFuns);

#endif // DIFFKEMP_SIMPLL_INDEPENDENTPASSES_H
