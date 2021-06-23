//===----- ModuleAnalysis.h - Transformation and comparison of modules ----===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of functions doing the actual semantic
/// comparison of functions and their dependencies in their modules.
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
                      Constant *VarValue,
                      bool UseDefaultValue,
                      bool ControlFlowOnly);

/// Simplify two corresponding modules for the purpose of their subsequent
/// semantic difference analysis. Tries to remove all the code that is
/// syntactically equal between the modules which should decrease the complexity
/// of the semantic diff.
void simplifyModulesDiff(Config &config, OverallResult &Result);

/// Run pre-process passes on the modules specified in the config and compare
/// them using simplifyModulesDiff. The output is written to files specified
/// in config.
void processAndCompare(Config &config, OverallResult &Result);

#endif // DIFFKEMP_SIMPLL_INDEPENDENTPASSES_H
