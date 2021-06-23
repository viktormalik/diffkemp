//== VarValueDependencySlicer.cpp-Removing code based on value of variable ==//
//
//        SimpLL - Program simplifier for analysis of semantic difference    //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author:
//===---------------------------------------------------------------------===//
///
/// \file
/// This file contains the implementation of the VarValueDependencySlicer pass.
/// The pass slice the program to a value of a global variable.
///
//===--------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_VARVALUEDEPENDENCYSLICER_H
#define DIFFKEMP_SIMPLL_VARVALUEDEPENDENCYSLICER_H

#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>

using namespace llvm;

class VarValueDependencySlicer
        : public PassInfoMixin<VarValueDependencySlicer> {
  public:
    PreservedAnalyses run(Module &Mod,
                          ModuleAnalysisManager &mam,
                          GlobalVariable *Var,
                          Constant *VarValue);
};

#endif // DIFFKEMP_SIMPLL_VARVALUEDEPENDENCYSLICER_H
