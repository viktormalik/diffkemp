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

#include "VarValueDependencySlicer.h"
#include "DebugInfo.h"
#include <Config.h>
#include <Utils.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <utility>
#include <vector>

using namespace llvm;

PreservedAnalyses VarValueDependencySlicer::run(Module &Mod,
                                                ModuleAnalysisManager &mam,
                                                GlobalVariable *Var,
                                                Constant *VarValue) {
    Constant *newConstant;

    if (!VarValue) {
        if (Var->hasInitializer())
            newConstant = Var->getInitializer();
        else
            return PreservedAnalyses::all();
    } else {
        newConstant = VarValue;
    }

    for (User *user : Var->users()) {
        if (LoadInst *loadInst = dyn_cast<LoadInst>(user)) {
            loadInst->replaceAllUsesWith(newConstant);
        }
    }

    return PreservedAnalyses::all();
}
