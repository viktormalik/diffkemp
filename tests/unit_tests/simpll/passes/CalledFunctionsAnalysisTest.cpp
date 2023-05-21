//===------------ CalledFunctionsAnalysisTest.cpp - Unit tests -------------==//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Tomas Glozar, tglozar@gmail.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains unit tests for the CalledFunctionsAnalysis pass.
///
//===----------------------------------------------------------------------===//

#include <gtest/gtest.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Passes/PassBuilder.h>
#if LLVM_VERSION_MAJOR >= 11
#include <llvm/IR/PassManagerImpl.h>
#endif
#include <passes/CalledFunctionsAnalysis.h>

using namespace llvm;

/// Utility function to create a void-returning empty function without
/// arguments and with external linkage.
static Function *createFunction(std::string Name, Module *Mod) {
    return Function::Create(
            FunctionType::get(Type::getVoidTy(Mod->getContext()), {}, false),
            GlobalValue::ExternalLinkage,
            Name,
            Mod);
}

/// Utility function to add a direct call between functions generated by
/// createFunction.
static void addCall(Function *Src, Function *Dest) {
    BasicBlock *BB;
    if (Src->isDeclaration()) {
        BB = BasicBlock::Create(Src->getParent()->getContext(), "", Src);
    } else {
        BB = &*Src->begin();
    }

    CallInst::Create(Dest, {}, "", BB);
}

/// Utility function to add a non-call usage of Dest to Src.
static void addNonCallUsage(Function *Src, Function *Dest) {
    BasicBlock *BB;
    if (Src->isDeclaration()) {
        BB = BasicBlock::Create(Src->getParent()->getContext(), "", Src);
    } else {
        BB = &*Src->begin();
    }

    AllocaInst *All = new AllocaInst(Dest->getType(), 0, "", BB);
    new StoreInst(All, Dest, BB);
}

/// Creates a module with a few test functions with calls and non-call function
/// uses.
/// Then CalledFunctionsAnalysis is run on the module and the its results are
/// checked.
TEST(CalledFunctionsAnalysisTest, Base) {
    LLVMContext Ctx;
    Module *Mod = new Module("test", Ctx);

    // Create functions.
    Function *Main = createFunction("main", Mod);
    Function *Fun1 = createFunction("fun1", Mod);
    Function *Fun2 = createFunction("fun2", Mod);
    Function *Fun3 = createFunction("fun3", Mod);
    Function *Fun4 = createFunction("fun4", Mod);
    Function *Indir = createFunction("indir", Mod);

    // Create calls.
    addCall(Main, Fun1);
    addCall(Main, Fun2);
    addCall(Fun1, Main);
    addCall(Fun2, Fun1);
    addNonCallUsage(Fun2, Indir);
    addCall(Fun3, Main);
    addCall(Fun3, Fun4);
    addCall(Fun4, Indir);
    addCall(Indir, Fun2);

    // Create return instructions at the end of all functions.
    std::vector<Function *> AllFuns{Main, Fun1, Fun2, Fun3, Fun4, Indir};
    for (Function *F : AllFuns) {
        ReturnInst::Create(Ctx, nullptr, &*F->begin());
    }

    // Run the analysis and check the result.
    AnalysisManager<Module, Function *> mam;
    mam.registerPass([] { return CalledFunctionsAnalysis(); });
    mam.registerPass([] { return PassInstrumentationAnalysis(); });

    auto Result = mam.getResult<CalledFunctionsAnalysis>(*Mod, Main);
    std::set<const Function *> ExpectedResult{Main, Fun1, Fun2, Indir};

    ASSERT_EQ(Result, ExpectedResult);
}
