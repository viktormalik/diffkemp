//===---- ModuleAnalysis.cpp - Transformation and comparison of modules ---===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains implementations of functions doing the actual semantic
/// comparison of functions and their dependencies in their modules.
///
//===----------------------------------------------------------------------===//

#include "ModuleAnalysis.h"
#include "DebugInfo.h"
#include "DifferentialFunctionComparator.h"
#include "ModuleComparator.h"
#include "ResultsCache.h"
#include "SourceCodeUtils.h"
#include "Utils.h"
#include "passes/CalledFunctionsAnalysis.h"
#include "passes/ControlFlowSlicer.h"
#include "passes/FieldAccessFunctionGenerator.h"
#include "passes/FunctionAbstractionsGenerator.h"
#include "passes/MergeNumberedFunctionsPass.h"
#include "passes/ReduceFunctionMetadataPass.h"
#include "passes/RemoveLifetimeCallsPass.h"
#include "passes/RemoveUnusedReturnValuesPass.h"
#include "passes/SeparateCallsToBitcastPass.h"
#include "passes/SimplifyKernelFunctionCallsPass.h"
#include "passes/SimplifyKernelGlobalsPass.h"
#include "passes/StructHashGeneratorPass.h"
#include "passes/StructureDebugInfoAnalysis.h"
#include "passes/StructureSizeAnalysis.h"
#include "passes/UnifyMemcpyPass.h"
#include "passes/VarDependencySlicer.h"
#include <llvm/IR/PassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/Scalar/DCE.h>
#include <llvm/Transforms/Scalar/LowerExpectIntrinsic.h>
/// Preprocessing functions run on each module at the beginning.
/// The following transformations are applied:
/// 1. Slicing of program w.r.t. to the value of some global variable.
///    Keeps only those instructions whose value or execution depends on
///    the value of the global variable.
///    This is only run if Var is specified.
/// 2. Removal of the arguments of calls to printing functions.
///    These arguments do not affect the code functionality.
///    TODO: this should be switchable by a CLI option.
/// 3. Unification of memcpy variants so that all use the llvm.memcpy intrinsic.
/// 4. Dead code elimination.
/// 5. Removing calls to llvm.expect.
void preprocessModule(Module &Mod,
                      Function *Main,
                      GlobalVariable *Var,
                      bool ControlFlowOnly) {
    if (Var) {
        // Slicing of the program w.r.t. the value of a global variable
        PassManager<Function, FunctionAnalysisManager, GlobalVariable *> fpm;
        FunctionAnalysisManager fam(false);
        PassBuilder pb;
        pb.registerFunctionAnalyses(fam);

        fpm.addPass(VarDependencySlicer{});
        fpm.run(*Main, fam, Var);
    }

    // Function passes
    FunctionPassManager fpm(false);
    FunctionAnalysisManager fam(false);
    PassBuilder pb;
    pb.registerFunctionAnalyses(fam);

    if (ControlFlowOnly)
        fpm.addPass(ControlFlowSlicer{});
    fpm.addPass(SimplifyKernelFunctionCallsPass{});
    fpm.addPass(UnifyMemcpyPass{});
    fpm.addPass(DCEPass{});
    fpm.addPass(LowerExpectIntrinsicPass{});
    fpm.addPass(ReduceFunctionMetadataPass{});
    fpm.addPass(SeparateCallsToBitcastPass{});

    for (auto &Fun : Mod)
        fpm.run(Fun, fam);

    // Module passes
    ModulePassManager mpm(false);
    ModuleAnalysisManager mam(false);
    pb.registerModuleAnalyses(mam);

    mpm.addPass(MergeNumberedFunctionsPass{});
    mpm.addPass(SimplifyKernelGlobalsPass{});
    mpm.addPass(RemoveLifetimeCallsPass{});
    mpm.addPass(StructHashGeneratorPass{});

    mpm.run(Mod, mam);
}

/// Simplification of modules to ease the semantic diff.
/// Removes all the code that is syntactically same between modules (hence it
/// must not be checked for semantic equivalence).
/// The following transformations are applied:
/// 1. Replacing indirect function calls and inline assemblies by abstraction
///    functions.
/// 2. Transformation of functions returning a value into void functions in case
///    the return value is never used within the module.
/// 3. Using debug information to compute offsets of the corresponding GEP
///    indices. Offsets are stored inside LLVM metadata.
/// 4. Removing bodies of functions that are syntactically equivalent.
void simplifyModulesDiff(Config &config, OverallResult &Result) {
    // Generate abstractions of indirect function calls and for inline
    // assemblies.
    AnalysisManager<Module, Function *> mam(false);
    mam.registerPass([] { return CalledFunctionsAnalysis(); });
    mam.registerPass([] { return FunctionAbstractionsGenerator(); });
    mam.registerPass([] { return StructureSizeAnalysis(); });
    mam.registerPass([] { return StructureDebugInfoAnalysis(); });
#if LLVM_VERSION_MAJOR >= 8
    mam.registerPass([] { return PassInstrumentationAnalysis(); });
#endif

    auto AbstractionGeneratorResultL =
            mam.getResult<FunctionAbstractionsGenerator>(*config.First,
                                                         config.FirstFun);
    auto AbstractionGeneratorResultR =
            mam.getResult<FunctionAbstractionsGenerator>(*config.Second,
                                                         config.SecondFun);

    auto StructSizeMapL = mam.getResult<StructureSizeAnalysis>(*config.First,
                                                               config.FirstFun);
    auto StructSizeMapR = mam.getResult<StructureSizeAnalysis>(
            *config.Second, config.SecondFun);
    auto StructDIL = mam.getResult<StructureDebugInfoAnalysis>(*config.First,
                                                               config.FirstFun);
    auto StructDIR = mam.getResult<StructureDebugInfoAnalysis>(
            *config.Second, config.SecondFun);

    // Module passes
    PassManager<Module,
                AnalysisManager<Module, Function *>,
                Function *,
                Module *>
            mpm;
    mpm.addPass(RemoveUnusedReturnValuesPass{});
    mpm.run(*config.First, mam, config.FirstFun, config.Second.get());
    mpm.run(*config.Second, mam, config.SecondFun, config.First.get());

    // Refreshing main functions is necessary because they can be replaced with
    // a new version by a pass
    config.refreshFunctions();

    DebugInfo DI(*config.First,
                 *config.Second,
                 config.FirstFun,
                 config.SecondFun,
                 mam.getResult<CalledFunctionsAnalysis>(*config.First,
                                                        config.FirstFun),
                 mam.getResult<CalledFunctionsAnalysis>(*config.Second,
                                                        config.SecondFun));

    // Compare functions for syntactical equivalence
    ModuleComparator modComp(*config.First,
                             *config.Second,
                             config,
                             &DI,
                             StructSizeMapL,
                             StructSizeMapR,
                             StructDIL,
                             StructDIR);

    if (config.FirstFun && config.SecondFun) {
        modComp.compareFunctions(config.FirstFun, config.SecondFun);

        DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                        dbgs() << "Syntactic comparison results:\n");
        bool allEqual = true;
        for (auto &funPairResult : modComp.ComparedFuns) {
            if (!funPairResult.first.first->isIntrinsic()
                && !isSimpllAbstraction(funPairResult.first.first)) {
                Result.functionResults.push_back(
                        std::move(funPairResult.second));
            }
            if (funPairResult.second.kind == Result::Kind::NOT_EQUAL) {
                allEqual = false;
                DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                dbgs() << funPairResult.first.first->getName()
                                       << " are syntactically different\n");
            }
        }
        if (allEqual) {
            // Functions are equal iff all functions that were compared by
            // module comparator (i.e. those that are recursively called by the
            // main functions) are equal.
            DEBUG_WITH_TYPE(
                    DEBUG_SIMPLL,
                    dbgs() << "All functions are syntactically equal\n");
            config.FirstFun->deleteBody();
            config.SecondFun->deleteBody();
            deleteAliasToFun(*config.First, config.FirstFun);
            deleteAliasToFun(*config.Second, config.SecondFun);
        }
    } else {
        for (auto &FunFirst : *config.First) {
            if (auto FunSecond =
                        config.Second->getFunction(FunFirst.getName())) {
                modComp.compareFunctions(&FunFirst, FunSecond);
            }
        }
    }
    Result.missingDefs = modComp.MissingDefs;
}

/// Write LLVM IR of a module into a file.
/// \param Mod LLVM module to write.
/// \param FileName Path to the file to write to.
void writeIRToFile(Module &Mod, StringRef FileName) {
    std::error_code errorCode;
    raw_fd_ostream stream(FileName, errorCode, sys::fs::F_None);
    Mod.print(stream, nullptr);
    stream.close();
}

/// Run pre-process passes on the modules specified in the config and compare
/// them using simplifyModulesDiff. The output is written to files specified
/// in config.
void processAndCompare(Config &config, OverallResult &Result) {
    // Run transformations
    preprocessModule(*config.First,
                     config.FirstFun,
                     config.FirstVar,
                     config.ControlFlowOnly);
    preprocessModule(*config.Second,
                     config.SecondFun,
                     config.SecondVar,
                     config.ControlFlowOnly);
    config.refreshFunctions();

    simplifyModulesDiff(config, Result);

    if (config.OutputLlvmIR) {
        // Write LLVM IR to output files
        writeIRToFile(*config.First, config.FirstOutFile);
        writeIRToFile(*config.Second, config.SecondOutFile);
    }
}
