//===----------------- FFI.cpp - C interface for SimpLL -------------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Tomas Glozar, tglozar@gmail.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains definitions of C functions used for interacting with
/// DiffKemp.
///
//===----------------------------------------------------------------------===//

#include "FFI.h"
#include "Config.h"
#include "ModuleAnalysis.h"
#include "Output.h"
#include "PatternAnalysis.h"
#include "library/DiffKempUtils.h"
#include "library/SysctlTable.h"
#include "passes/CalledFunctionsAnalysis.h"
#include <cstring>
#include <llvm/Transforms/Utils/Cloning.h>
#include <unordered_map>

/// Map to store LLVMContext objects for modules.
std::unordered_map<Module *, std::unique_ptr<LLVMContext>> ContextMap;

/// Map that manages unique pointers to modules.
std::unordered_map<Module *, std::unique_ptr<Module>> ModuleMap;

/// Utility function used to convert an iterable container of StringRefs into
/// a ptr_array.
/// This is for cases when the strings are owned by the LLVM context.
/// Note: the array has to be freed by the caller using freePointerArray.
template <class T> struct ptr_array stringRefContainerToPtrArray(T Container) {
    const char **Result = new const char *[Container.size()];
    size_t i = 0;
    for (StringRef FunName : Container) {
        Result[i++] = FunName.data();
    }

    return ptr_array{(void **)Result, Container.size()};
}

/// Utility function used to convert an iterable container of strings into
/// a ptr_array.
/// This is for cases when the strings are owned by the std::string objects
/// supplied in the parameter.
/// Note: the array has to be freed by the caller using freeStringArray.
template <class T> struct ptr_array stringContainerToPtrArray(T Container) {
    const char **Result = new const char *[Container.size()];
    size_t i = 0;
    for (const std::string &Str : Container) {
        char *CStr = new char[Str.size() + 1];
        std::strcpy(CStr, Str.c_str());
        Result[i++] = CStr;
    }

    return ptr_array{(void **)Result, Container.size()};
}

extern "C" {
void *loadModule(const char *Path) {
    SMDiagnostic err;
    std::unique_ptr<LLVMContext> Ctx = std::make_unique<LLVMContext>();
    std::unique_ptr<Module> Mod = parseIRFile(Path, err, *Ctx.get());
    Module *ModPtr = Mod.get();
    ModuleMap[ModPtr] = std::move(Mod);
    ContextMap[ModPtr] = std::move(Ctx);
    return (void *)ModPtr;
}

void freeModule(void *ModRaw) {
    Module *Mod = (Module *)ModRaw;
    ModuleMap.erase(Mod);
    ContextMap.erase(Mod);
}

void freePointerArray(struct ptr_array PtrArr) { delete[] PtrArr.arr; }

void freeStringArray(struct ptr_array PtrArr) {
    for (unsigned long i = 0; i < PtrArr.len; i++)
        delete[] (char *)PtrArr.arr[i];

    freePointerArray(PtrArr);
}

void *getFunction(void *ModRaw, const char *Fun) {
    Module *Mod = (Module *)ModRaw;
    return (void *)Mod->getFunction(std::string(Fun));
}

const char *getFunctionName(void *FunRaw) {
    Function *Fun = (Function *)FunRaw;
    if (Fun->hasName())
        return Fun->getName().data();
    else
        return nullptr;
}

int isDeclaration(void *FunRaw) {
    Function *Fun = (Function *)FunRaw;
    return Fun->isDeclaration();
}

/// Get all functions recursively called by FunRaw.
/// Note: this is a C interface wrapper for CalledFunctionsAnalysis.
struct ptr_array getCalledFunctions(void *FunRaw) {
    Function *Fun = (Function *)FunRaw;

    // Run CalledFunctionAnalysis to get the result as a std::set.
    AnalysisManager<Module, Function *> mam;
    mam.registerPass([] { return CalledFunctionsAnalysis(); });
    mam.registerPass([] { return PassInstrumentationAnalysis(); });

    CalledFunctionsAnalysis::Result CalledFunsSet =
            mam.getResult<CalledFunctionsAnalysis>(*Fun->getParent(), Fun);

    // Convert the set into a C array.
    // Note: the array has to be freed by the caller using freePointerArray.
    const Function **Result = new const Function *[CalledFunsSet.size()];
    size_t i = 0;
    for (const Function *Fun : CalledFunsSet) {
        Result[i++] = Fun;
    }

    return ptr_array{(void **)Result, CalledFunsSet.size()};
}

/// Find the name of the global variable that corresponds to the given module
/// parameter.
const char *findParamVarC(const char *Param, void *ModRaw) {
    Module *Mod = (Module *)ModRaw;
    StringRef ParamName = findParamVar(Param, Mod);
    if (ParamName != "")
        return ParamName.data();
    else
        return nullptr;
}

/// Find names of all functions using the given parameter (global variable).
struct ptr_array getFunctionsUsingParamC(const char *ParamName,
                                         int *indices,
                                         unsigned long indices_n,
                                         void *ModRaw) {
    Module *Mod = (Module *)ModRaw;
    auto FunNameSet = getFunctionsUsingParam(
            ParamName, std::vector<int>(indices, indices + indices_n), Mod);

    return stringRefContainerToPtrArray(FunNameSet);
}

/// Creates a SysctlTable object used for looking up sysctls in a sysctl table.
void *getSysctlTable(void *ModRaw, const char *CtlTable) {
    return new SysctlTable((Module *)ModRaw, CtlTable);
}

void freeSysctlTable(void *SysctlTableRaw) {
    delete (SysctlTable *)SysctlTableRaw;
}

/// Parse all sysctls entries that match the given pattern. Parsed entries
/// are LLVM objects of type "struct ctl_table" containing the sysctl
/// definition.
/// Returns the list of sysctl names.
struct ptr_array parseSysctls(const char *SysctlPattern, void *SysctlTableRaw) {
    SysctlTable *Table = (SysctlTable *)SysctlTableRaw;
    auto SysctlVector = Table->parseSysctls(SysctlPattern);

    return stringContainerToPtrArray(SysctlVector);
}

/// Get the proc handler function for the given sysctl option.
void *getProcFun(const char *Sysctl, void *SysctlTableRaw) {
    SysctlTable *Table = (SysctlTable *)SysctlTableRaw;
    return (void *)Table->getProcFun(Sysctl);
}

void freeKernelParam(struct kernel_param Param) {
    // Note: the string with the param name is not allocated by the FFI,
    // but by LLVM, and therefore it is not free.
    delete[] Param.indices;
}

/// Get the child node of the given sysctl table entry.
struct kernel_param getChild(const char *Sysctl, void *SysctlTableRaw) {
    SysctlTable *Table = (SysctlTable *)SysctlTableRaw;
    SysctlParam Result = Table->getChild(Sysctl);

    // Convert the indices vector into a C array.
    int *indices = new int[Result.indices.size()];
    size_t i = 0;
    for (int index : Result.indices) {
        indices[i++] = index;
    }

    return kernel_param{Result.Var ? Result.Var->getName().data() : nullptr,
                        indices,
                        Result.indices.size()};
}

/// Get the data variable for the given sysctl option.
struct kernel_param getData(const char *Sysctl, void *SysctlTableRaw) {
    SysctlTable *Table = (SysctlTable *)SysctlTableRaw;
    SysctlParam Result = Table->getData(Sysctl);

    // Convert the indices vector into a C array.
    int *indices = new int[Result.indices.size()];
    size_t i = 0;
    for (int index : Result.indices) {
        indices[i++] = index;
    }

    return kernel_param{Result.Var ? Result.Var->getName().data() : nullptr,
                        indices,
                        Result.indices.size()};
}

void runGeneratePattern(const char *path) { readPatternConfig(path); }

/// Simplifies modules and compares the specified functions.
void runSimpLL(void *ModL,
               void *ModR,
               const char *ModLOut,
               const char *ModROut,
               const char *FunL,
               const char *FunR,
               struct config Conf,
               char *Output) {
    Config config(FunL,
                  FunR,
                  (Module *)ModL,
                  (Module *)ModR,
                  ModLOut,
                  ModROut,
                  Conf.CacheDir,
                  Conf.Patterns,
                  Conf.Variable,
                  Conf.OutputLlvmIR,
                  Conf.ControlFlowOnly,
                  Conf.PrintAsmDiffs,
                  Conf.PrintCallStacks,
                  Conf.Verbosity);

    OverallResult Result;
    processAndCompare(config, Result);

    std::string outputString = reportOutputToString(Result);
    strcpy(Output, outputString.c_str());
}

/// Clones modules to get separate copies of them and runs the simplification
/// and comparison on the copies.
void cloneAndRunSimpLL(void *ModL,
                       void *ModR,
                       const char *ModLOut,
                       const char *ModROut,
                       const char *FunL,
                       const char *FunR,
                       struct config Conf,
                       char *Output) {
    std::unique_ptr<Module> UqModL(CloneModule(*((Module *)ModL)));
    std::unique_ptr<Module> UqModR(CloneModule(*((Module *)ModR)));
    runSimpLL(UqModL.get(),
              UqModR.get(),
              ModLOut,
              ModROut,
              FunL,
              FunR,
              Conf,
              Output);
}

/// Loads modules from the specified filers and runs the simplification and
/// comparison on the loaded objects, which are discarded after the comparison.
void parseAndRunSimpLL(const char *ModL,
                       const char *ModR,
                       const char *ModLOut,
                       const char *ModROut,
                       const char *FunL,
                       const char *FunR,
                       struct config Conf,
                       char *Output) {
    LLVMContext CtxL, CtxR;
    SMDiagnostic err;

    std::unique_ptr<Module> UqModL(parseIRFile(ModL, err, CtxL));
    std::unique_ptr<Module> UqModR(parseIRFile(ModR, err, CtxR));
    runSimpLL(UqModL.get(),
              UqModR.get(),
              ModLOut,
              ModROut,
              FunL,
              FunR,
              Conf,
              Output);
}

/// Runs preprocess passes on module and marks it as being preprocessed so they
/// won't be run again when the module is compared.
void preprocessModuleC(void *Mod, int ControlFlowOnly) {
    Module *LLVMMod = (Module *)Mod;
    preprocessModule(*LLVMMod, nullptr, nullptr, ControlFlowOnly);
}

void shutdownSimpLL() { llvm_shutdown(); }
} // extern "C"
