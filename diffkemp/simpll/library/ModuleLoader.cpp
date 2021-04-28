//===-------------- ModuleLoader.cpp - Module loading functions
//-------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Petr Silling, psilling@redhat.com
// Author: Tomas Glozar, tglozar@gmail.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the definitions of functions that handle the loading and
/// freeing of LLVM modules.
///
//===----------------------------------------------------------------------===//

#include "library/ModuleLoader.h"
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>

/// Load a new module into the given module and context maps and return
/// a pointer to the new module.
Module *loadModule(
        std::string &Path,
        std::unordered_map<Module *, std::unique_ptr<Module>> &ModuleMap,
        std::unordered_map<Module *, std::unique_ptr<LLVMContext>>
                &ContextMap) {
    SMDiagnostic err;
    std::unique_ptr<LLVMContext> Ctx = std::make_unique<LLVMContext>();
    std::unique_ptr<Module> Mod = parseIRFile(Path, err, *Ctx.get());
    Module *ModPtr = Mod.get();
    ModuleMap[ModPtr] = std::move(Mod);
    ContextMap[ModPtr] = std::move(Ctx);
    return ModPtr;
}

/// Free a module from the given module and context maps.
void freeModule(
        Module *Mod,
        std::unordered_map<Module *, std::unique_ptr<Module>> &ModuleMap,
        std::unordered_map<Module *, std::unique_ptr<LLVMContext>>
                &ContextMap) {
    ModuleMap.erase(Mod);
    ContextMap.erase(Mod);
}
