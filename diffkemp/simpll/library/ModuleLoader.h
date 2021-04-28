//===-------------- ModuleLoader.h - Module loading functions -------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Petr Silling, psilling@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of functions that handle loading and
/// freeing of LLVM modules.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_LIBRARY_MODULELOADER_H
#define DIFFKEMP_SIMPLL_LIBRARY_MODULELOADER_H

#include <llvm/IR/Module.h>
#include <unordered_map>

using namespace llvm;

/// Load a new module into the given module and context maps.
Module *loadModule(
        std::string &Path,
        std::unordered_map<Module *, std::unique_ptr<Module>> &ModuleMap,
        std::unordered_map<Module *, std::unique_ptr<LLVMContext>> &ContextMap);

/// Free a module from the given module and context maps.
void freeModule(
        Module *Mod,
        std::unordered_map<Module *, std::unique_ptr<Module>> &ModuleMap,
        std::unordered_map<Module *, std::unique_ptr<LLVMContext>> &ContextMap);

#endif // DIFFKEMP_SIMPLL_LIBRARY_MODULELOADER_H
