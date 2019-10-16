//= SimplifyKernelFunctionCallsPass.h - Simplifying kernel-specific functions //
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of the SimplifyKernelFunctionCallsPass
/// that removes arguments of some kernel functions that do not affect semantics
/// of the program.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_SIMPLIFYKERNELFUNCTIONCALLSPASS_H
#define DIFFKEMP_SIMPLL_SIMPLIFYKERNELFUNCTIONCALLSPASS_H

#include <llvm/ADT/StringSet.h>
#include <llvm/IR/PassManager.h>

using namespace llvm;

class SimplifyKernelFunctionCallsPass
        : public PassInfoMixin<SimplifyKernelFunctionCallsPass> {
  public:
    PreservedAnalyses run(Function &Fun, FunctionAnalysisManager &fam);
};

/// Returns true when the argument is a name of a kernel print function.
bool isKernelPrintFunction(const std::string &name);

/// Returns true when the argument is a name of a kernel warning function.
bool isKernelWarnFunction(const std::string &name);

/// Returns true when the argument is a name of a kernel-specific function.
bool isKernelSimplifiedFunction(const std::string &name);

#endif // DIFFKEMP_SIMPLL_SIMPLIFYKERNELFUNCTIONCALLSPASS_H
