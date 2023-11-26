//===------ SMTBlockComparator.h - SMT-based comparison of snippets -------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Frantisek Necas, frantisek.necas@protonmail.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains implementation of SMT-based formal verification of
/// equality of small code snippets.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SMTBLOCKCOMPARATOR_H
#define DIFFKEMP_SMTBLOCKCOMPARATOR_H

#include "Config.h"
#include "DifferentialFunctionComparator.h"
#include <llvm/IR/BasicBlock.h>

using namespace llvm;

/// A class for comparing short sequential code snippets using an SMT solver.
class SMTBlockComparator {
  public:
    SMTBlockComparator(const Config &config,
                       DifferentialFunctionComparator *fComp)
            : config(config), fComp(fComp) {}

    /// Compares two code snippets starting from InstL and InstR and tries to
    /// verify if there are two semantically equal sequences of instructions
    /// followed by a synchronization point.
    int compare(BasicBlock::const_iterator &InstL,
                BasicBlock::const_iterator &InstR);

  private:
    const Config &config;
    const DifferentialFunctionComparator *fComp;
};

#endif // DIFFKEMP_SMTBLOCKCOMPARATOR_H
