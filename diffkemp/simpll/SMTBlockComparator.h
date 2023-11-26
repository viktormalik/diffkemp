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

class SMTException : public std::exception {};

/// An exception thrown when no analyzable code snippets are found, i.e.
/// there are no snippets after which the remaining basic block instructions
/// can be synchronized.
class NoSynchronizationPointException : public SMTException {
  public:
    virtual const char *what() const noexcept {
        return "No synchronization point found";
    }
};

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
    DenseMap<const Value *, int> sn_mapL_backup;
    DenseMap<const Value *, int> sn_mapR_backup;
    std::unordered_map<int, std::pair<const Value *, const Value *>>
            mappedValuesBySnBackup;

    const Config &config;
    const DifferentialFunctionComparator *fComp;

    /// Moves InstL and InstR iterators to the end of the snippet to compare.
    /// The snippet must be followed by a synchronized instruction. If no such
    /// instruction is found, the whole basic block is to be considered.
    void findSnippetEnd(BasicBlock::const_iterator &InstL,
                        BasicBlock::const_iterator &InstR);
};

#endif // DIFFKEMP_SMTBLOCKCOMPARATOR_H
