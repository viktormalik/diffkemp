//===----- SMTBlockComparator.cpp - SMT-based comparison of snippets ------===//
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

#include "SMTBlockComparator.h"

using namespace llvm;

int SMTBlockComparator::compare(BasicBlock::const_iterator &InstL,
                                BasicBlock::const_iterator &InstR) {
    return 1;
}
