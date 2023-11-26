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

void SMTBlockComparator::findSnippetEnd(BasicBlock::const_iterator &InstL,
                                        BasicBlock::const_iterator &InstR) {
    auto BBL = const_cast<BasicBlock *>(InstL->getParent());
    auto BBR = const_cast<BasicBlock *>(InstR->getParent());
    auto StartR = InstR;

    while (InstL != BBL->end()) {
        if (fComp->maySkipInstruction(&*InstL) || isDebugInfo(*InstL)) {
            InstL++;
            continue;
        }

        // Try to find a matching instruction on the right
        InstR = StartR;
        while (InstR != BBR->end()) {
            if (fComp->maySkipInstruction(&*InstR) || isDebugInfo(*InstR)) {
                InstR++;
                continue;
            }

            // We want to ensure that the rest of the instructions in the basic
            // blocks are synchronized. Since we are using the same fComp
            // instance that is a caller of this method, we want to avoid
            // recursive SMT solver calls. Relocations could also modify the
            // underlying state, avoid them as well.
            sn_mapL_backup = fComp->sn_mapL;
            sn_mapR_backup = fComp->sn_mapR;
            mappedValuesBySnBackup = fComp->mappedValuesBySn;
            // Backup the inlining data -- we will want to restore it if
            // the snippets are found to be unequal since otherwise, wrong
            // inlining would be done.
            auto tryInlineBackup = fComp->ModComparator->tryInline;
            if (fComp->cmpBasicBlocksFromInstructions(
                        BBL, BBR, InstL, InstR, true)
                == 0) {
                // Found a synchronization point
                return;
            }
            fComp->ModComparator->tryInline = tryInlineBackup;
            fComp->sn_mapL = sn_mapL_backup;
            fComp->sn_mapR = sn_mapR_backup;
            fComp->mappedValuesBySn = mappedValuesBySnBackup;
            InstR++;
        }
        InstL++;
    }

    throw NoSynchronizationPointException();
}

int SMTBlockComparator::compare(BasicBlock::const_iterator &InstL,
                                BasicBlock::const_iterator &InstR) {
    // Back-up the start of the snippet
    auto StartL = InstL;
    auto StartR = InstR;

    // Instructions have been found to differ, undo the last comparison
    fComp->undoLastInstCompare(InstL, InstR);

    // Update InstL and InstR to point to the end of the snippet
    findSnippetEnd(InstL, InstR);

    return 1;
}
