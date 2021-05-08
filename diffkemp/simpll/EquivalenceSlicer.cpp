//===--------- EquivalenceSlicer.cpp - Slicing equal parts of code ---------==//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Tatiana Malecova, t.malecova@gmail.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains implementation of static slicer for removing parts of
/// code that are semantically equal.
///
//===----------------------------------------------------------------------===//

#include "EquivalenceSlicer.h"
#include <llvm/IR/CFG.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Utils/Cloning.h>

void EquivalenceSlicer::slice(DifferentialFunctionComparator *fComp) {
    if (!fComp->DifferingInstructions.first
        || !fComp->DifferingInstructions.second) {
        return;
    }
    // Queues for comparing basic blocks of functions and sets for remembering
    // processed basic blocks to avoid infinite loops
    std::queue<BasicBlock *> QL;
    std::queue<BasicBlock *> QR;
    std::set<BasicBlock *> pushed_QL;
    std::set<BasicBlock *> pushed_QR;
    // Get entry blocks
    QL.push(&(config.FirstFun->getEntryBlock()));
    QR.push(&(config.SecondFun->getEntryBlock()));

    fComp->getSnMaps().first->erase(fComp->DifferingInstructions.first);
    fComp->getSnMaps().second->erase(fComp->DifferingInstructions.second);

    while (!QL.empty() && !QR.empty()) {
        // The slicer considers more differing pairs of instructions, so it will
        // keep slicing until it did not checked all basic blocks

        // Back up of differing instruction got from DFC
        std::pair<const Instruction *, const Instruction *> DifferingInsts =
                fComp->DifferingInstructions;

        // First phase
        // We are comparing BBs by respecting a control flow, whereas we ignore
        // basic blocks that have only predecessor the differing BBs (in ignored
        // BBs we will try to find next synchronization)
        bool diff = false;
        while (!QL.empty() && !QR.empty()) {
            bool foundDifference = false;
            auto BBL = QL.front();
            auto BBR = QR.front();
            QL.pop();
            QR.pop();
            auto InstL = BBL->begin();
            auto InstR = BBR->begin();

            while (InstL != BBL->end() && InstR != BBR->end()) {

                if (fComp->maySkipInstruction(&*InstL)) {
                    InstL++;
                    continue;
                }
                if (fComp->maySkipInstruction(&*InstR)) {
                    InstR++;
                    continue;
                }

                if (isDebugInfo(*InstL)) {
                    InstL++;
                    continue;
                }
                if (isDebugInfo(*InstR)) {
                    InstR++;
                    continue;
                }

                if (!fComp->equal(&*InstL, &*InstR)) {
                    // DFC considers the instructions to be different, this may
                    // be caused by instructions that were not compared yet or
                    // differing instructions were found
                    if (std::make_pair(dyn_cast<const Instruction>(&*InstL),
                                       dyn_cast<const Instruction>(&*InstR))
                        == fComp->DifferingInstructions) {
                        DifferingInsts = fComp->DifferingInstructions;
                        // Found differing instructions
                        foundDifference = true;
                        diff = true;
                    } else {
                        if (!fComp->cmpBasicBlocks(BBL, BBR)) {
                            // Basic blocks were compared as equal, so we need
                            // to restore DifferingInstructions to real
                            // differing ones (the pair was changed to last
                            // compared instructions when calling
                            // cmpBasicBlocks)
                            fComp->DifferingInstructions = DifferingInsts;
                            InstL = BBL->getTerminator()->getIterator();
                            InstR = BBR->getTerminator()->getIterator();
                        } else {
                            // New differing instructions are found
                            DifferingInsts = fComp->DifferingInstructions;
                            // Moving iterators to differing instructions
                            foundDifference = true;
                            diff = true;
                            fComp->getSnMaps().first->erase(
                                    fComp->DifferingInstructions.first);
                            fComp->getSnMaps().second->erase(
                                    fComp->DifferingInstructions.second);
                        }
                    }
                }

                if (foundDifference)
                    // We do not need successors of differing BBs
                    break;

                if (InstL->isTerminator() && InstR->isTerminator()) {
                    // It is sufficient to find successors only at the end
                    // of current BBs
                    for (auto BB : successors(BBL)) {
                        if (pushed_QL.find(BB) == pushed_QL.end()) {
                            QL.push(BB);
                            pushed_QL.insert(BB);
                        }
                    }
                    for (auto BB : successors(BBR)) {
                        if (pushed_QR.find(BB) == pushed_QR.end()) {
                            QR.push(BB);
                            pushed_QR.insert(BB);
                        }
                    }
                }

                ++InstL;
                ++InstR;
            }
        }

        if (!diff) {
            break;
        }

        // Second phase
        // Searching for possible next synchronization after found difference
        // in functions
        bool foundSync = false;
        // Temporary sets for actual left instruction
        std::set<BasicBlock *> pushed_QRB = pushed_QR;
        std::set<const Instruction *> IncludedInstrsTemp = CFR.IncludedInstrs;
        std::set<const BasicBlock *> IncludedBasicBlocksTemp =
                CFR.IncludedBasicBlocks;

        // Search for the synchronization starts from differing instructions
        auto InstL = fComp->DifferingInstructions.first->getIterator();
        auto InstR = fComp->DifferingInstructions.second->getIterator();
        int map_size = fComp->getSizeOfMaps();

        auto BBL = const_cast<BasicBlock *>(
                fComp->DifferingInstructions.first->getParent());
        auto BBR = const_cast<BasicBlock *>(InstR->getParent());

        while (InstL != BBL->end()) {
            // Iterating through the left BB

            if (fComp->maySkipInstruction(&*InstL)) {
                InstL++;
                continue;
            }
            if (isDebugInfo(*InstL)) {
                InstL++;
                continue;
            }

            // We start from the differing instruction found in second function
            InstR = fComp->DifferingInstructions.second->getIterator();
            BBR = const_cast<BasicBlock *>(InstR->getParent());

            // Temporary sets for potentially unequal instruction - if the
            // synchronization is found, we are keeping the content of these
            // sets
            pushed_QRB = pushed_QR;
            IncludedInstrsTemp = CFR.IncludedInstrs;
            IncludedBasicBlocksTemp = CFR.IncludedBasicBlocks;
            while (InstR != BBR->end()) {
                // Iterating trough the right BB
                if (fComp->maySkipInstruction(&*InstR)) {
                    InstR++;
                    continue;
                }

                if (isDebugInfo(*InstR)) {
                    InstR++;
                    continue;
                }

                if (!InstL->isTerminator() && !InstR->isTerminator()) {
                    // Ignoring terminators to avoid false positive
                    // synchronization
                    fComp->valuesMustExist = true;
                    if (!fComp->cmpBasicBlocksFromInstructions(
                                BBL, BBR, *&InstL, *&InstR)) {
                        // Compared BB as equal
                        foundSync = true;
                    } else {
                        // Removing unequal instructions from synchronization
                        // maps
                        fComp->eraseFromMaps(map_size - 1);
                    }
                    fComp->valuesMustExist = false;
                    // Restore differing instructions
                    fComp->DifferingInstructions = DifferingInsts;
                }

                if (foundSync) {
                    break;
                }

                IncludedInstrsTemp.insert(&*InstR);
                IncludedBasicBlocksTemp.insert(BBR);
                if (InstR->isTerminator()) {
                    // Keep finding synchronization
                    for (auto BB : successors(BBR)) {
                        if (pushed_QRB.find(BB) == pushed_QRB.end()) {
                            QR.push(BB);
                            pushed_QRB.insert(BB);
                        }
                    }

                    if (!QR.empty()) {
                        // Get next right BB
                        BBR = QR.front();
                        QR.pop();
                        InstR = BBR->begin();
                        continue;
                    }
                }
                ++InstR;
            }

            if (foundSync) {
                break;
            }

            CFL.addToIncluded(&*InstL);
            CFL.IncludedBasicBlocks.insert(BBL);
            if (InstL->isTerminator()) {
                // Keep finding synchronzation
                for (auto BB : successors(BBL)) {
                    if (pushed_QL.find(BB) == pushed_QL.end()) {
                        QL.push(BB);
                        pushed_QL.insert(BB);
                    }
                }
                if (!QL.empty()) {
                    // Get next left BB
                    BBL = QL.front();
                    QL.pop();
                    InstL = BBL->begin();
                    continue;
                }
            }
            ++InstL;
        }

        CFR.IncludedInstrs = IncludedInstrsTemp;
        CFR.IncludedBasicBlocks = IncludedBasicBlocksTemp;
        pushed_QR = pushed_QRB;
        if (foundSync) {
            if (QL.size() != QR.size()) {
                while (!QL.empty()) {
                    // Instructions left in queue are not synchronized so we
                    // need to keep them
                    auto BB = QL.front();
                    for (auto &Inst : BB->getInstList()) {
                        if (fComp->maySkipInstruction(&Inst)) {
                            continue;
                        }
                        if (isDebugInfo(*&Inst)) {
                            continue;
                        }
                        CFL.addToIncluded(&Inst);
                    }
                    CFL.IncludedBasicBlocks.insert(BB);
                    QL.pop();
                }

                while (!QR.empty()) {
                    // Instructions left in queue are not synchronized so we
                    // need to keep them
                    auto BB = QR.front();
                    for (auto &Inst : BB->getInstList()) {
                        if (fComp->maySkipInstruction(&Inst)) {
                            continue;
                        }
                        if (isDebugInfo(*&Inst)) {
                            continue;
                        }
                        CFR.addToIncluded(&Inst);
                    }
                    CFR.IncludedBasicBlocks.insert(QR.front());
                    QR.pop();
                }
            }
        }
        for (auto BB : successors(BBL)) {
            if (pushed_QL.find(BB) == pushed_QL.end()) {
                QL.push(BB);
                pushed_QL.insert(BB);
            }
        }

        for (auto BB : successors(BBR)) {
            if (pushed_QR.find(BB) == pushed_QR.end()) {
                QR.push(BB);
                pushed_QR.insert(BB);
            }
        }
    }

    // At the end, we need to compare PHI instructions stored in phisToCompare
    for (auto &PhiPair : fComp->phisToCompare)
        if (fComp->cmpPHIs(PhiPair.first, PhiPair.second)) {
            if (CFL.checkPhiDependency(*PhiPair.first)) {
                CFL.addToDependent(PhiPair.first);
            }
            if (CFR.checkPhiDependency(*PhiPair.second))
                CFR.addToDependent(PhiPair.second);
        }

    // To keep valid functions we need to keep all operands of differing
    // instructions
    for (auto Inst : CFL.IncludedInstrs) {
        CFL.addAllOpsToIncluded(&*Inst);
    }
    for (auto Inst : CFR.IncludedInstrs) {
        CFR.addAllOpsToIncluded(&*Inst);
    }

    CFL.addAdditionalInsts(*config.FirstFun);
    CFR.addAdditionalInsts(*config.SecondFun);

    CFL.addDebugInfo(*config.FirstFun);
    CFR.addDebugInfo(*config.SecondFun);

    CFL.clearFunction(*config.FirstFun);
    CFR.clearFunction(*config.SecondFun);
}