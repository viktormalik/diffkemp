//=== VarDependencySlicer.cpp - Slicing program w.r.t. value of a variable ===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Tomas Glozar, tglozar@gmail.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the implementation of the VarDependencySlicer pass.
/// The pass slices the program w.r.t. to a value of a global variable.
///
//===----------------------------------------------------------------------===//

#include "VarDependencySlicer.h"
#include "DebugInfo.h"
#include <Config.h>
#include <Utils.h>
#include <llvm/Analysis/CFG.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Operator.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/Local.h>
#include <llvm/Transforms/Utils/UnifyFunctionExitNodes.h>

bool VarDependencySlicer::first_preprocessing = true;

PreservedAnalyses VarDependencySlicer::run(Function &Fun,
                                           FunctionAnalysisManager &fam,
                                           GlobalVariable *Var,
                                           std::vector<int> Indices,
                                           bool NoMissingDefsInPreprocess,
                                           OverallResult &Result) {
    if (Fun.isDeclaration())
        return PreservedAnalyses::all();

    Variable = Var;
    this->Indices = Indices;
    dontReturnMissingDef = NoMissingDefsInPreprocess;
    // Clear all sets
    DependentInstrs.clear();
    IncludedInstrs.clear();
    AffectedBasicBlocks.clear();
    IncludedBasicBlocks.clear();
    IncludedParams.clear();

    DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                    dbgs() << "Function: " << Fun.getName().str() << "\n");

    // First phase - determine which instructions are dependent on the parameter
    for (auto &BB : Fun) {
        if (isAffected(&BB))
            continue;
        for (auto &Instr : BB) {
            bool dependent = false;
            if (auto GEPInstr = dyn_cast<GetElementPtrInst>(&Instr)) {
                dependent = checkGEPDependency(GEPInstr);
            } else {
                for (auto &Op : Instr.operands()) {
                    if (checkDependency(&Op))
                        dependent = true;
                }
            }
            if (auto CallInstr = dyn_cast<CallInst>(&Instr)) {
                // Call instructions
                checkCallDependency(CallInstr);
            }
            if (auto PhiInstr = dyn_cast<PHINode>(&Instr)) {
                // Phi instructions
                if (checkPhiDependency(*PhiInstr))
                    dependent = true;
            }

            if (dependent) {
                addToDependent(&Instr);
                DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                { dbgs() << "Dependent: " << Instr << "\n"; });
                if (auto BranchInstr = dyn_cast<BranchInst>(&Instr)) {
                    auto affectedBBs = affectedBasicBlocks(BranchInstr);
                    addAllInstrs(affectedBBs);
                }
                if (auto StoreInstr = dyn_cast<StoreInst>(&Instr)) {
                    auto Ptr = StoreInstr->getPointerOperand();
                    if (auto PtrInstr = dyn_cast<Instruction>(Ptr)) {
                        addToDependent(PtrInstr);
                    }
                }
            }
        }
    }

    first_preprocessing = false;
    if (!MissingDefs.empty()) {
        Result.missingDefs.insert(Result.missingDefs.end(),
                                  MissingDefs.begin(),
                                  MissingDefs.end());
        return PreservedAnalyses::none();
    }

    // Second phase - determine which additional instructions we need to
    // produce a valid CFG
    DEBUG_WITH_TYPE(DEBUG_SIMPLL, dbgs() << "Second phase\n");
    // Recursively add all instruction operands to included
    for (auto &Inst : DependentInstrs) {
        if (isa<PHINode>(Inst))
            continue;
        addAllOpsToIncluded(Inst);
    }

    UnifyFunctionExitNodes unifyExitPass;
    unifyExitPass.runOnFunction(Fun);
    RetBB = unifyExitPass.getReturnBlock();

    for (auto &BB : Fun) {
        auto Term = dyn_cast<BranchInst>(BB.getTerminator());
        if (!Term)
            continue;
        if (isDependent(Term))
            continue;
        if (Term->getNumSuccessors() == 0)
            continue;

        // If there is just one necessary successor remove all others
        auto includedSucc = includedSuccessors(*Term, RetBB);
        if (includedSucc.size() <= 1) {
            auto NewSucc = includedSucc.empty() ? Term->getSuccessor(0)
                                                : *includedSucc.begin();

            // Notify successors about removing some branches
            for (auto TermSucc : successors(&BB)) {
                if (TermSucc != NewSucc)
                    TermSucc->removePredecessor(&BB, true);
            }
            // Create and insert new branch
            auto NewTerm = BranchInst::Create(NewSucc, Term);
            Term->eraseFromParent();
            IncludedInstrs.insert(NewTerm);
        } else {
            addToIncluded(Term);
            addAllOpsToIncluded(Term);
        }
    }
    IncludedInstrs.insert(DependentInstrs.begin(), DependentInstrs.end());

    // Add needed instructions coming to Phis to included
    for (auto &BB : Fun) {
        for (auto &Instr : BB) {
            if (auto Phi = dyn_cast<PHINode>(&Instr)) {
                if (!isIncluded(Phi))
                    continue;
                for (unsigned i = 0; i < Phi->getNumIncomingValues(); ++i) {
                    if (auto incomingInstr = dyn_cast<Instruction>(
                                Phi->getIncomingValue(i))) {
                        addToIncluded(incomingInstr);
                        addAllOpsToIncluded(incomingInstr);
                    }
                }
            }
        }
    }

    // Third phase - add useful debug info
    for (auto &BB : Fun) {
        for (auto &Inst : BB) {
            if (isIncludedDebugInfo(Inst)) {
                addToIncluded(&Inst);
            }
        }
    }

    // Fourth phase - remove unneeded instructions and keep the control flow
    std::vector<Instruction *> toRemove;
    int b = 0;
    for (auto &BB : Fun) {
        // Collect and clear all instruction that can be removed
        for (auto &Inst : BB) {
            if (!isIncluded(&Inst) && !Inst.isTerminator()) {
                DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                { dbgs() << "Clearing " << Inst << "\n"; });
                Inst.replaceAllUsesWith(UndefValue::get(Inst.getType()));
                toRemove.push_back(&Inst);
            }
        }
    }

    // Erase instructions
    for (auto &Inst : toRemove) {
        Inst->eraseFromParent();
    }
    // Clear BBs except first that have no incoming edges
    for (auto BB_it = ++Fun.begin(); BB_it != Fun.end();) {
        BasicBlock *BB = &*BB_it++;
        if (!isIncluded(BB) && pred_begin(BB) == pred_end(BB)) {
            DeleteDeadBlock(BB);
        }
    }
    // Erase basic blocks
    std::vector<BasicBlock *> BBToRemove;
    for (auto BB_it = ++Fun.begin(); BB_it != Fun.end();) {
        // First, erase all blocks that can be erased, except the entry one
        BasicBlock *BB = &*BB_it++;
        if (!isIncluded(BB)) {
            // When removing other than a first block, we need to
            // redirect incoming edges into the successor (a block that
            // is not included is guaranteed to have one successor).
            if (canRemoveBlock(BB)) {
                bool removed = TryToSimplifyUncondBranchFromEmptyBlock(BB);
                assert(removed || BB->getSingleSuccessor() == BB);
            }
        }
    }
    if (!isIncluded(&Fun.getEntryBlock())
        && canRemoveFirstBlock(&Fun.getEntryBlock())) {
        // Erase entry block if possible
        DeleteDeadBlock(&Fun.getEntryBlock());
    }

    // Remove unreachable BBs
    // @TODO There is a pass in LLVM for this but it fails. It might be fixed
    //       in a newer version of LLVM.
    deleteUnreachableBlocks(Fun);

    // If the return instruction is not included, we can transform the function
    // to return void
    if (RetBB && !RetBB->empty() && !isIncluded(RetBB->getTerminator())
        && !Fun.getReturnType()->isVoidTy()) {
        DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                        dbgs() << "Changing return type of " << Fun.getName()
                               << " to void.\n");
        changeToVoid(Fun);
    }

    DEBUG_WITH_TYPE(DEBUG_SIMPLL, {
        dbgs() << "Function " << Fun.getName().str() << " after cleanup:\n";
        Fun.print(dbgs());
        dbgs() << "\n";
    });
    return PreservedAnalyses::none();
}

/// Calculate the set of basic blocks affected by a conditional branch
/// A condition affects those blocks that are reachable through one branch
/// only: hence it is a difference of union and intersection of sets of
/// blocks reachable from individual branches.
std::vector<const BasicBlock *>
        VarDependencySlicer::affectedBasicBlocks(BranchInst *Branch) {
    std::set<const BasicBlock *> reachableUnion;
    std::set<const BasicBlock *> reachableIntersection;
    bool first = true;
    if (Branch->isConditional()) {
        for (auto Succ : Branch->successors()) {
            auto reachable = reachableBlocksThroughSucc(Branch, Succ);

            // Compute union with blocks reachable from other branches
            uniteWith(reachableUnion, reachable);

            // Compute intersection with blocks reachable from other branches
            if (first) {
                reachableIntersection = std::move(reachable);
                first = false;
            } else {
                intersectWith(reachableIntersection, reachable);
            }
        }
    }
    std::vector<const BasicBlock *> result;
    std::set_difference(reachableUnion.begin(),
                        reachableUnion.end(),
                        reachableIntersection.begin(),
                        reachableIntersection.end(),
                        std::back_inserter(result));
    return result;
}

/// Add all instructions of a basic block to included.
void VarDependencySlicer::addAllInstrs(
        const std::vector<const BasicBlock *> BBs) {
    for (auto BB : BBs) {
        AffectedBasicBlocks.insert(BB);
        IncludedBasicBlocks.insert(BB);
        for (auto &Instr : *BB) {
            DependentInstrs.insert(&Instr);
            DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                            { dbgs() << "Dependent: " << Instr << "\n"; });
        }
    }
}

/// Check if a value is dependent on the value of the global variable.
/// A value is dependent on a variable if it is the variable itself or if it is
/// a dependent value.
bool VarDependencySlicer::checkDependency(const Use *Op) {
    return checkDependencyAndDirectDependency(Op).first;
}

std::pair<bool, bool>
        VarDependencySlicer::checkDependencyAndDirectDependency(const Use *Op) {
    bool dependent = false;
    bool directly = false;
    if (dyn_cast<GlobalVariable>(Op) == Variable) {
        dependent = true;
        directly = true;
    } else if (auto GEPOp = dyn_cast<GEPOperator>(Op)) {
        dependent = checkGEPDependency(GEPOp);
    } else if (auto OpInst = dyn_cast<Instruction>(Op)) {
        if (isDependent(OpInst)) {
            dependent = true;
        }
    } else if (auto OpOperator = dyn_cast<Operator>(Op)) {
        for (auto &InnerOp : OpOperator->operands())
            if (checkDependency(&InnerOp))
                dependent = true;
    }
    return std::make_pair(dependent, directly);
}

bool VarDependencySlicer::checkGEPDependency(
        const GetElementPtrInst *GEPInstr) {
    if (auto GEPOp = dyn_cast<GEPOperator>(GEPInstr))
        return checkGEPDependency(GEPOp);
    return true;
}

bool VarDependencySlicer::checkGEPDependency(const GEPOperator *GEPInstr) {
    bool dependent = false;
    if (dyn_cast<GlobalVariable>(GEPInstr->getOperand(0)) == Variable) {
        if (GEPInstr->hasAllConstantIndices()) {
            std::vector<int> gep_indicies;
            for (int i = 1; i < GEPInstr->getNumIndices(); i++) {
                ConstantInt *CI =
                        dyn_cast<ConstantInt>(GEPInstr->getOperand(i));
                gep_indicies.push_back(CI->getZExtValue());
            }
            if (Indices == gep_indicies)
                dependent = true;
            else {
                gep_indicies.clear();
                APInt InstOffset(64, 0);
                GEPInstr->accumulateConstantOffset(
                        Variable->getParent()->getDataLayout(), InstOffset);
                if (Indices[0] == InstOffset.getZExtValue()) {
                    dependent = true;
                }
            }
        } else
            dependent = true;
    }
    return dependent;
}

bool VarDependencySlicer::checkCallDependency(const CallInst *CallInstr) {
    for (auto &Arg : CallInstr->arg_operands()) {
        auto dependency_info = checkDependencyAndDirectDependency(&Arg);
        if (dependency_info.first) {
            if (dependency_info.second && !Indices.empty()) {
                auto calledFunction =
                        getCalledFunction(CallInstr->getCalledValue());
                if (calledFunction->isDeclaration()) {
                    if (!dontReturnMissingDef) {
                        if (first_preprocessing)
                            MissingDefs.push_back({calledFunction, nullptr});
                        else
                            MissingDefs.push_back({nullptr, calledFunction});
                    }
                    return true;
                }

                for (auto &BB : *calledFunction) {
                    for (auto &Instr : BB) {
                        if (auto GEPInstr =
                                    dyn_cast<GetElementPtrInst>(&Instr)) {
                            if (checkGEPDependency(GEPInstr))
                                return true;
                        } else {
                            for (auto &Op : Instr.operands()) {
                                if (auto GEPOp = dyn_cast<GEPOperator>(Op)) {
                                    if (checkGEPDependency(GEPOp))
                                        return true;
                                }
                            }
                        }

                        if (auto CallInstr = dyn_cast<CallInst>(&Instr)) {
                            checkCallDependency(CallInstr);
                        }
                    }
                }
            } else
                return true;
        }
    }
    return false;
}

/// Add instruction to dependent instructions.
bool VarDependencySlicer::addToDependent(const Instruction *Instr) {
    return addToSet(Instr, DependentInstrs);
}

/// Add instruction to included instructions.
bool VarDependencySlicer::addToIncluded(const Instruction *Inst) {
    if (!isDependent(Inst))
        return addToSet(Inst, IncludedInstrs);
    else
        return false;
}

/// Add instruction to the given set of instructions.
bool VarDependencySlicer::addToSet(const Instruction *Inst,
                                   std::set<const Instruction *> &set) {

    bool added = false;
    auto add = set.insert(Inst);
    if (add.second) {
        added = true;
        IncludedBasicBlocks.insert(Inst->getParent());
    }
    return added;
}

/// Recursively add all operands of an instruction to included instructions.
/// \param Inst
/// \return
bool VarDependencySlicer::addAllOpsToIncluded(const Instruction *Inst) {
    bool added = false;
    for (auto &Op : Inst->operands()) {
        if (auto OpInst = dyn_cast<Instruction>(&Op)) {
            if (addToIncluded(OpInst)) {
                DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                { dbgs() << "Included: " << *OpInst << "\n"; });
                added = true;
                addAllOpsToIncluded(OpInst);
            }
            if (isa<AllocaInst>(OpInst))
                // For alloca, add all stores between the alloca and
                // the current instruction to included.
                addStoresToIncluded(OpInst, Inst);
        }
        if (auto OpParam = dyn_cast<Argument>(Op))
            IncludedParams.insert(OpParam);
    }
    return added;
}

/// Calculate which successors of a terminator instruction must be included.
/// We include a successor if there exists an included basic block that is
/// reachable only via this successor.
std::set<BasicBlock *>
        VarDependencySlicer::includedSuccessors(BranchInst &Terminator,
                                                const BasicBlock *ExitBlock) {

    // If block has multiple successors, choose which must be included
    if (Terminator.getNumSuccessors() == 0)
        return {};
    if (Terminator.getNumSuccessors() == 1)
        return {Terminator.getSuccessor(0)};

    auto TrueSucc = Terminator.getSuccessor(0);
    auto FalseSucc = Terminator.getSuccessor(1);

    // Find all included blocks (except exit block) that are reachable through
    // true edge
    auto reachableTrue = reachableBlocksThroughSucc(&Terminator, TrueSucc);
    intersectWith(reachableTrue, IncludedBasicBlocks);
    if (ExitBlock)
        reachableTrue.erase(ExitBlock);
    // Find all included blocks (except exit block) that are reachable through
    // false edge
    auto reachableFalse = reachableBlocksThroughSucc(&Terminator, FalseSucc);
    intersectWith(reachableFalse, IncludedBasicBlocks);
    if (ExitBlock)
        reachableFalse.erase(ExitBlock);

    if (reachableTrue != reachableFalse) {
        // If one successor covers all included blocks reachable from the other
        // successor, choose it
        if (std::includes(reachableTrue.begin(),
                          reachableTrue.end(),
                          reachableFalse.begin(),
                          reachableFalse.end()))
            return {TrueSucc};
        if (std::includes(reachableFalse.begin(),
                          reachableFalse.end(),
                          reachableTrue.begin(),
                          reachableTrue.end()))
            return {FalseSucc};
        // If neither of successors covers all blocks reachable by the other,
        // we have to follow both
        return {TrueSucc, FalseSucc};
    }

    // If sets of included blocks reachable through both successors are same
    // and non-empty, we need to decide which successor to keep.
    // One of them might reach other blocks through loop only and than we need
    // to keep the other one
    // TODO this should use loop analysis
    if (!reachableTrue.empty()) {
        if (!isPotentiallyReachable(TrueSucc, Terminator.getParent())) {
            return {TrueSucc};
        } else if (!isPotentiallyReachable(FalseSucc, Terminator.getParent())) {
            return {FalseSucc};
        } else {
            return {TrueSucc == ExitBlock ? FalseSucc : TrueSucc};
        }
    } else {
        return {TrueSucc == ExitBlock ? FalseSucc : TrueSucc};
    }
}

/// Check if a basic block can be removed.
/// If a removal of bb would result in a situation that there exists a phi
/// node Phi with two different incoming values for the same incoming
/// block (which is a predecessor Pred of bb), we cannot remove bb
bool VarDependencySlicer::canRemoveBlock(const BasicBlock *bb) {
    if (bb->getTerminator()->getNumSuccessors() != 1)
        return false;

    for (const PHINode &Phi : bb->getTerminator()->getSuccessor(0)->phis()) {
        for (unsigned i = 0; i < Phi.getNumIncomingValues(); ++i) {
            if (Phi.getIncomingBlock(i) == bb)
                continue;
            for (auto Pred : predecessors(bb)) {
                if (Phi.getBasicBlockIndex(Pred) < 0)
                    continue;

                auto PredPhiValue = Phi.getIncomingValueForBlock(Pred);
                if (PredPhiValue != Phi.getIncomingValueForBlock(bb))
                    return false;
            }
        }
    }

    return true;
}

/// Check if the first basic block can be removed.
// The first block cannot be removed if it has a successor that is included
// and has incoming edges (since first block cannot have incoming edges).
bool VarDependencySlicer::canRemoveFirstBlock(const BasicBlock *bb) {
    for (const auto &Succ : successors(bb)) {
        if (isIncluded(Succ) && pred_begin(Succ) != pred_end(Succ))
            return false;
    }
    return true;
}

/// Calculate the set of all basic blocks reachable from some block in
/// a function.
std::set<const BasicBlock *>
        VarDependencySlicer::reachableBlocks(const BasicBlock *Src,
                                             Function &Fun) {
    std::set<const BasicBlock *> result;
    for (auto &BB : Fun) {
        if (Src != &BB && isPotentiallyReachable(Src, &BB))
            result.insert(&BB);
    }
    return result;
}

/// Calculate a set of all basic blocks that are reachable via a successor of
/// a terminator instruction.
std::set<const BasicBlock *>
        VarDependencySlicer::reachableBlocksThroughSucc(Instruction *Terminator,
                                                        BasicBlock *Succ) {
    // Replace terminator by unconditional branch and find all blocks reachable
    // through the new branch (one that omits all other successors)
    auto NewBranch = BranchInst::Create(Succ, Terminator);
    Terminator->removeFromParent();
    auto reachable =
            reachableBlocks(NewBranch->getParent(), *Succ->getParent());

    // Restore original terminator
    Terminator->insertBefore(NewBranch);
    NewBranch->eraseFromParent();

    return reachable;
}

/// Set intersection. Result is stored in the first set.
void VarDependencySlicer::intersectWith(
        std::set<const BasicBlock *> &set,
        const std::set<const BasicBlock *> &other) {
    std::set<const BasicBlock *> tmpSet;
    std::set_intersection(set.begin(),
                          set.end(),
                          other.begin(),
                          other.end(),
                          std::inserter(tmpSet, tmpSet.begin()));
    set = std::move(tmpSet);
}

/// Set union. Result is stored in the first set.
void VarDependencySlicer::uniteWith(std::set<const BasicBlock *> &set,
                                    const std::set<const BasicBlock *> &other) {
    std::set<const BasicBlock *> tmpSet;
    std::set_union(set.begin(),
                   set.end(),
                   other.begin(),
                   other.end(),
                   std::inserter(tmpSet, tmpSet.begin()));
    set = std::move(tmpSet);
}

/// Check if an instruction is dependent on the value of the global variable.
bool VarDependencySlicer::isDependent(const Instruction *Instr) {
    return DependentInstrs.find(Instr) != DependentInstrs.end();
}

/// Check if an instruction must be included.
bool VarDependencySlicer::isIncluded(const Instruction *Instr) {
    return IncludedInstrs.find(Instr) != IncludedInstrs.end();
}

// Check if a basic block is affected by the value of the global variable.
bool VarDependencySlicer::isAffected(const BasicBlock *BB) {
    return AffectedBasicBlocks.find(BB) != AffectedBasicBlocks.end();
}

// Check if a basic block must be included.
bool VarDependencySlicer::isIncluded(const BasicBlock *BB) {
    return IncludedBasicBlocks.find(BB) != IncludedBasicBlocks.end();
}

// Check if a function parameter must be included.
bool VarDependencySlicer::isIncluded(const Argument *Param) {
    return IncludedParams.find(Param) != IncludedParams.end();
}

// Check if the instruction is a debug info that must be included.
bool VarDependencySlicer::isIncludedDebugInfo(const Instruction &Inst) {
    if (auto CallInstr = dyn_cast<CallInst>(&Inst)) {
        if (!CallInstr->getCalledFunction()
            || !isDebugInfo(*CallInstr->getCalledFunction()))
            return false;
        const auto *VarMD = dyn_cast<MetadataAsValue>(CallInstr->getOperand(0))
                                    ->getMetadata();
        if (const auto *Var = dyn_cast<ValueAsMetadata>(VarMD)) {
            if (auto InstrVar = dyn_cast<Instruction>(Var->getValue())) {
                return isIncluded(InstrVar);
            }
            if (auto Param = dyn_cast<Argument>(Var->getValue()))
                return isIncluded(Param);
        }
    }
    return false;
}

/// Check if a PHI instruction must be included.
bool VarDependencySlicer::checkPhiDependency(const PHINode &Phi) {
    Value *Val = nullptr;
    // Check if there are two incoming dependent blocks with different values
    bool has_included = false;
    for (auto incomingBB : Phi.blocks()) {
        if (isIncluded(incomingBB)) {
            has_included = true;
            auto *BBVal = Phi.getIncomingValueForBlock(incomingBB);
            if (!Val)
                Val = BBVal;
            else {
                if (BBVal != Val)
                    return true;
            }
        }
    }
    if (!has_included)
        return false;
    // If there are no such two blocks, check if there is another block with
    // a  different incoming value that is possibly not removed in future
    for (auto incomingBB : Phi.blocks()) {
        if (!isIncluded(incomingBB)) {
            auto *BBVal = Phi.getIncomingValueForBlock(incomingBB);
            if (BBVal != Val) {
                for (auto included : IncludedBasicBlocks) {
                    // Do not consider those blocks whose terminator is not
                    // included (since we search for included blocks where both
                    // branches can be included and one of them leads through
                    // a block from which a different value comes to PHI
                    if (!isIncluded(included->getTerminator()))
                        continue;

                    if (included->getTerminator()->getNumSuccessors() == 2) {
                        if (isPotentiallyReachable(
                                    included->getTerminator()->getSuccessor(0),
                                    incomingBB)
                            != isPotentiallyReachable(
                                    included->getTerminator()->getSuccessor(1),
                                    incomingBB))
                            return true;
                    }
                }
            }
        }
    }
    return false;
}

/// Add all stores to an allocated memory between the allocation and a read
/// access into included.
bool VarDependencySlicer::addStoresToIncluded(const Instruction *Alloca,
                                              const Instruction *Use) {
    bool added = false;
    std::list<const Instruction *> worklist;
    worklist.push_back(Alloca->getNextNode());
    std::set<const Instruction *> visited;
    visited.insert(Alloca);
    visited.insert(Use);
    while (!worklist.empty()) {
        const Instruction *Current = worklist.front();
        worklist.pop_front();
        // Add store instruction with alloca as operand
        if (auto Store = dyn_cast<StoreInst>(Current)) {
            if (Store->getPointerOperand() == Alloca) {
                if (addToIncluded(Store)) {
                    added = true;
                    addAllOpsToIncluded(Store);
                }
            }
        }
        // Add call instruction with alloca as operand
        if (auto Call = dyn_cast<CallInst>(Current)) {
            for (auto &Op : Call->operands()) {
                if (Op == Alloca) {
                    if (addToIncluded(Call)) {
                        added = true;
                        addAllOpsToIncluded(Call);
                    }
                }
            }
        }
        // If alloca is bitcasted or GEP-ed, run search for current
        if (auto BitCast = dyn_cast<BitCastInst>(Current)) {
            if (BitCast->getOperand(0) == Alloca)
                if (addStoresToIncluded(Current, Use))
                    added = true;
        }
        if (auto GEP = dyn_cast<GetElementPtrInst>(Current)) {
            if (GEP->getPointerOperand() == Alloca)
                if (addStoresToIncluded(Current, Use))
                    added = true;
        }

        std::vector<const Instruction *> next;
        if (auto Branch = dyn_cast<BranchInst>(Current)) {
            for (auto succ : Branch->successors())
                next.push_back(&*succ->begin());
        } else {
            if (Current->getNextNode())
                next.push_back(Current->getNextNode());
        }
        for (auto &n : next) {
            if (visited.find(n) == visited.end())
                worklist.push_back(n);
        }

        visited.insert(Current);
    }
    return added;
}

/// Recursively calculate the set of all blocks reachable from BB.
/// \param BB Block to search from
/// \param Reachable Set of reachale blocks (new found blocks will be added).
/// \param Visited Set of visited blocks.
void calculateReachableBlocksRecursive(BasicBlock *BB,
                                       std::set<BasicBlock *> &Reachable,
                                       std::set<BasicBlock *> &Visited) {
    if (Visited.find(BB) != Visited.end())
        return;
    Visited.insert(BB);

    for (auto Succ : successors(BB)) {
        Reachable.insert(Succ);
        calculateReachableBlocksRecursive(Succ, Reachable, Visited);
    }
}

/// Deleting unreachable blocks.
void VarDependencySlicer::deleteUnreachableBlocks(Function &Fun) {
    std::set<BasicBlock *> Reachable = {&Fun.getEntryBlock()};
    std::set<BasicBlock *> Visited;
    calculateReachableBlocksRecursive(&Fun.getEntryBlock(), Reachable, Visited);

    std::vector<BasicBlock *> toRemove;
    for (auto &BB : Fun) {
        if (Reachable.find(&BB) == Reachable.end()) {
            // Replace uses of instructions that will be deleted
            for (auto &Instr : BB)
                Instr.replaceAllUsesWith(UndefValue::get(Instr.getType()));
            // Notify successors about deletion of the block
            for (auto SI = succ_begin(&BB), E = succ_end(&BB); SI != E; ++SI)
                (*SI)->removePredecessor(&BB);
            BB.dropAllReferences();
            toRemove.push_back(&BB);
        }
    }
    // Actually delete unreachable blocks
    for (auto BB : toRemove)
        BB->eraseFromParent();
}

/// Change return type of the function to void.
/// This can be done only if the function is not called.
/// The function is cloned, the original function is kept with a new name having
/// ".old" suffix.
void VarDependencySlicer::changeToVoid(Function &Fun) {
    // Check if function is called from other function
    for (auto &Use : Fun.uses())
        if (isa<Instruction>(Use.getUser()))
            return;

    // Crete new function declaration
    auto NewType = FunctionType::get(
            Type::getVoidTy(Fun.getContext()),
            std::vector<Type *>(Fun.getFunctionType()->param_begin(),
                                Fun.getFunctionType()->param_end()),
            Fun.isVarArg());
    auto NewFun = Function::Create(
            NewType, Fun.getLinkage(), Fun.getName(), Fun.getParent());

    // Map function arguments
    ValueToValueMapTy ArgMap;
    for (auto AI = Fun.arg_begin(),
              NAI = NewFun->arg_begin(),
              E = Fun.arg_end();
         AI != E;
         ++AI, ++NAI) {
        ArgMap.insert({&*AI, &*NAI});
    }

    // Clone function
    SmallVector<ReturnInst *, 8> Returns;
    CloneFunctionInto(NewFun, &Fun, ArgMap, true, Returns);

    // Change return instructions to return void
    for (auto *Ret : Returns) {
        BasicBlock *RetBB = Ret->getParent();
        Ret->eraseFromParent();
        Ret = ReturnInst::Create(Fun.getContext(), RetBB);
    }

    // Rename functions
    // New function gets original name. Old functions gets ".old" suffix.
    auto Name = Fun.getName();
    Fun.setName(Name + ".old");
    NewFun->setName(Name);
}
