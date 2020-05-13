//===- VarDependencySlicer.h - Slicing program w.r.t. value of a variable -===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Tomas Glozar, tglozar@gmail.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of the VarDependencySlicer pass.
/// The pass slices the program w.r.t. to a value of a global variable.
///
//===----------------------------------------------------------------------===//

#ifndef PROJECT_VARDEPENDENCYSLICER_H
#define PROJECT_VARDEPENDENCYSLICER_H

#include <Result.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/PassManager.h>
#include <set>
#include <utility>
#include <vector>

using namespace llvm;

/// A pass slicing a program w.r.t. to a value of a global variable.
/// Only the instructions whose value or execution is dependent on the value of
/// the variable are kept, the rest is removed.
/// Also, additional instructions that are needed to produce a valid LLVM module
/// are kept.
class VarDependencySlicer : public PassInfoMixin<VarDependencySlicer> {
  public:
    PreservedAnalyses run(Function &Fun,
                          FunctionAnalysisManager &fam,
                          GlobalVariable *Var,
                          std::vector<int> Indices,
                          bool NoMissingDefsInPreprocess,
                          OverallResult &Result);

  private:
    GlobalVariable *Variable = nullptr;
    std::vector<int> Indices;
    // Instructions directly dependent on the parameter
    std::set<const Instruction *> DependentInstrs = {};
    // Instructions that must be included
    std::set<const Instruction *> IncludedInstrs = {};
    // Basics blocks whose execution is dependent on the parameter
    std::set<const BasicBlock *> AffectedBasicBlocks = {};
    // Basic blocks that must be included
    std::set<const BasicBlock *> IncludedBasicBlocks = {};
    // Function parameters to be included
    std::set<const Argument *> IncludedParams = {};

    // Return block
    BasicBlock *RetBB = nullptr;

    // MissingDefs
    bool dontReturnMissingDef = false;
    std::vector<GlobalValuePair> MissingDefs;

    // Functions for adding to sets
    void addAllInstrs(const std::vector<const BasicBlock *> BBs);
    bool addToSet(const Instruction *Inst, std::set<const Instruction *> &set);
    bool addToDependent(const Instruction *Instr);
    bool addToIncluded(const Instruction *Inst);
    bool addAllOpsToIncluded(const Instruction *Inst);
    bool addStoresToIncluded(const Instruction *Alloca, const Instruction *Use);

    // Functions for searching sets
    inline bool isDependent(const Instruction *Instr);
    inline bool isIncluded(const Instruction *Instr);
    inline bool isAffected(const BasicBlock *BB);
    inline bool isIncluded(const BasicBlock *BB);
    inline bool isIncluded(const Argument *Param);

    // Computing affected and included basic blocks
    std::vector<const BasicBlock *> affectedBasicBlocks(BranchInst *Branch);
    std::set<BasicBlock *> includedSuccessors(BranchInst &Terminator,
                                              const BasicBlock *ExitBlock);

    bool checkPhiDependency(const PHINode &Phi);
    bool checkGEPDependency(const GetElementPtrInst *GEPInstr);
    bool checkGEPDependency(const GEPOperator *GEPInstr);
    bool checkCallDependency(const CallInst *CallInstr);

    // Computing reachable blocks
    std::set<const BasicBlock *> reachableBlocks(const BasicBlock *Src,
                                                 Function &Fun);
    std::set<const BasicBlock *>
            reachableBlocksThroughSucc(Instruction *Terminator,
                                       BasicBlock *Succ);

    // Set operations
    void intersectWith(std::set<const BasicBlock *> &set,
                       const std::set<const BasicBlock *> &other);
    void uniteWith(std::set<const BasicBlock *> &set,
                   const std::set<const BasicBlock *> &other);

    bool checkDependency(const Use *Op);
    std::pair<bool, bool> checkDependencyAndDirectDependency(const Use *Op);

    bool canRemoveBlock(const BasicBlock *bb);
    bool canRemoveFirstBlock(const BasicBlock *bb);

    bool isIncludedDebugInfo(const Instruction &Inst);

    /// Delete all blocks that are not reachable from the entry block
    /// \param Fun
    void deleteUnreachableBlocks(Function &Fun);

    /// Change return type of the function to void if possible.
    void changeToVoid(Function &Fun);

    // Is the first function?
    static bool first_preprocessing;
};

#endif // PROJECT_VARDEPENDENCYSLICER_H
