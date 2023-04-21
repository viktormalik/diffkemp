//===- DifferentialFunctionComparator.h - Comparing functions for equality ===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of the DifferentialFunctionComparator
/// class that extends the FunctionComparator from the LLVM infrastructure for
/// the specific purposes of SimpLL.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_DIFFERENTIALFUNCTIONCOMPARATOR_H
#define DIFFKEMP_SIMPLL_DIFFERENTIALFUNCTIONCOMPARATOR_H

#include "DebugInfo.h"
#include "FunctionComparator.h"
#include "ModuleComparator.h"
#include "PatternComparator.h"
#include "Utils.h"

using namespace llvm;

/// Extension of FunctionComparator from LLVM designed to compare functions in
/// different modules (original FunctionComparator assumes that both functions
/// are in a single module).

class DifferentialFunctionComparator : public FunctionComparator {
  public:
    DifferentialFunctionComparator(const Function *F1,
                                   const Function *F2,
                                   const Config &config,
                                   const DebugInfo *DI,
                                   const PatternSet *PS,
                                   ModuleComparator *MC)
            : FunctionComparator(F1, F2, nullptr), config(config), DI(DI),
              LayoutL(F1->getParent()->getDataLayout()),
              LayoutR(F2->getParent()->getDataLayout()),
              PatternComp(PatternComparator(PS, this, F1, F2)),
              ModComparator(MC) {}

    int compare() override;
    /// Compares values by their synchronisation. The comparison is unsuccessful
    /// if the given values are not mapped to each other.
    int cmpValuesByMapping(const Value *L, const Value *R) const;
    /// Check if two instructions were already compared as equal.
    bool equal(const Instruction *L, const Instruction *R);
    /// Retrieves the value that is mapped to the given value, taken from one of
    /// the compared modules. When no such mapping exists, returns a null
    /// pointer.
    const Value *getMappedValue(const Value *Val, bool ValFromL) const;
    /// Storing pointers to instructions in which functions started to differ.
    mutable std::pair<const Instruction *, const Instruction *>
            DifferingInstructions;

  protected:
    /// Initialize relocation info
    void beginCompare() override;
    /// Specific comparison of GEP instructions/operators.
    /// Handles situation when there is an offset between matching GEP indices
    /// in the compared modules (when a struct type has different fields).
    int cmpGEPs(const GEPOperator *GEPL,
                const GEPOperator *GEPR) const override;
    /// Specific comparison of attribute lists.
    /// Attributes that do not affect the semantics of functions are removed.
    int cmpAttrs(const AttributeList L, const AttributeList R) const override;
    /// Compare CallInsts using cmpAllocs.
    int cmpOperations(const Instruction *L,
                      const Instruction *R,
                      bool &needToCmpOperands) const override;
    /// Handle comparing of memory allocation function in cases where the size
    /// of the composite type is different.
    int cmpAllocs(const CallInst *CL, const CallInst *CR) const;
    /// Compare two function calls where one has an extra argument.
    /// Such calls are compared as equal if they only differ in the last
    /// argument which is 0 or NULL.
    int cmpCallsWithExtraArg(const CallInst *CL, const CallInst *CR) const;
    /// Compares array types with equivalent element types and all integer types
    /// as equal when comparing the control flow only.
    int cmpTypes(Type *L, Type *R) const override;
    /// Do not compare bitwidth when comparing the control flow only.
    int cmpAPInts(const APInt &L, const APInt &R) const override;
    /// Does additional comparisons based on the C source to determine whether
    /// two call function arguments that may be compared as non-equal by LLVM
    /// are actually semantically equal.
    bool cmpCallArgumentUsingCSource(const CallInst *CIL,
                                     const CallInst *CIR,
                                     Value *OpL,
                                     Value *OpR,
                                     unsigned i) const;
    /// Compare two instructions along with their operands.
    int cmpOperationsWithOperands(const Instruction *L,
                                  const Instruction *R) const;
    /// Detect cast instructions and ignore them when comparing the control flow
    /// only. (The rest is the same as in LLVM.)
    int cmpBasicBlocks(const BasicBlock *BBL,
                       const BasicBlock *BBR) const override;
    /// Implement comparison of global values that does not use a
    /// GlobalNumberState object, since that approach does not fit the use case
    /// of comparing functions in two different modules.
    int cmpGlobalValues(GlobalValue *L, GlobalValue *R) const override;
    /// Specific comparing of sequences of field accesses.
    int cmpFieldAccess(BasicBlock::const_iterator &InstL,
                       BasicBlock::const_iterator &InstR) const;
    /// Specific comparing of values. Handles values generated from macros
    /// whose value changed and values where at least one of them is a cast.
    int cmpValues(const Value *L, const Value *R) const override;
    /// Specific comparing of constants. If one of them (or both) is a cast
    /// constant expression, compare its operand.
    int cmpConstants(const Constant *L, const Constant *R) const override;
    /// Specific comarison of memcpy instructions
    int cmpMemset(const CallInst *CL, const CallInst *CR) const;
    /// Compare and integer value with and LLVM constant
    int cmpIntWithConstant(uint64_t Integer, const Value *Const) const;
    /// Comparing PHI instructions
    int cmpPHIs(const PHINode *PhiL, const PHINode *PhiR) const;
    /// Try to find a matching instruction that has been moved forward in one of
    /// the basic blocks. If such instruction if found, a relocation is created.
    bool findMatchingOpWithOffset(BasicBlock::const_iterator &InstL,
                                  BasicBlock::const_iterator &InstR,
                                  Program prog_to_search) const;

  private:
    friend class PatternComparator;

    const Config &config;
    const DebugInfo *DI;

    const DataLayout &LayoutL, &LayoutR;

    mutable const DebugLoc *CurrentLocL, *CurrentLocR;
    mutable std::set<std::pair<const Value *, const Value *>> inverseConditions;
    mutable std::vector<std::pair<const PHINode *, const PHINode *>>
            phisToCompare;
    mutable std::unordered_map<const Value *, const Value *>
            ignoredInstructions;
    /// Contains pairs of values mapped by synchronisation maps. Enables
    /// retrieval of mapped values based on assigned numbers.
    mutable std::unordered_map<int, std::pair<const Value *, const Value *>>
            mappedValuesBySn;

    /// Relocation information type. Supports relocation of a sequential block
    /// of code.
    struct RelocationInfo {
        // Current status of the relocation.
        //  - None: no unresolved relocation found
        //  - Stored: found a block of code between instructions begin and end
        //            that is present in program prog, but was possibly
        //            relocated in the other program
        //  - Matching: trying to match the previously found relocated block
        //              to some block in the other program, after this is done,
        //              the comparison will continue from instruction restore
        //              in prog
        enum Status { None, Stored, Matching };
        Status status = None;
        Program prog;
        BasicBlock::const_iterator begin;
        BasicBlock::const_iterator end;
        BasicBlock::const_iterator restore;
        CallPair tryInlineBackup;
    };
    mutable RelocationInfo Reloc;

    mutable PatternComparator PatternComp;
    ModuleComparator *ModComparator;

    /// Try to find a syntax difference that could be causing the semantic
    /// difference that was found. Looks for differences that cannot be detected
    /// by simply diffing the compared functions - differences in macros, inline
    /// assembly code, or in types.
    void findDifference(const Instruction *L, const Instruction *R) const;

    /// Looks for inline assembly differences between the certain values.
    /// Note: passing the parent function is necessary in order to properly
    /// generate the SyntaxDifference object.
    std::vector<std::unique_ptr<SyntaxDifference>>
            findAsmDifference(const CallInst *IL, const CallInst *IR) const;

    /// Finds all differences between source types in GEPs inside two field
    /// access operations and records them using findTypeDifference.
    void findTypeDifferences(const GetElementPtrInst *FAL,
                             const GetElementPtrInst *FAR,
                             const Function *L,
                             const Function *R) const;

    /// Find and record a difference between structure types.
    void findTypeDifference(StructType *L,
                            StructType *R,
                            const Function *FL,
                            const Function *FR) const;

    /// Detects a change from a function to a macro between two instructions.
    /// This is necessary because such a change isn't visible in C source.
    void findMacroFunctionDifference(const Instruction *L,
                                     const Instruction *R) const;

    /// Takes all GEPs in a basic block and computes the sum of their offsets if
    /// constant (if not, it returns false).
    bool accumulateAllOffsets(const BasicBlock &BB, uint64_t &Offset) const;

    /// Check if the given instruction can be ignored (it does not affect
    /// semantics). Replacements of ignorable instructions are stored
    /// inside the ignored instructions map.
    bool maySkipInstruction(const Instruction *Inst) const;

    /// Check whether the given cast can be ignored (it does not affect
    /// semantics. First operands of ignorable casts are stored as their
    /// replacements inside the ignored instructions map.
    bool maySkipCast(const User *Cast) const;

    /// Check whether the given instruction is a repetitive variant of a
    /// previous load with no store instructions in between. Replacements of
    /// ignorable loads are stored inside the ignored instructions map.
    bool maySkipLoad(const LoadInst *Load) const;

    /// Retrive the replacement for the given value from the ignored
    /// instructions map. Try to generate the replacement if a bitcast is given.
    const Value *
            getReplacementValue(const Value *Replaced,
                                DenseMap<const Value *, int> &sn_map) const;

    /// Creates new value mappings according to the current pattern match.
    void createPatternMapping() const;

    /// Check if the given instruction has been matched to a pattern and,
    /// therefore, does not need to be analyzed nor mapped again.
    bool isPartOfPattern(const Instruction *Inst) const;

    /// Undo the changes made to synchronisation maps during the last
    /// instruction pair comparison.
    void undoLastInstCompare(BasicBlock::const_iterator &InstL,
                             BasicBlock::const_iterator &InstR) const;

    /// Does additional operations in cases when a difference between two
    /// CallInsts or their arguments is detected.
    /// This consists of three parts:
    /// 1. Compare the called functions using cmpGlobalValues (covers case when
    /// they are not compared in cmpBasicBlocks because there is another
    /// difference).
    /// 2. Try to inline the functions.
    /// 3. Look a macro-function difference.
    void processCallInstDifference(const CallInst *CL,
                                   const CallInst *CR) const;

    /// Check if there is a dependency between the given instruction and the
    /// currently stored relocation.
    /// There is a dependency if both the instruction and the relocated code
    /// (any instruction within it) access the same pointer and one of the
    /// accesses is a store and the other one is a load.
    bool isDependingOnReloc(const Instruction &Inst) const;

    friend class PatternGenerator;
};

#endif // DIFFKEMP_SIMPLL_DIFFERENTIALFUNCTIONCOMPARATOR_H
