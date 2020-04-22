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
                                   ModuleComparator *MC)
            : FunctionComparator(F1, F2, nullptr), config(config), DI(DI),
              LayoutL(F1->getParent()->getDataLayout()),
              LayoutR(F2->getParent()->getDataLayout()), ModComparator(MC) {}

    int compare() override;

  protected:
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
    /// Comparing of a structure size with a constant
    int cmpStructTypeSizeWithConstant(StructType *Type,
                                      const Value *Const) const;
    /// Comparing PHI instructions
    int cmpPHIs(const PHINode *PhiL, const PHINode *PhiR) const;

  private:
    const Config &config;
    const DebugInfo *DI;

    const DataLayout &LayoutL, &LayoutR;

    mutable const DebugLoc *CurrentLocL, *CurrentLocR;
    mutable std::set<std::pair<const Value *, const Value *>> inverseConditions;
    mutable std::vector<std::pair<const PHINode *, const PHINode *>>
            phisToCompare;

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
    /// access abstractions and records them using findTypeDifference.
    void findTypeDifferences(const Function *FAL,
                             const Function *FAR,
                             const Function *L,
                             const Function *R) const;

    /// Find and record a difference between structure types.
    void findTypeDifference(StructType *L,
                            StructType *R,
                            const Function *FL,
                            const Function *FR) const;

    /// Find type differences between calls to field access abstractions.
    void findTypeDifferenceInChainedFieldAccess(const CallInst *CL,
                                                const CallInst *CR) const;

    /// Detects a change from a function to a macro between two instructions.
    /// This is necessary because such a change isn't visible in C source.
    void findMacroFunctionDifference(const Instruction *L,
                                     const Instruction *R) const;

    /// Takes all GEPs in a basic block and computes the sum of their offsets if
    /// constant (if not, it returns false).
    bool accumulateAllOffsets(const BasicBlock &BB, uint64_t &Offset) const;

    /// Check if the given operation can be ignored (it does not affect
    /// semantics) for control flow only diffs.
    bool mayIgnore(const User *Inst) const;

    /// Does additional operations in cases when a difference between two
    /// CallInsts or their arguments is detected.
    /// This consists of three parts:
    /// 1. Compare the called functions using cmpGlobalValues (covers case when
    /// they are not compared in cmpBasicBlocks because there is another
    /// difference).
    /// 2. Try to inline the functions.
    /// 3. Look a macro-function differenfce.
    void processCallInstDifference(const CallInst *CL,
                                   const CallInst *CR) const;
};

#endif // DIFFKEMP_SIMPLL_DIFFERENTIALFUNCTIONCOMPARATOR_H
