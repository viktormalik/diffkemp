//===----- InstPatternComparator.h - Code pattern instruction matcher -----===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Petr Silling, psilling@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of the LLVM code pattern matcher. The
/// pattern matcher is a comparator extension of the LLVM FunctionComparator
/// tailored to comparison of general instruction-based patterns.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_INSTPATTERNCOMPARATOR_H
#define DIFFKEMP_SIMPLL_INSTPATTERNCOMPARATOR_H

#include "FunctionComparator.h"
#include "PatternSet.h"
#include <unordered_map>
#include <vector>

using namespace llvm;

/// Extension of LLVM FunctionComparator which compares a difference pattern
/// against its corresponding module function. Compared functions are expected
/// to lie in different modules. The module function is expected on the left
/// side while the pattern function is expected on the right side.
class InstPatternComparator : protected FunctionComparator {
  public:
    /// Pattern instructions matched to their respective module replacement
    /// instructions. Pattern instructions are used as keys.
    mutable InstructionMap InstMatchMap;
    /// Pattern input instructions and arguments matched to module input
    /// instructions and arguments. Pattern input is used for keys.
    mutable InputMap InputMatchMap;
    /// Module input instructions and arguments matched to pattern input
    /// instructions and arguments. Module input is used for keys.
    mutable InputMap ReverseInputMatchMap;

    InstPatternComparator(const Function *ModFun,
                          const Function *PatFun,
                          const InstPattern *ParentPattern)
            : FunctionComparator(ModFun, PatFun, nullptr),
              IsLeftSide(PatFun == ParentPattern->PatternL),
              ParentPattern(ParentPattern){};

    /// Compare the module function and the difference pattern from the starting
    /// module instruction.
    int compare() override;

    /// Set the starting module instruction.
    void setStartInstruction(const Instruction *StartModInst);

    /// Compare a module input value with a pattern input value.
    int cmpInputValues(const Value *L, const Value *R);

  protected:
    /// Compare a module GEP operation with a pattern GEP operation.
    int cmpGEPs(const GEPOperator *GEPL,
                const GEPOperator *GEPR) const override;

    /// Compare a module function instruction with a pattern instruction along
    /// with their operands.
    int cmpOperationsWithOperands(const Instruction *L,
                                  const Instruction *R) const;

    /// Compare a module function basic block with a pattern basic block.
    int cmpBasicBlocks(const BasicBlock *BBL,
                       const BasicBlock *BBR) const override;

    /// Compare global values by their names, because their indexes are not
    /// expected to be the same.
    int cmpGlobalValues(GlobalValue *L, GlobalValue *R) const override;

    /// Compare a module value with a pattern value using serial numbers.
    int cmpValues(const Value *L, const Value *R) const override;

  private:
    /// Set of mapped and synchronized values.
    using MappingSet = SmallPtrSet<const Value *, 16>;

    /// Whether the comparator has been created for the left pattern side.
    const bool IsLeftSide;
    /// The pattern which should be used during comparison.
    const InstPattern *ParentPattern;
    /// The staring instruction of the compared module function.
    mutable const Instruction *StartInst;
    /// Current position in the the compared module function.
    mutable const Instruction *PositionL;
    /// Current position in the the compared pattern function.
    mutable const Instruction *PositionR;
    /// Values placed into synchronisation maps during the comparison of the
    /// current instruction pair.
    mutable MappingSet NewlyMappedValuesL, NewlyMappedValuesR;
    /// Input instructions that have been mapped during the comparison of the
    /// current instruction pair.
    mutable MappingSet NewlyMappedInputL, NewlyMappedInputR;
    /// Current instruction group depth.
    mutable int GroupDepth = 0;

    /// Uses function comparison to try and match the given pattern to the
    /// corresponding module.
    int matchPattern() const;

    /// Erases newly mapped instructions from synchronization maps and input
    /// maps.
    void eraseNewlyMapped() const;

    /// Checks whether all currently mapped input instructions or arguments have
    /// an associated module counterpart.
    int checkInputMapping() const;

    /// Tries to map a module value (including possible predecessors) to a
    /// pattern input value.
    int mapInputValues(const Value *L, const Value *R) const;

    /// Position the basic block instruction iterator forward to the given
    /// instruction.
    void jumpToInst(BasicBlock::const_iterator &BBIterator,
                    const Instruction *Start) const;
};

#endif // DIFFKEMP_SIMPLL_INSTPATTERNCOMPARATOR_H
