//===----- ValuePatternComparator.h - Code pattern instruction matcher ----===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Petr Silling, psilling@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of the value pattern matcher. The
/// value pattern matcher is a comparator extension of the LLVM
/// FunctionComparator tailored to value pattern comparison.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_VALUEPATTERNCOMPARATOR_H
#define DIFFKEMP_SIMPLL_VALUEPATTERNCOMPARATOR_H

#include "FunctionComparator.h"
#include "PatternSet.h"
#include <unordered_map>
#include <vector>

using namespace llvm;

/// Extension of LLVM FunctionComparator which compares a pattern value against
/// a given module value. Compared values are expected to lie in different
/// functions from different modules. The module function is expected on the
/// left side while the pattern function is expected on the right side.
class ValuePatternComparator : protected FunctionComparator {
  public:
    ValuePatternComparator(const Function *ModFun,
                           const Function *PatFun,
                           const ValuePattern *ParentPattern)
            : FunctionComparator(ModFun, PatFun, nullptr),
              IsLeftSide(PatFun == ParentPattern->PatternL),
              ParentPattern(ParentPattern){};

    /// Compare the given module value with the pattern value.
    int compare() override;

    /// Set the module value that should be compared.
    void setComparedValue(const Value *ModVal);

  protected:
    /// Compare a module value with a pattern value without using serial
    /// numbers.
    int cmpValues(const Value *L, const Value *R) const override;

  private:
    /// Whether the comparator has been created for the left pattern side.
    const bool IsLeftSide;
    /// The pattern which should be used during comparison.
    const ValuePattern *ParentPattern;
    /// The module value that should be compared against the pattern value.
    mutable const Value *ComparedValue;
};

#endif // DIFFKEMP_SIMPLL_VALUEPATTERNCOMPARATOR_H
