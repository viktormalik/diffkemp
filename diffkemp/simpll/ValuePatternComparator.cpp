//===---- ValuePatternComparator.cpp - Code pattern instruction matcher ---===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Petr Silling, psilling@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the implementation of the value pattern matcher. The
/// value pattern matcher is a comparator extension of the LLVM
/// FunctionComparator tailored to value pattern comparison.
///
//===----------------------------------------------------------------------===//

#include "ValuePatternComparator.h"
#include "Config.h"
#include "Utils.h"

/// Compare the given module value with the pattern value.
int ValuePatternComparator::compare() {
    auto PatternValue =
            IsLeftSide ? ParentPattern->ValueL : ParentPattern->ValueR;

    // Compare load from a global variable by name.
    auto Load = dyn_cast<LoadInst>(ComparedValue);
    if (Load && isa<GlobalVariable>(PatternValue)) {
        return !namesMatch(Load->getOperand(0)->getName(),
                           PatternValue->getName(),
                           IsLeftSide);
    }

    // Compare all other values using the default implementation.
    return cmpValues(ComparedValue, PatternValue);
}

/// Set the module value that should be compared.
void ValuePatternComparator::setComparedValue(const Value *ModVal) {
    ComparedValue = ModVal;
}

/// Compare a module value with a pattern value without using serial numbers.
/// Note: Parts of this function have been adapted from FunctionComparator.
/// Therefore, LLVM licensing also applies here. See the LICENSE information
/// in the appropriate llvm-lib subdirectory for more details.
int ValuePatternComparator::cmpValues(const Value *L, const Value *R) const {
    // Catch self-reference case.
    if (L == FnL) {
        if (R == FnR)
            return 0;
        return -1;
    }
    if (R == FnR) {
        if (L == FnL)
            return 0;
        return 1;
    }

    const Constant *ConstL = dyn_cast<Constant>(L);
    const Constant *ConstR = dyn_cast<Constant>(R);
    if (ConstL && ConstR) {
        if (L == R)
            return 0;
        return cmpConstants(ConstL, ConstR);
    }

    if (ConstL)
        return 1;
    if (ConstR)
        return -1;

    const InlineAsm *InlineAsmL = dyn_cast<InlineAsm>(L);
    const InlineAsm *InlineAsmR = dyn_cast<InlineAsm>(R);

    if (InlineAsmL && InlineAsmR)
        return cmpInlineAsm(InlineAsmL, InlineAsmR);
    if (InlineAsmL)
        return 1;
    if (InlineAsmR)
        return -1;

    // Because only a single pair of values gets compared, general values cannot
    // be considered as equal.
    return 1;
}
