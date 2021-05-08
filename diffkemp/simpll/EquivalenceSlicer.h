//===---------- EquivalenceSlicer.h - Slicing equal parts of code ----------==//
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
#ifndef DIFFKEMP_EQUIVALENCESLICER_H
#define DIFFKEMP_EQUIVALENCESLICER_H

#include "Config.h"
#include "ControlFlowGraphUtils.h"
#include "DebugInfo.h"
#include "DifferentialFunctionComparator.h"
#include "Result.h"
#include "ResultsCache.h"
#include "SourceCodeUtils.h"
#include "Utils.h"
#include <llvm/IR/Module.h>
#include <queue>
#include <set>

using namespace llvm;

class EquivalenceSlicer {
    const Config &config;
    ControlFlowSlicer CFL;
    ControlFlowSlicer CFR;

  public:
    EquivalenceSlicer(const Config &config) : config(config) {}
    /// Removes semantically equal parts of compared functions and uses
    /// DifferentialFunctionComparator to get the first found difference
    void slice(DifferentialFunctionComparator *fComp);
};

#endif // DIFFKEMP_EQUIVALENCESLICER_H