//===------------------- Output.h - Reporting results ---------------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains declaration of a function for reporting results of the
/// simplification.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_OUTPUT_H
#define DIFFKEMP_SIMPLL_OUTPUT_H

#include "Config.h"
#include "ModuleAnalysis.h"
#include "ModuleComparator.h"
#include "Utils.h"

/// Report the overall result in YAML format to stdout.
/// \param Result The overall result of the comparison
void reportOutput(OverallResult &Result);

/// Report the overall result in YAML format to a string.
std::string reportOutputToString(OverallResult &result);

#endif // DIFFKEMP_SIMPLL_OUTPUT_H
