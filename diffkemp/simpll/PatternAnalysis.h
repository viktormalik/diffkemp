#ifndef DIFFKEMP_SIMPLL_PATTERN_ANALYSIS_H
#define DIFFKEMP_SIMPLL_PATTERN_ANALYSIS_H

#include <string>
#include <iostream>
#include <memory>

#include "PatternGenerator.h"

/// Generates pattern for function from two separate LLVM modules.
void generatePattern(std::string function,
                     std::string first,
                     std::string second);

#endif // DIFFKEMP_SIMPLL_PATTERN_ANALYSIS_H
