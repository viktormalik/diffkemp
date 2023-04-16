#ifndef DIFFKEMP_SIMPLL_PATTERN_ANALYSIS_H
#define DIFFKEMP_SIMPLL_PATTERN_ANALYSIS_H

#include <iostream>
#include <memory>
#include <string>

#include "Config.h"
#include "Output.h"
#include "PatternGenerator.h"

static auto gPatternGen = std::make_unique<PatternGenerator>();
// TODO: add some kind of result

/// @brief Generates pattern for function from LLVM module.
/// @param pattern: name of a pattern, for which we are inferencing.
/// @param function: name of a function, frow which we are going to try to
/// infere a pattern
/// @param fileName: file, that is an LLVM module, from which the function is
/// going to be extracted.
void generatePattern(std::string pattern,
                     std::string function,
                     std::string fileName);

void generatePatternV2(std::string pattern,
                       std::string first,
                       std::string second,
                       std::string f1,
                       std::string f2);

void readPatternConfigImpl(std::string configPath);

void reportPatternImpl();

#endif // DIFFKEMP_SIMPLL_PATTERN_ANALYSIS_H
