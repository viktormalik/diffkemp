#ifndef DIFFKEMP_SIMPLL_PATTERN_ANALYSIS_H
#define DIFFKEMP_SIMPLL_PATTERN_ANALYSIS_H

#include <iostream>
#include <memory>
#include <string>

#include "Config.h"
#include "Output.h"
#include "PatternGenerator.h"

static auto gPatternGen = std::make_unique<PatternGenerator>();
void readPatternConfig(std::string configPath);

#endif // DIFFKEMP_SIMPLL_PATTERN_ANALYSIS_H
