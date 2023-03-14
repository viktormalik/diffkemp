#include "PatternAnalysis.h"

void generatePattern([[maybe_unused]] std::string function,
                     std::string first,
                     std::string second) {
    auto pg = std::make_unique<PatternGenerator>() ;
    pg->addFileForInference(first);
    pg->addFileForInference(second);
}
