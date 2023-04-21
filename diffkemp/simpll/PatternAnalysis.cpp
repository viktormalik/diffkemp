#include "PatternAnalysis.h"

void readPatternConfig(std::string configPath) {
    std::vector<PatternInfo> patterns;
    auto buf = llvm::MemoryBuffer::getFile(configPath);
    llvm::yaml::Input yin(**buf);
    yin >> patterns;

    for (const auto &pattern : patterns) {
        for (const auto &c : pattern.candidates) {
            if (!gPatternGen->addFunctionPairToPattern(
                        std::make_pair(c.oldSnapshotPath, c.newSnapshotPath),
                        std::make_pair(c.function, c.function),
                        pattern.name)) {
            }
        }
        std::cout << *(*gPatternGen)[pattern.name] << std::endl;
    }
}
