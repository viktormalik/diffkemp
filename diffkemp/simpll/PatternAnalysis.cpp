#include "PatternAnalysis.h"

void generatePattern(std::string pattern,
                     std::string function,
                     std::string fileName) {
    gPatternGen->addFileForInference(pattern, function, fileName);
}

void readPatternConfigImpl(std::string configPath) {
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

void generatePatternV2(std::string pattern,
                       std::string first,
                       std::string second,
                       std::string f1,
                       std::string f2) {
    auto passed = gPatternGen->addFunctionPairToPattern(
            std::make_pair(first, second), std::make_pair(f1, f2), pattern);
    if (!passed) {
        std::cout << "error: failed misserably" << std::endl;
    }
};

// TODO: This should probably return a string, not just simply print it
void reportPatternImpl() {
    std::cout << "This is an evergoing problem" << std::endl;
    // llvm::yaml::Output output(llvm::outs());
    // output << *gPatternGen;
}
