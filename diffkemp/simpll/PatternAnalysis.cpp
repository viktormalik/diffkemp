#include "PatternAnalysis.h"

void readPatternConfig(std::string configPath) {
    std::vector<PatternInfo> patterns;
    auto buf = llvm::MemoryBuffer::getFile(configPath);
    llvm::yaml::Input yin(**buf);
    yin >> patterns;

    for (const auto &pattern : patterns) {
        bool isGenerationSuccess = true;
        for (const auto &c : pattern.candidates) {
            if (!gPatternGen->addFunctionPairToPattern(
                        std::make_pair(c.oldSnapshotPath, c.newSnapshotPath),
                        std::make_pair(c.function, c.function),
                        pattern.name)) {
                isGenerationSuccess = false;
                break;
            }
        }
        if (isGenerationSuccess) {
            std::cout << Color::makeGreen("Successfule generation of pattern: ")
                      << pattern.name << std::endl;
            std::cout << *(*gPatternGen)[pattern.name] << std::endl;
        } else {
            std::cout << Color::makeRed("Generation of pattern ")
                      << Color::makeRed(" failed: ") << "pattern '"
                      << pattern.name << "' could not be generated"
                      << std::endl;
        }
    }
}
