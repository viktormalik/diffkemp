#include "PatternAnalysis.h"

std::pair<std::string, std::string> getFunNames(const std::string &str) {
    size_t idx = str.find(',');
    if (idx != std::string::npos) {
        return std::make_pair(std::string(str.begin(), str.begin() + idx),
                              std::string(str.begin() + idx + 1, str.end()));
    }
    return std::make_pair(str, "");
}

void readPatternConfig(std::string configPath) {
    std::vector<PatternInfo> patterns;
    auto buf = llvm::MemoryBuffer::getFile(configPath);
    llvm::yaml::Input yin(**buf);
    yin >> patterns;

    for (const auto &pattern : patterns) {
        bool isGenerationSuccess = true;
        for (const auto &c : pattern.candidates) {
            auto fPair = getFunNames(c.function);
            fPair.second = fPair.second.empty() ? fPair.first : fPair.second;
            if (!gPatternGen->addFunctionPairToPattern(
                        std::make_pair(c.oldSnapshotPath, c.newSnapshotPath),
                        std::make_pair(fPair.first, fPair.second),
                        pattern.name)) {
                isGenerationSuccess = false;
                break;
            }
        }
        if (isGenerationSuccess) {
            std::cout << Color::makeGreen("Successfule generation of pattern: ")
                      << pattern.name << std::endl;
            std::cout << *(*gPatternGen)[pattern.name] << std::endl;
            auto pat = (*gPatternGen)[pattern.name];
            int i = 0;
            for (auto varIter = pat->variants.begin();
                 varIter != pat->variants.end();
                 ++varIter) {
                auto varMod = pat->generateVariant({*varIter, *(++varIter)},
                                                   "-var-" + std::to_string(i));
                gPatternGen->determinePatternRange(pat, *varMod);
                if (!varMod->empty()) {
                    llvm::outs() << Color::makeYellow(
                            "Generated Variant no. " + std::to_string(i) + ": ")
                                 << varMod->getName().str() << "\n";
                    varMod->print(llvm::outs(), nullptr);
                    ++i;
                }
            }
        } else {
            std::cout << Color::makeRed("Generation of pattern ")
                      << Color::makeRed(" failed: ") << "pattern '"
                      << pattern.name << "' could not be generated"
                      << std::endl;
        }
    }
}
