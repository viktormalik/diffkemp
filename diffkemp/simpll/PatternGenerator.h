#ifndef DIFFKEMP_SIMPLL_PATTERNGENERATOR_H
#define DIFFKEMP_SIMPLL_PATTERNGENERATOR_H

#include "Config.h"
#include "ModuleAnalysis.h"
#include "Result.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/MemorySSAUpdater.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/YAMLTraits.h>
#include <llvm/Transforms/Utils/Cloning.h>

using namespace llvm;

#include <iostream>
#include <memory>
#include <unordered_map>
#include <utility>

// class Pattern {};

class PatternGenerator {
  public:
    /// By default there is no pattern, hence it should be initialized to
    /// a nullptr.
    PatternGenerator() : firstCtx(), secondCtx(), isFreshRun(true){};

    virtual ~PatternGenerator() = default;

    void addFileForInference(std::string patternName,
                             std::string funcName,
                             std::string fileName);

    [[nodiscard]] bool addFunctionToPattern(
            std::pair<std::string, std::string> moduleFiles,
            std::pair<std::string, std::string> funNames,
            std::string patternName);

    friend std::ostream &operator<<(std::ostream &os, PatternGenerator &pg);

  private:
    /// Config is needed for invocation of DifferentialFunctionComparator class
    /// But it is going to be initialized everytime new function in the pattern
    /// config is found.
    // const Config &config;
    /// This has to be mutable in order to pass it to llvm::parseIRFile
    mutable LLVMContext firstCtx;
    mutable LLVMContext secondCtx;
    mutable bool isFreshRun;
    mutable std::unique_ptr<llvm::Module> pattern;
    mutable std::unordered_map<std::string, std::unique_ptr<llvm::Module>>
            patterns;

    void cloneFunction(llvm::Function &, llvm::Function &);
};

struct PatternCandidate {
    std::string function{""};
    std::string alias{""};
    std::string oldSnapshotPath;
    std::string newSnapshotPath;
};

struct PatternInfo {
    std::string name;
    std::vector<PatternCandidate> candidates;
};

namespace llvm {
namespace yaml {
template <> struct MappingTraits<PatternCandidate> {
    static void mapping(IO &io, PatternCandidate &candidate) {
        io.mapRequired("name", candidate.function);
        io.mapRequired("old_snapshot_path", candidate.oldSnapshotPath);
        io.mapRequired("new_snapshot_path", candidate.newSnapshotPath);
    }
};
} // namespace yaml
} // namespace llvm
LLVM_YAML_IS_SEQUENCE_VECTOR(PatternCandidate);

namespace llvm {
namespace yaml {
template <> struct MappingTraits<PatternInfo> {
    static void mapping(IO &io, PatternInfo &info) {
        io.mapRequired("name", info.name);
        io.mapRequired("candidates", info.candidates);
    }
};
} // namespace yaml
} // namespace llvm
LLVM_YAML_IS_SEQUENCE_VECTOR(PatternInfo);

#endif // DIFFKEMP_SIMPLL_PATTERNGENERATOR_H
