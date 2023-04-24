#ifndef DIFFKEMP_SIMPLL_PATTERNGENERATOR_H
#define DIFFKEMP_SIMPLL_PATTERNGENERATOR_H

#include "Config.h"
#include "DebugInfo.h"
#include "DifferentialFunctionComparator.h"
#include "ModuleAnalysis.h"
#include "Output.h"
#include "Result.h"
#include "passes/StructureSizeAnalysis.h"

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
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/Cloning.h>

using namespace llvm;

#include <iostream>
#include <map>
#include <memory>
#include <unordered_map>
#include <utility>

class PatternRepresentation {
    friend std::ostream &operator<<(std::ostream &os,
                                    PatternRepresentation &pg);
    friend class PatternGenerator;

  private:
    /// This has to be predeclared, because public Module needs it for its
    /// construction. But on the other hand, there is no reason to make the
    /// context public.
    LLVMContext context;

  public:
    PatternRepresentation(std::string name,
                          std::string funFirstName,
                          std::string funSecondName)
            : context(), mod(new Module(name, context)),
              funNames(funFirstName, funSecondName),
              functions(nullptr, nullptr),
              MDMap({{PATTERN_START,
                      MDNode::get(context,
                                  MDString::get(context, "pattern-start"))},
                     {PATTERN_END,
                      MDNode::get(context,
                                  MDString::get(context, "pattern-end"))}}){};
    void refreshFunctions();

    std::unique_ptr<Module> mod;
    std::pair<std::string, std::string> funNames;
    std::pair<Function *, Function *> functions;

  private:
    enum Metadata {
        PATTERN_START,
        PATTERN_END,
        DISSABLE_NAME_COMPARISON,
        GROUP_START,
        GROUP_END,
    };
    std::unordered_map<PatternRepresentation::Metadata, MDNode *> MDMap;
    bool MDSet{false};
};

class PatternGenerator {
  public:
    /// By default there is no pattern, hence it should be initialized to
    /// a nullptr.
    PatternGenerator() : firstCtx(), secondCtx(){};

    virtual ~PatternGenerator() = default;

    void addFileForInference(std::string patternName,
                             std::string funcName,
                             std::string fileName);

    bool addFunctionToPattern(Module *mod,
                              Function *PaternFun,
                              Function *CandidateFun,
                              std::string patterName);

    [[nodiscard]] bool addFunctionPairToPattern(
            std::pair<std::string, std::string> moduleFiles,
            std::pair<std::string, std::string> funNames,
            std::string patternName);

    PatternRepresentation *operator[](std::string patternName) {
        return patterns[patternName].get();
    }

  private:
    class MinimalModuleAnalysis {
      public:
        DifferentialFunctionComparator *operator->() const {
            return diffComp.get();
        }
        inline void addFunPair(std::pair<Function *, Function *> fPair) {
            modComp->ComparedFuns.emplace(fPair,
                                          Result(fPair.first, fPair.second));
        }
        MinimalModuleAnalysis(Config &conf);

      private:
        std::unique_ptr<DebugInfo> dbgInfo;
        std::unique_ptr<ModuleComparator> modComp;
        std::unique_ptr<DifferentialFunctionComparator> diffComp;

        /// Objects needed for creation of minimal ModuleAnalysis, they are
        /// initialized inline, because we want them to have default values.
        std::set<const Function *> CalledFirst{};
        std::set<const Function *> CalledSecond{};
        StructureSizeAnalysis::Result StructSizeMapL{};
        StructureSizeAnalysis::Result StructSizeMapR{};
        StructureDebugInfoAnalysis::Result StructDIMapL{};
        StructureDebugInfoAnalysis::Result StructDIMapR{};
    };
    /// Config is needed for invocation of DifferentialFunctionComparator class
    /// But it is going to be initialized everytime new function in the pattern
    /// config is found.
    // const Config &config;
    /// This has to be mutable in order to pass it to llvm::parseIRFile
    mutable LLVMContext firstCtx;
    mutable LLVMContext secondCtx;
    mutable std::map<std::string, std::unique_ptr<PatternRepresentation>>
            patterns;

    // DifferentialFunctionComparator diffFunComp;
    void attachMetadata(Instruction *instr, std::string metadataStr);
    void determinePatternRange(PatternRepresentation *PatRep);

    Function *cloneFunctionWithExtraArgument(Module *dstMod,
                                             Function *src,
                                             Type &newType);
    Function *cloneFunction(std::string prefix, Module *dstMod, Function *src);

    bool isValueGlobal(Value &val, Module &mod);
};

struct PatternCandidate {
    std::string function{""};
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
