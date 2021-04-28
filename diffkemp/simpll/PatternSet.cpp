//===----------- PatternSet.cpp - Unordered set of code patterns ----------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Petr Silling, psilling@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the implementation of the unordered LLVM code pattern
/// set. Pattern sets are generated from the given pattern configuration file
/// and hold all valid patterns that have been referenced there. Patterns may
/// be instruction-based, or value-based. Instruction-based patterns are
/// represented by multiple LLVM IR instructions, while value-based patterns
/// contain only a single return instruction, which describes a difference in
/// a single value.
///
//===----------------------------------------------------------------------===//

#include "PatternSet.h"
#include "Config.h"
#include "ModuleAnalysis.h"
#include "Utils.h"
#include "library/ModuleLoader.h"
#include <algorithm>
#include <llvm/Support/Debug.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/YAMLTraits.h>

using namespace llvm::yaml;
using namespace llvm::sys::path;

// YAML to PatternConfiguration mapping
namespace llvm {
namespace yaml {
template <> struct MappingTraits<PatternConfiguration> {
    static void mapping(IO &IO, PatternConfiguration &Config) {
        IO.mapOptional("on_parse_failure", Config.OnParseFailure);
        IO.mapOptional("patterns", Config.PatternFiles);
    }
};
} // namespace yaml
} // namespace llvm

/// Default DiffKemp prefix for all pattern information.
const std::string PatternSet::DefaultPrefix = "diffkemp.";
/// Prefix for the left (old) side of difference patterns.
const std::string PatternSet::PrefixL = "old.";
/// Prefix for the right (new) side of difference patterns.
const std::string PatternSet::PrefixR = "new.";
/// Complete prefix for the old side of difference patterns.
const std::string PatternSet::FullPrefixL =
        PatternSet::DefaultPrefix + PatternSet::PrefixL;
/// Complete prefix for the right side of difference patterns.
const std::string PatternSet::FullPrefixR =
        PatternSet::DefaultPrefix + PatternSet::PrefixR;
/// Name for the function defining final instuction mapping.
const std::string PatternSet::MappingFunctionName =
        PatternSet::DefaultPrefix + "mapping";
/// Name for pattern metadata nodes.
const std::string PatternSet::MetadataName =
        PatternSet::DefaultPrefix + "pattern";

/// Create a new pattern set based on the given configuration.
PatternSet::PatternSet(std::string ConfigPath) {
    if (ConfigPath.empty()) {
        return;
    }

    // If a pattern is used as a configuration file, only load the pattern.
    if (extension(ConfigPath) == ".ll") {
        addPattern(ConfigPath);
    } else {
        loadConfig(ConfigPath);
    }
}

/// Destroy the pattern set, clearing all modules and contexts.
PatternSet::~PatternSet() {
    PatternModules.clear();
    PatternContexts.clear();
}

/// Retrives pattern metadata attached to the given instruction, returning
/// true for valid pattern metadata nodes.
bool PatternSet::getPatternMetadata(PatternMetadata &Metadata,
                                    const Instruction &Inst) const {
    auto InstMetadata = Inst.getMetadata(MetadataName);
    if (!InstMetadata) {
        return false;
    }

    int OperandIndex = 0;
    while (OperandIndex < InstMetadata->getNumOperands()) {
        int IndexOffset =
                parseMetadataOperand(Metadata, InstMetadata, OperandIndex);

        // Continue only when the metadata operand gets parsed successfully.
        if (IndexOffset < 0) {
            return false;
        }

        OperandIndex += IndexOffset;
    }

    return true;
}

/// Load the given LLVM IR based difference pattern YAML configuration.
void PatternSet::loadConfig(std::string &ConfigPath) {
    auto ConfigFile = MemoryBuffer::getFile(ConfigPath);
    if (std::error_code EC = ConfigFile.getError()) {
        DEBUG_WITH_TYPE(
                DEBUG_SIMPLL,
                dbgs() << getDebugIndent() << "Failed to open difference "
                       << "pattern configuration " << ConfigPath << ".\n");
        return;
    }

    // Parse the configuration file.
    Input YamlFile(ConfigFile.get()->getBuffer());
    PatternConfiguration Config;
    YamlFile >> Config;

    if (YamlFile.error()) {
        DEBUG_WITH_TYPE(
                DEBUG_SIMPLL,
                dbgs() << getDebugIndent() << "Failed to parse difference "
                       << "pattern configuration " << ConfigPath << ".\n");
        return;
    }

    // Load all pattern files included in the configuration.
    for (auto &PatternFile : Config.PatternFiles) {
        addPattern(PatternFile);
    }
}

/// Add a new LLVM IR difference pattern file.
void PatternSet::addPattern(std::string &Path) {
    Module *PatternModule = loadModule(Path, PatternModules, PatternContexts);
    if (!PatternModule) {
        DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                        dbgs() << getDebugIndent()
                               << "Failed to parse difference pattern "
                               << "module " << Path << ".\n");
        return;
    }

    for (auto &Function : PatternModule->getFunctionList()) {
        // Select only defined functions that start with the left prefix.
        if (!Function.isDeclaration()
            && Function.getName().startswith_lower(FullPrefixL)) {
            StringRef Name = Function.getName().substr(FullPrefixL.size());
            std::string NameR = FullPrefixR;
            NameR += Name;

            // Find the corresponding pattern function with the right prefix.
            auto FunctionR = PatternModule->getFunction(NameR);
            if (FunctionR) {
                DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                dbgs() << getDebugIndent()
                                       << "Loading a new difference pattern "
                                       << Name.str() << " from module " << Path
                                       << ".\n");

                switch (getPatternType(&Function, FunctionR)) {
                case PatternType::INST: {
                    InstPattern NewInstPattern(
                            Name.str(), &Function, FunctionR);
                    if (initializeInstPattern(NewInstPattern)) {
                        InstPatterns.emplace(NewInstPattern);
                    }
                    break;
                }

                case PatternType::VALUE: {
                    ValuePattern NewValuePattern(
                            Name.str(), &Function, FunctionR);
                    if (initializeValuePattern(NewValuePattern)) {
                        ValuePatterns.emplace(NewValuePattern);
                    }
                    break;
                }

                default:
                    DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                    dbgs() << getDebugIndent()
                                           << "An unknown pattern type has "
                                              "been used.\n");
                    break;
                }
            }
        }
    }
}

/// Finds the pattern type associated with the given pattern functions.
PatternType PatternSet::getPatternType(const Function *FnL,
                                       const Function *FnR) {
    // Value patterns should only contain a single return instruction.
    auto EntryBBL = &FnL->getEntryBlock(), EntryBBR = &FnR->getEntryBlock();
    auto TermL = EntryBBL->getTerminator(), TermR = EntryBBR->getTerminator();
    if (TermL->getNumSuccessors() == 0 && TermR->getNumSuccessors() == 0) {
        auto InstLB = EntryBBL->begin(), InstRB = EntryBBR->begin();
        if (TermL == &*InstLB && TermR == &*InstRB) {
            return PatternType::VALUE;
        }
    }

    return PatternType::INST;
}

/// Initializes an instruction pattern, loading all metadata, start positions,
/// and the final instruction mapping. Unless the start position is chosen by
/// metadata, it is set to the first differing pair of pattern instructions.
/// Patterns with conflicting differences in concurrent branches are skipped,
/// returning false.
bool PatternSet::initializeInstPattern(InstPattern &Pat) {
    MappingInfo MappingInfoL, MappingInfoR;

    // Initialize both pattern sides.
    initializeInstPatternSide(Pat, MappingInfoL, true);
    initializeInstPatternSide(Pat, MappingInfoR, false);

    // Map input arguments from the left side to the right side.
    if (Pat.PatternL->arg_size() != Pat.PatternR->arg_size()) {
        DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                        dbgs() << getDebugIndent()
                               << "The number of input arguments does not "
                               << "match in pattern " << Pat.Name << ".\n");
        return false;
    }
    for (auto L = Pat.PatternL->arg_begin(), R = Pat.PatternR->arg_begin();
         L != Pat.PatternL->arg_end();
         ++L, ++R) {
        Pat.ArgumentMapping[&*L] = &*R;
    }

    // Create references for the expected final instruction mapping.
    if (MappingInfoL.second != MappingInfoR.second) {
        DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                        dbgs() << getDebugIndent()
                               << "The number of mapped instructions does "
                               << "not match in pattern " << Pat.Name << ".\n");
        return false;
    }
    if (MappingInfoL.first && MappingInfoR.first) {
        for (int i = 0; i < MappingInfoL.second; ++i) {
            auto MappedInstL =
                    dyn_cast<Instruction>(MappingInfoL.first->getOperand(i));
            auto MappedInstR =
                    dyn_cast<Instruction>(MappingInfoR.first->getOperand(i));
            if (!MappedInstL || !MappedInstR) {
                DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                dbgs() << getDebugIndent()
                                       << "Instruction mapping in pattern "
                                       << Pat.Name << " contains values that "
                                       << "do not reference instructions.\n");
                return false;
            }
            Pat.FinalMapping[MappedInstL] = MappedInstR;
        }
    }

    return true;
}

/// Initializes a single side of a pattern, loading all metadata, start
/// positions, and retrevies instruction mapping information.
void PatternSet::initializeInstPatternSide(InstPattern &Pat,
                                           MappingInfo &MapInfo,
                                           bool IsLeftSide) {
    PatternMetadata Metadata;
    InputSet *PatternInput;
    bool PatternEndFound = false;
    const Function *PatternSide;
    const Instruction **StartPosition;
    const Instruction *MappingInstruction = nullptr;
    if (IsLeftSide) {
        PatternInput = &Pat.InputL;
        PatternSide = Pat.PatternL;
        StartPosition = &Pat.StartPositionL;
    } else {
        PatternInput = &Pat.InputR;
        PatternSide = Pat.PatternR;
        StartPosition = &Pat.StartPositionR;
    }

    // Initialize input from pattern function arguments.
    for (auto &&Arg : PatternSide->args()) {
        PatternInput->insert(&Arg);
    }
    // Analyse instruction data of the selected pattern side.
    for (auto &&BB : PatternSide->getBasicBlockList()) {
        for (auto &Inst : BB) {
            // Load instruction metadata.
            Metadata = {};
            if (getPatternMetadata(Metadata, Inst)) {
                Pat.MetadataMap[&Inst] = Metadata;
                // If present, register start position metadata.
                if (!*StartPosition && Metadata.PatternStart) {
                    *StartPosition = &Inst;
                }
                if (!Metadata.PatternEnd && Metadata.PatternEnd) {
                    PatternEndFound = true;
                }
            }
            // Load input from instructions placed before the first difference
            // metadata. Do not include terminator intructions as these should
            // only be used as separators.
            if (!*StartPosition && !Inst.isTerminator()
                && !Metadata.NotAnInput) {
                PatternInput->insert(&Inst);
            }
            // Load instruction mapping information from the first mapping call
            // or pattern function return.
            auto Call = dyn_cast<CallInst>(&Inst);
            if (!MappingInstruction
                && (isa<ReturnInst>(Inst)
                    || (Call && Call->getCalledFunction()
                        && Call->getCalledFunction()->getName()
                                   == MappingFunctionName))) {
                MappingInstruction = &Inst;
            }
        }
    }

    // When no start metadata is present, use the first instruction.
    if (!*StartPosition) {
        *StartPosition = &*PatternSide->getEntryBlock().begin();
    }

    int MappedOperandsCount = 0;
    if (MappingInstruction) {
        // When end metadata is missing, add it to the mapping instruction.
        if (!PatternEndFound) {
            auto OriginalMetadata = Pat.MetadataMap.find(MappingInstruction);
            if (OriginalMetadata == Pat.MetadataMap.end()) {
                Metadata = {};
                Metadata.PatternEnd = true;
                Pat.MetadataMap[MappingInstruction] = Metadata;
            } else {
                Pat.MetadataMap[MappingInstruction].PatternEnd = true;
            }
        }

        // Get the number of possible instruction mapping operands.
        MappedOperandsCount = MappingInstruction->getNumOperands();
        // Ignore the last operand of calls since it references the called
        // function.
        if (isa<CallInst>(MappingInstruction)) {
            --MappedOperandsCount;
        }
    }
    // Generate mapping information.
    MapInfo = {MappingInstruction, MappedOperandsCount};
}

/// Initializes a value pattern loading value differences from both sides of
/// the pattern.
bool PatternSet::initializeValuePattern(ValuePattern &Pat) {
    // Find the compared return instruction on both sides.
    auto EntryBBL = &Pat.PatternL->getEntryBlock(),
         EntryBBR = &Pat.PatternR->getEntryBlock();
    auto TermL = EntryBBL->getTerminator(), TermR = EntryBBR->getTerminator();

    // Read the compared values.
    Pat.ValueL = TermL->getOperand(0);
    Pat.ValueR = TermR->getOperand(0);

    // Pointers in value patterns should reference global variables.
    if (Pat.ValueL->getType()->isPointerTy()
        && !isa<GlobalVariable>(Pat.ValueL)) {
        return false;
    }
    if (Pat.ValueR->getType()->isPointerTy()
        && !isa<GlobalVariable>(Pat.ValueR)) {
        return false;
    }

    return true;
}

/// Parses a single pattern metadata operand, including all dependent operands.
/// The total metadata operand offset is returned unless the parsing fails.
int PatternSet::parseMetadataOperand(PatternMetadata &PatternMetadata,
                                     const MDNode *InstMetadata,
                                     const int Index) const {
    // Metadata offsets
    const int PatternStartOffset = 1;
    const int PatternEndOffset = 1;
    const int GroupStartOffset = 1;
    const int GroupEndOffset = 1;
    const int DisableNameComparisonOffset = 1;
    const int NotAnInputOffset = 1;

    if (auto Type = dyn_cast<MDString>(InstMetadata->getOperand(Index).get())) {
        auto TypeName = Type->getString();
        if (TypeName == "pattern-start") {
            // pattern-start metadata: string type.
            PatternMetadata.PatternStart = true;
            return PatternStartOffset;
        } else if (TypeName == "pattern-end") {
            // pattern-end metadata: string type.
            PatternMetadata.PatternEnd = true;
            return PatternEndOffset;
        } else if (TypeName == "group-start") {
            // group-start metadata: string type.
            PatternMetadata.GroupStart = true;
            return GroupStartOffset;
        } else if (TypeName == "group-end") {
            // group-end metadata: string type.
            PatternMetadata.GroupEnd = true;
            return GroupEndOffset;
        } else if (TypeName == "disable-name-comparison") {
            // disable-name-comparison metadata: string type.
            PatternMetadata.DisableNameComparison = true;
            return DisableNameComparisonOffset;
        } else if (TypeName == "not-an-input") {
            // not-an-input metadata: string type.
            PatternMetadata.NotAnInput = true;
            return NotAnInputOffset;
        }
    }

    DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                    dbgs() << getDebugIndent()
                           << "Failed to parse pattern metadata "
                           << "from node " << *InstMetadata << ".\n");

    return -1;
}
