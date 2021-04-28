//===------------ PatternSet.h - Unordered set of code patterns -----------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Petr Silling, psilling@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of the unordered LLVM code pattern set.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_PATTERNSET_H
#define DIFFKEMP_SIMPLL_PATTERNSET_H

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/IR/Module.h>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace llvm;

/// Input instructions and arguments.
typedef SmallPtrSet<const Value *, 16> InputSet;

/// Mapping between input values from different pattern sides.
typedef DenseMap<const Value *, const Value *> InputMap;

/// Instructions pointer set.
typedef SmallPtrSet<const Instruction *, 32> InstructionSet;

/// Instruction to instruction mapping.
typedef DenseMap<const Instruction *, const Instruction *> InstructionMap;

// Forward declaration of the DifferentialFunctionComparator.
class DifferentialFunctionComparator;

/// Available types of difference patterns.
enum class PatternType { INST, VALUE };

/// Representation of difference pattern metadata configuration.
struct PatternMetadata {
    /// Marker for the first differing instruction pair.
    bool PatternStart = false;
    /// Marker for the last differing instruction pair.
    bool PatternEnd = false;
    /// Prevents skipping of module instructions when no match is found.
    bool GroupStart = false;
    /// End of the previous instruction group.
    bool GroupEnd = false;
    /// Disables the default name-based comparison of globals and structures.
    bool DisableNameComparison = false;
    /// Does not register the instruction as an input.
    bool NotAnInput = false;
};

/// Representation of the whole difference pattern configuration.
struct PatternConfiguration {
    /// Logging option for parse failures.
    std::string OnParseFailure;
    /// Vector of paths to pattern files.
    std::vector<std::string> PatternFiles;
};

/// Base pattern representation.
struct Pattern {
    /// Name of the pattern.
    const std::string Name;
    /// Function corresponding to the left part of the pattern.
    const Function *PatternL;
    /// Function corresponding to the right part of the pattern.
    const Function *PatternR;

    Pattern(const std::string &Name,
            const Function *PatternL,
            const Function *PatternR)
            : Name(Name), PatternL(PatternL), PatternR(PatternR) {}

    bool operator==(const Pattern &Rhs) const {
        return (Name == Rhs.Name && PatternL == Rhs.PatternL
                && PatternR == Rhs.PatternR);
    }
};

/// Representation of a difference pattern pair based on instruction matching.
struct InstPattern : public Pattern {
    /// Map of all included pattern metadata.
    mutable std::unordered_map<const Value *, PatternMetadata> MetadataMap;
    /// Input instructions and arguments for the left part of the pattern.
    mutable InputSet InputL;
    /// Input instructions and arguments for the right part of the pattern.
    mutable InputSet InputR;
    /// Mapping of input arguments from new to old part of the pattern.
    mutable InputMap ArgumentMapping;
    /// Final instruction mapping associated with the pattern.
    mutable InstructionMap FinalMapping;
    /// Comparison start position for the left part of the pattern.
    const Instruction *StartPositionL = nullptr;
    /// Comparison start position for the right part of the pattern.
    const Instruction *StartPositionR = nullptr;

    InstPattern(const std::string &Name,
                const Function *PatternL,
                const Function *PatternR)
            : Pattern(Name, PatternL, PatternR) {}
};

/// Representation of a pattern describing a difference in a single pair of
/// values.
struct ValuePattern : public Pattern {
    /// Compared value for the left part of the pattern.
    const Value *ValueL = nullptr;
    /// Compared value for the right part of the pattern.
    const Value *ValueR = nullptr;

    ValuePattern(const std::string &Name,
                 const Function *PatternL,
                 const Function *PatternR)
            : Pattern(Name, PatternL, PatternR) {}
};

// Define a hash function for general difference patterns.
namespace std {
template <> struct hash<Pattern> {
    std::size_t operator()(const Pattern &Pat) const noexcept {
        return std::hash<std::string>()(Pat.Name);
    }
};
} // namespace std

// Define a hash function for instruction difference patterns.
namespace std {
template <> struct hash<InstPattern> {
    std::size_t operator()(const InstPattern &Pat) const noexcept {
        return std::hash<Pattern>()(Pat);
    }
};
} // namespace std

// Define a hash function for value difference patterns.
namespace std {
template <> struct hash<ValuePattern> {
    std::size_t operator()(const ValuePattern &Pat) const noexcept {
        return std::hash<Pattern>()(Pat);
    }
};
} // namespace std

/// Compares difference patterns against functions, possibly eliminating reports
/// of prior semantic differences.
class PatternSet {
  public:
    /// Default DiffKemp prefix for all pattern information.
    static const std::string DefaultPrefix;
    /// Prefix for the left (old) side of difference patterns.
    static const std::string PrefixL;
    /// Prefix for the right (new) side of difference patterns.
    static const std::string PrefixR;
    /// Complete prefix for the left side of difference patterns.
    static const std::string FullPrefixL;
    /// Complete prefix for the right side of difference patterns.
    static const std::string FullPrefixR;
    /// Name for the function defining final instuction mapping.
    static const std::string MappingFunctionName;
    /// Name for pattern metadata nodes.
    static const std::string MetadataName;
    /// Set of loaded instruction difference patterns.
    std::unordered_set<InstPattern> InstPatterns;
    /// Set of loaded value difference patterns.
    std::unordered_set<ValuePattern> ValuePatterns;

    PatternSet(std::string ConfigPath);

    ~PatternSet();

    /// Retrives pattern metadata attached to the given instruction, returning
    /// true for valid pattern metadata nodes.
    bool getPatternMetadata(PatternMetadata &Metadata,
                            const Instruction &Inst) const;

  private:
    /// Basic information about the final instruction mapping present on one
    /// side of a pattern.
    using MappingInfo = std::pair<const Instruction *, int>;

    /// Settings applied to all pattern files.
    StringMap<std::string> GlobalSettings;
    /// Map of loaded pattern modules.
    std::unordered_map<Module *, std::unique_ptr<Module>> PatternModules;
    /// Map of loaded pattern module contexts.
    std::unordered_map<Module *, std::unique_ptr<LLVMContext>> PatternContexts;

    /// Load the given configuration file.
    void loadConfig(std::string &ConfigPath);

    /// Add a new difference pattern.
    void addPattern(std::string &Path);

    /// Finds the pattern type associated with the given pattern functions.
    PatternType getPatternType(const Function *FnL, const Function *FnR);

    /// Initializes a pattern, loading all metadata, start positions, and the
    /// final instruction mapping.
    bool initializeInstPattern(InstPattern &Pat);

    /// Initializes a single side of a pattern, loading all metadata, start
    /// positions, and retrevies instruction mapping information.
    void initializeInstPatternSide(InstPattern &Pat,
                                   MappingInfo &MapInfo,
                                   bool IsLeftSide);

    /// Initializes a value pattern loading value differences from both sides of
    /// the pattern.
    bool initializeValuePattern(ValuePattern &Pat);

    /// Parses a single pattern metadata operand, including all dependent
    /// operands.
    int parseMetadataOperand(PatternMetadata &PatternMetadata,
                             const MDNode *InstMetadata,
                             const int Index) const;
};

#endif // DIFFKEMP_SIMPLL_PATTERNSET_H
