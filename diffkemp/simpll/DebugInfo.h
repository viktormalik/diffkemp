//===------------- DebugInfo.h - Processing debug information -------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains classes and LLVM passes for processing and analysing
/// debugging information in the compared modules.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_DEBUGINFO_H
#define DIFFKEMP_SIMPLL_DEBUGINFO_H

#include "Config.h"
#include "Utils.h"
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/TypeFinder.h>
#include <llvm/IR/ValueMap.h>
#include <set>

using namespace llvm;

/// Analysing debug info of the module and extracting useful information.
/// The following information is extracted:
/// 1. Alignment of GEP indices.
///    In case the corresponding structure has a different set of fields between
///    the analysed modules, it might happen that corresponding fields are at
///    different indices. This analysis matches fields with same name and saves
///    the index offset into the metadata of a GEP instruction.
class DebugInfo {
  public:
    using StructFieldNamesMap =
            std::map<std::pair<StructType *, uint64_t>, StringRef>;
    DebugInfo(Module &modFirst,
              Module &modSecond,
              Function *funFirst,
              Function *funSecond,
              std::set<const Function *> &CalledFirst,
              std::set<const Function *> &CalledSecond,
              const Config &config)
            : FunFirst(funFirst), FunSecond(funSecond), ModFirst(modFirst),
              ModSecond(modSecond), CalledFirst(CalledFirst),
              CalledSecond(CalledSecond) {
        DebugInfoFirst.processModule(ModFirst);
        DebugInfoSecond.processModule(ModSecond);
        // Use debug info to gather useful information
        if (config.PatternStructAlignment)
            calculateGEPIndexAlignments();
        if (config.PatternNumericalMacros)
            calculateMacroAlignments();
        collectLocalVariables(CalledFirst, LocalVariableMapL);
        collectLocalVariables(CalledSecond, LocalVariableMapR);
        // Remove calls to debug info intrinsics from the functions - it may
        // cause some non-equalities in FunctionComparator.
        removeFunctionsDebugInfo(modFirst);
        removeFunctionsDebugInfo(modSecond);
    };

    /// Maps structure type and index to struct member names
    StructFieldNamesMap StructFieldNames;

    /// Maps constants potentially generated from a macro from the first module
    /// to corresponding values in the second module.
    std::map<const Constant *, std::string> MacroConstantMap;

    /// Maps local variable names to their values.
    std::unordered_map<std::string, const Value *> LocalVariableMapL;
    std::unordered_map<std::string, const Value *> LocalVariableMapR;

  private:
    Function *FunFirst;
    Function *FunSecond;
    Module &ModFirst;
    Module &ModSecond;
    DebugInfoFinder DebugInfoFirst;
    DebugInfoFinder DebugInfoSecond;
    std::set<const Function *> &CalledFirst, &CalledSecond;

    /// Mapping struct types to index maps that contain pairs of corresponding
    /// indices.
    std::map<StructType *, std::map<uint64_t, uint64_t>> IndexMaps;

    /// Mapping macro names to the set of constants in the first module having
    /// the macro value.
    std::map<std::string, std::set<const Constant *>> MacroUsageMap;

    /// Calculate alignments of the corresponding indices for one GEP
    /// instruction.
    void extractAlignmentFromInstructions(GetElementPtrInst *GEPL,
                                          GetElementPtrInst *GEPR);

    /// Calculate alignments of the corresponding indices of GEP instructions.
    void calculateGEPIndexAlignments();

    /// Calculate alignments of the corresponding macros
    void calculateMacroAlignments();

    /// Find all macros in the first module having the given value
    void collectMacrosWithValue(const Constant *Val);

    /// Find all local variables and create a map from their names to their
    /// values.
    void collectLocalVariables(
            std::set<const Function *> &Called,
            std::unordered_map<std::string, const Value *> &Map);

    /// Add an alignment of macros if it exists
    /// \param MacroName Name of the macro in the second module
    /// \param MacroValue Value of the macro in the second module
    void addAlignment(const std::string MacroName,
                      const std::string MacroValue);

    /// Get debug info for struct type with given name
    DICompositeType *getStructTypeInfo(const StringRef name,
                                       const Program prog) const;

    /// Get index of the struct member having the given name
    /// \param type Struct type debug info
    /// \param name Searched name
    static int getTypeMemberIndex(const DICompositeType &type,
                                  const StringRef name);

    /// Get name of the struct member on the given index
    /// \param type Struct type debug info.
    /// \param index Index of struct field.
    /// \return Name of the field.
    static StringRef getElementNameAtIndex(const DICompositeType &type,
                                           uint64_t index);

    /// Add metadata with the new offset to the GEP instruction
    /// \param GEP GEP instruction.
    /// \param index GEP instruction index.
    /// \param alignment New alignment.
    /// \param bitWidth Alignment bit width.
    /// \param c LLVM context.
    static void setNewAlignmentOfIndex(GetElementPtrInst &GEP,
                                       unsigned long index,
                                       uint64_t alignment,
                                       unsigned bitWidth,
                                       LLVMContext &c);

    /// Check if the struct element has the same index as the previous element
    /// (this situation may be caused by the compiler due to struct alignment).
    static bool isSameElemIndex(const DIDerivedType *TypeElem);

    /// Remove calls to debug info intrinsics from all functions in the module.
    void removeFunctionsDebugInfo(Module &Mod);
};

/// A pass to remove all debugging information from a function.
class RemoveDebugInfoPass : public PassInfoMixin<RemoveDebugInfoPass> {
  public:
    PreservedAnalyses run(Function &Fun, FunctionAnalysisManager &fam);
};

/// Check if function is a debug info function.
bool isDebugInfo(const Function &Fun);
/// Check if instruction is a call to debug info function.
bool isDebugInfo(const Instruction &Instr);

/// Get name of a struct type as it is specified in the C source.
std::string getStructTypeName(const StructType *type);

#endif // DIFFKEMP_SIMPLL_DEBUGINFO_H
