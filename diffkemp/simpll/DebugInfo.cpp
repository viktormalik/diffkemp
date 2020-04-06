//===------------ DebugInfo.cpp - Processing debug information ------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains analyses and passes for processing debugging information.
///
//===----------------------------------------------------------------------===//

#include "DebugInfo.h"
#include "Config.h"
#include <llvm/IR/Constants.h>
#include <llvm/Passes/PassBuilder.h>
#include <passes/FunctionAbstractionsGenerator.h>

using namespace llvm;

PreservedAnalyses RemoveDebugInfoPass::run(Function &Fun,
                                           FunctionAnalysisManager &fam) {
    std::vector<Instruction *> toRemove;
    for (auto &BB : Fun) {
        for (auto &Instr : BB) {
            if (isDebugInfo(Instr)) {
                toRemove.push_back(&Instr);
            }
        }
    }
    for (auto Instr : toRemove)
        Instr->eraseFromParent();
    return PreservedAnalyses();
}

bool isDebugInfo(const Instruction &Instr) {
    if (auto CallInstr = dyn_cast<CallInst>(&Instr)) {
        auto fun = CallInstr->getCalledFunction();
        if (fun)
            return isDebugInfo(*fun);
    }
    return false;
}

bool isDebugInfo(const Function &Fun) {
    return Fun.getIntrinsicID() == Intrinsic::dbg_declare
           || Fun.getIntrinsicID() == Intrinsic::dbg_value;
}

/// Get C name of the struct type. This can be extracted from the LLVM struct
/// name by stripping off the 'struct.' prefix and the '.*' suffix.
std::string getStructTypeName(const StructType *type) {
    std::string name = type->getName();
    name.erase(0, std::string("struct.").length());
    if (name.find_last_of(".") != std::string::npos)
        name.erase(name.find_last_of("."));
    return name;
}

DICompositeType *DebugInfo::getStructTypeInfo(const StringRef name,
                                              const Program prog) const {
    auto types = prog == Program::First ? DebugInfoFirst.types()
                                        : DebugInfoSecond.types();

    for (auto Type : types) {
        if (auto StructType = dyn_cast<DICompositeType>(Type)) {
            if (StructType->getName() == name)
                return StructType;
        }
    }
    return nullptr;
}

/// Calculate alignments of the corresponding indices for one GEP
/// instruction.
void DebugInfo::extractAlignmentFromInstructions(GetElementPtrInst *GEP,
                                                 GetElementPtrInst *OtherGEP) {
    if (GEP) {
        std::vector<Value *> indices;
        std::vector<Value *> indicesOther;

        User::op_iterator idx_other;
        if (OtherGEP)
            // If we have the other GEP, iterate over its indices, too
            idx_other = OtherGEP->idx_begin();

        // Iterate all indices
        for (auto idx = GEP->idx_begin(); idx != GEP->idx_end();
             ++idx, indices.push_back(*idx)) {
            auto indexedType = GEP->getIndexedType(GEP->getSourceElementType(),
                                                   ArrayRef<Value *>(indices));

            Type *indexedTypeOther = nullptr;
            if (OtherGEP)
                indexedTypeOther = OtherGEP->getIndexedType(
                        OtherGEP->getSourceElementType(),
                        ArrayRef<Value *>(indicesOther));

            if (!indexedType->isStructTy())
                continue;

            if (indexedTypeOther && !indexedTypeOther->isStructTy()) {
                // The type in the corresponding GEP instruction is different,
                // therefore it cannot be used
                indexedTypeOther = nullptr;
            }

            auto &indexMap = IndexMaps[dyn_cast<StructType>(indexedType)];

            if (auto IndexConstant = dyn_cast<ConstantInt>(*idx)) {
                // Numeric value of the current index
                uint64_t indexFirst = IndexConstant->getZExtValue();

                // Check if the corresponding index already exists
                auto otherIndex = indexMap.find(indexFirst);
                if (otherIndex != indexMap.end()) {
                    if (indexFirst != otherIndex->second) {
                        setNewAlignmentOfIndex(*GEP,
                                               indices.size(),
                                               otherIndex->second,
                                               IndexConstant->getBitWidth(),
                                               ModFirst.getContext());
                    }
                    continue;
                }

                indexMap.emplace(indexFirst, indexFirst);

                // Name of the current type (type being indexed)
                if (!dyn_cast<StructType>(indexedType)->hasName())
                    continue;
                std::string typeName =
                        getStructTypeName(dyn_cast<StructType>(indexedType));

                // Get name of the element at the current index in
                // the first module
                auto TypeDIFirst = getStructTypeInfo(typeName, Program::First);
                if (!TypeDIFirst)
                    continue;

                StringRef elementName =
                        getElementNameAtIndex(*TypeDIFirst, indexFirst);

                // Find index of the element with the same name in
                // the second module
                auto TypeDISecond =
                        getStructTypeInfo(typeName, Program::Second);
                if (!TypeDISecond)
                    continue;

                int indexSecond =
                        getTypeMemberIndex(*TypeDISecond, elementName);

                if (indexSecond > 0) {
                    if (indexFirst != indexSecond) {
                        // If indices do not match, align the first one to
                        // be the same as the second one
                        indexMap.at(indexFirst) = (unsigned)indexSecond;
                        setNewAlignmentOfIndex(*GEP,
                                               indices.size(),
                                               (unsigned)indexSecond,
                                               IndexConstant->getBitWidth(),
                                               ModFirst.getContext());
                        DEBUG_WITH_TYPE(
                                DEBUG_SIMPLL,
                                dbgs() << "Index alignment in:" << *GEP << "\n"
                                       << "                     " << indexFirst
                                       << " -> " << indexSecond << "\n");
                    }
                    // Insert the names of the indices into StructFieldNames.
                    StructFieldNames.insert(
                            {{dyn_cast<StructType>(indexedType), indexFirst},
                             elementName});

                    if (indexedTypeOther)
                        StructFieldNames.insert(
                                {{dyn_cast<StructType>(indexedTypeOther),
                                  indexSecond},
                                 elementName});
                    else
                        StructFieldNames.insert(
                                {{ModSecond.getTypeByName(
                                          indexedType->getStructName()),
                                  indexSecond},
                                 elementName});
                }
            }

            if (OtherGEP && idx_other != OtherGEP->idx_end()) {
                indicesOther.push_back(*idx_other);
                ++idx_other;
            }
        }
    }
}

/// For each GEP instruction, check if the accessed struct members of
/// the same name have the same alignment in both modules. If not, add
/// metadata to the instruction of one module containing new value of
/// the alignment
void DebugInfo::calculateGEPIndexAlignments() {
    // Check if any debug info was collected
    if (DebugInfoFirst.type_count() == 0 || DebugInfoSecond.type_count() == 0)
        return;

    for (auto &Fun : ModFirst) {
        auto OtherFun = ModSecond.getFunction(Fun.getName());
        if (!OtherFun)
            continue;

        if (CalledFirst.find(&Fun) == CalledFirst.end())
            continue;

        auto OtherBB = OtherFun->begin();
        for (auto &BB : Fun) {
            if (OtherBB != OtherFun->end()) {
                // The other basic block is available; iterate over its
                // instructions and try to find the corresponding GEP
                // instructions
                auto OtherInstr = OtherBB->begin();

                for (auto &Instr : BB) {
                    auto GEP = dyn_cast<GetElementPtrInst>(&Instr);

                    if (OtherInstr != OtherBB->end()) {
                        // The other instruction is available; try to get the
                        // corresponding GEP instruction
                        auto OtherGEP =
                                dyn_cast<GetElementPtrInst>(&*OtherInstr);

                        extractAlignmentFromInstructions(GEP, OtherGEP);

                        ++OtherInstr;
                    } else
                        // The end of the basic block has been reached in the
                        // other module, so the other GEP parameter can't be
                        // used
                        extractAlignmentFromInstructions(GEP, nullptr);
                }

                ++OtherBB;
            } else {
                // The other basic block is not available - iterate the
                // standard way without instructions in the other module
                for (auto &Instr : BB) {
                    auto GEP = dyn_cast<GetElementPtrInst>(&Instr);
                    extractAlignmentFromInstructions(GEP, nullptr);
                }
            }
        }
    }
}

/// Check if a struct element is at the same offset as the previous element. Th
/// is can be determined by checking if the value of DIFlagBitField is different
/// from the element offset.
bool DebugInfo::isSameElemIndex(const DIDerivedType *TypeElem) {
    if (TypeElem->getFlag("DIFlagBitField") && TypeElem->getExtraData()) {
        if (auto ExtraDataValue =
                    dyn_cast<ConstantAsMetadata>(TypeElem->getExtraData())) {
            if (auto ExtraDataConst =
                        dyn_cast<ConstantInt>(ExtraDataValue->getValue())) {
                if (ExtraDataConst->getZExtValue()
                    != TypeElem->getOffsetInBits())
                    return true;
            }
        }
    }
    return false;
}

/// Get index of the struct member having the given name.
/// Handles struct alignment:
///  - when multiple fields have the same offset
///  - when a field has explicit alignment and a padding is inserted
int DebugInfo::getTypeMemberIndex(const DICompositeType &type,
                                  const StringRef name) {
    unsigned index = 0;
    uint64_t nextOffset = 0;
    for (auto Elem : type.getElements()) {
        if (auto TypeElem = dyn_cast<DIDerivedType>(Elem)) {
            // Multiple fields with same alignments (bitfields)
            if (isSameElemIndex(TypeElem))
                index--;

            // Field with explicit alignment (padding is inserted)
            if (auto align = TypeElem->getAlignInBits()) {
                if (nextOffset % align != 0)
                    index++;
            }

            if (TypeElem->getName() == name)
                return index;
            nextOffset =
                    TypeElem->getOffsetInBits() + TypeElem->getSizeInBits();
            if (TypeElem->getSizeInBits() > 0)
                index++;
        } else {
            index++;
        }
    }
    return -1;
}

/// Get name of the struct member on the given index
/// Handles struct alignment when multiple fields have the same offset.
StringRef DebugInfo::getElementNameAtIndex(const DICompositeType &type,
                                           uint64_t index) {

    unsigned currentIndex = 0;
    for (auto Elem : type.getElements()) {
        if (auto TypeElem = dyn_cast<DIDerivedType>(Elem)) {
            if (TypeElem->getOffsetInBits() > 0 && !isSameElemIndex(TypeElem))
                currentIndex++;

            if (currentIndex == index)
                return TypeElem->getName();
        }
    }
    return "";
}

/// Add metadata with the new offset to the GEP instruction
void DebugInfo::setNewAlignmentOfIndex(GetElementPtrInst &GEP,
                                       unsigned long index,
                                       uint64_t alignment,
                                       unsigned bitWidth,
                                       LLVMContext &c) {
    MDNode *MD = MDNode::get(c,
                             ConstantAsMetadata::get(ConstantInt::get(
                                     c, APInt(bitWidth, alignment, false))));
    GEP.setMetadata("idx_align_" + std::to_string(index), MD);
}

/// Collects mappings of values for constants that are potentially generated
/// from macros. It finds all used constants that correpond to some macro value
/// in the first module and then finds values or given macros in the second
/// module. If the values differ, a mapping is created for the constant.
void DebugInfo::calculateMacroAlignments() {
    // Check if any debug info was collected
    if (DebugInfoFirst.type_count() == 0 || DebugInfoSecond.type_count() == 0)
        return;

    // Find all constants used in the first module whose values correspond to
    // some macro value.
    for (auto &Fun : ModFirst) {
        if (!ModSecond.getFunction(Fun.getName()))
            continue;
        if (CalledFirst.find(&Fun) == CalledFirst.end())
            continue;

        std::set<Constant *> VisitedConsts;
        for (const auto &BB : Fun) {
            for (const auto &Inst : BB) {
                for (const auto &Op : Inst.operands()) {
                    if (auto Const = dyn_cast<Constant>(&Op)) {
                        if (VisitedConsts.find(Const) == VisitedConsts.end())
                            VisitedConsts.insert(Const);
                        if (!isSimpllAbstraction(&Fun))
                            collectMacrosWithValue(
                                    Const, Fun.getSubprogram()->getUnit());
                    }
                }
            }
        }
    }

    // In second module, search for macros collected in the previous step and
    // if they have a different value between the modules, create a mapping.
    for (auto *CompileUnit : DebugInfoSecond.compile_units()) {
        for (auto *MacroNode : CompileUnit->getMacros()) {
            if (auto *Macro = dyn_cast<DIMacro>(MacroNode)) {
                addAlignment(Macro->getName(), Macro->getValue());
            }
        }
        for (auto *Enum : CompileUnit->getEnumTypes()) {
            for (auto *EnumField : Enum->getElements()) {
                if (auto *Enumerator = dyn_cast<DIEnumerator>(EnumField)) {
                    addAlignment(Enumerator->getName(),
                                 std::to_string(Enumerator->getValue()));
                }
            }
        }
    }
}

/// Find all macros and enum values that define a value corresponding to the
/// value of the given constant and add them to the MacroUsageMap.
void DebugInfo::collectMacrosWithValue(const Constant *Val,
                                       const DICompileUnit *CompileUnit) {
    std::string valStr = valueAsString(Val);
    if (valStr.empty())
        return;

    for (auto *MacroNode : CompileUnit->getMacros()) {
        if (auto *Macro = dyn_cast<DIMacro>(MacroNode)) {
            if (Macro->getValue() == valStr) {
                MacroUsageMap[Macro->getName()].insert(Val);
            }
        }
    }
    for (auto *Enum : CompileUnit->getEnumTypes()) {
        for (auto *EnumField : Enum->getElements()) {
            if (auto *Enumerator = dyn_cast<DIEnumerator>(EnumField)) {
                if (std::to_string(Enumerator->getValue()) == valStr) {
                    MacroUsageMap[Enumerator->getName()].insert(Val);
                }
            }
        }
    }
}

/// Find all local variables and create a map from their names to their
/// values.
void DebugInfo::collectLocalVariables(
        std::set<const Function *> &Called,
        std::unordered_map<std::string, const Value *> &Map) {
    for (auto Fun : Called) {
        for (auto &BB : *Fun) {
            for (auto &Inst : BB) {
                if (!isa<CallInst>(Inst))
                    continue;
                auto CInst = dyn_cast<CallInst>(&Inst);
                if (!getCalledFunction(CInst->getCalledValue())
                             ->getName()
                             .startswith("llvm.dbg"))
                    continue;

                // Get the value name and the value itself.
                auto WrappedVal = CInst->getOperand(0);
                auto ValMD =
                        dyn_cast<MetadataAsValue>(WrappedVal)->getMetadata();
                auto Val = dyn_cast<ValueAsMetadata>(ValMD);
                if (!Val)
                    continue;
#if LLVM_VERSION_MAJOR < 6
                MetadataAsValue *DIVal;
                if (getCalledFunction(CInst->getCalledValue())->getName()
                    == "llvm.dbg.declare")
                    DIVal = dyn_cast<MetadataAsValue>(CInst->getOperand(1));
                else
                    DIVal = dyn_cast<MetadataAsValue>(CInst->getOperand(2));
#else
                auto DIVal = dyn_cast<MetadataAsValue>(CInst->getOperand(1));
#endif
                auto DI = dyn_cast<DILocalVariable>(DIVal->getMetadata());
                auto Name = Fun->getName().str() + "::" + DI->getName().str();

                Map[Name] = Val->getValue();
            }
        }
    }
}

/// Add alignment for the given macro name and value from the second module.
/// Checks if a macro with the given name was used in the first module (by
/// querying the MacroUsageMap). If yes, and the macro value is different in
/// the second module, creates mapping between constants generated from the
/// macro in the first module and the new value from the second module.
void DebugInfo::addAlignment(std::string MacroName, std::string MacroValue) {
    auto MacroUsage = MacroUsageMap.find(MacroName);
    if (MacroUsage != MacroUsageMap.end()
        && valueAsString(*MacroUsage->second.begin()) != MacroValue) {
        for (auto *Constant : MacroUsage->second) {
            MacroConstantMap.emplace(Constant, MacroValue);
        }
    }
}

/// Remove calls to debug info intrinsics from all functions in the module.
/// We do not use LLVM's stripDebugInfo functions here since they remove other
/// information that we need later (particularly file names).
void DebugInfo::removeFunctionsDebugInfo(Module &Mod) {
    // Function passes
    PassBuilder pb;
    FunctionPassManager fpm(false);
    FunctionAnalysisManager fam(false);
    pb.registerFunctionAnalyses(fam);
    fpm.addPass(RemoveDebugInfoPass{});
    for (auto &F : Mod)
        fpm.run(F, fam);
}
