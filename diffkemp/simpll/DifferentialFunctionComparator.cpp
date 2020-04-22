//=== DifferentialFunctionComparator.cpp - Comparing functions for equality ==//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains implementation of specific comparison functions used
/// to compare functions from different modules on equality.
///
//===----------------------------------------------------------------------===//

#include "DifferentialFunctionComparator.h"
#include "Config.h"
#include "SourceCodeUtils.h"
#include "passes/FieldAccessFunctionGenerator.h"
#include "passes/FunctionAbstractionsGenerator.h"
#include <llvm/IR/GetElementPtrTypeIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <set>

// If a operand of a call instruction is detected to be generated from one of
// these macros, it should be always compared as equal.
std::set<std::string> ignoredMacroList = {
        "__COUNTER__", "__FILE__", "__LINE__", "__DATE__", "__TIME__"};

/// Run comparison of PHI instructions after comparing everything else. This is
/// to ensure that values and blocks incoming to PHIs are properly matched in
/// time of PHI comparison.
int DifferentialFunctionComparator::compare() {
    int Res = FunctionComparator::compare();
    if (Res == 0) {
        for (auto &PhiPair : phisToCompare)
            if (cmpPHIs(PhiPair.first, PhiPair.second))
                return 1;
        return 0;
    }
    return Res;
}

/// Compare GEPs. This code is copied from FunctionComparator::cmpGEPs since it
/// was not possible to simply call the original function.
/// Handles offset between matching GEP indices in the compared modules.
/// Uses data saved in StructFieldNames.
int DifferentialFunctionComparator::cmpGEPs(const GEPOperator *GEPL,
                                            const GEPOperator *GEPR) const {
    int OriginalResult = FunctionComparator::cmpGEPs(GEPL, GEPR);

    if (OriginalResult == 0)
        // The original function says the GEPs are equal - return the value
        return OriginalResult;

    if (!isa<StructType>(GEPL->getSourceElementType())
        || !isa<StructType>(GEPR->getSourceElementType()))
        // One of the types in not a structure - the original function is
        // sufficient for correct comparison
        return OriginalResult;

    if (getStructTypeName(dyn_cast<StructType>(GEPL->getSourceElementType()))
        != getStructTypeName(
                dyn_cast<StructType>(GEPR->getSourceElementType())))
        // Different structure names - the indices may be same by coincidence,
        // therefore index comparison can't be used
        return OriginalResult;

    unsigned int ASL = GEPL->getPointerAddressSpace();
    unsigned int ASR = GEPR->getPointerAddressSpace();

    if (int Res = cmpNumbers(ASL, ASR))
        return Res;

    if (int Res = cmpNumbers(GEPL->getNumIndices(), GEPR->getNumIndices()))
        return Res;

    if (GEPL->hasAllConstantIndices() && GEPR->hasAllConstantIndices()) {
        std::vector<Value *> IndicesL;
        std::vector<Value *> IndicesR;

        const GetElementPtrInst *GEPiL = dyn_cast<GetElementPtrInst>(GEPL);
        const GetElementPtrInst *GEPiR = dyn_cast<GetElementPtrInst>(GEPR);

        if (!GEPiL || !GEPiR)
            return OriginalResult;

        for (auto idxL = GEPL->idx_begin(), idxR = GEPR->idx_begin();
             idxL != GEPL->idx_end() && idxR != GEPR->idx_end();
             ++idxL, ++idxR) {
            auto ValueTypeL = GEPiL->getIndexedType(
                    GEPiL->getSourceElementType(), ArrayRef<Value *>(IndicesL));

            auto ValueTypeR = GEPiR->getIndexedType(
                    GEPiR->getSourceElementType(), ArrayRef<Value *>(IndicesR));

            auto NumericIndexL = dyn_cast<ConstantInt>(idxL->get())->getValue();
            auto NumericIndexR = dyn_cast<ConstantInt>(idxR->get())->getValue();

            if (!ValueTypeL->isStructTy() || !ValueTypeR->isStructTy()) {
                // If the indexed type is not a structure type, the indices have
                // to match in order for the instructions to be equivalent
                if (int Res = cmpValues(idxL->get(), idxR->get()))
                    return Res;

                IndicesL.push_back(*idxL);
                IndicesR.push_back(*idxR);

                continue;
            }

            // The indexed type is a structure type - compare the names of the
            // structure members from StructFieldNames.
            auto MemberNameL =
                    DI->StructFieldNames.find({dyn_cast<StructType>(ValueTypeL),
                                               NumericIndexL.getZExtValue()});

            auto MemberNameR =
                    DI->StructFieldNames.find({dyn_cast<StructType>(ValueTypeR),
                                               NumericIndexR.getZExtValue()});

            if (MemberNameL == DI->StructFieldNames.end()
                || MemberNameR == DI->StructFieldNames.end()
                || !MemberNameL->second.equals(MemberNameR->second))
                if (int Res = cmpValues(idxL->get(), idxR->get()))
                    return Res;

            IndicesL.push_back(*idxL);
            IndicesR.push_back(*idxR);
        }
    } else if (GEPL->getNumIndices() == 1 && GEPR->getNumIndices() == 1) {
        // If there is just a single (non-constant) index, it is an array
        // element access. We do not need to compare source element type since
        // members of the elements are not accessed here.
        // Just the index itself is compared.
        return cmpValues(GEPL->getOperand(1), GEPR->getOperand(1));
    } else
        // Indices can't be compared by name, because they are not constant
        return OriginalResult;

    return 0;
}

/// Ignore differences in attributes.
int DifferentialFunctionComparator::cmpAttrs(const AttributeList L,
                                             const AttributeList R) const {
    return 0;
}

/// Does additional operations in cases when a difference between two CallInsts
/// or their arguments is detected.
/// This consists of four parts:
/// 1. Compare the called functions using cmpGlobalValues (covers case when they
/// are not compared in cmpBasicBlocks because there is another difference).
/// 2. Try to inline the functions.
/// 3. Find type differences if the called functions are field access
///    abstractions.
/// 4. Look a macro-function difference.
void DifferentialFunctionComparator::processCallInstDifference(
        const CallInst *CL, const CallInst *CR) const {
    // Use const_cast to get a non-const point to the function (this is
    // analogical to the cast in FunctionComparator::cmpValues and effectively
    // analogical to CallInst::getCalledFunction, which returns a non-const
    // pointer even if the CallInst itself is const).
    Function *CalledL =
            const_cast<Function *>(getCalledFunction(CL->getCalledValue()));
    Function *CalledR =
            const_cast<Function *>(getCalledFunction(CR->getCalledValue()));
    // Compare both functions using cmpGlobalValues in order to
    // ensure that any differences inside them are detected even
    // if they wouldn't be otherwise compared because of the
    // code stopping before comparing instruction arguments.
    // This inner difference may also cause a difference in the
    // the original function that is not visible in C source
    // code.
    cmpGlobalValues(CalledL, CalledR);
    // If the call instructions are different (cmpOperations
    // doesn't compare the called functions) or the called
    // functions have different names, try inlining them.
    // (except for case when one of the function is a SimpLL
    // abstraction).
    if (!isSimpllAbstractionDeclaration(CalledL)
        && !isSimpllAbstractionDeclaration(CalledR))
        ModComparator->tryInline = {CL, CR};
    // If the call instructions are both calls to field access abstractions,
    // try to find a type difference between them.
    if (isSimpllFieldAccessAbstraction(CalledL)
        && isSimpllFieldAccessAbstraction(CalledR))
        findTypeDifferences(
                CalledL, CalledR, CL->getFunction(), CR->getFunction());

    // Look for a macro-function difference.
    findMacroFunctionDifference(CL, CR);
}

/// Compare allocation instructions using separate cmpAllocs function in case
/// standard comparison returns something other than zero.
int DifferentialFunctionComparator::cmpOperations(
        const Instruction *L,
        const Instruction *R,
        bool &needToCmpOperands) const {
    int Result = FunctionComparator::cmpOperations(L, R, needToCmpOperands);

    // Check whether the instruction is a call instruction.
    if (isa<CallInst>(L) || isa<CallInst>(R)) {
        if (isa<CallInst>(L) && isa<CallInst>(R)) {
            const CallInst *CL = dyn_cast<CallInst>(L);
            const CallInst *CR = dyn_cast<CallInst>(R);

            const Function *CalledL = getCalledFunction(CL->getCalledValue());
            const Function *CalledR = getCalledFunction(CR->getCalledValue());
            if (CalledL && CalledR) {
                if (CalledL->getName() == CalledR->getName()) {
                    // Check whether both instructions call an alloc function.
                    if (isAllocFunction(*CalledL)) {
                        if (!cmpAllocs(CL, CR)) {
                            needToCmpOperands = false;
                            return 0;
                        }
                    }

                    if (CalledL->getIntrinsicID() == Intrinsic::memset
                        && CalledR->getIntrinsicID() == Intrinsic::memset) {
                        if (!cmpMemset(CL, CR)) {
                            needToCmpOperands = false;
                            return 0;
                        }
                    }

                    if (Result && config.ControlFlowOnly
                        && abs(CL->getNumOperands() - CR->getNumOperands())
                                   == 1) {
                        needToCmpOperands = false;
                        return cmpCallsWithExtraArg(CL, CR);
                    }
                }
            }
        }
    }

    // Handling branches with inverse conditions.
    // If such branches are found, successors of one of them are swapped, since
    // their order is important in different parts of FunctionComparator (in
    // particular in compare()).
    if (isa<BranchInst>(L) && isa<BranchInst>(R)) {
        auto BranchL = dyn_cast<BranchInst>(L);
        auto BranchR = dyn_cast<BranchInst>(R);
        if (BranchL->isConditional() && BranchR->isConditional()) {
            auto conds = std::make_pair(BranchL->getCondition(),
                                        BranchR->getCondition());
            if (inverseConditions.find(conds) != inverseConditions.end()) {
                // Swap successors of one of the branches
                BranchInst *BranchNew = const_cast<BranchInst *>(BranchR);
                auto *tmpSucc = BranchNew->getSuccessor(0);
                BranchNew->setSuccessor(0, BranchNew->getSuccessor(1));
                BranchNew->setSuccessor(1, tmpSucc);
                return 0;
            }
        }
    }

    // If PHI nodes are compared and they have the same number of incoming
    // values, treat them as equal for now. They will be compared at the end,
    // after all their incoming values have been compared and matched.
    if (isa<PHINode>(L) && isa<PHINode>(R)) {
        auto PhiL = dyn_cast<PHINode>(L);
        auto PhiR = dyn_cast<PHINode>(R);
        if (PhiL->getNumIncomingValues() == PhiR->getNumIncomingValues()) {
            needToCmpOperands = false;
            phisToCompare.emplace_back(PhiL, PhiR);
            return 0;
        }
    }

    if (Result) {
        // Do not make difference between signed and unsigned for control flow
        // only
        if (config.ControlFlowOnly && isa<ICmpInst>(L) && isa<ICmpInst>(R)) {
            auto *ICmpL = dyn_cast<ICmpInst>(L);
            auto *ICmpR = dyn_cast<ICmpInst>(R);
            if (ICmpL->getUnsignedPredicate()
                == ICmpR->getUnsignedPredicate()) {
                return 0;
            }
        }
        // Handle alloca of a structure type with changed layout
        if (isa<AllocaInst>(L) && isa<AllocaInst>(R)) {
            StructType *TypeL = dyn_cast<StructType>(
                    dyn_cast<AllocaInst>(L)->getAllocatedType());
            StructType *TypeR = dyn_cast<StructType>(
                    dyn_cast<AllocaInst>(R)->getAllocatedType());
            if (TypeL && TypeR
                && TypeL->getStructName() == TypeR->getStructName())
                return cmpNumbers(dyn_cast<AllocaInst>(L)->getAlignment(),
                                  dyn_cast<AllocaInst>(R)->getAlignment());
        }
        // Handle inverse conditions
        if (isa<CmpInst>(L) && isa<CmpInst>(R)) {
            auto CmpL = dyn_cast<CmpInst>(L);
            auto CmpR = dyn_cast<CmpInst>(R);

            // All users of the conditions must be branching instructions
            if (std::all_of(
                        CmpL->users().begin(),
                        CmpL->users().end(),
                        [](const User *user) { return isa<BranchInst>(*user); })
                && std::all_of(CmpR->users().begin(),
                               CmpR->users().end(),
                               [](const User *user) {
                                   return isa<BranchInst>(*user);
                               })) {

                // It is sufficient to compare the predicates here since the
                // operands are compared in cmpBasicBlocks.
                if (CmpL->getPredicate() == CmpR->getInversePredicate()) {
                    inverseConditions.emplace(L, R);
                    return 0;
                }
            }
        }
    }

    return Result;
}

/// Finds all differences between source types in GEPs inside two field access
/// abstractions and records them using findTypeDifference.
void DifferentialFunctionComparator::findTypeDifferences(
        const Function *FAL,
        const Function *FAR,
        const Function *L,
        const Function *R) const {
    std::vector<Type *> SrcTypesL = getFieldAccessSourceTypes(FAL);
    std::vector<Type *> SrcTypesR = getFieldAccessSourceTypes(FAR);
    for (int i = 0; i < std::min(SrcTypesL.size(), SrcTypesR.size()); i++) {
        StructType *STyL = dyn_cast<StructType>(SrcTypesL[i]);
        StructType *STyR = dyn_cast<StructType>(SrcTypesR[i]);
        if (!STyL || !STyR)
            continue;
        if (!STyL->hasName() || !STyR->hasName()
            || (STyL->getName() != STyR->getName()))
            continue;
        findTypeDifference(STyL, STyR, L, R);
    }
}

/// Find and record a difference between structure types.
void DifferentialFunctionComparator::findTypeDifference(
        StructType *L,
        StructType *R,
        const Function *FL,
        const Function *FR) const {
    if (cmpTypes(L, R)) {
        std::unique_ptr<TypeDifference> diff =
                std::make_unique<TypeDifference>();
        diff->name = L->getName().startswith("struct.") ? L->getName().substr(7)
                                                        : L->getName();

        // Try to get the debug info for the structure type.
        DICompositeType *DCTyL = nullptr, *DCTyR = nullptr;
        if (ModComparator->StructDIMapL.find(diff->name)
            != ModComparator->StructDIMapL.end()) {
            DCTyL = ModComparator->StructDIMapL[diff->name];
        }
        if (ModComparator->StructDIMapR.find(diff->name)
            != ModComparator->StructDIMapR.end()) {
            DCTyR = ModComparator->StructDIMapR[diff->name];
        }
        if (!DCTyL || !DCTyR)
            // Debug info not found.
            return;

        diff->function = FL->getName();
        diff->FileL = joinPath(DCTyL->getDirectory(), DCTyL->getFilename());
        diff->FileR = joinPath(DCTyR->getDirectory(), DCTyR->getFilename());
        // Note: for some reason the starting line of the struct in the debug
        // info is the first attribute, skipping the actual declaration.
        // This is fixed by decrementing the line number.
        diff->LineL = DCTyL->getLine() - 1;
        diff->LineR = DCTyR->getLine() - 1;
        diff->StackL.push_back(CallInfo{diff->name + " (type)",
                                        FL->getSubprogram()->getFilename(),
                                        FL->getSubprogram()->getLine()});
        diff->StackR = CallStack();
        diff->StackR.push_back(CallInfo{diff->name + " (type)",
                                        FR->getSubprogram()->getFilename(),
                                        FR->getSubprogram()->getLine()});
        ModComparator->ComparedFuns.at({FnL, FnR})
                .addDifferingObject(std::move(diff));
    }
}

/// Detects a change from a function to a macro between two instructions.
/// This is necessary because such a change isn't visible in C source.
void DifferentialFunctionComparator::findMacroFunctionDifference(
        const Instruction *L, const Instruction *R) const {
    // When trying to inline, it is possible that in one of the versions
    // a function correspond to a macro in the other. This is fixed by inlining
    // when the code is actually equal, but would lead to a difference being
    // reported in a function in which there is no apparent syntactic one. For
    // this reason a SyntaxDifference object is generated to report
    // the difference between the function and the macro.

    // First look whether this is the case described above.
    auto LineL = extractLineFromLocation(L->getDebugLoc());
    auto LineR = extractLineFromLocation(R->getDebugLoc());
    auto &MacrosL = ModComparator->MacroDiffs.getAllMacroUsesAtLocation(
            L->getDebugLoc(), 0);
    auto &MacrosR = ModComparator->MacroDiffs.getAllMacroUsesAtLocation(
            R->getDebugLoc(), 0);
    std::string NameL;
    std::string NameR;
    if (isa<CallInst>(L))
        NameL = getCalledFunction(dyn_cast<CallInst>(L)->getCalledValue())
                        ->getName();
    if (isa<CallInst>(R))
        NameR = getCalledFunction(dyn_cast<CallInst>(R)->getCalledValue())
                        ->getName();

    // Note: the line has to actually have been found for the comparison to make
    // sense.
    if ((LineL != "") && (LineR != "") && (LineL == LineR)
        && ((MacrosL.find(NameL) == MacrosL.end()
             && MacrosR.find(NameL) != MacrosR.end())
            || (MacrosL.find(NameR) != MacrosL.end()
                && MacrosR.find(NameR) == MacrosR.end()))) {
        std::string trueName;
        if ((MacrosL.find(NameL) == MacrosL.end()
             && MacrosR.find(NameL) != MacrosR.end())) {
            trueName = NameL;
            NameR = NameL + " (macro)";
            ModComparator->tryInline = {dyn_cast<CallInst>(L), nullptr};
        } else {
            trueName = NameR;
            NameL = NameR + " (macro)";
            ModComparator->tryInline = {nullptr, dyn_cast<CallInst>(R)};
        }

        DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                        dbgs() << getDebugIndent() << "Writing function-macro "
                               << "syntactic difference\n");

        std::unique_ptr<SyntaxDifference> diff =
                std::make_unique<SyntaxDifference>();
        diff->function = L->getFunction()->getName();
        diff->name = trueName;
        diff->BodyL = "[macro function difference]";
        diff->BodyR = "[macro function difference]";
        diff->StackL =
                CallStack{CallInfo{NameL,
                                   L->getDebugLoc()->getFile()->getFilename(),
                                   L->getDebugLoc()->getLine()}};
        diff->StackR =
                CallStack{CallInfo{NameR,
                                   R->getDebugLoc()->getFile()->getFilename(),
                                   R->getDebugLoc()->getLine()}};
        ModComparator->ComparedFuns.at({FnL, FnR})
                .addDifferingObject(std::move(diff));
    }
}

/// Compare structure size with a constant.
/// @return 0 if equal, 1 otherwise.
int DifferentialFunctionComparator::cmpStructTypeSizeWithConstant(
        StructType *Type, const Value *Const) const {
    uint64_t ConstValue = dyn_cast<ConstantInt>(Const)->getZExtValue();
    return ConstValue != LayoutL.getTypeStoreSize(Type);
}

/// Handle comparing of memory allocation function in cases where the size
/// of the composite type is different.
int DifferentialFunctionComparator::cmpAllocs(const CallInst *CL,
                                              const CallInst *CR) const {
    // Look whether the sizes for allocation match. If yes, then return zero
    // (ignore flags).
    if (cmpValues(CL->op_begin()->get(), CR->op_begin()->get()) == 0) {
        return 0;
    }

    // The instruction is a call instrution calling the function kzalloc. Now
    // look whether the next instruction is a BitCastInst casting to a structure
    // type.
    if (!isa<BitCastInst>(CL->getNextNode())
        || !isa<BitCastInst>(CR->getNextNode()))
        return 1;

    // Check if kzalloc has constant size of the allocated memory
    if (!isa<ConstantInt>(CL->getOperand(0))
        || !isa<ConstantInt>(CR->getOperand(0)))
        return 1;

    // Get the allocated structure types
    StructType *STyL = getStructType(CL->getNextNode());
    StructType *STyR = getStructType(CR->getNextNode());

    // Return 0 (equality) if both allocated types are structs of the same name
    // and each struct has a size equal to the size of the allocated memory.
    return !STyL || !STyR
           || cmpStructTypeSizeWithConstant(STyL, CL->getOperand(0))
           || cmpStructTypeSizeWithConstant(STyR, CR->getOperand(0))
           || STyL->getStructName() != STyR->getStructName();
}

/// Check if the given operation can be ignored (it does not affect semantics)
/// for control flow only diffs.
bool DifferentialFunctionComparator::mayIgnore(const User *Inst) const {
    if (config.ControlFlowOnly)
        return isa<AllocaInst>(Inst) || isCast(Inst) || isZeroGEP(Inst);
    else {
        if (isa<AllocaInst>(Inst) || isZeroGEP(Inst))
            return true;
        if (isCast(Inst)) {
            auto SrcTy = Inst->getOperand(0)->getType();
            auto DestTy = Inst->getType();

            if (auto StrTy = dyn_cast<StructType>(SrcTy)) {
                if (StrTy->hasName() && StrTy->getName().startswith("union"))
                    // Casting from a union type is safe to ignore, because in
                    // case it was generated by something different than a
                    // replacement of a simple type with a union, the
                    // comparator would detect a difference when the value is
                    // used, since it would have a different type.
                    return true;
            }

            if (SrcTy->isPointerTy() && DestTy->isPointerTy())
                // Pointer cast is also safe to ignore - in case there will be
                // an instruction affected by this (i.e. a store, load or GEP),
                // there would be an additional difference that will be detected
                // later.
                return true;

            if (SrcTy->isIntegerTy() && DestTy->isIntegerTy()) {
                // Integer-to-integer casts are safe to ignore only if two
                // conditions are met:
                // 1. The destination type is larger than the source type.
                // 2. No arithmetic is performed on the number.
                // Note: store instructions aren't a problem here, since the
                // types of the arithmetic instructions would be different if
                // there is arithmetic performed on the value after being
                // loaded back.
                auto IntTySrc = dyn_cast<IntegerType>(SrcTy);
                auto IntTyDest = dyn_cast<IntegerType>(DestTy);
                if (IntTySrc->getBitWidth() > IntTyDest->getBitWidth()) {
                    return false;
                }
                // Look for arithmetic operations in the uses of the cast and
                // in the uses of all values that are generated by further
                // casting.
                std::vector<const User *> UserStack;
                UserStack.push_back(Inst);
                while (!UserStack.empty()) {
                    const User *U = UserStack.back();
                    UserStack.pop_back();
                    if (isa<BinaryOperator>(U)) {
                        return false;
                    } else if (isa<CastInst>(U)) {
                        for (const User *UU : U->users())
                            UserStack.push_back(UU);
                    }
                }
                return true;
            }
        }
        return false;
    }
}

bool mayIgnoreMacro(std::string macro) {
    return ignoredMacroList.find(macro) != ignoredMacroList.end();
}

/// Does additional comparisons based on the C source to determine whether two
/// call function arguments that may be compared as non-equal by LLVM are
/// actually semantically equal.
bool DifferentialFunctionComparator::cmpCallArgumentUsingCSource(
        const CallInst *CIL,
        const CallInst *CIR,
        Value *OpL,
        Value *OpR,
        unsigned i) const {
    // Try to prepare C source argument values to be used in operand
    // comparison.
    std::vector<std::string> CArgsL, CArgsR;
    const Function *CFL = getCalledFunction(CIL->getCalledValue());
    const Function *CFR = getCalledFunction(CIR->getCalledValue());
    const BasicBlock *BBL = CIL->getParent();
    const BasicBlock *BBR = CIR->getParent();

    // Use the appropriate C source call argument extraction function depending
    // on whether it is an inline assembly call or not.
    if (CFL->getName().startswith(SimpllInlineAsmPrefix))
        CArgsL = findInlineAssemblySourceArguments(CIL->getDebugLoc(),
                                                   getInlineAsmString(CFL),
                                                   &ModComparator->MacroDiffs);
    else
        CArgsL = findFunctionCallSourceArguments(
                CIL->getDebugLoc(), CFL->getName(), &ModComparator->MacroDiffs);

    if (CFR->getName().startswith(SimpllInlineAsmPrefix))
        CArgsR = findInlineAssemblySourceArguments(CIR->getDebugLoc(),
                                                   getInlineAsmString(CFR),
                                                   &ModComparator->MacroDiffs);
    else
        CArgsR = findFunctionCallSourceArguments(
                CIR->getDebugLoc(), CFL->getName(), &ModComparator->MacroDiffs);

    if ((CArgsL.size() > i) && (CArgsR.size() > i)) {
        if (mayIgnoreMacro(CArgsL[i]) && mayIgnoreMacro(CArgsR[i])
            && (CArgsL[i] == CArgsR[i])) {
            DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                            dbgs() << getDebugIndent()
                                   << "Comparing integers as equal because of "
                                   << "correspondence to an ignored macro\n");
            return 0;
        }

        if (StringRef(CArgsL[i]).startswith("sizeof")
            && StringRef(CArgsR[i]).startswith("sizeof")
            && isa<ConstantInt>(OpL) && isa<ConstantInt>(OpR)) {
            // Both arguments are sizeofs; look whether they correspond to
            // a changed size of the same structure.
            int IntL = dyn_cast<ConstantInt>(OpL)->getZExtValue();
            int IntR = dyn_cast<ConstantInt>(OpR)->getZExtValue();
            auto SizeL = ModComparator->StructSizeMapL.find(IntL);
            auto SizeR = ModComparator->StructSizeMapR.find(IntR);

            // Extract the identifiers inside sizeofs
            auto IdL = getSubstringToMatchingBracket(CArgsL[i], 6);
            auto IdR = getSubstringToMatchingBracket(CArgsR[i], 6);
            IdL = IdL.substr(1, IdL.size() - 2);
            IdR = IdR.substr(1, IdR.size() - 2);

            auto TyIdL = getCSourceIdentifierType(
                    IdL, BBL->getParent(), DI->LocalVariableMapL);
            auto TyIdR = getCSourceIdentifierType(
                    IdR, BBR->getParent(), DI->LocalVariableMapR);

            // If the types are structure types, prepare their names for
            // comparison. (Use different generic names for non-structure
            // types.)
            // Note: since the sizeof is different the types would likely be
            // also compared as different using cmpTypes.
            auto TyIdLName =
                    TyIdL && TyIdL->isStructTy()
                            ? dyn_cast<StructType>(TyIdL)->getStructName()
                            : "Type 1";
            auto TyIdRName =
                    TyIdR && TyIdR->isStructTy()
                            ? dyn_cast<StructType>(TyIdR)->getStructName()
                            : "Type 2";

            // In case both values belong to the sizes of the same structure or
            // the original types were found and their names (in the case of
            // structure types) are the same, compare the sizeof as equal.
            if ((SizeL != ModComparator->StructSizeMapL.end()
                 && SizeR != ModComparator->StructSizeMapR.end()
                 && SizeL->second == SizeR->second)
                || (TyIdL != nullptr && TyIdR != nullptr
                    && (!cmpTypes(TyIdL, TyIdR) || TyIdLName == TyIdRName))) {
                DEBUG_WITH_TYPE(
                        DEBUG_SIMPLL,
                        dbgs() << getDebugIndent()
                               << "Comparing integers as equal because of"
                               << "correspondence to structure type sizes\n");
                return 0;
            }
        }
    }

    return 1;
}

/// Detect cast instructions and ignore them when comparing the control flow
/// only.
/// Note: this function was copied from FunctionComparator.
int DifferentialFunctionComparator::cmpBasicBlocks(
        const BasicBlock *BBL, const BasicBlock *BBR) const {
    BasicBlock::const_iterator InstL = BBL->begin(), InstLE = BBL->end();
    BasicBlock::const_iterator InstR = BBR->begin(), InstRE = BBR->end();

    while (InstL != InstLE && InstR != InstRE) {
        if ((&InstL->getDebugLoc())->get())
            CurrentLocL = &InstL->getDebugLoc();
        if ((&InstR->getDebugLoc())->get())
            CurrentLocR = &InstR->getDebugLoc();

        if (int Res = cmpOperationsWithOperands(&*InstL, &*InstR)) {
            // Try to find longer chains of memory accesses that result into
            // accessing the same (corresponding) memory.
            if (isa<GetElementPtrInst>(&*InstL)
                && isa<GetElementPtrInst>(&*InstR)) {
                if (cmpFieldAccess(InstL, InstR) == 0)
                    continue;
            }

            // Some operations not affecting semantics and control flow may be
            // ignored (currently allocas and casts). This may help to handle
            // some small changes that do not affect semantics (it is also
            // useful in combination with function inlining).
            if (mayIgnore(&*InstL) || mayIgnore(&*InstR)) {
                // Reset serial counters
                sn_mapL.erase(&*InstL);
                sn_mapR.erase(&*InstR);
                // One of the compared operations will be skipped and the
                // comparison will be repeated.
                if (mayIgnore(&*InstL))
                    InstL++;
                else
                    InstR++;
                continue;
            }

            if (Res) {
                // The instructions are indeed different, try to find the source
                // of the difference.
                findDifference(&*InstL, &*InstR);
                return Res;
            }
        }

        if (isa<CallInst>(&*InstL) && isa<CallInst>(&*InstR)) {
            // Try to find a type difference in chained field accesses.
            // This has to be done even if the operations are equal, since field
            // accesses return a pointer and all pointers are compared as equal
            // in FunctionComparator.
            findTypeDifferenceInChainedFieldAccess(dyn_cast<CallInst>(&*InstL),
                                                   dyn_cast<CallInst>(&*InstR));
        }

        ++InstL;
        ++InstR;
    }

    if (InstL != InstLE && InstR == InstRE)
        return 1;
    if (InstL == InstLE && InstR != InstRE)
        return -1;
    return 0;
}

/// Looks for inline assembly differences between the call instructions.
std::vector<std::unique_ptr<SyntaxDifference>>
        DifferentialFunctionComparator::findAsmDifference(
                const CallInst *IL, const CallInst *IR) const {
    auto FunL = getCalledFunction(IL->getCalledValue());
    auto FunR = getCalledFunction(IR->getCalledValue());
    auto ParentL = IL->getFunction();
    auto ParentR = IR->getFunction();

    if (!FunL || !FunR)
        // Both values have to be functions
        return {};

    if (!FunL->getName().startswith(SimpllInlineAsmPrefix)
        || !FunR->getName().startswith(SimpllInlineAsmPrefix))
        // Both functions have to be assembly abstractions
        return {};

    StringRef AsmL = getInlineAsmString(FunL);
    StringRef AsmR = getInlineAsmString(FunR);
    if (AsmL == AsmR)
        // The difference is somewhere else
        return {};

    // Iterate over inline asm operands and generate a C-like identifier for
    // every one of them.
    // Note: because this function's main purpose is to show inline assembly
    // differences in cases when the original macro containing them cannot be
    // found by SourceCodeUtils, the original arguments in the C source code
    // also cannot be localed, therefore the C-like identifier is used instead.
    std::string argumentNamesL, argumentNamesR;
    for (auto T : {std::make_tuple(IL, &argumentNamesL),
                   std::make_tuple(IR, &argumentNamesR)}) {
        // The identifier generation is done separately for the left and right
        // call instruction; vector of tuples and pointers are used in order to
        // re-use the code for both.
        const CallInst *I = std::get<0>(T);
        std::string *argumentNames = std::get<1>(T);

        for (int i = 0; i < I->getNumArgOperands(); i++) {
            const Value *Op = I->getArgOperand(i);
            std::string OpName = getIdentifierForValue(
                    Op, DI->StructFieldNames, I->getFunction());

            if (*argumentNames == "")
                *argumentNames += OpName;
            else
                *argumentNames += ", " + OpName;
        }
    }

    // Create difference object.
    // Note: the call stack is left empty here, it will be added in reportOutput
    std::unique_ptr<SyntaxDifference> diff =
            std::make_unique<SyntaxDifference>();
    diff->BodyL = AsmL.str() + " (args: " + argumentNamesL + ")";
    diff->BodyR = AsmR.str() + " (args: " + argumentNamesR + ")";
    diff->StackL = CallStack();
    diff->StackL.push_back(CallInfo{"(generated assembly code)",
                                    ParentL->getSubprogram()->getFilename(),
                                    ParentL->getSubprogram()->getLine()});
    diff->StackR = CallStack();
    diff->StackR.push_back(CallInfo{"(generated assembly code)",
                                    ParentR->getSubprogram()->getFilename(),
                                    ParentR->getSubprogram()->getLine()});
    diff->function = ParentL->getName();
    diff->name = "assembly code "
                 + std::to_string(++ModComparator->asmDifferenceCounter);

    std::vector<std::unique_ptr<SyntaxDifference>> Result;
    Result.push_back(std::move(diff));

    return Result;
}

/// Implement comparison of global values that does not use a
/// GlobalNumberState object, since that approach does not fit the use case
/// of comparing functions in two different modules.
int DifferentialFunctionComparator::cmpGlobalValues(GlobalValue *L,
                                                    GlobalValue *R) const {
    auto GVarL = dyn_cast<GlobalVariable>(L);
    auto GVarR = dyn_cast<GlobalVariable>(R);

    if (GVarL && GVarR && GVarL->hasInitializer() && GVarR->hasInitializer()
        && GVarL->isConstant() && GVarR->isConstant()) {
        // Constant global variables are compared using their initializers.
        return cmpConstants(GVarL->getInitializer(), GVarR->getInitializer());
    } else if (L->hasName() && R->hasName()) {
        // Both values are named, compare them by names
        auto NameL = L->getName();
        auto NameR = R->getName();

        // Remove number suffixes
        if (hasSuffix(NameL))
            NameL = NameL.substr(0, NameL.find_last_of("."));
        if (hasSuffix(NameR))
            NameR = NameR.substr(0, NameR.find_last_of("."));
        if (NameL == NameR
            || (isPrintFunction(NameL) && isPrintFunction(NameR))) {
            if (isa<Function>(L) && isa<Function>(R)) {
                // Functions compared as being the same have to be also compared
                // by ModuleComparator.
                auto FunL = dyn_cast<Function>(L);
                auto FunR = dyn_cast<Function>(R);

                // Do not compare SimpLL abstractions.
                if (!isSimpllAbstraction(FunL) && !isSimpllAbstraction(FunR)
                    && (!isPrintFunction(L->getName())
                        && !isPrintFunction(R->getName()))) {
                    // Store the called functions into the current
                    // functions' callee set.
                    ModComparator->ComparedFuns.at({FnL, FnR})
                            .First.addCall(FunL, CurrentLocL->getLine());
                    ModComparator->ComparedFuns.at({FnL, FnR})
                            .Second.addCall(FunR, CurrentLocR->getLine());
                    if (ModComparator->ComparedFuns.find({FunL, FunR})
                        == ModComparator->ComparedFuns.end())
                        ModComparator->compareFunctions(FunL, FunR);
                }

                if (FunL->getName().startswith(SimpllInlineAsmPrefix)
                    && FunR->getName().startswith(SimpllInlineAsmPrefix)) {
                    // Compare inline assembly code abstractions using metadata
                    // generated in FunctionAbstractionGenerator.
                    StringRef asmL = getInlineAsmString(FunL);
                    StringRef asmR = getInlineAsmString(FunR);
                    StringRef constraintL = getInlineAsmConstraintString(FunL);
                    StringRef constraintR = getInlineAsmConstraintString(FunR);

                    return !(asmL == asmR && constraintL == constraintR);
                }
            }
            return 0;

        } else if (NameL != NameR && GVarL && GVarR && GVarL->isConstant()
                   && GVarR->isConstant() && !GVarL->hasInitializer()
                   && !GVarR->hasInitializer()) {
            // Externally defined constants (those without initializer
            // and with different names) need to have their definitions linked.
            ModComparator->MissingDefs.push_back({GVarL, GVarR});
            return 1;
        }

        else
            return 1;
    } else
        return L != R;
}

// Takes all GEPs in a basic block and computes the sum of their offsets if
// constant (if not, it returns false).
bool DifferentialFunctionComparator::accumulateAllOffsets(
        const BasicBlock &BB, uint64_t &Offset) const {
    for (auto &Inst : BB) {
        if (auto GEP = dyn_cast<GetElementPtrInst>(&Inst)) {
            APInt InstOffset(32, 0);
            if (!GEP->accumulateConstantOffset(BB.getModule()->getDataLayout(),
                                               InstOffset))
                return false;
            Offset += InstOffset.getZExtValue();
        }
    }
    return true;
}

/// Check if the given instruction is a memory access (i.e. a GEP or a pointer
/// bitcast) to the given pointer. If so, return true and set the size of the
/// offset that the instruction adds to the pointer, otherwise return false.
bool isMemoryAccessToPtr(const Instruction *Inst,
                         const Value *Ptr,
                         int &Offset) {
    if (isa<GetElementPtrInst>(Inst)
        || ((isa<CastInst>(Inst) && !isa<PtrToIntInst>(Inst)))) {
        if (Ptr != Inst->getOperand(0))
            return false;
        if (auto *GEP = dyn_cast<GetElementPtrInst>(Inst)) {
            APInt InstOffset(64, 0);
            if (!GEP->accumulateConstantOffset(
                        Inst->getParent()->getModule()->getDataLayout(),
                        InstOffset)) {
                return false;
            }
            Offset = InstOffset.getZExtValue();
        }
        return true;
    }
    return false;
}

/// Specific comparing of sequences of field accesses.
int DifferentialFunctionComparator::cmpFieldAccess(
        BasicBlock::const_iterator &InstL,
        BasicBlock::const_iterator &InstR) const {
    // Compare the complete offset of an unbroken sequence of GEP and bitcast
    // in each module, starting from InstL and InstR, respectively.
    // It is possible that although the process contains unpacking several
    // anonymous unions and structs, the offset stays the same, which means that
    // without a doubt this is semantically equal (and probably would, in fact,
    // be also equal in machine code).

    // Backup the instruction iterators - if the comparison is not successful,
    // we'll have to restore them.
    auto FirstInstL = InstL;
    auto FirstInstR = InstR;

    auto GEPL = dyn_cast<GetElementPtrInst>(InstL);
    auto GEPR = dyn_cast<GetElementPtrInst>(InstR);

    if (!(GEPL && GEPR))
        return 1;

    const Value *PtrL = GEPL->getOperand(0);
    const Value *PtrR = GEPR->getOperand(0);

    if (int CmpPtrs = cmpValues(PtrL, PtrR))
        return CmpPtrs;

    uint64_t OffsetL = 0, OffsetR = 0;
    bool l_end = false, r_end = false;
    while (!l_end && !r_end) {
        int offset = 0;
        if (isMemoryAccessToPtr(&*InstL, PtrL, offset)) {
            OffsetL += offset;
            PtrL = &*InstL;
            InstL++;
        } else {
            l_end = true;
        }

        if (isMemoryAccessToPtr(&*InstR, PtrR, offset)) {
            OffsetR += offset;
            PtrR = &*InstR;
            InstR++;
        } else {
            r_end = true;
        }
    }

    if (OffsetL == OffsetR) {
        // Makes sure that the resulting pointers coming out of the sequences
        // are synchronized (have the same serial number).
        cmpValues(PtrL, PtrR);
        return 0;
    }

    // Restore instruction iterators to their original values
    InstL = FirstInstL;
    InstR = FirstInstR;
    return 1;
}

/// Handle values generated from macros and enums whose value changed.
/// The new values are pre-computed by DebugInfo.
/// Also handles comparing in case at least one of the values is a cast -
/// when comparing the control flow only, it compares the original value instead
/// of the cast.
int DifferentialFunctionComparator::cmpValues(const Value *L,
                                              const Value *R) const {
    // Detect casts and GEPs with all indices equal to 0 and use the original
    // value instead.
    const User *UL = dyn_cast<User>(L);
    const User *UR = dyn_cast<User>(R);
    const User *CL = (UL && (isCast(UL) || isZeroGEP(UL))) ? UL : nullptr;
    const User *CR = (UR && (isCast(UR) || isZeroGEP(UR))) ? UR : nullptr;

    if (CL && CR && mayIgnore(CL) && mayIgnore(CR)) {
        // Both instruction are casts - compare the original values before
        // the cast
        return cmpValues(CL->getOperand(0), CR->getOperand(0));
    } else if (CL && mayIgnore(CL)) {
        // The left value is a cast - use the original value of it in the
        // comparison
        return cmpValues(CL->getOperand(0), R);
    } else if (CR && mayIgnore(CR)) {
        // The right value is a cast - use the original value of it in the
        // comparison
        return cmpValues(L, CR->getOperand(0));
    }

    int result = FunctionComparator::cmpValues(L, R);
    if (result) {
        if (isa<Constant>(L) && isa<Constant>(R)) {
            auto *ConstantL = dyn_cast<Constant>(L);
            auto *ConstantR = dyn_cast<Constant>(R);
            auto MacroMapping = DI->MacroConstantMap.find(ConstantL);
            if (MacroMapping != DI->MacroConstantMap.end()
                && MacroMapping->second == valueAsString(ConstantR))
                return 0;
        } else if (isa<BasicBlock>(L) && isa<BasicBlock>(R)) {
            // In case functions have different numbers of BBs, they may be
            // compared as unequal here. However, this can be caused by moving
            // part of the functionality into a function and hence we'll
            // treat the BBs as equal here to continue comparing and maybe
            // try inlining.
            // We also need to remove a BB that was newly inserted in cmpValues
            // since the serial maps would not be synchronized otherwise.
            if (sn_mapL.size() != sn_mapR.size()) {
                if (sn_mapL[L] == (sn_mapL.size() - 1))
                    sn_mapL.erase(L);
                if (sn_mapR[R] == (sn_mapR.size() - 1))
                    sn_mapR.erase(R);
            }
            return 0;
        }
    }
    return result;
}

/// Specific comparing of constants. If one of them (or both) is a cast
/// constant expression, compare its operand.
int DifferentialFunctionComparator::cmpConstants(const Constant *L,
                                                 const Constant *R) const {
    int Result = FunctionComparator::cmpConstants(L, R);

    if (Result == 0)
        return Result;

    if (config.ControlFlowOnly) {
        // Look whether the constants is a cast ConstantExpr
        const ConstantExpr *UEL = dyn_cast<ConstantExpr>(L);
        const ConstantExpr *UER = dyn_cast<ConstantExpr>(R);

        // We want to compare only casts by their operands
        if (UEL && !UEL->isCast())
            UEL = nullptr;
        if (UER && !UER->isCast())
            UER = nullptr;

        if (UEL && UER) {
            return cmpConstants(UEL->getOperand(0), UER->getOperand(0));
        } else if (UEL) {
            return cmpConstants(UEL->getOperand(0), R);
        } else if (UER) {
            return cmpConstants(L, UER->getOperand(0));
        }
    }

    return Result;
}

int DifferentialFunctionComparator::cmpCallsWithExtraArg(
        const CallInst *CL, const CallInst *CR) const {
    // Distinguish which call has more parameters
    const CallInst *CallExtraArg;
    const CallInst *CallOther;
    if (CL->getNumOperands() > CR->getNumOperands()) {
        CallExtraArg = CL;
        CallOther = CR;
    } else {
        CallExtraArg = CR;
        CallOther = CL;
    }

    // The last extra argument must be 0 (false) or NULL
    auto *LastOp = CallExtraArg->getOperand(CallExtraArg->getNumOperands() - 2);
    if (auto ConstLastOp = dyn_cast<Constant>(LastOp)) {
        if (!(ConstLastOp->isNullValue() || ConstLastOp->isZeroValue()))
            return 1;

        // Compare function return types (types of the call instructions)
        if (int Res = cmpTypes(CallExtraArg->getType(), CallOther->getType()))
            return Res;

        // For each argument (except the extra one), compare its type and value.
        // Last argument is not compared since it is the called function.
        for (unsigned i = 0, e = CallOther->getNumOperands() - 1; i != e; ++i) {
            auto Arg1 = CallExtraArg->getOperand(i);
            auto Arg2 = CallOther->getOperand(i);
            if (int Res = cmpTypes(Arg1->getType(), Arg2->getType()))
                return Res;
            if (int Res = cmpValues(Arg1, Arg2))
                return Res;
        }
        return 0;
    }
    return 1;
}

/// Compares array types with equivalent element types as equal when
/// comparing the control flow only.
int DifferentialFunctionComparator::cmpTypes(Type *L, Type *R) const {
    // Compare union as equal to another type in case it is at least of the same
    // size.
    // Note: a union type is represented in Clang-generated LLVM IR by a
    // structure type.
    if (L->isStructTy() || R->isStructTy()) {
        Type *Ty;
        StructType *StrTy;
        const DataLayout *TyLayout, *StrTyLayout;
        if (L->isStructTy()) {
            StrTy = dyn_cast<StructType>(L);
            Ty = R;
            StrTyLayout = &LayoutL;
            TyLayout = &LayoutR;
        } else {
            StrTy = dyn_cast<StructType>(R);
            Ty = L;
            StrTyLayout = &LayoutR;
            TyLayout = &LayoutL;
        }

        if (StrTy->getStructName().startswith("union")
            && (StrTyLayout->getTypeAllocSize(StrTy)
                >= TyLayout->getTypeAllocSize(Ty))) {
            return 0;
        }
    }

    // Compare integer types (except the boolean type) as the same when
    // comparing the control flow only.
    if (L->isIntegerTy() && R->isIntegerTy() && config.ControlFlowOnly) {
        if (L->getIntegerBitWidth() == 1 || R->getIntegerBitWidth() == 1)
            return !(L->getIntegerBitWidth() == R->getIntegerBitWidth());
        return 0;
    }

    if (!L->isArrayTy() || !R->isArrayTy() || !config.ControlFlowOnly)
        return FunctionComparator::cmpTypes(L, R);

    ArrayType *AL = dyn_cast<ArrayType>(L);
    ArrayType *AR = dyn_cast<ArrayType>(R);

    return cmpTypes(AL->getElementType(), AR->getElementType());
}

/// Do not compare bitwidth when comparing the control flow only.
int DifferentialFunctionComparator::cmpAPInts(const APInt &L,
                                              const APInt &R) const {
    int Result = FunctionComparator::cmpAPInts(L, R);
    if (!config.ControlFlowOnly || !Result) {
        return Result;
    } else {
        // The function ugt uses APInt::compare, which can compare only integers
        // of the same bitwidth. When we want to also compare integers of
        // different bitwidth, a different approach has to be used.
        return cmpNumbers(L.getZExtValue(), R.getZExtValue());
    }

    return 0;
}

/// Comparison of memset functions.
/// Handles situation when memset sets the memory occupied by a structure, but
/// the structure size changed.
int DifferentialFunctionComparator::cmpMemset(const CallInst *CL,
                                              const CallInst *CR) const {
    // Compare all except the third operand (size to set).
    for (unsigned i = 0; i < CL->getNumArgOperands(); ++i) {
        if (i == 2)
            continue;
        if (int Res = cmpValues(CL->getArgOperand(i), CR->getArgOperand(i)))
            return Res;
    }

    if (!cmpValues(CL->getArgOperand(2), CR->getArgOperand(2)))
        return 0;

    // Get the struct types of memset destinations.
    StructType *STyL = getStructType(CL->getOperand(0));
    StructType *STyR = getStructType(CR->getOperand(0));

    // Return 0 (equality) if both memory destinations are structs of the same
    // name and each memset size is equal to the corresponding struct size.
    return !STyL || !STyR
           || cmpStructTypeSizeWithConstant(STyL, CL->getOperand(2))
           || cmpStructTypeSizeWithConstant(STyR, CR->getOperand(2))
           || STyL->getStructName() != STyR->getStructName();
}

/// Comparing PHI instructions.
/// Handle different order of incoming values - for each incoming value-block
/// pair, try to find a matching pair in the other PHI instruction.
/// In order to work properly, all the incoming values and blocks should already
/// be analysed.
int DifferentialFunctionComparator::cmpPHIs(const PHINode *PhiL,
                                            const PHINode *PhiR) const {
    for (unsigned i = 0; i < PhiL->getNumIncomingValues(); ++i) {
        bool match = false;
        for (unsigned j = 0; j < PhiR->getNumIncomingValues(); ++j) {
            if (!cmpValues(PhiL->getIncomingBlock(i), PhiR->getIncomingBlock(j))
                && !cmpValues(PhiL->getIncomingValue(i),
                              PhiR->getIncomingValue(j)))
                match = true;
        }
        if (!match)
            return 1;
    }
    return 0;
}

/// Compare two instructions along with their operands.
int DifferentialFunctionComparator::cmpOperationsWithOperands(
        const Instruction *L, const Instruction *R) const {
    // Contains code copied out of the original cmpBasicBlocks since it is more
    // convenient to have the code in a separate function.

    bool needToCmpOperands = true;
    if (int Res = cmpOperations(L, R, needToCmpOperands)) {
        auto *CallL = dyn_cast<CallInst>(L);
        auto *CallR = dyn_cast<CallInst>(R);
        if (CallL != CallR) {
            auto *CalledL = CallL ? getCalledFunction(CallL->getCalledValue())
                                  : nullptr;
            auto *CalledR = CallR ? getCalledFunction(CallR->getCalledValue())
                                  : nullptr;
            // If just one of the instructions is a call, it is possible that
            // some logic has been moved into a function. We'll try to inline
            // that function and compare again.
            if (CalledL && !isSimpllAbstractionDeclaration(CalledL))
                ModComparator->tryInline = {CallL, nullptr};
            else if (CalledR && !isSimpllAbstractionDeclaration(CalledR))
                ModComparator->tryInline = {nullptr, CallR};
        }
        return Res;
    }
    if (needToCmpOperands) {
        assert(L->getNumOperands() == R->getNumOperands());

        for (unsigned i = 0, e = L->getNumOperands(); i != e; ++i) {
            Value *OpL = L->getOperand(i);
            Value *OpR = R->getOperand(i);

            if (int Res = cmpValues(OpL, OpR)) {
                if (isa<CallInst>(L) && isa<CallInst>(R)) {
                    Res = cmpCallArgumentUsingCSource(dyn_cast<CallInst>(L),
                                                      dyn_cast<CallInst>(R),
                                                      OpL,
                                                      OpR,
                                                      i);
                }
                return Res;
            }
            // cmpValues should ensure this is true.
            assert(cmpTypes(OpL->getType(), OpR->getType()) == 0);
        }
    }
    return 0;
}

/// Try to find a syntax difference that could be causing the semantic
/// difference that was found. Looks for differences that cannot be detected
/// by simply diffing the compared functions - differences in macros, inline
/// assembly code, or in types.
void DifferentialFunctionComparator::findDifference(
        const Instruction *L, const Instruction *R) const {
    // Try to find macros that could be causing the difference
    auto macroDiffs = ModComparator->MacroDiffs.findMacroDifferences(&*L, &*R);
    ModComparator->ComparedFuns.at({FnL, FnR})
            .addDifferingObjects(std::move(macroDiffs));

    // If the instructions are calls, try some additional methods to locate the
    // difference.
    if (isa<CallInst>(&*L) || isa<CallInst>(&*R)) {
        auto *CallL = dyn_cast<CallInst>(&*L);
        auto *CallR = dyn_cast<CallInst>(&*R);

        if (CallL && CallR) {
            if (config.PrintAsmDiffs) {
                // Try to find assembly functions causing the difference
                ModComparator->ComparedFuns.at({FnL, FnR})
                        .addDifferingObjects(findAsmDifference(CallL, CallR));
            }

            processCallInstDifference(CallL, CallR);
        } else {
            // Look for a macro-function difference.
            findMacroFunctionDifference(&*L, &*R);
        }
    }

    // Check whether there is an additional cast because of a structure type
    // change.
    if (isa<CastInst>(L) || isa<CastInst>(R)) {
        auto Cast = isa<CastInst>(L) ? dyn_cast<CastInst>(L)
                                     : dyn_cast<CastInst>(R);
        Value *Op = stripAllCasts(Cast->getOperand(0));
        CallInst *CI;
        LoadInst *LI;
        Function *Fun;
        PointerType *PTy;
        StructType *STy;
        // Check if the operator of a cast is a call to a field access
        // abstraction from a named structure type (either directly or through a
        // load).
        if (((CI = dyn_cast<CallInst>(Op))
             || ((LI = dyn_cast<LoadInst>(Op))
                 && (CI = dyn_cast<CallInst>(
                             stripAllCasts(LI->getOperand(0))))))
            && (Fun = getCalledFunction(CI->getCalledValue()))
            && isSimpllFieldAccessAbstraction(Fun)
            && (PTy = dyn_cast<PointerType>(CI->getOperand(0)->getType()))
            && (STy = dyn_cast<StructType>(PTy->getPointerElementType()))
            && STy->hasName()) {
            // Value is casted after being extracted from a structure.
            // There is probably a change in the structure type causing
            // the cast.
            // Try to find the type in the other structure.
            auto OtherSTy = R->getModule()->getTypeByName(STy->getName());
            if (OtherSTy) {
                if (Cast == L)
                    findTypeDifference(
                            STy, OtherSTy, L->getFunction(), R->getFunction());
                else
                    findTypeDifference(
                            OtherSTy, STy, L->getFunction(), R->getFunction());
            }
        }
    }

    // Check whether there is a load type difference because of a structure type
    // change.
    if (isa<LoadInst>(L) && isa<LoadInst>(R)
        && cmpTypes(L->getType(), R->getType())) {
        Value *OpL = stripAllCasts(L->getOperand(0));
        Value *OpR = stripAllCasts(R->getOperand(0));
        CallInst *CL;
        CallInst *CR;
        Function *FL;
        Function *FR;
        // Check whether both load instructions operate on the return value of a
        // call of a field access abstraction from named structure types of the
        // same name.
        if ((CL = dyn_cast<CallInst>(OpL)) && (CR = dyn_cast<CallInst>(OpR))
            && (FL = getCalledFunction(CL->getCalledValue()))
            && (FR = getCalledFunction(CR->getCalledValue()))
            && isSimpllFieldAccessAbstraction(FL)
            && isSimpllFieldAccessAbstraction(FR)) {
            findTypeDifferences(FL, FR, L->getFunction(), R->getFunction());
        }
    }
}

/// Find type differences between calls to field access abstractions.
void DifferentialFunctionComparator::findTypeDifferenceInChainedFieldAccess(
        const CallInst *CL, const CallInst *CR) const {
    const Function *CalledL = getCalledFunction(CL->getCalledValue());
    const Function *CalledR = getCalledFunction(CR->getCalledValue());

    if (isSimpllFieldAccessAbstraction(CalledL)
        && isSimpllFieldAccessAbstraction(CalledR)
        && isa<PointerType>(CL->getType()) && isa<PointerType>(CR->getType())
        && (dyn_cast<PointerType>(CL->getType())->getElementType()
            != dyn_cast<PointerType>(CR->getType())->getElementType())) {
        // It is possible that the abstractions are a part of a longer field
        // access chain. In this case it is necessary to look for the
        // possibility of a structure type change in the previous field access
        // call (which wouldn't be done otherwise because the detection is based
        // on loads).
        // Note: the fact that the chain hasn't been generated as a single
        // abstraction is caused by Clang separating the debug scopes of each
        // part.
        auto FACallL = dyn_cast<CallInst>(CL->getOperand(0));
        auto FACallR = dyn_cast<CallInst>(CR->getOperand(0));
        auto FAOpL = FACallL ? getCalledFunction(FACallL->getCalledValue())
                             : nullptr;
        auto FAOpR = FACallR ? getCalledFunction(FACallR->getCalledValue())
                             : nullptr;

        // Check whether the field access operand is another field access.
        // Furthermore check if the second field access takes a structure
        // pointer.
        if (FAOpL && FAOpR && isSimpllFieldAccessAbstraction(FAOpL)
            && isSimpllFieldAccessAbstraction(FAOpR)) {
            // Field access operand is another field access call.
            // Compare all of its source types for structure type differences.
            findTypeDifferences(
                    FAOpL, FAOpR, CL->getFunction(), CR->getFunction());
        }
    }
}
