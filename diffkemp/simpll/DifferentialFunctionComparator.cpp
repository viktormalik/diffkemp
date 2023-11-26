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
#include "DebugInfo.h"
#include "FieldAccessUtils.h"
#include "Logger.h"
#include "SourceCodeUtils.h"
#include "passes/FunctionAbstractionsGenerator.h"
#include "passes/SimplifyKernelFunctionCallsPass.h"
#include <deque>
#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/GetElementPtrTypeIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <set>
#include <unordered_set>

// If a operand of a call instruction is detected to be generated from one of
// these macros, it should be always compared as equal.
std::set<std::string> ignoredMacroList = {
        "__COUNTER__", "__FILE__", "__LINE__", "__DATE__", "__TIME__"};

/// Initialize relocation info
void DifferentialFunctionComparator::beginCompare() {
    FunctionComparator::beginCompare();
    Reloc.status = RelocationInfo::None;
}

/// Run comparison of PHI instructions after comparing everything else. This is
/// to ensure that values and blocks incoming to PHIs are properly matched in
/// time of PHI comparison.
int DifferentialFunctionComparator::compare() {
    int Res = FunctionComparator::compare();
    // The result is 1 (not equal) if there is an unmatched relocation (since
    // that means that one of the functions has some extra code).
    if (Reloc.status != RelocationInfo::None) {
        ModComparator->tryInline = Reloc.tryInlineBackup;
        return 1;
    }
    if (Res == 0) {
        for (auto &PhiPair : phisToCompare)
            if (cmpPHIs(PhiPair.first, PhiPair.second))
                return 1;
        // Functions are equal so we don't have differing instructions
        DifferingInstructions = {nullptr, nullptr};
        return 0;
    }
    return Res;
}

/// Compares values by their synchronisation. The comparison is unsuccessful
/// if the given values are not mapped to each other.
int DifferentialFunctionComparator::cmpValuesByMapping(const Value *L,
                                                       const Value *R) const {
    // Ensure that no new serial numbers will be assigned.
    if (sn_mapL.find(L) == sn_mapL.end())
        return -1;
    if (sn_mapR.find(R) == sn_mapR.end())
        return 1;

    return sn_mapL[L] != sn_mapR[R];
}

/// Compare GEPs. This code is copied from FunctionComparator::cmpGEPs since it
/// was not possible to simply call the original function.
/// Handles offset between matching GEP indices in the compared modules.
/// Uses data saved in StructFieldNames.
int DifferentialFunctionComparator::cmpGEPs(const GEPOperator *GEPL,
                                            const GEPOperator *GEPR) const {
    PREP_LOG("GEP", GEPL, GEPR);

    int OriginalResult = FunctionComparator::cmpGEPs(GEPL, GEPR);
    if (!config.Patterns.StructAlignment)
        return OriginalResult;

    if (OriginalResult == 0)
        // The original function says the GEPs are equal - return the value
        RETURN_WITH_LOG(OriginalResult);

    if (isa<ArrayType>(GEPL->getSourceElementType())
        && isa<ArrayType>(GEPR->getSourceElementType())) {
        // When the only difference is the size of the accessed array,
        // it is not considered a semantic change if the index type is an enum

        if (GEPL->getNumOperands() != 3 || GEPR->getNumOperands() != 3)
            // We only handle GEPs that access exactly one array element
            RETURN_WITH_LOG(OriginalResult);

        if (GEPL->getSourceElementType()->getArrayNumElements()
            == GEPR->getSourceElementType()->getArrayNumElements())
            RETURN_WITH_LOG(OriginalResult);

        auto *STyL = cast<ArrayType>(GEPL->getSourceElementType());
        auto *STyR = cast<ArrayType>(GEPR->getSourceElementType());
        if (int Res = cmpTypes(STyL->getElementType(), STyR->getElementType()))
            // The array element type must be the same
            RETURN_WITH_LOG(Res);

        for (unsigned i = 0, e = GEPL->getNumOperands(); i != e; ++i) {
            if (int Res = cmpValues(GEPL->getOperand(i), GEPR->getOperand(i)))
                RETURN_WITH_LOG(Res);
        }

        auto TypeL = getVariableTypeInfo(GEPL->getOperand(2));
        auto TypeR = getVariableTypeInfo(GEPR->getOperand(2));

        if (!TypeL || !TypeR)
            RETURN_WITH_LOG(OriginalResult);

        if (TypeL->getTag() == dwarf::DW_TAG_enumeration_type
            && TypeR->getTag() == dwarf::DW_TAG_enumeration_type)
            RETURN_WITH_LOG(0);

        RETURN_WITH_LOG(OriginalResult);
    }

    if (!isa<StructType>(GEPL->getSourceElementType())
        || !isa<StructType>(GEPR->getSourceElementType()))
        // One of the types in not a structure - the original function is
        // sufficient for correct comparison
        RETURN_WITH_LOG(OriginalResult);

    if (getStructTypeName(dyn_cast<StructType>(GEPL->getSourceElementType()))
        != getStructTypeName(
                dyn_cast<StructType>(GEPR->getSourceElementType())))
        // Different structure names - the indices may be same by coincidence,
        // therefore index comparison can't be used
        RETURN_WITH_LOG(OriginalResult);

    unsigned int ASL = GEPL->getPointerAddressSpace();
    unsigned int ASR = GEPR->getPointerAddressSpace();

    if (int Res = cmpNumbers(ASL, ASR))
        RETURN_WITH_LOG(Res);

    if (int Res = cmpNumbers(GEPL->getNumIndices(), GEPR->getNumIndices()))
        RETURN_WITH_LOG(Res);

    if (GEPL->hasAllConstantIndices() && GEPR->hasAllConstantIndices()) {
        std::vector<Value *> IndicesL;
        std::vector<Value *> IndicesR;

        const GetElementPtrInst *GEPiL = dyn_cast<GetElementPtrInst>(GEPL);
        const GetElementPtrInst *GEPiR = dyn_cast<GetElementPtrInst>(GEPR);

        if (!GEPiL || !GEPiR)
            RETURN_WITH_LOG(OriginalResult);

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
                    RETURN_WITH_LOG(Res);

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
                    RETURN_WITH_LOG(Res);

            IndicesL.push_back(*idxL);
            IndicesR.push_back(*idxR);
        }
    } else if (GEPL->getNumIndices() == 1 && GEPR->getNumIndices() == 1) {
        // If there is just a single (non-constant) index, it is an array
        // element access. We do not need to compare source element type since
        // members of the elements are not accessed here.
        // Just the index itself is compared.
        RETURN_WITH_LOG(cmpValues(GEPL->getOperand(1), GEPR->getOperand(1)));
    } else
        // Indices can't be compared by name, because they are not constant
        RETURN_WITH_LOG(OriginalResult);

    RETURN_WITH_LOG(0);
}

/// Ignore differences in attributes.
int DifferentialFunctionComparator::cmpAttrs(const AttributeList /*L*/,
                                             const AttributeList /*R*/) const {
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
    Function *CalledL = const_cast<Function *>(getCalledFunction(CL));
    Function *CalledR = const_cast<Function *>(getCalledFunction(CR));
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

    // Look for a macro-function difference.
    findMacroFunctionDifference(CL, CR);
}

/// Compare allocation instructions using separate cmpAllocs function in case
/// standard comparison returns something other than zero.
int DifferentialFunctionComparator::cmpOperations(
        const Instruction *L,
        const Instruction *R,
        bool &needToCmpOperands) const {
    // Do not compare PHI nodes yet. They will be compared at the end,
    // when all of their incoming values and blocks are already compared.
    if (isa<PHINode>(L) && isa<PHINode>(R)) {
        needToCmpOperands = false;
        return 0;
    }

    // Need to store comparing instructions in DifferingInstructions pair
    DifferingInstructions = {L, R};

    int Result = FunctionComparator::cmpOperations(L, R, needToCmpOperands);

    // Check whether the instruction is a call instruction.
    if (isa<CallInst>(L) || isa<CallInst>(R)) {
        if (isa<CallInst>(L) && isa<CallInst>(R)) {
            const CallInst *CL = dyn_cast<CallInst>(L);
            const CallInst *CR = dyn_cast<CallInst>(R);

            const Function *CalledL = getCalledFunction(CL);
            const Function *CalledR = getCalledFunction(CR);
            if (CalledL && CalledR) {
                if (CalledL->getName() == CalledR->getName()) {
                    // Check whether both instructions call an alloc function.
                    if (isAllocFunction(*CalledL)) {
                        if (config.Patterns.StructAlignment
                            && !cmpAllocs(CL, CR)) {
                            needToCmpOperands = false;
                            return 0;
                        }
                    }

                    if (CalledL->getIntrinsicID() == Intrinsic::memset
                        && CalledR->getIntrinsicID() == Intrinsic::memset) {
                        if (config.Patterns.StructAlignment
                            && !cmpMemset(CL, CR)) {
                            needToCmpOperands = false;
                            return 0;
                        }
                    }

                    if (Result && config.Patterns.ControlFlowOnly
                        && std::abs(static_cast<long>(CL->getNumOperands())
                                    - CR->getNumOperands())
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
                PREP_LOG("swap-branches", L, R);
                LOG_KEEP_FORCE(0);
                BranchInst *BranchNew = const_cast<BranchInst *>(BranchR);
                auto *tmpSucc = BranchNew->getSuccessor(0);
                BranchNew->setSuccessor(0, BranchNew->getSuccessor(1));
                BranchNew->setSuccessor(1, tmpSucc);
                return 0;
            }
        }
    }

    if (Result) {
        // Do not make difference between signed and unsigned when ignoring
        // type casts.
        if (config.Patterns.TypeCasts && isa<ICmpInst>(L) && isa<ICmpInst>(R)) {
            auto *ICmpL = dyn_cast<ICmpInst>(L);
            auto *ICmpR = dyn_cast<ICmpInst>(R);
            if (ICmpL->getUnsignedPredicate()
                == ICmpR->getUnsignedPredicate()) {
                return 0;
            }
        }
        // Handle alloca of a structure type with changed layout
        if (config.Patterns.StructAlignment && isa<AllocaInst>(L)
            && isa<AllocaInst>(R)) {
            StructType *TypeL = dyn_cast<StructType>(
                    dyn_cast<AllocaInst>(L)->getAllocatedType());
            StructType *TypeR = dyn_cast<StructType>(
                    dyn_cast<AllocaInst>(R)->getAllocatedType());
            if (TypeL && TypeR
                && TypeL->getStructName() == TypeR->getStructName())
#if LLVM_VERSION_MAJOR >= 14
                return cmpAligns(dyn_cast<AllocaInst>(L)->getAlign(),
                                 dyn_cast<AllocaInst>(R)->getAlign());
#else
                return cmpNumbers(dyn_cast<AllocaInst>(L)->getAlignment(),
                                  dyn_cast<AllocaInst>(R)->getAlignment());
#endif
        }
        // Record inverse conditions
        if (config.Patterns.InverseConditions && isa<CmpInst>(L)
            && isa<CmpInst>(R)) {
            auto CmpL = dyn_cast<CmpInst>(L);
            auto CmpR = dyn_cast<CmpInst>(R);

            // Only apply inverse condition pattern if the operands are of the
            // same type, i.e. cmpOperations returned non-0 because of the
            // values rather than the types.
            // It is sufficient to compare the predicates here since the
            // values of operands are compared in cmpBasicBlocks.
            if (cmpOperandTypes(CmpL, CmpR) == 0
                && CmpL->getPredicate() == CmpR->getInversePredicate()) {
                if (checkInverseCondUsers(L) && checkInverseCondUsers(R)) {
                    PREP_LOG("inv-cond-add", L, R);
                    LOG_KEEP_FORCE(0);

                    inverseConditions.emplace(L, R);
                    return 0;
                }
            }
        }
    }

    return Result;
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
        NameL = getCalledFunction(dyn_cast<CallInst>(L))->getName().str();
    if (isa<CallInst>(R))
        NameR = getCalledFunction(dyn_cast<CallInst>(R))->getName().str();

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

        LOG("Writing function-macro syntactic difference\n");

        std::unique_ptr<SyntaxDifference> diff =
                std::make_unique<SyntaxDifference>();
        diff->function = L->getFunction()->getName().str();
        diff->name = trueName;
        diff->BodyL = "[macro function difference]";
        diff->BodyR = "[macro function difference]";
        diff->StackL = CallStack{
                CallInfo{NameL,
                         L->getDebugLoc()->getFile()->getFilename().str(),
                         L->getDebugLoc()->getLine()}};
        diff->StackR = CallStack{
                CallInfo{NameR,
                         R->getDebugLoc()->getFile()->getFilename().str(),
                         R->getDebugLoc()->getLine()}};
        ModComparator->ComparedFuns.at({FnL, FnR})
                .addDifferingObject(std::move(diff));
    }
}

/// Compare an integer value with an LLVM constant
int DifferentialFunctionComparator::cmpIntWithConstant(
        uint64_t Integer, const Value *Const) const {
    if (!isa<ConstantInt>(Const))
        return 1;
    uint64_t ConstValue = dyn_cast<ConstantInt>(Const)->getZExtValue();
    return ConstValue != Integer;
}

/// Handle comparing of memory allocation function in cases where the size
/// of the composite type is different.
/// Note: this should only be called if
/// config.Patterns.HandleStructAlignment is true.
int DifferentialFunctionComparator::cmpAllocs(const CallInst *CL,
                                              const CallInst *CR) const {
    PREP_LOG("alloc", CL, CR);

    // Look whether the sizes for allocation match. If yes, then return zero
    // (ignore flags).
    if (cmpValues(CL->op_begin()->get(), CR->op_begin()->get()) == 0) {
        RETURN_WITH_LOG(0);
    }

    // Check if kzalloc has constant size of the allocated memory
    if (!isa<ConstantInt>(CL->getOperand(0))
        || !isa<ConstantInt>(CR->getOperand(0)))
        RETURN_WITH_LOG(1);

    // If the next instruction is a bitcast, compare its type instead
    const Value *ValL =
            isa<BitCastInst>(CL->getNextNode()) ? CL->getNextNode() : CL;
    const Value *ValR =
            isa<BitCastInst>(CR->getNextNode()) ? CR->getNextNode() : CR;

    // Retrieve type names and sizes
    TypeInfo TypeInfoL = getPointeeStructTypeInfo(ValL, &LayoutL);
    TypeInfo TypeInfoR = getPointeeStructTypeInfo(ValR, &LayoutR);

    // Compare the names and check if type sizes correspond with allocs
    RETURN_WITH_LOG(TypeInfoL.Name.empty() || TypeInfoR.Name.empty()
                    || TypeInfoL.Name.compare(TypeInfoR.Name)
                    || cmpIntWithConstant(TypeInfoL.Size, CL->getOperand(0))
                    || cmpIntWithConstant(TypeInfoR.Size, CR->getOperand(0)));
}

/// Check if the given instruction can be skipped because it does not
/// affect semantics or can be handled later. If yes, the instruction
/// is stored within the set/map of ignored, skipped or replaced
/// instructions (depending on the context).
bool DifferentialFunctionComparator::maySkipInstruction(
        const Instruction *Inst) const {
    if (isa<AllocaInst>(Inst)) {
        // An alloca can always be skipped without any replacement.
        // It can later be synchronized if used as an operand.
        skippedInstructions.insert(Inst);
        return true;
    }
    if (config.Patterns.GroupVars && Inst->isSafeToRemove()
        && Inst->user_begin() != Inst->user_end()
        && allUsersAreExtraMemInsts(Inst)) {
        // If this is a safe instruction (not a store, call, or a terminator),
        // it can be ignored if its users are extra memory instructions, i.e.,
        // stores to or loads from ignored local variables. This is useful for
        // skipping GEPs that return pointers to such variables.
        ignoredInstructions.insert(Inst);
        return true;
    }
    if (config.Patterns.ReorderedBinOps && isReorderableBinaryOp(Inst)
        && maySkipReorderableBinaryOp(Inst)) {
        ignoredInstructions.insert(Inst);
        return true;
    }
    if (isCast(Inst)) {
        if (config.Patterns.TypeCasts) {
            replacedInstructions.insert({Inst, Inst->getOperand(0)});
            return true;
        }
        return maySkipCast(Inst);
    }
    if (isZeroGEP(Inst)) {
        replacedInstructions.insert({Inst, Inst->getOperand(0)});
        return true;
    }
    if (auto Store = dyn_cast<StoreInst>(Inst)) {
        return maySkipStore(Store);
    }
    if (auto Load = dyn_cast<LoadInst>(Inst)) {
        return maySkipLoad(Load);
    }
    return false;
}

/// Check whether the given cast can be ignored (it does not affect
/// semantics. First operands of ignorable casts are stored as their
/// replacements inside the replaced instructions map.
bool DifferentialFunctionComparator::maySkipCast(const User *Cast) const {
    auto SrcTy = Cast->getOperand(0)->getType();
    auto DestTy = Cast->getType();

    if (auto StrTy = dyn_cast<StructType>(SrcTy)) {
        if (StrTy->hasName() && StrTy->getName().startswith("union")) {
            // Casting from a union type is safe to ignore, because in
            // case it was generated by something different than a
            // replacement of a simple type with a union, the
            // comparator would detect a difference when the value is
            // used, since it would have a different type.
            replacedInstructions.insert({Cast, Cast->getOperand(0)});
            return true;
        }
    }
    if (SrcTy->isPointerTy() && DestTy->isPointerTy()) {
        // Pointer cast is also safe to ignore - in case there will be
        // an instruction affected by this (i.e. a store, load or GEP),
        // there would be an additional difference that will be detected
        // later.
        replacedInstructions.insert({Cast, Cast->getOperand(0)});
        return true;
    }
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
        if (IntTySrc->getBitWidth() <= IntTyDest->getBitWidth()) {
            // Look for arithmetic operations in the uses of the cast
            // and in the uses of all values that are generated by
            // further casting.
            std::vector<const User *> UserStack;
            UserStack.push_back(Cast);
            while (!UserStack.empty()) {
                const User *U = UserStack.back();
                UserStack.pop_back();
                if (isa<BinaryOperator>(U)) {
                    return false;
                }
                if (isa<CastInst>(U)) {
                    for (const User *UU : U->users())
                        UserStack.push_back(UU);
                }
            }
            replacedInstructions.insert({Cast, Cast->getOperand(0)});
            return true;
        }
    }
    return false;
}

/// Check whether the given load may be skipped, i.e., it is either
/// a repetitive variant of a previous load, or a load from an ignored
/// local variable. Load replacements are stored inside the replaced
/// instructions map.
bool DifferentialFunctionComparator::maySkipLoad(const LoadInst *Load) const {
    auto BB = Load->getParent();
    if (!BB) {
        return false;
    }

    // Check if all backward CFG paths starting in the load lead to either:
    // - a single load from the same pointer,
    // - a single ignored store to the same pointer.
    // There must be no conflicting stores or calls on any of the paths.
    bool first = true;
    const Value *Replacement = nullptr;
    std::deque<const BasicBlock *> blockQueue = {BB};
    std::unordered_set<const BasicBlock *> visitedBlocks = {BB};
    while (!blockQueue.empty()) {
        BB = blockQueue.front();
        blockQueue.pop_front();

        // Look for a suitable load or store in the current block. If it's not
        // here, we will have to check all predecessors.
        bool checkPredecessors = true;
        for (auto it = BB->rbegin(); it != BB->rend(); ++it) {
            // Skip all instructions before the compared load when in the first
            // block (note that we are iterating in reverse).
            if (first) {
                if (&*it == Load) {
                    first = false;
                }
                continue;
            }

            const Value *ReplacementCandidate = nullptr;
            if (auto PreviousLoad = dyn_cast<LoadInst>(&*it)) {
                if (Load->getPointerOperand()
                    == PreviousLoad->getPointerOperand()) {
                    // Found an identical load, save the candidate.
                    ReplacementCandidate = PreviousLoad;
                }
            } else if (auto Store = dyn_cast<StoreInst>(&*it)) {
                if (ignoredInstructions.count(Store)
                    && Store->getPointerOperand()
                               == Load->getPointerOperand()) {
                    // Found a previously ignored store, save the candidate.
                    ReplacementCandidate = Store->getValueOperand();
                }
            }
            if (ReplacementCandidate) {
                if (Replacement) {
                    // If another suitable replacement has been found previously
                    // in a different CFG path, fail. We need a single one.
                    return false;
                }
                // Otherwise remember this replacement.
                Replacement = ReplacementCandidate;
                checkPredecessors = false;
                break;
            } else if (mayStoreTo(&*it, Load->getPointerOperand())) {
                // Fail if a possibly conflicting store or call is found.
                return false;
            }
        }

        if (checkPredecessors) {
            auto BBPredecessors = predecessors(BB);
            if (BBPredecessors.begin() == BBPredecessors.end()) {
                // Fail if no more predecessors are available. This means that
                // we've reached the top of the function, and we haven't found
                // a suitable replacement, so we cannot skip the load.
                return false;
            }
            // Queue up all unvisited predecessors.
            for (auto PredBB : BBPredecessors) {
                if (visitedBlocks.find(PredBB) == visitedBlocks.end()) {
                    visitedBlocks.insert(PredBB);
                    blockQueue.push_back(PredBB);
                }
            }
        }
    }
    // Successfully found a replacement,
    if (Replacement) {
        replacedInstructions.insert({Load, Replacement});
        return true;
    }
    return false;
}

/// Check whether the given store can be skipped because it stores to
/// an ignored local variable and the stored value will be compared later.
bool DifferentialFunctionComparator::maySkipStore(
        const StoreInst *Store) const {
    if (!config.Patterns.GroupVars) {
        return false;
    }
    // We can skip a store if it stores to a skipped/ignored local variable.
    // This is allowed because any load from the same variable will
    // also fail, and will have to be skipped and replaced with the
    // stored value. As a result, the corresponding alloca cannot be
    // directly compared as an operand from this point onward, because
    // its contents may differ from now. Hence, we move it from
    // skippedInstructions to ignoredInstructions.
    auto Alloca = getAllocaOp(Store);
    if (!Alloca) {
        return false;
    }
    auto SkippedIt = skippedInstructions.find(Alloca);
    if (SkippedIt != skippedInstructions.end()) {
        skippedInstructions.erase(SkippedIt);
        ignoredInstructions.insert(Alloca);
        ignoredInstructions.insert(Store);
        return true;
    }
    if (ignoredInstructions.count(Alloca)) {
        ignoredInstructions.insert(Store);
        return true;
    }
    return false;
}

/// Check whether the given reorderable binary operator can be skipped.
/// It can only be skipped if all its users are binary operations
/// of the same kind.
bool DifferentialFunctionComparator::maySkipReorderableBinaryOp(
        const Instruction *Op) const {
    return std::all_of(Op->user_begin(), Op->user_end(), [Op](auto user) {
        auto userOp = dyn_cast<BinaryOperator>(user);
        return userOp && userOp->getOpcode() == Op->getOpcode();
    });
}

bool mayIgnoreMacro(std::string macro) {
    return ignoredMacroList.find(macro) != ignoredMacroList.end();
}

/// Retrieve the replacement for the given value from the replaced instructions
/// map. Try to generate the replacement if a bitcast is given.
const Value *DifferentialFunctionComparator::getReplacementValue(
        const Value *Replaced, DenseMap<const Value *, int> &sn_map) const {
    // Find the replacement value.
    auto replacementIt = replacedInstructions.find(Replaced);
    if (replacementIt == replacedInstructions.end()) {
        // Before failing, check whether the replaced value is an ignorable
        // bitcast or zero GEP operator. If so, return its operand. Note that if
        // the replacing operand is an instruction, it must already have been
        // synchronized, otherwise an invalid synchronisation could be created
        // in cmpValues.
        Value *Result = nullptr;
        if (auto BitCast = dyn_cast<BitCastOperator>(Replaced)) {
            if (maySkipCast(BitCast))
                Result = BitCast->getOperand(0);
        }
        if (auto GEP = dyn_cast<GEPOperator>(Replaced)) {
            if (isZeroGEP(GEP))
                Result = GEP->getOperand(0);
        }
        if ((Result && !isa<Instruction>(Result))
            || sn_map.find(Result) != sn_map.end())
            return Result;
        return nullptr;
    }
    return replacementIt->second;
}

/// Creates new value mappings according to the current pattern match.
void DifferentialFunctionComparator::createPatternMapping() const {
    for (auto &&MappedInstPair : CustomPatternComp.InstMappings) {
        // If the instructions are already mapped, do not map them again.
        if (sn_mapL.find(MappedInstPair.first) != sn_mapL.end()
            || sn_mapR.find(MappedInstPair.second) != sn_mapR.end())
            continue;

        mappedValuesBySn[sn_mapL.size()] = {MappedInstPair.first,
                                            MappedInstPair.second};
        sn_mapL.try_emplace(MappedInstPair.first, sn_mapL.size());
        sn_mapR.try_emplace(MappedInstPair.second, sn_mapR.size());
    }
}

/// Check if the given instruction has been matched to a pattern and,
/// therefore, does not need to be analyzed nor mapped again.
bool DifferentialFunctionComparator::isPartOfPattern(
        const Instruction *Inst) const {
    return CustomPatternComp.AllInstMatches.find(Inst)
           != CustomPatternComp.AllInstMatches.end();
}

/// Undo the changes made to synchronisation maps during the last
/// instruction pair comparison.
void DifferentialFunctionComparator::undoLastInstCompare(
        BasicBlock::const_iterator &InstL,
        BasicBlock::const_iterator &InstR) const {
    sn_mapL.erase(&*InstL);
    sn_mapR.erase(&*InstR);
    mappedValuesBySn.erase(sn_mapL.size());
    ComparedInstL--;
    ComparedInstR--;
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
    const Function *CFL = getCalledFunction(CIL);
    const Function *CFR = getCalledFunction(CIR);
    const BasicBlock *BBL = CIL->getParent();
    const BasicBlock *BBR = CIR->getParent();

    // Use the appropriate C source call argument extraction function depending
    // on whether it is an inline assembly call or not.
    if (CFL->getName().startswith(SimpllInlineAsmPrefix))
        CArgsL =
                findInlineAssemblySourceArguments(CIL->getDebugLoc(),
                                                  getInlineAsmString(CFL).str(),
                                                  &ModComparator->MacroDiffs);
    else
        CArgsL = findFunctionCallSourceArguments(CIL->getDebugLoc(),
                                                 CFL->getName().str(),
                                                 &ModComparator->MacroDiffs);

    if (CFR->getName().startswith(SimpllInlineAsmPrefix))
        CArgsR =
                findInlineAssemblySourceArguments(CIR->getDebugLoc(),
                                                  getInlineAsmString(CFR).str(),
                                                  &ModComparator->MacroDiffs);
    else
        CArgsR = findFunctionCallSourceArguments(CIR->getDebugLoc(),
                                                 CFL->getName().str(),
                                                 &ModComparator->MacroDiffs);

    if ((CArgsL.size() > i) && (CArgsR.size() > i)) {
        if (mayIgnoreMacro(CArgsL[i]) && mayIgnoreMacro(CArgsR[i])
            && (CArgsL[i] == CArgsR[i])) {
            LOG("Comparing integers as equal because of "
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

            if (SizeL != ModComparator->StructSizeMapL.end()
                && SizeR != ModComparator->StructSizeMapR.end()
                && SizeL->second == SizeR->second) {
                LOG("Comparing integers as equal because of "
                    << "correspondence to structure type sizes\n");
                return 0;
            }

            // Extract the identifiers inside sizeofs
            auto IdL = getSubstringToMatchingBracket(CArgsL[i], 6);
            auto IdR = getSubstringToMatchingBracket(CArgsR[i], 6);
            IdL = IdL.substr(1, IdL.size() - 2);
            IdR = IdR.substr(1, IdR.size() - 2);

            const DIType *DITypeL = getCSourceIdentifierType(
                    IdL, BBL->getParent(), DI->LocalVariableMapL);
            const DIType *DITypeR = getCSourceIdentifierType(
                    IdR, BBR->getParent(), DI->LocalVariableMapR);

            if (DITypeL && DITypeR
                && DITypeL->getName() == DITypeR->getName()) {
                LOG("Comparing integers as equal because of "
                    << "correspondence of structure names\n");
                return 0;
            }
        }
    }
    return 1;
}

/// Note: Parts of this function have been adapted from FunctionComparator.
/// Therefore, LLVM licensing also applies here. See the LICENSE information
/// in the appropriate llvm-lib subdirectory for more details.
int DifferentialFunctionComparator::cmpBasicBlocks(
        const BasicBlock *BBL, const BasicBlock *BBR) const {
    BasicBlock::const_iterator InstL = BBL->begin();
    BasicBlock::const_iterator InstR = BBR->begin();
    return cmpBasicBlocksFromInstructions(BBL, BBR, InstL, InstR);
}

/// Compare basic blocks from the specified instructions.
int DifferentialFunctionComparator::cmpBasicBlocksFromInstructions(
        const llvm::BasicBlock *BBL,
        const llvm::BasicBlock *BBR,
        BasicBlock::const_iterator InstL,
        BasicBlock::const_iterator InstR,
        bool suppressRelocationsAndSMT) const {
    BasicBlock::const_iterator InstLE = BBL->end();
    BasicBlock::const_iterator InstRE = BBR->end();

    while (InstL != InstLE && InstR != InstRE) {
        if (isDebugInfo(*InstL)) {
            InstL++;
            continue;
        }
        if (isDebugInfo(*InstR)) {
            InstR++;
            continue;
        }

        // Skip instructions matched to a pattern because such instructions have
        // been analyzed by the pattern function comparator and have already
        // been mapped according to the pattern.
        if (isPartOfPattern(&*InstL) || isPartOfPattern(&*InstR)) {
            while (InstL != InstLE && isPartOfPattern(&*InstL)) {
                InstL++;
                ComparedInstL++;
            }
            while (InstR != InstRE && isPartOfPattern(&*InstR)) {
                InstR++;
                ComparedInstR++;
            }
            continue;
        }

        if ((&InstL->getDebugLoc())->get())
            CurrentLocL = &InstL->getDebugLoc();
        else
            CurrentLocL = nullptr;
        if ((&InstR->getDebugLoc())->get())
            CurrentLocR = &InstR->getDebugLoc();
        else
            CurrentLocR = nullptr;
        if (config.ExtendedStat) {
            if (CurrentLocL && InstL->getDebugLoc().get())
                ComparedLinesL.emplace(
                        InstL->getDebugLoc()->getFilename().str(),
                        InstL->getDebugLoc().getLine());
            if (CurrentLocR && InstR->getDebugLoc().get())
                ComparedLinesR.emplace(
                        InstR->getDebugLoc()->getFilename().str(),
                        InstR->getDebugLoc()->getLine());
        }

        if (int Res = cmpOperationsWithOperands(&*InstL, &*InstR)) {
            // Detect a difference caused by a field access change that does
            // not affect semantics.
            // Note: this has to be done here, because cmpFieldAccess moves the
            // instruction iterators to the end of the field access if the
            // field accesses are equal.
            if (Res && isa<GetElementPtrInst>(&*InstL)
                && isa<GetElementPtrInst>(&*InstR)) {
                if (cmpFieldAccess(InstL, InstR) == 0)
                    continue;
            }

            // Some operations not affecting semantics and control flow may be
            // ignored (currently allocas and casts). This may help to handle
            // some small changes that do not affect semantics (it is also
            // useful in combination with function inlining). If such an
            // operation is detected, reset serial counters, skip the ignored
            // operation, and repeat the comparison.
            bool MaySkipL = maySkipInstruction(&*InstL);
            bool MaySkipR = maySkipInstruction(&*InstR);
            if (MaySkipL || MaySkipR) {
                undoLastInstCompare(InstL, InstR);
                if (MaySkipL)
                    InstL++;
                if (MaySkipR)
                    InstR++;
                continue;
            }

            // If both instructions are reorderable binary operators, they may
            // have skipped operands. Try to recursively collect all operands
            // and compare their sets.
            if (config.Patterns.ReorderedBinOps
                && isReorderableBinaryOp(&*InstL)
                && isReorderableBinaryOp(&*InstR)) {
                Instruction::BinaryOps OpcodeL =
                        dyn_cast<BinaryOperator>(&*InstL)->getOpcode();
                Instruction::BinaryOps OpcodeR =
                        dyn_cast<BinaryOperator>(&*InstR)->getOpcode();
                std::multiset<int> SnSetL, SnSetR;
                std::multiset<int64_t> ConstantsSetL, ConstantsSetR;
                if (OpcodeL == OpcodeR
                    && collectBinaryOperands(
                            &*InstL, OpcodeL, SnSetL, ConstantsSetL, sn_mapL)
                    && collectBinaryOperands(
                            &*InstR, OpcodeR, SnSetR, ConstantsSetR, sn_mapR)
                    && SnSetL == SnSetR && ConstantsSetL == ConstantsSetR) {
                    InstL++;
                    InstR++;
                    continue;
                }
            }

            // If one of the instructions is a logical not, it is possible that
            // it will be used in an inverse condition. Hence, we skip it here
            // and mark that it may be inverse-matching the condition that
            // has been previously mapped to the operand of the not operation.
            if (config.Patterns.InverseConditions
                && ((isLogicalNot(&*InstL) && checkInverseCondUsers(&*InstL))
                    || (isLogicalNot(&*InstR)
                        && checkInverseCondUsers(&*InstR)))) {
                // Note: This does not handle cases with a lot of `not`:
                // %1 = cmp true, false | %1 = cmp true, false
                //                      | %2 = not %1 (%2 = xor %1, true)
                //                      | %3 = not %2 (%3 = xor %2, true)
                //                      | %4 = not %3 (%4 = xor %3, true)
                // br %1, l %T, l %F    | br %4, l %F, l %T

                undoLastInstCompare(InstL, InstR);
                // Pair of left and right instructions which could be
                // potentionally used for branching in program.
                // The pair could look like this: L cmp ... | R 'not' ...
                std::pair<const Value *, const Value *> condPair;
                // Previous pair of instructions which was believed to be
                // used for branching. One of the current instructions
                // (the not/xor instruction) uses one of the instruction
                // from the prevCondPair.
                std::pair<const Value *, const Value *> prevCondPair;
                if (isLogicalNot(&*InstL)) {
                    condPair = {&*InstL,
                                getMappedValue(InstL->getOperand(0), true)};
                    prevCondPair = {InstL->getOperand(0),
                                    getMappedValue(InstL->getOperand(0), true)};
                    replacedInstructions.emplace(&*InstL, InstL->getOperand(0));
                    InstL++;
                    ComparedInstL++;
                } else {
                    condPair = {getMappedValue(InstR->getOperand(0), false),
                                &*InstR};
                    prevCondPair = {getMappedValue(InstR->getOperand(0), false),
                                    InstR->getOperand(0)};
                    replacedInstructions.emplace(&*InstR, InstR->getOperand(0));
                    InstR++;
                    ComparedInstR++;
                }

                // If the previous pair already had inverse instructions,
                // remove it from the set. Otherwise, add the current pair.
                size_t erased = inverseConditions.erase(prevCondPair);
                if (!erased)
                    inverseConditions.insert(condPair);

                LOG_VERBOSE_EXTRA((erased ? "Removed" : " Added")
                                  << " inverse condition\n");
                auto &pairToLog = (erased) ? prevCondPair : condPair;
                LOG_INDENT();
                LOG_VERBOSE_EXTRA("L: " << (*(pairToLog.first)) << "\n");
                LOG_VERBOSE_EXTRA("R: " << (*(pairToLog.second)) << "\n");
                LOG_UNINDENT();

                continue;
            }

            if (Reloc.status == RelocationInfo::Stored) {
                // If there is an inequality found and we have previously found
                // a possibly relocated block, try to match it now.
                Reloc.status = RelocationInfo::Matching;
                LOG("Try to match the relocated block\n");
                // The instructions are not equal
                undoLastInstCompare(InstL, InstR);
                // Move instruction in the module that contains the relocated
                // block to the block beginning and re-run the comparison.
                // Also, backup the moved instruction so that we know where to
                // restore the comparison from after the block is matched.
                if (Reloc.prog == Program::First) {
                    Reloc.restore = InstL;
                    InstL = Reloc.begin;
                    continue;
                } else {
                    Reloc.restore = InstR;
                    InstR = Reloc.begin;
                    continue;
                }
            }

            // Try to find the source of the difference.
            // Note that below are two more ways of finding function equality
            // (custom pattern matching and relocation comparison). However,
            // if these fail, we need the information found by this function,
            // hence we run it now. If the equality is shown in the end, the
            // found differences will be just ignored.
            if (Res)
                findDifference(&*InstL, &*InstR);

            // The difference cannot be skipped. Try to match it to one of the
            // loaded difference patterns. Continue the comparison if a suitable
            // starting pattern match gets found.
            if (CustomPatternComp.matchPattern(&*InstL, &*InstR)) {
                undoLastInstCompare(InstL, InstR);
                createPatternMapping();
                continue;
            }

            // Try to find a match by moving one of the instruction iterators
            // forward (find a code relocation).
            if (config.Patterns.Relocations && !suppressRelocationsAndSMT
                && Reloc.status == RelocationInfo::None) {
                if (findMatchingOpWithOffset(InstL, InstR, Program::Second)
                    || findMatchingOpWithOffset(InstL, InstR, Program::First)) {
                    Res = 0;
                }
            }

            if (Res && config.UseSMT && !suppressRelocationsAndSMT)
                Res = SMTComparator->compare(InstL, InstR);

            if (Res)
                return Res;
        } else {
            InstEqual++;

            if (Reloc.status == RelocationInfo::Stored) {
                // If there is a dependency between the skipped instruction and
                // the relocated code, fail the comparison
                if (Reloc.prog == Program::First && isDependingOnReloc(*InstL))
                    return 1;
                if (Reloc.prog == Program::Second && isDependingOnReloc(*InstR))
                    return 1;
            }
            if (Reloc.status == RelocationInfo::Matching) {
                // If the relocated code has been entirely matched, we can
                // continue from the restore point.
                if (Reloc.prog == Program::First && InstL == Reloc.end) {
                    LOG("Relocated block matched\n");
                    InstL = Reloc.restore;
                    InstR++;
                    Reloc.status = RelocationInfo::None;
                    continue;
                } else if (Reloc.prog == Program::Second
                           && InstR == Reloc.end) {
                    LOG("Relocated block matched\n");
                    InstL++;
                    InstR = Reloc.restore;
                    Reloc.status = RelocationInfo::None;
                    continue;
                }
            }
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
    auto FunL = getCalledFunction(IL);
    auto FunR = getCalledFunction(IR);
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

        for (unsigned i = 0; i < I->arg_size(); i++) {
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
    diff->StackL.push_back(
            CallInfo{"(generated assembly code)",
                     ParentL->getSubprogram()->getFilename().str(),
                     ParentL->getSubprogram()->getLine()});
    diff->StackR = CallStack();
    diff->StackR.push_back(
            CallInfo{"(generated assembly code)",
                     ParentR->getSubprogram()->getFilename().str(),
                     ParentR->getSubprogram()->getLine()});
    diff->function = ParentL->getName().str();
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
    PREP_LOG("global value", L, R);
    // We got here from a GV initializer list of this variable, skip the
    // comparison.
    if (assumedEqual.count(std::make_pair(L, R)))
        return 0;

    auto GVarL = dyn_cast<GlobalVariable>(L);
    auto GVarR = dyn_cast<GlobalVariable>(R);

    if (GVarL && GVarR && GVarL->hasInitializer() && GVarR->hasInitializer()
        && GVarL->isConstant() && GVarR->isConstant()) {
        // Constant global variables are compared using their initializers.
        // Some initializers may refer back to the global variables, therefore
        // we have to assume equality of the global variable to prevent infinite
        // recursion.
        assumedEqual.insert(std::make_pair(L, R));
        int r = cmpConstants(GVarL->getInitializer(), GVarR->getInitializer());
        assumedEqual.erase(std::make_pair(L, R));
        RETURN_WITH_LOG_NEQ(r);
    } else if (L->hasName() && R->hasName()) {
        // Both values are named, compare them by names
        auto NameL = L->getName();
        auto NameR = R->getName();

        // Remove number suffixes
        if (hasSuffix(NameL.str()))
            NameL = NameL.substr(0, NameL.find_last_of("."));
        if (hasSuffix(NameR.str()))
            NameR = NameR.substr(0, NameR.find_last_of("."));
        if (NameL == NameR
            || (isKernelPrintFunction(NameL.str())
                && isKernelPrintFunction(NameR.str()))) {
            if (isa<Function>(L) && isa<Function>(R)) {
                // Functions compared as being the same have to be also compared
                // by ModuleComparator.
                auto FunL = dyn_cast<Function>(L);
                auto FunR = dyn_cast<Function>(R);

                // Do not compare SimpLL abstractions and intrinsic functions.
                if (!isSimpllAbstraction(FunL) && !isSimpllAbstraction(FunR)
                    && !isKernelPrintFunction(L->getName().str())
                    && !isKernelPrintFunction(R->getName().str())
                    && !FunL->isIntrinsic() && !FunR->isIntrinsic()) {
                    // Store the called functions into the current
                    // functions' callee set.
                    ModComparator->ComparedFuns.at({FnL, FnR})
                            .First.addCall(FunL,
                                           CurrentLocL ? CurrentLocL->getLine()
                                                       : 0);
                    ModComparator->ComparedFuns.at({FnL, FnR})
                            .Second.addCall(FunR,
                                            CurrentLocR ? CurrentLocR->getLine()
                                                        : 0);
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

                    RETURN_WITH_LOG_NEQ(
                            !(asmL == asmR && constraintL == constraintR));
                }
            }
            RETURN_WITH_LOG_NEQ(0);

        } else if (NameL != NameR && GVarL && GVarR && GVarL->isConstant()
                   && GVarR->isConstant() && !GVarL->hasInitializer()
                   && !GVarR->hasInitializer()) {
            // Externally defined constants (those without initializer
            // and with different names) need to have their definitions linked.
            ModComparator->MissingDefs.push_back({GVarL, GVarR});
            RETURN_WITH_LOG_NEQ(1);
        }

        else
            RETURN_WITH_LOG_NEQ(1);
    } else
        RETURN_WITH_LOG_NEQ(L != R);
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

/// Finds all differences between source types in GEPs inside two field access
/// operations and records them using findTypeDifference.
void DifferentialFunctionComparator::findTypeDifferences(
        const GetElementPtrInst *FAL,
        const GetElementPtrInst *FAR,
        const Function *L,
        const Function *R) const {
    std::vector<Type *> SrcTypesL = getFieldAccessSourceTypes(FAL);
    std::vector<Type *> SrcTypesR = getFieldAccessSourceTypes(FAR);
    for (unsigned long i = 0; i < std::min(SrcTypesL.size(), SrcTypesR.size());
         i++) {
        StructType *STyL = dyn_cast<StructType>(SrcTypesL[i]);
        StructType *STyR = dyn_cast<StructType>(SrcTypesR[i]);
        if (!STyL || !STyR)
            continue;
        if (!STyL->hasName() || !STyR->hasName())
            continue;
        std::string STyLShortName = hasSuffix(STyL->getName().str())
                                            ? dropSuffix(STyL->getName().str())
                                            : STyL->getName().str();
        std::string STyRShortName = hasSuffix(STyR->getName().str())
                                            ? dropSuffix(STyR->getName().str())
                                            : STyR->getName().str();
        if (STyLShortName != STyRShortName)
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
        diff->name = L->getName().startswith("struct.")
                             ? L->getName().substr(7).str()
                             : L->getName().str();

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

        diff->function = FL->getName().str();
        diff->FileL = joinPath(DCTyL->getDirectory(), DCTyL->getFilename());
        diff->FileR = joinPath(DCTyR->getDirectory(), DCTyR->getFilename());
        // Note: for some reason the starting line of the struct in the debug
        // info is the first attribute, skipping the actual declaration.
        // This is fixed by decrementing the line number.
        diff->LineL = DCTyL->getLine() - 1;
        diff->LineR = DCTyR->getLine() - 1;
        diff->StackL.push_back(
                CallInfo{diff->name + " (type)",
                         FL->getSubprogram()->getFilename().str(),
                         FL->getSubprogram()->getLine()});
        diff->StackR = CallStack();
        diff->StackR.push_back(
                CallInfo{diff->name + " (type)",
                         FR->getSubprogram()->getFilename().str(),
                         FR->getSubprogram()->getLine()});
        ModComparator->ComparedFuns.at({FnL, FnR})
                .addDifferingObject(std::move(diff));
    }
}

/// Specific comparing of sequences of field accesses.
/// Note: this should only be called if
/// config.Patterns.HandleStructAlignment is true.
int DifferentialFunctionComparator::cmpFieldAccess(
        BasicBlock::const_iterator &InstL,
        BasicBlock::const_iterator &InstR) const {
    // Compare the complete offset of an unbroken sequence of GEP and bitcast
    // in each module, starting from InstL and InstR, respectively.
    // It is possible that although the process contains unpacking several
    // anonymous unions and structs, the offset stays the same, which means that
    // without a doubt this is semantically equal (and probably would, in fact,
    // be also equal in machine code).

    PREP_LOG("field access", &*InstL, &*InstR);

    // Backup the instruction iterators - if the comparison is not successful,
    // we'll have to restore them.
    auto FirstInstL = InstL;
    auto FirstInstR = InstR;

    auto GEPL = dyn_cast<GetElementPtrInst>(InstL);
    auto GEPR = dyn_cast<GetElementPtrInst>(InstR);

    if (!(GEPL && GEPR))
        RETURN_WITH_LOG(1);

    if (!GEPL->hasAllConstantIndices() || !GEPR->hasAllConstantIndices())
        RETURN_WITH_LOG(1);

    const Value *PtrL = GEPL->getOperand(0);
    const Value *PtrR = GEPR->getOperand(0);

    if (int CmpPtrs = cmpValues(PtrL, PtrR))
        RETURN_WITH_LOG(CmpPtrs);

    uint64_t OffsetL = 0, OffsetR = 0;
    bool l_end = false, r_end = false;
    while (!l_end || !r_end) {
        int offset = 0;
        if (!l_end && isConstantMemoryAccessToPtr(&*InstL, PtrL, offset)) {
            OffsetL += offset;
            PtrL = &*InstL;
            InstL++;
        } else {
            l_end = true;
        }

        if (!r_end && isConstantMemoryAccessToPtr(&*InstR, PtrR, offset)) {
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
        RETURN_WITH_LOG(0);
    }

    // Restore instruction iterators to their original values
    InstL = FirstInstL;
    InstR = FirstInstR;
    RETURN_WITH_LOG(1);
}

/// Handle values generated from macros and enums whose value changed.
/// The new values are pre-computed by DebugInfo.
/// Also handles ignoring of type casts - when this feature is turned on, it
/// compares the original value instead of the cast.
int DifferentialFunctionComparator::cmpValues(const Value *L,
                                              const Value *R) const {
    PREP_LOG("value", L, R);

    // If both values are reversibly skipped, we can try to compare them
    // fully - as instructions. This can help match reordered allocas.
    auto SkippedEnd = skippedInstructions.end();
    auto SkippedItL = skippedInstructions.find(L);
    auto SkippedItR = skippedInstructions.find(R);
    if (SkippedItL != SkippedEnd && SkippedItR != SkippedEnd) {
        auto InstL = dyn_cast<Instruction>(*SkippedItL);
        auto InstR = dyn_cast<Instruction>(*SkippedItR);
        // The instructions must not be marked as skipped now, because
        // comparison of operations calls cmpValues immediately again.
        skippedInstructions.erase(SkippedItL);
        skippedInstructions.erase(SkippedItR);
        if (!cmpOperationsWithOperands(InstL, InstR)) {
            RETURN_WITH_LOG_NEQ(0);
        }
        // If the comparison failed, restore the ignored instructions map
        // and synchronization maps, which are polluted.
        skippedInstructions.insert(InstL);
        skippedInstructions.insert(InstR);
        sn_mapL.erase(InstL);
        sn_mapR.erase(InstR);
        mappedValuesBySn.erase(sn_mapL.size());
        RETURN_WITH_LOG_NEQ(1);
    }

    // If either of the values is irreversibly ignored, or if only one of
    // the instructions is skipped, fail.
    if (ignoredInstructions.count(L) || ignoredInstructions.count(R)
        || SkippedItL != SkippedEnd || SkippedItR != SkippedEnd) {
        RETURN_WITH_LOG_NEQ(1);
    }

    // Use replacements for other ignored values, bitcasts, and zero GEPs.
    auto replaceL = getReplacementValue(L, sn_mapL);
    auto replaceR = getReplacementValue(R, sn_mapR);
    if (replaceL || replaceR) {
        // Repeat the comparison with replacements for all ignored values.
        RETURN_WITH_LOG_NEQ(
                cmpValues(replaceL ? replaceL : L, replaceR ? replaceR : R));
    }

    auto oldSnMapSize = sn_mapL.size();
    int result = FunctionComparator::cmpValues(L, R);
    if (result) {
        if (isa<Constant>(L) && isa<Constant>(R)) {
            auto *ConstantL = dyn_cast<Constant>(L);
            auto *ConstantR = dyn_cast<Constant>(R);
            auto MacroMapping = DI->MacroConstantMap.find(ConstantL);
            if (MacroMapping != DI->MacroConstantMap.end()
                && MacroMapping->second == valueAsString(ConstantR))
                RETURN_WITH_LOG_NEQ(0);
        } else if (isa<BasicBlock>(L) && isa<BasicBlock>(R)) {
            // In case functions have different numbers of BBs, they may be
            // compared as unequal here. However, this can be caused by moving
            // part of the functionality into a function and hence we'll
            // treat the BBs as equal here to continue comparing and maybe
            // try inlining.
            // We also need to remove a BB that was newly inserted in cmpValues
            // since the serial maps would not be synchronized otherwise.
            if (sn_mapL.size() != sn_mapR.size()) {
                if ((unsigned long)sn_mapL[L] == (sn_mapL.size() - 1))
                    sn_mapL.erase(L);
                if ((unsigned long)sn_mapR[R] == (sn_mapR.size() - 1))
                    sn_mapR.erase(R);
            }
            RETURN_WITH_LOG_NEQ(0);
        }
        if (CustomPatternComp.matchValues(L, R)) {
            // If the values correspond to a value pattern, consider them equal.
            RETURN_WITH_LOG_NEQ(0);
        }
    } else if (oldSnMapSize == (sn_mapL.size() - 1)) {
        // When the values have just been added to the serial number maps,
        // remember their mapping.
        mappedValuesBySn[oldSnMapSize] = {L, R};
        if (isa<PHINode>(L) && isa<PHINode>(R)) {
            // Store PHI nodes for further comparison.
            phisToCompare.emplace_back(dyn_cast<PHINode>(L),
                                       dyn_cast<PHINode>(R));
        } else if (isa<PHINode>(L) || isa<PHINode>(R)) {
            // A PHI instruction cannot be equal to a non-PHI instruction.
            result = 1;
        }
    }
    RETURN_WITH_LOG_NEQ(result);
}

/// Specific comparing of constants. If one of them (or both) is a cast
/// constant expression and ignoring casts is turned on, compare its operand.
int DifferentialFunctionComparator::cmpConstants(const Constant *L,
                                                 const Constant *R) const {
    int Result = FunctionComparator::cmpConstants(L, R);

    if (Result == 0)
        return Result;

    if (config.Patterns.TypeCasts) {
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

/// Specific comparison of unions, integer, and array types when specific
/// semantic patterns are allowed.
int DifferentialFunctionComparator::cmpTypes(Type *L, Type *R) const {
    PREP_LOG("type", L, R);

    // Compare union as equal to another type in case it is at least of the same
    // size.
    // Note: a union type is represented in Clang-generated LLVM IR by a
    // structure type.
    if (config.Patterns.StructAlignment
        && (L->isStructTy() || R->isStructTy())) {
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
            RETURN_WITH_LOG_NEQ(0);
        }
    }

    // Compare integer types (except the boolean type) as the same when
    // ignoring the type casts.
    if (config.Patterns.TypeCasts && L->isIntegerTy() && R->isIntegerTy()) {
        if (L->getIntegerBitWidth() == 1 || R->getIntegerBitWidth() == 1)
            RETURN_WITH_LOG_NEQ(
                    !(L->getIntegerBitWidth() == R->getIntegerBitWidth()));
        RETURN_WITH_LOG_NEQ(0);
    }

    if (!config.Patterns.TypeCasts || !L->isArrayTy() || !R->isArrayTy())
        RETURN_WITH_LOG_NEQ(FunctionComparator::cmpTypes(L, R));

    ArrayType *AL = dyn_cast<ArrayType>(L);
    ArrayType *AR = dyn_cast<ArrayType>(R);

    RETURN_WITH_LOG_NEQ(cmpTypes(AL->getElementType(), AR->getElementType()));
}

/// Do not compare integer bitwidth when ignoring the type casts.
int DifferentialFunctionComparator::cmpAPInts(const APInt &L,
                                              const APInt &R) const {
    int Result = FunctionComparator::cmpAPInts(L, R);
    if (!config.Patterns.TypeCasts || !Result) {
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
/// Note: this should only be called if
/// config.Patterns.HandleStructAlignment is true.
int DifferentialFunctionComparator::cmpMemset(const CallInst *CL,
                                              const CallInst *CR) const {
    PREP_LOG("memset", CL, CR);

    // Compare all except the third operand (size to set).
    for (unsigned i = 0; i < CL->arg_size(); ++i) {
        if (i == 2)
            continue;
        if (int Res = cmpValues(CL->getArgOperand(i), CR->getArgOperand(i)))
            RETURN_WITH_LOG(Res);
    }

    // If the structure sizes are equal, we can end right away
    if (!cmpValues(CL->getArgOperand(2), CR->getArgOperand(2)))
        RETURN_WITH_LOG(0);

    // Get the destination pointers
    const Value *destL = CL->getArgOperand(0);
    const Value *destR = CR->getArgOperand(0);

    // If the destination is a bitcast, compare the original source value
    const Value *ValL = isa<BitCastInst>(destL)
                                ? dyn_cast<BitCastInst>(destL)->getOperand(0)
                                : destL;
    const Value *ValR = isa<BitCastInst>(destR)
                                ? dyn_cast<BitCastInst>(destR)->getOperand(0)
                                : destR;

    TypeInfo TypeInfoL = getPointeeStructTypeInfo(ValL, &LayoutL);
    TypeInfo TypeInfoR = getPointeeStructTypeInfo(ValR, &LayoutR);

    // Return 0 (equality) if both memory destinations are structs of the same
    // name and each memset size is equal to the corresponding struct size.

    RETURN_WITH_LOG(TypeInfoL.Name.empty() || TypeInfoR.Name.empty()
                    || TypeInfoL.Name.compare(TypeInfoR.Name)
                    || cmpIntWithConstant(TypeInfoL.Size, CL->getOperand(2))
                    || cmpIntWithConstant(TypeInfoR.Size, CR->getOperand(2)));
}

/// Comparing PHI instructions.
/// Handle different order of incoming values - for each incoming value-block
/// pair, try to find a matching pair in the other PHI instruction.
/// In order to work properly, all the incoming values and blocks should already
/// be analysed.
int DifferentialFunctionComparator::cmpPHIs(const PHINode *PhiL,
                                            const PHINode *PhiR) const {
    PREP_LOG("PHI", PhiL, PhiR);

    if (PhiL->getNumIncomingValues() != PhiR->getNumIncomingValues())
        RETURN_WITH_LOG_NEQ(1);
    for (unsigned i = 0; i < PhiL->getNumIncomingValues(); ++i) {
        bool match = false;
        for (unsigned j = 0; j < PhiR->getNumIncomingValues(); ++j) {
            auto BBL_sn = sn_mapL[PhiL->getIncomingBlock(i)];
            auto BBR_sn = sn_mapR[PhiR->getIncomingBlock(j)];
            if (BBL_sn == BBR_sn
                && cmpValues(PhiL->getIncomingValue(i),
                             PhiR->getIncomingValue(j))
                           == 0) {
                match = true;
                break;
            }
        }
        if (!match)
            RETURN_WITH_LOG_NEQ(1);
    }
    RETURN_WITH_LOG_NEQ(0);
}

/// Compare two instructions along with their operands.
int DifferentialFunctionComparator::cmpOperationsWithOperands(
        const Instruction *L, const Instruction *R) const {
    PREP_LOG("instruction", L, R);

    ComparedInstR++;
    ComparedInstL++;

    // Contains code copied out of the original cmpBasicBlocks since it is more
    // convenient to have the code in a separate function.
    bool needToCmpOperands = true;
    if (int Res = cmpOperations(L, R, needToCmpOperands)) {
        auto *CallL = dyn_cast<CallInst>(L);
        auto *CallR = dyn_cast<CallInst>(R);
        if (CallL || CallR) {
            auto *CalledL = CallL ? getCalledFunction(CallL) : nullptr;
            auto *CalledR = CallR ? getCalledFunction(CallR) : nullptr;
            if (!(CalledL && CalledR)) {
                // If just one of the instructions is a call, it is possible
                // that some logic has been moved into a function. We'll try to
                // inline that function and compare again.
                if (CalledL && !isSimpllAbstractionDeclaration(CalledL))
                    ModComparator->tryInline = {CallL, nullptr};
                else if (CalledR && !isSimpllAbstractionDeclaration(CalledR))
                    ModComparator->tryInline = {nullptr, CallR};
            }
        }
        RETURN_WITH_LOG(Res);
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
                RETURN_WITH_LOG(Res);
            }
            // cmpValues should ensure this is true.
            assert(cmpTypes(OpL->getType(), OpR->getType()) == 0);
        }
    }
    RETURN_WITH_LOG(0);
}

int DifferentialFunctionComparator::cmpOperandTypes(
        const llvm::Instruction *L, const llvm::Instruction *R) const {
    if (int Res = cmpNumbers(L->getNumOperands(), R->getNumOperands()))
        return Res;
    for (unsigned i = 0, e = L->getNumOperands(); i != e; ++i) {
        if (int Res = cmpTypes(L->getOperand(i)->getType(),
                               R->getOperand(i)->getType()))
            return Res;
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

    // Check whether there is a field access difference because of
    // a structure type change.
    // Note: if one field access is longer than the other, it is possible
    // that a difference is found when the pointer is already outside the
    // field access in one of the modules. For this reason the previous
    // instruction is also tested for the presence of a field access.
    auto FAL = getFieldAccessStart(L);
    auto FAR = getFieldAccessStart(R);
    if (!FAL && L->getPrevNode())
        FAL = getFieldAccessStart(L->getPrevNode());
    if (!FAR && R->getPrevNode())
        FAR = getFieldAccessStart(R->getPrevNode());
    if (FAL && FAR) {
        findTypeDifferences(FAL, FAR, L->getFunction(), R->getFunction());
    }

    // Check whether there is a load type difference because of a structure
    // type change.
    if (isa<LoadInst>(L) && isa<LoadInst>(R)
        && cmpTypes(L->getType(), R->getType())) {
        const GetElementPtrInst *FAL;
        const GetElementPtrInst *FAR;
        // Check whether both load instructions operate on the result of a
        // a field access operation from named structure types of the same
        // name.
        if ((FAL = getFieldAccessStart(L->getOperand(0)))
            && (FAR = getFieldAccessStart(R->getOperand(0)))) {
            findTypeDifferences(FAL, FAR, L->getFunction(), R->getFunction());
        }
    }
}

bool DifferentialFunctionComparator::equal(const Instruction *InstL,
                                           const Instruction *InstR) {
    auto L = sn_mapL.find(InstL);
    auto R = sn_mapR.find(InstR);

    if (L == sn_mapL.end() || R == sn_mapR.end())
        // Values are not in maps
        return false;

    if (std::make_pair(InstL, InstR) == DifferingInstructions)
        // The instructions are the last instructions that were compared as
        // non-equal.
        return false;

    return L->second == R->second;
}

/// Retrieves the value that is mapped to the given value, taken from one of the
/// compared modules. When no such mapping exists, returns a null pointer.
const Value *
        DifferentialFunctionComparator::getMappedValue(const Value *Val,
                                                       bool ValFromL) const {
    auto sn_map = ValFromL ? &sn_mapL : &sn_mapR;

    // Ensure that the given value exists in the selected synchronization map.
    auto MappedValue = sn_map->find(Val);
    if (MappedValue == sn_map->end())
        return nullptr;

    // Find the mapped value based on its serial number.
    auto MappedPair = mappedValuesBySn[MappedValue->second];
    return ValFromL ? MappedPair.second : MappedPair.first;
}

/// Try to find a matching instruction that has been moved forward in one of
/// the basic blocks. If such an instruction is found, the relocation Reloc is
/// made valid and its begin and end are set to the first and the last
/// instructions of the block that was skipped during the search.
/// \param InstL Starting instruction in the first program.
/// \param InstR Starting instruction in the second program.
/// \param prog_to_search The program to search a matching instruction in (the
///                       corresponding iterator will be moved).
/// \return True if a matching instruction was found, otherwise false.
bool DifferentialFunctionComparator::findMatchingOpWithOffset(
        BasicBlock::const_iterator &InstL,
        BasicBlock::const_iterator &InstR,
        Program prog_to_search) const {
    // Choose which instruction will be moved and backup it
    auto &MovedInst = prog_to_search == Program::First ? InstL : InstR;
    auto MovedInstBackup = MovedInst;

    auto tryInlineBackup = ModComparator->tryInline;

    // Mark the possible relocation beginning.
    Reloc.begin = MovedInst;

    auto BBEnd = MovedInst->getParent()->end();

    // Reset the serial counters since InstL and InstR were already compared as
    // non-equal and start from the following instruction.
    undoLastInstCompare(InstL, InstR);
    ++MovedInst;
    while (MovedInst != BBEnd) {
        auto sn_mapL_backup = sn_mapL;
        auto sn_mapR_backup = sn_mapR;
        auto mappedValuesBySnBackup = mappedValuesBySn;
        if (cmpOperationsWithOperands(&*InstL, &*InstR) == 0) {
            // Found possible relocation - mark the end.
            auto end = MovedInst;
            end--;
            // Relocation must not end with a debuginfo instruction as those
            // are skipped and the end wouldn't be properly identified
            while (isDebugInfo(*end))
                end--;
            Reloc.end = end;
            // Make sure that the first equal instruction is not depending on
            // the relocation
            if (isDependingOnReloc(*MovedInst))
                return false;
            Reloc.status = RelocationInfo::Stored;
            Reloc.prog = prog_to_search;
            Reloc.tryInlineBackup = tryInlineBackup;

            LOG("Possible relocation found:\n"
                << "    from: " << *Reloc.begin << "\n"
                << "      to: " << *Reloc.end << "\n");
            return true;
        }
        // Restore serial maps since the instructions do not match
        sn_mapL = sn_mapL_backup;
        sn_mapR = sn_mapR_backup;
        mappedValuesBySn = mappedValuesBySnBackup;

        MovedInst++;
    }
    MovedInst = MovedInstBackup;
    ModComparator->tryInline = tryInlineBackup;
    return false;
}

/// Check if there is a dependency between the given instruction and the
/// currently stored relocation.
/// There is a dependency if both the instruction and the relocated code (any
/// instruction within it) access the same pointer and one of the accesses
/// is a store and the other one is a load.
/// \param Inst Instruction to check
/// \return True if there exists a dependency, otherwise false.
bool DifferentialFunctionComparator::isDependingOnReloc(
        const Instruction &Inst) const {
    auto *Load = dyn_cast<LoadInst>(&Inst);
    auto *Store = dyn_cast<StoreInst>(&Inst);
    if (!Load && !Store)
        return false;

    auto RelocInst = Reloc.begin;
    do {
        if (!Load)
            Load = dyn_cast<LoadInst>(&*RelocInst);
        if (!Store)
            Store = dyn_cast<StoreInst>(&*RelocInst);

        if (Load && Store
            && Load->getPointerOperand() == Store->getPointerOperand())
            return true;

        RelocInst++;
    } while (RelocInst != Reloc.end);

    return false;
}

/// Recursively collect operands of reorderable binary operators.
/// Leafs must be constants or already synchronized values.
/// Return false if a non-synchronized non-constant leaf is found.
bool DifferentialFunctionComparator::collectBinaryOperands(
        const Value *Val,
        Instruction::BinaryOps Opcode,
        std::multiset<int> &SNs,
        std::multiset<int64_t> &Constants,
        DenseMap<const Value *, int> &sn_map) const {
    // Substitute an ignored value with its replacement when necessary
    auto replacementIt = replacedInstructions.find(Val);
    const Value *newVal = replacementIt == replacedInstructions.end()
                                  ? Val
                                  : replacementIt->second;
    // Constant leaf
    if (auto Const = dyn_cast<ConstantInt>(newVal)) {
        Constants.insert(Const->getSExtValue());
        return true;
    }
    // Non-leaf; try to collect its operands recursively
    auto BinOp = dyn_cast<BinaryOperator>(newVal);
    if (BinOp && BinOp->getOpcode() == Opcode) {
        std::multiset<int> tmpSNs;
        std::multiset<int64_t> tmpConstants;
        bool leftSuccess = collectBinaryOperands(
                BinOp->getOperand(0), Opcode, tmpSNs, tmpConstants, sn_map);
        bool rightSuccess = collectBinaryOperands(
                BinOp->getOperand(1), Opcode, tmpSNs, tmpConstants, sn_map);
        if (leftSuccess && rightSuccess) {
            SNs.merge(tmpSNs);
            Constants.merge(tmpConstants);
            return true;
        }
    }
    // Already synchronized leaf; do not match if it is the last value inserted
    // into the synchronization map - it has just been compared as not equal
    auto snMapIt = sn_map.find(newVal);
    if (snMapIt != sn_map.end()
        && (unsigned)snMapIt->second != sn_map.size() - 1) {
        SNs.insert(snMapIt->second);
        return true;
    }
    return false;
}

/// Recursively check if users of the instruction with an inverse condition
/// are only branch or not instructions.
/// This is done to prevent false negatives.
/// Return true if the users are acceptable for inverse conditions pattern,
/// false otherwise.
bool DifferentialFunctionComparator::checkInverseCondUsers(
        const Instruction *inst) const {
    for (auto user : inst->users()) {
        auto userInstruction = dyn_cast<Instruction>(user);
        bool isNotInstruction = isLogicalNot(userInstruction);
        if (!isNotInstruction && !isa<BranchInst>(userInstruction)) {
            return false;
        } else if (isNotInstruction
                   && !checkInverseCondUsers(userInstruction)) {
            return false;
        }
    }
    return true;
}

/// Check whether the pointer operand of the given instruction (e.g., load)
/// points into local memory allocated by an ignored alloca.
template <typename InstType>
bool DifferentialFunctionComparator::hasIgnoredAllocaOp(
        const InstType *Inst) const {
    if (auto Alloca = getAllocaOp(Inst))
        return ignoredInstructions.count(Alloca)
               || skippedInstructions.count(Alloca);
    return false;
}

/// Check whether all uses of the provided value are extra memory instructions,
/// i.e. loads from or stores to ignored memory.
bool DifferentialFunctionComparator::allUsersAreExtraMemInsts(
        const Instruction *Inst) const {
    return std::all_of(
            Inst->user_begin(), Inst->user_end(), [&](const User *user) {
                if (auto store = dyn_cast<StoreInst>(user))
                    return hasIgnoredAllocaOp(store);
                if (auto load = dyn_cast<LoadInst>(user))
                    return hasIgnoredAllocaOp(load);
                return false;
            });
}
