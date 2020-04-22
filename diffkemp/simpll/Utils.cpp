//===------------------- Utils.cpp - Utility functions --------------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the implementations of utility functions.
///
//===----------------------------------------------------------------------===//

#include "Utils.h"
#include "Config.h"
#include <algorithm>
#include <iostream>
#include <llvm/IR/Module.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/LineIterator.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Scalar/DCE.h>
#include <llvm/Transforms/Scalar/NewGVN.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <numeric>
#include <set>
#include <sstream>

/// Level of debug indentation. Each level corresponds to two characters.
static unsigned int debugIndentLevel = 0;

// Invalid attributes for void functions and calls.
static std::vector<Attribute::AttrKind> badVoidAttributes = {
        Attribute::AttrKind::ByVal,
        Attribute::AttrKind::InAlloca,
        Attribute::AttrKind::Nest,
        Attribute::AttrKind::NoAlias,
        Attribute::AttrKind::NoCapture,
        Attribute::AttrKind::NonNull,
        Attribute::AttrKind::ReadNone,
        Attribute::AttrKind::ReadOnly,
        Attribute::AttrKind::SExt,
        Attribute::AttrKind::StructRet,
        Attribute::AttrKind::ZExt,
        Attribute::AttrKind::Dereferenceable,
        Attribute::AttrKind::DereferenceableOrNull};

/// Extract called function from a called value Handles situation when the
/// called value is a bitcast.
const Function *getCalledFunction(const Value *CalledValue) {
    const Function *fun = dyn_cast<Function>(CalledValue);
    if (!fun) {
        if (auto BitCast = dyn_cast<BitCastOperator>(CalledValue)) {
            fun = dyn_cast<Function>(BitCast->getOperand(0));
        } else if (auto Alias = dyn_cast<GlobalAlias>(CalledValue)) {
            fun = getCalledFunction(Alias->getAliasee());
        }
    }
    return fun;
}

/// Extract called function from a called value. Handles situation when the
/// called value is a bitcast.
Function *getCalledFunction(Value *CalledValue) {
    return const_cast<Function *>(
            getCalledFunction(const_cast<const Value *>(CalledValue)));
}

/// Extracts value from an arbitrary number of casts.
const Value *stripAllCasts(const Value *Val) {
    if (auto Cast = dyn_cast<CastInst>(Val))
        return stripAllCasts(Cast->getOperand(0));
    else if (auto Cast = dyn_cast<BitCastOperator>(Val))
        // Handle bitcast constant expressions.
        return stripAllCasts(Cast->getOperand(0));
    else
        return Val;
}

/// Extracts value from an arbitrary number of casts.
Value *stripAllCasts(Value *Val) {
    return const_cast<Value *>(stripAllCasts(const_cast<const Value *>(Val)));
}

/// Get name of a type so that it can be used as a variable in Z3.
std::string typeName(const Type *Type) {
    std::string result;
    raw_string_ostream rso(result);
    Type->print(rso);
    rso.str();
    // We must do some modifications to the type name so that is is usable as
    // a Z3 variable
    result.erase(std::remove(result.begin(), result.end(), ' '), result.end());
    std::replace(result.begin(), result.end(), '(', '$');
    std::replace(result.begin(), result.end(), ')', '$');
    std::replace(result.begin(), result.end(), ',', '_');
    return result;
}

/// Find alias which points to the given function and delete it.
void deleteAliasToFun(Module &Mod, Function *Fun) {
    std::vector<GlobalAlias *> toRemove;
    for (auto &alias : Mod.aliases()) {
        if (alias.getAliasee() == Fun)
            toRemove.push_back(&alias);
    }
    for (auto &alias : toRemove) {
        alias->replaceAllUsesWith(Fun);
        alias->eraseFromParent();
    }
}

/// Check if the substring behind the last dot ('.') contains only numbers.
bool hasSuffix(std::string Name) {
    size_t dotPos = Name.find_last_of('.');
    return dotPos != std::string::npos
           && (Name.find_last_not_of("0123456789.") < dotPos
               || Name.substr(dotPos) == ".void");
}

/// Remove everything behind the last dot ('.'). Assumes that hasSuffix returned
/// true for the name.
std::string dropSuffix(std::string Name) {
    return Name.substr(0, Name.find_last_of('.'));
}

/// Join directory path with a filename in case the filename does not already
/// contain the directory.
std::string joinPath(StringRef DirName, StringRef FileName) {
    return FileName.startswith(DirName)
                   ? FileName.str()
                   : DirName.str() + sys::path::get_separator().str()
                             + FileName.str();
}

/// Extracts file name and directory name from the DebugInfo
std::string getFileForFun(const Function *Fun) {
    if (auto *SubProgram = Fun->getSubprogram()) {
        if (auto *File = SubProgram->getFile())
            return joinPath(File->getDirectory(), File->getFilename());
    }
    return "";
}

/// Check if function has side effect (has 'store' instruction or calls some
/// other function with side effect).
bool hasSideEffect(const Function &Fun, std::set<const Function *> &Visited) {
    if (Fun.isDeclaration()) {
        return !(Fun.getIntrinsicID() == Intrinsic::dbg_declare
                 || Fun.getIntrinsicID() == Intrinsic::dbg_value
                 || Fun.getIntrinsicID() == Intrinsic::expect);
    }
    Visited.insert(&Fun);
    for (auto &BB : Fun) {
        for (auto &Inst : BB) {
            if (isa<StoreInst>(&Inst))
                return true;
            if (auto Call = dyn_cast<CallInst>(&Inst)) {
                const Function *called = Call->getCalledFunction();
                if (!called)
                    return true;
                if (Visited.find(called) != Visited.end())
                    continue;
                if (hasSideEffect(*called, Visited))
                    return true;
            }
        }
    }
    return false;
}

/// Calls recursive variant of the function with empty set of visited functions.
bool hasSideEffect(const Function &Fun) {
    std::set<const Function *> visited;
    return hasSideEffect(Fun, visited);
}

/// Returns true if the function is one of the supported allocators
bool isAllocFunction(const Function &Fun) {
    return Fun.getName() == "kzalloc" || Fun.getName() == "__kmalloc"
           || Fun.getName() == "kmalloc";
}

/// Retuns true if the given value is a cast (instruction or constant
/// expression)
bool isCast(const Value *Val) {
    if (isa<CastInst>(Val))
        return true;

    if (auto CExpr = dyn_cast<ConstantExpr>(Val))
        return CExpr->isCast();

    return false;
}

/// Returns true if the given value is a GEP instruction with all indices equal
/// to zero.
bool isZeroGEP(const Value *Val) {
    if (isa<GetElementPtrInst>(Val)) {
        auto Inst = dyn_cast<User>(Val);
        for (unsigned i = 1; i < Inst->getNumOperands(); ++i) {
            auto Int = dyn_cast<ConstantInt>(Inst->getOperand(i));
            if (!(Int && Int->getZExtValue() == 0)) {
                return false;
            }
        }
        return true;
    }
    return false;
}

/// Get value of the given constant as a string
std::string valueAsString(const Constant *Val) {
    if (auto *IntVal = dyn_cast<ConstantInt>(Val)) {
        return std::to_string(IntVal->getSExtValue());
    }
    return "";
}

/// Extract struct type of the value.
/// Works if the value is of pointer type which can be even bitcasted.
/// Returns nullptr if the value is not a struct.
StructType *getStructType(const Value *Value) {
    StructType *Type = nullptr;
    if (auto PtrTy = dyn_cast<PointerType>(Value->getType())) {
        // Value is a pointer
        if (auto *StructTy = dyn_cast<StructType>(PtrTy->getElementType())) {
            // Value points to a struct
            Type = StructTy;
        } else if (auto *BitCast = dyn_cast<BitCastInst>(Value)) {
            // Value is a bicast, get the original type
            if (auto *SrcPtrTy = dyn_cast<PointerType>(BitCast->getSrcTy())) {
                if (auto *SrcStructTy =
                            dyn_cast<StructType>(SrcPtrTy->getElementType()))
                    Type = SrcStructTy;
            }
        }
    } else
        Type = dyn_cast<StructType>(Value->getType());
    return Type;
}

/// Extracts source types for all GEPs in a field accecss abstraction.
std::vector<Type *> getFieldAccessSourceTypes(const Function *FA) {
    std::vector<Type *> TypeVec;
    const BasicBlock *BB = &*FA->begin();
    for (const Instruction &I : *BB) {
        if (auto GEP = dyn_cast<GetElementPtrInst>(&I)) {
            TypeVec.push_back(GEP->getSourceElementType());
            // If the GEP has another GEP as its argument, add it to the vector.
            if (auto InnerGEP =
                        dyn_cast<GEPOperator>(GEP->getPointerOperand())) {
                TypeVec.push_back(InnerGEP->getSourceElementType());
            }
        }
    }
    return TypeVec;
}

/// Run simplification passes on the function
///  - simplify CFG
///  - dead code elimination
void simplifyFunction(Function *Fun) {
    PassBuilder pb;
    FunctionPassManager fpm(false);
    FunctionAnalysisManager fam(false);
    pb.registerFunctionAnalyses(fam);
#if LLVM_VERSION_MAJOR > 5
    fpm.addPass(SimplifyCFGPass{});
#endif
    fpm.addPass(DCEPass{});
    fpm.addPass(NewGVNPass{});
    fpm.run(*Fun, fam);
}

/// Removes empty attribute sets from an attribute list.
/// This function is used when some attributes are removed to clean up.
AttributeList cleanAttributeList(AttributeList AL) {
    // Copy over all attributes to a new attribute list.
    AttributeList NewAttrList;

    // There are three possible indices for attribute sets
    std::vector<AttributeList::AttrIndex> indices{AttributeList::FirstArgIndex,
                                                  AttributeList::FunctionIndex,
                                                  AttributeList::ReturnIndex};

    for (AttributeList::AttrIndex i : indices) {
        AttributeSet AttrSet = AL.getAttributes(i);
        if (AttrSet.getNumAttributes() != 0) {
            AttrBuilder AB;
            for (const Attribute &A : AttrSet)
                AB.addAttribute(A);
            NewAttrList = NewAttrList.addAttributes(AL.getContext(), i, AB);
        }
    }

    return NewAttrList;
}

/// Get a non-const pointer to a call instruction in a function
CallInst *findCallInst(const CallInst *Call, Function *Fun) {
    if (!Call)
        return nullptr;
    for (auto &BB : *Fun) {
        for (auto &Inst : BB) {
            if (&Inst == Call)
                return dyn_cast<CallInst>(&Inst);
        }
    }
    return nullptr;
}

/// Gets C source file from a DIScope and the module.
std::string getSourceFilePath(DIScope *Scope) {
    return joinPath(Scope->getDirectory(), Scope->getFilename());
}

/// Returns true when the argument is a name of a kernel print function.
bool isPrintFunction(std::string name) {
    const static std::set<std::string> List = {
            "_dev_info", "dev_warn", "dev_err", "sprintf", "printk"};

    return List.find(name) != List.end();
}

/// Checks whether the character is valid for a C identifier.
bool isValidCharForIdentifier(char ch) {
    if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')
        || (ch >= '0' && ch <= '9') || ch == '_')
        return true;
    else
        return false;
}

/// Checks whether the character is valid for the first character of
/// a C identifier.
bool isValidCharForIdentifierStart(char ch) {
    if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_')
        return true;
    else
        return false;
}

/// Finds the string given in the second argument and replaces it with the one
/// given in the third argument.
void findAndReplace(std::string &input, std::string find, std::string replace) {
    if (find == "")
        return;

    int position = 0;
    while ((position = input.find(find, position)) != std::string::npos) {
        input.replace(position, find.length(), replace);
        position += replace.length();
    }
}

/// Convert constant expression to instruction. (Copied from LLVM and modified
/// to work outside the ConstantExpr class; otherwise the function is the same,
/// the only purpose of copying the function is making it work on constant
/// input.)
/// Note: this is for constant ConstantExpr pointers; for non-constant ones,
/// the built-in getAsInstruction method is sufficient.
const Instruction *getConstExprAsInstruction(const ConstantExpr *CEx) {
    SmallVector<Value *, 4> ValueOperands(CEx->op_begin(), CEx->op_end());
    ArrayRef<Value *> Ops(ValueOperands);

    switch (CEx->getOpcode()) {
    case Instruction::Trunc:
    case Instruction::ZExt:
    case Instruction::SExt:
    case Instruction::FPTrunc:
    case Instruction::FPExt:
    case Instruction::UIToFP:
    case Instruction::SIToFP:
    case Instruction::FPToUI:
    case Instruction::FPToSI:
    case Instruction::PtrToInt:
    case Instruction::IntToPtr:
    case Instruction::BitCast:
    case Instruction::AddrSpaceCast:
        return CastInst::Create(
                (Instruction::CastOps)CEx->getOpcode(), Ops[0], CEx->getType());
    case Instruction::Select:
        return SelectInst::Create(Ops[0], Ops[1], Ops[2]);
    case Instruction::InsertElement:
        return InsertElementInst::Create(Ops[0], Ops[1], Ops[2]);
    case Instruction::ExtractElement:
        return ExtractElementInst::Create(Ops[0], Ops[1]);
    case Instruction::InsertValue:
        return InsertValueInst::Create(Ops[0], Ops[1], CEx->getIndices());
    case Instruction::ExtractValue:
        return ExtractValueInst::Create(Ops[0], CEx->getIndices());
    case Instruction::ShuffleVector:
        return new ShuffleVectorInst(Ops[0], Ops[1], Ops[2]);

    case Instruction::GetElementPtr: {
        const auto *GO = cast<GEPOperator>(CEx);
        if (GO->isInBounds())
            return GetElementPtrInst::CreateInBounds(
                    GO->getSourceElementType(), Ops[0], Ops.slice(1));
        return GetElementPtrInst::Create(
                GO->getSourceElementType(), Ops[0], Ops.slice(1));
    }
    case Instruction::ICmp:
    case Instruction::FCmp:
        return CmpInst::Create((Instruction::OtherOps)CEx->getOpcode(),
                               (CmpInst::Predicate)CEx->getPredicate(),
                               Ops[0],
                               Ops[1]);

    default:
        assert(CEx->getNumOperands() == 2 && "Must be binary operator?");
        BinaryOperator *BO = BinaryOperator::Create(
                (Instruction::BinaryOps)CEx->getOpcode(), Ops[0], Ops[1]);
        if (isa<OverflowingBinaryOperator>(BO)) {
            BO->setHasNoUnsignedWrap(
                    CEx->getRawSubclassOptionalData()
                    & OverflowingBinaryOperator::NoUnsignedWrap);
            BO->setHasNoSignedWrap(CEx->getRawSubclassOptionalData()
                                   & OverflowingBinaryOperator::NoSignedWrap);
        }
        if (isa<PossiblyExactOperator>(BO))
            BO->setIsExact(CEx->getRawSubclassOptionalData()
                           & PossiblyExactOperator::IsExact);
        return BO;
    }
}

/// Generates human-readable C-like identifier for type.
std::string getIdentifierForType(Type *Ty) {
    if (auto STy = dyn_cast<StructType>(Ty)) {
        // Remove prefix and append "struct"
        if (STy->getStructName().startswith("union"))
            return "union " + STy->getStructName().str().substr(6);
        else if (STy->getStructName().startswith("struct"))
            return "struct " + STy->getStructName().str().substr(7);
        else
            return "<unknown>";
    } else if (auto IntTy = dyn_cast<IntegerType>(Ty)) {
        if (IntTy->getBitWidth() == 1)
            return "bool";
        else
            return "int" + std::to_string(IntTy->getBitWidth()) + "_t";
    } else if (auto ArrTy = dyn_cast<ArrayType>(Ty)) {
        return getIdentifierForType(ArrTy->getElementType()) + "[]";
    } else if (Ty->isVoidTy()) {
        return "void";
    } else if (auto PointTy = dyn_cast<PointerType>(Ty)) {
        return getIdentifierForType(PointTy->getElementType()) + " *";
    } else
        return "<unknown>";
}

/// Generates human-readable C-like identifier for value.
std::string getIdentifierForValue(
        const Value *Val,
        const std::map<std::pair<StructType *, uint64_t>, StringRef>
                &StructFieldNames,
        const Function *Parent) {
    // This function uses a different approach for different types of values.
    if (auto GEPi = dyn_cast<GetElementPtrInst>(Val)) {
        // GEP instruction.
        // First find the original variable name, then try to append the names
        // of all indices.
        std::string name = getIdentifierForValue(
                GEPi->getOperand(0), StructFieldNames, Parent);

        std::vector<Value *> Indices;

        for (auto idx = GEPi->idx_begin(); idx != GEPi->idx_end(); ++idx) {
            auto ValueType = GEPi->getIndexedType(GEPi->getSourceElementType(),
                                                  ArrayRef<Value *>(Indices));
            Value *Index = idx->get();

            if (isa<ConstantInt>(Index)
                && (dyn_cast<ConstantInt>(Index)->getValue().getZExtValue()
                    == 0)
                && (idx == GEPi->idx_begin())) {
                // Do not print the first zero index
                continue;
            }

            if (isa<StructType>(ValueType)) {
                // Structure type indexing
                auto NumericIndex = dyn_cast<ConstantInt>(Index)->getValue();
                auto IndexName =
                        StructFieldNames.find({dyn_cast<StructType>(ValueType),
                                               NumericIndex.getZExtValue()});
                if (IndexName != StructFieldNames.end()) {
                    // We can use the index name to create a C-like syntax.
                    name += "->" + IndexName->second.str();
                } else {
                    name += "->" + std::to_string(NumericIndex.getZExtValue());
                }
            } else {
                // Array type indexing (the index doesn't have to be constant)
                std::string IdxName =
                        getIdentifierForValue(Index, StructFieldNames, Parent);

                // Remove reference operator to match C syntax
                name = name.substr(2, name.size() - 3);

                if (IdxName != "") {
                    name += "[" + IdxName + "]";
                } else {
                    name += "[<unknown>]";
                }
            }

            // We get the pointer to the data, not the data itself.
            name = "&(" + name + ")";
        }

        return name;
    } else if (auto CEx = dyn_cast<ConstantExpr>(Val)) {
        // Constant expressions are converted to instructions.
        return getIdentifierForValue(
                getConstExprAsInstruction(CEx), StructFieldNames, Parent);
    } else if (auto BitCast = dyn_cast<BitCastInst>(Val)) {
        // Bit casts are expanded to C-like cast syntax.
        std::string Casted = getIdentifierForValue(
                BitCast->getOperand(0), StructFieldNames, Parent);
        return "((" + getIdentifierForType(BitCast->getDestTy()) + ") " + Casted
               + ")";
    } else if (auto ZExt = dyn_cast<ZExtInst>(Val)) {
        // ZExt is treated the same as a statement without it
        return getIdentifierForValue(
                ZExt->getOperand(0), StructFieldNames, Parent);
    } else if (auto Load = dyn_cast<LoadInst>(Val)) {
        // Load instruction is treated as the dereference operator
        std::string Internal = getIdentifierForValue(
                Load->getOperand(0), StructFieldNames, Parent);

        if (Internal[0] == '&')
            // Reference and dereference operator cancel out.
            // (delete & and parethenses)
            return Internal.substr(2, Internal.size() - 3);
        else
            return "*(" + Internal + ")";
    } else if (Val->hasName()) {
        // If everything fails, try to get the name directly from the value
        return Val->getName().str();
    } else if (auto Const = dyn_cast<Constant>(Val)) {
        // Constant to string is already implemented in a different function
        return valueAsString(Const);
    } else if (Parent) {
        // Check if the value is a function argument - in case it is, extract
        // the argument name.
        int idx = 0;

        // Extract register number
        std::string ValDump;
        llvm::raw_string_ostream ValDumStrm(ValDump);
        Val->print(ValDumStrm);
        std::string RegName = ValDump.substr(ValDump.find_last_of('%') + 1,
                                             std::string::npos);
        int RegNum;
        std::istringstream Iss(RegName);
        Iss >> RegNum;
        if (Iss.fail())
            return "<unknown>";

        // Find the argument name
        if (RegNum >= Parent->arg_size())
            // Not a function argument
            return "<unknown>";

        DISubprogram *Sub = Parent->getSubprogram();
#if LLVM_VERSION_MAJOR < 7
        DINodeArray funArgs = Sub->getVariables();
#else
        DINodeArray funArgs = Sub->getRetainedNodes();
#endif
        for (DINode *Node : funArgs) {
            if (idx == RegNum) {
                DILocalVariable *LocVar = dyn_cast<DILocalVariable>(Node);
                if (!LocVar)
                    return "<unknown>";
                return LocVar->getName().str();
            }
            ++idx;
        }

        return "<unknown>";
    } else
        return "<unknown>";
}

/// Retrieves the type of the value based its C source code expression.
Type *getCSourceIdentifierType(
        std::string expr,
        const Function *Parent,
        const std::unordered_map<std::string, const Value *>
                &LocalVariableMap) {
    // First we have to remove pointer operators from the call.
    if (expr[0] == '*') {
        // Dereference operator. Return the original type.
        Type *Ty = getCSourceIdentifierType(
                expr.substr(1), Parent, LocalVariableMap);
        if (!Ty)
            return nullptr;

        PointerType *PTy = dyn_cast<PointerType>(Ty);
        return PTy->getElementType();
    } else if (expr[0] == '&') {
        // Reference operator. Return a pointer type.
        Type *InnerTy = getCSourceIdentifierType(
                expr.substr(1), Parent, LocalVariableMap);

        // Note: assuming von Neumann architecture with single address space.
        if (!InnerTy)
            return nullptr;
        else
            return PointerType::get(InnerTy, 0);
    } else {
        // Determine whether the expression is an identifier at this point.
        // If not, it is not supported.
        std::vector<bool> Tmp;
        std::transform(expr.begin(),
                       expr.end(),
                       std::back_inserter(Tmp),
                       isValidCharForIdentifier);
        if (!std::accumulate(Tmp.begin(), Tmp.end(), true, [](auto a, auto b) {
                return a && b;
            })) {
            // There are some characters that are not allowed in an identifier.
            return nullptr;
        }

        // Now try to look up the identifier first in global variables, then in
        // local variables.
        auto Glob = Parent->getParent()->getGlobalVariable(expr);
        if (Glob)
            return Glob->getValueType();

        auto Loc = LocalVariableMap.find(Parent->getName().str() + "::" + expr);
        if (Loc != LocalVariableMap.end())
            return Loc->second->getType();

        // If everything failed, return null.
        return nullptr;
    }
}

/// Copies properties from one call instruction to another.
void copyCallInstProperties(CallInst *srcCall, CallInst *destCall) {
    destCall->setAttributes(srcCall->getAttributes());
    destCall->setCallingConv(srcCall->getCallingConv());

    if (srcCall->isTailCall()) {
        destCall->setTailCall();
    }

    if (!srcCall->getType()->isVoidTy() && destCall->getType()->isVoidTy()) {
        // Remove attributes that are incompatible with void calls.
        for (Attribute::AttrKind AK : badVoidAttributes) {
            destCall->removeAttribute(AttributeList::ReturnIndex, AK);
            destCall->removeAttribute(AttributeList::FunctionIndex, AK);
        }

        destCall->setAttributes(cleanAttributeList(destCall->getAttributes()));
    }
}

/// Copies properties from one function to another.
void copyFunctionProperties(Function *srcFun, Function *destFun) {
    destFun->copyAttributesFrom(srcFun);
    destFun->setSubprogram(srcFun->getSubprogram());

    if (!srcFun->getType()->isVoidTy() && destFun->getType()->isVoidTy()) {
        for (Attribute::AttrKind AK : badVoidAttributes) {
            // Remove attributes that are incompatible with void functions.
            destFun->removeAttribute(AttributeList::ReturnIndex, AK);
            destFun->removeAttribute(AttributeList::FunctionIndex, AK);
        }
        destFun->setAttributes(cleanAttributeList(destFun->getAttributes()));
    }

    // Set the names of all arguments of the new function
    for (Function::arg_iterator AI = srcFun->arg_begin(),
                                AE = srcFun->arg_end(),
                                NAI = destFun->arg_begin();
         AI != AE;
         ++AI, ++NAI) {
        NAI->takeName(AI);
    }
}

/// Converts value to its string representation.
/// Note: Currently the only place that calls this is returns.gdb, which lacks
/// the ability to directly dump values because GDB can't call the corresponding
/// methods.
std::string valueToString(const Value *Val) {
    std::string ValDump;
    llvm::raw_string_ostream DumpStrm(ValDump);
    Val->print(DumpStrm);
    return DumpStrm.str();
}

/// Converts type to its (LLVM IR) string representation.
std::string typeToString(Type *Ty) {
    std::string TyDump;
    llvm::raw_string_ostream DumpStrm(TyDump);
    Ty->print(DumpStrm);
    return DumpStrm.str();
}

/// Get a string matching the current indentation level.
/// \param prefix Indentation prefix character, defaults to space.
std::string getDebugIndent(const char prefixChar) {
    return std::string(debugIndentLevel * 2, prefixChar);
}

/// Increase the level of debug indentation by one.
void increaseDebugIndentLevel() { debugIndentLevel++; }

/// Decrease the level of debug indentation by one.
void decreaseDebugIndentLevel() {
    assert(debugIndentLevel > 0);
    debugIndentLevel--;
}
