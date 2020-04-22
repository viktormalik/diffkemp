//===-------------------- Utils.h - Utility functions ---------------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of utility functions and enums.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_UTILS_H
#define DIFFKEMP_SIMPLL_UTILS_H

#include <llvm/IR/Constants.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <unordered_map>

using namespace llvm;

/// Enumeration type to easy distinguishing between the compared programs.
enum Program { First, Second };

typedef std::pair<Function *, Function *> FunPair;
typedef std::pair<const Function *, const Function *> ConstFunPair;
typedef std::pair<const GlobalValue *, const GlobalValue *> GlobalValuePair;

/// Extract called function from a called value. Handles situation when the
/// called value is a bitcast.
const Function *getCalledFunction(const Value *CalledValue);

/// Extract called function from a called value. Handles situation when the
/// called value is a bitcast.
Function *getCalledFunction(Value *CalledValue);

/// Extracts value from an arbitrary number of casts.
const Value *stripAllCasts(const Value *Val);

/// Extracts value from an arbitrary number of casts.
Value *stripAllCasts(Value *Val);

/// Get name of a type.
std::string typeName(const Type *Type);

/// Delete alias to a function
void deleteAliasToFun(Module &Mod, Function *Fun);

/// Check if an LLVM name has a .<NUMBER> suffix.
bool hasSuffix(std::string Name);

/// Drop the .<NUMBER> suffix from the LLVM name.
std::string dropSuffix(std::string Name);

/// Join directory path with a filename in case the filename does not already
/// contain the directory.
std::string joinPath(StringRef DirName, StringRef FileName);

/// Get absolute path to a file in which the given function is defined.
/// Requires debug info to work correctly.
std::string getFileForFun(const Function *Fun);

/// Check if function has side-effect.
bool hasSideEffect(const Function &Fun);

/// Check if the function is an allocator
bool isAllocFunction(const Function &Fun);

/// Retuns true if the given value is a cast (instruction or constant
/// expression)
bool isCast(const Value *Val);

/// Returns true if the given instruction is a GEP with all indices equal to 0
bool isZeroGEP(const Value *Val);

/// Run simplification passes on the function
///  - simplify CFG
///  - dead code elimination
void simplifyFunction(Function *Fun);

/// Get value of the given constant as a string
std::string valueAsString(const Constant *Val);

/// Extracts source types for all GEPs in a field accecss abstraction.
std::vector<Type *> getFieldAccessSourceTypes(const Function *FA);

/// Extract struct type of the value.
/// Works if the value is of pointer type which can be even bitcasted.
StructType *getStructType(const Value *Value);

/// Removes empty attribute sets from an attribute list.
AttributeList cleanAttributeList(AttributeList AL);

/// Get a non-const pointer to a call instruction in a function
CallInst *findCallInst(const CallInst *Call, Function *Fun);

/// Gets C source file from a DIScope and the module.
std::string getSourceFilePath(DIScope *Scope);

/// Returns true when the argument is a name of a kernel print function.
bool isPrintFunction(std::string name);

/// Checks whether the character is valid for a C identifier.
bool isValidCharForIdentifier(char ch);

/// Checks whether the character is valid for the first character of
/// a C identifier.
bool isValidCharForIdentifierStart(char ch);

/// Finds the string given in the second argument and replaces it with the one
/// given in the third argument.
void findAndReplace(std::string &input, std::string find, std::string replace);

/// Convert constant expression to instruction. (Copied from LLVM and modified)
/// Note: this is for constant ConstantExpr pointers; for non-constant ones,
/// the built-in getAsInstruction method is sufficient.
const Instruction *getConstExprAsInstruction(const ConstantExpr *CEx);

/// Retrives type of the value based its C source code identifier.
Type *getCSourceIdentifierType(
        std::string expr,
        const Function *Parent,
        const std::unordered_map<std::string, const Value *> &LocalVariableMap);

/// Generates human-readable C-like identifier for type.
std::string getIdentifierForType(Type *Ty);

/// Finds the name of a value (in case there exists one).
std::string getIdentifierForValue(
        const Value *Val,
        const std::map<std::pair<StructType *, uint64_t>, StringRef>
                &StructFieldNames,
        const Function *Parent = nullptr);

/// Copies properties from one call instruction to another.
void copyCallInstProperties(CallInst *srcCall, CallInst *destCall);

/// Copies properties from one function to another.
void copyFunctionProperties(Function *srcFun, Function *destFun);

/// Converts value to its string representation.
/// Note: Currently the only place that calls this is returns.gdb, which lacks
/// the ability to directly dump values because GDB can't call the corresponding
/// methods.
std::string valueToString(const Value *Val);

/// Converts type to its (LLVM IR) string representation.
std::string typeToString(Type *Ty);

/// Get a string matching the current indentation level.
/// \param prefixChar Indentation prefix character, defaults to space.
std::string getDebugIndent(const char prefixChar = ' ');

/// Increase the level of debug indentation by one.
void increaseDebugIndentLevel();

/// Decrease the level of debug indentation by one.
void decreaseDebugIndentLevel();

#endif // DIFFKEMP_SIMPLL_UTILS_H
