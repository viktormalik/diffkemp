//===---- SourceCodeUtils.h - Functions for working with C source code -----==//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Tomas Glozar, tglozar@gmail.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains declarations of functions working with various things
/// related to C source code analysis.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_MACRO_UTILS_H
#define DIFFKEMP_SIMPLL_MACRO_UTILS_H

#include "ModuleComparator.h"
#include "Utils.h"

#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <string>
#include <unordered_map>

using namespace llvm;

/// Returned macros with value and location.
struct MacroElement {
    // The macro name is shortened, therefore it has to be stored as the whole
    // string, because otherwise the content would be dropped on leaving the
    // block in which the shortening is done.
    std::string name;
    // Full macro name. (Used for extracting parameters.)
    std::string fullName;
    // The body is in DebugInfo, parentMacro is a key in the map, therefore
    // both can be stored by reference.
    StringRef body, parentMacro;
    // This is the line in the C source code on which the macro is located.
    int line;
    std::string sourceFile;
    // This is a list of the arguments of the macro and its parameters.
    std::vector<std::string> args, params;
};

/// Gets all macros used on the line in the form of a key to value map.
std::unordered_map<std::string, MacroElement>
        getAllMacrosOnLine(StringRef line, StringMap<StringRef> macroMap);

/// Takes a list of parameter-argument pairs and expand them on places where
/// are a part of a composite macro name joined by ##.
void expandCompositeMacroNames(std::vector<std::string> params,
                               std::vector<std::string> args,
                               std::string &body);

/// Extract the line corresponding to the DILocation from the C source file.
std::string extractLineFromLocation(DILocation *LineLoc, int offset = 0);

/// Gets all macros used on a certain DILocation in the form of a key to value
/// map.
std::unordered_map<std::string, MacroElement> getAllMacrosAtLocation(
        DILocation *LineLoc, const Module *Mod, int lineOffset = 0);

/// Finds macro differences at the locations of the instructions L and R and
/// return them as a vector.
/// This is used when a difference is suspected to be in a macro in order to
/// include that difference into ModuleComparator, and therefore avoid an
/// empty diff.
std::vector<std::unique_ptr<SyntaxDifference>> findMacroDifferences(
        const Instruction *L, const Instruction *R, int lineOffset = 0);

// Takes a string and the position of the first bracket and returns the
// substring in the brackets.
std::string getSubstringToMatchingBracket(std::string str, size_t position);

/// Tries to convert C source syntax of inline ASM (the input may include other
/// code, the inline asm is found and extracted) to the LLVM syntax.
/// Returns a pair of strings - the first one contains the converted ASM, the
/// second one contains (unparsed) arguments.
std::pair<std::string, std::string>
        convertInlineAsmToLLVMFormat(std::string input);

/// Takes a LLVM inline assembly with the corresponding call location and
/// retrieves the corresponding arguments in the C source code.
std::vector<std::string> findInlineAssemblySourceArguments(
        DILocation *LineLoc, const Module *Mod, std::string inlineAsm);

// Takes in a string with C function call arguments and splits it into a vector.
std::vector<std::string> splitArgumentsList(std::string argumentString);

/// Takes a function name with the corresponding call location and retrieves
/// the corresponding arguments in the C source code.
std::vector<std::string> findFunctionCallSourceArguments(
        DILocation *LineLoc, const Module *Mod, std::string functionName);

/// Expand simple non-argument macros in string. The macros are determined by
/// macro-body pairs.
std::string expandMacros(std::vector<std::string> macros,
                         std::vector<std::string> bodies,
                         std::string Input);

/// Get number of lines of code of a function.
unsigned countLoc(const Function &Fun);

#endif // DIFFKEMP_SIMPLL_MACRO_UTILS_H
