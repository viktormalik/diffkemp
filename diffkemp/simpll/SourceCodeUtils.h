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

#ifndef DIFFKEMP_SIMPLL_SOURCE_CODE_UTILS_H
#define DIFFKEMP_SIMPLL_SOURCE_CODE_UTILS_H

#include "Result.h"
#include "Utils.h"

#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <string>
#include <unordered_map>

using namespace llvm;

/// Specialisation of Definition from "Result.h", contains more information
/// (body, parameters, ...) which are used for finding macro difference.
struct MacroDef : public Definition {
    // Note: The macro name is shortened, therefore it has to be stored as the
    // whole string, not as a StringRef (otherwise the content would be dropped
    // after the block in which the shortening is done).

    // Full macro name (including parameters).
    std::string fullName;
    // The body is in DebugInfo therefore it can be stored by reference.
    StringRef body;
    // List of the macro parameters.
    std::vector<std::string> params;
};

/// Macro usage containing macro definition, caller macro, and arguments
struct MacroUse {
    // Pointer to the macro definition and to the parent macro of this usage
    const MacroDef *def;
    const MacroUse *parent;
    // Location of the macro use in C source.
    int line;
    std::string sourceFile;
    // List of arguments of the macro use
    std::vector<std::string> args;
};

/// Class for finding differences in macros. Contains collections of macro
/// definitions and macro usages
class MacroDiffAnalysis {
  public:
    /// Find macro differences at the locations of the instructions L and R and
    /// return them as a vector.
    /// This is used when a difference is suspected to be in a macro in order to
    /// include that difference into ModuleComparator, and therefore avoid an
    /// empty diff.
    std::vector<std::unique_ptr<SyntaxDifference>> findMacroDifferences(
            const Instruction *L, const Instruction *R, int lineOffset = 0);

    /// Get all macros used on a certain DILocation in the form of a StringMap
    /// mapping macro names to MacroUse objects
    const StringMap<MacroUse> &getAllMacroUsesAtLocation(DILocation *Loc,
                                                         int lineOffset = 0);

  private:
    /// Collect all macros defined in the given compile unit and store them into
    /// MacroDefMaps
    void collectMacroDefs(DICompileUnit *CompileUnit);
    /// Collect all macros used at the given location and store the uses into
    /// MacroUsesAtLocation
    void collectMacroUsesAtLocation(DILocation *Loc,
                                    const StringMap<MacroDef> &macroDefs,
                                    int lineOffset = 0);

    /// Collection of macro definition maps for each compilation unit
    /// Every macro definition map is represented as a StringMap mapping macro
    /// names to MacroDef objects.
    std::map<DICompileUnit *, StringMap<MacroDef>> MacroDefMaps;
    /// Collection of used macros for each program location.
    /// All macro uses for a location are represented as a StringMap mapping
    /// macro names to MacroUse objects. MacroUse objects contain pointers to
    /// macro definitions stored in MacroDefMaps.
    std::map<DILocation *, StringMap<MacroUse>> MacroUsesAtLocation;
};

/// Takes a list of parameter-argument pairs and expand them on places where
/// are a part of a composite macro name joined by ##.
void expandCompositeMacroNames(std::vector<std::string> params,
                               std::vector<std::string> args,
                               std::string &body);

/// Extract the line corresponding to the DILocation from the C source file.
std::string extractLineFromLocation(DILocation *LineLoc, int offset = 0);

/// Takes a string and the position of the first bracket and returns the
/// substring in the brackets.
std::string getSubstringToMatchingBracket(std::string str, size_t position);

/// Tries to convert C source syntax of inline ASM (the input may include other
/// code, the inline asm is found and extracted) to the LLVM syntax.
/// Returns a pair of strings - the first one contains the converted ASM, the
/// second one contains (unparsed) arguments.
std::pair<std::string, std::string>
        convertInlineAsmToLLVMFormat(std::string input);

/// Takes a LLVM inline assembly with the corresponding call location and
/// retrieves the corresponding arguments in the C source code.
std::vector<std::string>
        findInlineAssemblySourceArguments(DILocation *LineLoc,
                                          std::string inlineAsm,
                                          MacroDiffAnalysis *MacroDiffs);

// Takes in a string with C function call arguments and splits it into a vector.
std::vector<std::string> splitArgumentsList(std::string argumentString);

/// Takes a function name with the corresponding call location and retrieves
/// the corresponding arguments in the C source code.
std::vector<std::string>
        findFunctionCallSourceArguments(DILocation *LineLoc,
                                        std::string functionName,
                                        MacroDiffAnalysis *MacroDiffs);

/// Expand simple non-argument macros in string. The macros are determined by
/// macro-body pairs.
std::string expandMacros(std::vector<std::string> macros,
                         std::vector<std::string> bodies,
                         std::string Input);

#endif // DIFFKEMP_SIMPLL_SOURCE_CODE_UTILS_H
