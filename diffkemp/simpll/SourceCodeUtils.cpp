//===---- SourceCodeUtils.cpp - Functions for working with C source code ---==//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Tomas Glozar, tglozar@gmail.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains implementation of functions working with various things
/// related to C source code analysis.
///
//===----------------------------------------------------------------------===//

#include "SourceCodeUtils.h"
#include "Config.h"
#include "Utils.h"
#include <llvm/ADT/STLExtras.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/LineIterator.h>
#include <llvm/Support/raw_ostream.h>

/// Gets all macros used on the line in the form of a key to value map.
std::unordered_map<std::string, MacroElement>
        getAllMacrosOnLine(StringRef line, StringMap<MacroElement> macroMap) {
    // Transform macroMap into a second that will contain only macros that are
    // used on the line.
    // Note: For the purpose of the algorithm the starting line is treated as a
    // with a space as its key, making it an invalid identifier for a macro.
    std::unordered_map<std::string, MacroElement> usedMacroMap;
    bool mapChanged = true;
    usedMacroMap[" "] = MacroElement{"<>", "<>", line, StringRef(""), 0, ""};

    // The bodies of all macros currently present in usedMacroMap are examined;
    // every time another macro is found in it, it is added to the map.
    // This process is repeated over and over until it gets to a state when the
    // map does not change after the iteration (this means that all macros
    // already are in it).
    while (mapChanged) {
        mapChanged = false;

        // This vector is used to store the entries that are to be added to the
        // map, but can't be added directly, because it would break the cycle
        // because it is iterating over the same map.
        std::vector<std::pair<StringRef, MacroElement>> entriesToAdd;

        for (auto &Entry : usedMacroMap) {
            // Go through the macro body to find all string that could possibly
            // be macro identifiers. Because we know which characters the
            // identifier starts and ends with, we can go through the string
            // from the left, record all such substrings and test them using the
            // macro map provided in the function arguments.
            std::string macroBody = Entry.second.body.str();
            expandCompositeMacroNames(
                    Entry.second.params, Entry.second.args, macroBody);
            std::string potentialMacroName;

            for (int i = 0; i < macroBody.size(); i++) {
                if (potentialMacroName.size() == 0) {
                    // We are looking for the beginning of an identifier.
                    if (isValidCharForIdentifierStart(macroBody[i]))
                        potentialMacroName += macroBody[i];
                } else if (!isValidCharForIdentifier(macroBody[i])
                           || (i == (macroBody.size() - 1))) {
                    // We found the end of the identifier.
                    if (i == (macroBody.size() - 1)
                        && isValidCharForIdentifier(macroBody[i]))
                        // We found the end of the entire macro - append the
                        // last character, too
                        potentialMacroName += macroBody[i];
                    auto potentialMacro = macroMap.find(potentialMacroName);
                    if (potentialMacro != macroMap.end()) {
                        // Macro used by the currently processed macro was
                        // found.
                        // Add it to entriesToAdd in order for it to be added to
                        // the map.
                        MacroElement macro = potentialMacro->second;
                        macro.parentMacro = Entry.first;

                        // Retrieve macro arguments
                        std::string rawArguments =
                                getSubstringToMatchingBracket(macroBody, i);
                        if (macro.fullName.find("(") != std::string::npos) {
                            // If the full name of the macro contains the
                            // opening bracket, it means the macro has
                            // parameters that can be parsed and put into the
                            // MacroElement structure.
                            std::string rawParameters =
                                    getSubstringToMatchingBracket(
                                            macro.fullName,
                                            macro.fullName.find("("));
                            macro.params = splitArgumentsList(rawParameters);
                        }
                        // Replace parameters from the parent macro with
                        // arguments if the parent macro has parameters.
                        if (!Entry.second.params.empty())
                            rawArguments = expandMacros(Entry.second.params,
                                                        Entry.second.args,
                                                        rawArguments);
                        macro.args = splitArgumentsList(rawArguments);

                        entriesToAdd.push_back(
                                {potentialMacro->first(), macro});
                    }

                    // Reset the potential identifier.
                    potentialMacroName = "";
                } else
                    // We are in the middle of the identifier.
                    potentialMacroName += macroBody[i];
            }
        }

        int originalMapSize = usedMacroMap.size();
        for (std::pair<StringRef, MacroElement> &Entry : entriesToAdd) {
            if (usedMacroMap.find(Entry.first.str()) == usedMacroMap.end()) {
                usedMacroMap[Entry.first.str()] = Entry.second;
                DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                dbgs() << getDebugIndent() << "Adding macro "
                                       << Entry.first << " : "
                                       << Entry.second.body << ", parent macro "
                                       << Entry.second.parentMacro << "\n");
            }
        }

        if (originalMapSize < usedMacroMap.size())
            mapChanged = true;
    }

    return usedMacroMap;
}

/// Takes a list of parameter-argument pairs and expand them on places where
/// are a part of a composite macro name joined by ##.
void expandCompositeMacroNames(std::vector<std::string> params,
                               std::vector<std::string> args,
                               std::string &body) {
    for (auto arg : zip(params, args)) {
        int position = 0;
        while ((position = body.find(std::get<0>(arg) + "##", position))
               != std::string::npos) {
            if (position != 0 && isValidCharForIdentifier(body[position - 1]))
                // Do not replace parts of identifiers.
                continue;
            body.replace(
                    position, std::get<0>(arg).length() + 2, std::get<1>(arg));
            position += std::get<1>(arg).length() + 2;
        }
    }
}

/// Extract the line corresponding to the DILocation from the C source file.
std::string extractLineFromLocation(DILocation *LineLoc, int offset) {
    // Get the path of the source file corresponding to the module where the
    // difference was found
    if (LineLoc == nullptr)
        // Line location was not found
        return "";

    auto sourcePath = getSourceFilePath(dyn_cast<DIScope>(LineLoc->getScope()));

    // Open the C source file corresponding to the location and extract the line
    auto sourceFile = MemoryBuffer::getFile(Twine(sourcePath));
    if (sourceFile.getError()) {
        // Source file was not found, return empty string
        return "";
    }

    // Read the source file by lines, stop at the right number (the line that
    // is referenced by the DILocation)
    // The code also tries to include other lines belonging to the statement by
    // counting parenthesis - in case the line is only a part of the statement,
    // the other parts are added to it.
    line_iterator it(**sourceFile);
    std::string line, previousLine;
    while (!it.is_at_end()
           && it.line_number() != (LineLoc->getLine() + offset)) {
        ++it;
        if (it->count('(') < it->count(')'))
            // The line is a continuation of the previous one
            line += it->str();
        else {
            previousLine = line;
            line = it->str();
        }
    }

    // Detect and fix unfinished bracket expressions.
    if (StringRef(line).count('(') > StringRef(line).count(')')) {
        do {
            ++it;
            line += it->str();
        } while (!it.is_at_end()
                 && (StringRef(line).count(')') < StringRef(line).count('(')));
    }

    // Detect and fix unfinished return expressions.
    std::string lineWithoutWhitespace = line;
    findAndReplace(lineWithoutWhitespace, " ", "");
    findAndReplace(lineWithoutWhitespace, "\t", "");

    if (StringRef(lineWithoutWhitespace).startswith("return")
        && !StringRef(line).contains(";")) {
        do {
            ++it;
            line += it->str();
        } while (!it.is_at_end() && !StringRef(line).contains(";"));
    }

    return line;
}

/// Gets all macros used on a certain DILocation in the form of a key to value
/// map.
std::unordered_map<std::string, MacroElement> getAllMacrosAtLocation(
        DILocation *LineLoc, const Module *Mod, int lineOffset) {
    // Store generated macro maps for modules to avoid having to regenerate them
    // many times when comparing a module that has to be inlined a lot.
    static std::map<const DICompileUnit *, StringMap<MacroElement>>
            MacroMapCache;

    if (!LineLoc || LineLoc->getNumOperands() == 0) {
        // DILocation has no scope or is not present - cannot get macro stack
        DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                        dbgs() << getDebugIndent()
                               << "Scope for macro not found\n");
        return std::unordered_map<std::string, MacroElement>();
    }

    std::string line = extractLineFromLocation(LineLoc, lineOffset);
    if (line == "") {
        // Source file was not found
        DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                        dbgs() << getDebugIndent()
                               << "Source for macro not found\n");
        return std::unordered_map<std::string, MacroElement>();
    }

    DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                    dbgs() << getDebugIndent()
                           << "Looking for all macros on line:" << line
                           << "\n");

    // Get macro array from debug info
    DISubprogram *Sub = LineLoc->getScope()->getSubprogram();
    DIMacroNodeArray RawMacros = Sub->getUnit()->getMacros();

    // Create a map from macro identifiers to their values.
    StringMap<MacroElement> macroMap;

    // Try loading macro map from cache.
    if (MacroMapCache.find(Sub->getUnit()) != MacroMapCache.end())
        macroMap = MacroMapCache[Sub->getUnit()];
    else {
        // Map is not in cache, generate it.

        StringMap<const DIMacroFile *> macroFileMap;
        std::vector<const DIMacroFile *> macroFileStack;

        // First all DIMacroFiles (these represent directly included headers)
        // are added to a stack.
        for (const DIMacroNode *Node : RawMacros) {
            if (const DIMacroFile *File = dyn_cast<DIMacroFile>(Node))
                macroFileStack.push_back(File);
        }

        // Next a DFS algorithm (using the stack created in the previous
        // step) used to add all macros that are found inside the file on
        // the top of the stack to a map and to add all macro files
        // referenced from the top macro file to the stack (these represent
        // indirectly included headers).
        while (!macroFileStack.empty()) {
            const DIMacroFile *MF = macroFileStack.back();
            DIMacroNodeArray A = MF->getElements();
            macroFileStack.pop_back();

            for (const DIMacroNode *Node : A)
                if (const DIMacroFile *File = dyn_cast<DIMacroFile>(Node))
                    // The macro node is another macro file - add it to the
                    // stack
                    macroFileStack.push_back(File);
                else if (const DIMacro *Macro = dyn_cast<DIMacro>(Node)) {
                    // The macro node is an actual macro - add an object
                    // representing it (containing its full name) with its
                    // shortened name as the key.
                    std::string macroName = Macro->getName().str();

                    // If the macro name contains arguments, remove them for
                    // the purpose of the map key.
                    auto position = macroName.find('(');
                    if (position != std::string::npos) {
                        macroName = macroName.substr(0, position);
                    }

                    MacroElement element;
                    element.name = macroName;
                    element.fullName = Macro->getName();
                    element.body = Macro->getValue();
                    element.parentMacro = "N/A";
                    element.sourceFile = MF->getFile()->getFilename().str();
                    element.line = Macro->getLine();
                    macroMap[macroName] = element;
                }
        }
        // Put map into cache.
        MacroMapCache[Sub->getUnit()] = macroMap;
    }

    // Add information about the original line to the map, then return the map
    auto macrosOnLine = getAllMacrosOnLine(line, macroMap);
    macrosOnLine[" "].sourceFile =
            getSourceFilePath(dyn_cast<DIScope>(LineLoc->getScope()));
    macrosOnLine[" "].line = LineLoc->getLine();

    return macrosOnLine;
}

/// Finds macro differences at the locations of the instructions L and R and
/// return them as a vector.
/// This is used when a difference is suspected to be in a macro in order to
/// include that difference into ModuleComparator, and therefore avoid an
/// empty diff.
std::vector<std::unique_ptr<SyntaxDifference>> findMacroDifferences(
        const Instruction *L, const Instruction *R, int lineOffset) {
    // Try to discover a macro difference
    auto MacrosL = getAllMacrosAtLocation(
            L->getDebugLoc(), L->getModule(), lineOffset);
    auto MacrosR = getAllMacrosAtLocation(
            R->getDebugLoc(), R->getModule(), lineOffset);

    std::vector<std::unique_ptr<SyntaxDifference>> result;

    for (auto Elem : MacrosL) {
        if (Elem.second.name == "<>")
            // Note: this is the final parent "macro" element representing the
            // actual line in the source file on which the macro is used
            continue;

        auto LValue = MacrosL.find(Elem.first);
        auto RValue = MacrosR.find(Elem.first);

        if (LValue != MacrosL.end() && RValue != MacrosR.end()
            && LValue->second.body != RValue->second.body) {
            // Macro difference found - get the macro stacks and insert the
            // object into the macro differences array to be passed on to
            // ModuleComparator.
            CallStack StackL, StackR;

            // Insert all macros between the differing macro and the original
            // macro that the line contains to the stack.
            // Note: the lines on which are the macros have to be shifted,
            // because we want the line on which the macro is used, not the line
            // on which it is defined.
            MacroElement currentElement = LValue->second;
            while (currentElement.parentMacro != "") {
                auto parent = MacrosL[currentElement.parentMacro];
                StackL.push_back(CallInfo(currentElement.name + " (macro)",
                                          parent.sourceFile,
                                          parent.line));
                currentElement = parent;
            }
            currentElement = RValue->second;
            while (currentElement.parentMacro != "") {
                auto parent = MacrosR[currentElement.parentMacro];
                StackR.push_back(CallInfo(currentElement.name + " (macro)",
                                          parent.sourceFile,
                                          parent.line));
                currentElement = parent;
            }

            // Invert stacks to match the format of actual call stacks
            std::reverse(StackL.begin(), StackL.end());
            std::reverse(StackR.begin(), StackR.end());

            DEBUG_WITH_TYPE(
                    DEBUG_SIMPLL,
                    dbgs() << getDebugIndent() << "Left stack:\n\t";
                    dbgs() << getDebugIndent() << LValue->second.body << "\n";
                    for (CallInfo &elem
                         : StackL) {
                        dbgs() << getDebugIndent() << "\t\tfrom " << elem.fun
                               << " in file " << elem.file << " on line "
                               << elem.line << "\n";
                    });
            DEBUG_WITH_TYPE(
                    DEBUG_SIMPLL,
                    dbgs() << getDebugIndent() << "Right stack:\n\t";
                    dbgs() << getDebugIndent() << RValue->second.body << "\n";
                    for (CallInfo &elem
                         : StackR) {
                        dbgs() << getDebugIndent() << "\t\tfrom " << elem.fun
                               << " in file " << elem.file << " on line "
                               << elem.line << "\n";
                    });

            result.push_back(std::make_unique<SyntaxDifference>(
                    Elem.first,
                    LValue->second.body,
                    RValue->second.body,
                    StackL,
                    StackR,
                    L->getFunction()->getName()));
        }
    }

    if (result.empty() && lineOffset == 0)
        // There are some cases in which the code cause a difference not on the
        // line where it is, but on the next line (typically volatile vs
        // non-volatile inline assembly). For these cases try comparing the
        // previous line.
        return findMacroDifferences(L, R, -1);

    return result;
}

// Takes a string and the position of the first bracket and returns the
// substring in the brackets.
std::string getSubstringToMatchingBracket(std::string str, size_t position) {
    int bracketCounter = 0;
    std::string output;

    do {
        if (str[position] == '(')
            ++bracketCounter;
        else if (str[position] == ')')
            --bracketCounter;
        output += str[position++];
    } while (position < str.size() && bracketCounter != 0);

    if (position > str.size())
        return "";
    else
        return output;
}

/// Tries to convert C source syntax of inline ASM (the input may include other
/// code, the inline asm is found and extracted) to the LLVM syntax.
/// Returns a pair of strings - the first one contains the converted ASM, the
/// second one contains (unparsed) arguments.
std::pair<std::string, std::string>
        convertInlineAsmToLLVMFormat(std::string input) {
    size_t position = input.find("asm");

    if (position == std::string::npos)
        // No asm present
        return {"", ""};

    // Find the first bracket
    position = input.find('(', position);

    // Opening bracket not found
    if (position == std::string::npos)
        return {"", ""};

    // Extract the asm body
    std::string extractedBody = getSubstringToMatchingBracket(input, position);

    // Closing bracket not found
    // Note: there is a (currently unfixed) case when the inline asm is split
    // onto multiple lines and is not joined properly.
    if (extractedBody == "")
        return {"", ""};

    // Remove the first and last bracket from the expression
    extractedBody = extractedBody.substr(1, extractedBody.size() - 2);

    // Do section joining.
    // Note: a section is a substring inside quotation marks and we want to
    // stop the extraction once a colon is reached.
    std::string newBody;
    int firstQuotationMark;
    int lastQuotationMark = -1;

    // Check if the string has a even number of quotes (if not, it is somehow
    // malformed and should not be analyzed)
    if (std::count(extractedBody.begin(), extractedBody.end(), '\"') % 2 == 1)
        return {"", ""};

    while (extractedBody.find('\"', lastQuotationMark + 1) != std::string::npos
           && extractedBody.find(':', lastQuotationMark + 1)
                      > extractedBody.find('\"', lastQuotationMark + 1)) {
        firstQuotationMark = extractedBody.find('\"', lastQuotationMark + 1);
        lastQuotationMark = extractedBody.find('\"', firstQuotationMark + 1);
        newBody += extractedBody.substr(firstQuotationMark + 1,
                                        lastQuotationMark - firstQuotationMark
                                                - 1);
    }

    // Replaces inline asm argument syntax (assuming there are 20 or less
    // arguments)
    for (int i = 0; i < 20; i++)
        findAndReplace(newBody,
                       "%c" + std::to_string(i),
                       "${" + std::to_string(i) + ":c}");
    // Replace escape sequences
    findAndReplace(newBody, "\\t", "\t");
    findAndReplace(newBody, "\\n", "\n");

    // Extract arguments
    auto colon = extractedBody.find(':', lastQuotationMark + 1);
    if (colon == std::string::npos)
        // No colon - no arguments
        return {newBody, ""};
    std::string arguments = extractedBody.substr(colon);

    return {newBody, arguments};
}

/// Takes a LLVM inline assembly with the corresponding call location and
/// retrieves the corresponding arguments in the C source code.
std::vector<std::string> findInlineAssemblySourceArguments(
        DILocation *LineLoc, const Module *Mod, std::string inlineAsm) {
    // Empty inline asm string cannot be found by the function
    if (inlineAsm == "")
        return {};

    // The function searches for the inline asm at two locations - the first one
    // is the line in the original C source code corresponding to the debug info
    // location, the second one are macros used on that line.
    std::string line = extractLineFromLocation(LineLoc);
    if (line == "")
        return {};
    auto MacroMap = getAllMacrosAtLocation(LineLoc, Mod);

    std::vector<std::string> inputs;
    std::vector<std::pair<std::string, std::string>> candidates;

    // Collect all inputs in which we want to search for the inline asm.
    inputs.push_back(line);
    for (auto Elem : MacroMap)
        inputs.push_back(Elem.second.body);

    // Extract the candidates (i.e. the inputs that contain inline asm).
    for (auto input : inputs) {
        auto output = convertInlineAsmToLLVMFormat(input);

        if (output != std::pair<std::string, std::string>{"", ""})
            candidates.push_back(output);
    }

    // If there is more than one candidate, compare the candidates character by
    // character to the inline asm from LLVM IR and discards a candidate when
    // its character at the current position does not correspond. This is
    // repeated until one or no candidate is left.
    int position = 0;
    while (candidates.size() > 1) {
        auto it = std::remove_if(candidates.begin(),
                                 candidates.end(),
                                 [&position, &inlineAsm](auto &candidate) {
                                     return candidate.first[position]
                                                    != inlineAsm[position]
                                            || candidate.first == "";
                                 });
        candidates.erase(it, candidates.end());

        ++position;
    }

    // If there is no candidate, return empty vector
    if (candidates.size() == 0)
        return {};

    std::string rawArguments = candidates[0].second;

    // Parse the argument list.
    // The arguments are strings inside brackets - each outermost bracket pair
    // contains one of them. getSubstringToMatchingBracket is used to extract
    // them.
    std::vector<std::string> result;
    position = -1;

    while (position < int(rawArguments.size())) {
        position = rawArguments.find('(', position + 1);
        if (position == std::string::npos)
            // There is no additional bracket.
            break;
        std::string argument =
                getSubstringToMatchingBracket(rawArguments, position);
        if (argument.empty() || (argument.size() == 1 && argument[0] == 0)) {
            // Parsing failed, probably because of invalid input
            return {};
        }
        position += argument.size();
        result.push_back(argument.substr(1, argument.size() - 2));
    }

    return result;
}

// Takes in a string with C function call arguments and splits it into a vector.
std::vector<std::string> splitArgumentsList(std::string argumentString) {
    std::vector<std::string> unstrippedArguments;
    std::string currentArgument;

    int bracketCounter = 0, position = 1;
    while (position < argumentString.size()) {
        if (argumentString[position] == '(')
            ++bracketCounter;
        else if (argumentString[position] == ')')
            --bracketCounter;
        if ((bracketCounter == 0 && (argumentString[position] == ','))
            || bracketCounter == -1) {
            // Next argument
            unstrippedArguments.push_back(currentArgument);
            currentArgument = "";
            ++position;
        } else {
            currentArgument += argumentString[position++];
        }
    }

    // Remove whitespace from beginning and end of arguments
    std::vector<std::string> arguments;
    for (std::string arg : unstrippedArguments) {
        if (arg.find_first_not_of(" ") == std::string::npos)
            arguments.push_back(arg);
        else
            arguments.push_back(arg.substr(arg.find_first_not_of(" "),
                                           arg.find_last_not_of(" ")
                                                   - arg.find_first_not_of(" ")
                                                   + 1));
    }

    return arguments;
}

/// Takes a function name with the corresponding call location and retrieves
/// the corresponding arguments in the C source code.
std::vector<std::string> findFunctionCallSourceArguments(
        DILocation *LineLoc, const Module *Mod, std::string functionName) {
    // The function searches for the function call at two locations - the first
    // one is the line in the original C source code corresponding to the debug
    // info location, the second one are macros used on that line.
    std::string line = extractLineFromLocation(LineLoc);
    if (line == "")
        return {};
    auto MacroMap = getAllMacrosAtLocation(LineLoc, Mod);

    std::vector<std::string> inputs;
    std::string argumentString;

    // Collect all inputs in which we want to search for the function call.
    inputs.push_back(line);
    for (auto Elem : MacroMap)
        inputs.push_back(Elem.second.body);

    // Extract the function calls from them
    for (auto input : inputs) {
        std::string identifier, result;
        int i = input.find(functionName);

        if (i == std::string::npos)
            continue;

        argumentString =
                getSubstringToMatchingBracket(input, input.find('(', i));
    }

    return splitArgumentsList(argumentString);
}

/// Expand simple non-argument macros in string. The macros are determined by
/// macro-body pairs.
std::string expandMacros(std::vector<std::string> macros,
                         std::vector<std::string> bodies,
                         std::string Input) {
    std::string Output = Input;
    for (auto Pair : zip(macros, bodies)) {
        findAndReplace(Output, std::get<0>(Pair), std::get<1>(Pair));
    }
    return Output;
}

/// Get number of lines of code of a function.
unsigned countLoc(const Function &Fun) {
    if (auto subprogram = Fun.getSubprogram()) {
        unsigned startLine = subprogram->getLine();
        unsigned endLine = 0;

        for (const auto &BB : Fun) {
            for (const auto &Instr : BB) {
                if (auto debugLoc = Instr.getDebugLoc()) {
                    unsigned line = debugLoc->getLine();
                    if (line > endLine)
                        endLine = line;
                }
            }
        }
        return endLine != 0 ? (endLine - startLine + 1) : 0;
    }
    return 0;
}
