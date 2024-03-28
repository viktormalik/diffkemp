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
#include "Logger.h"
#include "Utils.h"
#include <deque>
#include <llvm/ADT/STLExtras.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/LineIterator.h>
#include <llvm/Support/raw_ostream.h>

/// Gets all macros used on a certain DILocation in the form of a StringMap
/// mapping macro names to MacroUse objects
void MacroDiffAnalysis::collectMacroUsesAtLocation(
        DILocation *Loc, const StringMap<MacroDef> &macroDefs, int lineOffset) {

    if (MacroUsesAtLocation.find(Loc) == MacroUsesAtLocation.end())
        MacroUsesAtLocation.emplace(Loc, StringMap<MacroUse>());

    std::string line = extractLineFromLocation(Loc, lineOffset);
    if (line.empty()) {
        // Source line was not found
        LOG_VERBOSE_EXTRA("Source for macro not found\n");
        return;
    }

    LOG_VERBOSE_EXTRA("Looking for all macros on line:" << line << "\n");

    StringMap<MacroUse> &ResultMacroUses = MacroUsesAtLocation.at(Loc);

    // Search for all macros used at the line. The algorithm uses a queue to
    // store strings that must be explored. Initially, the queue contains the
    // current line and every time a macro identifier is found, the
    // corresponding macro body is added to the queue.
    std::deque<std::pair<std::string, const MacroUse *>> toExpand;
    toExpand.emplace_front(line, nullptr);
    while (!toExpand.empty()) {
        auto macroToExpand = toExpand.back();
        toExpand.pop_back();

        std::string macroBody = macroToExpand.first;
        const MacroUse *parentMacroUse = macroToExpand.second;

        if (parentMacroUse)
            expandCompositeMacroNames(parentMacroUse->def->params,
                                      parentMacroUse->args,
                                      macroBody);

        std::string potentialMacroName;
        // Go through the macro body to find all strings that could possibly
        // be macro identifiers. This is basically a FSM matching all valid C
        // identifiers.
        for (unsigned long i = 0; i < macroBody.size(); i++) {
            if (potentialMacroName.empty()) {
                // We are looking for the beginning of an identifier.
                if (isValidCharForIdentifierStart(macroBody[i]))
                    potentialMacroName += macroBody[i];
            } else if (!isValidCharForIdentifier(macroBody[i])
                       || (i == (macroBody.size() - 1))) {
                // We found the end of the identifier.
                if (i == (macroBody.size() - 1)
                    && isValidCharForIdentifier(macroBody[i]))
                    // We found the end of the entire macro - append the last
                    // character, too
                    potentialMacroName += macroBody[i];
                // Check if the identifier is a macro - try to find a
                // corresponding macro defintion.
                auto potentialMacroDef = macroDefs.find(potentialMacroName);
                if (ResultMacroUses.find(potentialMacroName)
                            == ResultMacroUses.end()
                    && potentialMacroDef != macroDefs.end()) {
                    // Macro used by the currently processed macro was found.
                    MacroUse newMacroUse;
                    newMacroUse.def = &potentialMacroDef->second;
                    newMacroUse.parent = parentMacroUse;
                    // Use location is either the current line (for the directly
                    // used macro) or the location of the definition of macro
                    // which uses the current macro.
                    newMacroUse.line = parentMacroUse
                                               ? parentMacroUse->def->line
                                               : Loc->getLine();
                    newMacroUse.sourceFile =
                            parentMacroUse ? parentMacroUse->def->sourceFile
                                           : getSourceFilePath(Loc->getScope());

                    // Retrieve macro arguments
                    std::string rawArguments =
                            getSubstringToMatchingBracket(macroBody, i);
                    // Replace parameters from the parent macro with arguments
                    // if the parent macro has parameters.
                    if (parentMacroUse && !parentMacroUse->def->params.empty())
                        rawArguments = expandMacros(parentMacroUse->def->params,
                                                    parentMacroUse->args,
                                                    rawArguments);
                    newMacroUse.args = splitArgumentsList(rawArguments);

                    auto inserted = ResultMacroUses.insert(
                            {potentialMacroDef->second.name, newMacroUse});
                    if (inserted.second) {
                        // If the macro use is new (it was not in the result
                        // map, yet), add its body into the toExpand queue.
                        LOG_VERBOSE_EXTRA(
                                "Adding macro "
                                << newMacroUse.def->name << " : "
                                << newMacroUse.def->body << ", parent macro "
                                << (parentMacroUse ? parentMacroUse->def->name
                                                   : "")
                                << "\n");
                        toExpand.emplace_back(newMacroUse.def->body.str(),
                                              &inserted.first->second);
                    }
                }

                // Reset the potential identifier.
                potentialMacroName = "";
            } else
                // We are in the middle of the identifier.
                potentialMacroName += macroBody[i];
        }
    }
}

/// Takes a list of parameter-argument pairs and expand them on places where
/// are a part of a composite macro name joined by ##.
void expandCompositeMacroNames(std::vector<std::string> params,
                               std::vector<std::string> args,
                               std::string &body) {
    for (auto arg : zip(params, args)) {
        unsigned long position = 0;
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

/// Get all macros used on a certain DILocation in the form of a StringMap
/// mapping macro names to MacroUse objects
const StringMap<MacroUse> &
        MacroDiffAnalysis::getAllMacroUsesAtLocation(DILocation *Loc,
                                                     int lineOffset) {
    if (!Loc || Loc->getNumOperands() == 0) {
        // DILocation has no scope or is not present - cannot get macro stack
        LOG_VERBOSE_EXTRA("Scope for macro not found\n");
        MacroUsesAtLocation.emplace(Loc, StringMap<MacroUse>());
        return MacroUsesAtLocation.at(Loc);
    }

    // Get macro definitions (collect them if they do not exist)
    auto compileUnit = Loc->getScope()->getSubprogram()->getUnit();
    if (MacroDefMaps.find(compileUnit) == MacroDefMaps.end()) {
        collectMacroDefs(compileUnit);
    }
    const auto &macroDefMap = MacroDefMaps.at(compileUnit);

    // Collect macro usages if they do not exist (or if the line offset is
    // non-zero) and return them
    if (MacroUsesAtLocation.find(Loc) == MacroUsesAtLocation.end()
        || lineOffset != 0) {
        collectMacroUsesAtLocation(Loc, macroDefMap, lineOffset);
    }
    return MacroUsesAtLocation.at(Loc);
}

/// Find macro differences at the locations of the instructions L and R and
/// return them as a vector.
/// This is used when a difference is suspected to be in a macro in order to
/// include that difference into ModuleComparator, and therefore avoid an
/// empty diff.
std::vector<std::unique_ptr<SyntaxDifference>>
        MacroDiffAnalysis::findMacroDifferences(const Instruction *L,
                                                const Instruction *R,
                                                int lineOffset) {
    // Try to discover a macro difference
    auto &MacroUsesL = getAllMacroUsesAtLocation(L->getDebugLoc(), lineOffset);
    auto &MacroUsesR = getAllMacroUsesAtLocation(R->getDebugLoc(), lineOffset);

    std::vector<std::unique_ptr<SyntaxDifference>> result;

    for (const auto &MacroUseL : MacroUsesL) {
        auto MacroUseR = MacroUsesR.find(MacroUseL.first());

        if (MacroUseR != MacroUsesR.end()
            && MacroUseL.second.def->body != MacroUseR->second.def->body) {
            // Macro difference found - get the macro stacks and insert the
            // object into the macro differences array to be passed on to
            // ModuleComparator.
            CallStack StackL, StackR;

            // Insert all macros between the differing macro and the original
            // macro that the line contains to the stack. The MacroUse object of
            // the originally used macro has a nullptr parent.
            for (const MacroUse *m = &MacroUseL.second; m != nullptr;
                 m = m->parent) {
                StackL.push_back(CallInfo(
                        m->def->name + " (macro)", m->sourceFile, m->line));
            }
            for (const MacroUse *m = &MacroUseR->second; m != nullptr;
                 m = m->parent) {
                StackR.push_back(CallInfo(
                        m->def->name + " (macro)", m->sourceFile, m->line));
            }

            // Invert stacks to match the format of actual call stacks
            std::reverse(StackL.begin(), StackL.end());
            std::reverse(StackR.begin(), StackR.end());

            LOG_VERBOSE_EXTRA("Left stack:\n\t");
            LOG_VERBOSE_EXTRA(MacroUseL.second.def->body << "\n");
            for (CallInfo &elem : StackL) {
                LOG_VERBOSE_EXTRA("\t\tfrom " << elem.fun << " in file "
                                              << elem.file << " on line "
                                              << elem.line << "\n");
            }
            LOG_VERBOSE_EXTRA("Right stack:\n\t");
            LOG_VERBOSE_EXTRA(MacroUseR->second.def->body << "\n");
            for (CallInfo &elem : StackR) {
                LOG_VERBOSE_EXTRA("\t\tfrom " << elem.fun << " in file "
                                              << elem.file << " on line "
                                              << elem.line << "\n");
            };
            // Copying info about last macros definition for the 'user' so he
            // knows where the difference was found (eg. for showing diff).
            auto lastDefL = std::make_unique<Definition>(
                    MacroUseL.second.def->name,
                    MacroUseL.second.def->line,
                    MacroUseL.second.def->sourceFile);
            auto lastDefR = std::make_unique<Definition>(
                    MacroUseR->second.def->name,
                    MacroUseR->second.def->line,
                    MacroUseR->second.def->sourceFile);
            result.push_back(std::make_unique<SyntaxDifference>(
                    SyntaxDifference::SyntaxKind::MACRO,
                    MacroUseL.second.def->name,
                    MacroUseL.second.def->body.str(),
                    MacroUseR->second.def->body.str(),
                    StackL,
                    StackR,
                    L->getFunction()->getName().str(),
                    std::move(lastDefL),
                    std::move(lastDefR)));
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

/// Collect all macros defined in the given compile unit and store them into
/// MacroDefMaps
void MacroDiffAnalysis::collectMacroDefs(DICompileUnit *CompileUnit) {
    // Get macros definitions from debug info
    DIMacroNodeArray RawMacros = CompileUnit->getMacros();
    StringMap<const DIMacroFile *> macroFileMap;
    std::vector<const DIMacroFile *> macroFileStack;

    StringMap<MacroDef> macroDefs;

    // First all DIMacroFiles (these represent directly included headers)
    // are added to a stack.
    for (const DIMacroNode *Node : RawMacros) {
        if (const DIMacroFile *File = dyn_cast<DIMacroFile>(Node))
            macroFileStack.push_back(File);
    }

    // Next a DFS algorithm (using the stack created in the previous step) used
    // to add all macro definitions that are found inside the file on the top of
    // the stack to a map and to add all macro files referenced from the top
    // macro file to the stack (these represent indirectly included headers).
    while (!macroFileStack.empty()) {
        const DIMacroFile *MF = macroFileStack.back();
        DIMacroNodeArray A = MF->getElements();
        macroFileStack.pop_back();

        for (const DIMacroNode *Node : A)
            if (const DIMacroFile *File = dyn_cast<DIMacroFile>(Node))
                // The macro node is another macro file - add it to the stack
                macroFileStack.push_back(File);
            else if (const DIMacro *Macro = dyn_cast<DIMacro>(Node)) {
                // The macro node is an actual macro definition - add an object
                // representing it (containing its full name) with its shortened
                // name as the key to the result map.
                std::string macroName = Macro->getName().str();

                // If the macro name contains parameters, remove them for the
                // purpose of the map key.
                auto position = macroName.find('(');
                if (position != std::string::npos) {
                    macroName = macroName.substr(0, position);
                }

                MacroDef element;
                element.name = macroName;
                element.fullName = Macro->getName().str();
                element.body = Macro->getValue();
                element.sourceFile = MF->getFile()->getFilename().str();
                element.line = Macro->getLine();

                if (element.fullName.find('(') != std::string::npos) {
                    // If the full name of the macro contains the opening
                    // bracket, it means the macro has parameters that can be
                    // parsed and put into the MacroElement structure.
                    std::string rawParameters = getSubstringToMatchingBracket(
                            element.fullName, element.fullName.find('('));
                    element.params = splitArgumentsList(rawParameters);
                }

                macroDefs.insert({element.name, element});
            }
    }
    // Put the created macro definition map into cache
    MacroDefMaps.emplace(CompileUnit, std::move(macroDefs));
}

// Takes a string and the position of the first bracket and returns the
// substring in the brackets.
std::string getSubstringToMatchingBracket(std::string str, size_t position) {
    if (position == std::string::npos)
        return "";
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
std::vector<std::string>
        findInlineAssemblySourceArguments(DILocation *LineLoc,
                                          std::string inlineAsm,
                                          MacroDiffAnalysis *MacroDiffs) {
    // Empty inline asm string cannot be found by the function
    if (inlineAsm == "")
        return {};

    // The function searches for the inline asm at two locations - the first one
    // is the line in the original C source code corresponding to the debug info
    // location, the second one are macros used on that line.
    std::string line = extractLineFromLocation(LineLoc);
    if (line == "")
        return {};
    auto MacroMap = MacroDiffs->getAllMacroUsesAtLocation(LineLoc, 0);

    std::vector<std::string> inputs;
    std::vector<std::pair<std::string, std::string>> candidates;

    // Collect all inputs in which we want to search for the inline asm.
    inputs.push_back(line);
    for (auto &Elem : MacroMap)
        inputs.push_back(Elem.second.def->body.str());

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
    long position = 0;
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

    while (position < long(rawArguments.size())) {
        position = rawArguments.find('(', position + 1);
        if (position == long(std::string::npos))
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

    unsigned long position = 1;
    int bracketCounter = 0;
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
std::vector<std::string>
        findFunctionCallSourceArguments(DILocation *LineLoc,
                                        std::string functionName,
                                        MacroDiffAnalysis *MacroDiffs) {
    // The function searches for the function call at two locations - the first
    // one is the line in the original C source code corresponding to the debug
    // info location, the second one are macros used on that line.
    std::string line = extractLineFromLocation(LineLoc);
    if (line == "")
        return {};
    auto &MacroMap = MacroDiffs->getAllMacroUsesAtLocation(LineLoc, 0);

    std::vector<std::string> inputs;
    std::string argumentString;

    // Collect all inputs in which we want to search for the function call.
    inputs.push_back(line);
    for (auto &Elem : MacroMap)
        inputs.push_back(Elem.second.def->body.str());

    // Extract the function calls from them
    for (auto input : inputs) {
        std::string identifier, result;
        unsigned long i = input.find(functionName);

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
