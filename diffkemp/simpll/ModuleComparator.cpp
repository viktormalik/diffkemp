//===------------ ModuleComparator.cpp - Comparing LLVM modules -----------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains definitions of methods of the ModuleComparator class that
/// can be used for semantic comparison of two LLVM modules.
///
//===----------------------------------------------------------------------===//

#include "ModuleComparator.h"
#include "Config.h"
#include "DifferentialFunctionComparator.h"
#include "Utils.h"
#include "passes/FunctionAbstractionsGenerator.h"
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/Cloning.h>

/// Semantic comparison of functions.
/// Function declarations are equal if they have the same name.
/// Functions with body are compared using custom FunctionComparator that
/// is designed for comparing functions between different modules.
void ModuleComparator::compareFunctions(Function *FirstFun,
                                        Function *SecondFun) {
    DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                    dbgs() << getDebugIndent() << "Comparing \""
                           << FirstFun->getName() << "\" and \""
                           << SecondFun->getName() << "\" { ";
                    increaseDebugIndentLevel(););
    ComparedFuns.emplace(std::make_pair(FirstFun, SecondFun),
                         Result(FirstFun, SecondFun));

    // Check if the functions is in the ignored list.
    if (ResCache.isFunctionPairCached(FirstFun, SecondFun)) {
        DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                        dbgs() << getDebugIndent() << "ignored }\n");
        ComparedFuns.at({FirstFun, SecondFun}).kind = Result::UNKNOWN;
        return;
    }

    // Comparing function declarations (function without bodies).
    if (FirstFun->isDeclaration() || SecondFun->isDeclaration()) {
        // Drop suffixes of function names. This is necessary in order to
        // successfully compare an original void-returning function with one
        // generated by RemoveUnusedReturnValuesPass, which will have a number
        // suffix.
        auto FirstFunName = FirstFun->getName().str();
        if (hasSuffix(FirstFunName))
            FirstFunName = dropSuffix(FirstFunName);
        auto SecondFunName = SecondFun->getName().str();
        if (hasSuffix(SecondFunName))
            SecondFunName = dropSuffix(SecondFunName);

        if (config.ControlFlowOnly) {
            // If checking control flow only, it suffices that one of the
            // functions is a declaration to treat them equal.
            if (FirstFunName == SecondFunName)
                ComparedFuns.at({FirstFun, SecondFun}).kind =
                        Result::ASSUMED_EQUAL;
            else
                ComparedFuns.at({FirstFun, SecondFun}).kind = Result::NOT_EQUAL;
        } else {
            if (FirstFun->isDeclaration() && SecondFun->isDeclaration()
                && FirstFunName == SecondFunName)
                ComparedFuns.at({FirstFun, SecondFun}).kind =
                        Result::ASSUMED_EQUAL;
            else if (FirstFunName != SecondFunName)
                ComparedFuns.at({FirstFun, SecondFun}).kind = Result::NOT_EQUAL;
            else {
                // One function has a body, the second one does not; add the
                // missing definition
                if (FirstFunName == SecondFunName)
                    ComparedFuns.at({FirstFun, SecondFun}).kind =
                            Result::ASSUMED_EQUAL;
                if (FirstFun->isDeclaration())
                    this->MissingDefs.push_back({FirstFun, nullptr});
                else if (SecondFun->isDeclaration())
                    this->MissingDefs.push_back({nullptr, SecondFun});
            }
        }

        DEBUG_WITH_TYPE(
                DEBUG_SIMPLL,

                decreaseDebugIndentLevel();
                switch (ComparedFuns.at({FirstFun, SecondFun}).kind) {
                    case Result::NOT_EQUAL:
                        dbgs() << "declaration, names are "
                               << Color::makeRed("not equal") << " }\n";
                        break;
                    case Result::ASSUMED_EQUAL:
                        dbgs() << "declaration, "
                               << Color::makeGreen("assumed equal") << " }\n";
                });

        return;
    }
    DEBUG_WITH_TYPE(DEBUG_SIMPLL, dbgs() << "\n");

    // Comparing functions with bodies using custom FunctionComparator.
    DifferentialFunctionComparator fComp(FirstFun, SecondFun, config, DI, this);
    int result = fComp.compare();

    DEBUG_WITH_TYPE(DEBUG_SIMPLL, decreaseDebugIndentLevel());
    if (result == 0) {
        DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                        dbgs() << getDebugIndent() << "} "
                               << Color::makeGreen("equal\n"););
        ComparedFuns.at({FirstFun, SecondFun}).kind = Result::EQUAL;
    } else {
        DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                        dbgs() << getDebugIndent() << "} "
                               << Color::makeRed("not equal\n")
                               << Color::makeRed("========== ")
                               << "Found difference between \""
                               << FirstFun->getName() << "\" and \""
                               << SecondFun->getName() << "\""
                               << Color::makeRed(" ==========\n"););
        ComparedFuns.at({FirstFun, SecondFun}).kind = Result::NOT_EQUAL;

        while (tryInline.first || tryInline.second) {
            DEBUG_WITH_TYPE(DEBUG_SIMPLL, increaseDebugIndentLevel());

            // Try to inline the problematic function calls
            CallInst *inlineFirst = findCallInst(tryInline.first, FirstFun);
            CallInst *inlineSecond = findCallInst(tryInline.second, SecondFun);

            ConstFunPair missingDefs;
            bool inlined = false;
            Function *InlinedFunFirst =
                    !inlineFirst ? nullptr : getCalledFunction(inlineFirst);
            Function *InlinedFunSecond =
                    !inlineSecond ? nullptr : getCalledFunction(inlineSecond);

            // If the called function is a declaration, add it to missingDefs.
            // Otherwise, inline the call and simplify the function.
            // The above is done for the first and the second call to inline.
            if (inlineFirst) {
                const Function *toInline = getCalledFunction(inlineFirst);

                DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                dbgs() << getDebugIndent() << "Inlining \""
                                       << toInline->getName()
                                       << "\" in first\n");
                if (toInline->isDeclaration()) {
                    DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                    dbgs() << getDebugIndent()
                                           << "Missing definition\n");
                    if (!toInline->isIntrinsic()
                        && !isSimpllAbstraction(toInline))
                        missingDefs.first = toInline;
                } else {
                    InlineFunctionInfo ifi;
                    if (inlineCall(inlineFirst))
                        inlined = true;
                }
            }
            if (inlineSecond) {
                const Function *toInline = getCalledFunction(inlineSecond);
                DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                dbgs() << getDebugIndent() << "Inlining \""
                                       << toInline->getName()
                                       << "\" in second\n");
                if (toInline->isDeclaration()) {
                    DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                    dbgs() << getDebugIndent()
                                           << "Missing definition\n");
                    if (!toInline->isIntrinsic()
                        && !isSimpllAbstraction(toInline))
                        missingDefs.second = toInline;
                } else {
                    InlineFunctionInfo ifi;
                    if (inlineCall(inlineSecond))
                        inlined = true;
                }
            }
            // If some function to be inlined does not have a declaration,
            // store it into MissingDefs (will be reported at the end).
            if (missingDefs.first || missingDefs.second) {
                MissingDefs.push_back(missingDefs);
            }
            tryInline = {nullptr, nullptr};
            // If nothing was inlined, do not continue
            if (!inlined) {
                DEBUG_WITH_TYPE(DEBUG_SIMPLL, decreaseDebugIndentLevel());
                break;
            }
            simplifyFunction(FirstFun);
            simplifyFunction(SecondFun);
            // Reset the function diff result
            ComparedFuns.at({FirstFun, SecondFun}).kind = Result::UNKNOWN;
            // Re-run the comparison
            DifferentialFunctionComparator fCompSecond(
                    FirstFun, SecondFun, config, DI, this);
            result = fCompSecond.compare();
            // If the functions are equal after the inlining and there is a
            // call to the inlined function, mark it as weak.
            if (!result) {
                if (InlinedFunFirst)
                    for (const CallInfo &CI :
                         ComparedFuns.at({FirstFun, SecondFun}).First.calls) {
                        if (CI.fun == InlinedFunFirst->getName().str())
                            CI.weak = true;
                    }
                if (InlinedFunSecond)
                    for (const CallInfo &CI :
                         ComparedFuns.at({FirstFun, SecondFun}).Second.calls) {
                        if (CI.fun == InlinedFunSecond->getName().str())
                            CI.weak = true;
                    }
            }

            DEBUG_WITH_TYPE(DEBUG_SIMPLL, decreaseDebugIndentLevel());
            if (result == 0) {
                DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                dbgs() << getDebugIndent() << "\""
                                       << FirstFun->getName() << "\" and \""
                                       << SecondFun->getName() << "\" "
                                       << Color::makeGreen("equal")
                                       << " after inlining\n");
                ComparedFuns.at({FirstFun, SecondFun}).kind = Result::EQUAL;
            } else {
                DEBUG_WITH_TYPE(DEBUG_SIMPLL,
                                dbgs() << getDebugIndent() << "\""
                                       << FirstFun->getName() << "\" and \""
                                       << SecondFun->getName() << "\" still "
                                       << Color::makeRed("not equal")
                                       << " after inlining\n");
                ComparedFuns.at({FirstFun, SecondFun}).kind = Result::NOT_EQUAL;
            }
        }
    }
}
