//===------------------- Output.h - Reporting results ---------------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains classes for reporting results of the simplification.
/// Result is printed as a YAML to stdout.
/// Uses LLVM's YAML library.
///
//===----------------------------------------------------------------------===//

#include "Output.h"
#include <llvm/Support/YAMLTraits.h>

using namespace llvm::yaml;

// CallInfo to YAML
namespace llvm::yaml {
template <> struct MappingTraits<CallInfo> {
    static void mapping(IO &io, CallInfo &callinfo) {
        io.mapRequired("function", callinfo.fun);
        io.mapRequired("file", callinfo.file);
        io.mapRequired("line", callinfo.line);
    }
};
} // namespace llvm::yaml

// CallStack (vector of CallInfo) to YAML
LLVM_YAML_IS_SEQUENCE_VECTOR(CallInfo);

// Info about a single function in a non-equal function pair
struct FunctionInfo {
    std::string name;
    std::string file;
    int line;
    CallStack callstack;
    std::string diffKind;
    bool covered;

    // Default constructor is needed for YAML serialisation so that the struct
    // can be used as an optional YAML field.
    FunctionInfo() {}
    FunctionInfo(const std::string &name,
                 const std::string &file,
                 const CallStack &callstack,
                 std::string diffKind = "function",
                 int line = 0,
                 bool covered = false)
            : name(name), file(file), line(line), callstack(callstack),
              diffKind(diffKind), covered(covered) {}
};

// Syntactic diff body
struct SyndiffBody {
    std::string name, LBody, RBody;
};

// SyndiffBody to YAML
namespace llvm::yaml {
template <> struct MappingTraits<SyndiffBody> {
    static void mapping(IO &io, SyndiffBody &body) {
        io.mapRequired("name", body.name);
        io.mapRequired("left-value", body.LBody);
        io.mapRequired("right-value", body.RBody);
    }
};
} // namespace llvm::yaml

// Vector of MacroBody to YAML
LLVM_YAML_IS_SEQUENCE_VECTOR(SyndiffBody);

// FunctionInfo to YAML
namespace llvm::yaml {
template <> struct MappingTraits<FunctionInfo> {
    static void mapping(IO &io, FunctionInfo &info) {
        std::string name = info.name;
        if (hasSuffix(name)) {
            // Remove LLVM suffix.
            name = name.substr(0, name.find("."));
        }
        io.mapRequired("function", name);
        io.mapOptional("file", info.file);
        if (info.line != 0)
            io.mapOptional("line", info.line);
        io.mapOptional("callstack", info.callstack);
        io.mapRequired("diff-kind", info.diffKind);
        io.mapRequired("covered", info.covered);
    }
};
} // namespace llvm::yaml

// Pair of different functions that will be reported
struct DiffFunPair {
    FunctionInfo first, second;
};

// DiffFunPair to YAML
namespace llvm::yaml {
template <> struct MappingTraits<DiffFunPair> {
    static void mapping(IO &io, DiffFunPair &funs) {
        io.mapOptional("first", funs.first);
        io.mapOptional("second", funs.second);
    }
};
} // namespace llvm::yaml

// Vector of DiffFunPair to YAML
LLVM_YAML_IS_SEQUENCE_VECTOR(DiffFunPair);

// Pair of function names
typedef std::pair<std::string, std::string> MissingDefPair;

// MissingDefPair to YAML
namespace llvm::yaml {
template <> struct MappingTraits<MissingDefPair> {
    static void mapping(IO &io, MissingDefPair &funs) {
        io.mapOptional("first", funs.first);
        io.mapOptional("second", funs.second);
    }
};
} // namespace llvm::yaml

// Overall report: contains pairs of different (non-equal) functions
struct ResultReport {
    std::vector<DiffFunPair> diffFuns;
    std::vector<MissingDefPair> missingDefs;
    std::vector<SyndiffBody> syndiffBodies;
    unsigned analysedLoc;
};

// Report to YAML
namespace llvm::yaml {
template <> struct MappingTraits<ResultReport> {
    static void mapping(IO &io, ResultReport &result) {
        io.mapOptional("diff-functions", result.diffFuns);
        io.mapOptional("missing-defs", result.missingDefs);
        io.mapOptional("syndiff-defs", result.syndiffBodies);
        io.mapOptional("analysed-loc", result.analysedLoc);
    }
};
} // namespace llvm::yaml

void reportOutput(Config &config, ComparisonResult &Result) {
    ResultReport report;

    // Transform non-equal functions into a function name set
    std::set<StringRef> differingFunsSet;
    for (FunPair FP : Result.nonequalFuns) {
        differingFunsSet.insert(FP.first->getName());
        differingFunsSet.insert(FP.second->getName());
    }

    for (auto &nonFunDiff : Result.differingObjects) {
        // Look whether the function the syntactic difference was found in is
        // among functions declared as non-equal.
        // In case it is not, then the syntax difference should not be shown,
        // since the function in which it was found is equal.
        bool skipDiff = false;

        if (Result.nonequalFuns.size() > 0) {
            // The case when nonequalFuns is empty is uninteresting - in that
            // case macro differences are irrelevant
            auto ModuleL = Result.nonequalFuns[0].first->getParent();
            auto ModuleR = Result.nonequalFuns[0].second->getParent();

            // Function has to exist in both modules to prevent mistakes.
            if ((ModuleL->getFunction(nonFunDiff->function) == nullptr)
                || (ModuleR->getFunction(nonFunDiff->function) == nullptr))
                continue;

            if (differingFunsSet.find(nonFunDiff->function)
                == differingFunsSet.end()) {
                skipDiff = true;
            }
        } else {
            skipDiff = true;
        }

        if (skipDiff)
            continue;

        // Try to append call stack of function to the syndiff stack if possible
        CallStack toAppendLeft, toAppendRight;
        for (auto diff : Result.nonequalFuns) {
            // Find the right function diff
            if (diff.first->getName() == nonFunDiff->function) {
                CallStack CS = getCallStack(*config.FirstFun, *diff.first);
                if (!CS.empty())
                    toAppendLeft = CS;
            }
            if (diff.second->getName() == nonFunDiff->function) {
                CallStack CS = getCallStack(*config.SecondFun, *diff.second);
                if (!CS.empty())
                    toAppendRight = CS;
            }
        }
        if (toAppendLeft.size() > 0)
            nonFunDiff->StackL.insert(nonFunDiff->StackL.begin(),
                                      toAppendLeft.begin(),
                                      toAppendLeft.end());
        if (toAppendRight.size() > 0)
            nonFunDiff->StackR.insert(nonFunDiff->StackR.begin(),
                                      toAppendRight.begin(),
                                      toAppendRight.end());

        // Add functions used in call stack to set (also include the parent
        // function)
        for (CallInfo CI : nonFunDiff->StackL)
            Result.coveredFuns.insert(CI.fun);
        for (CallInfo CI : nonFunDiff->StackR)
            Result.coveredFuns.insert(CI.fun);
        Result.coveredFuns.insert(nonFunDiff->function);

        if (auto synDiff = unique_dyn_cast<SyntaxDifference>(nonFunDiff)) {
            report.diffFuns.push_back({FunctionInfo(synDiff->name,
                                                    synDiff->StackL[0].file,
                                                    synDiff->StackL,
                                                    "syntactic"),
                                       FunctionInfo(synDiff->name,
                                                    synDiff->StackR[0].file,
                                                    synDiff->StackR,
                                                    "syntactic")});
            report.syndiffBodies.push_back(
                    SyndiffBody{synDiff->name, synDiff->BodyL, synDiff->BodyR});
        } else if (auto typeDiff =
                           unique_dyn_cast<TypeDifference>(nonFunDiff)) {
            report.diffFuns.push_back({FunctionInfo(typeDiff->name,
                                                    typeDiff->FileL,
                                                    typeDiff->StackL,
                                                    "type",
                                                    typeDiff->LineL),
                                       FunctionInfo(typeDiff->name,
                                                    typeDiff->FileR,
                                                    typeDiff->StackR,
                                                    "type",
                                                    typeDiff->LineR)});
        }
    }
    for (auto &funPair : Result.nonequalFuns) {
        bool covered = Result.coveredFuns.find(funPair.first->getName())
                       != Result.coveredFuns.end();
        report.diffFuns.push_back(
                {FunctionInfo(
                         funPair.first->getName(),
                         getFileForFun(funPair.first),
                         getCallStack(*config.FirstFun, *funPair.first),
                         "function",
                         funPair.first->getSubprogram()
                                 ? funPair.first->getSubprogram()->getLine()
                                 : 0,
                         covered),
                 FunctionInfo(
                         funPair.second->getName(),
                         getFileForFun(funPair.second),
                         getCallStack(*config.SecondFun, *funPair.second),
                         "function",
                         funPair.second->getSubprogram()
                                 ? funPair.second->getSubprogram()->getLine()
                                 : 0,
                         covered)});
    }
    for (auto &funPair : Result.missingDefs) {
        report.missingDefs.emplace_back(
                funPair.first ? funPair.first->getName() : "",
                funPair.second ? funPair.second->getName() : "");
    }

    report.analysedLoc = Result.analysedLoc;

    llvm::yaml::Output output(outs());
    output << report;
}
