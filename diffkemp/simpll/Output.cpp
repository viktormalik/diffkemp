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
namespace llvm {
namespace yaml {
template <> struct MappingTraits<CallInfo> {
    static void mapping(IO &io, CallInfo &callinfo) {
        io.mapRequired("function", callinfo.fun);
        io.mapRequired("file", callinfo.file);
        io.mapRequired("line", callinfo.line);
        io.mapRequired("weak", callinfo.weak);
    }
};
} // namespace yaml
} // namespace llvm

// Vector of CallInfo objects to YAML (used both for callstacks and sets of
// callees).
LLVM_YAML_IS_SEQUENCE_VECTOR(CallInfo)

// FunctionInfo to YAML
namespace llvm {
namespace yaml {
template <> struct MappingTraits<FunctionInfo> {
    static void mapping(IO &io, FunctionInfo &info) {
        std::string name = info.name;
        io.mapRequired("function", name);
        io.mapOptional("file", info.file);
        io.mapOptional("line", info.line, 0);
        auto calls =
                std::vector<CallInfo>(info.calls.begin(), info.calls.end());
        io.mapOptional("calls", calls);
    }
};
} // namespace yaml
} // namespace llvm

// NonFunctionDifference (stored in unique_ptr) to YAML
namespace llvm {
namespace yaml {
template <> struct MappingTraits<std::unique_ptr<NonFunctionDifference>> {
    static void mapping(IO &io, std::unique_ptr<NonFunctionDifference> &diff) {
        io.mapRequired("name", diff->name);
        io.mapRequired("function", diff->function);
        io.mapRequired("stack-first", diff->StackL);
        io.mapRequired("stack-second", diff->StackR);

        if (auto syntaxDiff = unique_dyn_cast<SyntaxDifference>(diff)) {
            io.mapOptional("body-first", syntaxDiff->BodyL);
            io.mapOptional("body-second", syntaxDiff->BodyR);
        } else if (auto typeDiff = unique_dyn_cast<TypeDifference>(diff)) {
            io.mapOptional("file-first", typeDiff->FileL);
            io.mapOptional("file-second", typeDiff->FileR);
            io.mapOptional("line-first", typeDiff->LineL);
            io.mapOptional("line-second", typeDiff->LineR);
        }
    }
};
} // namespace yaml
} // namespace llvm

LLVM_YAML_IS_SEQUENCE_VECTOR(std::unique_ptr<NonFunctionDifference>)

// Result::Kind to YAML
namespace llvm {
namespace yaml {
template <> struct ScalarEnumerationTraits<Result::Kind> {
    static void enumeration(IO &io, Result::Kind &kind) {
        io.enumCase(kind, "equal", Result::Kind::EQUAL);
        io.enumCase(kind, "not-equal", Result::Kind::NOT_EQUAL);
        io.enumCase(kind, "assumed-equal", Result::Kind::ASSUMED_EQUAL);
        io.enumCase(kind, "unknown", Result::Kind::UNKNOWN);
    }
};
} // namespace yaml
} // namespace llvm

// Result to YAML
namespace llvm {
namespace yaml {
template <> struct MappingTraits<Result> {
    static void mapping(IO &io, Result &result) {
        io.mapRequired("result", result.kind);
        io.mapRequired("first", result.First);
        io.mapRequired("second", result.Second);
        io.mapOptional("differing-objects", result.DifferingObjects);
    }
};
} // namespace yaml
} // namespace llvm

LLVM_YAML_IS_SEQUENCE_VECTOR(Result)

// GlobalValue (used for missing definitions) to YAML
namespace llvm {
namespace yaml {
template <> struct MappingTraits<GlobalValuePair> {
    static void mapping(IO &io, GlobalValuePair &globvals) {
        std::string nameFirst =
                globvals.first ? globvals.first->getName().str() : "";
        io.mapOptional("first", nameFirst, std::string());
        std::string nameSecond =
                globvals.second ? globvals.second->getName().str() : "";
        io.mapOptional("second", nameSecond, std::string());
    }
};
} // namespace yaml
} // namespace llvm

LLVM_YAML_IS_SEQUENCE_VECTOR(GlobalValuePair)

// OverallResult to YAML
namespace llvm {
namespace yaml {
template <> struct MappingTraits<OverallResult> {
    static void mapping(IO &io, OverallResult &result) {
        io.mapOptional("function-results", result.functionResults);
        io.mapOptional("missing-defs", result.missingDefs);
    }
};
} // namespace yaml
} // namespace llvm

/// Report the overall result in YAML format to stdout.
void reportOutput(OverallResult &result) {
    llvm::yaml::Output output(outs());
    output << result;
}

/// Report the overall result in YAML format to a string.
std::string reportOutputToString(OverallResult &result) {
    std::string DumpStr;
    llvm::raw_string_ostream DumpStrm(DumpStr);
    llvm::yaml::Output output(DumpStrm);
    output << result;
    return DumpStrm.str();
}
