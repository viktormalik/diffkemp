//===-------------------- Result.h - Comparison result --------------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Viktor Malik, vmalik@redhat.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains declarations of classes for representation of function
/// comparison result.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SIMPLL_RESULT_H
#define DIFFKEMP_SIMPLL_RESULT_H

#include "Utils.h"
#include <llvm/IR/Function.h>
#include <memory>
#include <set>
#include <string>
#include <vector>

using namespace llvm;

/// Encapsulates statistics about analysis of a single function.
struct FunctionStats {
    unsigned instCnt = 0;
    unsigned instEqualCnt = 0;
    unsigned linesCnt = 0;

    // Default constructor needed for YAML serialisation.
    FunctionStats() {}
};

/// Type for function call information: contains the called function and its
/// call location (file and line).
struct CallInfo {
    std::string fun;
    std::string file;
    unsigned line;
    mutable bool weak;

    // Default constructor needed for YAML serialisation.
    CallInfo() {}
    CallInfo(const std::string &fun, const std::string &file, unsigned int line)
            : fun(fun), file(file), line(line), weak(false) {}
    bool operator<(const CallInfo &Rhs) const { return fun < Rhs.fun; }
};

/// Call stack - list of call entries
typedef std::vector<CallInfo> CallStack;

/// Type for information about a single function. Contains the function name,
/// definition location (file and line), and a list of called functions (in the
/// form of a set of CallInfo objects).
struct FunctionInfo {
    std::string name;
    std::string file;
    int line;
    FunctionStats stats;
    std::set<CallInfo> calls;

    // Default constructor is needed for YAML serialisation so that the struct
    // can be used as an optional YAML field.
    FunctionInfo() {}
    FunctionInfo(const std::string &name,
                 const std::string &file,
                 int line,
                 std::set<CallInfo> calls = {})
            : name(name), file(file), line(line), calls(std::move(calls)) {}

    /// Add a new function call.
    /// @param Callee Called function.
    /// @param Call line.
    void addCall(const Function *Callee, int l) {
        calls.insert(CallInfo(Callee->getName().str(), file, l));
    }
};

/// Type for storing informations about definition of an analysed
// 'object' (eg. macro).
struct Definition {
    // Name of the 'object'.
    std::string name;
    // Line in the sourceFile on which is located definition of the 'object'.
    int line;
    // A C source file name in which is located the 'object' definition.
    std::string sourceFile;
    Definition() = default;
    Definition(std::string name, int line, std::string sourceFile)
            : name(name), line(line), sourceFile(sourceFile) {}
};

/// Generic type for non-function differences.
struct NonFunctionDifference {
    /// Discriminator for LLVM-style RTTI (dyn_cast<> et al.)
    enum DiffKind { SynDiff, TypeDiff };
    /// Name of the object.
    std::string name;
    /// Stacks containing the differing objects and all other objects affected
    /// by the difference (for both modules).
    CallStack StackL, StackR;
    /// The function in which the difference was found
    std::string function;
    NonFunctionDifference(DiffKind Kind) : Kind(Kind){};
    NonFunctionDifference(std::string name,
                          CallStack StackL,
                          CallStack StackR,
                          std::string function,
                          DiffKind Kind)
            : name(name), StackL(StackL), StackR(StackR), function(function),
              Kind(Kind) {}
    DiffKind getKind() const { return Kind; }

  private:
    const DiffKind Kind;
};

/// Syntactic difference between objects that cannot be found in the original
/// source files.
/// There are multiple kinds of the differences: a macro diff,
/// inline assembly diff, function-macro diff or macro-function diff.
struct SyntaxDifference : public NonFunctionDifference {
    enum SyntaxKind {
        MACRO,
        ASSEMBLY,
        FUNCTION_MACRO,
        MACRO_FUNCTION,
        UNKNOWN
    };
    SyntaxKind syntaxKind;
    /// The difference.
    std::string BodyL, BodyR;
    // Informations about definitions of last 'object' (macro) in call stack.
    // For now it is used in macro, function-macro, macro-function differences.
    std::unique_ptr<Definition> lastDefL, lastDefR;
    SyntaxDifference()
            : NonFunctionDifference(SynDiff), syntaxKind(SyntaxKind::UNKNOWN){};
    SyntaxDifference(SyntaxKind syntaxKind,
                     std::string name,
                     std::string BodyL,
                     std::string BodyR,
                     CallStack StackL,
                     CallStack StackR,
                     std::string function,
                     std::unique_ptr<Definition> lastDefL,
                     std::unique_ptr<Definition> lastDefR)
            : NonFunctionDifference(name, StackL, StackR, function, SynDiff),
              syntaxKind(syntaxKind), BodyL(BodyL), BodyR(BodyR),
              lastDefL(std::move(lastDefL)), lastDefR(std::move(lastDefR)) {}
    static bool classof(const NonFunctionDifference *Diff) {
        return Diff->getKind() == SynDiff;
    }

  private:
    DiffKind kind;
};

/// Represents a difference between structure types (the actual diff is
/// generated by diffkemp in a way similar to functions diffs).
struct TypeDifference : public NonFunctionDifference {
    /// The files and lines where the type definitions are (for both modules).
    std::string FileL, FileR;
    int LineL, LineR;
    TypeDifference() : NonFunctionDifference(TypeDiff) {}
    static bool classof(const NonFunctionDifference *Diff) {
        return Diff->getKind() == TypeDiff;
    }

  private:
    DiffKind kind;
};

/// Result of comparison of a pair of functions.
/// Contains the result kind (equal, not equal, or unknown), information about
/// the compared functions, and a list of non-function objects that may cause
/// difference between the functions (such as macros, inline assembly code, or
/// types).
class Result {
  public:
    /// Possible results of function comparison.
    enum Kind { EQUAL, ASSUMED_EQUAL, NOT_EQUAL, UNKNOWN };

    Kind kind = UNKNOWN;
    FunctionInfo First;
    FunctionInfo Second;

    std::vector<std::unique_ptr<NonFunctionDifference>> DifferingObjects;

    // Default constructor needed for YAML serialisation.
    Result() {}
    Result(Function *FirstFun, Function *SecondFun);

    /// Add new differing object.
    void addDifferingObject(std::unique_ptr<NonFunctionDifference> Object);
    /// Add multiple SyntaxDifference objects.
    void addDifferingObjects(
            std::vector<std::unique_ptr<SyntaxDifference>> &&Object);
    /// Add multiple TypeDifference objects.
    void addDifferingObjects(
            std::vector<std::unique_ptr<TypeDifference>> &&Object);
};

/// The overall results containing results of all compared function pairs and a
/// list of missing definitions.
struct OverallResult {
    std::vector<Result> functionResults;
    std::vector<GlobalValuePair> missingDefs;
};

#endif // DIFFKEMP_SIMPLL_RESULT_H
