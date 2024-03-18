//===------ SMTBlockComparator.h - SMT-based comparison of snippets -------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Frantisek Necas, frantisek.necas@protonmail.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains implementation of SMT-based formal verification of
/// equality of small code snippets.
///
//===----------------------------------------------------------------------===//

#ifndef DIFFKEMP_SMTBLOCKCOMPARATOR_H
#define DIFFKEMP_SMTBLOCKCOMPARATOR_H

#include "Config.h"
#include "DifferentialFunctionComparator.h"
#include "z3++.h"
#include <llvm/IR/BasicBlock.h>

using namespace llvm;

class SMTException : public std::exception {};

/// An exception thrown when an unsupported operation is included in the
/// analyzed snippet.
class UnsupportedOperationException : public SMTException {
  private:
    std::string message;

  public:
    UnsupportedOperationException(const std::string &msg)
            : message("Failed to encode operation into SMT: " + msg) {}
    virtual const char *what() const noexcept { return message.c_str(); }
};

/// An exception thrown when no analyzable code snippets are found, i.e.
/// there are no snippets after which the remaining basic block instructions
/// can be synchronized.
class NoSynchronizationPointException : public SMTException {
  public:
    virtual const char *what() const noexcept {
        return "No synchronization point found";
    }
};

/// An exception thrown when the set timeout is reached.
class OutOfTimeException : public SMTException {
  public:
    virtual const char *what() const noexcept {
        return "SMT solver ran out of time";
    }
};

/// An exception thrown when we fail to establish output variable mapping
/// that is used to construct the SMT postcondition.
class IndistinguishableOutputVarsException : public SMTException {
  public:
    virtual const char *what() const noexcept {
        return "Failed to establish output variable mapping";
    }
};

/// A class for comparing short sequential code snippets using an SMT solver.
class SMTBlockComparator {
  public:
    SMTBlockComparator(const Config &config,
                       DifferentialFunctionComparator *fComp)
            : config(config), fComp(fComp) {}

    /// Compares two code snippets starting from InstL and InstR and tries to
    /// verify if there are two semantically equal sequences of instructions
    /// followed by a synchronization point.
    int compare(BasicBlock::const_iterator &InstL,
                BasicBlock::const_iterator &InstR);

  private:
    unsigned remainingTime = 0;
    DenseMap<const Value *, int> sn_mapL_backup;
    DenseMap<const Value *, int> sn_mapR_backup;
    std::unordered_map<int, std::pair<const Value *, const Value *>>
            mappedValuesBySnBackup;
    inline static const std::string LPrefix = "l_var_";
    inline static const std::string RPrefix = "r_var_";

    const Config &config;
    const DifferentialFunctionComparator *fComp;

    /// Moves InstL and InstR iterators to the end of the snippet to compare.
    /// The snippet must be followed by a synchronized instruction. If no such
    /// instruction is found, the whole basic block is to be considered.
    void findSnippetEnd(BasicBlock::const_iterator &InstL,
                        BasicBlock::const_iterator &InstR);

    /// Compare semantics of snippets <StartL, EndL> and <StartR, EndR>.
    int compareSnippets(BasicBlock::const_iterator &StartL,
                        BasicBlock::const_iterator &EndL,
                        BasicBlock::const_iterator &StartR,
                        BasicBlock::const_iterator &EndR);

    /// Performs the comparison of blocks. This is just a convenience function
    /// to perform cleanup nicely in one place.
    int doCompare(BasicBlock::const_iterator &InstL,
                  BasicBlock::const_iterator &InstR);

    /// Creates a Z3 constant based on the LLVM constant.
    static z3::expr createConstant(z3::context &c, const Constant *constant);

    /// Creates a Z3 variable of the given name and type.
    static z3::expr createVar(z3::context &c, const char *name, Type *type);

    /// Creates a Z3 variable or constant with the given prefix based on
    /// an LLVM value.
    static z3::expr createExprFromValue(z3::context &c,
                                        const std::string &prefix,
                                        const Value *val);

    /// Create a Z3 variable prefixed with the given string that represents
    /// the result register of an LLVM Instruction.
    static z3::expr createResultVar(z3::context &c,
                                    const std::string &prefix,
                                    BasicBlock::const_iterator Inst);

    /// Create Z3 equalities mapping InstL operands based on the current state
    /// of varmap to the variables in the right function.
    void mapOperands(z3::solver &s,
                     z3::context &c,
                     BasicBlock::const_iterator InstL);

    /// Checks whether the given Instruction is an output instruction of the
    /// code snippet ending on Instruction End, i.e. if any of its use is
    /// located after End.
    static bool isOutputVar(BasicBlock::const_iterator Inst,
                            BasicBlock::const_iterator End);

    /// Collects output variables in the given code snippet.
    static std::set<const Value *>
            collectOutputVars(BasicBlock::const_iterator Start,
                              BasicBlock::const_iterator End);

    /// Construct SMT formula postcondition expressing that output variables
    /// must be equal.
    z3::expr constructPostCondition(z3::context &c,
                                    BasicBlock::const_iterator StartL,
                                    BasicBlock::const_iterator EndL,
                                    BasicBlock::const_iterator StartR,
                                    BasicBlock::const_iterator EndR);

    /// Encodes the given LLVM Instruction into a Z3 assertion. Prefixes
    /// operands and the result register with the given prefix.
    void encodeInstruction(z3::solver &s,
                           z3::context &c,
                           const std::string &prefix,
                           BasicBlock::const_iterator Inst);

    /// Encode CMP instruction.
    z3::expr encodeCmpInstruction(z3::context &c,
                                  z3::expr &res,
                                  const std::string &prefix,
                                  const CmpInst *Inst);

    /// Encode a binary operation.
    static z3::expr encodeBinaryOperator(z3::context &c,
                                         z3::expr &res,
                                         const std::string &prefix,
                                         const BinaryOperator *Inst);

    /// Encode an overflowing binary operation. This includes signed/unsigned
    /// add, mul, sub, shl as per the LLVM docs.
    static z3::expr encodeOverflowingBinaryOperator(
            z3::context &c,
            z3::expr &res,
            const std::string &prefix,
            const OverflowingBinaryOperator *Inst);

    /// Encode a call instruction. Note that only some functions are supported.
    static z3::expr encodeFunctionCall(z3::context &c,
                                       z3::expr &res,
                                       const std::string &prefix,
                                       const CallInst *Inst);
};

#endif // DIFFKEMP_SMTBLOCKCOMPARATOR_H
