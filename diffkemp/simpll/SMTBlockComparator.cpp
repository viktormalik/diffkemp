//===----- SMTBlockComparator.cpp - SMT-based comparison of snippets ------===//
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

#include "SMTBlockComparator.h"
#include "z3++.h"
#include "llvm/IR/Instruction.def"
#include <ctime>
#include <llvm/IR/Operator.h>
#include <sstream>
#if LLVM_VERSION_MAJOR < 11
#include "llvm/Analysis/OrderedBasicBlock.h"
#endif

using namespace llvm;

void SMTBlockComparator::findSnippetEnd(BasicBlock::const_iterator &InstL,
                                        BasicBlock::const_iterator &InstR) {
    auto BBL = const_cast<BasicBlock *>(InstL->getParent());
    auto BBR = const_cast<BasicBlock *>(InstR->getParent());
    auto StartR = InstR;

    while (InstL != BBL->end()) {
        if (fComp->maySkipInstruction(&*InstL) || isDebugInfo(*InstL)) {
            InstL++;
            continue;
        }

        // Try to find a matching instruction on the right
        InstR = StartR;
        while (InstR != BBR->end()) {
            if (fComp->maySkipInstruction(&*InstR) || isDebugInfo(*InstR)) {
                InstR++;
                continue;
            }

            // We want to ensure that the rest of the instructions in the basic
            // blocks are synchronized. Since we are using the same fComp
            // instance that is a caller of this method, we want to avoid
            // recursive SMT solver calls. Relocations could also modify the
            // underlying state, avoid them as well.
            sn_mapL_backup = fComp->sn_mapL;
            sn_mapR_backup = fComp->sn_mapR;
            mappedValuesBySnBackup = fComp->mappedValuesBySn;
            // Backup the inlining data -- we will want to restore it if
            // the snippets are found to be unequal since otherwise, wrong
            // inlining would be done.
            auto tryInlineBackup = fComp->ModComparator->tryInline;
            if (fComp->cmpBasicBlocksFromInstructions(
                        BBL, BBR, InstL, InstR, true)
                == 0) {
                // Found a synchronization point
                return;
            }
            fComp->ModComparator->tryInline = tryInlineBackup;
            fComp->sn_mapL = sn_mapL_backup;
            fComp->sn_mapR = sn_mapR_backup;
            fComp->mappedValuesBySn = mappedValuesBySnBackup;
            InstR++;
        }
        InstL++;
    }

    throw NoSynchronizationPointException();
}

z3::expr SMTBlockComparator::createVar(z3::context &c,
                                       const char *name,
                                       Type *type) {
    if (type->isDoubleTy()) {
        return c.fpa_const(name, 11, 53);
    } else if (type->isFloatTy()) {
        return c.fpa_const(name, 8, 24);
    } else if (type->isIntegerTy()) {
        if (type->getIntegerBitWidth() == 1) {
            return c.bool_const(name);
        } else {
            return c.bv_const(name, type->getIntegerBitWidth());
        }
    }
    std::string msg = "Unsupported operand type " + typeToString(type);
    throw UnsupportedOperationException(msg);
}

z3::expr SMTBlockComparator::createConstant(z3::context &c,
                                            const Constant *constant) {
    if (constant->getType()->isIntegerTy()) {
        auto value = dyn_cast<ConstantInt>(constant)->getSExtValue();
        auto bitWidth = constant->getType()->getIntegerBitWidth();
        if (bitWidth == 1) {
            return c.bool_val(value);
        } else {
            return c.bv_val(value, bitWidth);
        }
    } else if (constant->getType()->isFloatTy()) {
#if LLVM_VERSION_MAJOR < 11
        return c.fpa_val(
                dyn_cast<ConstantFP>(constant)->getValueAPF().convertToFloat());
#else
        return c.fpa_val(
                dyn_cast<ConstantFP>(constant)->getValue().convertToFloat());
#endif
    } else if (constant->getType()->isDoubleTy()) {
#if LLVM_VERSION_MAJOR < 11
        return c.fpa_val(
                dyn_cast<ConstantFP>(constant)->getValueAPF().convertToFloat());
#else
        return c.fpa_val(
                dyn_cast<ConstantFP>(constant)->getValue().convertToDouble());
#endif
    } else {
        throw UnsupportedOperationException("Unsupported constant type");
    }
}

z3::expr SMTBlockComparator::createExprFromValue(z3::context &c,
                                                 const std::string &prefix,
                                                 const Value *val) {
    if (isa<Constant>(val)) {
        return createConstant(c, dyn_cast<Constant>(val));
    } else {
        std::stringstream stream;
        stream << val;
        auto nameStr = prefix + stream.str();
        return createVar(c, nameStr.c_str(), val->getType());
    }
}

z3::expr SMTBlockComparator::createResultVar(z3::context &c,
                                             const std::string &prefix,
                                             BasicBlock::const_iterator Inst) {
    std::stringstream stream;
    stream << &*Inst;
    auto nameStr = prefix + stream.str();
    return createVar(c, nameStr.c_str(), Inst->getType());
}

void SMTBlockComparator::mapOperands(z3::solver &s,
                                     z3::context &c,
                                     BasicBlock::const_iterator InstL) {
    for (const auto op : InstL->operand_values()) {
        if (fComp->sn_mapL.find(op) != fComp->sn_mapL.end()) {
            auto serialNumber = fComp->sn_mapL[op];
            if (fComp->mappedValuesBySn.find(serialNumber)
                != fComp->mappedValuesBySn.end()) {
                auto values = fComp->mappedValuesBySn[serialNumber];
                auto left = createExprFromValue(c, LPrefix, values.first);
                auto right = createExprFromValue(c, RPrefix, values.second);
                s.add(left == right);
            }
        }
    }
}

z3::expr SMTBlockComparator::encodeCmpInstruction(z3::context &c,
                                                  z3::expr &res,
                                                  const std::string &prefix,
                                                  const CmpInst *Inst) {
    auto op1 = createExprFromValue(c, prefix, Inst->getOperand(0));
    auto op2 = createExprFromValue(c, prefix, Inst->getOperand(1));

    // Z3 operator overloads default to signed comparison, encode unsigned
    // comparison explicitly.
    z3::expr e(c);
    switch (Inst->getPredicate()) {
    case CmpInst::ICMP_EQ:
    case CmpInst::FCMP_UEQ:
    case CmpInst::FCMP_OEQ:
        e = (op1 == op2);
        break;
    case CmpInst::ICMP_NE:
    case CmpInst::FCMP_UNE:
    case CmpInst::FCMP_ONE:
        e = (op1 != op2);
        break;
    case CmpInst::FCMP_TRUE:
        e = c.bool_val(true);
        break;
    case CmpInst::FCMP_FALSE:
        e = c.bool_val(false);
        break;
    case CmpInst::ICMP_UGE:
        e = z3::expr(c, Z3_mk_bvuge(c, op1, op2));
        break;
    case CmpInst::ICMP_SGE:
    case CmpInst::FCMP_UGE:
    case CmpInst::FCMP_OGE:
        e = (op1 >= op2);
        break;
    case CmpInst::ICMP_ULE:
        e = z3::expr(c, Z3_mk_bvule(c, op1, op2));
        break;
    case CmpInst::ICMP_SLE:
    case CmpInst::FCMP_ULE:
    case CmpInst::FCMP_OLE:
        e = (op1 <= op2);
        break;
    case CmpInst::ICMP_UGT:
        e = z3::expr(c, Z3_mk_bvugt(c, op1, op2));
        break;
    case CmpInst::ICMP_SGT:
    case CmpInst::FCMP_UGT:
    case CmpInst::FCMP_OGT:
        e = (op1 > op2);
        break;
    case CmpInst::ICMP_ULT:
        e = z3::expr(c, Z3_mk_bvult(c, op1, op2));
        break;
    case CmpInst::ICMP_SLT:
    case CmpInst::FCMP_ULT:
    case CmpInst::FCMP_OLT:
        e = (op1 < op2);
        break;
    default:
        break;
    }
    if (e) {
        return res == e;
    }
    return e;
}

z3::expr SMTBlockComparator::encodeOverflowingBinaryOperator(
        z3::context &c,
        z3::expr &res,
        const std::string &prefix,
        const OverflowingBinaryOperator *Inst) {
    auto op1 = createExprFromValue(c, prefix, Inst->getOperand(0));
    auto op2 = createExprFromValue(c, prefix, Inst->getOperand(1));

    // If an overflowing operation has the nsw/nuw keyword, if the instruction
    // overflows, it produces a poison (undefined) value. We want to encode
    // this behavior as: <no overflow> => (res == op1 <op> op2). This way,
    // if the operation can overflow, res remains a free variable, i.e.
    // undefined value.
    if (isa<AddOperator>(Inst)) {
        if (Inst->hasNoSignedWrap()) {
            auto precond = z3::bvadd_no_overflow(op1, op2, true)
                           && z3::bvadd_no_underflow(op1, op2);
            return z3::implies(precond, res == (op1 + op2));
        } else if (Inst->hasNoUnsignedWrap()) {
            auto precond = z3::bvadd_no_overflow(op1, op2, false)
                           && z3::bvadd_no_underflow(op1, op2);
            return z3::implies(precond, res == (op1 + op2));
        } else {
            return res == (op1 + op2);
        }
    } else if (isa<SubOperator>(Inst)) {
        if (Inst->hasNoSignedWrap()) {
            auto precond = z3::bvsub_no_overflow(op1, op2)
                           && z3::bvsub_no_underflow(op1, op2, true);
            return z3::implies(precond, res == (op1 - op2));
        } else if (Inst->hasNoUnsignedWrap()) {
            auto precond = z3::bvsub_no_overflow(op1, op2)
                           && z3::bvsub_no_underflow(op1, op2, false);
            return z3::implies(precond, res == (op1 - op2));
        } else {
            return res == (op1 - op2);
        }
    } else if (isa<MulOperator>(Inst)) {
        if (Inst->hasNoSignedWrap()) {
            auto precond = z3::bvmul_no_overflow(op1, op2, true)
                           && z3::bvmul_no_underflow(op1, op2);
            return z3::implies(precond, res == (op1 * op2));
        } else if (Inst->hasNoUnsignedWrap()) {
            auto precond = z3::bvmul_no_overflow(op1, op2, false)
                           && z3::bvmul_no_underflow(op1, op2);
            return z3::implies(precond, res == (op1 * op2));
        } else {
            return res == (op1 * op2);
        }
    } else if (isa<ShlOperator>(Inst)) {
        // FIXME: z3 has no check for shl overflow, encode it manually?
        //  According to the LLVM ref: If the nuw keyword is present, then the
        //  shift produces a poison value if it shifts out any non-zero bits.
        //  If the nsw keyword is present, then the shift produces a poison
        //  value if it shifts out any bits that disagree with the resultant
        //  sign bit.
        //  Encoding similar to z3::bvadd_no_overflow is not possible, Z3
        //  extract doesn't accept a variable number of bits.
        return res == z3::shl(op1, op2);
    }

    return {c};
}

z3::expr SMTBlockComparator::encodeBinaryOperator(
        z3::context &c,
        z3::expr &res,
        const std::string &prefix,
        const llvm::BinaryOperator *Inst) {
    z3::expr e(c);

    if (auto overflowing = dyn_cast<OverflowingBinaryOperator>(Inst)) {
        if ((e = encodeOverflowingBinaryOperator(
                     c, res, prefix, overflowing))) {
            return e;
        }
    }

    auto op1 = createExprFromValue(c, prefix, Inst->getOperand(0));
    auto op2 = createExprFromValue(c, prefix, Inst->getOperand(1));

    switch (Inst->getOpcode()) {
    case Instruction::Add:
    case Instruction::FAdd:
        e = (res == (op1 + op2));
        break;
    case Instruction::Sub:
    case Instruction::FSub:
        e = (res == (op1 - op2));
        break;
    case Instruction::Mul:
    case Instruction::FMul:
        e = (res == (op1 * op2));
        break;
    case Instruction::FDiv:
    case Instruction::SDiv:
    case Instruction::UDiv:
        e = (res == (op1 / op2));
        break;
    case Instruction::FRem:
    case Instruction::SRem:
    case Instruction::URem:
        e = (res == z3::mod(op1, op2));
        break;
    case Instruction::Shl:
        e = (res == z3::shl(op1, op2));
        break;
    case Instruction::AShr:
        e = (res == z3::ashr(op1, op2));
        break;
    case Instruction::LShr:
        e = (res == z3::lshr(op1, op2));
        break;
    case Instruction::And:
        if (op1.is_bool() && op2.is_bool()) {
            e = (res == (op1 && op2));
        } else if (op1.is_bv() && op2.is_bv()) {
            e = (res == (op1 & op2));
        }
        break;
    case Instruction::Or:
        if (op1.is_bool() && op2.is_bool()) {
            e = (res == (op1 && op2));
        } else if (op1.is_bv() && op2.is_bv()) {
            e = (res == (op1 | op2));
        }
        break;
    case Instruction::Xor:
        e = (res == (op1 ^ op2));
        break;
    default:
        break;
    }

    return e;
}

z3::expr SMTBlockComparator::encodeFunctionCall(z3::context &c,
                                                z3::expr &res,
                                                const std::string &prefix,
                                                const CallInst *Inst) {
    z3::expr e(c);
    auto name = (std::string)Inst->getCalledFunction()->getName();
    if (Inst->getIntrinsicID() == Intrinsic::fmuladd) {
        auto op1 = createExprFromValue(c, prefix, Inst->getArgOperand(0));
        auto op2 = createExprFromValue(c, prefix, Inst->getArgOperand(1));
        auto op3 = createExprFromValue(c, prefix, Inst->getArgOperand(2));
        e = (res == op1 * op2 + op3);
    }
    if (name == "acos" || name == "asin" || name == "atan" || name == "cos"
        || name == "cosh" || name == "sin" || name == "sinh" || name == "tanh"
        || name == "exp" || name == "log" || name == "log10"
        || name == "sqrt") {
        // Represent these floating point functions as uninterpreted. While
        // z3 offers some support for these, see:
        //  https://link.springer.com/chapter/10.1007%2F978-3-642-38574-2_12
        // The C++ API doesn't expose them. Furthermore, they are defined
        // only for reals, rather than floats/doubles.
        auto sort = c.fpa_sort(11, 53);
        auto func = z3::function(name.c_str(), sort, sort);
        auto op1 = createExprFromValue(c, prefix, Inst->getArgOperand(0));
        e = (res == func(op1));
    }
    return e;
}

void SMTBlockComparator::encodeInstruction(z3::solver &s,
                                           z3::context &c,
                                           const std::string &prefix,
                                           BasicBlock::const_iterator Inst) {
    if (isDebugInfo(*Inst)) {
        return;
    }
    z3::expr e(c);
    auto res = createResultVar(c, prefix, Inst);
    if (isa<UnaryOperator>(Inst)) {
        auto op = createExprFromValue(c, prefix, Inst->getOperand(0));
        if (Inst->getOpcode() == Instruction::FNeg) {
            e = (res == -op);
        }
    }
    if (auto binOp = dyn_cast<BinaryOperator>(Inst)) {
        e = encodeBinaryOperator(c, res, prefix, binOp);
    } else if (auto cmpInst = dyn_cast<CmpInst>(Inst)) {
        e = encodeCmpInstruction(c, res, prefix, cmpInst);
    } else if (auto call = dyn_cast<CallInst>(Inst)) {
        e = encodeFunctionCall(c, res, prefix, call);
    }

    if (e) {
        s.add(e);
    } else {
        std::string msg = "Unsupported instruction with opcode"
                          + std::to_string(Inst->getOpcode());
        throw UnsupportedOperationException(msg);
    }
}

int SMTBlockComparator::compareSnippets(BasicBlock::const_iterator &StartL,
                                        BasicBlock::const_iterator &EndL,
                                        BasicBlock::const_iterator &StartR,
                                        BasicBlock::const_iterator &EndR) {
    // There must be at least one instruction on each side, otherwise,
    // there would be no operands to map, as well as no output variables
    if (StartL == EndL || StartR == EndR) {
        return 1;
    }
    z3::context c;
    z3::solver s(c);
    if (config.SMTTimeout > 0) {
        // Convert seconds to milliseconds
        s.set("timeout", remainingTime * 1000);
    }

    // Construct a formula consisting of 3 parts connected by conjunction:
    //   1. equality of input variables of the snippet based on varmap
    //   2. encoding of the instructions
    //   3. postcondition defining equality of output variables
    // If such a formula is UNSAT, it means there are no inputs to the snippets
    // such that their outputs differ, i.e. the snippets are EQUAL.
    //
    // To encode the instructions, we make use of the SSA property of LLVM IR.
    // Thanks to this fact, we can conveniently name SMT variables using
    // addresses of pointers of instructions.

    auto InstL = StartL;
    auto InstR = StartR;
    while (InstL != EndL) {
        mapOperands(s, c, InstL);
        encodeInstruction(s, c, LPrefix, InstL);
        InstL++;
    }
    while (InstR != EndR) {
        encodeInstruction(s, c, RPrefix, InstR);
        InstR++;
    }

    std::time_t start = std::time(nullptr);
    switch (s.check()) {
    case z3::unsat:
        return 0;
    default:
        // If SAT (blocks not equal), SMT solving may be run once again.
        // Decrease the remaining time by the time taken.
        auto elapsed = std::time(nullptr) - start;
        if (config.SMTTimeout > 0) {
            if (elapsed >= remainingTime) {
                throw OutOfTimeException();
            } else {
                remainingTime -= elapsed;
            }
        }
        return 1;
    }
}

int SMTBlockComparator::doCompare(BasicBlock::const_iterator &InstL,
                                  BasicBlock::const_iterator &InstR) {
    // Back-up the start of the snippet
    auto StartL = InstL;
    auto StartR = InstR;
    auto BBL = const_cast<BasicBlock *>(InstL->getParent());
    auto BBR = const_cast<BasicBlock *>(InstR->getParent());

    // Instructions have been found to differ, undo the last comparison
    fComp->undoLastInstCompare(InstL, InstR);

    do {
        // Update InstL and InstR to point to the end of the snippet.
        // There may be multiple possible synchronization points in the
        // functions and the first one may be the incorrect one. We need to
        // check them all.
        findSnippetEnd(InstL, InstR);

        try {
            int res = compareSnippets(StartL, InstL, StartR, InstR);

            if (res == 0)
                return res;

            // Restore the original state of fComp so that we can look for
            // another synchronization point.
            fComp->sn_mapL = sn_mapL_backup;
            fComp->sn_mapR = sn_mapR_backup;
            fComp->mappedValuesBySn = mappedValuesBySnBackup;
            // Move iterators forward to avoid finding the same sync
            InstR++;
            if (InstR == BBR->end()) {
                InstR = StartR;
                InstL++;
                if (InstL == BBL->end())
                    return res;
            }
        } catch (z3::exception &err) {
            throw UnsupportedOperationException(err.what());
        }
    } while (InstL != BBL->end() || InstR != BBR->end());

    // No synchronization point resulted in an EQUAL result.
    return 1;
}

int SMTBlockComparator::compare(BasicBlock::const_iterator &InstL,
                                BasicBlock::const_iterator &InstR) {
    remainingTime = config.SMTTimeout;
    int res = doCompare(InstL, InstR);
    // Move the iterators back by one since internally, we work with the
    // first instructions that are synchronized after the snippets but
    // DifferentialFunctionComparator does Inst{L,R}++.
    InstL--;
    InstR--;
    // There may be some mess in the maps.
    // Reset the maps and let function comparator do a fresh mapping.
    // Ideally we would do this as RAII via a destructor, however the calling
    // function (cmpBasicBlocks) is declared as const in LLVM and we need to
    // modify the members in this class.
    fComp->sn_mapL = sn_mapL_backup;
    fComp->sn_mapR = sn_mapR_backup;
    fComp->mappedValuesBySn = mappedValuesBySnBackup;
    return res;
}
