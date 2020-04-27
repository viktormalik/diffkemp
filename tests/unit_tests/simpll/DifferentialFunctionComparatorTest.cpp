//===--------- DifferentialFunctionComparatorTest.cpp - Unit tests ---------==//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Tomas Glozar, tglozar@gmail.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains unit tests for the DifferentialFunctionComparator class,
/// along with necessary classes and fixtures used by them.
///
//===----------------------------------------------------------------------===//

#include <Config.h>
#include <DebugInfo.h>
#include <DifferentialFunctionComparator.h>
#include <ModuleComparator.h>
#include <ResultsCache.h>
#include <gtest/gtest.h>
#include <passes/FieldAccessFunctionGenerator.h>
#include <passes/StructureDebugInfoAnalysis.h>
#include <passes/StructureSizeAnalysis.h>

#if LLVM_VERSION_MAJOR > 7
/// This class is used to expose protected functions in
/// DifferentialFunctionComparator.
class TestComparator : public DifferentialFunctionComparator {
  public:
    TestComparator(const Function *F1,
                   const Function *F2,
                   const Config &config,
                   const DebugInfo *DI,
                   ModuleComparator *MC)
            : DifferentialFunctionComparator(F1, F2, config, DI, MC) {}
    int testCompareSignature(bool keepSN = false) {
        if (!keepSN)
            beginCompare();
        return compareSignature();
    }

    int testCmpAttrs(const AttributeList L,
                     const AttributeList R,
                     bool keepSN = false) {
        if (!keepSN)
            beginCompare();
        return cmpAttrs(L, R);
    }

    int testCmpAllocs(const CallInst *CL,
                      const CallInst *CR,
                      bool keepSN = false) {
        if (!keepSN)
            beginCompare();
        return cmpAllocs(CL, CR);
    }

    int testCmpConstants(const Constant *CL,
                         const Constant *CR,
                         bool keepSN = false) {
        if (!keepSN)
            beginCompare();
        return cmpConstants(CL, CR);
    }

    int testCmpMemset(const CallInst *CL,
                      const CallInst *CR,
                      bool keepSN = false) {
        if (!keepSN)
            beginCompare();
        return cmpMemset(CL, CR);
    }

    int testCmpCallsWithExtraArg(const CallInst *CL,
                                 const CallInst *CR,
                                 bool keepSN = false) {
        if (!keepSN)
            beginCompare();
        return cmpCallsWithExtraArg(CL, CR);
    }

    int testCmpBasicBlocks(BasicBlock *BBL,
                           BasicBlock *BBR,
                           bool keepSN = false) {
        if (!keepSN)
            beginCompare();
        return cmpBasicBlocks(BBL, BBR);
    }

    int testCmpGEPs(const GEPOperator *GEPL,
                    const GEPOperator *GEPR,
                    bool keepSN = false) {
        if (!keepSN)
            beginCompare();
        return cmpGEPs(GEPL, GEPR);
    }

    int testCmpGlobalValues(GlobalValue *L,
                            GlobalValue *R,
                            bool keepSN = false) {
        if (!keepSN)
            beginCompare();
        return cmpGlobalValues(L, R);
    }

    int testCmpValues(const Value *L, const Value *R, bool keepSN = false) {
        if (!keepSN)
            beginCompare();
        return cmpValues(L, R);
    }

    int testCmpOperations(const Instruction *L,
                          const Instruction *R,
                          bool &needToCmpOperands,
                          bool keepSN = false) {
        if (!keepSN)
            beginCompare();
        return cmpOperations(L, R, needToCmpOperands);
    }

    int testCmpTypes(Type *TyL, Type *TyR, bool keepSN = false) {
        if (!keepSN)
            beginCompare();
        return cmpTypes(TyL, TyR);
    }

    void setLeftSerialNumber(const Value *Val, int i) { sn_mapL[Val] = i; }

    void setRightSerialNumber(const Value *Val, int i) { sn_mapR[Val] = i; }
};

/// Test fixture providing contexts, modules, functions, a Config object,
/// a ModuleComparator, a TestComparator and debug metadata for the tests in
/// this file.
class DifferentialFunctionComparatorTest : public ::testing::Test {
  public:
    // Modules used for testing.
    LLVMContext CtxL, CtxR;
    Module ModL{"left", CtxL};
    Module ModR{"right", CtxR};

    // Functions to be tested.
    Function *FL, *FR;

    // Objects necessary to create a DifferentialFunctionComparator.
    Config Conf{"F", "F", ""};
    std::set<const Function *> CalledFirst;
    std::set<const Function *> CalledSecond;
    ResultsCache Cache{""};
    StructureSizeAnalysis::Result StructSizeMapL;
    StructureSizeAnalysis::Result StructSizeMapR;
    StructureDebugInfoAnalysis::Result StructDIMapL;
    StructureDebugInfoAnalysis::Result StructDIMapR;
    // Note: DbgInfo depends on FL and FR, ModComp depends on DbgInfo; therefore
    // both objects need to be created in the constructor;
    std::unique_ptr<DebugInfo> DbgInfo;
    std::unique_ptr<ModuleComparator> ModComp;

    // TestComparator is used to expose otherwise protected functions.
    std::unique_ptr<TestComparator> DiffComp;

    // Debug metadata is used mainly for checking the detection of macros
    // and types.
    DISubprogram *DSubL, *DSubR;

    DifferentialFunctionComparatorTest() : ::testing::Test() {
        // Create one function in each module for testing purposes.
        FL = Function::Create(
                FunctionType::get(Type::getVoidTy(CtxL), {}, false),
                GlobalValue::ExternalLinkage,
                "F",
                &ModL);
        FR = Function::Create(
                FunctionType::get(Type::getVoidTy(CtxR), {}, false),
                GlobalValue::ExternalLinkage,
                "F",
                &ModR);

        // Create the DebugInfo object and a ModuleComparator.
        // Note: DifferentialFunctionComparator cannot function without
        // ModuleComparator and DebugInfo.
        DbgInfo = std::make_unique<DebugInfo>(
                ModL, ModR, FL, FR, CalledFirst, CalledSecond);
        ModComp = std::make_unique<ModuleComparator>(ModL,
                                                     ModR,
                                                     Conf,
                                                     DbgInfo.get(),
                                                     StructSizeMapL,
                                                     StructSizeMapR,
                                                     StructDIMapL,
                                                     StructDIMapR);
        // Add function pair to ComparedFuns.
        // Note: even though ModuleComparator is not tested here,
        // DifferentialFunctionComparator expects the presence of the key in
        // the map, therefore it is necessary to do this here.
        ModComp->ComparedFuns.insert({{FL, FR}, Result{}});

        // Generate debug metadata.
        generateDebugMetadata();

        // Finally create the comparator.
        DiffComp = std::make_unique<TestComparator>(
                FL, FR, Conf, DbgInfo.get(), ModComp.get());
    }

    /// Generates a file, compile unit and subprogram for each module.
    void generateDebugMetadata(DICompositeTypeArray DTyArrL = {},
                               DICompositeTypeArray DTyArrR = {},
                               DIMacroNodeArray DMacArrL = {},
                               DIMacroNodeArray DMacArrR = {}) {
        DIFile *DScoL = DIFile::get(CtxL, "test", "test");
        DIFile *DScoR = DIFile::get(CtxR, "test", "test");
        DICompileUnit *DCUL = DICompileUnit::getDistinct(
                CtxL,
                0,
                DScoL,
                "test",
                false,
                "",
                0,
                "test",
                DICompileUnit::DebugEmissionKind::FullDebug,
                DTyArrL,
                DIScopeArray{},
                DIGlobalVariableExpressionArray{},
                DIImportedEntityArray{},
                DMacArrL,
                0,
                false,
                false,
                DICompileUnit::DebugNameTableKind::Default,
                0);
        DICompileUnit *DCUR = DICompileUnit::getDistinct(
                CtxR,
                0,
                DScoR,
                "test",
                false,
                "",
                0,
                "test",
                DICompileUnit::DebugEmissionKind::FullDebug,
                DTyArrR,
                DIScopeArray{},
                DIGlobalVariableExpressionArray{},
                DIImportedEntityArray{},
                DMacArrR,
                0,
                false,
                false,
                DICompileUnit::DebugNameTableKind::Default,
                0);
        DSubL = DISubprogram::get(CtxL,
                                  DScoL,
                                  "test",
                                  "test",
                                  DScoL,
                                  1,
                                  nullptr,
                                  1,
                                  nullptr,
                                  0,
                                  0,
                                  DINode::DIFlags{},
                                  DISubprogram::DISPFlags{},
                                  DCUL);
        DSubR = DISubprogram::get(CtxR,
                                  DScoR,
                                  "test",
                                  "test",
                                  DScoR,
                                  1,
                                  nullptr,
                                  1,
                                  nullptr,
                                  0,
                                  0,
                                  DINode::DIFlags{},
                                  DISubprogram::DISPFlags{},
                                  DCUR);
    }

    /// Compares two functions using cmpGlobalValues called through
    /// cmpBasicBlocks on a pair of auxilliary basic blocks containing calls
    /// to the functions.
    int testFunctionComparison(Function *FunL, Function *FunR) {
        const std::string auxFunName = "AuxFunComp";

        // Testing function comparison is a little bit tricky, because for the
        // callee generation the call location must be set at the time the
        // comparison is done.
        // To ensure this a pair of auxilliary functions containing a call to
        // the functions is added, along with their locations.
        if (auto OldFun = ModL.getFunction(auxFunName)) {
            OldFun->eraseFromParent();
        }
        if (auto OldFun = ModR.getFunction(auxFunName)) {
            OldFun->eraseFromParent();
        }

        Function *AuxFL = Function::Create(
                FunctionType::get(Type::getVoidTy(CtxL), {}, false),
                GlobalValue::ExternalLinkage,
                auxFunName,
                &ModL);
        Function *AuxFR = Function::Create(
                FunctionType::get(Type::getVoidTy(CtxR), {}, false),
                GlobalValue::ExternalLinkage,
                auxFunName,
                &ModR);
        BasicBlock *BBL = BasicBlock::Create(CtxL, "", AuxFL);
        BasicBlock *BBR = BasicBlock::Create(CtxR, "", AuxFR);

        CallInst *CL = CallInst::Create(FunL->getFunctionType(), FunL, "", BBL);
        CallInst *CR = CallInst::Create(FunR->getFunctionType(), FunR, "", BBR);

        // Add debug info.
        DILocation *DLocL = DILocation::get(CtxL, 1, 1, DSubL);
        DILocation *DLocR = DILocation::get(CtxR, 1, 1, DSubR);
        CL->setDebugLoc(DebugLoc{DLocL});
        CR->setDebugLoc(DebugLoc{DLocR});

        // Finish the basic blocks with return instructions and return the
        // result of cmpBasicBlocks.
        ReturnInst::Create(CtxL, BBL);
        ReturnInst::Create(CtxR, BBR);

        return DiffComp->testCmpBasicBlocks(BBL, BBR);
    }
};

/// Tests a comparison of two GEPs of a structure type with indices compared by
/// value.
TEST_F(DifferentialFunctionComparatorTest, CmpGepsSimple) {
    // Create structure types to test the GEPs.
    StructType *STyL =
            StructType::create({Type::getInt8Ty(CtxL), Type::getInt16Ty(CtxL)});
    STyL->setName("struct");
    StructType *STyR =
            StructType::create({Type::getInt8Ty(CtxR), Type::getInt16Ty(CtxR)});
    STyR->setName("struct");

    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    AllocaInst *VarL = new AllocaInst(STyL, 0, "var", BBL);
    AllocaInst *VarR = new AllocaInst(STyR, 0, "var", BBR);
    GetElementPtrInst *GEP1L = GetElementPtrInst::Create(
            STyL,
            VarL,
            {ConstantInt::get(Type::getInt32Ty(CtxL), 0),
             ConstantInt::get(Type::getInt32Ty(CtxL), 0)},
            "",
            BBL);
    GetElementPtrInst *GEP1R = GetElementPtrInst::Create(
            STyR,
            VarR,
            {ConstantInt::get(Type::getInt32Ty(CtxR), 0),
             ConstantInt::get(Type::getInt32Ty(CtxR), 0)},
            "",
            BBR);
    GetElementPtrInst *GEP2L = GetElementPtrInst::Create(
            STyL,
            VarL,
            {ConstantInt::get(Type::getInt32Ty(CtxL), 0),
             ConstantInt::get(Type::getInt32Ty(CtxL), 0)},
            "",
            BBL);
    GetElementPtrInst *GEP2R = GetElementPtrInst::Create(
            STyR,
            VarR,
            {ConstantInt::get(Type::getInt32Ty(CtxR), 0),
             ConstantInt::get(Type::getInt32Ty(CtxR), 1)},
            "",
            BBR);

    ASSERT_EQ(DiffComp->testCmpGEPs(dyn_cast<GEPOperator>(GEP1L),
                                    dyn_cast<GEPOperator>(GEP1R)),
              0);
    ASSERT_EQ(DiffComp->testCmpGEPs(dyn_cast<GEPOperator>(GEP2L),
                                    dyn_cast<GEPOperator>(GEP2R)),
              1);
}

/// Tests a comparison of two GEPs of a structure type with a constant index
/// that has to compared using debug info.
TEST_F(DifferentialFunctionComparatorTest, CmpGepsRenamed) {
    // Create structure types to test the GEPs.
    StructType *STyL =
            StructType::create({Type::getInt8Ty(CtxL), Type::getInt8Ty(CtxL)});
    STyL->setName("struct.test");
    StructType *STyR = StructType::create({Type::getInt8Ty(CtxR),
                                           Type::getInt8Ty(CtxR),
                                           Type::getInt8Ty(CtxR)});
    STyR->setName("struct.test");

    // Add entries to DebugInfo.
    // Note: attr3 is added between attr1 and attr2, causing the index shifting
    // tested here.
    std::string attr1("attr1"), attr2("attr2"), attr3("attr3");
    DbgInfo->StructFieldNames[{STyL, 0}] = attr1;
    DbgInfo->StructFieldNames[{STyL, 1}] = attr2;
    DbgInfo->StructFieldNames[{STyR, 0}] = attr1;
    DbgInfo->StructFieldNames[{STyR, 1}] = attr3;
    DbgInfo->StructFieldNames[{STyR, 2}] = attr2;

    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    AllocaInst *VarL = new AllocaInst(STyL, 0, "var", BBL);
    AllocaInst *VarR = new AllocaInst(STyR, 0, "var", BBR);
    GetElementPtrInst *GEP1L = GetElementPtrInst::Create(
            STyL,
            VarL,
            {ConstantInt::get(Type::getInt32Ty(CtxL), 0),
             ConstantInt::get(Type::getInt32Ty(CtxL), 1)},
            "",
            BBL);
    GetElementPtrInst *GEP1R = GetElementPtrInst::Create(
            STyR,
            VarR,
            {ConstantInt::get(Type::getInt32Ty(CtxR), 0),
             ConstantInt::get(Type::getInt32Ty(CtxR), 2)},
            "",
            BBR);
    GetElementPtrInst *GEP2L = GetElementPtrInst::Create(
            STyL,
            VarL,
            {ConstantInt::get(Type::getInt32Ty(CtxL), 0),
             ConstantInt::get(Type::getInt32Ty(CtxL), 0)},
            "",
            BBL);
    GetElementPtrInst *GEP2R = GetElementPtrInst::Create(
            STyR,
            VarR,
            {ConstantInt::get(Type::getInt32Ty(CtxR), 0),
             ConstantInt::get(Type::getInt32Ty(CtxR), 2)},
            "",
            BBR);

    // The structures have the same name, therefore the corresponding indices
    // should be compared as equal (while non-corresponding ones stay not
    // equal).
    ASSERT_EQ(DiffComp->testCmpGEPs(dyn_cast<GEPOperator>(GEP1L),
                                    dyn_cast<GEPOperator>(GEP1R)),
              0);
    ASSERT_EQ(DiffComp->testCmpGEPs(dyn_cast<GEPOperator>(GEP2L),
                                    dyn_cast<GEPOperator>(GEP2R)),
              1);

    // Now rename one of the structures and check whether the comparison result
    // changed.
    STyL->setName("struct.1");
    STyR->setName("struct.2");
    ASSERT_EQ(DiffComp->testCmpGEPs(dyn_cast<GEPOperator>(GEP1L),
                                    dyn_cast<GEPOperator>(GEP1R)),
              -1);
}

/// Tests a comparison of two GEPs of different array types that don't go into
/// its elements (therefore the type difference should be ignored).
TEST_F(DifferentialFunctionComparatorTest, CmpGepsArray) {
    // Create structure types to test the GEPs.
    ArrayType *ATyL = ArrayType::get(Type::getInt8Ty(CtxL), 2);
    ArrayType *ATyR = ArrayType::get(Type::getInt16Ty(CtxR), 3);

    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    AllocaInst *VarL = new AllocaInst(ATyL, 0, "var", BBL);
    AllocaInst *VarR = new AllocaInst(ATyR, 0, "var", BBR);
    GetElementPtrInst *GEP1L = GetElementPtrInst::Create(
            ATyL, VarL, {ConstantInt::get(Type::getInt32Ty(CtxL), 0)}, "", BBL);
    GetElementPtrInst *GEP1R = GetElementPtrInst::Create(
            ATyR, VarR, {ConstantInt::get(Type::getInt32Ty(CtxR), 0)}, "", BBR);
    GetElementPtrInst *GEP2L = GetElementPtrInst::Create(
            ATyL, VarL, {ConstantInt::get(Type::getInt32Ty(CtxL), 0)}, "", BBL);
    GetElementPtrInst *GEP2R = GetElementPtrInst::Create(
            ATyR, VarR, {ConstantInt::get(Type::getInt32Ty(CtxR), 1)}, "", BBR);

    ASSERT_EQ(DiffComp->testCmpGEPs(dyn_cast<GEPOperator>(GEP1L),
                                    dyn_cast<GEPOperator>(GEP1R)),
              0);
    ASSERT_EQ(DiffComp->testCmpGEPs(dyn_cast<GEPOperator>(GEP2L),
                                    dyn_cast<GEPOperator>(GEP2R)),
              -1);
}

/// Tests attribute comparison (currently attributes are always ignored).
TEST_F(DifferentialFunctionComparatorTest, CmpAttrs) {
    AttributeList L, R;
    ASSERT_EQ(DiffComp->testCmpAttrs(L, R), 0);
}

/// Tests specific comparison of intermediate comparison operations in cases
/// when the signedness differs while comparing with control flow only.
TEST_F(DifferentialFunctionComparatorTest, CmpOperationsICmp) {
    bool needToCmpOperands;

    // Create two global variables and comparison instructions using them.
    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    GlobalVariable *GVL =
            new GlobalVariable(Type::getInt8Ty(CtxL),
                               true,
                               GlobalValue::ExternalLinkage,
                               ConstantInt::get(Type::getInt32Ty(CtxL), 6));
    GlobalVariable *GVR =
            new GlobalVariable(Type::getInt8Ty(CtxR),
                               true,
                               GlobalValue::ExternalLinkage,
                               ConstantInt::get(Type::getInt32Ty(CtxR), 6));

    ICmpInst *ICmpL =
            new ICmpInst(*BBL, CmpInst::Predicate::ICMP_UGT, GVL, GVL);
    ICmpInst *ICmpR =
            new ICmpInst(*BBR, CmpInst::Predicate::ICMP_SGT, GVR, GVR);

    ASSERT_EQ(DiffComp->testCmpOperations(ICmpL, ICmpR, needToCmpOperands), -1);
    Conf.ControlFlowOnly = true;
    ASSERT_EQ(DiffComp->testCmpOperations(ICmpL, ICmpR, needToCmpOperands), 0);
}

/// Tests specific comparison of allocas of a structure type whose layout
/// changed.
TEST_F(DifferentialFunctionComparatorTest, CmpOperationsAllocas) {
    bool needToCmpOperands;

    // Create two structure types and allocas using them.
    StructType *STyL =
            StructType::create({Type::getInt8Ty(CtxL), Type::getInt8Ty(CtxL)});
    STyL->setName("struct.test");
    StructType *STyR = StructType::create({Type::getInt8Ty(CtxR),
                                           Type::getInt8Ty(CtxR),
                                           Type::getInt8Ty(CtxR)});
    STyR->setName("struct.test");

    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    AllocaInst *AllL = new AllocaInst(STyL, 0, "var", BBL);
    AllocaInst *AllR = new AllocaInst(STyR, 0, "var", BBR);

    ASSERT_EQ(DiffComp->testCmpOperations(AllL, AllR, needToCmpOperands), 0);
}

/// Tests the comparison of calls to allocation functions.
TEST_F(DifferentialFunctionComparatorTest, CmpAllocs) {
    // Create auxilliary functions to serve as the allocation functions.
    Function *AuxFL = Function::Create(
            FunctionType::get(PointerType::get(Type::getVoidTy(CtxL), 0),
                              {Type::getInt32Ty(CtxL)},
                              false),
            GlobalValue::ExternalLinkage,
            "AuxFL",
            &ModL);
    Function *AuxFR = Function::Create(
            FunctionType::get(PointerType::get(Type::getVoidTy(CtxR), 0),
                              {Type::getInt32Ty(CtxR)},
                              false),
            GlobalValue::ExternalLinkage,
            "AuxFR",
            &ModR);

    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    // Test call instructions with the same value.
    CallInst *CL =
            CallInst::Create(AuxFL->getFunctionType(),
                             AuxFL,
                             {ConstantInt::get(Type::getInt32Ty(CtxL), 42)},
                             "",
                             BBL);
    CallInst *CR =
            CallInst::Create(AuxFR->getFunctionType(),
                             AuxFR,
                             {ConstantInt::get(Type::getInt32Ty(CtxR), 42)},
                             "",
                             BBR);
    ASSERT_EQ(DiffComp->testCmpAllocs(CL, CR), 0);

    // Create structure types and calls for testing of allocation comparison
    // in cases where the structure size changed.
    StructType *STyL =
            StructType::create({Type::getInt8Ty(CtxL), Type::getInt8Ty(CtxL)});
    STyL->setName("struct.test");
    StructType *STyR = StructType::create({Type::getInt8Ty(CtxR),
                                           Type::getInt8Ty(CtxR),
                                           Type::getInt8Ty(CtxR)});
    STyR->setName("struct.test");
    uint64_t STyLSize = ModL.getDataLayout().getTypeStoreSize(STyL);
    uint64_t STyRSize = ModR.getDataLayout().getTypeStoreSize(STyR);
    CL = CallInst::Create(AuxFL->getFunctionType(),
                          AuxFL,
                          {ConstantInt::get(Type::getInt32Ty(CtxL), STyLSize)},
                          "",
                          BBL);
    CR = CallInst::Create(AuxFR->getFunctionType(),
                          AuxFR,
                          {ConstantInt::get(Type::getInt32Ty(CtxR), STyRSize)},
                          "",
                          BBR);

    // Add casts to allow cmpAllocs to check whether the structure types match.
    CastInst *CastL = CastInst::CreateTruncOrBitCast(CL, STyL, "", BBL);
    CastInst *CastR = CastInst::CreateTruncOrBitCast(CR, STyR, "", BBR);
    ASSERT_EQ(DiffComp->testCmpAllocs(CL, CR), 0);

    // Repeat the test again, but now with different structure types.
    StructType *STyR2 = StructType::create({Type::getInt8Ty(CtxR),
                                            Type::getInt8Ty(CtxR),
                                            Type::getInt8Ty(CtxR)});
    STyR2->setName("struct.test2");
    uint64_t STyR2Size = ModR.getDataLayout().getTypeStoreSize(STyR2);
    CL = CallInst::Create(AuxFL->getFunctionType(),
                          AuxFL,
                          {ConstantInt::get(Type::getInt32Ty(CtxL), STyLSize)},
                          "",
                          BBL);
    CR = CallInst::Create(AuxFR->getFunctionType(),
                          AuxFR,
                          {ConstantInt::get(Type::getInt32Ty(CtxR), STyRSize)},
                          "",
                          BBR);
    CastL = CastInst::CreateTruncOrBitCast(CL, STyL, "", BBL);
    CastR = CastInst::CreateTruncOrBitCast(CR, STyR2, "", BBR);
    ASSERT_EQ(DiffComp->testCmpAllocs(CL, CR), 1);
}

/// Tests the comparison of calls to memset functions.
TEST_F(DifferentialFunctionComparatorTest, CmpMemsets) {
    // Create auxilliary functions to serve as the memset functions.
    Function *AuxFL = Function::Create(
            FunctionType::get(PointerType::get(Type::getVoidTy(CtxL), 0),
                              {PointerType::get(Type::getVoidTy(CtxL), 0),
                               Type::getInt32Ty(CtxL),
                               Type::getInt32Ty(CtxL)},
                              false),
            GlobalValue::ExternalLinkage,
            "AuxFL",
            &ModL);
    Function *AuxFR = Function::Create(
            FunctionType::get(PointerType::get(Type::getVoidTy(CtxR), 0),
                              {PointerType::get(Type::getVoidTy(CtxR), 0),
                               Type::getInt32Ty(CtxR),
                               Type::getInt32Ty(CtxR)},
                              false),
            GlobalValue::ExternalLinkage,
            "AuxFR",
            &ModR);

    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    // Create structure types and allocas that will be used by the memset calls.
    StructType *STyL =
            StructType::create({Type::getInt8Ty(CtxL), Type::getInt8Ty(CtxL)});
    STyL->setName("struct.test");
    StructType *STyR = StructType::create({Type::getInt8Ty(CtxR),
                                           Type::getInt8Ty(CtxR),
                                           Type::getInt8Ty(CtxR)});
    STyR->setName("struct.test");
    uint64_t STyLSize = ModL.getDataLayout().getTypeStoreSize(STyL);
    uint64_t STyRSize = ModR.getDataLayout().getTypeStoreSize(STyR);
    AllocaInst *AllL = new AllocaInst(STyL, 0, "var", BBL);
    AllocaInst *AllR = new AllocaInst(STyR, 0, "var", BBR);

    // First test two memsets that differ in value that is set.
    CallInst *CL = CallInst::Create(
            AuxFL->getFunctionType(),
            AuxFL,
            {AllL,
             ConstantInt::get(Type::getInt32Ty(CtxL), 5),
             ConstantInt::get(Type::getInt32Ty(CtxL), STyLSize)},
            "",
            BBL);
    CallInst *CR = CallInst::Create(
            AuxFR->getFunctionType(),
            AuxFR,
            {AllR,
             ConstantInt::get(Type::getInt32Ty(CtxR), 6),
             ConstantInt::get(Type::getInt32Ty(CtxR), STyRSize)},
            "",
            BBR);
    ASSERT_EQ(DiffComp->testCmpMemset(CL, CR), -1);

    // Then test a case when the set value is the same and the arguments differ
    // only in the structure size.
    CL = CallInst::Create(AuxFL->getFunctionType(),
                          AuxFL,
                          {AllL,
                           ConstantInt::get(Type::getInt32Ty(CtxL), 5),
                           ConstantInt::get(Type::getInt32Ty(CtxL), STyLSize)},
                          "",
                          BBL);
    CR = CallInst::Create(AuxFR->getFunctionType(),
                          AuxFR,
                          {AllR,
                           ConstantInt::get(Type::getInt32Ty(CtxR), 5),
                           ConstantInt::get(Type::getInt32Ty(CtxR), STyRSize)},
                          "",
                          BBR);
    ASSERT_EQ(DiffComp->testCmpMemset(CL, CR), 0);
}

/// Tests comparing calls with an extra argument.
TEST_F(DifferentialFunctionComparatorTest, CmpCallsWithExtraArg) {
    // Create auxilliary functions to serve as the called functions.
    Function *AuxFL = Function::Create(
            FunctionType::get(Type::getVoidTy(CtxL),
                              {Type::getInt32Ty(CtxL), Type::getInt32Ty(CtxL)},
                              false),
            GlobalValue::ExternalLinkage,
            "AuxFL",
            &ModL);
    Function *AuxFR = Function::Create(
            FunctionType::get(
                    Type::getVoidTy(CtxR), {Type::getInt32Ty(CtxR)}, false),
            GlobalValue::ExternalLinkage,
            "AuxFR",
            &ModR);

    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    // First compare calls where the additional parameter is not zero.
    CallInst *CL =
            CallInst::Create(AuxFL->getFunctionType(),
                             AuxFL,
                             {ConstantInt::get(Type::getInt32Ty(CtxL), 5),
                              ConstantInt::get(Type::getInt32Ty(CtxL), 6)},
                             "",
                             BBL);
    CallInst *CR =
            CallInst::Create(AuxFR->getFunctionType(),
                             AuxFR,
                             {ConstantInt::get(Type::getInt32Ty(CtxR), 5)},
                             "",
                             BBR);
    ASSERT_EQ(DiffComp->testCmpCallsWithExtraArg(CL, CR), 1);
    ASSERT_EQ(DiffComp->testCmpCallsWithExtraArg(CR, CL), 1);

    // Then compare calls when the additional parameter is zero.
    CL = CallInst::Create(AuxFL->getFunctionType(),
                          AuxFL,
                          {ConstantInt::get(Type::getInt32Ty(CtxL), 5),
                           ConstantInt::get(Type::getInt32Ty(CtxL), 0)},
                          "",
                          BBL);
    CR = CallInst::Create(AuxFR->getFunctionType(),
                          AuxFR,
                          {ConstantInt::get(Type::getInt32Ty(CtxR), 5)},
                          "",
                          BBR);
    ASSERT_EQ(DiffComp->testCmpCallsWithExtraArg(CL, CR), 0);
    ASSERT_EQ(DiffComp->testCmpCallsWithExtraArg(CR, CL), 0);
}

/// Tests several cases where cmpTypes should detect a semantic equivalence.
TEST_F(DifferentialFunctionComparatorTest, CmpTypes) {
    // Try to compare a union type of a greater size than the other type.
    StructType *STyL = StructType::create({Type::getInt32Ty(CtxL)});
    Type *IntTyR = Type::getInt16Ty(CtxL);
    STyL->setName("union.test");
    ASSERT_EQ(DiffComp->testCmpTypes(STyL, IntTyR), 0);
    ASSERT_EQ(DiffComp->testCmpTypes(IntTyR, STyL), 0);
    // Rename the type to remove "union" from the name and check the result
    // again.
    STyL->setName("struct.test");
    ASSERT_EQ(DiffComp->testCmpTypes(STyL, IntTyR), 1);
    ASSERT_EQ(DiffComp->testCmpTypes(IntTyR, STyL), -1);

    // Then try to compare a union type of smaller size that the other type.
    STyL = StructType::create({Type::getInt16Ty(CtxL)});
    IntTyR = Type::getInt32Ty(CtxL);
    STyL->setName("union.test");
    ASSERT_EQ(DiffComp->testCmpTypes(STyL, IntTyR), 1);
    ASSERT_EQ(DiffComp->testCmpTypes(IntTyR, STyL), -1);

    // Integer types and array times with the same element type should compare
    // as equivalent when comparing with control flow only.
    ASSERT_EQ(DiffComp->testCmpTypes(Type::getInt16Ty(CtxL),
                                     Type::getInt8Ty(CtxR)),
              1);
    ASSERT_EQ(DiffComp->testCmpTypes(ArrayType::get(Type::getInt8Ty(CtxL), 10),
                                     ArrayType::get(Type::getInt8Ty(CtxR), 11)),
              -1);
    Conf.ControlFlowOnly = true;
    ASSERT_EQ(DiffComp->testCmpTypes(Type::getInt16Ty(CtxL),
                                     Type::getInt8Ty(CtxR)),
              0);
    ASSERT_EQ(DiffComp->testCmpTypes(ArrayType::get(Type::getInt8Ty(CtxL), 10),
                                     ArrayType::get(Type::getInt8Ty(CtxR), 11)),
              0);
    // Boolean type should stay unequal.
    ASSERT_EQ(DiffComp->testCmpTypes(ArrayType::get(Type::getInt1Ty(CtxL), 10),
                                     ArrayType::get(Type::getInt8Ty(CtxR), 11)),
              1);
}

/// Tests whether calls are properly marked for inlining while comparing
/// basic blocks.
TEST_F(DifferentialFunctionComparatorTest, CmpBasicBlocksInlining) {
    // Create the basic blocks with terminator instructions (to make sure that
    // after skipping the alloca created below, the end of the block is not
    // encountered).
    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    auto *RetL = ReturnInst::Create(CtxL, BBL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);
    auto *RetR = ReturnInst::Create(CtxR, BBR);

    // Create auxilliary functions to inline.
    Function *AuxFL = Function::Create(
            FunctionType::get(
                    Type::getVoidTy(CtxL), {Type::getInt32Ty(CtxR)}, false),
            GlobalValue::ExternalLinkage,
            "AuxFL",
            &ModL);
    Function *AuxFR = Function::Create(
            FunctionType::get(
                    Type::getVoidTy(CtxR), {Type::getInt32Ty(CtxR)}, false),
            GlobalValue::ExternalLinkage,
            "AuxFR",
            &ModR);

    // Test inlining on the left.
    CallInst *CL = CallInst::Create(AuxFL->getFunctionType(), AuxFL, "", RetL);
    AllocaInst *AllR = new AllocaInst(Type::getInt8Ty(CtxR), 0, "var", RetR);

    ASSERT_EQ(DiffComp->testCmpBasicBlocks(BBL, BBR), 1);
    std::pair<const CallInst *, const CallInst *> expectedPair{CL, nullptr};
    ASSERT_EQ(ModComp->tryInline, expectedPair);

    CL->eraseFromParent();
    AllR->eraseFromParent();

    // Test inlining on the right.
    ModComp->tryInline = {nullptr, nullptr};
    AllocaInst *AllL = new AllocaInst(Type::getInt8Ty(CtxL), 0, "var", RetL);
    CallInst *CR = CallInst::Create(AuxFR->getFunctionType(), AuxFR, "", RetR);

    ASSERT_EQ(DiffComp->testCmpBasicBlocks(BBL, BBR), -1);
    expectedPair = {nullptr, CR};
    ASSERT_EQ(ModComp->tryInline, expectedPair);

    AllL->eraseFromParent();
    CR->eraseFromParent();

    // Test inlining on both sides.
    CL = CallInst::Create(AuxFL->getFunctionType(),
                          AuxFL,
                          {ConstantInt::get(Type::getInt32Ty(CtxL), 5)},
                          "",
                          RetL);
    CR = CallInst::Create(AuxFR->getFunctionType(),
                          AuxFR,
                          {ConstantInt::get(Type::getInt32Ty(CtxR), 6)},
                          "",
                          RetR);
    ReturnInst::Create(CtxL, BBL);
    ReturnInst::Create(CtxR, BBR);

    ASSERT_EQ(DiffComp->testCmpBasicBlocks(BBL, BBR), 1);
    expectedPair = {CL, CR};
    ASSERT_EQ(ModComp->tryInline, expectedPair);
}

/// Tests ignoring of instructions that don't cause a semantic difference in
/// cmpBasicBlocks.
/// Note: the functioning of mayIgnore is tested in the test for cmpValues.
TEST_F(DifferentialFunctionComparatorTest, CmpBasicBlocksIgnore) {
    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    new AllocaInst(Type::getInt8Ty(CtxL), 0, "var", BBL);
    new AllocaInst(Type::getInt8Ty(CtxR), 0, "var1", BBR);
    new AllocaInst(Type::getInt8Ty(CtxR), 0, "var2", BBR);
    ReturnInst::Create(CtxL, BBL);
    ReturnInst::Create(CtxR, BBR);

    ASSERT_EQ(DiffComp->testCmpBasicBlocks(BBL, BBR), 0);
    ASSERT_EQ(DiffComp->testCmpBasicBlocks(BBR, BBL), 0);
}

/// Tests the comparison of constant global variables using cmpGlobalValues.
TEST_F(DifferentialFunctionComparatorTest, CmpGlobalValuesConstGlobalVars) {
    GlobalVariable *GVL1 =
            new GlobalVariable(Type::getInt8Ty(CtxL),
                               true,
                               GlobalValue::ExternalLinkage,
                               ConstantInt::get(Type::getInt32Ty(CtxL), 6));
    GlobalVariable *GVR1 =
            new GlobalVariable(Type::getInt8Ty(CtxR),
                               true,
                               GlobalValue::ExternalLinkage,
                               ConstantInt::get(Type::getInt32Ty(CtxR), 6));
    GlobalVariable *GVR2 =
            new GlobalVariable(Type::getInt8Ty(CtxR),
                               true,
                               GlobalValue::ExternalLinkage,
                               ConstantInt::get(Type::getInt32Ty(CtxR), 5));

    ASSERT_EQ(DiffComp->testCmpGlobalValues(GVL1, GVR1), 0);
    ASSERT_EQ(DiffComp->testCmpGlobalValues(GVL1, GVR2), 1);
}

/// Tests the comparison of non-constant global variables using cmpGlobalValues.
TEST_F(DifferentialFunctionComparatorTest, CmpGlobalValuesNonConstGlobalVars) {
    GlobalVariable *GVL1 =
            new GlobalVariable(Type::getInt8Ty(CtxL),
                               false,
                               GlobalValue::ExternalLinkage,
                               ConstantInt::get(Type::getInt32Ty(CtxL), 6),
                               "test.0");
    GlobalVariable *GVR1 =
            new GlobalVariable(Type::getInt8Ty(CtxR),
                               false,
                               GlobalValue::ExternalLinkage,
                               ConstantInt::get(Type::getInt32Ty(CtxR), 6),
                               "test.1");
    GlobalVariable *GVR2 =
            new GlobalVariable(Type::getInt8Ty(CtxR),
                               false,
                               GlobalValue::ExternalLinkage,
                               ConstantInt::get(Type::getInt32Ty(CtxR), 6),
                               "test2.1");

    ASSERT_EQ(DiffComp->testCmpGlobalValues(GVL1, GVR1), 0);
    ASSERT_EQ(DiffComp->testCmpGlobalValues(GVL1, GVR2), 1);
}

/// Tests the comparison of functions using cmpGlobalValues.
TEST_F(DifferentialFunctionComparatorTest, CmpGlobalValuesFunctions) {
    // Create auxilliary functions for the purpose of inlining tests.
    Function *AuxFL = Function::Create(
            FunctionType::get(Type::getVoidTy(CtxL), {}, false),
            GlobalValue::ExternalLinkage,
            "Aux",
            &ModL);
    Function *AuxFR = Function::Create(
            FunctionType::get(Type::getVoidTy(CtxR), {}, false),
            GlobalValue::ExternalLinkage,
            "Aux",
            &ModR);
    ASSERT_EQ(testFunctionComparison(AuxFL, AuxFR), 0);
    ASSERT_NE(ModComp->ComparedFuns.find({AuxFL, AuxFR}),
              ModComp->ComparedFuns.end());

    // Test comparison of print functions (they should be always compared as
    // equal).
    AuxFL = Function::Create(
            FunctionType::get(Type::getVoidTy(CtxL), {}, false),
            GlobalValue::ExternalLinkage,
            "printk",
            &ModL);
    AuxFR = Function::Create(
            FunctionType::get(Type::getVoidTy(CtxR), {}, false),
            GlobalValue::ExternalLinkage,
            "printk",
            &ModR);
    ASSERT_EQ(testFunctionComparison(AuxFL, AuxFR), 0);
    ASSERT_EQ(ModComp->ComparedFuns.find({AuxFL, AuxFR}),
              ModComp->ComparedFuns.end());
}

/// Tests the comparison of field access abstractions using cmpGlobalValues.
TEST_F(DifferentialFunctionComparatorTest, CmpGlobalValuesFieldAccesses) {
    // Create the structure types for the test case.
    StructType *UnionL = StructType::create({Type::getInt8Ty(CtxL)});
    UnionL->setName("union.test");
    StructType *STyL = StructType::create(UnionL);
    STyL->setName("struct.test");
    StructType *STyR = StructType::create({Type::getInt8Ty(CtxR)});
    STyR->setName("struct.test");

    // Create the abstractions and create GEPs inside them.
    Function *AuxFL = Function::Create(
            FunctionType::get(PointerType::get(Type::getInt8Ty(CtxL), 0),
                              {PointerType::get(STyL, 0)},
                              false),
            GlobalValue::InternalLinkage,
            SimpllFieldAccessFunName + ".0",
            &ModL);
    Function *AuxFR = Function::Create(
            FunctionType::get(PointerType::get(Type::getInt8Ty(CtxL), 0),
                              {PointerType::get(STyR, 0)},
                              false),
            GlobalValue::InternalLinkage,
            SimpllFieldAccessFunName + ".0",
            &ModR);

    BasicBlock *BBL = BasicBlock::Create(CtxL, "", AuxFL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", AuxFR);

    GetElementPtrInst *GEPL1 = GetElementPtrInst::Create(
            STyL,
            AuxFL->arg_begin(),
            {ConstantInt::get(Type::getInt32Ty(CtxL), 0),
             ConstantInt::get(Type::getInt32Ty(CtxL), 0)},
            "",
            BBL);
    GetElementPtrInst *GEPL2 = GetElementPtrInst::Create(
            UnionL,
            GEPL1,
            {ConstantInt::get(Type::getInt32Ty(CtxL), 0),
             ConstantInt::get(Type::getInt32Ty(CtxL), 0)},
            "",
            BBL);
    GetElementPtrInst *GEPR = GetElementPtrInst::Create(
            STyR,
            AuxFR->arg_begin(),
            {ConstantInt::get(Type::getInt32Ty(CtxL), 0),
             ConstantInt::get(Type::getInt32Ty(CtxL), 0)},
            "",
            BBR);
    ReturnInst::Create(CtxL, GEPL1, BBL);
    ReturnInst::Create(CtxR, GEPR, BBR);

    // Compare the field accesses.
    ASSERT_EQ(testFunctionComparison(AuxFL, AuxFR), 0);
    ASSERT_EQ(ModComp->ComparedFuns.find({AuxFL, AuxFR}),
              ModComp->ComparedFuns.end());

    // Compare the field access again with a different name.
    AuxFL->setName("not-a-field-access");
    AuxFR->setName("not-a-field-access");
    ASSERT_EQ(testFunctionComparison(AuxFL, AuxFR), 0);
    ASSERT_NE(ModComp->ComparedFuns.find({AuxFL, AuxFR}),
              ModComp->ComparedFuns.end());
}

/// Test the comparison of constant global variables with missing initializers
/// using cmpGlobalValues (they should be added to the list of missing
/// definitions).
TEST_F(DifferentialFunctionComparatorTest, CmpGlobalValuesMissingDefs) {
    GlobalVariable *GVL1 = new GlobalVariable(
            Type::getInt8Ty(CtxL), true, GlobalValue::ExternalLinkage);
    GVL1->setName("missing");
    GlobalVariable *GVR1 = new GlobalVariable(
            Type::getInt8Ty(CtxR), true, GlobalValue::ExternalLinkage);
    GVR1->setName("missing2");
    ASSERT_EQ(DiffComp->testCmpGlobalValues(GVL1, GVR1), 1);
    ASSERT_EQ(ModComp->MissingDefs.size(), 1);
    ASSERT_EQ(ModComp->MissingDefs[0].first, GVL1);
    ASSERT_EQ(ModComp->MissingDefs[0].second, GVR1);
}

/// Tests ignoring of pointer casts using cmpBasicBlocks and cmpValues.
TEST_F(DifferentialFunctionComparatorTest, CmpValuesPointerCasts) {
    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    IntToPtrInst *PtrL =
            new IntToPtrInst(ConstantInt::get(Type::getInt32Ty(CtxL), 0),
                             PointerType::get(Type::getInt8Ty(CtxL), 0),
                             "",
                             BBL);
    IntToPtrInst *PtrR =
            new IntToPtrInst(ConstantInt::get(Type::getInt32Ty(CtxR), 0),
                             PointerType::get(Type::getInt8Ty(CtxR), 0),
                             "",
                             BBR);
    CastInst *CastL = new BitCastInst(
            PtrL, PointerType::get(Type::getInt32Ty(CtxL), 0), "", BBL);

    ReturnInst::Create(CtxL, CastL, BBL);
    ReturnInst::Create(CtxR, PtrR, BBR);

    // First, cmpBasicBlocks must be run to identify instructions to ignore
    // and then, cmpValues should ignore those instructions.
    ASSERT_EQ(DiffComp->testCmpBasicBlocks(BBL, BBR), 0);
    ASSERT_EQ(DiffComp->testCmpValues(PtrL, PtrR, true), 0);
    ASSERT_EQ(DiffComp->testCmpValues(CastL, PtrR, true), 0);
}

/// Test ignoring of a cast from a union type using cmpBasicBlocks and
/// cmpValues.
TEST_F(DifferentialFunctionComparatorTest, CmpValuesCastFromUnion) {
    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    StructType *UnionL = StructType::create({Type::getInt8Ty(CtxL)});
    UnionL->setName("union.test");
    Constant *ConstL = ConstantStruct::get(
            UnionL, {ConstantInt::get(Type::getInt8Ty(CtxL), 0)});
    Constant *ConstR = ConstantInt::get(Type::getInt8Ty(CtxR), 0);
    Constant *ConstR2 = ConstantInt::get(Type::getInt8Ty(CtxR), 1);
    CastInst *CastL = new BitCastInst(ConstL, Type::getInt8Ty(CtxL), "", BBL);

    ReturnInst::Create(CtxL, CastL, BBL);
    ReturnInst::Create(CtxR, ConstR, BBR);

    // First, cmpBasicBlocks must be run to identify instructions to ignore
    // and then, cmpValues should ignore those instructions.
    ASSERT_EQ(DiffComp->testCmpBasicBlocks(BBL, BBR), 0);
    ASSERT_EQ(DiffComp->testCmpValues(CastL, ConstR), 0);

    BBR->getTerminator()->eraseFromParent();
    ReturnInst::Create(CtxR, ConstR2, BBR);

    ASSERT_EQ(DiffComp->testCmpBasicBlocks(BBL, BBR), 1);
    ASSERT_EQ(DiffComp->testCmpValues(CastL, ConstR2), 1);
}

/// Test ignoring of a truncated integer using cmpBasicBlocks and cmpValues.
TEST_F(DifferentialFunctionComparatorTest, CmpValuesIntTrunc) {
    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    Constant *ConstL = ConstantInt::get(Type::getInt16Ty(CtxL), 0);
    Constant *ConstR = ConstantInt::get(Type::getInt16Ty(CtxR), 0);
    CastInst *CastL = new TruncInst(ConstL, Type::getInt8Ty(CtxL), "", BBL);

    ReturnInst::Create(CtxL, CastL, BBL);
    ReturnInst::Create(CtxR, ConstR, BBR);

    // First, cmpBasicBlocks must be run to identify instructions to ignore
    // and then, cmpValues should ignore those instructions.
    ASSERT_EQ(DiffComp->testCmpBasicBlocks(BBL, BBR), 1);
    ASSERT_EQ(DiffComp->testCmpValues(CastL, ConstR), -1);

    Conf.ControlFlowOnly = true;
    ASSERT_EQ(DiffComp->testCmpBasicBlocks(BBL, BBR), 0);
    ASSERT_EQ(DiffComp->testCmpValues(CastL, ConstR), 0);
    ASSERT_EQ(DiffComp->testCmpValues(ConstR, CastL), 0);
    Conf.ControlFlowOnly = false;
}

/// Test ignoring of an extended integer value with an unextended one
/// first without arithmetic instructions present (the extension should be
/// ignored), then again with them (the extension should not be ignored).
TEST_F(DifferentialFunctionComparatorTest, CmpValuesIntExt) {
    BasicBlock *BBL = BasicBlock::Create(CtxL, "", FL);
    BasicBlock *BBR = BasicBlock::Create(CtxR, "", FR);

    Constant *ConstL = ConstantInt::get(Type::getInt16Ty(CtxL), 0);
    Constant *ConstR = ConstantInt::get(Type::getInt16Ty(CtxR), 0);
    CastInst *CastL = new SExtInst(ConstL, Type::getInt32Ty(CtxL), "", BBL);

    auto *RetL = ReturnInst::Create(CtxL, CastL, BBL);
    auto *RetR = ReturnInst::Create(CtxR, ConstR, BBR);

    // First, cmpBasicBlocks must be run to identify instructions to ignore
    // and then, cmpValues should ignore those instructions.
    ASSERT_EQ(DiffComp->testCmpBasicBlocks(BBL, BBR), 1);
    ASSERT_EQ(DiffComp->testCmpValues(CastL, ConstR), 0);

    CastL->eraseFromParent();
    RetL->eraseFromParent();
    RetR->eraseFromParent();

    CastInst *CastL2 = new SExtInst(ConstL, Type::getInt64Ty(CtxL), "", BBL);
    BinaryOperator *ArithmL = BinaryOperator::Create(
            Instruction::BinaryOps::Add, CastL2, CastL2, "", BBL);
    BinaryOperator *ArithmR = BinaryOperator::Create(
            Instruction::BinaryOps::Add, ConstR, ConstR, "", BBR);
    ReturnInst::Create(CtxL, ArithmL, BBL);
    ReturnInst::Create(CtxR, ArithmR, BBR);

    ASSERT_EQ(DiffComp->testCmpBasicBlocks(BBL, BBR), 1);
    ASSERT_EQ(DiffComp->testCmpValues(CastL2, ConstR), -1);
}

/// Tests comparison of constants that were generated from macros.
TEST_F(DifferentialFunctionComparatorTest, CmpValuesMacroConstantMap) {
    // Create two different constants.
    Constant *ConstL = ConstantInt::get(Type::getInt8Ty(CtxR), 0);
    Constant *ConstR = ConstantInt::get(Type::getInt8Ty(CtxR), 1);

    // Compare them without entries in MacroConstantMap.
    ASSERT_EQ(DiffComp->testCmpValues(ConstL, ConstR), 1);
    ASSERT_EQ(DiffComp->testCmpValues(ConstL, ConstR), 1);

    // Compare them with corresponding entries in MacroConstantMap.
    DbgInfo->MacroConstantMap.insert({ConstL, "1"});
    DbgInfo->MacroConstantMap.insert({ConstR, "0"});

    ASSERT_EQ(DiffComp->testCmpValues(ConstL, ConstR), 0);
    ASSERT_EQ(DiffComp->testCmpValues(ConstL, ConstR), 0);

    // Compare them with non equal entries in MacroConstantMap.
    DbgInfo->MacroConstantMap.erase(ConstL);
    DbgInfo->MacroConstantMap.erase(ConstR);
    DbgInfo->MacroConstantMap.insert({ConstL, "42"});
    DbgInfo->MacroConstantMap.insert({ConstR, "93"});

    ASSERT_EQ(DiffComp->testCmpValues(ConstL, ConstR), 1);
    ASSERT_EQ(DiffComp->testCmpValues(ConstL, ConstR), 1);
}

/// Tests comparison of constant expressions containing bitcasts.
TEST_F(DifferentialFunctionComparatorTest, CmpConstants) {
    Conf.ControlFlowOnly = true;
    Constant *ConstL = ConstantInt::get(Type::getInt8Ty(CtxR), 0);
    Constant *ConstL2 = ConstantInt::get(Type::getInt8Ty(CtxR), 1);
    Constant *ConstR =
            ConstantExpr::getIntegerCast(ConstL, Type::getInt8Ty(CtxR), false);

    ASSERT_EQ(DiffComp->testCmpConstants(ConstL, ConstR), 0);
    ASSERT_EQ(DiffComp->testCmpConstants(ConstR, ConstL), 0);
    ASSERT_EQ(DiffComp->testCmpConstants(ConstL2, ConstR), -1);
    ASSERT_EQ(DiffComp->testCmpConstants(ConstR, ConstL2), 1);
}
#endif
