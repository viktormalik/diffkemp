#include "PatternGenerator.h"

void PatternRepresentation::refreshFunctions() {
    functions.first = mod->getFunction(funNames.first);
    functions.second = mod->getFunction(funNames.second);
}

PatternGenerator::MinimalModuleAnalysis::MinimalModuleAnalysis(Config &conf) {
    conf.refreshFunctions();
    /// All of these are not inside of the member initializer list,
    /// because we want to be sure, that functions are refreshed in
    /// conf.
    dbgInfo = std::make_unique<DebugInfo>(*conf.First,
                                          *conf.Second,
                                          conf.FirstFun,
                                          conf.SecondFun,
                                          CalledFirst,
                                          CalledSecond);
    modComp = std::make_unique<ModuleComparator>(*conf.First,
                                                 *conf.Second,
                                                 conf,
                                                 dbgInfo.get(),
                                                 StructSizeMapL,
                                                 StructSizeMapR,
                                                 StructDIMapL,
                                                 StructDIMapR);
    diffComp = std::make_unique<DifferentialFunctionComparator>(
            conf.FirstFun,
            conf.SecondFun,
            conf,
            dbgInfo.get(),
            &modComp.get()->Patterns,
            modComp.get());
    addFunPair(std::make_pair(conf.FirstFun, conf.SecondFun));
}

void PatternGenerator::determinePatternRange(PatternRepresentation *PatRep) {
    Config conf{PatRep->functions.first->getName().str(),
                PatRep->functions.second->getName().str(),
                PatRep->mod.get(),
                PatRep->mod.get()};

    auto semDiff = MinimalModuleAnalysis(conf);

    auto BBL = PatRep->functions.first->begin();
    auto BBR = PatRep->functions.second->begin();
    bool insidePatternValueRange = false;
    while (BBL != PatRep->functions.first->end()
           && BBR != PatRep->functions.second->end()) {
        auto InL = BBL->begin();
        auto InR = BBR->begin();
        while (InL != BBL->end() || InR != BBR->end()) {
            if (semDiff->maySkipInstruction(&(*InL))) {
                InL++;
                continue;
            }
            if (semDiff->maySkipInstruction(&(*InR))) {
                InR++;
                continue;
            }

            /// WARNING: This way of registration is really fishy, I should add
            /// some kind of checking to it. Otherwise, I may not actually
            /// catch some nasty bug.
            if (semDiff->cmpOperationsWithOperands(&(*InL), &(*InR))) {
                if (!insidePatternValueRange) { // and difference is in type
                                                // or value
                    InL->setMetadata(
                            "diffkemp.pattern",
                            PatRep->MDMap
                                    [PatternRepresentation::PATTERN_START]);
                    InR->setMetadata(
                            PatRep->mod->getMDKindID("diffkemp.pattern"),
                            PatRep->MDMap
                                    [PatternRepresentation::PATTERN_START]);
                    insidePatternValueRange = true;
                }
            }
            if (insidePatternValueRange) {
                if (InR->isTerminator()) {
                    InR->setMetadata(
                            PatRep->mod->getMDKindID("diffkemp.pattern"),
                            PatRep->MDMap[PatternRepresentation::PATTERN_END]);
                }
                if (InL->isTerminator()) {
                    InL->setMetadata(
                            PatRep->mod->getMDKindID("diffkemp.pattern"),
                            PatRep->MDMap[PatternRepresentation::PATTERN_END]);
                }
            }
            InR++;
            InL++;
        }
        BBR++;
        BBL++;
    }
}

bool isOpFunctionArg(Use &Op, Function &Fun) {
    for (auto argIter = Fun.arg_begin(); argIter != Fun.arg_end(); ++argIter) {
        if (Op.get() == argIter) {
            return true;
        }
    }
    return false;
}

void PatternGenerator::attachMetadata(Instruction *instr,
                                      std::string metadataStr) {
    // TODO: add some checking
    auto &instrContext = instr->getContext();
    MDNode *node =
            MDNode::get(instrContext, MDString::get(instrContext, metadataStr));
    instr->setMetadata("diffkemp.pattern", node);
}

/// TODO: Make this one function
/// TODO: Refactor me in accordance to FunctionType
Function *PatternGenerator::cloneFunctionWithExtraArgument(Module *dstMod,
                                                           Function *src,
                                                           Type &newType) {
    auto newFunTypeParams = src->getFunctionType()->params().vec();
    newFunTypeParams.push_back(&newType);
    auto newFunType =
            FunctionType::get(src->getReturnType(), newFunTypeParams, false);
    auto dst = Function::Create(
            newFunType, src->getLinkage(), "tmp" + src->getName(), dstMod);
    llvm::ValueToValueMapTy tmpValueMap;
    auto patternFuncArgIter = dst->arg_begin();
    for (auto &arg : src->args()) {
        patternFuncArgIter->setName(arg.getName());
        tmpValueMap[&arg] = &(*patternFuncArgIter++);
    }

    llvm::SmallVector<llvm::ReturnInst *, 8> returns;
    llvm::CloneFunctionInto(dst,
                            src,
                            tmpValueMap,
                            llvm::CloneFunctionChangeType::DifferentModule,
                            returns);
    /// We dont have to check for global variables, as they are already
    /// initialized from pattern initialization.
    return dst;
};

Function *PatternGenerator::cloneFunction(std::string prefix,
                                          Module *mod,
                                          Function *src) {
    auto dst = Function::Create(src->getFunctionType(),
                                src->getLinkage(),
                                prefix + src->getName().str(),
                                mod);
    llvm::ValueToValueMapTy tmpValueMap;
    auto patternFuncArgIter = dst->arg_begin();
    for (auto &arg : src->args()) {
        patternFuncArgIter->setName(arg.getName());
        tmpValueMap[&arg] = &(*patternFuncArgIter++);
    }

    llvm::SmallVector<llvm::ReturnInst *, 8> returns;
    llvm::CloneFunctionInto(dst,
                            src,
                            tmpValueMap,
                            llvm::CloneFunctionChangeType::DifferentModule,
                            returns);
    /// TODO: check if failed

    /// initialize global variables
    for (auto &BB : *dst) {
        for (auto &Inst : BB) {
            for (auto &Op : Inst.operands()) {
                if (auto *Global = dyn_cast<GlobalVariable>(Op)) {
                    auto newGlobal = dst->getParent()->getOrInsertGlobal(
                            Global->getName(), Global->getType());
                    Op->replaceAllUsesWith(newGlobal);
                }
            }
        }
    }
    /// Provide declaration of used functions
    for (auto &BB : *dst) {
        for (auto &Inst : BB) {
            if (auto call = dyn_cast<CallInst>(&Inst)) {
                auto calledFun = call->getCalledFunction();
                call->setCalledFunction(
                        Function::Create(calledFun->getFunctionType(),
                                         calledFun->getLinkage(),
                                         prefix + calledFun->getName(),
                                         dst->getParent()));
            }
        }
    }
    return dst;
};

bool PatternGenerator::addFunctionToPattern(Module *mod,
                                            Function *PatternFun,
                                            Function *CandidateFun,
                                            std::string patternName) {
    Config conf{PatternFun->getName().str(),
                CandidateFun->getName().str(),
                this->patterns[patternName]->mod.get(),
                mod};
    auto semDiff = MinimalModuleAnalysis(conf);

    /// Create a temporary function, that is going to be augmented, if inference
    /// succeeds, then it is going to replace the PatternFun

    auto BBL = PatternFun->begin();
    auto BBR = CandidateFun->begin();
    Function *tmpFun = nullptr;
    std::set<std::pair<Instruction *, Value *>> markedValues;
    std::set<Value *> parametrizedValues;
    while (BBL != PatternFun->end() && BBR != CandidateFun->end()) {
        auto InL = BBL->begin();
        auto InR = BBR->begin();
        while (InL != BBL->end() || InR != BBR->end()) {
            if (semDiff->maySkipInstruction(&(*InL))) {
                InL++;
                continue;
            }
            if (semDiff->maySkipInstruction(&(*InR))) {
                InR++;
                continue;
            }

            if (InL->getOpcode() != InR->getOpcode()) {
                /// We cannot create a pattern from code snippets with differing
                /// operations.
                return false;
            }
            if (InL->getNumOperands() != InR->getNumOperands()) {
                /// TODO: check if there is any operation with variatic number
                /// of operands
                return false;
            }
            for (auto &it : InL->operands()) {
                if (isOpFunctionArg(it, *PatternFun)) {
                    std::cout << "operand is a function argument" << std::endl;
                }
            }
            if (semDiff->cmpOperationsWithOperands(&(*InL), &(*InR))) {
                /// Ignore call instructions, as they usually call to different
                /// functions
                if (dyn_cast<CallInst>(InL)) {
                    ++InL, ++InR;
                    continue;
                }
                /// Same operations shouldn't have different operands count
                for (auto OpL = InL->op_begin(), OpR = InR->op_begin();
                     OpL != InL->op_end() && OpR != InR->op_end();
                     ++OpL, ++OpR) {
                    if (OpL->get()->getType() != OpR->get()->getType()) {
                        std::cout << "Not Implemented" << std::endl;
                    } else {
                        if (semDiff->cmpValues(OpL->get(), OpR->get())) {
                            /// Value is already parametrized
                            if (parametrizedValues.find(OpL->get())
                                != parametrizedValues.end()) {
                                std::cout << "already parametrized"
                                          << std::endl;
                                continue;
                            }
                            /// difference is in the value
                            /// Marked for future copy
                            auto pp = std::make_pair(&(*InL), OpL->get());
                            if (isOpFunctionArg(*OpL, *PatternFun)
                                || std::find(markedValues.begin(),
                                             markedValues.end(),
                                             pp)
                                           != markedValues.end()) {
                                std::cout << "parametrized twice, man"
                                          << std::endl;
                                continue;
                            }
                            if (tmpFun) {
                                tmpFun = cloneFunctionWithExtraArgument(
                                        tmpFun->getParent(),
                                        tmpFun,
                                        *OpL->get()->getType());
                            } else {
                                tmpFun = cloneFunctionWithExtraArgument(
                                        PatternFun->getParent(),
                                        PatternFun,
                                        *OpL->get()->getType());
                            }
                            // Skip if operand is a global variable
                            bool shouldContinue = false;
                            for (auto &Global :
                                 PatternFun->getParent()->globals()) {
                                if (OpL->get()->getValueID()
                                    == Global.getValueID()) {
                                    shouldContinue = true;
                                }
                            }
                            if (shouldContinue) {
                                continue;
                            }
                            markedValues.insert(
                                    std::make_pair(&(*InL), OpL->get()));
                            parametrizedValues.insert(OpL->get());
                        }
                    }
                }
            }
            InR++;
            InL++;
        }
        BBR++;
        BBL++;
    }
    if (tmpFun) {
        for (BBL = PatternFun->begin(), BBR = tmpFun->begin();
             BBL != PatternFun->end();
             ++BBL, ++BBR) {
            for (auto InL = BBL->begin(), InR = BBR->begin(); InL != BBL->end();
                 ++InL, ++InR) {
                for (auto &value : markedValues) {
                    if (value.first == &(*InL)) {
                        for (auto OpL = InL->op_begin(), OpR = InR->op_begin();
                             OpL != InL->op_end() && OpR != InR->op_end();
                             ++OpL, ++OpR) {
                            if (OpL->get() == value.second) {
                                auto ArgIter = tmpFun->arg_end();
                                --ArgIter;
                                OpR->get()->replaceAllUsesWith(ArgIter);
                            }
                        }
                    }
                }
                InR->copyMetadata(*InL);
            }
        }
        auto pastFunName = PatternFun->getName();
        PatternFun->eraseFromParent();
        tmpFun->setName(pastFunName);
    }
    return true;
}

bool PatternGenerator::addFunctionPairToPattern(
        std::pair<std::string, std::string> moduleFiles,
        std::pair<std::string, std::string> funNames,
        std::string patternName) {

    llvm::SMDiagnostic err;
    std::unique_ptr<Module> ModL(parseIRFile(moduleFiles.first, err, firstCtx));
    std::unique_ptr<Module> ModR(
            parseIRFile(moduleFiles.second, err, secondCtx));
    if (!ModL || !ModR) {
        err.print("Diffkemp", llvm::errs());
        return false;
    }

    Function *FunL = ModL->getFunction(funNames.first);
    Function *FunR = ModR->getFunction(funNames.second);
    if (!FunL || !FunR) {
        // TODO: Add some kind of exception
        return false;
    }

    /// This pattern is not yet initialized, and we have to initialize it by
    /// copying module functions to it.
    if (this->patterns.find(patternName) == this->patterns.end()) {
        this->patterns[patternName] = std::make_unique<PatternRepresentation>(
                patternName,
                ("diffkemp.old." + FunL->getName()).str(),
                ("diffkemp.new." + FunR->getName()).str());
        auto PatternRepr = this->patterns[patternName].get();
        PatternRepr->functions.first =
                cloneFunction("diffkemp.old.", PatternRepr->mod.get(), FunL);
        PatternRepr->functions.second =
                cloneFunction("diffkemp.new.", PatternRepr->mod.get(), FunR);

        auto attrs = AttributeList();
        PatternRepr->functions.first->setAttributes(attrs);
        PatternRepr->functions.second->setAttributes(attrs);

        /// Pattern was empty, hence provided functions are the most
        /// specific pattern that we can infere, thus generation have
        /// been successful and we are returning true.
        // TODO: Uncomment me for final version
        this->determinePatternRange(this->patterns[patternName].get());
        return true;
    }

    auto resultL = this->addFunctionToPattern(
            ModL.get(),
            this->patterns[patternName]->functions.first,
            FunL,
            patternName);
    auto resultR = this->addFunctionToPattern(
            ModR.get(),
            this->patterns[patternName]->functions.second,
            FunR,
            patternName);
    this->patterns[patternName]->refreshFunctions();
    this->determinePatternRange(this->patterns[patternName].get());

    if (!resultL || !resultR) {
        return false;
    }
    return true;
}

std::ostream &operator<<(std::ostream &os, PatternRepresentation &pat) {
    std::string tmpStr;
    raw_string_ostream tmp(tmpStr);
    tmp << *(pat.mod);
    os << tmpStr;
    return os;
}
