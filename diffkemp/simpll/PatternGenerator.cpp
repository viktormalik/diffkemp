#include "PatternGenerator.h"

void PatternRepresentation::refreshFunctions() {
    functions.first = mod->getFunction(funNames.first);
    functions.second = mod->getFunction(funNames.second);
}

std::unique_ptr<Module> PatternRepresentation::generateVariant(
        std::pair<std::vector<InstructionVariant>,
                  std::vector<InstructionVariant>> var,
        std::string variantSuffix) {

    auto varMod = std::make_unique<Module>(mod->getName().str() + variantSuffix,
                                           this->context);

    /// It is impossible to have an odd number of variants, as we are inserting
    /// even empty ones.
    assert(variants.size() % 2 == 0);

    /// Perform no variation, if both variation vectors are empty.
    if (var.first.empty() && var.second.empty()) {
        return varMod;
    }
    auto VarFunL = cloneFunction(varMod.get(),
                                 functions.first,
                                 "",
                                 {},
                                 Type::getVoidTy(varMod->getContext()));
    auto VarFunR = cloneFunction(varMod.get(),
                                 functions.second,
                                 "",
                                 {},
                                 Type::getVoidTy(varMod->getContext()));

    applyVariant(var.first, VarFunL, true);
    applyVariant(var.second, VarFunR, false);

    return varMod;
}

void PatternRepresentation::replaceStructRelatedInst(Instruction &Inst,
                                                     InstructionVariant &var) {
    if (auto InstAlloca = dyn_cast<AllocaInst>(&Inst)) {
        auto NewInstAlloca = new AllocaInst(var.newType, 0, "", &Inst);
        NewInstAlloca->setAlignment(InstAlloca->getAlign());
        InstAlloca->replaceAllUsesWith(NewInstAlloca);
        InstAlloca->eraseFromParent();
    } else if (auto InstGep = dyn_cast<GetElementPtrInst>(&Inst)) {
        std::vector<Value *> operands;
        for (auto &op : InstGep->operands()) {
            operands.push_back(op);
        }
        auto ptr = operands[0];
        operands.erase(operands.begin());
        auto NewInstGep = GetElementPtrInst::Create(
                var.newType, ptr, operands, "", InstGep);
        InstGep->replaceAllUsesWith(NewInstGep);
        InstGep->eraseFromParent();
    }
}

void PatternRepresentation::replaceGlobalRelatedInst(Instruction &Inst,
                                                     InstructionVariant &var) {
    GlobalVariable *newGlobal = nullptr;
    auto varMod = Inst.getParent()->getParent()->getParent();
    for (auto &global : mod->globals()) {
        if (var.newGlobal->getName() == global.getName()) {
            newGlobal = &global;
            break;
        }
    }
    if (!newGlobal) {
        newGlobal = new GlobalVariable(*varMod,
                                       var.newGlobalInfo.type,
                                       false,
                                       var.newGlobalInfo.linkage,
                                       0,
                                       var.newGlobalInfo.name);
        newGlobal->setAlignment(var.newGlobalInfo.align);
    }
    Inst.getOperandUse(var.opPos).set(newGlobal);
}

void PatternRepresentation::applyVariant(std::vector<InstructionVariant> &vars,
                                         Function *VarFun,
                                         bool isLeftSide) {
    Function *Fun = isLeftSide ? functions.first : functions.second;
    for (auto &var : vars) {
        bool found = false;
        for (auto BBL = Fun->begin(), BBR = VarFun->begin(); BBL != Fun->end();
             ++BBL, ++BBR) {
            for (auto InstL = BBL->begin(), InstR = BBR->begin();
                 InstL != BBL->end();
                 ++InstL, ++InstR) {
                if (&(*InstL) == var.inst) {
                    switch (var.kind) {
                    case InstructionVariant::TYPE:
                        replaceStructRelatedInst(*InstR, var);
                        break;
                    case InstructionVariant::GLOBAL:
                        replaceGlobalRelatedInst(*InstR, var);
                        break;
                    }
                    found = true;
                    break;
                }
            }
            if (found) {
                break;
            }
        }
    }
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

void PatternGenerator::determinePatternRange(PatternRepresentation *PatRep,
                                             Module &mod) {
    auto FunL = mod.getFunction(PatRep->functions.first->getName());
    auto FunR = mod.getFunction(PatRep->functions.second->getName());

    Config conf{FunL->getName().str(), FunR->getName().str(), &mod, &mod};

    auto semDiff = MinimalModuleAnalysis(conf);

    bool insidePatternValueRange = false;
    for (auto BBL = FunL->begin(), BBR = FunR->begin();
         BBL != FunL->end() || BBR != FunR->end();
         ++BBL, ++BBR) {
        for (auto InstL = BBL->begin(), InstR = BBR->begin();
             InstL != BBL->end() || InstR != BBR->end();
             ++InstL, ++InstR) {
            if (semDiff->cmpOperationsWithOperands(&(*InstL), &(*InstR)) != 0) {
                if (!PatRep->isMetadataSet || &mod != PatRep->mod.get()) {
                    /// Metadata needs to be registered in the module, but
                    /// for some reason they are not accessible the first
                    /// time that they are used.
                    InstL->setMetadata(
                            "diffkemp.pattern",
                            PatRep->MDMap
                                    [PatternRepresentation::PATTERN_START]);
                    PatRep->isMetadataSet = true;
                } else {
                    InstL->setMetadata(
                            mod.getMDKindID("diffkemp.pattern"),
                            PatRep->MDMap
                                    [PatternRepresentation::PATTERN_START]);
                }
                InstR->setMetadata(
                        mod.getMDKindID("diffkemp.pattern"),
                        PatRep->MDMap[PatternRepresentation::PATTERN_START]);
                insidePatternValueRange = true;
                break;
            }
        }
        if (insidePatternValueRange) {
            break;
        }
    }

    if (!insidePatternValueRange) {
        return;
    }

    for (auto &BBL : *FunL) {
        for (auto &InstL : BBL) {
            if (auto RetInst = dyn_cast<ReturnInst>(&InstL)) {
                RetInst->setMetadata(
                        mod.getMDKindID("diffkemp.pattern"),
                        PatRep->MDMap[PatternRepresentation::PATTERN_END]);
            }
        }
    }
    for (auto &BBR : *FunR) {
        for (auto &InstR : BBR) {
            if (auto RetInst = dyn_cast<ReturnInst>(&InstR)) {
                RetInst->setMetadata(
                        mod.getMDKindID("diffkemp.pattern"),
                        PatRep->MDMap[PatternRepresentation::PATTERN_END]);
            }
        }
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

void remapVariants(Function *src,
                   Function *dst,
                   std::vector<InstructionVariant> &vars) {
    for (auto BBL = src->begin(), BBR = dst->begin(); BBL != src->end();
         ++BBL, ++BBR) {
        for (auto InstL = BBL->begin(), InstR = BBR->begin();
             InstL != BBL->end();
             ++InstL, ++InstR) {
            for (auto &var : vars) {
                if (var.inst == &(*InstL)) {
                    var.inst = &(*InstR);
                }
            }
        }
    }
}

Function *cloneFunction(Module *dstMod,
                        Function *src,
                        std::string prefix,
                        std::vector<Type *> newArgTypes,
                        Type *newReturnType,
                        StructTypeRemapper *remapper) {
    /// Merge new function argument types with new types and popule one from
    /// the source with value mapping between source and destination
    /// functions.
    auto newFunTypeParams = src->getFunctionType()->params().vec();
    for (auto &newArgType : newArgTypes) {
        newFunTypeParams.push_back(newArgType);
    }
    FunctionType *newFunType;
    if (!newReturnType) {
        newFunType = FunctionType::get(
                src->getReturnType(), newFunTypeParams, false);
    } else {
        newFunType = FunctionType::get(newReturnType, newFunTypeParams, false);
    }
    auto dst = Function::Create(
            newFunType, src->getLinkage(), prefix + src->getName(), dstMod);
    llvm::ValueToValueMapTy tmpValueMap;
    auto patternFuncArgIter = dst->arg_begin();
    for (auto &arg : src->args()) {
        patternFuncArgIter->setName(arg.getName());
        tmpValueMap[&arg] = &(*patternFuncArgIter++);
    }

    auto tmpRemapper = std::make_unique<StructTypeRemapper>();
    if (!remapper) {
        /// If function contains any structure types, that are already present
        /// in the destination context (comparison is done by name only), then
        /// remap instances of the source structure type to destination contexts
        /// structure type.
        std::vector<StructType *> foundStructTypes;
        for (auto &BB : *src) {
            for (auto &Inst : BB) {
                if (auto InstAlloca = dyn_cast<AllocaInst>(&Inst)) {
                    if (auto StType = dyn_cast<StructType>(
                                InstAlloca->getAllocatedType())) {
                        foundStructTypes.push_back(StType);
                    }
                } else if (auto InstGEP = dyn_cast<GetElementPtrInst>(&Inst)) {
                    if (auto StType = dyn_cast<StructType>(
                                InstGEP->getSourceElementType())) {
                        foundStructTypes.push_back(StType);
                    }
                }
            }
        }
        for (auto &StructL : dstMod->getIdentifiedStructTypes()) {
            /// We want to prevent double mapping
            auto structIter = foundStructTypes.begin();
            while (structIter != foundStructTypes.end()) {
                if ((*structIter)->getStructName()
                    == StructL->getStructName()) {
                    tmpRemapper->addNewMapping(*structIter, StructL);
                    foundStructTypes.erase(structIter);
                } else {
                    ++structIter;
                }
            }
        }

        /// Check if source module has any structure types that have suffix,
        /// that is because of the shared context of read IR. Try to remap the
        /// structure to it's variant without suffix in the destination module.
        ///
        /// If there is non, then create it and then remap source to it.
        for (auto &sTypeL : src->getParent()->getIdentifiedStructTypes()) {
            if (hasSuffix(sTypeL->getStructName().str())) {
                auto nameWithoutSuffix =
                        dropSuffix(sTypeL->getStructName().str());
                bool found = false;
                for (auto &sTypeR : dstMod->getIdentifiedStructTypes()) {
                    if (nameWithoutSuffix == sTypeR->getStructName().str()) {
                        tmpRemapper->addNewMapping(sTypeL, sTypeR);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    auto sTypeR = StructType::create(dstMod->getContext(),
                                                     sTypeL->elements(),
                                                     nameWithoutSuffix);
                    tmpRemapper->addNewMapping(sTypeL, sTypeR);
                }
            }
        }
        if (!tmpRemapper->empty()) {
            remapper = tmpRemapper.get();
        }
    }

    /// Builtin clone functions needs to know whether cloning appears in the
    /// same module.
    CloneFunctionChangeType changeType =
            (dstMod == src->getParent()
                     ? CloneFunctionChangeType::LocalChangesOnly
                     : CloneFunctionChangeType::DifferentModule);

    SmallVector<llvm::ReturnInst *, 8> returns;
    CloneFunctionInto(
            dst, src, tmpValueMap, changeType, returns, "", nullptr, remapper);
    return dst;
}

bool PatternGenerator::addFunctionToPattern(Module *mod,
                                            Function *PatternFun,
                                            Function *CandidateFun,
                                            std::string patternName) {
    Config conf{PatternFun->getName().str(),
                CandidateFun->getName().str(),
                this->patterns[patternName]->mod.get(),
                mod};
    auto semDiff = MinimalModuleAnalysis(conf);

    /// Create a temporary function, that is going to be augmented, if
    /// inference succeeds, then it is going to replace the PatternFun

    auto BBL = PatternFun->begin();
    auto BBR = CandidateFun->begin();
    Function *tmpFun = nullptr;
    std::set<std::pair<Instruction *, Value *>> markedValues;
    std::set<Value *> parametrizedValues;
    std::vector<Type *> futureParamTypes;
    std::vector<InstructionVariant> variants;
    while (BBL != PatternFun->end() && BBR != CandidateFun->end()) {
        auto InL = BBL->begin();
        auto InR = BBR->begin();
        while (InL != BBL->end() || InR != BBR->end()) {
            if (InL->getOpcode() != InR->getOpcode()) {
                /// We cannot create a pattern from code snippets with
                /// differing operations.
                return false;
            }
            auto AllocaL = dyn_cast<AllocaInst>(InL);
            auto AllocaR = dyn_cast<AllocaInst>(InR);
            if (AllocaL && AllocaR) {
                auto StructL =
                        dyn_cast<StructType>(AllocaL->getAllocatedType());
                auto StructR =
                        dyn_cast<StructType>(AllocaR->getAllocatedType());
                if (StructL != nullptr && StructR != nullptr) {
                    if (StructL != StructR) {
                        variants.emplace_back(&(*InL), StructR, 0);
                    }
                }
            }
            auto GepL = dyn_cast<GetElementPtrInst>(InL);
            auto GepR = dyn_cast<GetElementPtrInst>(InR);
            if (GepL && GepR) {
                auto StructL =
                        dyn_cast<StructType>(GepL->getSourceElementType());
                auto StructR =
                        dyn_cast<StructType>(GepR->getSourceElementType());
                if (StructL != nullptr && StructR != nullptr) {
                    if (StructL != StructR) {
                        variants.emplace_back(&(*InL), StructR, 0);
                    }
                }
            }
            if (!semDiff->cmpOperationsWithOperands(&(*InL), &(*InR))) {
                ++InL, ++InR;
                continue;
            }
            /// Ignore call instructions, as they usually call to different
            /// functions
            if (dyn_cast<CallInst>(InL)) {
                ++InL, ++InR;
                continue;
            }
            /// Same operations shouldn't have different operands count
            unsigned i = 0;
            for (auto OpL = InL->op_begin(), OpR = InR->op_begin();
                 OpL != InL->op_end() && OpR != InR->op_end();
                 ++OpL, ++OpR) {
                /// Elementar types usually use different instructions,
                /// hence at this point they are already out of the game.
                auto OpTypeL = OpL->get()->getType();
                auto OpTypeR = OpR->get()->getType();
                if (OpTypeL != OpTypeR) {
                    std::cout << "Not Implemented" << std::endl;
                } else {
                    if (semDiff->cmpValues(OpL->get(), OpR->get())) {
                        // Skip if operand is a global variable
                        if (isValueGlobal(*OpL->get(),
                                          *PatternFun->getParent())) {
                            GlobalVariable *gvar =
                                    dyn_cast<GlobalVariable>(OpR->get());
                            variants.emplace_back(&(*InL), gvar, i);
                            ++i;
                            continue;
                        }
                        /// Value is already parametrized
                        if (parametrizedValues.find(OpL->get())
                            != parametrizedValues.end()) {
                            ++i;
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
                            ++i;
                            continue;
                        }
                        futureParamTypes.push_back(OpL->get()->getType());
                        if (tmpFun) {
                            tmpFun = cloneFunction(tmpFun->getParent(),
                                                   tmpFun,
                                                   "tmp.",
                                                   {OpL->get()->getType()});
                        } else {
                            tmpFun = cloneFunction(PatternFun->getParent(),
                                                   PatternFun,
                                                   "tmp.",
                                                   {OpL->get()->getType()});
                        }
                        markedValues.insert(
                                std::make_pair(&(*InL), OpL->get()));
                        parametrizedValues.insert(OpL->get());
                    }
                }
                ++i;
            }
            InR++;
            InL++;
        }
        BBR++;
        BBL++;
    }
    for (auto &var : variants) {
        switch (var.kind) {
        case InstructionVariant::TYPE:
            if (auto s = dyn_cast<StructType>(var.newType)) {
                std::cout << "New Type Name: " << s->getStructName().str()
                          << std::endl;
            }
        }
    }
    /// We want to insert even empty variants, as we want to make pairings
    this->patterns[patternName]->variants.push_back(variants);
    /// Mapping of new function arguments to new parameters
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

        for (auto &var : this->patterns[patternName]->variants) {
            remapVariants(PatternFun, tmpFun, var);
        }

        PatternFun->eraseFromParent();
        tmpFun->setName(pastFunName);

        /// It has been observed, that sometimes the temporary functions were
        /// not erased. Because of that we want to be sure as otherwise they
        /// polute the output.
        std::vector<Function *> toRemove;
        for (auto &f : tmpFun->getParent()->getFunctionList()) {
            if (f.getName().str().rfind("tmp", 0) == 0) {
                toRemove.push_back(&f);
            }
        }
        for (auto &f : toRemove) {
            f->eraseFromParent();
        }
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
        const std::string oldPrefix = "diffkemp.old.";
        const std::string newPrefix = "diffkemp.new.";
        this->patterns[patternName] = std::make_unique<PatternRepresentation>(
                patternName,
                (oldPrefix + FunL->getName()).str(),
                (newPrefix + FunR->getName()).str());
        auto PatternRepr = this->patterns[patternName].get();
        PatternRepr->functions.first =
                cloneFunction(PatternRepr->mod.get(), FunL, oldPrefix);
        PatternRepr->functions.second =
                cloneFunction(PatternRepr->mod.get(), FunR, newPrefix);

        auto attrs = AttributeList();
        PatternRepr->functions.first->setAttributes(attrs);
        PatternRepr->functions.second->setAttributes(attrs);

        /// Pattern was empty, hence provided functions are the most
        /// specific pattern that we can infere, thus generation have
        /// been successful and we are returning true.
        // TODO: Uncomment me for final version
        this->determinePatternRange(this->patterns[patternName].get(),
                                    *this->patterns[patternName]->mod.get());
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
    this->determinePatternRange(this->patterns[patternName].get(),
                                *this->patterns[patternName]->mod.get());

    if (!resultL || !resultR) {
        this->patterns.erase(patternName);
        return false;
    }
    return true;
}

bool PatternGenerator::isValueGlobal(Value &val, Module &mod) {
    for (auto &global : mod.globals()) {
        if (val.getValueID() == global.getValueID()) {
            return true;
        }
    }
    return false;
}

std::ostream &operator<<(std::ostream &os, PatternRepresentation &pat) {
    std::string tmpStr;
    raw_string_ostream tmp(tmpStr);
    tmp << *(pat.mod);
    os << tmpStr;
    return os;
}
