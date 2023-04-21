#include "PatternGenerator.h"

void PatternGenerator::attachMetadata(Instruction *instr,
                                      std::string metadataStr) {
    // TODO: add some checking
    auto &instrContext = instr->getContext();
    MDNode *node =
            MDNode::get(instrContext, MDString::get(instrContext, metadataStr));
    instr->setMetadata("diffkemp.pattern", node);
}

Function *PatternGenerator::cloneFunction(Function *dst, Function *src) {
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
    // check if failed
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
    conf.refreshFunctions();

    std::set<const Function *> CalledFirst;
    std::set<const Function *> CalledSecond;
    auto DI = std::make_unique<DebugInfo>(*conf.First,
                                          *conf.Second,
                                          PatternFun,
                                          CandidateFun,
                                          CalledFirst,
                                          CalledSecond);

    StructureSizeAnalysis::Result StructSizeMapL;
    StructureSizeAnalysis::Result StructSizeMapR;
    StructureDebugInfoAnalysis::Result StructDIMapL;
    StructureDebugInfoAnalysis::Result StructDIMapR;

    auto modComp = std::make_unique<ModuleComparator>(*conf.First,
                                                      *conf.Second,
                                                      conf,
                                                      DI.get(),
                                                      StructSizeMapL,
                                                      StructSizeMapR,
                                                      StructDIMapL,
                                                      StructDIMapR);

    if (!(conf.FirstFun && conf.SecondFun)) {
        return false;
    }
    auto diffComp = std::make_unique<DifferentialFunctionComparator>(
            PatternFun,
            CandidateFun,
            conf,
            DI.get(),
            &modComp.get()->Patterns,
            modComp.get());

    modComp->ComparedFuns.emplace(std::make_pair(PatternFun, CandidateFun),
                                  Result(PatternFun, CandidateFun));

    auto BBL = PatternFun->begin();
    auto BBR = CandidateFun->begin();
    bool insidePatternValueRange = false;
    while (BBL != PatternFun->end() && BBR != CandidateFun->end()) {
        auto InL = BBL->begin();
        auto InR = BBR->begin();
        while (InL != BBL->end() || InR != BBR->end()) {
            if (diffComp->maySkipInstruction(&(*InL))) {
                InL++;
                continue;
            }
            if (diffComp->maySkipInstruction(&(*InR))) {
                InR++;
                continue;
            }

            if (diffComp->cmpOperationsWithOperands(&(*InL), &(*InR))) {
                if (!insidePatternValueRange) { // and difference is in type
                                                // or value
                    attachMetadata(&(*InL), "pattern-start");
                    attachMetadata(&(*InR), "pattern-start");
                    insidePatternValueRange = true;
                }
                std::cout << "Functions do differ" << std::endl;
                // patterns lasts till the next non-differing
                // instruction
            } else {
                if (insidePatternValueRange) {
                    std::cout << "end of pattern range" << std::endl;
                    attachMetadata(&(*InL), "pattern-end");
                    attachMetadata(&(*InR), "pattern-end");
                }
            }
            InR++;
            InL++;
        }
        BBR++;
        BBL++;
    }
    return true;
}

bool PatternGenerator::addFunctionPairToPattern(
        std::pair<std::string, std::string> moduleFiles,
        std::pair<std::string, std::string> funNames,
        std::string patternName) {

    std::cout << "Process of adding function to a pattern" << std::endl;

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

    if (this->patterns.find(patternName) == this->patterns.end()) {
        this->patterns[patternName] =
                std::make_unique<PatternRepresentation>(patternName);
        auto PatternRepr = this->patterns[patternName].get();
        PatternRepr->functions.first = cloneFunction(
                Function::Create(FunL->getFunctionType(),
                                 FunL->getLinkage(),
                                 "diffkemp.old." + FunL->getName(),
                                 PatternRepr->mod.get()),
                FunL);
        PatternRepr->functions.second = cloneFunction(
                Function::Create(FunR->getFunctionType(),
                                 FunR->getLinkage(),
                                 "diffkemp.new." + FunR->getName(),
                                 PatternRepr->mod.get()),
                FunR);
        std::cout << *PatternRepr << std::endl;
        /// Pattern was empty, hence provided functions are the most
        /// specific pattern that we can infere, thus generation have
        /// been successful and we are returning true.
        // TODO: Uncomment me for final version
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

    std::cout << *(this->patterns[patternName]) << std::endl;

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
