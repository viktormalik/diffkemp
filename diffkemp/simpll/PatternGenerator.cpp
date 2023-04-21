#include "PatternGenerator.h"

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

bool PatternGenerator::addFunctionToPattern(
        std::pair<std::string, std::string> moduleFiles,
        std::pair<std::string, std::string> funNames,
        std::string patternName) {
    llvm::SMDiagnostic err;
    std::unique_ptr<Module> oldMod(
            parseIRFile(moduleFiles.first, err, firstCtx));
    std::unique_ptr<Module> newMod(
            parseIRFile(moduleFiles.second, err, secondCtx));
    if (!oldMod || !newMod) {
        err.print("Diffkemp", llvm::errs());
        return false;
    }

    auto oldFun = oldMod->getFunction(funNames.first);
    auto newFun = newMod->getFunction(funNames.second);
    if (!oldFun || !newFun) {
        // TODO: Add some kind of exception
        return false;
    }
    if (this->patterns.find(patternName) == this->patterns.end()) {
        this->patterns[patternName] =
                std::make_unique<llvm::Module>(patternName, this->firstCtx);
    }

    Config conf{funNames.first, funNames.second, oldMod.get(), newMod.get()};
    OverallResult res;
    processAndCompare(conf, res);

    for (const auto &result : res.functionResults) {
        for (const auto &obj : result.DifferingObjects) {
            std::cout << obj->getKind() << std::endl;
        }
    }
    return true;

#if 0
    /// If patter is null, then this is the first call of the function,
    /// in that case copy the function from first module and use it in
    /// further generation.
    pattern = std::make_unique<llvm::Module>("tmp", ctx);

    auto patternFunc = llvm::Function::Create(func->getFunctionType(),
                                              func->getLinkage(),
                                              "diffkemp.old.add",
                                              *pattern);
    if (patternFunc == nullptr) {
        std::cerr << "Error: Pattern func is null" << std::endl;
        return;
    }

    cloneFunction(*patternFunc, *func);
    for (auto &BB : *patternFunc) {
        for (auto &inst : BB) {
            /// Attach some kind of metadata in case of `store`
            /// instruction
            if (inst.getOpcodeName() == std::string("store")) {
                auto &instrCtx = inst.getContext();
                llvm::MDNode *node = llvm::MDNode::get(
                        instrCtx,
                        llvm::MDString::get(instrCtx, "pattern-start"));
                inst.setMetadata("diffkemp.pattern", node);
            } else if (inst.getOpcodeName() == std::string("add")) {
                auto &instrCtx = inst.getContext();
                llvm::MDNode *node = llvm::MDNode::get(
                        instrCtx, llvm::MDString::get(instrCtx, "pattern-end"));
                inst.setMetadata("diffkemp.pattern", node);
            }
        }
    }
    pattern->dump();
    return true;
#endif
}

void PatternGenerator::addFileForInference(std::string patternName,
                                           std::string funcName,
                                           std::string fileName) {
    llvm::SMDiagnostic err;
    std::unique_ptr<llvm::Module> mod(
            llvm::parseIRFile(fileName, err, firstCtx));
    if (!mod) {
        err.print("Diffkemp", llvm::errs());
    }
    auto func = mod->getFunction(funcName);
    if (func == nullptr) {
        // TODO: Add some kind of exception
        return;
    }
    if (this->patterns.find(patternName) == this->patterns.end()) {
        this->patterns[patternName] =
                std::make_unique<llvm::Module>(patternName, firstCtx);
    }
    /// If patter is null, then this is the first call of the function,
    /// in that case copy the function from first module and use it in
    /// further generation.
    pattern = std::make_unique<llvm::Module>("tmp", firstCtx);

    auto patternFunc = llvm::Function::Create(func->getFunctionType(),
                                              func->getLinkage(),
                                              "diffkemp.old.add",
                                              *pattern);
    if (patternFunc == nullptr) {
        std::cerr << "Error: Pattern func is null" << std::endl;
        return;
    }

    cloneFunction(*patternFunc, *func);
    for (auto &BB : *patternFunc) {
        for (auto &inst : BB) {
            /// Attach some kind of metadata in case of `store`
            /// instruction
            if (inst.getOpcodeName() == std::string("store")) {
                auto &instrCtx = inst.getContext();
                llvm::MDNode *node = llvm::MDNode::get(
                        instrCtx,
                        llvm::MDString::get(instrCtx, "pattern-start"));
                inst.setMetadata("diffkemp.pattern", node);
            } else if (inst.getOpcodeName() == std::string("add")) {
                auto &instrCtx = inst.getContext();
                llvm::MDNode *node = llvm::MDNode::get(
                        instrCtx, llvm::MDString::get(instrCtx, "pattern-end"));
                inst.setMetadata("diffkemp.pattern", node);
            }
        }
    }
    pattern->dump();
    isFreshRun = false;
}

std::ostream &operator<<(std::ostream &os, PatternRepresentation &pat) {
    std::string tmpStr;
    raw_string_ostream tmp(tmpStr);
    tmp << *(pat.mod);
    os << tmpStr;
    return os;
}
