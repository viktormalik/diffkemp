#include "PatternGenerator.h"

void PatternGenerator::addFileForInference(std::string fileName) const {
    llvm::SMDiagnostic err;
    std::unique_ptr<llvm::Module> mod(llvm::parseIRFile(fileName, err, ctx));
    if (!mod) {
        err.print("Diffkemp", llvm::errs());
    }
    for (auto &func : *mod) {
        /// If patter is null, then this is the first call of the function,
        /// in that case copy the function from first module and use it in
        /// further generation.
        if (isFreshRun) {
            pattern = std::make_unique<llvm::Module>("tmp", ctx);

            auto patternFunc = llvm::Function::Create(func.getFunctionType(),
                                                      func.getLinkage(),
                                                      "tmp_add",
                                                      *pattern);
            if (patternFunc == nullptr) {
                std::cerr << "Error: Pattern func is null" << std::endl;
                return;
            }

            llvm::ValueToValueMapTy tmp_vmap;

            auto patternFuncArgIter = patternFunc->arg_begin();
            for (auto &arg : func.args()) {
                patternFuncArgIter->setName(arg.getName());
                tmp_vmap[&arg] = &(*patternFuncArgIter++);
            }

            llvm::SmallVector<llvm::ReturnInst *, 8> returns;
            llvm::CloneFunctionInto(
                    patternFunc,
                    &func,
                    tmp_vmap,
                    llvm::CloneFunctionChangeType::DifferentModule,
                    returns);

            for (auto &BB : *patternFunc) {
                for (auto &inst : BB) {
                    /// Attach some kind of metadata in case of `store`
                    /// instruction
                    if (inst.getOpcodeName() == std::string("store")) {
                        auto &instrCtx = inst.getContext();
                        llvm::MDNode *node = llvm::MDNode::get(
                                instrCtx,
                                llvm::MDString::get(instrCtx,
                                                    "Diffkemp Patter"));
                        inst.setMetadata("diffkemp.pattern", node);
                    }
                }
            }
            pattern->dump();
            isFreshRun = false;
        }
    }
}
