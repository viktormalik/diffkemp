#ifndef DIFFKEMP_SIMPLL_PATTERNGENERATOR_H
#define DIFFKEMP_SIMPLL_PATTERNGENERATOR_H

#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/MemorySSAUpdater.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Transforms/Utils/Cloning.h>

#include <iostream>

class PatternGenerator {
  public:
    PatternGenerator() : ctx(), isFreshRun(true) {
    };

    virtual ~PatternGenerator() = default;

    PatternGenerator(const PatternGenerator &other) noexcept {
        isFreshRun = other.isFreshRun;
    };

    PatternGenerator(PatternGenerator &&other) noexcept {
        std::swap(isFreshRun, other.isFreshRun);
    };

    PatternGenerator &operator=(const PatternGenerator &other) {
        return *this = PatternGenerator(other);
    }

    PatternGenerator &operator=(PatternGenerator &&other) {
        std::swap(isFreshRun, other.isFreshRun);
        return *this;
    }

    void addFileForInference(std::string fileName);

  private:
    llvm::LLVMContext ctx;
    bool isFreshRun;
    std::unique_ptr<llvm::Module> pattern;
};

#endif // DIFFKEMP_SIMPLL_PATTERNGENERATOR_H
