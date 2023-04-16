//===------------------ FFI.h - C interface for SimpLL --------------------===//
//
//       SimpLL - Program simplifier for analysis of semantic difference      //
//
// This file is published under Apache 2.0 license. See LICENSE for details.
// Author: Tomas Glozar, tglozar@gmail.com
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains declarations of C functions and structure types used for
/// interacting with DiffKemp.
///
//===----------------------------------------------------------------------===//

#ifdef __cplusplus
extern "C" {
#endif

// CFFI_DECLARATIONS_START
// Note: this comment is an identifier for simpll_build.py. Any changes made to
// it should be reflected there.

struct config {
    const char *CacheDir;
    const char *Patterns;
    const char *Variable;
    int OutputLlvmIR;
    int ControlFlowOnly;
    int PrintAsmDiffs;
    int PrintCallStacks;
    int Verbosity;
};

struct ptr_array {
    void **arr;
    unsigned long len;
};

struct kernel_param {
    const char *name;
    int *indices;
    unsigned long indices_n;
};

void *loadModule(const char *Path);

void freeModule(void *ModRaw);

void freePointerArray(struct ptr_array PtrArr);

void freeStringArray(struct ptr_array PtrArr);

void *getFunction(void *ModRaw, const char *Fun);

const char *getFunctionName(void *FunRaw);

int isDeclaration(void *FunRaw);

/// Get all functions recursively called by FunRaw.
/// Note: this is a C interface wrapper for CalledFunctionsAnalysis.
struct ptr_array getCalledFunctions(void *FunRaw);

/// Find the name of the global variable that corresponds to the given module
/// parameter.
const char *findParamVarC(const char *Param, void *ModRaw);

/// Find names of all functions using the given parameter (global variable).
struct ptr_array getFunctionsUsingParamC(const char *ParamName,
                                         int *indices,
                                         unsigned long indices_n,
                                         void *ModRaw);

/// Creates a SysctlTable object used for looking up sysctls in a sysctl table.
void *getSysctlTable(void *ModRaw, const char *CtlTable);

void freeSysctlTable(void *SysctlTableRaw);

/// Parse all sysctls entries that match the given pattern. Parsed entries
/// are LLVM objects of type "struct ctl_table" containing the sysctl
/// definition.
/// Returns the list of sysctl names.
struct ptr_array parseSysctls(const char *SysctlPattern, void *SysctlTableRaw);

/// Get the proc handler function for the given sysctl option.
void *getProcFun(const char *Sysctl, void *SysctlTableRaw);

void freeKernelParam(struct kernel_param Param);

/// Get the child node of the given sysctl table entry.
struct kernel_param getChild(const char *Sysctl, void *SysctlTableRaw);

/// Get the data variable for the given sysctl option.
struct kernel_param getData(const char *Sysctl, void *SysctlTableRaw);

void readPatternConfig(const char *path);
void runGeneratePattern(const char *function, const char *fileName);
void runGeneratePatternV2(const char *firstFun,
                          const char *firstFileName,
                          const char *secondFun,
                          const char *secondFileName);
void reportPattern();

/// Simplifies modules and compares the specified functions.
void runSimpLL(void *ModL,
               void *ModR,
               const char *ModLOut,
               const char *ModROut,
               const char *FunL,
               const char *FunR,
               struct config Conf,
               char *Output);

/// Clones modules to get separate copies of them and runs the simplification
/// and comparison on the copies.
void cloneAndRunSimpLL(void *ModL,
                       void *ModR,
                       const char *ModLOut,
                       const char *ModROut,
                       const char *FunL,
                       const char *FunR,
                       struct config Conf,
                       char *Output);

/// Loads modules from the specified filers and runs the simplification and
/// comparison on the loaded objects, which are discarded after the comparison.
void parseAndRunSimpLL(const char *ModL,
                       const char *ModR,
                       const char *ModLOut,
                       const char *ModROut,
                       const char *FunL,
                       const char *FunR,
                       struct config Conf,
                       char *Output);

/// Runs preprocess passes on module and marks it as being preprocessed so they
/// won't be run again when the module is compared.
void preprocessModuleC(void *Mod, int ControlFlowOnly);

void shutdownSimpLL();

// CFFI_DECLARATIONS_END
// Note: this comment is an identifier for simpll_build.py. Any changes made to
// it should be reflected there.

#ifdef __cplusplus
}
#endif
