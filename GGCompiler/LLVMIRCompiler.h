// LLVMIRCompiler.h

struct LLVMState {
  llvm::Module *module;
  llvm::IRBuilder<> *builder;
  llvm::LLVMContext *context;
  llvm::ExecutionEngine *engine;
};

void IRCompile(LLVMState &llvm, const char* obj_file, const char* exe_file);
