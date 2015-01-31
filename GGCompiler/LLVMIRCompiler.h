// LLVMIRCompiler.h

struct LLVMState {
  llvm::Module *module;
  llvm::IRBuilder<> *builder;
  llvm::LLVMContext *context;
};

void IRCompile(LLVMState &llvm);
