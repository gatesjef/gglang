// gg_main.cpp

#include "precompile.h"
#include "GGParser.h"
#include "LLVMIRCompiler.h"
//#include "GGLLVMEmitter.h"

void print_result(const CompileResult &result) {
  printf(result.error.error_string.c_str());
}

int main(int argc, char* argv[]) {
  std::string source_file = "..\\main.gg";
  std::string exe_file = "..\\me.exe";

  if (argc == 2)
  {
    source_file = argv[1];
    exe_file = to_exe_file(source_file);
  }

  //parse_program(source_file);
  //GGToken output = GGCompile(source_file);
  //GGLLVMEmitProgram(output);
  //return 0;

  CompileResult result = compile_program(source_file, exe_file);
  if (is_success(result) == false) {
    print_result(result);
  } else {
    printf("success\n");
  }

  char buffer[128];
  gets(buffer);
}

/*
define i32 @main() {
  %pi = alloca <{ i32, i32 }>
  store <{ i32, i32 }> zeroinitializer, <{ i32, i32 }>* %pi
  %1 = getelementptr <{ i32, i32 }>* %pi, i32 0, i32 0
  //%2 = getelementptr <{ i32, i32 }>* %pi, i32 0, i32 0
  //%3 = load i32* %2
  store i32 15, i32* %1
  %4 = getelementptr <{ i32, i32 }>* %pi, i32 0, i32 1
  //%5 = getelementptr <{ i32, i32 }>* %pi, i32 0, i32 1
  //%6 = load i32* %5
  store i32 -1, i32* %4
  
  
  
  %7 = getelementptr <{ i32, i32 }>* %pi, i32 0, i32 0
  %8 = load i32* %7
  %9 = getelementptr <{ i32, i32 }>* %pi, i32 0, i32 1
  %10 = load i32* %9
  %addtmp = add i32 %8, %10
  %addtmp1 = add i32 %addtmp, 65
  %11 = call i32 @putchar(i32 %addtmp1)
  ret i32 1
}
*/