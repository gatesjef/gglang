// gg_main.cpp

#include "precompile.h"
#include "GGCompiler.h"
#include "GGLLVMEmitter.h"

void error(const char *format, ...)
{
  exit(-1);
}

int fsize(FILE *file) {
  fseek(file , 0 , SEEK_END);
  int size = ftell (file);
  rewind(file);
  return size;
}

int main(int argc, char* argv[])
{
  const char *input_filename = "../main.gg";
  FILE *input_file = fopen(input_filename, "rb");

  if (input_file == NULL) {
    error("cannot open %s", input_filename);
  }

  int file_size = fsize(input_file);
  char *file_data = (char *)malloc(file_size + 1);
  int read = fread(file_data, 1, file_size, input_file);
  file_data[file_size] = 0;

  if (read != file_size) {
    error("cannon read %s", input_filename);
  }

  GGToken output = GGCompile(file_data);

  //if (GGParseOutput.!= COMPILE_ERROR_NONE) {
  //	error(compile_results.error.error_string);
  //}
  GGLLVMEmitProgram(output);

  //system("..\\me.exe");

  return 0;
}
