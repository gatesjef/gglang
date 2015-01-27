// GGParser.h

struct FileLocation {
  int file_index;
  int line;
  int column;
};

struct Error {
  FileLocation location;
  std::string error_string;
};

enum ResultType {
  RESULT_SUCCESS,
  RESULT_ERROR,
  RESULT_NONE,
};

struct CompileResult {
  ResultType result;
  Error error;
};

CompileResult compile_program(const char *source_file);
bool is_success(const CompileResult &result);
