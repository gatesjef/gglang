// GGParser.h

//enum ParseResultType {
//  RESULT_SUCCESS,
//  RESULT_ERROR,
//  RESULT_NONE,
//};
//
//struct Token;
//struct ParseResult
//{
//    ParseResultType type;
//    Error error;
//    Token *start;
//    Token *end;
//};
//
//ParseResult parse_program(const char * source_file);

struct Error {
};

struct CompileResult {
  Error error;
};

CompileResult compile_program(const char *source_file);

