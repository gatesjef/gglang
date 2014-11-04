// GGParser.h

enum ParseResultType {
  RESULT_SUCCESS,
  RESULT_ERROR,
  RESULT_NONE,
};

struct Token;
struct ParseResult
{
    ParseResultType type;
    Token *start;
    Token *end;
};

ParseResult parse_program(const char * source_file);