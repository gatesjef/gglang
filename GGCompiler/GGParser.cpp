// GGParser.cpp

#include "Precompile.h"
//#include "GGCompiler.h"

struct SubString {
  const char *start;
  int length;
};

enum TokenType {
  TOKEN_NONE,

TOKEN_LIST_START,
  TOKEN_PROGRAM,
  TOKEN_FUNCTION_BODY,
  TOKEN_FUNCTION_PARAMS,
  TOKEN_FUNCTION_TYPE_PARAMS,
  TOKEN_FUNCTION_CALL_PARAMS,
  TOKEN_STRUCT_FIELDS,
  TOKEN_BLOCK_STATEMENT,
TOKEN_LIST_END,

TOKEN_TUPLE_START,
  TOKEN_IMPORT_STATEMENT,

  TOKEN_IF_STATEMENT,
  TOKEN_FOR_STATEMENT,
  TOKEN_SWITCH_STATEMENT,
  TOKEN_RETURN_STATEMENT,
  TOKEN_EXPRESSION_STATEMENT,
  TOKEN_ASSIGMENT_STATEMENT,

  TOKEN_EXTERNAL_FUNCTION_DECLARATION,

  TOKEN_TYPEDEF_DEFINITION,
  TOKEN_STRUCT_DEFINITION,
  TOKEN_ENUM_DEFINITION,
  TOKEN_LLVM_TYPE_DEFINITION,
  TOKEN_VARIABLE_DEFINITION,
  TOKEN_FUNCTION_DEFINITION,

  TOKEN_FUNCTION_PARAM,

  TOKEN_POINTER_TYPE,
  TOKEN_ARRAY_TYPE,
  TOKEN_FUNCTION_TYPE,

  TOKEN_BINARY_EXPRESSION,
  TOKEN_UNARY_EXPRESSION,
  TOKEN_OP_POSTFIX_INC,
  TOKEN_OP_POSTFIX_DEC,
  TOKEN_OP_MEMBER,
  TOKEN_OP_FUNCTION_CALL,
  TOKEN_OP_ARRAY_INDEX,
TOKEN_TUPLE_END,

TOKEN_SUBSTRING_START,
  TOKEN_RAW_LLVM_TYPE,
  TOKEN_INLINE_LLVM,

  TOKEN_COMPOUND_ASSIGMENT_OPERATOR,
  TOKEN_BINARY_OPERATOR,
  TOKEN_UNARY_OPERATOR,
  TOKEN_POST_OPERATOR,

  TOKEN_STRING_LITERAL,
  TOKEN_INTEGER_LITERAL,
  TOKEN_FLOAT_LITERAL,

  TOKEN_IDENTIFIER,
  //TOKEN_TYPE_IDENTIFIER,
  //TOKEN_FUNCTION_IDENTIFIER,
  //TOKEN_VARIABLE_IDENTIFIER,
TOKEN_SUBSTRING_END,

TOKEN_OP_START,
  TOKEN_ASSIGNMENT_OP,
  TOKEN_ADD_ASSIGNMENT_OP,
  TOKEN_SUB_ASSIGNMENT_OP,
  TOKEN_MUL_ASSIGNMENT_OP,
  TOKEN_DIV_ASSIGNMENT_OP,
  TOKEN_REM_ASSIGNMENT_OP,
  TOKEN_AND_ASSIGNMENT_OP,
  TOKEN_OR_ASSIGNMENT_OP,
  TOKEN_XOR_ASSIGNMENT_OP,
  TOKEN_NOT_ASSIGNMENT_OP,
  TOKEN_PRE_INC_OP,
  TOKEN_PRE_DEC_OP,
  TOKEN_POSITIVE_OP,
  TOKEN_NEGATIVE_OP,
  TOKEN_ADDRESS_OP,
  TOKEN_DEREF_OP,
  TOKEN_LOGICAL_NOT_OP,
  TOKEN_BITWISE_NOT_OP,
  TOKEN_ADD_OP,
  TOKEN_SUB_OP,
  TOKEN_MUL_OP,
  TOKEN_DIV_OP,
  TOKEN_REM_OP,
  TOKEN_LOGICAL_AND_OP,
  TOKEN_LOGICAL_OR_OP,
  TOKEN_BITWISE_AND_OP,
  TOKEN_BITWISE_OR_OP,
  TOKEN_BITWISE_XOR_OP,
  TOKEN_LSHIFT_OP,
  TOKEN_RSHIFT_OP,
  TOKEN_CMP_EQ_OP,
  TOKEN_CMP_NEQ_OP,
  TOKEN_CMP_LTE_OP,
  TOKEN_CMP_GTE_OP,
  TOKEN_CMP_LT_OP,
  TOKEN_CMP_GT_OP,
TOKEN_OP_END,
};

struct ParseTable;
struct Type;

struct Token {
  TokenType type;
  Type *expr_type;

  union {
    SubString substring;
    struct {
      Token *start;
      Token *end;
    };
    const ParseTable *entry;
  };

  Token *next;
};

struct Error {
};

struct TokenPool {
};

const int MAX_TOKENS = 16 * 1024;
struct ParserState {
  const char *current_line_start;
  int current_line_number;
  const char *current_char;

  std::string current_directory;
  //SubString current_filename;

  Token program;
  Error error;

  //TokenPool token_pool;
  Token token_pool[MAX_TOKENS];
  int num_tokens;

  std::vector<std::string> files;
  std::vector<char *> file_data;
  int current_file_idx;
};

void consume_whitespace(ParserState &parser) {
  enum CommentMode {
    none,
    end_of_line,
  };
  CommentMode comment_mode = none;

  while(1) {
    switch(*parser.current_char) {
    case '\t':
    case ' ':
      ++parser.current_char;
      break;

    case '\r':
      if (parser.current_char[1] == '\n') ++parser.current_char;
      ++parser.current_char;
      parser.current_line_start = parser.current_char;
      ++parser.current_line_number;
      if (comment_mode == end_of_line) comment_mode = none;
      break;
    case '\n':
      if (parser.current_char[1] == '\n') ++parser.current_char;
      ++parser.current_char;
      parser.current_line_start = parser.current_char;
      ++parser.current_line_number;
      if (comment_mode == end_of_line) comment_mode = none;
      break;

    case '/':
      if (parser.current_char[1] == '/') {
        comment_mode = end_of_line;
        parser.current_char += 2;
      }
      //else if (parser.current_char[1] == '*') {
      //  comment_mode = inline_comments;
      //}
      else {
        return;
      }
      break;
    case '\0':
    default:
      if (comment_mode == end_of_line) {
        ++parser.current_char;
        break;
      }
      return;
    }
  }
}

enum ParseResultType {
  RESULT_SUCCESS,
  RESULT_ERROR,
  RESULT_NONE,
};

struct ParseResult
{
    ParseResultType type;
    Error error;
    Token *start;
    Token *end;
};

const ParseResult PARSE_EMPTY = {};
const ParseResult PARSE_NONE = {RESULT_NONE};

ParseResult parse_exact(ParserState &parser, const char *str) {
  int length = strlen(str);
  if (strncmp(str, parser.current_char, length) != 0) {
    return PARSE_NONE;
  }

  parser.current_char += length;
  consume_whitespace(parser);
  return PARSE_EMPTY;
}

ParseResult make_error(ParserState &parser, const char *error_fmt, ...) {
  ParseResult retval;
  retval.type = RESULT_ERROR;
  return retval;
}

ParseResult parse_exact_error(ParserState &parser, const char *str, const char *error) {
  int length = strlen(str);
  if (strncmp(str, parser.current_char, length) != 0) {
    return make_error(parser, error);
  }

  parser.current_char += length;
  consume_whitespace(parser);

  return PARSE_EMPTY;
}

ParseResult parse_import_exact(ParserState &parser) {
  return parse_exact(parser, "import");
}

ParseResult parse_extern_exact(ParserState &parser) {
  return parse_exact(parser, "extern");
}

ParseResult parse_semicolon(ParserState &parser) {
  return parse_exact_error(parser, ";", "Missing semicolon.");
}

//void make_token(ParserState &parser, Token& container, TokenType token, const char *start, const char *c)
//{
//  assert(is_substring_token(token));
//  Token &token = alloc_token(parser);
//  token.type = type;
//  token.substring.start = start;
//  token.substring.length = end-start;
//}

void consume_escape_sequence(const char *&c) {
  // TODO
  ++c;
}

Token *token_alloc(ParserState &parser) {
  assert(parser.num_tokens < MAX_TOKENS);
  Token *retval = &parser.token_pool[parser.num_tokens++];
  retval->start = 0;
  retval->end = 0;
  retval->next = 0;
  return retval;
}

ParseResult make_success(Token *new_token) {
  ParseResult retval;
  retval.type = RESULT_SUCCESS;
  retval.start = new_token;
  retval.end = new_token;
  return retval;
}

ParseResult make_result(ParserState &parser, TokenType type, const char *start, const char *end) {
  Token *new_token = token_alloc(parser);
  new_token->type = type;
  new_token->substring.start = start;
  new_token->substring.length = end - start;
  return make_success(new_token);
}

bool is_error(const ParseResult& result) {
  return (result.type == RESULT_ERROR);
}

bool is_success(const ParseResult& result) {
  return (result.type == RESULT_SUCCESS);
}

bool is_none(const ParseResult& result) {
  return (result.type == RESULT_NONE);
}

ParseResult make_result(ParserState &parser, TokenType type, const ParseResult &result_subtokens) {
  if (is_success(result_subtokens) == false) return result_subtokens;
  Token *new_token = token_alloc(parser);
  new_token->type = type;
  new_token->start = result_subtokens.start;
  new_token->end = result_subtokens.end;
  return make_success(new_token);
}

ParseResult make_result(ParserState &parser, TokenType type, Token *subtoken) {
  assert(subtoken);
  Token *new_token = token_alloc(parser);
  new_token->type = type;
  new_token->start = subtoken;
  new_token->end = subtoken;
  return make_success(new_token);
}

ParseResult make_result(ParserState &parser, TokenType type, const ParseTable *entry) {
  assert(entry);
  Token *new_token = token_alloc(parser);
  new_token->type = type;
  new_token->entry = entry;
  return make_success(new_token);
}

//ParseResult make_result(ParserState &parser, TokenType type) {
//  return make_result(parser, type, NULL);
//}

ParseResult parse_string_literal(ParserState &parser) {
  if (*parser.current_char != '\"') return PARSE_NONE;
  
  const char *start = parser.current_char + 1;
  for(const char *c = start;;c++) {
    switch(*c) {
      case 0:
        return make_error(parser, "Unexpected end-of-file string");
      case '\n':
      case '\r':
        return make_error(parser, "Unexpected end-of-line in string");
      case '\\':
        consume_escape_sequence(c);
        break;
      case '"':
        parser.current_char = c+1;
        consume_whitespace(parser);
        return make_result(parser, TOKEN_STRING_LITERAL, start, c);
      default:
        break;
    }
  }
}

ParseResult parse_optional_string_literal(ParserState &parser) {
  ParseResult result = parse_string_literal(parser);
  if (is_none(result)) return PARSE_EMPTY;
  return result;
}

//ParseResult parse_1_to_3_strings(ParserState &parser, Token *&container) {
//  int i = 0;
//  for(; i < 3; ++i) {
//    ParseResult result = parse_string_literal(parser, *container++);
//    if (is_error(result)) return result;
//    else if (result == PARSE_NONE) break;
//  }
//
//  if (i == 0) {
//    make_error(parser, "Missing filename for import statement");
//    return PARSE_ERROR;
//  } 
//
//  return PARSE_SUCCESS;
//}

typedef ParseResult (*ParseFn)(ParserState&);

//ParseResult continue_parse_sequence(ParserState &parser, const ParseFn *statements, int num_statements, ParseResult &retval) {
//  for(int i = 0; i < num_statements; ++i) {
//    ParseResult result = statements[i](parser);
//    if (!is_success(result)) return result;
//    retval_append(retval, result);
//  } 
//
//  return retval;
//}

void halt();

void append_result(ParseResult &retval, ParseResult &new_result) {
  assert(retval.type == RESULT_SUCCESS);
  switch (new_result.type) {
  case RESULT_SUCCESS:
    if (new_result.start == NULL) {
      // nothing
    } else if (retval.start == NULL) {
      retval = new_result;
    } else {
      retval.end->next = new_result.start;
      retval.end = new_result.end;
    }
    break;
  case RESULT_ERROR:
    retval = new_result;
    break;
  case RESULT_NONE:
    break;
  default:
    halt();
  }
}

ParseResult parse_sequence(ParserState &parser, const ParseFn *statements, int num_statements) {
  ParseResult retval = PARSE_EMPTY;
  for(int i = 0; i < num_statements; ++i) {
    ParseResult result = statements[i](parser);
    if (is_error(result)) return result;
    else if (is_none(result)) {
      if (i != 0) return make_error(parser, "...");
      else return result;
    }
    append_result(retval, result);
  } 

  return retval;
}

#define ARRAYSIZE(ar) (sizeof((ar))/sizeof((ar)[0]))

int expand_tokens(const ParseResult &statement, Token *subtokens[], int max_subtokens) {
  int num_tokens = 0;
  for(Token *cur = statement.start->start; cur != NULL; cur = cur->next) {
    assert(num_tokens < max_subtokens);
    subtokens[num_tokens++] = cur;
  }
  return num_tokens;
}

void parser_add_file(ParserState &parser, const SubString &relative_path);

void emit_import_statement(ParserState &parser, const ParseResult &import_statement) {
  Token *subtokens[3];
  int num_subtokens = expand_tokens(import_statement, subtokens, 3);
  
  assert(num_subtokens >= 1);
  assert(num_subtokens <= 3);

  switch(num_subtokens) {
  case 1:
    parser_add_file(parser, subtokens[0]->substring);
    break;
  case 2:
  case 3:
    // TODO
  default:
    halt();
  }
}


ParseResult parse_import_statement(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_import_exact,
    parse_string_literal,
    parse_optional_string_literal,
    parse_optional_string_literal,
    parse_semicolon,
  };
  static const int num_statements = ARRAYSIZE(statements);

  ParseResult result = parse_sequence(parser, statements, num_statements);
  ParseResult retval = make_result(parser, TOKEN_IMPORT_STATEMENT, result);
  if (is_success(retval)) {
    emit_import_statement(parser, retval);
  }

  return retval;
}

ParseResult parse_pointer_type(ParserState &parser, Token *base_type) {
  ParseResult result = parse_exact(parser, "*");
  if (is_success(result) == false) return result;
  return make_result(parser, TOKEN_POINTER_TYPE, base_type);
}

typedef bool (*CharPredicate)(char c0);
ParseResult parse_one_or_more_pred(ParserState &parser, CharPredicate pred, TokenType tokenType) {
  const char *data = parser.current_char;
  while(data) {
    if (pred(*data) == false) break;
    data++;
  }

  if (data == parser.current_char) {
    return PARSE_NONE;
  }

  ParseResult result = make_result(parser, tokenType, parser.current_char, data);
  parser.current_char = data;
  return result;
}

static bool isDigit(char c) {
  return isdigit(c) != 0;
}

ParseResult parse_integer_literal(ParserState &parser) {
  ParseResult result = parse_one_or_more_pred(parser, isDigit, TOKEN_INTEGER_LITERAL);
  if (is_success(result) == false) return result;
  consume_whitespace(parser);
  return result;
};

ParseResult parse_constexpr_integer(ParserState &parser) {
  // TODO;
  return parse_integer_literal(parser);
}

ParseResult parse_array_type(ParserState &parser, Token *base_type) {
  ParseResult result = parse_exact(parser, "[");
  if (is_success(result) == false) return result;

  ParseResult size = parse_constexpr_integer(parser);
  if (is_success(result) == false) {
    return make_error(parser, "Invalid size for array type (atm these have to be integer literals)");
  }
  result = parse_exact(parser, "]");
  if (is_success(result) == false) {
    return make_error(parser, "Array type missing closing brace");
  }

  ParseResult retval = make_result(parser, TOKEN_ARRAY_TYPE, base_type);
  append_result(retval, size);
  return retval;
}

static bool ischar(char c0, char c1)
{
  return c0 == c1;
};

ParseResult parse_identifier(ParserState &parser) {
  const char* data = parser.current_char;

  if (isalpha(data[0]) || ischar('_', data[0])) {
    data++;
  } else {
    return PARSE_NONE;
  }

  while(isalpha(data[0]) || ischar('_', data[0]) || isdigit(data[0])) {
    data++;
  }

  ParseResult result = make_result(parser, TOKEN_IDENTIFIER, parser.current_char, data);
  parser.current_char = data;
  consume_whitespace(parser);
  return result;
}

ParseResult parse_type_identifier(ParserState &parser) {
  return parse_identifier(parser);
}

ParseResult parse_first_of_deep(ParserState &parser, const ParseFn *fns, int num_fns) {
  for(int i = 0; i < num_fns; ++i) {
    const char *current_char = parser.current_char;
    int current_line_number = parser.current_line_number;
    const char *current_line_start = parser.current_line_start;
    ParseResult result = fns[i](parser);
    if (is_success(result)) return result;
    parser.current_char = current_char;
    parser.current_line_number = current_line_number;
    parser.current_line_start = current_line_start;
  }

  return PARSE_NONE;
}

ParseResult parse_first_of(ParserState &parser, const ParseFn *fns, int num_fns) {
  for(int i = 0; i < num_fns; ++i) {
    ParseResult result = fns[i](parser);
    if (is_error(result)) return result;
    else if (is_success(result)) return result;
  }

  return PARSE_NONE;
}

ParseResult parse_function_type(ParserState &parser, Token *retval_type);

ParseResult parse_type_declaration(ParserState &parser) {
  const static ParseFn fns[] = { 
    //parse_anonymous_struct, 
    //parse_anonymous_enum, 
    //parse_anonymous_llvm_type, 
    parse_type_identifier 
  };
  const static int num_fns = ARRAYSIZE(fns);

  ParseResult base = parse_first_of(parser, fns, num_fns);
  if (is_success(base) == false) {
    return base;
  }
  assert(base.start == base.end && base.start != NULL);

  while(1) {
    //const static ParseFn fns[] = { 
    //  parse_pointer_type,
    //  parse_array_type,
    //  parse_function_type,
    //};
    //const static int num_fns = ARRAYSIZE(fns);
    //ParseResult result = parse_first_of(parser, fns, num_fns);

    ParseResult result;
    result = parse_pointer_type(parser, base.start);
    if (!is_success(result)) {
      result = parse_array_type(parser, base.start);
      if (!is_success(result)) {
        result = parse_function_type(parser, base.start);
      }
    }

    if (is_success(result)) {
      assert(result.start == result.end && result.start != NULL);
      base = result;
      continue;
    } else if (is_none(result)) return base;
    else if (is_error(result)) return result;
    else halt();
  }
}

ParseResult parse_zero_or_more_separated(ParserState &parser, ParseFn fn, const char *separator) {
  ParseResult retval = PARSE_EMPTY;

  while(1) {
    ParseResult result = fn(parser);
    if (is_success(result) == false) break;
    append_result(retval, result);

    result = parse_exact(parser, separator);
    if (is_success(result) == false) break;
  }

  return retval;
}

ParseResult parse_function_type_params(ParserState &parser) {
  return parse_zero_or_more_separated(parser, parse_type_declaration, ",");
}

ParseResult parse_function_type(ParserState &parser, Token *retval_type) {
  {
    ParseResult result = parse_exact(parser, "(");
    if (is_success(result) == false) return result;
  }

  ParseResult params = parse_function_type_params(parser);
  if (is_success(params) == false) return params;

  {
    ParseResult result = parse_exact(parser, ")");
    if (is_success(result) == false) {
      return make_error(parser, "Function type missing closing paren");
    }
  }

  ParseResult retval = make_result(parser, TOKEN_FUNCTION_TYPE, retval_type);
  append_result(retval, params);
  return retval;
}

ParseResult parse_operator_exact(ParserState &parser) {
  return parse_exact(parser, "operator");
}

ParseResult parse_binary_operator(ParserState &parser);
ParseResult parse_operator_identifier(ParserState &parser) {
  static const ParseFn fns[] = { 
    parse_operator_exact,
    parse_binary_operator,
  };
  static const int num = ARRAYSIZE(fns);
  ParseResult retval = parse_sequence(parser, fns, num);
  return retval;
}

ParseResult parse_function_identifier(ParserState &parser) {
  static const ParseFn fns[] = { 
    parse_operator_identifier, 
    parse_identifier, 
  };
  static const int num = ARRAYSIZE(fns);
  return parse_first_of(parser, fns, num);
}

ParseResult parse_left_paren(ParserState &parser) {
  return parse_exact(parser, "(");
}

ParseResult parse_right_paren(ParserState &parser) {
  return parse_exact(parser, ")");
}

ParseResult parse_expression(ParserState &parser);
ParseResult parse_constexpr(ParserState &parser) {
  // TODO
  return parse_expression(parser);
}
ParseResult parse_function_param(ParserState &parser) {
  const static ParseFn fns[] = { 
    parse_type_declaration,
    parse_type_identifier,
  };
  const static int num_fns = ARRAYSIZE(fns);

  //Token *set = token_alloc_tuple(parser, container, TOKEN_FUNCTION_PARAM, 3);
  ParseResult result = parse_sequence(parser, fns, num_fns);
  if (is_success(result) == false) return result;

  if (is_success(parse_exact(parser, "="))) {
    ParseResult expr = parse_constexpr(parser);
    append_result(result, expr);
  }

  return make_result(parser, TOKEN_FUNCTION_PARAM, result);
}

ParseResult parse_post_inc_op(ParserState &parser, Token *lhs) {
  ParseResult result = parse_exact(parser, "++");
  if (is_success(result) == false) return result;
  return make_result(parser, TOKEN_OP_POSTFIX_INC, lhs);
}

ParseResult parse_post_dec_op(ParserState &parser, Token *lhs) {
  ParseResult result = parse_exact(parser, "--");
  if (is_success(result) == false) return result;
  return make_result(parser, TOKEN_OP_POSTFIX_DEC, lhs);
}

ParseResult parse_member_identifier(ParserState &parser) {
  return parse_identifier(parser);
}

ParseResult parse_member_op(ParserState &parser, Token *lhs) {
  ParseResult result = parse_exact(parser, ".");
  if (is_success(result) == false) return result;

  ParseResult member = parse_member_identifier(parser);
  if (is_success(member) == false) {
    return make_error(parser, "Invalid member identifier");
  }

  return make_result(parser, TOKEN_OP_MEMBER, member);
}

ParseResult parse_function_call_params(ParserState &parser) {
  return parse_zero_or_more_separated(parser, parse_expression, ",");
}

ParseResult parse_function_call_op(ParserState &parser, Token *lhs) {
  {
    ParseResult result = parse_exact(parser, "(");
    if (is_success(result) == false) return result;
  }

  ParseResult params = parse_function_call_params(parser);
  if (is_success(params) == false) {
    return make_error(parser, "Invalid function call params");
  }

  {
    ParseResult result = parse_exact(parser, ")");
    if (is_success(result) == false)  {
      make_error(parser, "Missing left paren ')' for function call");
      return result;
    }
  }

  ParseResult retval = make_result(parser, TOKEN_OP_FUNCTION_CALL, lhs);
  append_result(retval, params);
  return retval;
}

ParseResult parse_array_index_op(ParserState &parser, Token *lhs) {
  {
    ParseResult result = parse_exact(parser, "[");
    if (is_success(result) == false) return result;
  }

  ParseResult array_index = parse_expression(parser);
  if (is_success(array_index) == false) {
    return make_error(parser, "Invalid function call params");
  }

  {
    ParseResult result = parse_exact(parser, "]");
    if (is_success(result) == false)  {
      make_error(parser, "Missing left bracket ']' for function call");
      return result;
    }
  }

  ParseResult retval = make_result(parser, TOKEN_OP_ARRAY_INDEX, lhs);
  append_result(retval, array_index);
  return retval;

  //if (parse_exact(parser "[") == false) return false;
  //make_token_containter(TOKEN_OP_FUNCTION_CALL, 1);

  //if (parse_expression(parser) == false) {
  //  //make_error("Invalid function params");
  //  return false;
  //}

  //if (parse_exact(parser "]") == false) {
  //  make_error("Missing left bracket ']' for array index");
  //  return false;
  //}

  //return true;
}

//ParseResult parse_postfix_op(Parser &parser) {
//  ParseFn statemtents = {
//    parse_post_inc_op
//    parse_post_dec_op
//    parse_member_op
//    parse_function_call_op,
//    parse_array_index_op,
//  };
//
//  parse_first_of(parser, statemtents);
//}

ParseResult parse_numeric_literal(ParserState &parser) {
  ParseResult retval = parse_integer_literal(parser);
  if (is_success(retval) == false) return retval;

  if (is_success(parse_exact(parser, ".")) == false) {
    return retval;
  }

  assert(retval.start == retval.end && retval.start != NULL);
  retval.start->type = TOKEN_FLOAT_LITERAL;

  ParseResult retval2 = parse_integer_literal(parser);
  if (is_success(retval2) == false) {
    return retval;
  }

  assert(retval2.start == retval2.end && retval2.start != NULL);

  retval.start->substring.length = retval2.start->substring.start + retval2.start->substring.length - retval.start->substring.start;

  if (is_success(parse_exact(parser, "e")) == false) {
    return retval;
  }

  // TODO -exponents
  //parse_exact(parser, "-");

  ParseResult retval3 = parse_integer_literal(parser);
  if (is_success(retval3) == false) {
    return retval;
  }

  assert(retval3.start == retval3.end && retval3.start != NULL);
  retval.start->substring.length = retval3.start->substring.start + retval3.start->substring.length - retval.start->substring.start;

  return retval;

  //static const ParseFn fns[] = {
  //  parse_integer_literal, 
  //  //parse_float_literal,
  //};
  //static const int num_fns = ARRAYSIZE(fns);
  //return parse_first_of(parser, fns, num_fns);
}

ParseResult parse_literal(ParserState &parser) {
  static const ParseFn fns[] = {
    parse_numeric_literal, 
    parse_string_literal,
  };
  static const int num_fns = ARRAYSIZE(fns);
  return parse_first_of(parser, fns, num_fns);
}

ParseResult parse_variable_identifier(ParserState &parser) {
  return parse_identifier(parser);
}

ParseResult parse_paren_expression(ParserState &parser) {
  static const ParseFn fns[] = {
    parse_left_paren, 
    parse_string_literal,
    parse_right_paren,
  };
  static const int num_fns = ARRAYSIZE(fns);
  return parse_sequence(parser, fns, num_fns);
}


ParseResult parse_atomic_expression(ParserState &parser) {
  static const ParseFn fns[] = {
    parse_paren_expression,
    parse_literal, 
    parse_variable_identifier,
  };
  static const int num_fns = ARRAYSIZE(fns);
  return parse_first_of(parser, fns, num_fns);
}

ParseResult parse_postfix_expression(ParserState &parser) {
  ParseResult base = parse_atomic_expression(parser);
  if (is_success(base) == false) return base;

  while(1) {
    ParseResult result;
    result = parse_post_inc_op(parser, base.start);
    if (!is_success(result)) {
      result = parse_post_dec_op(parser, base.start);
      if (!is_success(result)) {
        result = parse_member_op(parser, base.start);
        if (!is_success(result)) {
          result = parse_function_call_op(parser, base.start);
          if (!is_success(result)) {
            result = parse_array_index_op(parser, base.start);
          }
        }
      }
    }
  
    if (is_success(result)) {
      //assert(result.start == result.end && result.start != NULL);
      base = result;
      continue;
    } else if (is_none(result)) return base;
    else if (is_error(result)) return result;
    else halt();
  }

  //parse_zero_or_more(parse_postfix_op);
  //return true;
}

struct ParseTable {
  const char *str;
  TokenType type;
  int precedence;
};

 ParseResult parse_first_table(ParserState &parser, const ParseTable *table, int num_entries) {
   for(int i = 0; i < num_entries; ++i) {
     const ParseTable &entry = table[i];
     ParseResult result = parse_exact(parser, entry.str);
     if (is_success(result)) {
       return make_result(parser, entry.type, &entry);
     } else if (is_error(result)) {
       return result;
     } 
   }

   return PARSE_NONE;
 }

ParseResult parse_assignment_operator(ParserState &parser) {
  static const ParseTable table[] = {
    "=",  TOKEN_ASSIGNMENT_OP,    16,
    "+=", TOKEN_ADD_ASSIGNMENT_OP,16,
    "-=", TOKEN_SUB_ASSIGNMENT_OP,16,
    "*=", TOKEN_MUL_ASSIGNMENT_OP,16,
    "/=", TOKEN_DIV_ASSIGNMENT_OP,16,
    "%=", TOKEN_REM_ASSIGNMENT_OP,16,
    "&=", TOKEN_AND_ASSIGNMENT_OP,16,
    "|=", TOKEN_OR_ASSIGNMENT_OP, 16,
    "^=", TOKEN_XOR_ASSIGNMENT_OP,16,
    "~=", TOKEN_NOT_ASSIGNMENT_OP,16,
  };
  static const int num_entries = ARRAYSIZE(table);

  return parse_first_table(parser, table, num_entries);
}

ParseResult parse_unary_operator(ParserState &parser) {
  static const ParseTable table[] = {
    "++", TOKEN_PRE_INC_OP,     3,
    "--", TOKEN_PRE_DEC_OP,     3,
    "+",  TOKEN_POSITIVE_OP,    3,
    "-",  TOKEN_NEGATIVE_OP,    3,
    "&",  TOKEN_ADDRESS_OP,     3,
    "*",  TOKEN_DEREF_OP,       3,
    "!",  TOKEN_LOGICAL_NOT_OP, 3,
    "~",  TOKEN_BITWISE_NOT_OP, 3,
  };
  static const int num_entries = ARRAYSIZE(table);

  return parse_first_table(parser, table, num_entries);
}

ParseResult parse_binary_operator(ParserState &parser) {
  static const ParseTable table[] = {
    "*",  TOKEN_MUL_OP, 5,
    "/",  TOKEN_DIV_OP, 5,
    "%",  TOKEN_REM_OP, 5,

    "+",  TOKEN_ADD_OP, 6,
    "-",  TOKEN_SUB_OP, 6,

    "<<", TOKEN_LSHIFT_OP,  7,
    ">>", TOKEN_RSHIFT_OP,  7,

    "<=", TOKEN_CMP_LTE_OP, 8,
    ">=", TOKEN_CMP_GTE_OP, 8,
    "<",  TOKEN_CMP_LT_OP,  8,
    ">",  TOKEN_CMP_GT_OP,  8,

    "==", TOKEN_CMP_EQ_OP,  9,
    "!=", TOKEN_CMP_NEQ_OP, 9,

    "&",  TOKEN_BITWISE_AND_OP, 10,
    "|",  TOKEN_BITWISE_OR_OP,  11,
    "^",  TOKEN_BITWISE_XOR_OP, 12,
    "&&", TOKEN_LOGICAL_AND_OP, 13,
    "||", TOKEN_LOGICAL_OR_OP,  14,
  };
  static const int num_entries = ARRAYSIZE(table);
  return parse_first_table(parser, table, num_entries);
}

Token *get_token(ParseResult &result) {
  assert(result.type == RESULT_SUCCESS);
  assert(result.start == result.end && result.start != NULL);
  return result.start;
}


int get_precedence(ParseResult &result) {
  const Token *token = get_token(result);
  return token->entry->precedence;
}

ParseResult parse_unary_expression(ParserState &parser) {
  ParseResult op = parse_unary_operator(parser);
  if (is_error(op)) return op;
  if (is_none(op)) {
    return parse_postfix_expression(parser);
  }

  ParseResult base = parse_unary_expression(parser);
  if (!is_success(base)) {
    return make_error(parser, "Invalid subexpression in unary expression ");
  }

  ParseResult retval = make_result(parser, TOKEN_UNARY_EXPRESSION, op);
  append_result(retval, base);
  return retval;
}


ParseResult peek(ParserState &parser, ParseFn fn) {
  const char *current_char = parser.current_char;
  int current_line_number = parser.current_line_number;
  const char *current_line_start = parser.current_line_start;
  ParseResult result = fn(parser);
  parser.current_char = current_char;
  parser.current_line_number = current_line_number;
  parser.current_line_start = current_line_start;
  return result;
}

ParseResult parse_binary_expression(ParserState &parser, int minimum_precedence, const ParseResult &arg_lhs) {
  ParseResult lhs = arg_lhs;

  while(1) {
    ParseResult op = parse_binary_operator(parser);
    if (is_error(op)) return op;
    if (is_none(op)) {
      return lhs;
    }

    int precedence = get_precedence(op);
    if (precedence < minimum_precedence) {
      return lhs;
    }

    ParseResult rhs = parse_unary_expression(parser);
    assert(rhs.type >= 0 && rhs.type <= 3);
    if (is_success(rhs) == false) {
      return make_error(parser, "Invalid right hand side in binary expression ");
    }

    ParseResult next_op = peek(parser, parse_binary_operator);
    if (is_success(next_op)) {
      int next_precedence = get_precedence(next_op);
      if (precedence < next_precedence) {
        rhs = parse_binary_expression(parser, precedence+1, rhs);
        if (is_success(rhs) == false) {
          return make_error(parser, "Invalid right hand side in binary expression ");
        }
      }
    }

    ParseResult new_lhs = make_result(parser, TOKEN_BINARY_EXPRESSION, lhs);
    append_result(new_lhs, op);
    append_result(new_lhs, rhs);
    lhs = new_lhs;
  }
}

//ParseResult parse_binary_expression(Parser &parser) {
//  parse_binary_expression(Parser &parser, 0, ) {
//}

ParseResult parse_expression(ParserState &parser) {
  ParseResult lhs = parse_unary_expression(parser);
  if (is_success(lhs) == false) return lhs;
  return parse_binary_expression(parser, 0, lhs);
}

/////////////////////////////////
ParseResult parse_compile_time_expression(ParserState &parser) {
  // TODO
  return parse_expression(parser);
}

ParseResult parse_zero_or_more(ParserState &parser, ParseFn fn) {
  ParseResult retval = PARSE_EMPTY;

  while(1) {
    ParseResult results = fn(parser);
    if (is_error(results)) return results;
    if (is_none(results)) return retval;
    append_result(retval, results);
  }
}

ParseResult parse_function_params(ParserState &parser) {
  ParseResult result = parse_zero_or_more_separated(parser, parse_function_param, ",");
  return make_result(parser, TOKEN_FUNCTION_PARAMS, result);
}

ParseResult parse_extern_function_declaration(ParserState &parser) {
  ParseFn statments[] = {
    parse_extern_exact,
    parse_type_declaration,
    parse_function_identifier,
    parse_left_paren,
    parse_function_type_params,
    parse_right_paren,
    parse_semicolon,
  };
  static const int num_statements = ARRAYSIZE(statments);

  ParseResult result = parse_sequence(parser, statments, num_statements);
  return make_result(parser, TOKEN_EXTERNAL_FUNCTION_DECLARATION, result);
}

ParseResult parse_typedef_exact(ParserState &parser) {
  return parse_exact(parser, "typedef");
}

ParseResult parse_declaration_assignment_operator(ParserState &parser) {
  return parse_exact(parser, "=");
}

ParseResult parse_typedef_definition(ParserState &parser) {
  static const ParseFn statments[] = {
    parse_typedef_exact, 
    parse_type_identifier, 
    parse_declaration_assignment_operator, 
    parse_type_declaration
  };
  static const int num_statements = ARRAYSIZE(statments);

  ParseResult result = parse_sequence(parser, statments, num_statements);
  return make_result(parser, TOKEN_TYPEDEF_DEFINITION, result);
}

ParseResult parse_struct_exact(ParserState &parser) {
  return parse_exact(parser, "struct");
}

ParseResult parse_left_brace(ParserState &parser) {
  return parse_exact(parser, "{");
}

ParseResult parse_right_brace(ParserState &parser) {
  return parse_exact(parser, "}");
}

ParseResult parse_variable_definition_part_2(ParserState &parser, ParseResult &partial_results) {
  if (is_success(parse_declaration_assignment_operator(parser))) {
    ParseResult expr = parse_expression(parser);
    if (is_success(expr) == false) {
      return make_error(parser, "Invalid expression for variable initilizer");
    }
    append_result(partial_results, expr);
  } 

  if (is_success(parse_semicolon(parser)) == false) {
      return make_error(parser, "Missing semiclon");
  }

  ParseResult retval = make_result(parser, TOKEN_VARIABLE_DEFINITION, partial_results);
  return retval;
}

ParseResult parse_variable_definition(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_type_declaration, 
    parse_type_identifier, 
  };
  static const int num_statements = ARRAYSIZE(statements);
  ParseResult result = parse_sequence(parser, statements, num_statements);
  if (is_success(result) == false) return result;

  return parse_variable_definition_part_2(parser, result);
}

ParseResult parse_struct_field(ParserState &parser) {
  return parse_variable_definition(parser);
}

ParseResult parse_struct_fields(ParserState &parser) {
  ParseResult result = parse_zero_or_more(parser, parse_struct_field);
  return make_result(parser, TOKEN_STRUCT_FIELDS, result);
}

ParseResult parse_struct_definition(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_struct_exact, 
    parse_type_identifier, 
    parse_left_brace, 
    parse_struct_fields,
    parse_right_brace, 
  };
  static const int num_statements = ARRAYSIZE(statements);

  ParseResult result = parse_sequence(parser, statements, num_statements);
  return make_result(parser, TOKEN_STRUCT_DEFINITION, result);
}

ParseResult parse_llvm_type_exact(ParserState &parser) {
  return parse_exact(parser, "llvm_type");
}

ParseResult parse_llvm_exact(ParserState &parser) {
  return parse_exact(parser, "llvm");
}

ParseResult parse_if_exact(ParserState &parser) {
  return parse_exact(parser, "if");
}

ParseResult parse_return_exact(ParserState &parser) {
  return parse_exact(parser, "return");
}

ParseResult parse_until(ParserState &parser, const char *str, TokenType type) {
  const char *start = parser.current_char;
  while(1) {
    if (*parser.current_char == 0) {
      return make_error(parser, "Unexpected end of file");
    }

    if (strcmp(str, parser.current_char) == 0) {
      ParseResult retval = make_result(parser, type, start, parser.current_char);
      parser.current_char += strlen(str);
      consume_whitespace(parser);
      return retval;
    }

    parser.current_char++;
  }
}

ParseResult parse_raw_llvm_type(ParserState &parser) {
  return parse_until(parser, "}", TOKEN_RAW_LLVM_TYPE);
  //make_error("Unterminated LLVM type declaration");
}

ParseResult parse_raw_llvm(ParserState &parser) {
  return parse_until(parser, "}", TOKEN_INLINE_LLVM);
  //make_error("Unterminated LLVM statement block");
}

#define ARRAYSIZE(ar) (sizeof((ar))/sizeof((ar)[0]))


ParseResult parse_llvm_type_definition(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_llvm_type_exact, 
    parse_type_identifier, 
    parse_left_brace, 
    parse_raw_llvm_type,
    parse_right_brace, 
  };
  static const int num_statements = ARRAYSIZE(statements);

  ParseResult result = parse_sequence(parser, statements, num_statements);
  return make_result(parser, TOKEN_LLVM_TYPE_DEFINITION, result);
}

ParseResult parse_type_definition(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_typedef_definition,
    parse_struct_definition,
    //parse_enum_definition, TODO
    parse_llvm_type_definition,
  };
  static const int num_statements = ARRAYSIZE(statements);

  return parse_first_of(parser, statements, num_statements);
}

ParseResult parse_function_body(ParserState &parser);

ParseResult parse_block_statement(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_left_brace,
    parse_function_body,
    parse_right_brace,
  };
  static const int num_statements = ARRAYSIZE(statements);
  ParseResult result = parse_sequence(parser, statements, num_statements);
  if (is_success(result)) {
    get_token(result)->type = TOKEN_BLOCK_STATEMENT;
  }
  return result;

  //return make_result(parser, TOKEN_BLOCK_STATEMENT, result);
}

ParseResult parse_return_statement(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_return_exact,
    parse_expression,
    parse_semicolon,
  };
  static const int num_statements = ARRAYSIZE(statements);

  ParseResult result = parse_sequence(parser, statements, num_statements);
  return make_result(parser, TOKEN_RETURN_STATEMENT, result);
}

ParseResult parse_expression_statement(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_expression,
    parse_semicolon,
  };
  static const int num_statements = ARRAYSIZE(statements);

  ParseResult result = parse_sequence(parser, statements, num_statements);
  return make_result(parser, TOKEN_EXPRESSION_STATEMENT, result);
}

ParseResult parse_inline_llvm(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_llvm_exact,
    parse_left_brace,
    parse_raw_llvm,
    parse_right_brace,
  };
  static const int num_statements = ARRAYSIZE(statements);

  ParseResult result = parse_sequence(parser, statements, num_statements);
  return make_result(parser, TOKEN_INLINE_LLVM, result);
}

ParseResult parse_statement(ParserState &parser);

ParseResult parse_if_statement (ParserState &parser) {
  static const ParseFn statements[] = {
    parse_if_exact,
    parse_left_paren,
    parse_expression,
    parse_right_paren,
    parse_statement,
  };
  static const int num_statements = ARRAYSIZE(statements);

  ParseResult result = parse_sequence(parser, statements, num_statements);
  if (is_success(result) == false) return result;

  if (is_success(parse_exact(parser, "else"))) {
    ParseResult else_statement = parse_statement(parser);
    if (is_error(result)) return result;
    else if (is_none(result)) {
      return make_error(parser, "Missing statement for else clause");
    }
    append_result(result, else_statement);
  }

  return make_result(parser, TOKEN_IF_STATEMENT, result);
}

ParseResult parse_assignment_statement(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_expression,
    parse_assignment_operator,
    parse_expression,
    parse_semicolon,
  };
  static const int num_statements = ARRAYSIZE(statements);

  ParseResult result = parse_sequence(parser, statements, num_statements);
  return make_result(parser, TOKEN_ASSIGMENT_STATEMENT, result);
}

ParseResult parse_function_definition(ParserState &parser);
//ParseResult parse_definition(ParserState &parser);

//ParseResult parse_annoying_statement(ParserState &parser) {
//  identifier = parse_identifier;
//  if finish_type_declaration {
//    finish_definition();      // function def or variable def
//  } else {
//    finish_exspression
//    if (assigment operator) {
//      finish_assignment
//    }
//  }
//
//  identifer -> typejunk -> identifer -> ( -> ...  // function declaration
//  identifer -> typejunk -> identifer -> ...       // variable declaration
//  identifer -> identifer -> (                     // function declaration
//  identifer -> identifer -> ...                   // variable declaration
//  identifer -> op junk ... -> ;                   // expression statement
//  identifer -> op junk ... -> *= ... ;            // assignment statement
//  op junk -> identifer ... ;                      // expression statement
//  op junk -> identifer ... *= ;                   // assignment statement
//
//
//  foo[32] identifier()
//}

ParseResult parse_statement(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_block_statement,
    parse_return_statement,
    //parse_for_statement,
    parse_if_statement,
    // parse_switch_statement,
    parse_inline_llvm,

    parse_expression_statement,
    parse_assignment_statement,

    parse_type_definition,
    //parse_definition,
    parse_variable_definition,
    parse_function_definition,
  };
  static const int num_statements = ARRAYSIZE(statements);
  return parse_first_of_deep(parser, statements, num_statements);
}

ParseResult parse_function_body(ParserState &parser) {
  ParseResult result = parse_zero_or_more(parser, parse_statement);
  return make_result(parser, TOKEN_FUNCTION_BODY, result);
}

ParseResult parse_function_definition(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_type_declaration, 
    parse_function_identifier, 
    parse_left_paren, 
    parse_function_params, 
    parse_right_paren, 
    parse_left_brace, 
    parse_function_body,
    parse_right_brace, 
  };
  static const int num_statements = ARRAYSIZE(statements);
  ParseResult result = parse_sequence(parser, statements, num_statements);
  return make_result(parser, TOKEN_FUNCTION_DEFINITION, result);
}

// either variable or function definition
ParseResult parse_definition(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_type_declaration,
    parse_identifier, 
  };
  static const int num_statements = ARRAYSIZE(statements);

  ParseResult partial_results = parse_sequence(parser, statements, num_statements);
  if (is_success(partial_results) == false) return partial_results;

  ParseResult paren = parse_left_paren(parser);
  if (is_success(paren)) {
    // function definition
    static const ParseFn statements[] = {
      parse_function_params, 
      parse_right_paren, 
      parse_left_brace, 
      parse_function_body,
      parse_right_brace, 
    };
    static const int num_statements = ARRAYSIZE(statements);
    ParseResult result = parse_sequence(parser, statements, num_statements);
    ParseResult retval = make_result(parser, TOKEN_FUNCTION_DEFINITION, partial_results);
    append_result(retval, result);
    return retval;
  } else {
    //variable definition
    return parse_variable_definition_part_2(parser, partial_results);
  }
}

ParseResult parse_program_statements(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_import_statement, 
    parse_extern_function_declaration, 
    parse_type_definition, 
    parse_variable_definition, 
    parse_function_definition, 
    //parse_definition,
  };
  static const int num_statements = ARRAYSIZE(statements);
  return parse_first_of_deep(parser, statements, num_statements);
}

void parser_init(ParserState &parser) {
  //parser.current_directory.length = 0;
  parser.num_tokens = 0;
}

std::string make_full_path(const std::string &directory, const SubString &relative_path) {
  std::string retval = directory;
  //if (directory.length > 0) {
  //  retval.append(directory.start, directory.length);
  //}
  if (relative_path.length > 0) {
    retval.append(relative_path.start, relative_path.length);
  }
  return retval;
}

bool parser_lookup_file(ParserState &parser, const std::string &path) {
  for(auto file : parser.files) {
    if (file == path) return true;
  }

  return false;
}

void error(const char *format, ...);

char *file_read_all(const char *input_filename) {
  FILE *input_file = fopen(input_filename, "rb");
  if (input_file == NULL) {
    error("cannot open %s", input_filename);
  }

  fseek(input_file, 0, SEEK_END);
  int file_size  = ftell (input_file);
  rewind(input_file);

  char *file_data = (char *)malloc(file_size + 1);
  int read = fread(file_data, 1, file_size, input_file);
  file_data[file_size] = 0;

  if (read != file_size) {
    error("cannon read %s", input_filename);
  }

  return file_data;
}

void parser_add_file(ParserState &parser, const SubString &relative_path) {
  std::string path = make_full_path(parser.current_directory, relative_path);
  if (parser_lookup_file(parser, path) == false) {
    char *data = file_read_all(path.c_str());
    assert(data);
    parser.files.push_back(path);
    parser.file_data.push_back(data);
  }
}

void path_split_directory_filename(const char *path, std::string &directory, SubString &filename) {
  int length = strlen(path);
  for(int i = length - 1; i >= 0; --i) {
    if (path[i] == '/') {
      directory = "";
      directory.append(path, i+1);
      //directory.start = path;
      //directory.length = i+1;
      filename.start = path + i + 1;
      filename.length = length - i - 1;
      return;
    }
  }

  directory = "";
  //directory.start = 0;
  //directory.length = 0;
  filename.start = path;
  filename.length = length;
}

void parser_set_file_index(ParserState &parser, int i) {
  SubString filename;
  path_split_directory_filename(parser.files[i].c_str(), parser.current_directory, filename);
  parser.current_file_idx = i;

  parser.current_char = parser.file_data[i];
  parser.current_line_start = parser.current_char;
  parser.current_line_number = 0;
}

SubString make_substring(const char *str) {
  SubString retval;
  retval.start = str;
  retval.length = strlen(str);
  return retval;
}

ParseResult parse_program(const char *source_file) {
  ParserState parser;
  parser_init(parser);
  parser_add_file(parser, make_substring(source_file));

  ParseResult retval = PARSE_EMPTY;
  for(int i = 0; i < (int)parser.files.size(); ++i) {
    parser_set_file_index(parser, i);
    consume_whitespace(parser);
    ParseResult result = parse_zero_or_more(parser, parse_program_statements);
    if (is_error(result)) return result;
    append_result(retval, result);
  }

  ParseResult program = make_result(parser, TOKEN_PROGRAM, retval);
  return program;
}

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////

enum TypeKind {
  //BASE_TYPE,
  LLVM_TYPE,
  TYPEDEF_TYPE,
  STRUCT_TYPE,
  ARRAY_TYPE,
  POINTER_TYPE,
  FUNCTION_TYPE,
};

//struct TypeList;
struct Type;

//struct Field
//{
//  int index;
//  Type *type;
//
//  SubString identifier;
//};

struct Type
{
  llvm::Type *llvm_type;
  TypeKind kind;

  union {
    struct { // llvm_type
      SubString identifier;
    };
    struct { // array type
      Type *base;
      int64_t count;
    };
    struct { // function type
      Type *retval;
      Token *param_list;
    };

    struct { // struct type
      //std::vector<Field> fields;
      Token *fields;
    };
  };

};

//struct TypeList
//{
//  Type *type;
//  TypeList *next;
//};

struct Function
{
  //Type *retval_type;
  //TypeList *param_types;
  Type *type;

  TokenType op;
  SubString identifier;
};

struct Value
{
  Type *type;
};

struct SymbolDatabase {
  //std::map<SubString, Type> types;
  //std::map<SubString, Value> variables;
  //std::multimap<SubString, Function> functions;
  std::vector<Type *> types;
  std::vector<Value *> variables;
  std::vector<Function *> functions;
} db;

Type *db_add_pointer_type(Type *subtype) {
  Type *new_type = new Type;
  new_type->kind = POINTER_TYPE;
  new_type->base = subtype;
  db.types.push_back(new_type);
  return new_type;
}

Type *db_add_array_type(Type *subtype, int64_t count) {
  Type *new_type = new Type;
  new_type->kind = ARRAY_TYPE;
  new_type->base = subtype;
  new_type->count = count;
  db.types.push_back(new_type);
  return new_type;
}

//void copy_param_list(TypeList *&dest, TypeList *source) {
//}

bool compare_type_list(Token *dest, Token *source) {
  while(1) {
    if (dest == NULL && source == NULL) return true;
    if (dest == NULL || source == NULL) return false;
    if (dest->expr_type != source->expr_type) return false;
    dest = dest->next;
    source = source->next;
  }
}


bool expand_typelist(Token *param_list, Type **param_types, int expected_params) {
  int actual_params = 0;
  for(Token *current = param_list; current != NULL; current = current->next) {
    if (actual_params < expected_params) {
      assert(current->expr_type);
      param_types[actual_params] = current->expr_type;
    }
    ++actual_params;
  }

  return (actual_params == expected_params);
}

bool compare_function_params(Function *function, Type *lhs, Type *rhs) {
  Type *param_types[2];
  bool correct_params = expand_typelist(function->type->param_list, param_types, 2);
  if (correct_params) return false;
  if (lhs != param_types[0]) return false;
  if (rhs != param_types[1]) return false;
  return true;
}

bool compare_function_params(Function *function, Type *lhs) {
  Type *param_types[1];
  bool correct_params = expand_typelist(function->type->param_list, param_types, 1);
  if (correct_params) return false;
  if (lhs != param_types[0]) return false;
  return true;
}

bool substring_cmp(const SubString &substring0, const SubString &substring1);

Type *db_lookup_type_by_identifier(const SubString &identifier) {
  //foreach(type in db) {
  for(auto *type : db.types) {
    if (type->kind == LLVM_TYPE ||
      type->kind == TYPEDEF_TYPE ||
      type->kind == STRUCT_TYPE ||
      type->kind == LLVM_TYPE) {
        if (substring_cmp(identifier, type->identifier)) {
          return type;
        }
    }
  }

  return NULL;
}

Type *db_add_struct_type(const SubString &identifier, Token *fields) {
  if (db_lookup_type_by_identifier(identifier) != NULL) return NULL;

  Type *new_type = new Type;
  new_type->kind = STRUCT_TYPE;
  new_type->identifier = identifier;
  new_type->fields = fields;
  db.types.push_back(new_type);
  return new_type;
}

Type *db_add_function_type(Type *retval_type, Token *param_list) {
  Type *new_type = new Type;
  new_type->kind = FUNCTION_TYPE;
  new_type->retval = retval_type;
  new_type->param_list = param_list;
  //copy_param_list(new_type->params, param_list);
  db.types.push_back(new_type);
  return new_type;
}

Type *db_get_pointer_type(Type *subtype) {
  //foreach(type in db) {
  for(auto *type : db.types) {
    if (type->kind == POINTER_TYPE &&
      type->base == subtype) {
        return type;
    }
  }

  return db_add_pointer_type(subtype);
}

Type *db_get_array_type(Type *subtype, int64_t count) {
  //foreach(type in db) {
  for(auto *type : db.types) {
    if (type->kind == ARRAY_TYPE &&
      type->base == subtype &&
      type->count == count) {
        return type;
    }
  }

  return db_add_array_type(subtype, count);
}

int expand_tokens(Token *token, Token *subtokens[], int max_subtokens) {
  int num_tokens = 0;
  for(Token *cur = token->start; cur != NULL; cur = cur->next) {
    assert(num_tokens < max_subtokens);
    subtokens[num_tokens++] = cur;
  }
  return num_tokens;
}

Type *emit_type_declaration(Token *declaration);

Type *db_get_field_type(Type *type, const SubString &identifier) {
  //foreach(type in db) {
  //for(auto &field : type->fields) {
  for(Token *field = type->fields->start; field != NULL; field = field->next) {
    Token *subtokens[3];
    int num_subtokens = expand_tokens(field, subtokens, 3);
    Token *type_token = subtokens[0];
    Token *identifier_token = subtokens[1];
    if (substring_cmp(identifier_token->substring, identifier)) {
        return emit_type_declaration(type_token);
    }
  }

  return NULL;
}

Type *db_get_base_type(const SubString &identifier) {
  //foreach(type in db) {
  for(auto *type : db.types) {
    if ((type->kind == LLVM_TYPE ||
      type->kind == STRUCT_TYPE ||
      type->kind == TYPEDEF_TYPE) &&
      substring_cmp(type->identifier, identifier)) {
        return type;
    }
  }

  return NULL;
}

Type *db_get_function_type(Type *retval_type, Token *param_list) {
  //foreach(type in db) {
  for(auto *type : db.types) {
    if (type->kind == FUNCTION_TYPE &&
      type->retval == retval_type &&
      compare_type_list(type->param_list, param_list)) {
        return type;
    }
  }

  return db_add_function_type(retval_type, param_list);
}

Function *db_function_lookup(const SubString &identifier, Token *param_list) {
  for(auto *function : db.functions) {
    if (substring_cmp(function->identifier, identifier) &&
        compare_type_list(function->type->param_list, param_list)) {
        return function;
    }
  }

  return NULL;
}

Function *db_operator_lookup(TokenType op, Type *type_lhs, Type *type_rhs) {
  for(auto *function : db.functions) {
    if (function->op == op &&
        compare_function_params(function, type_lhs, type_rhs)) {
        return function;
    }
  }

  return NULL;
}

Function *db_operator_lookup(TokenType op, Type *type_lhs) {
  for(auto *function : db.functions) {
    if (function->op == op &&
        compare_function_params(function, type_lhs)) {
        return function;
    }
  }

  return NULL;
}

enum TypecheckResultType {
  //TYPECHECK_EMPTY,
  TYPECHECK_TYPE,
  TYPECHECK_ERROR,
};

struct TypecheckResult {
    TypecheckResultType result;

    union {
      Type *type;
      Error error;
    };
};

const TypecheckResult TYPECHECK_SUCCESS = {};

bool is_success(const TypecheckResult &result) {
  return result.result != TYPECHECK_ERROR;
}

TypecheckResult make_error(const char *, ...) {
  TypecheckResult  result;
  result.result = TYPECHECK_ERROR;
  return result;
}

//const char *to_string(const TypecheckResult &result) {
//  switch(type->kind) {
//  case BASE_TYPE:
//    return type.identifier
//  }
//  return to_string(result->type
//}

TypecheckResult make_result(const TypecheckResult &result_lhs, const TypecheckResult &result_rhs) {
  if (result_lhs.type != result_rhs.type) {
    return make_error("Type mismatch"); //: %s, %s", to_string(result_lhs), to_string(result_rhs));
  }
  return TYPECHECK_SUCCESS;
}
 
//TypecheckResult make_result(const TypecheckResult &result_lhs, Token *assignment_operator, const TypecheckResult &result_rhs) {
//}

//TypecheckResult make_result(Field *field) {
//  if (field == NULL) {
//    return make_error("No field");
//  }
//
//  TypecheckResult  result;
//  result.result = TYPECHECK_TYPE;
//  result.type = field->type;
//  return result;
//}

TypecheckResult make_result(Type *type) {
  TypecheckResult  result;
  result.result = TYPECHECK_TYPE;
  result.type = type;
  return result;
}

TypecheckResult make_result(Function *function) {
  if (function == NULL) {
    return make_error("No function for operator %s with types %s %s");
  } else {
    return make_result(function->type->retval);
  }
}

bool is_boolean(Type *type) {
  if (type->kind != LLVM_TYPE) return false;
  return type->llvm_type->isIntegerTy() &&
    type->llvm_type->getScalarSizeInBits() == 1;
}


bool is_integer(Type *type) {
  if (type->kind != LLVM_TYPE) return false;
  return type->llvm_type->isIntegerTy();
  // check against llvm type for integerness
}

//int64_t to_int64(Value *value) {
//  assert(is_integer(value->type));
//  value->llvm_value
//}
//
//int64_t to_int64(Constant *value) {
//  assert(is_integer(value->type));
//  value->llvm_value->get
//}

//Value *eval_expression(Token *expression) {
//}

//Value *eval_const_expression(Token *expression) {
//  // TODO
//  switch(expr->type) {
//    case TOKEN_INTEGER_LITERAL
//  }
//}

uint64_t substring_to_uint64(const SubString &substring) {
  uint64_t retval = 0;
  for(int i = 0; i < substring.length; ++i)
  {
    char c = substring.start[i];
    assert(isdigit(c));
    retval *= 10;
    retval += c - '0';
  }

  return retval;
}

uint64_t emit_compile_time_constant_integer(Token *expression) {
  assert(expression->type == TOKEN_INTEGER_LITERAL);
  uint64_t retval = substring_to_uint64(expression->substring);
  return retval;

  //TODO:
  //Value *value = eval_const_expression(expression);
  //assert(is_integer(value->type));
  //const llvm::APInt &apint = value->llvm_constant->getUniqueInteger();
  //assert(apint.getActiveBits())
  //to_int64(value);
}

//TypeList *emit_param_type_list(Token *param_list) {
//}

Type *emit_pointer_type(Token *declaration) {
  Token *subtokens[1];
  int num_subtokens = expand_tokens(declaration, subtokens, 1);
  Type *subtype = emit_type_declaration(subtokens[0]);
  return db_get_pointer_type(subtype);
}

Type *emit_array_type(Token *declaration) {
  Token *subtokens[2];
  int num_subtokens = expand_tokens(declaration, subtokens, 2);
  Type *subtype = emit_type_declaration(subtokens[0]);
  int64_t count = emit_compile_time_constant_integer(subtokens[1]);
  return db_get_array_type(subtype, count);
}

Type *emit_function_type(Token *declaration) {
  Token *subtokens[2];
  int num_subtokens = expand_tokens(declaration, subtokens, 2);
  Type *retval_type = emit_type_declaration(subtokens[0]);
  Token *param_list = subtokens[1];
  return db_get_function_type(retval_type, param_list);
}

Type *emit_base_type(Token *type_identifier) {
  db_get_base_type(type_identifier->substring);
}

Type *emit_type_declaration(Token *declaration) {
  switch(declaration->type) {
  case TOKEN_IDENTIFIER:
    return emit_base_type(declaration);
  case TOKEN_POINTER_TYPE:
    return emit_pointer_type(declaration);
  case TOKEN_ARRAY_TYPE:
    return emit_array_type(declaration);
  case TOKEN_FUNCTION_TYPE:
    return emit_function_type(declaration);
  //case TOKEN_PARAMETERIZED_TYPE:
  //  return emit_pointer_type(declaration);
  default:
    halt();
  }

  return NULL;
}

TypecheckResult typecheck_operator_call(TokenType op, const TypecheckResult &lhs, const TypecheckResult &rhs) {
  Function *function = db_operator_lookup(op, lhs.type, rhs.type);
  return make_result(function);
}

TypecheckResult typecheck_operator_call(TokenType op, const TypecheckResult &lhs) {
  Function *function = db_operator_lookup(op, lhs.type);
  return make_result(function);
}

TypecheckResult typecheck_expression(Token *expression);

TypecheckResult typecheck_type_declaration(Token *declaration) {
  // really just gets the type
  Type *type = emit_type_declaration(declaration);
  if (type == NULL) {
    return make_error("Invalid type declaration");
  }
  return make_result(type);
}

TypecheckResult typecheck_binary_expression(Token *expression) {
  Token *subtokens[3];
  int num_subtokens = expand_tokens(expression, subtokens, 3);
  TypecheckResult result_lhs = typecheck_expression(subtokens[0]);
  Token *op = subtokens[1];
  TypecheckResult result_rhs = typecheck_expression(subtokens[2]);
  return typecheck_operator_call(op->type, result_lhs, result_rhs);
}

TypecheckResult typecheck_unary_expression(Token *expression){
  Token *subtokens[2];
  int num_subtokens = expand_tokens(expression, subtokens, 2);
  Token *op = subtokens[0];
  TypecheckResult result_lhs = typecheck_expression(subtokens[1]);
  return typecheck_operator_call(op->type, result_lhs);
}

TypecheckResult typecheck_postfix_inc_expression(Token *expression){
  Token *subtokens[1];
  int num_subtokens = expand_tokens(expression, subtokens, 1);
  TypecheckResult result_lhs = typecheck_expression(subtokens[0]);
  return typecheck_operator_call(TOKEN_OP_POSTFIX_INC, result_lhs);
}

TypecheckResult typecheck_postfix_dec_expression(Token *expression){
  Token *subtokens[1];
  int num_subtokens = expand_tokens(expression, subtokens, 1);
  TypecheckResult result_lhs = typecheck_expression(subtokens[0]);
  return typecheck_operator_call(TOKEN_OP_POSTFIX_DEC, result_lhs);
}

TypecheckResult typecheck_member_derefernce_expression(Token *expression){
  Token *subtokens[2];
  int num_subtokens = expand_tokens(expression, subtokens, 2);
  TypecheckResult result_lhs = typecheck_expression(subtokens[0]);
  if (is_success(result_lhs) == false) return result_lhs;

  Token *field_identifier = subtokens[1];
  Type *field_type = db_get_field_type(result_lhs.type, field_identifier->substring);
  return make_result(field_type);
}

TypecheckResult typecheck_variable_definition(Token *statement);

TypecheckResult typecheck_function_call_expression(Token *expression){
  Token *subtokens[2];
  int num_subtokens = expand_tokens(expression, subtokens, 2);

  Token *function_identifier = subtokens[0];
  Token *function_call_params = subtokens[1];

  for(Token *param = function_call_params->start; param!= NULL; param = param->next) {
    TypecheckResult result = typecheck_expression(param);
    if (is_success(result) == false) return result;
  }

  Function *function = db_function_lookup(function_identifier->substring, function_call_params->start);
  if (function == NULL) {
    return make_error("Unknown function");
  }

  return make_result(function->type->retval);
}

bool is_array(Type *type) {
  return type->kind == ARRAY_TYPE;
}

bool is_compile_time(Type *) {
  // TODO
  return true;
}

bool is_compile_time_integer(Type *type) {
  return is_compile_time(type) && is_integer(type);
}

TypecheckResult typecheck_array_dereference_expression(Token *expression){
  Token *subtokens[2];
  int num_subtokens = expand_tokens(expression, subtokens, 1);
  TypecheckResult result_base = typecheck_expression(subtokens[0]);
  if (is_success(result_base) == false) return result_base;
  if (is_array(result_base.type) == false) {
    return make_error("Expected array");
  }

  TypecheckResult result_index = typecheck_expression(subtokens[1]);
  if (is_success(result_index) == false) return result_index;
  if (is_compile_time_integer(result_index.type) == false) {
    return make_error("Expected compile time integer");
  }

  return make_result(result_base.type->base);
}

TypecheckResult typecheck_expression(Token *expression) {
  switch(expression->type) {
  case TOKEN_BINARY_EXPRESSION:
    return typecheck_binary_expression(expression);
  case TOKEN_UNARY_EXPRESSION:
    return typecheck_unary_expression(expression);
  case TOKEN_OP_POSTFIX_INC:
    return typecheck_postfix_inc_expression(expression);
  case TOKEN_OP_POSTFIX_DEC:
    return typecheck_postfix_dec_expression(expression);
  case TOKEN_OP_MEMBER:
    return typecheck_member_derefernce_expression(expression);
  case TOKEN_OP_FUNCTION_CALL:
    return typecheck_function_call_expression(expression);
  case TOKEN_OP_ARRAY_INDEX:
    return typecheck_array_dereference_expression(expression);
  default:
    halt();
  }

  return make_error("...");
}

TypecheckResult typecheck_variable_definition(Token *statement) {
  Token *subtokens[3];
  int num_subtokens = expand_tokens(statement, subtokens, 3);

  TypecheckResult result_lhs = typecheck_type_declaration(subtokens[0]);
  TypecheckResult result_rhs = typecheck_expression(subtokens[2]);
  return make_result(result_lhs, result_rhs);
}

TypecheckResult typecheck_struct_definition(Token *statement) {
  Token *subtokens[2];
  int num_subtokens = expand_tokens(statement, subtokens, 2);

  for(Token *statement = subtokens[1]; statement != NULL; statement = statement->next) {
    TypecheckResult result = typecheck_variable_definition(statement);
    if (is_success(result) == false) return result;
  }

  return TYPECHECK_SUCCESS;
}

TypecheckResult typecheck_function_params(Token *statement) {
  Token *subtokens[1];
  int num_subtokens = expand_tokens(statement, subtokens, 1);

  for(Token *statement = subtokens[0]; statement != NULL; statement = statement->next) {
    TypecheckResult result = typecheck_variable_definition(statement);
    if (is_success(result) == false) return result;
  }

  return TYPECHECK_SUCCESS;
}

TypecheckResult typecheck_boolean_expression(Token *expression) {
  TypecheckResult result = typecheck_expression(expression);
  if (is_success(result) == false) return result;
  if (is_boolean(result.type) == false) {
    return make_error("Expected boolean.");
  }

  return result;
}

TypecheckResult typecheck_function_statement(Token *statement, const TypecheckResult &result_retval);

TypecheckResult typecheck_if_statement(Token *statement, const TypecheckResult &result_retval) {
  Token *subtokens[3];
  int num_subtokens = expand_tokens(statement, subtokens, 3);

  TypecheckResult result = typecheck_boolean_expression(subtokens[0]);
  if (is_success(result) == false) return result;

  TypecheckResult result_then = typecheck_function_statement(subtokens[1], result_retval);
  if (is_success(result_then) == false) return result_then;

  TypecheckResult result_else = typecheck_function_statement(subtokens[2], result_retval);
  if (is_success(result_else) == false) return result_else;

  return TYPECHECK_SUCCESS;
}

//TypecheckResult typecheck_for_statement(Token *statement, const TypecheckResult &result_retval) {
//}

//TypecheckResult typecheck_switch_statement(Token *statement, const TypecheckResult &result_retval) {
//}

TypecheckResult typecheck_return_statement(Token *statement, const TypecheckResult &result_retval) {
  Token *subtokens[1];
  int num_subtokens = expand_tokens(statement, subtokens, 1);

  TypecheckResult result_rhs = typecheck_expression(subtokens[0]);
  return make_result(result_retval, result_rhs);
}

TypecheckResult typecheck_expression_statement(Token *statement) {
  return typecheck_expression(statement);
}

TypecheckResult typecheck_assignment_statement(Token *statement) {
  Token *subtokens[4];
  int num_subtokens = expand_tokens(statement, subtokens, 4);

  TypecheckResult result_lhs = typecheck_type_declaration(subtokens[0]);
  Token *assigment_operator = subtokens[2];
  TypecheckResult result_rhs = typecheck_expression(subtokens[3]);
  
  return typecheck_operator_call(assigment_operator->type, result_lhs, result_rhs);
  //return make_result(result_lhs, assigment_operator, result_rhs);
}

TypecheckResult typecheck_function_statement(Token *statement, const TypecheckResult &result_retval) {
  switch(statement->type) {
    case TOKEN_IF_STATEMENT:
      return typecheck_if_statement(statement, result_retval);
    //case TOKEN_FOR_STATEMENT:
    //  return typecheck_for_statement(statement, result_retval);
    //case TOKEN_SWITCH_STATEMENT:
    //  return typecheck_switch_statement(statement, result_retval);
    case TOKEN_RETURN_STATEMENT:
      return typecheck_return_statement(statement, result_retval);
    case TOKEN_EXPRESSION_STATEMENT:
      return typecheck_expression_statement(statement);
    case TOKEN_ASSIGMENT_STATEMENT:
      return typecheck_assignment_statement(statement);
    default:
      halt();
  }

  return make_error("...");
}

TypecheckResult typecheck_function_body(Token *body, const TypecheckResult &result_retval) {
  Token *subtokens[1];
  int num_subtokens = expand_tokens(body, subtokens, 1);

  for(Token *statement = subtokens[0]; statement != NULL; statement = statement->next) {
    TypecheckResult result = typecheck_function_statement(statement, result_retval);
    if (is_success(result) == false) return result;
  }

  return result_retval;
}

TypecheckResult typecheck_function_definition(Token *statement) {
  Token *subtokens[4];
  int num_subtokens = expand_tokens(statement, subtokens, 4);

  TypecheckResult result_params = typecheck_function_params(subtokens[2]);
  if (is_success(result_params) == false) return result_params;

  TypecheckResult result_retval = typecheck_type_declaration(subtokens[0]);
  TypecheckResult result_body = typecheck_function_body(subtokens[3], result_retval);
  return result_body;
}

TypecheckResult typecheck_program(Token *program) {
  for(Token *program_statement = program->start; program_statement != NULL; program_statement = program_statement->next) {
    switch(program_statement->type) {
    case TOKEN_EXTERNAL_FUNCTION_DECLARATION:
    case TOKEN_TYPEDEF_DEFINITION:
    case TOKEN_LLVM_TYPE_DEFINITION:
      break;
    case TOKEN_VARIABLE_DEFINITION:
      typecheck_variable_definition(program_statement);
      break;
    case TOKEN_STRUCT_DEFINITION:
      typecheck_struct_definition(program_statement);
      break;
    //case TOKEN_ENUM_DEFINITION:
    //  typecheck_enum_definition(program_statement);
    //  break;
    case TOKEN_FUNCTION_DEFINITION:
      typecheck_function_definition(program_statement);
      break;
    default:
      halt();
    }
  }

  return make_error("...");
}

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////

struct EmitResult {
  Error error;
};

const EmitResult EMIT_SUCCESS = {};

struct Emitter {
};

struct LLVM {
  llvm::Module *module;
  llvm::IRBuilder<> *builder;
  llvm::LLVMContext *context;

  //DBItem db_items[MAX_DB_ITEMS];
  //int num_db_items;
  //int db_scope;

  //FieldInfo fields[MAX_FIELDS];
  //int num_fields;

  //FunctionParamInfo function_params[MAX_FIELDS];
  //int num_function_params;
};

llvm::StringRef to_string_ref(const SubString &substring) {
  return llvm::StringRef(substring.start, substring.length);
}

llvm::Type *emit_struct_declaration(LLVM &llvm, Token *struct_def) {
  Token *subtokens[2];
  int num_subtokens = expand_tokens(struct_def, subtokens, 2);
  const Token &identifier = *subtokens[0];
  const Token &struct_body = *subtokens[1];

  llvm::StringRef name = to_string_ref(identifier.substring);
  llvm::StructType *type = llvm::StructType::create(*llvm.context, name);

  //DBItem &item = db_add_struct_type(llvm, identifier, type);
  //item.typeInfo.numFields = struct_body.num_subtokens;
  //item.typeInfo.fields = db_alloc_fields(llvm, struct_body.num_subtokens);
  //for(int i = 0; i < struct_body.num_subtokens; ++i) {
  //  item.typeInfo.fields[i].fieldName = struct_body.subtokens[i].subtokens[1].substring;
  //}


  return type;
}

llvm::Type *llvm_emit_llvm_type_definition(LLVM &llvm, Token *type_def) {
  assert(type_def.num_subtokens == 2);
  const GGToken &identifier = type_def.subtokens[0];
  const GGToken &type_decl = type_def.subtokens[1];

  llvm::Type *type = get_type(llvm, type_decl);
  db_add_type(llvm, identifier, type);
  return type;
}

void emit_global_type_definitions(LLVM &llvm, Token *program) {
  for(Token *program_statement = program->start; program_statement != NULL; program_statement = program_statement->next) {
    switch(program_statement->type) {
    case TOKEN_STRUCT_DEFINITION:
      emit_struct_declaration(llvm, program_statement);
      break;
    case TOKEN_LLVM_TYPE_DEFINITION:
      emit_llvm_type_definition(llvm, program_statement);
      break;
    case TOKEN_TYPEDEF_DEFINITION:
    case TOKEN_IMPORT_STATEMENT:
    case TOKEN_VARIABLE_DEFINITION:
    case TOKEN_FUNCTION_DEFINITION:
    case TOKEN_EXTERNAL_FUNCTION_DECLARATION:
      break;
    default:
      halt();
    }
  }

  for(Token *program_statement = program->start; program_statement != NULL; program_statement = program_statement->next) {
    switch(program_statement->token) {
    case TOKEN_STRUCT_DEFINITION:
      emit_struct_definition(llvm, program_statement);
      break;
    case TOKEN_TYPEDEF_DEFINITION:
      emit_type_definition(llvm, program_statement);
      break;
    case TOKEN_LLVM_TYPE_DEFINITION:
    case TOKEN_IMPORT_STATEMENT:
    case TOKEN_VARIABLE_DEFINITION:
    case TOKEN_FUNCTION_DEFINITION:
    case TOKEN_EXTERNAL_FUNCTION_DECLARATION:
      break;
    default:
      halt();
    }
  }
}
LLVM LLVMInit(const char *dest_file) {
  LLVM llvm = {};
  llvm.context = &llvm::getGlobalContext();
  llvm.builder = new llvm::IRBuilder<>(*llvm.context);
  llvm.module = new llvm::Module(dest_file, *llvm.context);
  return llvm;
}

EmitResult emit_program(Token *program, const char *dest_file) {
  LLVM llvm = LLVMInit(dest_file);

  EmitResult result = emit_global_type_definitions(llvm, program);
  if(is_success(result) == false) return result;
  result = emit_global_function_prototypes(llvm, program);
  if(is_success(result) == false) return result;
  result = emit_global_variable_definitions(llvm, program);
  if(is_success(result) == false) return result;
  result = emit_global_function_definitions(llvm, program);
  if(is_success(result) == false) return result;

  llvm.module->dump();

  //IRCompile(llvm);
  return EMIT_SUCCESS;
}

bool is_success(const EmitResult &result) {
  return true;
}

struct CompileResult {
  Error error;
};

CompileResult make_result(const ParseResult &parse_result) {
  CompileResult retval;
  retval.error = parse_result.error;
  return retval;
}

CompileResult make_result(const TypecheckResult &typecheck_result) {
  CompileResult retval;
  retval.error = typecheck_result.error;
  return retval;
}

CompileResult make_result(const EmitResult &emit_result) {
  CompileResult retval;
  retval.error = emit_result.error;
  return retval;
}

std::string to_dest_file(const char *source_file) {
  std::string directory;
  SubString filename;
  path_split_directory_filename(source_file, directory, filename);
  std::string retval = directory.append(filename.start, filename.length);
  size_t endPos = retval.find_last_of(".");
  retval = retval.substr(endPos);
  return retval;
}

const CompileResult COMPILE_SUCCESS = {};

CompileResult compile_program(const char *source_file) {
  ParseResult parse_result = parse_program(source_file);
  if (is_success(parse_result) == false) return make_result(parse_result);

  TypecheckResult typecheck_result = typecheck_program(parse_result.start);
  if (is_success(typecheck_result) == false) return make_result(typecheck_result);

  std::string dest_file = to_dest_file(source_file);

  EmitResult emit_result = emit_program(parse_result.start, dest_file.c_str());
  if (is_success(emit_result) == false) return make_result(typecheck_result);
  
  return COMPILE_SUCCESS;
}