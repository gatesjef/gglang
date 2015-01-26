// GGParser.cpp

#include "Precompile.h"
#include "GGParser.h"


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
//struct Type;

struct SubString {
  const char *start;
  int length;
};

struct Token {
  TokenType type;
  //Type *expr_type;

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

//struct Function
//{
//  //Type *retval_type;
//  //TypeList *param_types;
//  //Type *type;
//
//  llvm::Function *function;
//  Token *token;
//
//  SubString identifier;
//  TokenType op;
//  Token *param_types;
//  Token *retval;
//};

//struct Value
//{
//  SubString identifier;
//  Type *type;
//  llvm::Value *llvm_value;
//};

enum TypeKind {
  //BASE_TYPE,
  LLVM_TYPE,
  //TYPEDEF_TYPE,
  STRUCT_TYPE,
  ARRAY_TYPE,
  POINTER_TYPE,
  FUNCTION_TYPE,
};

struct StructDef;

struct LLVMTypeDef {
  SubString identifier;
  Token *raw_llvm;
  // size?

  llvm::Type *llvm_type;
  Token *token;
};

struct TypeDef {
  int scope;
  TypeKind kind;

  union {
    struct LlvmType {
      LLVMTypeDef type_definition;
    } llvm_def;
    struct StructType {
      StructDef *type_definition;
    } struct_def;
    struct PointerType {
      TypeDef *base_type;
    } pointer;
    struct ArrayType {
      TypeDef *base_type;
      uint64_t count;
    } array;
    struct FunctionType {
      //FunctionDef *function_def;
      TypeDef *retval_type;
      Token *param_list;
    } function;
  };

  Token *token;
  llvm::Type *llvm_type;
};

struct VariableDef {
  int scope;
  SubString identifier;
  TypeDef *type;
  Token *initializer_value;

  llvm::Value *llvm_value;
  Token *token;
};

struct FieldDef {
  SubString identifier;
  int index;
  TypeDef *type;
  Token *value;

  llvm::Type *llvm_type;
  Token *token;
};

struct StructDef {
  int scope;
  SubString identifier;
  FieldDef *fields;
  int num_fields;

  llvm::Type *llvm_type;
  Token *token;
};

struct TypeDef;

enum Linkage {
  LINKAGE_INTERNAL,
  LINKAGE_EXTERNAL,
};

struct FunctionDef {
  int scope;
  Linkage linkage;
  SubString identifier;
  TokenType op;
  TypeDef *retval_type;
  TypeDef **param_types;
  int num_params;
  Token *body;

  llvm::Function *llvm_function;
  Token *token;
};

//template <typename T> 
//struct Table {
//};

#include "list"
#define Table std::list

struct SymbolDatabase {
  Table<TypeDef> types;
  Table<StructDef> structs;
  Table<FunctionDef> functions;
  Table<VariableDef> variables;
  int scope;

  //std::map<SubString, Type> types;
  //std::map<SubString, Value> variables;
  //std::multimap<SubString, Function> functions;
  //std::vector<Type *> types;
  //std::vector<Value *> variables;
  //std::vector<Function *> functions;
};

//struct LLVM {
//  llvm::Module *module;
//  llvm::IRBuilder<> *builder;
//  llvm::LLVMContext *context;
//
//  //DBItem db_items[MAX_DB_ITEMS];
//  //int num_db_items;
//  //int db_scope;
//
//  //FieldInfo fields[MAX_FIELDS];
//  //int num_fields;
//
//  //FunctionParamInfo function_params[MAX_FIELDS];
//  //int num_function_params;
//};

struct G {
  //ParserState parser;
  SymbolDatabase db;
  LLVMState llvm;
} g;

void db_push_scope() {
  g.db.scope++;
}

void db_pop_scope() {
  g.db.scope--;

  for(auto &vars : g.db.functions) {
    if (vars.scope > g.db.scope) {
      //erase..
    }
  }

}

void LLVMInit(const char *dest_file) {
  g.llvm.context = &llvm::getGlobalContext();
  g.llvm.builder = new llvm::IRBuilder<>(*g.llvm.context);
  g.llvm.module = new llvm::Module(dest_file, *g.llvm.context);
  //return llvm;
}

//void GlobalsInit() {
//  //ParserInit(g.parser);
//  LLVMInit(g.llvm);
//}

//struct TokenPool {
//};

//struct TypeList;
//struct Type;

//struct Field
//{
//  int index;
//  Type *type;
//
//  SubString identifier;
//};

//struct Type
//{
//  //Type = BaseType token | PointerType base | ArrayType base count | FunctionType retval params
//
//  TypeKind kind;
//
//  union {
//    struct BaseType {
//      Token *type_definition;
//    } base;
//    struct PointerType {
//      Type *base_type;
//    } pointer;
//    struct ArrayType {
//      Type *base_type;
//      uint64_t count;
//    } array;
//    struct FunctionType {
//      Type *retval_type;
//      Token *param_list;
//    } function;
//  };
//
//  //Type *base_type;
//
//  llvm::Type *llvm_type;
//
//  //union {
//  //  struct { // llvm_type
//  //    SubString identifier;
//  //  };
//  //  struct { // array type
//  //    Type *base;
//  //    int64_t count;
//  //  };
//  //  struct { // function type
//  //    Type *retval;
//  //    Token *param_list;
//  //  };
//
//  //  struct { // struct type
//  //    //std::vector<Field> fields;
//  //    Token *fields;
//  //  };
//  //};
//};

//struct TypeList
//{
//  Type *type;
//  TypeList *next;
//};

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

enum EmitResultType  {
  EMIT_OK,
  EMIT_ERROR,
};

struct EmitResult {
  EmitResultType type;
  Error error;
};

const EmitResult EMIT_SUCCESS = {};

EmitResult make_emit_error(const char *error_fmt, ...) {
  EmitResult retval;
  retval.type = EMIT_ERROR;
  return retval;
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
  ParseResult result = parse_zero_or_more_separated(parser, parse_expression, ",");
  if (is_success(result) == false) return result;

  return make_result(parser, TOKEN_FUNCTION_CALL_PARAMS, result);
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

  lhs->next = params.start;

  {
    ParseResult result = parse_exact(parser, ")");
    if (is_success(result) == false)  {
      make_error(parser, "Missing left paren ')' for function call");
      return result;
    }
  }

  ParseResult subtokens;
  subtokens.type = RESULT_SUCCESS;
  subtokens.start = lhs;
  lhs->next = params.start;
  subtokens.end = params.start;

  ParseResult retval = make_result(parser, TOKEN_OP_FUNCTION_CALL, subtokens);
  //append_result(retval, params);
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

bool substring_cmp(const SubString &substring0, const SubString &substring1) {
  if (substring0.length != substring1.length) return false;
  return strncmp(substring0.start, substring1.start, substring0.length) == 0;
}

bool substring_cmp(const SubString &substring, const char *str) {
  return strncmp(substring.start, str, substring.length) == 0;
}

//SubString get_identifier(Token *type_definition) {
//  return type_definition->start->substring;
//}

//TypeDef *db_lookup_type_by_identifier(const SubString &identifier) {
//  for(auto type : db.types) {
//    if (type.kind == BASE_TYPE &&
//      substring_cmp(get_identifier(type->base.type_definition), identifier) == true) {
//      return type;
//    }
//  }
//
//  return NULL;
//}
//
//bool db_add_type_definition(Token *token) {
//  SubString identifier = get_identifier(token);
//  Type *existing_type = db_lookup_type_by_identifier(identifier);
//  if (existing_type != NULL) return false;
//  Type *new_type = new Type;
//  new_type->kind = BASE_TYPE;
//  new_type->base.type_definition = token;
//  new_type->llvm_type = NULL;
//  db.types.push_back(new_type);
//  return true;
//}


//
//Type *db_lookup_type_by_identifier(const SubString &identifier) {
//  //foreach(type in db) {
//  for(auto *type : db.types) {
//    if (type->kind == LLVM_TYPE ||
//      type->kind == TYPEDEF_TYPE ||
//      type->kind == STRUCT_TYPE ||
//      type->kind == LLVM_TYPE) {
//        if (substring_cmp(identifier, type->identifier)) {
//          return type;
//        }
//    }
//  }
//
//  return NULL;
//}
//
//Type *db_add_typedef_type_partial(const SubString &identifier) {
//  if (db_lookup_type_by_identifier(identifier) != NULL) return NULL;
//
//  Type *new_type = new Type;
//  new_type->kind = TYPEDEF_TYPE;
//  new_type->identifier = identifier;
//  db.types.push_back(new_type);
//  return new_type;
//}

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

//Type *db_add_struct_type_partial(const SubString &identifier) {
//  if (db_lookup_type_by_identifier(identifier) != NULL) return NULL;
//
//  Type *new_type = new Type;
//  new_type->kind = STRUCT_TYPE;
//  new_type->identifier = identifier;
//  db.types.push_back(new_type);
//  return new_type;
//}
//
//Type *db_add_llvm_type_partial(const SubString &identifier) {
//  if (db_lookup_type_by_identifier(identifier) != NULL) return NULL;
//
//  Type *new_type = new Type;
//  new_type->kind = LLVM_TYPE;
//  new_type->identifier = identifier;
//  db.types.push_back(new_type);
//  return new_type;
//}

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
  int len = strlen(str);
  while(1) {
    if (*parser.current_char == 0) {
      return make_error(parser, "Unexpected end of file");
    }

    if (strncmp(str, parser.current_char, len) == 0) {
      ParseResult retval = make_result(parser, type, start, parser.current_char);
      //parser.current_char += len;
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

  ParseResult result = parse_first_of(parser, statements, num_statements);
  if (is_success(result)) {
    //db_add_type_definition(result.start);
  }
  return result;
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

TypeDef *db_add_pointer_type(TypeDef *subtype) {
  TypeDef new_type = {};
  new_type.kind = POINTER_TYPE;
  new_type.pointer.base_type = subtype;
  new_type.llvm_type = NULL;
  new_type.scope = g.db.scope;
  g.db.types.push_back(new_type);
  return &g.db.types.back();
}
////
TypeDef *db_add_array_type(TypeDef *subtype, int64_t count) {
  TypeDef new_type = {};
  new_type.kind = ARRAY_TYPE;
  new_type.array.base_type = subtype;
  new_type.array.count = count;
  new_type.llvm_type = NULL;
  new_type.scope = g.db.scope;
  g.db.types.push_back(new_type);
  return &g.db.types.back();
}

//void copy_param_list(TypeList *&dest, TypeList *source) {
//}

TypeDef *emit_type_declaration(Token *declaration);

bool compare_type_list(Token *dest, Token *source) {
  while(1) {
    if (dest == NULL && source == NULL) return true;
    if (dest == NULL || source == NULL) return false;
    
    TypeDef *type_source = emit_type_declaration(source->start);
    TypeDef *type_dest = emit_type_declaration(dest->start);

    if (type_source != type_dest) return false;

    dest = dest->next;
    source = source->next;
  }
}


//bool expand_typelist(Token *param_list, Type **param_types, int expected_params) {
//  int actual_params = 0;
//  for(Token *current = param_list; current != NULL; current = current->next) {
//    if (actual_params < expected_params) {
//      assert(current->expr_type);
//      param_types[actual_params] = current->expr_type;
//    }
//    ++actual_params;
//  }
//
//  return (actual_params == expected_params);
//}

//bool compare_function_params(Function *function, Type *lhs, Type *rhs) {
//  Type *param_types[2];
//  bool correct_params = expand_typelist(function->type->param_list, param_types, 2);
//  if (correct_params) return false;
//  if (lhs != param_types[0]) return false;
//  if (rhs != param_types[1]) return false;
//  return true;
//}
//
//bool compare_function_params(Function *function, Type *lhs) {
//  Type *param_types[1];
//  bool correct_params = expand_typelist(function->type->param_list, param_types, 1);
//  if (correct_params) return false;
//  if (lhs != param_types[0]) return false;
//  return true;
//}

//Type *db_add_struct_type(const SubString &identifier, Token *fields) {
//  if (db_lookup_type_by_identifier(identifier) != NULL) return NULL;
//
//  Type *new_type = new Type;
//  new_type->kind = STRUCT_TYPE;
//  new_type->identifier = identifier;
//  new_type->fields = fields;
//  db.types.push_back(new_type);
//  return new_type;
//}
//

//EmitResult db_add_varaible(const SubString &identifier, llvm::Value *variable) {
//  void *existing = db_lookup_any(identifier);
//  assert(existing == NULL);
//  assert(llvm.num_db_items < MAX_DB_ITEMS);
//  DBItem &item = llvm.db_items[llvm.num_db_items++];
//  item.itemType = IDENTIFIER_VARIABLE;
//  item.identifier = identifier.substring;
//  item.value = value;
//  item.scope = llvm.db_scope;
//}

TypeDef *db_add_function_type(TypeDef *retval_type, Token *param_list) {
  TypeDef new_type = {};
  new_type.kind = FUNCTION_TYPE;
  new_type.function.retval_type = retval_type;
  new_type.function.param_list = param_list;
  new_type.llvm_type = NULL;
  //copy_param_list(new_type->params, param_list);
  new_type.scope = g.db.scope;
  g.db.types.push_back(new_type);
  return &g.db.types.back();
}
//
TypeDef *db_get_pointer_type(TypeDef *subtype) {
  //foreach(type in db) {
  for(auto &type : g.db.types) {
    if (type.kind == POINTER_TYPE &&
      type.pointer.base_type == subtype) {
        return &type;
    }
  }

  return db_add_pointer_type(subtype);
}
////
TypeDef *db_get_array_type(TypeDef *subtype, int64_t count) {
  //foreach(type in db) {
  for(auto &type : g.db.types) {
    if (type.kind == ARRAY_TYPE &&
      type.array.base_type== subtype &&
      type.array.count == count) {
        return &type;
    }
  }

  return db_add_array_type(subtype, count);
}
//
int expand_tokens(Token *token, Token *subtokens[], int max_subtokens) {
  int num_tokens = 0;
  for(Token *cur = token->start; cur != NULL; cur = cur->next) {
    assert(num_tokens < max_subtokens);
    subtokens[num_tokens++] = cur;
  }
  return num_tokens;
}

void expand_tokens(Token *token, Token *&subtokens0) {
  Token *subtokens[1];
  int num_tokens = expand_tokens(token, subtokens, 1);
  assert(num_tokens == 1);

  subtokens0 = subtokens[0];
}

void expand_tokens(Token *token, Token *&subtokens0, Token *&subtokens1) {
  Token *subtokens[2];
  int num_tokens = expand_tokens(token, subtokens, 2);
  assert(num_tokens == 2);

  subtokens0 = subtokens[0];
  subtokens1 = subtokens[1];
}

void expand_tokens(Token *token, Token *&subtokens0, Token *&subtokens1, Token *&subtokens2) {
  Token *subtokens[3];
  int num_tokens = expand_tokens(token, subtokens, 3);
  assert(num_tokens == 3);

  subtokens0 = subtokens[0];
  subtokens1 = subtokens[1];
  subtokens2 = subtokens[2];
}

void expand_tokens(Token *token, Token *&subtokens0, Token *&subtokens1, Token *&subtokens2, Token *&subtokens3) {
  Token *subtokens[4];
  int num_tokens = expand_tokens(token, subtokens, 4);
  assert(num_tokens == 4);

  subtokens0 = subtokens[0];
  subtokens1 = subtokens[1];
  subtokens2 = subtokens[2];
  subtokens3 = subtokens[3];
}

//
//Type *emit_type_declaration(Token *declaration);
//
TypeDef *type_get_field_type(TypeDef *type, const SubString &name) {
  assert(type->kind == STRUCT_TYPE);
  //assert(type->base.type_definition->type == TOKEN_STRUCT_DEFINITION);
  StructDef *struct_def = type->struct_def.type_definition;

  //for(Token *field = type->base.type_definition->start; field != NULL; field = field->next) {
  for(int i = 0; i < struct_def->num_fields; ++i) {
    FieldDef &field = struct_def->fields[i];
    if (substring_cmp(field.identifier, name))
      return field.type;
  }

  halt();
  return NULL;
}

//
//Type *db_get_field_type(Type *type, const SubString &identifier) {
//  //foreach(type in db) {
//  //for(auto &field : type->fields) {
//  for(Token *field = type->fields->start; field != NULL; field = field->next) {
//    Token *subtokens[3];
//    int num_subtokens = expand_tokens(field, subtokens, 3);
//    Token *type_token = subtokens[0];
//    Token *identifier_token = subtokens[1];
//    if (substring_cmp(identifier_token->substring, identifier)) {
//        return emit_type_declaration(type_token);
//    }
//  }
//
//  return NULL;
//}
//
//Type *db_get_base_type(const SubString &identifier) {
//  //foreach(type in db) {
//  for(auto *type : db.types) {
//    if ((type->kind == LLVM_TYPE ||
//      type->kind == STRUCT_TYPE ||
//      type->kind == TYPEDEF_TYPE) &&
//      substring_cmp(type->identifier, identifier)) {
//        return type;
//    }
//  }
//
//  return NULL;
//}
//

TypeDef *db_get_function_type(TypeDef *retval_type, Token *param_list) {
  //foreach(type in db) {
  for(auto &type : g.db.types) {
    if (type.kind == FUNCTION_TYPE &&
      type.function.retval_type == retval_type &&
      compare_type_list(type.function.param_list, param_list)) {
        return &type;
    }
  }

  return db_add_function_type(retval_type, param_list);
}
//
//bool compare_param(Type *type0, Type *type1) {
//  return (type0 == type1);
//}
//
//bool compare_param(Token *param0, Type *type1) {
//  return compare_param(emit_type_declaration(param0), type1);
//}
//
//bool compare_param(Token *param0, Token *param1) {
//  return compare_param(emit_type_declaration(param0), emit_type_declaration(param1));
//}
//
//bool compare_function_params(Function *function, Token *param_list) {
//  //for(Token *function_param = function->param_types; param != NULL; param = param->next) {
//
//  Token *function_param = function->param_types->start;
//  Token *list_param = param_list->start;
//
//  for(;;) {
//    if (function_param == NULL && list_param == NULL) return true;
//    else if (function_param == NULL || list_param == NULL ) return false;
//    if (compare_param(function_param, list_param) == false) return false;
//
//    function_param = function_param->next;
//    list_param = list_param->next;
//  }
//}
//
//bool compare_function_params1(Function *function, Type *lhs) {
//  Token *params[1];
//  int num_tokens = expand_tokens(function->param_types, params, 1);
//  if (num_tokens != 1) return false;
//
//  if (compare_param(params[0], lhs) == false) return false;
//  return true;
//}
//
//bool compare_function_params2(Function *function, Type *lhs, Type *rhs) {
//  Token *params[2];
//  int num_tokens = expand_tokens(function->param_types, params, 2);
//  if (num_tokens != 2) return false;
//
//  if (compare_param(params[0], lhs) == false) return false;
//  if (compare_param(params[1], rhs) == false) return false;
//  return true;
//}
//
//const int MAX_PARAMS = 256;
//
//bool compare_function_param_types(Function *function, Type *call_params[], int num_call_params) {
//  Token *function_params[MAX_PARAMS];
//  int num_function_params = expand_tokens(function->param_types, function_params, MAX_PARAMS);
//  if (num_call_params != num_function_params) return false;
//
//  for(int i = 0; i < num_call_params; ++i) {
//    if (call_params[i] != function_params[i]->expr_type) return false;
//  }
//
//  return true;
//}

//void db_add_function(Function *function) {
//  db.functions.push_back(function);
//}

TokenType operator_identifer_to_op(const SubString &identifier) {
  halt(); // todo
  return TOKEN_ADD_OP;
}

//Function *db_function_lookup(const SubString &identifier, Token *param_list) {
//  for(auto *function : db.functions) {
//    if (substring_cmp(function->identifier, identifier) &&
//      compare_function_params(function, param_list)) {
//        return function;
//    }
//  }
//
//  return NULL;
//}
//
//Function *db_function_lookup(const SubString &identifier, Type *param_types[], int num_params) {
//  for(auto *function : db.functions) {
//    if (substring_cmp(function->identifier, identifier) &&
//      compare_function_param_types(function, param_types, num_params)) {
//        return function;
//    }
//  }
//
//  return NULL;
//}
//
//Function *db_operator_lookup(TokenType op, Token *param_list) {
//  for(auto *function : db.functions) {
//    if (function->op == op && compare_function_params(function, param_list)) {
//      return function;
//    }
//  }
//
//  return NULL;
//}
//
//
//Function *db_operator_lookup(TokenType op, Type *type_lhs) {
//  for(auto *function : db.functions) {
//    if (function->op == op &&
//      compare_function_params1(function, type_lhs)) {
//        return function;
//    }
//  }
//
//  return NULL;
//}



//struct TypeDef {
//  TypeKind kind;
//  TypeDef *parent;
//
//  TypeDef *
//
//  llvm::Type *llvm_type;
//};


//struct TokenJob {
//  Token *token
//  Dependancy dependancy;
//};
//
//struct Program {
//  std::vector<TokenJob>
//  Token *program;
//};
//
//process_imports() {
//
//}
//
//
//void find_import_statement() {
//
//}
//
//void eval_imports(Program &program) {
//  TokenJob job;
//  if (find_import_statement(program, job)) {
//    eval_import_statement(program, job);
//    return true;
//  }
//
//  return false;
//}
//
//bool find_compile_time_statement(Program &program, Token *&statement) {
//
//  return false;
//}
//
//void eval_compile_time_statements(Program &program) {
//  foreach(statement in program) {
//    if (statement is compile_time) {
//      eval_compile_time_statement(statement);
//      eval_imports(program);
//    }
//  }
//}
//
//void eval_main(program) {
//  Token* main_statement;
//  if (find_main(program, main_statement)) {
//    eval_main(program, main_statement);
//    return true;
//  }
//
//  return error("No main function.");
//}
//
//void eval_program(Program &program) {
//  eval_imports(program);
//  eval_compile_time_statements(program));
//  eval_main(program);
//}


int count_subtokens(Token *token) {
  int count = 0;
  for(Token *cur = token->start; cur != NULL; cur = cur->next) {
    count++;
  }

  return count;
}

TypeDef **alloc_types(int num_params) {
  return (TypeDef **)malloc(sizeof(TypeDef *) * num_params);
}

FieldDef *alloc_fields(int num_params) {
  return (FieldDef *)malloc(sizeof(FieldDef) * num_params);
}

//TypeDef *db_lookup_type(Token *token_type_declaration) {
//
//}

void function_fill_params(Token *function_params, TypeDef **&types, int &num_params) {
  num_params = count_subtokens(function_params);
  types = alloc_types(num_params);

  Token *function_param = function_params->start;
  for(int i = 0; i < num_params; ++i)
  {
    assert(function_param != NULL);

    Token *function_param_type, *function_param_identifier;
    expand_tokens(function_param, function_param_type, function_param_identifier);
    types[i] = emit_type_declaration(function_param_type);
    function_param = function_param->next;
  }
}

enum DeclarationResultType {
  DB_ERROR,
  DB_SUCCESS,
};

struct DeclarationResult {
  DeclarationResultType type;
  std::string error;
};

DeclarationResult make_db_error(const char *error, const std::string &substr) {
  DeclarationResult retval;
  retval.type = DB_ERROR;
  retval.error = error + substr;
  return retval;
}

DeclarationResult make_db_success() {
  DeclarationResult retval;
  retval.type = DB_SUCCESS;
  return retval;
}

std::string to_cstring(SubString &substring) {
  return std::string(substring.start, substring.length);
}

DeclarationResult function_fill_fields(Token *struct_fields, FieldDef *&fields, int &num_fields) {
  num_fields = count_subtokens(struct_fields);
  fields = alloc_fields(num_fields);

  Token *field_token = struct_fields->start;
  for(int i = 0; i < num_fields; ++i)
  {
    assert(field_token != NULL);

    Token *field_type, *field_identifier, *field_value;
    expand_tokens(field_token, field_type, field_identifier, field_value);

    // check if duplicate field
    for(int x = 0; x < i; ++x) {
      if (substring_cmp(fields[x].identifier, field_identifier->substring) == false) {
        return make_db_error("Duplicate field: %s", to_cstring(field_identifier->substring));
      }
    }

    FieldDef &field = fields[i];
    field.identifier = field_identifier->substring;
    field.index = i;
    field.type = emit_type_declaration(field_type);
    field.value = field_value;
    field.token = field_token;
  }

  return make_db_success();
}

TypeDef *db_lookup_type_by_identifier(const SubString &identifier) {
  for(auto &type : g.db.types) {
    if (type.kind == LLVM_TYPE && substring_cmp(type.llvm_def.type_definition.identifier, identifier) == true) {
      return &type;
    }
    else if (type.kind == STRUCT_TYPE && substring_cmp(type.struct_def.type_definition->identifier, identifier) == true) {
      return &type;
    }
    //else if (type.kind == TYPEDEF_TYPE) {

    //}
  }

  return NULL;
};

bool db_type_exists(SubString &identifier) {
  TypeDef *type = db_lookup_type_by_identifier(identifier);
  if (type == NULL) return false;
  return true;
}

enum TypecheckResultType {
  //TYPECHECK_EMPTY,
  TYPECHECK_TYPE,
  TYPECHECK_ERROR,
};

struct TypecheckResult {
  TypecheckResultType result;

  union {
    TypeDef *type;
    Error error;
  };
};

const TypecheckResult TYPECHECK_SUCCESS = {};

FunctionDef *db_lookup_premain_initizliation_function() {
  // todo
  return NULL;
}

TypecheckResult typecheck_expression(Token *expression);

bool compare_params_types_to_value_types(TypeDef **function_param_types, Token **call_values, int num_params) {
  for(int i = 0; i < num_params; ++i) {
    TypeDef *param_type = function_param_types[i];
    Token *call_value = call_values[i];

    TypeDef *call_type = typecheck_expression(call_value).type;
    if (param_type != call_type) return false;
  }

  return true;
}

bool compare_params_types(TypeDef **function_param_types, Token **declaration_params, int num_params) {
  for(int i = 0; i < num_params; ++i) {
    TypeDef *param_type = function_param_types[i];
    Token *declaration_param = declaration_params[i];
    Token *declaration_param_type_token = declaration_param->start;

    TypeDef *declaration_param_type = emit_type_declaration(declaration_param_type_token);
    if (param_type != declaration_param_type) return false;
  }

  return true;
}

const int MAX_PARAMS = 128;

FunctionDef *db_function_call_lookup(const SubString &identifier, Token *function_params) {
  assert(function_params->type == TOKEN_FUNCTION_CALL_PARAMS);
  for(auto &function : g.db.functions) {
    if (substring_cmp(function.identifier, identifier) == false) continue;
    
    Token *subtokens[MAX_PARAMS];
    int num_params = expand_tokens(function_params, subtokens, MAX_PARAMS);
    if (function.num_params != num_params) continue;

    if (compare_params_types_to_value_types(function.param_types, subtokens, num_params) == false) {
      continue;
    }

    return &function;
  }

  return NULL;
};

FunctionDef *db_function_definition_lookup(const SubString &identifier, Token *function_params) {
  assert(function_params->type == TOKEN_FUNCTION_PARAMS);
  for(auto &function : g.db.functions) {
    if (substring_cmp(function.identifier, identifier) == false) continue;

    Token *subtokens[MAX_PARAMS];
    int num_params = expand_tokens(function_params, subtokens, MAX_PARAMS);
    if (function.num_params != num_params) continue;

    if (compare_params_types(function.param_types, subtokens, num_params) == false) {
      continue;
    }

    return &function;
  }

  return NULL;
};

FunctionDef *db_unary_operator_lookup(TokenType op, TypeDef *type_lhs) {
  for(auto &function : g.db.functions) {
    if (function.op == op &&
      function.num_params == 1 &&
      function.param_types[0] == type_lhs) {
        return &function;
    }
  }

  return NULL;
}

FunctionDef *db_binary_operator_lookup(TokenType op, TypeDef *type_lhs, TypeDef *type_rhs) {
  for(auto &function : g.db.functions) {
    if (function.op == op &&
      function.num_params == 2 &&
      function.param_types[0] == type_lhs &&
      function.param_types[1] == type_rhs) {
        return &function;
    }
  }

  return NULL;
}

void db_struct_add(StructDef &struct_def) {
  struct_def.scope = g.db.scope;
  g.db.structs.push_back(struct_def);
  StructDef *new_struct = &g.db.structs.back();

  TypeDef type = {};
  type.kind = STRUCT_TYPE;
  type.token = struct_def.token;
  type.llvm_type = struct_def.llvm_type;
  type.struct_def.type_definition = new_struct;
  g.db.types.push_back(type);
}

void db_llvm_type_add(LLVMTypeDef &type_def) {
  TypeDef type = {};
  type.kind = LLVM_TYPE;
  type.token = type_def.token;
  type.llvm_type = type_def.llvm_type;
  type.llvm_def.type_definition = type_def;
  type.scope = g.db.scope;
  g.db.types.push_back(type);
}


//bool function_def_exists(const FunctionDef &function) {
//
//}

bool variable_exists(const SubString &identifier) {
  for(auto &var : g.db.variables) {
    if (substring_cmp(var.identifier, identifier) == true) {
      return true;
    }
  }

  return false;
}

DeclarationResult db_add_function_definition(Token *function_definition) {
  FunctionDef function;

  Token *function_retval, *function_identifier, *function_params, *function_body;
  expand_tokens(
    function_definition,
    function_retval,
    function_identifier,
    function_params,
    function_body);

  if (db_function_definition_lookup(function_identifier->substring, function_params)) {
    return make_db_error("Duplication function: %s", to_cstring(function_identifier->substring));
  }

  function.linkage = LINKAGE_INTERNAL;
  function.identifier = function_identifier->substring;
  function_fill_params(function_params, function.param_types, function.num_params);
  function.retval_type = emit_type_declaration(function_retval);
  function.token = function_definition;
  function.body = function_body;
  function.llvm_function = NULL;
  function.scope = g.db.scope;

  g.db.functions.push_back(function);
  return make_db_success();
}

DeclarationResult db_add_external_function_declaration(Token *function_declaration) {
  FunctionDef function;

  Token *function_retval, *function_identifier, *function_params;
  expand_tokens(
    function_declaration,
    function_retval,
    function_identifier,
    function_params);

  if (db_function_definition_lookup(function_identifier->substring, function_params)) {
    return make_db_error("Duplication function: %s", to_cstring(function_identifier->substring));
  }

  function.linkage = LINKAGE_EXTERNAL;
  function.identifier = function_identifier->substring;
  function_fill_params(function_params, function.param_types, function.num_params);
  function.retval_type = emit_type_declaration(function_retval);
  function.token = function_declaration;
  function.body = NULL;
  function.llvm_function = NULL;
  function.scope = g.db.scope;

  g.db.functions.push_back(function);

  return make_db_success();
}


DeclarationResult db_add_variable_definition(Token *variable_statement, VariableDef *&new_variable) {
  VariableDef var;

  Token *variable_type, *variable_identifier, *variable_value;
  expand_tokens(
    variable_statement,
    variable_type,
    variable_identifier,
    variable_value);

  if (variable_exists(variable_identifier->substring)) {
    return make_db_error("Duplication variable: %s", to_cstring(variable_identifier->substring));
  }

  var.identifier = variable_identifier->substring;
  var.type = emit_type_declaration(variable_type);
  var.initializer_value = variable_value;

  var.llvm_value = NULL;
  var.token = variable_statement;
  var.scope = g.db.scope;

  g.db.variables.push_back(var);
  new_variable = &g.db.variables.back();

  return make_db_success();
}

bool is_success(const DeclarationResult &result) {
  return result.type == DB_SUCCESS;
}

//void db_type_add(SubString &identifier, TypeKind kind, void *type) {
//
//}

DeclarationResult db_add_struct_definition(Token *struct_definition) {
  StructDef struct_def = {};

  Token *struct_identifier, *struct_fields;
  expand_tokens(
    struct_definition,
    struct_identifier,
    struct_fields);

  if (db_type_exists(struct_identifier->substring)) {
    return make_db_error("Duplicate type: %s", to_cstring(struct_identifier->substring));
  }

  struct_def.identifier = struct_identifier->substring;
  DeclarationResult field_result = function_fill_fields(struct_fields, struct_def.fields, struct_def.num_fields);
  if (!is_success(field_result)) return field_result;
  struct_def.token = struct_definition;
  
  db_struct_add(struct_def);
  return make_db_success();
}

DeclarationResult db_add_llvm_type_definition(Token *type_definition) {
  LLVMTypeDef type_def = {};

  Token *type_identifier, *type_body;
  expand_tokens(
    type_definition,
    type_identifier,
    type_body);

  if (db_type_exists(type_identifier->substring)) {
    return make_db_error("Duplicate type: %s", to_cstring(type_identifier->substring));
  }

  type_def.identifier = type_identifier->substring;
  type_def.raw_llvm = type_body;
  type_def.token = type_definition;

  db_llvm_type_add(type_def);
  
  return make_db_success();
}

//FunctionDef *db_unary_operator_lookup(TokenType op, TypeDef *type) {
//}

//FunctionDef *db_binary_operator_lookup(TokenType op, TypeDef *lhs, TypeDef *rhs) {
//}

DeclarationResult db_add_global_declarations(Token *program) {
  // pass 1 do types
  // pass 2 do rest
  // TODO this will fail for types that need fwd declared types

  for(Token *program_statement = program->start; program_statement != NULL; program_statement = program_statement->next) {
    DeclarationResult result;
    switch(program_statement->type) {
    case TOKEN_STRUCT_DEFINITION:
      result = db_add_struct_definition(program_statement);
      break;
    case TOKEN_LLVM_TYPE_DEFINITION:
      result = db_add_llvm_type_definition(program_statement);
      break;
    case TOKEN_FUNCTION_DEFINITION:
    case TOKEN_EXTERNAL_FUNCTION_DECLARATION:
    case TOKEN_VARIABLE_DEFINITION:
    case TOKEN_IMPORT_STATEMENT:
      continue;
    default:
      halt();
    }

    if (!is_success(result)) return result;
  }

  for(Token *program_statement = program->start; program_statement != NULL; program_statement = program_statement->next) {
    DeclarationResult result;
    switch(program_statement->type) {
    case TOKEN_FUNCTION_DEFINITION:
      result = db_add_function_definition(program_statement);
      break;
    case TOKEN_EXTERNAL_FUNCTION_DECLARATION:
      result = db_add_external_function_declaration(program_statement);
      break;
    case TOKEN_VARIABLE_DEFINITION: {
        VariableDef *unused_variable_def;
        result = db_add_variable_definition(program_statement, unused_variable_def);
      } break;
    case TOKEN_STRUCT_DEFINITION:
    case TOKEN_LLVM_TYPE_DEFINITION:
    case TOKEN_IMPORT_STATEMENT:
      continue;
    default:
      halt();
    }

    if (!is_success(result)) return result;
  }

  return make_db_success();
}


//bool db_add_function(Token *token) {
//  Function &function = *new Function;
//  function.function = NULL;
//  function.identifier = token->start->next->substring;
//  function.param_types = token->start->next->next;
//  function.retval = token->start;
//  function.token = token;
//
//  const char *OPERATOR_STR = "operator";
//  int len = strlen(OPERATOR_STR);
//  if (function.identifier.length > len && strncmp(OPERATOR_STR, function.identifier.start, function.identifier.length) == 0) {
//    function.op = operator_identifer_to_op(function.identifier);
//    Function *existing_function = db_operator_lookup(function.op, function.param_types);
//    if (existing_function) { return false; }
//  } else {
//    function.op = TOKEN_NONE;
//    Function *existing_function = db_function_lookup(function.identifier, function.param_types);
//    if (existing_function) { return false; }
//  }
//
//
//  db.functions.push_back(&function);
//  return true;
//}

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

TypecheckResult make_result(TypeDef *type) {
  TypecheckResult  result;
  result.result = TYPECHECK_TYPE;
  result.type = type;
  return result;
}

TypeDef *function_get_retval_type(FunctionDef *function) {
  return function->retval_type;
}

TypecheckResult make_result(FunctionDef *function) {
  if (function == NULL) {
    return make_error("No function for operator %s with types %s %s");
  } else {
    TypeDef *retval_type = function_get_retval_type(function);
    return make_result(retval_type);
  }
}

llvm::Type *llvm_get_type(TypeDef *type);

bool is_boolean(TypeDef *type) {
  if (type->kind != LLVM_TYPE) return false;
  llvm::Type *llvm_type = llvm_get_type(type);

  return llvm_type ->isIntegerTy() &&
    llvm_type->getScalarSizeInBits() == 1;
}
//
//
bool is_integer(TypeDef *type) {
  if (type->kind != LLVM_TYPE) return false;
  llvm::Type *llvm_type = llvm_get_type(type);
  return llvm_type->isIntegerTy();
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

///*
TypeDef *emit_pointer_type(Token *declaration) {
  Token *subtokens[1];
  int num_subtokens = expand_tokens(declaration, subtokens, 1);
  TypeDef *subtype = emit_type_declaration(subtokens[0]);
  return db_get_pointer_type(subtype);
}

TypeDef *emit_array_type(Token *declaration) {
  Token *subtokens[2];
  int num_subtokens = expand_tokens(declaration, subtokens, 2);
  TypeDef *subtype = emit_type_declaration(subtokens[0]);
  int64_t count = emit_compile_time_constant_integer(subtokens[1]);
  return db_get_array_type(subtype, count);
}

TypeDef *emit_function_type(Token *declaration) {
  Token *subtokens[2];
  int num_subtokens = expand_tokens(declaration, subtokens, 2);
  TypeDef *retval_type = emit_type_declaration(subtokens[0]);
  Token *param_list = subtokens[1];
  return db_get_function_type(retval_type, param_list);
}

TypeDef *emit_base_type(Token *type_identifier) {
  TypeDef *retval = db_lookup_type_by_identifier(type_identifier->substring);
  if (retval == NULL) {
    halt();
  }

  return retval;
}

//TypeDef *emit_base_type(Token *type_identifier) {
//  TypeDef *retval = db_lookup_type_by_identifier(type_identifier->substring);
//  if (retval == NULL) {
//    halt();
//  }
//
//  return retval;
//}

TypeDef *emit_type_declaration(Token *declaration) {
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
//*/

TypecheckResult typecheck_operator_call(TokenType op, const TypecheckResult &lhs, const TypecheckResult &rhs) {
  FunctionDef *function = db_binary_operator_lookup(op, lhs.type, rhs.type);
  return make_result(function);
}

TypecheckResult typecheck_operator_call(TokenType op, const TypecheckResult &lhs) {
  FunctionDef *function = db_unary_operator_lookup(op, lhs.type);
  return make_result(function);
}

TypecheckResult typecheck_type_declaration(Token *declaration) {
  // really just gets the type
  TypeDef *type = emit_type_declaration(declaration);
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
  TypeDef *field_type = type_get_field_type(result_lhs.type, field_identifier->substring);
  return make_result(field_type);
}

TypecheckResult typecheck_variable_definition(Token *statement);

TypecheckResult typecheck_function_call_expression(Token *expression){
  Token *subtokens[2];
  int num_subtokens = expand_tokens(expression, subtokens, 2);

  if (num_subtokens == 1) {

  }


  Token *function_identifier = subtokens[0];
  Token *function_call_params = subtokens[1];

  for(Token *param = function_call_params->start; param!= NULL; param = param->next) {
    TypecheckResult result = typecheck_expression(param);
    if (is_success(result) == false) return result;
  }

  FunctionDef *function = db_function_call_lookup(function_identifier->substring, function_call_params);
  if (function == NULL) {
    return make_error("Unknown function");
  }

  return make_result(function_get_retval_type(function));
}

bool is_array(TypeDef *type) {
  return type->kind == ARRAY_TYPE;
}

bool is_compile_time(TypeDef *) {
  // TODO
  return true;
}

bool is_compile_time_integer(TypeDef *type) {
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

  return make_result(result_base.type->array.base_type);
}



TypeDef *db_lookup_integer_type(int numBits) {
  for(auto &type : g.db.types) {
    if (type.kind == LLVM_TYPE && substring_cmp(type.llvm_def.type_definition.identifier, "i32") == true) {
        return &type;
    }
  }

  return NULL;
};

TypecheckResult typecheck_integer_literal(Token *literal){
  //llvm::Type *integer_type = llvm::IntegerType(g.llvm.context, 32);
  TypeDef *type = db_lookup_integer_type(32);
  //Type *type = new Type();
  //type->kind = BASE_TYPE;
  //type->llvm_type = integer_type;
  return make_result(type); //make_result(integer_type);
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
  case TOKEN_INTEGER_LITERAL:
    return typecheck_integer_literal(expression);
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

TypecheckResult typecheck_function_params(Token *param_list) {
  for(Token *param = param_list->start; param != NULL; param = param->next) {
    TypecheckResult result = typecheck_variable_definition(param);
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
  Token *function_return_value = subtokens[0];
  Token *function_identifier = subtokens[1];
  Token *function_param_list = subtokens[2];
  Token *function_body = subtokens[3];

  TypecheckResult result_params = typecheck_function_params(function_param_list);
  if (is_success(result_params) == false) return result_params;

  TypecheckResult result_retval = typecheck_type_declaration(function_return_value);
  TypecheckResult result_body = typecheck_function_body(function_body, result_retval);

  //db_add_function(statement);
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

  return TYPECHECK_SUCCESS;
}

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////

struct Emitter {
};

llvm::StringRef to_string_ref(const SubString &substring) {
  return llvm::StringRef(substring.start, substring.length);
}

//llvm::Type *emit_struct_declaration(LLVM &llvm, Token *struct_def) {
//  Token *subtokens[2];
//  int num_subtokens = expand_tokens(struct_def, subtokens, 2);
//  const Token &identifier = *subtokens[0];
//  const Token &struct_body = *subtokens[1];
//
//  llvm::StringRef name = to_string_ref(identifier.substring);
//  llvm::StructType *type = llvm::StructType::create(*llvm.context, name);
//
//  //DBItem &item = db_add_struct_type(llvm, identifier, type);
//  //item.typeInfo.numFields = struct_body.num_subtokens;
//  //item.typeInfo.fields = db_alloc_fields(llvm, struct_body.num_subtokens);
//  //for(int i = 0; i < struct_body.num_subtokens; ++i) {
//  //  item.typeInfo.fields[i].fieldName = struct_body.subtokens[i].subtokens[1].substring;
//  //}
//
//
//  return type;
//}

static bool isWhitespace(char c) {
  switch(c) {
  case ' ':
  case '\t':
  case '\r':
  case '\n':
    return true;
  default:
    return false;
  }
}

SubString substring_trim(const SubString &substring) {
  SubString retval = substring;

  while(retval.length) {
    if (isWhitespace(retval.start[0])) {
      retval.start++;
      retval.length--;
    } else {
      break;
    }
  }

  while(retval.length) {
    if (isWhitespace(retval.start[retval.length - 1])) {
      retval.length--;
    } else {
      break;
    }
  }

  return retval;
}

llvm::Type *llvm_emit_llvm_type(Token *type_definition) {
  assert(type_definition->type == TOKEN_LLVM_TYPE_DEFINITION);
  // todo vector types...
  assert(type_definition->substring.length > 0);
  assert(type_definition->substring.start != NULL);

  Token *raw_llvm = type_definition->end;
  assert(raw_llvm->type == TOKEN_RAW_LLVM_TYPE);

  SubString trimmed_content = substring_trim(raw_llvm->substring);

  assert(trimmed_content.length > 0);
  assert(trimmed_content.start != NULL);

  switch(trimmed_content.start[0]) {
  case 'u':
  case 'i': {
    SubString digits = trimmed_content;
    digits.start++;
    digits.length--;

    uint32_t size = (uint32_t)substring_to_uint64(digits);
    return llvm::IntegerType::get(*g.llvm.context, size);
            }
  default:
    if (substring_cmp(trimmed_content, "void")) {
      return llvm::Type::getVoidTy(*g.llvm.context);
    } else if (substring_cmp(trimmed_content, "half")) {
      return llvm::Type::getHalfTy(*g.llvm.context);
    } else if (substring_cmp(trimmed_content, "float")) {
      return llvm::Type::getFloatTy(*g.llvm.context);
    } else if (substring_cmp(trimmed_content, "double")) {
      return llvm::Type::getDoubleTy(*g.llvm.context);
    } else if (substring_cmp(trimmed_content, "fp128")) {
      return llvm::Type::getFP128Ty(*g.llvm.context);
    } else {
      halt();
    }
    break;
  }

  return NULL;
}

//llvm::Type *llvm_get_type(Type *type);

llvm::Type *llvm_emit_struct_field(Token *field) {
  Token *subtokens[3];
  int num_subtokens = expand_tokens(field, subtokens, 3);
  Token *type_declaration = subtokens[0];
  Token *identifier = subtokens[1];
  Token *default_value = subtokens[2];

  TypeDef *type = emit_type_declaration(type_declaration);
  return llvm_get_type(type);
}
//
//llvm::Type *llvm_emit_typedef_type(Token *type_definition) {
//  Type *type = emit_type_declaration(type_definition);
//  return llvm_get_type(type);
//}

static llvm::ArrayRef<llvm::Value *> to_array_ref(llvm::Value **values, int num_values) {
  return llvm::ArrayRef<llvm::Value *>(values, num_values);
}

static llvm::ArrayRef<llvm::Type *> to_array_ref(llvm::Type **types, int num_types) {
  return llvm::ArrayRef<llvm::Type *>(types, num_types);
}

const int IS_PACKED = true;


llvm::Type *llvm_emit_struct_type(Token *type_definition) {
  assert(type_definition->type == TOKEN_STRUCT_DEFINITION);
  Token *subtokens[2];
  int num_subtokens = expand_tokens(type_definition, subtokens, 2);
  Token *identifier = subtokens[0];
  Token *struct_fields = subtokens[1];

  llvm::StringRef name = to_string_ref(identifier->substring);
  llvm::StructType *type = llvm::StructType::create(*g.llvm.context, name);

  llvm::Type *fields[MAX_PARAMS];
  //for(int i = 0; i < struct_body.num_subtokens; ++i) {
  int num_fields = 0;
  for(Token *field = struct_fields->start->start; field != NULL; field = field->next) {
    llvm::Type *field_type = llvm_emit_struct_field(field);
    fields[num_fields++] = field_type;
  }

  llvm::ArrayRef<llvm::Type *> ref_fields = to_array_ref(fields, num_fields);
  type->setBody(ref_fields, IS_PACKED); 

  //llvm::Type *type = llvm::StructType::get(*llvm.context, ref_fields, IS_PACKED);
  return type;
}



llvm::Type *llvm_emit_base_type(TypeDef *type) {
  assert(type->llvm_type == NULL);
  //assert(type->kind == BASE_TYPE);

  switch(type->kind) {
  case LLVM_TYPE:
    return llvm_emit_llvm_type(type->token);
  //case TOKEN_TYPEDEF_DEFINITION:
  //  return llvm_emit_typedef_type(type->base.type_definition);
  case STRUCT_TYPE:
    return llvm_emit_struct_type(type->token);
  //case TOKEN_ENUM_DEFINITION:
  //  return llvm_emit_enum_type(type->base.type_definition);
    break;
  default:
    halt();
  }

  return NULL;
}

llvm::Type *llvm_emit_pointer_type(TypeDef *pointer_type) {
  //assert(type_token->type == TOKEN_POINTER_TYPE);
  //Token *subtokens[1];
  //int num_subtokens = expand_tokens(type_token, subtokens, 1);
  assert(pointer_type->kind == POINTER_TYPE);
  //Token *base_type_declaration = ;
  //Type *base_type = emit_type_declaration(base_type_declaration);
  llvm::Type *base_llvm_type = llvm_get_type(pointer_type->pointer.base_type);
  return llvm::PointerType::get(base_llvm_type, 0);
}

llvm::Type *llvm_emit_array_type(TypeDef *array_type) {
  //assert(type_token.token == TOKEN_COMPOUND_ARRAY_TYPE);
  //assert(type_token.num_subtokens == 2);
  //const GGToken &base_type_token = type_token.subtokens[0];
  //const GGToken &array_size = type_token.subtokens[1];
  assert(array_type->kind == ARRAY_TYPE);

  llvm::Type *base_type = llvm_get_type(array_type->array.base_type);
  //uint64_t num_elements = eval_constexpr(array_size);
  return llvm::ArrayType::get(base_type, array_type->array.count);
}

//llvm::Type *emit_param_type(Token *param) {
//  assert(param->type == TOKEN_FUNCTION_PARAM);
//  Token *subtokens[3];
//  int num_subtokens = expand_tokens(param, subtokens, 3);
//  Token *type_declaration = subtokens[0];
//  Token *identifier = subtokens[1];
//  Token *default_value = subtokens[2];
//  Type *type = emit_type_declaration(type_declaration);
//  return llvm_get_type(type);
//}
//

int get_function_type_param_llvm_types(Token *param_list, llvm::Type **types, int max_params) {
  int num_params = 0;
  for(Token *param = param_list->start; param != NULL; param = param->next) {
    TypeDef *type_def = emit_type_declaration(param->start);
    types[num_params++] = llvm_get_type(type_def);
  }
  return num_params;
}

int get_function_param_llvm_types(FunctionDef *function, llvm::Type **types, int max_params) {
  //int num_params = 0;
  //for(Token *token = param_list->start; token != NULL; token = token->next) {
  assert(max_params < function->num_params);
  for(int i = 0; i < function->num_params; ++i) {
    types[i] = llvm_get_type(function->param_types[i]);
  }
  return function->num_params;
}
//
enum {
  FIXED_ARGS = false,
  VAR_ARGS = true,
};
//
llvm::Type *llvm_emit_function_type(TypeDef *function_type) {
  //assert(type_token.token == TOKEN_COMPOUND_FUNCTION_TYPE);
  //assert(type_token.num_subtokens == 2);
  //const GGToken &return_type_token = type_token.subtokens[0];
  //const GGToken &param_type_tokens = type_token.subtokens[1];
  assert(function_type->kind == FUNCTION_TYPE);

  llvm::Type *retval_type = llvm_get_type(function_type->function.retval_type);

  llvm::Type *param_types[MAX_PARAMS];
  int num_params = get_function_type_param_llvm_types(function_type->function.param_list, param_types, MAX_PARAMS);
  llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, num_params);
  return llvm::FunctionType::get(retval_type, args, FIXED_ARGS);
}


llvm::Type *llvm_emit_type(TypeDef *type) {
  assert(type->llvm_type == NULL);

  llvm::Type *llvm_type;
  switch(type->kind) {
  case STRUCT_TYPE:
  case LLVM_TYPE:
    llvm_type = llvm_emit_base_type(type);
    break;
  case POINTER_TYPE:
    llvm_type = llvm_emit_pointer_type(type);
    break;
  case ARRAY_TYPE:
    llvm_type = llvm_emit_array_type(type);
    break;
  case FUNCTION_TYPE:
    llvm_type = llvm_emit_function_type(type);
    break;
  default:
    halt();
  }
  //Token *type_definition = type->type_definition;
  //llvm::Type *llvm_type = llvm_emit_type_defintiion(type_definition);
  type->llvm_type = llvm_type;
  return llvm_type;
}
//
llvm::Type *llvm_get_type(TypeDef *type) {
  if (type->llvm_type == NULL) {
    return llvm_emit_type(type);
  } else {
    return type->llvm_type;
  }
}

//llvm::Type *llvm_get_type(Token *type_declaration) {
//
//  if (type->llvm_type == NULL) {
//    return type->llvm_type;
//  } else {
//    return llvm_emit_type(type);
//  }
//}


//llvm::Type *llvm_get_type(Token *type_declaration) {
//  switch(type_declaration->type) {
//  case TOKEN_IDENTIFIER:
//    return llvm_get_type_identifier(llvm, type_declaration);
//  case TOKEN_RAW_LLVM_TYPE:
//    return llvm_get_llvm_type(llvm, type_declaration);
//  case TOKEN_POINTER_TYPE:
//    return llvm_get_pointer_type(llvm, type_declaration);
//  case TOKEN_ARRAY_TYPE:
//    return llvm_get_array_type(llvm, type_declaration);
//  case TOKEN_FUNCTION_TYPE:
//    return llvm_get_function_type(llvm, type_declaration);
//  default:
//    halt();
//  }
//  assert(type_declaration->token == TOKEN_TYPE_IDENTIFIER);
//  Type *type = db_lookup_type(type_declaration->substring);
//  assert(type);
//
//  if (type->llvm_type) {
//    return type->llvm_type;
//  } else {
//    return llvm_emit_type(type);
//  }
//}

//llvm::Type* llvm_get_type(LLVM &llvm, const Token *type_declaration) {
//  switch(type_declaration->type) {
//  case TOKEN_IDENTIFIER:
//    return llvm_get_type_identifier(llvm, type_declaration);
//  case TOKEN_RAW_LLVM_TYPE:
//    return llvm_get_llvm_type(llvm, type_declaration);
//  case TOKEN_POINTER_TYPE:
//    return llvm_get_pointer_type(llvm, type_declaration);
//  case TOKEN_ARRAY_TYPE:
//    return llvm_get_array_type(llvm, type_declaration);
//  case TOKEN_FUNCTION_TYPE:
//    return llvm_get_function_type(llvm, type_declaration);
//  default:
//    halt();
//  }
//
//  return NULL;
//}

//llvm::Type *llvm_emit_llvm_type_definition(LLVM &llvm, Token *type_def) {
//  Token *subtokens[2];
//  int num_subtokens = expand_tokens(type_def, subtokens, 2);
//  const Token &identifier = *subtokens[0];
//  const Token &type_decl = *subtokens[1];
//
//  llvm::Type *type = llvm_get_type(llvm, type_decl);
//  db_set_llvm_type(llvm, identifier, type);
//  return type;
//}
//
//void emit_global_type_definitions(LLVM &llvm, Token *program) {
//  for(Token *program_statement = program->start; program_statement != NULL; program_statement = program_statement->next) {
//    switch(program_statement->type) {
//    case TOKEN_STRUCT_DEFINITION:
//      emit_struct_definition(llvm, program_statement);
//      break;
//    case TOKEN_LLVM_TYPE_DEFINITION:
//      emit_llvm_type_definition(llvm, program_statement);
//      break;
//    case TOKEN_TYPEDEF_DEFINITION:
//      emit_typedef_definition(llvm, program_statement);
//      break;
//    case TOKEN_IMPORT_STATEMENT:
//    case TOKEN_VARIABLE_DEFINITION:
//    case TOKEN_FUNCTION_DEFINITION:
//    case TOKEN_EXTERNAL_FUNCTION_DECLARATION:
//      break;
//    default:
//      halt();
//    }
//  }
//}

//void emit_struct_definition(Token *type_definition) {
//  assert(type_definition->type == TOKEN_STRUCT_DEFINITION);
//  // ?? do i need to do something here
//  db_add_type_definition(type_definition);
//}

//void *db_lookup_any(const SubString &substring) {
//  for(auto *variable : g.db.variables) {
//    if (substring_cmp(variable->identifier, substring)) return variable;
//  }
//
//  return NULL;
//}
//
//Value *db_lookup_variable(const SubString &substring) {
//  for(auto *variable : g.db.variables) {
//    if (substring_cmp(variable->identifier, substring)) return variable;
//  }
//
//  return NULL;
//}

//Function *db_lookup_function_call(const SubString &substring, Token *param_list) {
//  for(auto *function : g.db.functions) {
//    if (substring_cmp(function->identifier, substring)) {
//      // TODO compare params;
//      return function;
//    }
//  }
//
//  return NULL;
//}

//Function *db_lookup_function_call(const SubString &substring, Value *param_list[], int count) {
//  for(auto *function : g.db.functions) {
//    if (substring_cmp(function->identifier, substring)) {
//      // TODO compare params;
//      return function;
//    }
//  }
//
//  return NULL;
//}

//EmitResult db_add_variable(const SubString &identifier, Value *value) {
//  void *existing = db_lookup_any(identifier);
//  if (existing != NULL) return make_emit_error("Variable already defined");
//  //value->scope = g.db.db_scope;
//  g.db.variables.push_back(value);
//
//  return EMIT_SUCCESS;
//}

llvm::Value *CreateAlloca(FunctionDef *function, const SubString &identifier, TypeDef *type) {
  llvm::StringRef name = to_string_ref(identifier);
  //GetType *type = emit_type_declaration(token_type);
  //llvm::Type *llvm_type = type->llvm_type;
  llvm::Value *llvm_value = g.llvm.builder->CreateAlloca(type->llvm_type, 0, name);
  //Value *value = new Value;
  //value->type = type;
  //value->llvm_value = llvm_value;
  return llvm_value;
}

void CreateStore(llvm::Value *val, llvm::Value *address) //llvm.builder->CreateStore(retval, lvalue);
{
  g.llvm.builder->CreateStore(val, address);
}

//static llvm::Value *CreateAllocaZero(LLVM &llvm, llvm::Function *TheFunction, const GGToken &identifier, const GGToken &type) {
//static Value *CreateAllocaZero(Function *function, Token *token_identifier, Token *token_type) {
//  Value *value = CreateAlloca(function, token_identifier, token_type);
//  llvm::Constant *zero = llvm::Constant::getNullValue(value->type->llvm_type);
//  g.llvm.builder->CreateStore(zero, value->llvm_value);
//  return value;
//}

static const bool SIGNED = true;

llvm::Value *emit_one(/*TypeDef *type*/) {
  return llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 1, SIGNED);
}

llvm::Value *emit_zero() {
  return llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 0, SIGNED);
}

llvm::Value *emit_rvalue_expression(Token *expression);

llvm::Value *emit_lvalue_unary_op(Token *token) {
  Token *subtokens[2];
  expand_tokens(token, subtokens, 2);
  Token *op_token = subtokens[0];
  Token *lhs_token = subtokens[1];

  switch(op_token->type) {
  case TOKEN_DEREF_OP: {
    llvm::Value *lhsVal = emit_rvalue_expression(lhs_token);
    return lhsVal;
  }
  //case '&':
  //case '+':
  //case '-':
  default:
    halt();
  }

  return NULL;
}

llvm::Value *emit_lvalue_expression(Token *expression);

llvm::Value *emit_lvalue_array_dereference(Token *token) {
  Token *subtokens[2];
  expand_tokens(token, subtokens, 2);
  Token *array_pointer = subtokens[0];
  Token *index_expr = subtokens[1];
  //assert(token.num_subtokens == 2);
  //const GGToken &array_pointer = token.subtokens[0];
  //const GGToken &index_expr = token.subtokens[1];

  llvm::Value *lhsVal = emit_lvalue_expression(array_pointer);
  assert(lhsVal);

  llvm::Value *indexVal = emit_rvalue_expression(index_expr);
  assert(indexVal);

  //return llvm.builder->CreateGEP(lhsVal, indexVal);

  llvm::Value *zero = emit_zero();
  llvm::Value *idxs[] = { zero, indexVal };
  int num_idxs = 2; //ARRAYSIZE(idxs);
  llvm::ArrayRef<llvm::Value *> array_ref = to_array_ref(idxs, num_idxs);
  llvm::Value *arrayElement = g.llvm.builder->CreateGEP(lhsVal, array_ref);

  return arrayElement;
}

int field_idx_lookup(TypeDef *type, const SubString &identifier) {
  assert(type->kind == STRUCT_TYPE);
  StructDef *struct_def = type->struct_def.type_definition;
  for(int i = 0; i < struct_def->num_fields; ++i) {
    FieldDef *field = &struct_def->fields[i];
    if (substring_cmp(field->identifier, identifier))
      return field->index;
  }

  halt();
  return -1;
}

llvm::Value *field_index(TypeDef *type, Token *field_identifier) {
  int field_idx = field_idx_lookup(type, field_identifier->substring);
  return llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), field_idx, SIGNED);
}

llvm::Value *emit_lvalue_member_identifier(Token *token) {
  Token *subtokens[2];
  expand_tokens(token, subtokens, 2);
  Token *basePointerExpr = subtokens[0];
  Token *fieldIdentifier = subtokens[1];
  //assert(token.num_subtokens == 2);
  //const GGToken &basePointerExpr = token.subtokens[0];
  //const GGToken &fieldIdentifier = token.subtokens[1];

  llvm::Value *basePointer = emit_lvalue_expression(basePointerExpr);
  assert(basePointer);

  TypeDef *basePointerType = typecheck_expression(basePointerExpr).type;
  //assert(basePointerType->isPointerTy());
  //llvm::Type *baseValueType = basePointerType->getContainedType(0);

  llvm::Value *fieldIndex = field_index(basePointerType, fieldIdentifier);
  llvm::Value *zero = llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 0, SIGNED);

  llvm::Value *idxs[] = {zero, fieldIndex};
  const int num_idxs = 2;

  llvm::Value *fieldPointer = g.llvm.builder->CreateGEP(basePointer, to_array_ref(idxs, num_idxs));

  return fieldPointer;
}

VariableDef *db_lookup_variable(SubString &identifier) {
  for(auto &varaible : g.db.variables) {
    if (substring_cmp(varaible.identifier, identifier) == 0) {
      return &varaible;
    }
  }

  return NULL;
}

llvm::Value *emit_lvalue_identifier(Token *token) {
  assert(token->start == NULL);
  //assert(token.num_subtokens == 0);
  return db_lookup_variable(token->substring)->llvm_value;
}

llvm::Value *emit_lvalue_expression(Token *expression) {
  switch(expression->type) {
  case TOKEN_UNARY_EXPRESSION:
    return emit_lvalue_unary_op(expression);
  case TOKEN_OP_ARRAY_INDEX:
    return emit_lvalue_array_dereference(expression);
  case TOKEN_OP_MEMBER:
    return emit_lvalue_member_identifier(expression);
  case TOKEN_IDENTIFIER:
    return emit_lvalue_identifier(expression);
  //case TOKEN_COMPOUND_BINARY_OPERATION:
  //case TOKEN_COMPOUND_UNARY_POST_OPERATION:
  //case TOKEN_COMPOUND_FUNCTION_CALL:
  //case TOKEN_LITERAL_INTEGER:
  default:
    halt();
  }

  return NULL;
}

llvm::Value *CreateAdd(llvm::Value *rhs_value, llvm::Value *lhs_value) {
  return g.llvm.builder->CreateAdd(rhs_value, lhs_value);
}

llvm::Value *CreateSub(llvm::Value *rhs_value, llvm::Value *lhs_value) {
  return g.llvm.builder->CreateSub(rhs_value, lhs_value);
}

llvm::Value *CreateMul(llvm::Value *rhs_value, llvm::Value *lhs_value) {
  return g.llvm.builder->CreateMul(rhs_value, lhs_value);
}

llvm::Value *CreateSDiv(llvm::Value *rhs_value, llvm::Value *lhs_value) {
  return g.llvm.builder->CreateSDiv(rhs_value, lhs_value);
}

llvm::Value *CreateSRem(llvm::Value *rhs_value, llvm::Value *lhs_value) {
  return g.llvm.builder->CreateSRem(rhs_value, lhs_value);
}

llvm::Value *CreateLoad(llvm::Value *rhs_value) {
  return g.llvm.builder->CreateLoad(rhs_value);
}

llvm::Value *emit_rvalue_unary_op(Token *token) {
  Token *subtokens[2];
  expand_tokens(token, subtokens, 2);
  Token *op_token = subtokens[0];
  Token *lhs_token = subtokens[1];

  //assert(token.num_subtokens == 2);
  //const GGToken &op_token = token.subtokens[0];
  //const GGToken &lhsToken = token.subtokens[1];
  
  switch(op_token->type) {
  case TOKEN_PRE_INC_OP: {
      llvm::Value *lvalue = emit_lvalue_expression(lhs_token);
      llvm::Value *rvalue = emit_rvalue_expression(lhs_token);
      llvm::Value *one    = emit_one(); // llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 1, SIGNED);
      llvm::Value *retval = CreateAdd(rvalue, one); // llvm.builder->CreateAdd(rvalue, one);
      CreateStore(retval, lvalue); //llvm.builder->CreateStore(retval, lvalue);
      return retval;
  }
  case TOKEN_POSITIVE_OP: {
      return emit_rvalue_expression(lhs_token);
  }
  case TOKEN_PRE_DEC_OP: {
      llvm::Value *lvalue = emit_lvalue_expression(lhs_token);
      llvm::Value *rvalue = emit_rvalue_expression(lhs_token);
      llvm::Value *one   = emit_one(); //llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 1, SIGNED);
      llvm::Value *retval = CreateSub(rvalue, one); //llvm.builder->CreateSub(rvalue, one);
      CreateStore(retval, lvalue); //llvm.builder->CreateStore(retval, lvalue);
      return retval;
    } 
  case TOKEN_NEGATIVE_OP: {
      llvm::Value *value = emit_rvalue_expression(lhs_token);
      llvm::Value *zero = emit_zero(); //llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 0, SIGNED);
      return CreateSub(zero, value);
      //return llvm.builder->CreateSub(zero, value);
    }
  case TOKEN_ADDRESS_OP: {
      return emit_lvalue_expression(lhs_token);
    }
  case TOKEN_DEREF_OP: {
      llvm::Value *address = emit_rvalue_expression(lhs_token);
      llvm::Value *value = CreateLoad(address); //llvm.builder->CreateLoad(address);
      return value;
    }
  default:
    halt();
  }

  return NULL;
}

llvm::Value *emit_rvalue_binary_op(Token *token) {
  Token *subtokens[3];
  expand_tokens(token, subtokens, 3);
  Token *lhs_token = subtokens[0];
  Token *op_token = subtokens[1];
  Token *rhs_token = subtokens[2];
  //assert(token.num_subtokens == 3);
  //const GGToken &lhsToken = token.subtokens[0];
  //const GGToken &op_token = token.subtokens[1];
  //const GGToken &rhsToken = token.subtokens[2];

  llvm::Value *lhs = emit_rvalue_expression(lhs_token);
  llvm::Value *rhs = emit_rvalue_expression(rhs_token);

  TypeDef *lhs_type = typecheck_expression(lhs_token).type;
  TypeDef *rhs_type = typecheck_expression(rhs_token).type;

  //GGSubString function_identifier = op_token.substring;
  //switch(*op_token.substring.start) {
  //case '+': {
  //  GGToken replacementToken;
  //  token.substring.start = "op_++";
  //  token.substring.length
  //  return llvm.builder->CreateAdd(lhs, rhs, "addtmp");
  //          }
  //case '-': {
  //  return llvm.builder->CreateSub(lhs, rhs, "subtmp");
  //          }
  //case '*': {
  //  return llvm.builder->CreateMul(lhs, rhs, "multmp");
  //          }
  //case '/': {
  //  return llvm.builder->CreateSDiv(lhs, rhs, "divtmp");
  //          }
  //case '%': {
  //  return llvm.builder->CreateSRem(lhs, rhs, "remtmp");
  //          }
  //          //case OP_BINARY_AND:
  //          //case OP_BINARY_OR:
  //          //case OP_BINARY_XOR:
  //          //case OP_LOGICAL_AND:
  //          //case OP_LOGICAL_OR:
  //          //case OP_COMPARE_EQUAL:
  //          //case OP_COMPARE_NOT_EQUAL:
  //          //case OP_COMPARE_LESS_THAN:
  //          //case OP_COMPARE_LESS_THAN_EQUAL:
  //          //case OP_COMPARE_GREATER_THAN:
  //          //case OP_COMPARE_GREATER_THAN_EQUAL:
  //default:
  //  halt();
  //}

  //Value *param_expressions[2];
  //param_expressions[0] = lhs;
  //param_expressions[1] = rhs;

  //llvm::StringRef name = to_string_ref(function_identifier);
  FunctionDef *callee = db_binary_operator_lookup(op_token->type, lhs_type, rhs_type);

  llvm::Value *llvm_param_expressions[2];
  llvm_param_expressions[0] = lhs;
  llvm_param_expressions[1] = rhs;
  llvm::ArrayRef<llvm::Value *> ref_params = to_array_ref(llvm_param_expressions, 2);
  llvm::Value *result = g.llvm.builder->CreateCall(callee->llvm_function, ref_params);
  return result;
}

llvm::Value *emit_rvalue_array_dereference(Token *array_dereference) {
  llvm::Value *lvalue = emit_lvalue_array_dereference(array_dereference);
  llvm::Value *rvalue = CreateLoad(lvalue);
  return rvalue;
}

llvm::Value *emit_rvalue_unary_post_op(Token *token, TokenType opType) {
  Token *subtokens[2];
  expand_tokens(token, subtokens, 2);
  Token *value_expr = subtokens[0];
  Token *op_expr = subtokens[1];
  //assert(token.num_subtokens == 2);
  //const GGToken &value_expr = token.subtokens[0];
  //const GGToken &op_expr= token.subtokens[1];

  llvm::Value *lvalue = emit_lvalue_expression(value_expr);
  llvm::Value *const1 = emit_one();
  llvm::Value *value = CreateLoad(lvalue);

  llvm::Value *newValue;
  switch(opType) {
  case TOKEN_OP_POSTFIX_INC:
    newValue = CreateAdd(value, const1);
    break;
  case TOKEN_OP_POSTFIX_DEC:
    newValue = CreateSub(value, const1);
    break;
  default:
    halt();
  }

  CreateStore(newValue, lvalue);
  return value;
}

llvm::Value *emit_rvalue_member_identifier(Token *member_identifier) {
  llvm::Value *lvalue = emit_lvalue_member_identifier(member_identifier);
  llvm::Value *rvalue = CreateLoad(lvalue);
  return rvalue;
}

int emit_function_call_params(Token *params, llvm::Value **values, int maxParams)
{
  //assert(params.num_subtokens <= maxParams);
  int i = 0;
  //for(int i = 0; i< params.num_subtokens; ++i) {
  for(Token *param = params->start; param != NULL; param = param->next) {
    assert(i < maxParams);
    llvm::Value *value = emit_rvalue_expression(param);
    values[i] = value;
    i++;
  }

  return i;
}

llvm::Value *emit_rvalue_function_call(Token *function_call) {
  Token *subtokens[2];
  expand_tokens(function_call, subtokens, 2);
  Token *function_identifier = subtokens[0];
  Token *function_params = subtokens[1];

  //llvm::Function *callee = llvm.module->getFunction(name);
  FunctionDef *callee = db_function_call_lookup(function_identifier->substring, function_params);
  llvm::StringRef name = to_string_ref(function_identifier->substring);

  llvm::Value *retval = NULL;
  if (function_params->start == NULL)
  {
    retval = g.llvm.builder->CreateCall(callee->llvm_function);
  }
  else
  {
    llvm::Value *param_expressions[MAX_PARAMS];
    int num_params = emit_function_call_params(function_params, param_expressions, MAX_PARAMS);

    llvm::ArrayRef<llvm::Value *> ref_params = to_array_ref(param_expressions, num_params);
    retval = g.llvm.builder->CreateCall(callee->llvm_function, ref_params);
    //assert(retval);
  }

  return retval;
}

llvm::Value *emit_rvalue_integer_literal(Token *integer_literal) {
  //assert(integer_literal.token == TOKEN_LITERAL_INTEGER);
  int num_bits = 32; //get_integer_type_num_bits(integer_literal);

  llvm::IntegerType *type = llvm::IntegerType::get(*g.llvm.context, num_bits);
  llvm::StringRef str = to_string_ref(integer_literal->substring);

  const int RADIX = 10;
  llvm::Value *retval = llvm::ConstantInt::get(type, str, RADIX);
  return retval;
}

llvm::Value *emit_rvalue_float_literal(Token *float_literal) {
  //assert(float_literal.token == TOKEN_LITERAL_FLOAT);
  llvm::Type *type = llvm::Type::getFloatTy(*g.llvm.context);
  llvm::StringRef str = to_string_ref(float_literal->substring);
  llvm::Value *retval = llvm::ConstantFP::get(type, str);
  return retval;
}

llvm::Value *emit_rvalue_string_literal(Token *string_literal) {
  //assert(string_literal.token == TOKEN_LITERAL_STRING);

  //llvm::Type *type = get_type(llvm, type_token);
  //llvm::StringRef name = to_string_ref(identifier.substring);

  //const int MAX_STRING_LENGTH = 1024;
  //assert(MAX_STRING_LENGTH - 1 > string_literal.substring.length);
  //char data[MAX_STRING_LENGTH];
  //int lenWithNull = string_literal_to_char_data(data, string_literal.substring);

  //llvm::Type *i8Type = llvm::IntegerType::get(*llvm.context, 8);
  //llvm::ArrayType *i8ArrayType = llvm::ArrayType::get(i8Type, string_literal.substring.length + 1);

  //GGSubString str_data;
  //str_data.start = data;
  //str_data.length = lenWithNull - 1;
  //llvm::ArrayRef<uint8_t> elts = to_array_ref(data, lenWithNull);
  //llvm::Constant *initializer = llvm::ConstantDataArray::getString(*llvm.context, to_string_ref(str_data), false);

  //llvm::Value *global_string = llvm.builder->CreateGlobalString(to_string_ref(str_data));
  //llvm::Value *

  llvm::StringRef stringRef(string_literal->substring.start, string_literal->substring.length);
  llvm::Value *array_value = g.llvm.builder->CreateGlobalString(stringRef);
  llvm::Value *zero = llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 0, SIGNED);
  llvm::Value *zeros[] = {zero, zero};
  llvm::Value *i8pointer = g.llvm.builder->CreateGEP(array_value, to_array_ref(zeros, 2));

  //llvm::Value *lvalue = emit_lvalue_identifier(llvm, identifier);
  //llvm::Value *rvalue = llvm.builder->CreateLoad(i8pointer);

  return i8pointer;

  //llvm::ConstantArray::get(i8ArrayType, data);
  //llvm::Value *value = new llvm::VariableDef(*llvm.module, i8ArrayType, true, llvm::VariableDef::LinkOnceAnyLinkage, initializer);

  //llvm.module->getOrInsertGlobal(
  //  
  //  ) GlobalList.push_back(value);
  //db_add_variable(llvm, identifier, value);

  //int num_bits = get_integer_type_num_bits(integer_literal);

  //llvm::IntegerType *type = llvm::IntegerType::get(*llvm.context, num_bits);
  //llvm::StringRef str = to_string_ref(integer_literal.substring);

  //const int RADIX = 10;
  //llvm::Value *retval = llvm::ConstantInt::get(type, str, RADIX);
  //return retval;
  //return value;
}

llvm::Value *emit_rvalue_identifier(Token *identifier) {
  llvm::Value *lvalue = emit_lvalue_identifier(identifier);
  llvm::Value *rvalue = CreateLoad(lvalue);
  return rvalue;
}

llvm::Value *emit_rvalue_expression(Token *expression) {
  switch(expression->type) {
  case TOKEN_UNARY_EXPRESSION:
    return emit_rvalue_unary_op(expression);
  case TOKEN_BINARY_EXPRESSION:
    return emit_rvalue_binary_op(expression);
  case TOKEN_OP_ARRAY_INDEX:
    return emit_rvalue_array_dereference(expression);
  case TOKEN_OP_POSTFIX_INC:
    return emit_rvalue_unary_post_op(expression, TOKEN_OP_POSTFIX_INC);
  case TOKEN_OP_POSTFIX_DEC:
    return emit_rvalue_unary_post_op(expression, TOKEN_OP_POSTFIX_DEC);
  case TOKEN_OP_MEMBER:
    return emit_rvalue_member_identifier(expression);
  case TOKEN_OP_FUNCTION_CALL:
    return emit_rvalue_function_call(expression);
  case TOKEN_INTEGER_LITERAL:
    return emit_rvalue_integer_literal(expression);
  case TOKEN_FLOAT_LITERAL:
    return emit_rvalue_float_literal(expression);
  case TOKEN_STRING_LITERAL:
    return emit_rvalue_string_literal(expression);
  case TOKEN_IDENTIFIER:
    return emit_rvalue_identifier(expression);

    //case EXPRESSION_BINARY_OP;
    //case EXPRESSION_UNARY_OP;
    //case EXPRESSION_FUNCTION_CALL;
    //case TOKEN_COMPOUND_LITERAL_INTEGER:
    //
    //	return emit_variable(llvm, expression);
    //case EXPRESSION_STRING_LITERAL;
    //case EXPRESSION_FLOAT_LITERAL;
  default:
    halt();
  }

  return NULL;
}

llvm::Value *emit_zero_fill(FunctionDef *function, TypeDef *type) {
  llvm::Constant *zero = llvm::Constant::getNullValue(type->llvm_type);
  return zero;
}

//llvm::Value *emit_rvalue_expression(FunctionDef *function, Token *expression) {
//
//}

EmitResult emit_variable_initalization(FunctionDef *function, VariableDef *variable) {
  llvm::Value *value_variable = CreateAlloca(function, variable->identifier, variable->type);

  llvm::Value *value_initaizlier;
  if (variable->initializer_value == NULL) {
    // TODO for non-zero defaults
    value_initaizlier = emit_zero_fill(function, variable->type);
  } else {
    value_initaizlier = emit_rvalue_expression(variable->initializer_value);
  }

  CreateStore(value_initaizlier, value_variable);
  return EMIT_SUCCESS; //make_success(value_variable);
}

EmitResult emit_local_variable_definition(FunctionDef *function, Token *program_statement) {
  assert(program_statement->type == TOKEN_VARIABLE_DEFINITION);
  VariableDef *variable;
  DeclarationResult result = db_add_variable_definition(program_statement, variable);
  return emit_variable_initalization(function, variable);
}
//
//
//
//  Token *subtokens[3];
//  int num_subtokens = expand_tokens(program_statement, subtokens, 3);
//  Token *type_decl = subtokens[0];
//  Token *identifier = subtokens[1];
//  Token *initalizer_value = subtokens[2];
//
//  if (variable_def_exists())
//
//  Type *type = emit_type_declaration(type_decl);
//  //llvm::Value* value = emit_value(initalizer_value);
//
//  //Type *typedec = emit_type_declaration(type_decl);
//  //llvm::Type *type = llvm_get_type(typedec);
//
//  //llvm::StringRef name = to_string_ref(identifier->substring);
//  llvm::Value *value;
//  if (initalizer_value)
//  {
//    // TODO
//    value = CreateAllocaZero(function, identifier, type_decl);
//  }
//  else 
//  {
//    llvm::Value *= emit_value(function, initalizer_value);
//    //value = CreateAlloca(function, identifier, type_decl);
//    //Value *initial_value = emit_rvalue_expression(initalizer_value);
//    //CreateStore(initial_value, value);
//  }
//
//  VariableDef var;
//  var.
//
//  return db_add_variable(identifier->substring, value);
//}

EmitResult emit_global_variable_initialization(Token *program_statement) {
  //assert(program_statement->type == TOKEN_VARIABLE_DEFINITION);
  //VariableDef *variable;
  //DeclarationResult result = db_add_variable_definition(program_statement, variable);
  //return emit_variable_initalization(function, variable);


  assert(program_statement->type == TOKEN_VARIABLE_DEFINITION);
  Token *subtokens[3];
  int num_subtokens = expand_tokens(program_statement, subtokens, 3);
  Token *type_decl = subtokens[0];
  Token *identifier = subtokens[1];
  Token *initalizer_value = subtokens[2];

  VariableDef *variable = db_lookup_variable(identifier->substring);
  FunctionDef *function = db_lookup_premain_initizliation_function();
  return emit_variable_initalization(function, variable);


  //TypeDef *type = emit_type_declaration(type_decl);

  //llvm::Constant *constant;
  //if (initalizer_value)
  //{
  //  // TODO
  //  constant = llvm::Constant::getNullValue(type);

  //  //uint64_t val64 = 0;
  //  //val64 = eval_constexpr(value);
  //  //constant = llvm::ConstantInt::get(type, val64);
  //}
  //else 
  //{
  //  constant = llvm::Constant::getNullValue(type);
  //}

  //llvm::StringRef name = to_string_ref(identifier->substring);
  //llvm::Value *llvm_value = new llvm::VariableDef(*g.llvm.module, type, false, llvm::VariableDef::ExternalLinkage, constant, name);

  //Value *value = new Value;
  //value->type = typedec;
  //value->llvm_value = llvm_value;

  //return db_add_variable(identifier->substring, value);
}

void emit_assignment_statement(Token *assigment) {
  Token *subtokens[3];
  int num_subtokens = expand_tokens(assigment, subtokens, 3);
  Token *lhs_expr = subtokens[0];
  Token *assigment_op = subtokens[1];
  Token *rhs_expr = subtokens[2];
  //assert(assigment.token == TOKEN_COMPOUND_ASSIGNMENT_STATEMENT);
  //assert(assigment.num_subtokens == 3);
  //const GGToken &lhs_expr = assigment.subtokens[0];
  //const GGToken &assigment_op = assigment.subtokens[1];
  //const GGToken &rhs_expr = assigment.subtokens[2];

  llvm::Value *lhs = emit_lvalue_expression(lhs_expr);
  llvm::Value *rhs = emit_rvalue_expression(rhs_expr);

  switch(assigment_op->type) {
  case TOKEN_ASSIGNMENT_OP: {
    CreateStore(rhs, lhs);
  } break;
  case TOKEN_ADD_ASSIGNMENT_OP: {
    llvm::Value *r_lhs = emit_rvalue_expression(lhs_expr);
    llvm::Value *newVal = CreateAdd(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  case TOKEN_SUB_ASSIGNMENT_OP: {
    llvm::Value *r_lhs = emit_rvalue_expression(lhs_expr);
    llvm::Value *newVal = CreateSub(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  case TOKEN_MUL_ASSIGNMENT_OP: {
    llvm::Value *r_lhs = emit_rvalue_expression(lhs_expr);
    llvm::Value *newVal = CreateMul(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  case TOKEN_DIV_ASSIGNMENT_OP: {
    llvm::Value *r_lhs = emit_rvalue_expression(lhs_expr);
    llvm::Value *newVal = CreateSDiv(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  case TOKEN_REM_ASSIGNMENT_OP: {
    llvm::Value *r_lhs = emit_rvalue_expression(lhs_expr);
    llvm::Value *newVal = CreateSRem(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  //TOKEN_AND_ASSIGNMENT_OP,
  //TOKEN_OR_ASSIGNMENT_OP,
  //TOKEN_XOR_ASSIGNMENT_OP,
  //TOKEN_NOT_ASSIGNMENT_OP,
  default:
    halt();
  }
}

void emit_return_statement(Token *return_statement) {
  Token *subtokens[1];
  int num_subtokens = expand_tokens(return_statement, subtokens, 1);
  Token *return_expression = subtokens[0];
  //assert(return_statement.token == TOKEN_COMPOUND_RETURN_STATEMENT);
  //assert(return_statement.num_subtokens == 1);
  //GGToken &return_expression = return_statement.subtokens[0];

  llvm::Value *retval = emit_rvalue_expression(return_expression);
  g.llvm.builder->CreateRet(retval);
}

std::string to_llvm_type_str(llvm::Type *type)
{
  std::string data;
  llvm::raw_string_ostream stream(data);
  type->getContainedType(0)->print(stream);
  return stream.str();
}

//void LinesRemove(GGSubString *lines, int num_lines, int line_to_remove) {
//  std::vector<std::string> lines2;
//  int i = 0;
//  lines2.erase(&lines2[i]);
//  lines2.insert(&lines2[i], 
//
//}

static std::string format_string(const std::string fmt, ...) {
  int size = ((int)fmt.size()) * 2 + 50;   // use a rubric appropriate for your code
  std::string str;
  va_list ap;
  while (1) {     // maximum 2 passes on a POSIX system...
    str.resize(size);
    va_start(ap, fmt);
    int n = vsnprintf((char *)str.data(), size, fmt.c_str(), ap);
    va_end(ap);
    if (n > -1 && n < size) {  // everything worked
      str.resize(n);
      return str;
    }
    if (n > -1)  // needed size returned
      size = n + 1;   // for null char
    else
      size *= 2;      // guess at a larger size (o/s specific)
  }
  return str;
}

typedef std::vector<std::string> Lines;

enum LLVMReplacement {
  LLVM_REPLACEMENT_NONE,
  LLVM_REPLACEMENT_ASSIGNMENT,
  LLVM_REPLACEMENT_EXPRESSION,
  LLVM_REPLACEMENT_FIRST_EXPRESSION,
};

static const char *find_char(const char *start, const char *end, char c) {
  const char *result = (const char *)memchr(start, c, end-start);
  return result;
}

static const char *find_any_of(const char *start, const char *end, char *chars) {
  for(const char *cur = start; cur != end; ++cur) {
    for(const char *a_char = chars; *a_char != 0; ++a_char) {
      if (*cur == *a_char) return cur;
    }
  }

  return NULL;
}

LLVMReplacement matches_replacement(std::string &line, std::string &token_str, SubString &token_substr, std::string &old_lhs, std::string &old_rhs) {
  const char *end = line.data() + line.length();
  const char *replacement = find_char(line.data(), end, '$');
  if (replacement == NULL) return LLVM_REPLACEMENT_NONE;

  const char *space = find_any_of(replacement, end, ", \t\r\n\0");
  if (space == NULL) {
    space = end;
  }

  const char *equals = find_char(space, end, '=');

  old_lhs = std::string(line.data(), replacement);
  //old_lhs.start = line.data();
  //old_lhs.length = replacement - old_lhs.start;

  token_str = std::string(replacement + 1, space);
  token_substr.start = replacement + 1;
  token_substr.length = space - token_substr.start;

  old_rhs = std::string(space, end);
  //old_rhs.start = space;
  //old_rhs.length = end - space;

  if (equals == NULL) {
    const char *equals = find_char(line.data(), end, '=');
    assert(equals);
    //if (equals == NULL) return LLVM_REPLACEMENT_FIRST_EXPRESSION;

    const char *first_item = find_char(equals, end, '%');
    if (first_item == NULL) return LLVM_REPLACEMENT_FIRST_EXPRESSION;
    if (first_item > replacement) return LLVM_REPLACEMENT_FIRST_EXPRESSION;
    return LLVM_REPLACEMENT_EXPRESSION;
  } else {
    return LLVM_REPLACEMENT_ASSIGNMENT;
  }
}

int lines_replace_tokens(Lines &lines) {
  int temp_n = 0;
  for(int i = 0; i < (int)lines.size();) {
    auto line = lines[i];

    std::string old_lhs;
    std::string token_str;
    SubString token_substr;
    std::string old_rhs;
    LLVMReplacement replacement = matches_replacement(line, token_str, token_substr, old_lhs, old_rhs);
    switch(replacement) {
    case LLVM_REPLACEMENT_ASSIGNMENT: {
      llvm::Type *type = db_lookup_variable(token_substr)->llvm_value->getType();
      std::string type_str = to_llvm_type_str(type);
      std::string new_line = format_string("%s%%%s%d%s", old_lhs.c_str(), token_str.c_str(), temp_n, old_rhs.c_str());
      std::string new_store = format_string("store %s %%%s%d, %s* %%%s", type_str.c_str(), token_str.c_str(), temp_n, type_str.c_str(), token_str.c_str());

      lines.erase(lines.begin() + i); //lines.remove(i); //line);
      lines.insert(lines.begin() + i, new_line);
      lines.insert(lines.begin() + i+1, new_store);
      //next = i;
                                      } break;
    case LLVM_REPLACEMENT_FIRST_EXPRESSION: 
    case LLVM_REPLACEMENT_EXPRESSION: {
      llvm::Type *type = db_lookup_variable(token_substr)->llvm_value->getType();
      std::string type_str = to_llvm_type_str(type);
      std::string new_load  = format_string("%%%s%d = load %s* %%%s", token_str.c_str(), temp_n, type_str.c_str(), token_str.c_str());
      std::string new_line        = format_string("%s%s %%%s%d%s", old_lhs.c_str(), type_str.c_str(), token_str.c_str(), temp_n, old_rhs.c_str());
      std::string new_line_no_type= format_string("%s%%%s%d%s", old_lhs.c_str(), token_str.c_str(), temp_n, old_rhs.c_str());
      if (replacement == LLVM_REPLACEMENT_EXPRESSION) new_line = new_line_no_type;

      lines.erase(lines.begin() + i); //lines.remove(line);
      lines.insert(lines.begin() + i, new_load); //lines.insert(new_load);
      lines.insert(lines.begin() + i+1, new_line); //lines.insert(new_line);
      ++i; //next = new_line;
                                      } break;
    case LLVM_REPLACEMENT_NONE:
      ++i;
      break;
    default:
      halt();
    }

    ++temp_n;
  }

  //return lines.count
  // TODO
  return 0;
}


//
//void emit_inline_llvm(LLVM &llvm, GGToken &inline_llvm) {
//  assert(inline_llvm.token == TOKEN_COMPOUND_INLINE_LLVM);
//  assert(inline_llvm.num_subtokens == 0);
//
//  // $token = *    --> %token.N = * 
//  //                   store type %token.N type* %token
//  // * $token *    --> %token.N = load %token.N
//  //                   * %token.N *
//
//  GGSubString lines[MAX_LLVM_LINES];
//
//  int temp_n = 0;
//  int num_lines = substring_to_lines(inline_llvm.substring, lines, MAX_LLVM_LINES);
//  num_lines = lines_replace_tokens(llvm, lines, num_lines, MAX_LLVM_LINES);
//  lines_to_llvm(llvm, lines, num_lines);
//
//  //char max_llvm[1024];
//  //int output_spot = 0;
//  //for(int i = 0; i < inline_llvm.substring.length; ++i) {
//  //  if (inline_llvm.substring.start[i] == '%') {
//  //    GGSubString token;
//  //    token.start = &inline_llvm.substring.start[i+1];
//  //    const char *end = find_first_non_token(token.start);
//  //    token.length = end - token.start;
//  //    const char *next_token = find_first_non_whitespace(end+1);
//  //    const char *end_of_line = find_first_end_of_line(end+1);
//
//  //    if (*next_token == '=') {
//
//  //    } else {            
//  //                                        
//  //    }
//
//  //    // 
//
//  //  } else {
//  //    max_llvm[output_spot++] = inline_llvm.substring.start[i]
//  //  }
//  //}
//
//  
//  
//}

void trim(std::string &str)
{
  size_t startPos = str.find_first_not_of(" \t\r\n");
  if (startPos != str.npos)
    str = str.substr(startPos);
  else 
    str.clear();

  size_t endPos = str.find_last_not_of(" \t\r\n");
  if (endPos != str.npos)
    str = str.substr(0, endPos+1);
  else
    str.clear();

}

void substring_to_lines(const SubString &substring, Lines &lines) {
  const char* prevStart = substring.start;

  for(int i = 0; i < substring.length; ++i)
  {
    const char* potentialEnd = &substring.start[i];
    if (substring.start[i] == '\r') {
      if (substring.start[i] == '\n') ++i;
    } else if (substring.start[i] == '\n') {
      if (substring.start[i] == '\n') ++i;
    } else if (i == substring.length-1) 
    {
      // do string and exit
    } else {
      continue;
    }

    std::string newStr(prevStart, potentialEnd);
    trim(newStr);
    if (newStr.length() > 0) {
      lines.push_back(newStr);
    }

    if (i == substring.length-1) break;

    ++i;
    prevStart = &substring.start[i+1];
  }
}

static void lines_to_buffer(const Lines &lines, char *buffer, int buffer_size) {
  const char *start = buffer;
  buffer[0] = 0;
  for(auto line : lines) {
    buffer = strcat(buffer, line.data());
    assert(buffer - start < buffer_size);
    buffer = strcat(buffer, "\n");
  }
}

void replace_inline_llvm_bindings(Token *raw_llvm, char replaced_llvm_buffer[1024]) 
{
  //GGSubString lines[MAX_LLVM_LINES];
  Lines lines;
  substring_to_lines(raw_llvm->substring, lines);
  lines_replace_tokens(lines);
  lines_to_buffer(lines, replaced_llvm_buffer, 1024);
}

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/AsmParser/Parser.h"
#include "LLParser2.h"

static bool ParseAssembly2(llvm::MemoryBuffer *F, llvm::Module &M, llvm::SMDiagnostic &Err, llvm::Function *Function, llvm::BasicBlock *BB) 
{
  llvm::SourceMgr SM;
  //std::unique_ptr<MemoryBuffer> Buf = llvm::MemoryBuffer::getMemBuffer(F, false);
  //SM.AddNewSourceBuffer(std::move(Buf), SMLoc());
  return llvm::LLParser2(F, SM, Err, &M).RunSubFunction(Function, BB);
}


void emit_inline_llvm(FunctionDef *function, Token *inline_llvm) {
  Token *subtokens[1];
  int num_subtokens = expand_tokens(inline_llvm, subtokens, 1);
  Token *raw_llvm = subtokens[0];

  g.llvm.builder->GetInsertPoint();

  char replaced_llvm_buffer[1024];
  replace_inline_llvm_bindings(raw_llvm, replaced_llvm_buffer);

  llvm::StringRef llvmAssembly(replaced_llvm_buffer);
  llvm::MemoryBuffer *memory = llvm::MemoryBuffer::getMemBuffer(llvmAssembly, "<string>", true);
  llvm::SMDiagnostic error;
  g.llvm.module->dump();
  bool retval = ParseAssembly2(memory, *g.llvm.module, error, function->llvm_function, g.llvm.builder->GetInsertBlock());
  assert(retval);
}

void emit_function_declaration(FunctionDef *function) {
  //if (function_body->start == NULL) {}
  //  function = db_function_lookup(function_identifier, NULL);
  //} else {
  //  llvm::Type *param_types[MAX_PARAMS];
  //  int num_params = get_param_types(llvm, function_params, param_types, MAX_PARAMS);
  //  function = db_lookup_function_declaration(llvm, function_identifier, param_types, num_params);
  //}

  llvm::Type *retval_type = llvm_get_type(function->retval_type);

  llvm::FunctionType *functionType;
  if (function->num_params == 0) 
  {
    functionType = llvm::FunctionType::get(retval_type, FIXED_ARGS);
  }
  else
  {
    llvm::Type *param_types[MAX_PARAMS];
    int num_params = get_function_param_llvm_types(function, param_types, MAX_PARAMS);
    llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, num_params);
    functionType = llvm::FunctionType::get(retval_type, args, FIXED_ARGS);

    //emit_paramater_bindings(llvm, function_params);
  }

  llvm::StringRef name = to_string_ref(function->identifier);
  function->llvm_function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, g.llvm.module);
}

EmitResult emit_function_definition(Token *function_definition) {
  assert(function_definition->type == TOKEN_FUNCTION_DEFINITION);
  //assert(program_statement->type == TOKEN_VARIABLE_DEFINITION);
  Token *subtokens[4];
  int num_subtokens = expand_tokens(function_definition, subtokens, 4);
  Token *function_return_type = subtokens[0];
  Token *function_identifier  = subtokens[1];
  Token *function_params      = subtokens[2];
  Token *function_body        = subtokens[3];

  FunctionDef *function = db_function_definition_lookup(function_identifier->substring, function_params); 
  //llvm::Function *function;


  if (function->llvm_function == NULL) {
    emit_function_declaration(function);
  }


  //assert(function);


  db_push_scope();

  assert(function->llvm_function);

  llvm::BasicBlock *entry = llvm::BasicBlock::Create(*g.llvm.context, "", function->llvm_function);
  g.llvm.builder->SetInsertPoint(entry);


  int i = 0;
  llvm::Function::arg_iterator arg_iterator = function->llvm_function->arg_begin();
  for(Token *param = function_params->start; param!= NULL; param = param->next, arg_iterator++) {
    Token *subtokens[2];
    int num_subtokens = expand_tokens(param, subtokens, 2);
    Token *param_type = subtokens[0];
    Token *param_identifier = subtokens[1];

    TypeDef *type = emit_type_declaration(param_type);

    if (variable_exists(param_identifier->substring)) {
      return make_emit_error("Duplicate varaible %s", to_cstring(param_identifier->substring).c_str());
    }

    llvm::Argument *AI = arg_iterator;
    llvm::Value *alloca = CreateAlloca(function, param_identifier->substring, type);
    CreateStore(AI, alloca);

    VariableDef var;
    var.identifier = param_identifier->substring;
    var.llvm_value = alloca;
    var.type = type;
    var.initializer_value = NULL;
    var.token = param;
    var.scope = g.db.scope;
    g.db.variables.push_back(var);
    //db_add_variable(param_identifier->substring, alloca);
    //db_add_parameter_declaration()
  }


  //for (llvm::Function::arg_iterator AI = function->function->arg_begin(); i < function_params.num_subtokens; ++AI, ++i) {
  //  const GGToken &param_type = function_params.subtokens[i].subtokens[0];
  //  const GGToken &param_identifier = function_params.subtokens[i].subtokens[1];
  //  //AI->setName(name);

  //  llvm::StringRef name = to_string_ref(param_identifier.substring);

  //  llvm::Value *alloca = CreateAlloca(llvm, function, param_identifier, param_type);
  //  llvm.builder->CreateStore(AI, alloca);

  //  db_add_variable(llvm, param_identifier, alloca);
  //}

  //llvm::BasicBlock *BB = BasicBlock::Create(*llvm.context, "entry", function);
  //llvm.builder->SetInsertPoint(BB);

  for(Token *statement = function_body->start; statement!= NULL; statement = statement->next) {
  //for(int i = 0; i < function_body.num_subtokens; ++i) {
    //GGToken &subtoken = function_body.subtokens[i];
    switch(statement->type) {
    case TOKEN_VARIABLE_DEFINITION:
      emit_local_variable_definition(function, statement);
      break;
    case TOKEN_ASSIGMENT_STATEMENT:
      emit_assignment_statement(statement);
      break;
    case TOKEN_RETURN_STATEMENT:
      emit_return_statement(statement);
      break;
    case TOKEN_EXPRESSION_STATEMENT:
      //assert(subtoken.num_subtokens == 1);
      assert(statement->start);
      assert(statement->start->next == NULL);
      emit_rvalue_expression(statement->start);
      break;
    case TOKEN_INLINE_LLVM:
      emit_inline_llvm(function, statement);
      break;
    default: 
      halt();
    }
  }
  db_pop_scope();

  //verifyFunction(*function);

  //llvm::Function *function = lookup_function(function_definition);
  //llvm_emit_function_body(llvm, function, function_body);
  //function->addFnAttr("nounwind");

  //db_add_function(function_identifier, function_params);

  return EMIT_SUCCESS;
}

void emit_external_function_declaration(Token *function_declaration) {
  Token *subtokens[3];
  int num_subtokens = expand_tokens(function_declaration, subtokens, 3);
  Token *function_return_type = subtokens[0];
  Token *function_identifier  = subtokens[1];
  Token *function_param_types = subtokens[2];
  //assert(function_declaration.token == TOKEN_COMPOUND_EXTERNAL_FUNCTION_DECLARATION);
  //assert(function_declaration.num_subtokens == 3);
  //const GGToken &function_return_type = function_declaration.subtokens[0];
  //const GGToken &function_identifier  = function_declaration.subtokens[1];
  //const GGToken &function_param_types = function_declaration.subtokens[2];

  FunctionDef *function = db_function_definition_lookup(function_identifier->substring, function_param_types);

  //TypeDef *retval_type = emit_type_declaration(function_return_type);
  //llvm::Type *retval_type = get_type(llvm, function_return_type);

  //llvm::FunctionType *functionType;
  llvm::FunctionType *function_type;
  if (function_param_types->start == NULL) {
    function_type = llvm::FunctionType::get(function->retval_type->llvm_type, FIXED_ARGS);
  } else {
    int num_params = 0;
    llvm::Type *param_types[MAX_PARAMS];
    num_params = get_function_param_llvm_types(function, param_types, MAX_PARAMS);

    llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, num_params);
    function_type = llvm::FunctionType::get(function->retval_type->llvm_type, args, FIXED_ARGS);

    //emit_paramater_bindings(llvm, function_params);
  }

  llvm::StringRef name = to_string_ref(function_identifier->substring);

  llvm::Function *llvm_function = llvm::Function::Create(function_type, llvm::Function::ExternalLinkage, name, g.llvm.module);
  assert(llvm_function);

  //function->addFnAttr("nounwind");
  //Function *function = new Function;
  //function->function = llvm_function;
  //function->identifier = function_identifier->substring;
  //function->param_types = function_param_types;
  //db_add_function(function);

  //if (function_param_types->start == NULL) {
  //  db_add_function(function_identifier, function, NULL, 0);
  //} else {
  //  db_add_function(function_identifier, function, param_types, num_params);
  //}
}

//llvm::Type *llvm_emit_llvm_type_definition(LLVM &llvm, const GGToken &type_def) {
//  assert(type_def.num_subtokens == 2);
//  const GGToken &identifier = type_def.subtokens[0];
//  const GGToken &type_decl = type_def.subtokens[1];
//
//  llvm::Type *type = get_type(llvm, type_decl);
//  db_add_type(llvm, identifier, type);
//  return type;
//}

EmitResult emit_program_statement(Token *program_statement) {
    switch(program_statement->type) {
    case TOKEN_STRUCT_DEFINITION:
      //emit_struct_definition(program_statement);
      break;
    case TOKEN_LLVM_TYPE_DEFINITION:
      //emit_llvm_type_definition(program_statement);
      break;
    case TOKEN_TYPEDEF_DEFINITION:
      //emit_typedef_definition(program_statement);
      break;
    case TOKEN_VARIABLE_DEFINITION:
      emit_global_variable_initialization(program_statement);
      break;
    case TOKEN_FUNCTION_DEFINITION:
      emit_function_definition(program_statement);
      break;
    case TOKEN_EXTERNAL_FUNCTION_DECLARATION:
      emit_external_function_declaration(program_statement);
      break;
    case TOKEN_IMPORT_STATEMENT:
      break;
    default:
      halt();
    }

  return EMIT_SUCCESS;
}

bool is_success(const EmitResult &result) {
  return true;
}

EmitResult emit_program(Token *program, const char *dest_file) {
  LLVMInit(dest_file);

  for(Token *program_statement = program->start; program_statement != NULL; program_statement = program_statement->next) {
    EmitResult result = emit_program_statement(program_statement);
    if(is_success(result) == false) return result;
  }


  //EmitResult result = emit_global_type_definitions(llvm, program);
  //if(is_success(result) == false) return result;
  //result = emit_global_function_prototypes(llvm, program);
  //if(is_success(result) == false) return result;
  //result = emit_global_variable_definitions(llvm, program);
  //if(is_success(result) == false) return result;
  //result = emit_global_function_definitions(llvm, program);
  //if(is_success(result) == false) return result;

  g.llvm.module->dump();

  //IRCompile(llvm);
  return EMIT_SUCCESS;
}

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
  retval = retval.substr(0, endPos);
  retval += ".out";
  return retval;
}

//void db_add_global_declarations(Token *program) {
//  for(Token *program_statement = program->start; program_statement != NULL; program_statement = program_statement->next) {
//    switch(program_statement->type) {
//      case TOKEN_FUNCTION_DEFINITION: {
//        db_add_function(program_statement);
//      } break;
//    }
//}

const CompileResult COMPILE_SUCCESS = {};

void IRCompile(LLVMState &llvm);

CompileResult compile_program(const char *source_file) {
  ParseResult parse_result = parse_program(source_file);
  if (is_success(parse_result) == false) return make_result(parse_result);

  db_add_global_declarations(parse_result.start);

  TypecheckResult typecheck_result = typecheck_program(parse_result.start);
  if (is_success(typecheck_result) == false) return make_result(typecheck_result);

  std::string dest_file = to_dest_file(source_file);

  EmitResult emit_result = emit_program(parse_result.start, dest_file.c_str());
  if (is_success(emit_result) == false) return make_result(typecheck_result);
  
  g.llvm.module->dump();
  IRCompile(g.llvm);

  return COMPILE_SUCCESS;
}