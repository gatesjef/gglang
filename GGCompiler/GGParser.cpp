// GGParser.cpp

#include "Precompile.h"
#include "GGParser.h"
#include "LLVMIRCompiler.h"


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
  TOKEN_OPERATOR_IDENTIFIER,
  //TOKEN_TYPE_IDENTIFIER,
  //TOKEN_FUNCTION_IDENTIFIER,
  //TOKEN_VARIABLE_IDENTIFIER,
TOKEN_SUBSTRING_END,

TOKEN_OP_START,
  TOKEN_VARARG_PARAM,
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

std::string string_format_arg_list(const char *format, va_list args) {
  int length = _vscprintf(format, args) + 1;
  char *formatted = (char *)malloc(sizeof(char) * length);

  int write_count = _vsnprintf(formatted, length, format, args);
  assert(write_count >= 0);

  std::string retval = formatted;
  free(formatted);

  return retval;
}

std::string string_format(const char *format, ...) {
  va_list args;
  va_start(args, format);

  std::string retval = string_format_arg_list(format, args);

  va_end(args);

  return retval;
}


struct ParseTable;
//struct Type;

struct SubString {
  const char *start;
  int length;
};

struct Token {
  TokenType type;
  FileLocation location;

  //union {
    SubString substring;
    //struct {
      Token *start;
      Token *end;
    //};
    const ParseTable *entry;
  //};

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

enum TypeKind {
  //BASE_TYPE,
  ARBITRARY_INTEGER,
  //ARBITRARY_FLOAT,
  
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

struct LLVMTypeDefinition;
struct StructDefinition;

struct TypeDef {
  int scope;
  TypeKind kind;
  SubString identifier;

  union {
    struct LlvmType {
      LLVMTypeDef type_definition;
      LLVMTypeDefinition *new_type_definition;
    } llvm_def;
    struct StructType {
      StructDef *type_definition;
      StructDefinition *new_type_definition;
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

#include "list"
#define Table std::list

struct VariableDefinition;
struct FunctionDefinition;
struct ExternFunctionDeclaration;

struct TypeDefinitionDBEntry {
  int scope;
  TypeDef *varaible;
};

struct VaraibleDefinitionDBEntry {
  int scope;
  VariableDefinition *variable;
};

struct FunctionDefinitionDBEntry {
  int scope;
  FunctionDefinition        *function;
  ExternFunctionDeclaration *extern_function;
};

struct SymbolDatabase {
  //Table<TypeDef> types;
  //Table<StructDef> structs;
  //Table<FunctionDef> functions;
  //Table<VariableDef> variables;


  Table<FunctionDefinitionDBEntry> functions;
  Table<VaraibleDefinitionDBEntry> variables;
  Table<TypeDef> types;

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
  ParserState parser;
  SymbolDatabase db;
  LLVMState llvm;
} g;

void db_push_scope() {
  g.db.scope++;
}

bool varaible_out_of_scope(const VaraibleDefinitionDBEntry &var) {
  return var.scope > g.db.scope;
}

void db_pop_scope() {
  g.db.scope--;
  

  auto remove = std::remove_if(g.db.variables.begin(), g.db.variables.end(), varaible_out_of_scope);
  g.db.variables.erase(remove, g.db.variables.end());

  //for(auto &vars : g.db.functions) {
  //  if (vars.scope > g.db.scope) {
  //    //erase..
  //  }
  //}

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
      if (comment_mode == end_of_line) {
        parser.current_char += 1;
      } else if (parser.current_char[1] == '/') {
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
      return;
    default:
      if (comment_mode == end_of_line) {
        ++parser.current_char;
        break;
      }
      return;
    }
  }
}

//enum ParseResultType {
//  RESULT_SUCCESS,
//  RESULT_ERROR,
//  RESULT_NONE,
//};

struct ParseResult {
    ResultType result;
    Error error;
    Token *start;
    Token *end;
    int sequence_index;
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

//enum EmitResultType  {
//  EMIT_OK,
//  EMIT_ERROR,
//};

struct EmitResult {
  ResultType result;
  Error error;
};

const EmitResult EMIT_SUCCESS = {};

EmitResult make_emit_error(const char *error_fmt, ...) {
  EmitResult retval;
  retval.result = RESULT_ERROR;
  return retval;
}

ParseResult make_sequence_error(ParserState &parser, int i) {
  ParseResult retval;
  retval.result = RESULT_ERROR;
  retval.error.location.file_index = parser.current_file_idx;
  retval.error.location.line = parser.current_line_number;
  retval.error.location.column = parser.current_char - parser.current_line_start;
  retval.error.error_string = "sequence error";
  retval.sequence_index = i;
  return retval;
}

#define FORMAT_ERROR_STRING(dest, format) \
  va_list args; \
  va_start(args, format); \
  dest = string_format_arg_list(format, args); \
  va_end(args);


ParseResult make_error(ParserState &parser, const char *error_fmt, ...) {
  ParseResult retval;
  retval.result = RESULT_ERROR;
  retval.error.location.file_index = parser.current_file_idx;
  retval.error.location.line = parser.current_line_number;
  retval.error.location.column = parser.current_char - parser.current_line_start;
  FORMAT_ERROR_STRING(retval.error.error_string, error_fmt); // retval.error.error_string = error_fmt;
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
  retval.result = RESULT_SUCCESS;
  retval.start = new_token;
  retval.end = new_token;
  return retval;
}

ParseResult make_result(ParserState &parser, TokenType type, const char *start, const char *end) {
  Token *new_token = token_alloc(parser);
  new_token->type = type;
  new_token->substring.start = start;
  new_token->substring.length = end - start;
  new_token->location.file_index = parser.current_file_idx;
  new_token->location.line = parser.current_line_number;
  new_token->location.column = start - parser.current_line_start;

  return make_success(new_token);
}

bool is_error(const ParseResult& result) {
  return (result.result == RESULT_ERROR);
}

bool is_success(const ParseResult& result) {
  return (result.result == RESULT_SUCCESS);
}

bool is_none(const ParseResult& result) {
  return (result.result == RESULT_NONE);
}

ParseResult make_result(ParserState &parser, TokenType type, const ParseResult &result_subtokens) {
  if (is_success(result_subtokens) == false) return result_subtokens;
  Token *new_token = token_alloc(parser);
  new_token->type = type;
  new_token->start = result_subtokens.start;
  new_token->end = result_subtokens.end;

  if (result_subtokens.start) {
    new_token->location = result_subtokens.start->location;
  } else {
    new_token->location.file_index = parser.current_file_idx;
    new_token->location.line = parser.current_line_number;
    new_token->location.column = 0;
  }


  return make_success(new_token);
}

ParseResult make_result(ParserState &parser, TokenType type, Token *subtoken) {
  assert(subtoken);
  Token *new_token = token_alloc(parser);
  new_token->type = type;
  new_token->start = subtoken;
  new_token->end = subtoken;

  if (subtoken) {
    new_token->location = subtoken->location;
  } else {
    new_token->location.file_index = parser.current_file_idx;
    new_token->location.line = parser.current_line_number;
    new_token->location.column = 0;
  }

  return make_success(new_token);
}

ParseResult make_result(ParserState &parser, TokenType type, const ParseTable *entry) {
  assert(entry);
  Token *new_token = token_alloc(parser);
  new_token->type = type;
  new_token->entry = entry;

  new_token->location.file_index = parser.current_file_idx;
  new_token->location.line = parser.current_line_number;
  new_token->location.column = 0;

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

void append_result(Token *token, const ParseResult &results) {
  assert(token->start);
  assert(token->end);
  token->end->next = results.start;
  token->end = results.end;
}

void append_result_old(ParseResult &retval, ParseResult &new_result) {
  assert(retval.result == RESULT_SUCCESS);
  switch (new_result.result) {
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
      if (i != 0) return make_sequence_error(parser, i);
      else return result;
    }
    append_result_old(retval, result);
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

ParseResult parser_add_file(ParserState &parser, const SubString &relative_path);

ParseResult emit_import_statement(ParserState &parser, const ParseResult &import_statement) {
  Token *subtokens[3];
  int num_subtokens = expand_tokens(import_statement, subtokens, 3);
  
  assert(num_subtokens >= 1);
  assert(num_subtokens <= 3);

  switch(num_subtokens) {
  case 1:
    return parser_add_file(parser, subtokens[0]->substring);
  case 2:
  case 3:
    // TODO
  default:
    halt();
  }

  return make_error(parser, "Unknown import statement");
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
  if (retval.result == RESULT_SUCCESS) {
    ParseResult import_result = emit_import_statement(parser, retval);
    if (is_success(import_result) == false) return import_result;
    return retval;
  }

  if (retval.sequence_index == 0) {
    return PARSE_NONE;
  } else {
    return make_error(parser, "Invalid import statement");
  }
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
  append_result(retval.start, size);
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
    if (result.result == RESULT_SUCCESS) return result;
    //if (result.result == RESULT_ERROR) return result;
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
    append_result_old(retval, result);

    result = parse_exact(parser, separator);
    if (is_success(result) == false) break;
  }

  return retval;
}

ParseResult parse_varargs(ParserState &parser) {
  return parse_exact(parser, "...");
}

ParseResult parse_param_type_declaration(ParserState &parser) {
  ParseResult result = parse_varargs(parser);
  if (is_success(result)) {
    return make_result(parser, TOKEN_VARARG_PARAM, result);
  } else {
    ParseResult result = parse_type_declaration(parser);
    if (is_success(result) == false) return result;
    return make_result(parser, TOKEN_FUNCTION_PARAM, result);
  }
}

ParseResult parse_function_type_params(ParserState &parser) {
  ParseResult result = parse_zero_or_more_separated(parser, parse_param_type_declaration, ",");
  if (is_success(result) == false) return result;
  return make_result(parser, TOKEN_FUNCTION_PARAMS, result);
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
  append_result(retval.start, params);
  return retval;
}

ParseResult parse_operator_exact(ParserState &parser) {
  return parse_exact(parser, "operator");
}

ParseResult parse_any_operator(ParserState &parser);
ParseResult parse_operator_identifier(ParserState &parser) {
  static const ParseFn fns[] = { 
    parse_operator_exact,
    parse_any_operator,
  };
  static const int num = ARRAYSIZE(fns);
  ParseResult retval = parse_sequence(parser, fns, num);
  return retval;
}

ParseResult parse_function_identifier(ParserState &parser) {
  ParseResult op_identifier = parse_operator_identifier(parser);
  if (is_success(op_identifier)) {
    return make_result(parser, TOKEN_OPERATOR_IDENTIFIER, op_identifier);
  }

  return parse_identifier(parser);

  //static const ParseFn fns[] = { 
  //  parse_operator_identifier, 
  //  parse_identifier, 
  //};
  //static const int num = ARRAYSIZE(fns);
  //return parse_first_of(parser, fns, num);
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
    append_result(result.start, expr);
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
  subtokens.result = RESULT_SUCCESS;
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
  append_result(retval.start, array_index);
  return retval;
}

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

ParseResult parse_any_operator(ParserState &parser) {
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
  
    "++", TOKEN_PRE_INC_OP,     3,
    "--", TOKEN_PRE_DEC_OP,     3,
    "!",  TOKEN_LOGICAL_NOT_OP, 3,
    "~",  TOKEN_BITWISE_NOT_OP, 3,
  };
  static const int num_entries = ARRAYSIZE(table);
  return parse_first_table(parser, table, num_entries);
}

Token *get_token(ParseResult &result) {
  assert(result.result == RESULT_SUCCESS);
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
  append_result(retval.start, base);
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
    assert(rhs.result >= 0 && rhs.result <= 3);
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
    append_result(new_lhs.start, op);
    append_result(new_lhs.start, rhs);
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
    append_result_old(retval, results);
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
    append_result_old(partial_results, expr);
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
  int len = strlen(str);
  while(1) {
    if (*parser.current_char == 0) {
      return make_error(parser, "Unexpected end of file");
    }

    if (*parser.current_char == '\r' ||
        *parser.current_char == '\n') 
    {
      char old_char = *parser.current_char;
      parser.current_char++;
      if ((*parser.current_char == '\r' ||
        *parser.current_char == '\n') &&
        *parser.current_char != old_char) 
      {
        parser.current_char++;
      }

      parser.current_line_number++;
      parser.current_line_start = parser.current_line_start = parser.current_char;
      continue;
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
    append_result(result.start, else_statement);
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

ParseResult parse_variable_or_function_definition(ParserState &parser);

ParseResult parse_statement(ParserState &parser) {
  {
    static const ParseFn statements[] = {
      parse_block_statement,
      parse_return_statement,
      //parse_for_statement,
      parse_if_statement,
      // parse_switch_statement,
      parse_inline_llvm,

      parse_type_definition,
    };
    static const int num_statements = ARRAYSIZE(statements);

    ParseResult result = parse_first_of(parser, statements, num_statements);
    if (is_success(result)) return result;
    if (is_error(result)) return result;
  }

  {
    static const ParseFn difficult_statements[] = {
      parse_variable_or_function_definition,  // 
      parse_assignment_statement,             // 
      parse_expression_statement,             // 
      //parse_function_definition,
    };
    static const int num_difficult_statements = ARRAYSIZE(difficult_statements);
    ParseResult result = parse_first_of_deep(parser, difficult_statements, num_difficult_statements);
    if (result.result == RESULT_ERROR) {
      return make_error(parser, "Unknown statement");
    }

    return result;
  }

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

int count_subtokens(Token *token) {
  int count = 0;
  for(Token *cur = token->start; cur != NULL; cur = cur->next) {
    count++;
  }

  return count;
}

// either variable or function definition
ParseResult parse_variable_or_function_definition(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_type_declaration,
    parse_function_identifier, 
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
    if (is_success(result) == false) return result;

    ParseResult retval = make_result(parser, TOKEN_FUNCTION_DEFINITION, partial_results);
    assert(retval.start == retval.end);
    assert(count_subtokens(retval.start) == 2);
    assert(retval.start->type == TOKEN_FUNCTION_DEFINITION);

    append_result(retval.start, result);
    assert(retval.start == retval.end);
    assert(count_subtokens(retval.start) == 4);
    assert(retval.start->type == TOKEN_FUNCTION_DEFINITION);

    return retval;
  } else {
    //variable definition
    if (partial_results.end->type == TOKEN_OPERATOR_IDENTIFIER) {
      return make_error(parser, "Invalid operator definition");
    }

    return parse_variable_definition_part_2(parser, partial_results);
  }
}

ParseResult parse_program_statements(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_import_statement,             // import
    parse_extern_function_declaration,  // extern void x(int y);
    parse_type_definition,              // struct ..., type ..., enum ..., llvm type
    parse_variable_or_function_definition,
  };
  static const int num_statements = ARRAYSIZE(statements);
  ParseResult result = parse_first_of(parser, statements, num_statements);
  return result;
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
    //error("cannot open %s", input_filename);
    return NULL;
  }

  fseek(input_file, 0, SEEK_END);
  int file_size  = ftell (input_file);
  rewind(input_file);

  char *file_data = (char *)malloc(file_size + 1);
  int read = fread(file_data, 1, file_size, input_file);
  file_data[file_size] = 0;

  if (read != file_size) {
    //error("cannon read %s", input_filename);
    return NULL;
  }

  return file_data;
}

ParseResult parser_add_file(ParserState &parser, const SubString &relative_path) {
  std::string path = make_full_path(parser.current_directory, relative_path);
  if (parser_lookup_file(parser, path) == false) {
    char *data = file_read_all(path.c_str());
    if (data == NULL) {
      return make_error(parser, "cannot import file %s", path.c_str());
    }
    parser.files.push_back(path);
    parser.file_data.push_back(data);
  }

  return make_success(NULL);
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
  parser_init(g.parser);
  ParseResult result = parser_add_file(g.parser, make_substring(source_file));
  if (is_success(result) == false) return result;

  ParseResult retval = PARSE_EMPTY;
  for(int i = 0; i < (int)g.parser.files.size(); ++i) {
    parser_set_file_index(g.parser, i);
    consume_whitespace(g.parser);
    ParseResult result = parse_zero_or_more(g.parser, parse_program_statements);
    if (is_error(result)) return result;
    if (*g.parser.current_char != 0) {
      return make_error(g.parser, "Unexpected token - couldn't parse program statement.");
    }
    append_result_old(retval, result);
  }

  ParseResult program = make_result(g.parser, TOKEN_PROGRAM, retval);
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

//TokenType operator_identifer_to_op(const SubString &identifier) {
//  halt(); // todo
//  return TOKEN_ADD_OP;
//}

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

    int param_subtokens = count_subtokens(function_param);
    if (param_subtokens == 1) {
      Token *function_param_type;
      expand_tokens(function_param, function_param_type);
      types[i] = emit_type_declaration(function_param_type);
      function_param = function_param->next;

    } else {
      Token *function_param_type, *function_param_identifier;
      expand_tokens(function_param, function_param_type, function_param_identifier);
      types[i] = emit_type_declaration(function_param_type);
      function_param = function_param->next;
    }
  }
}

//enum DeclarationResultType {
//  DB_ERROR,
//  DB_SUCCESS,
//};
//
struct DeclarationResult {
  ResultType result;
  Error error;
};

const DeclarationResult DECLARATION_SUCCESS = {};

DeclarationResult make_db_error(const char *error, const std::string &substr) {
  DeclarationResult retval;
  retval.result = RESULT_ERROR;
  retval.error.error_string = error + substr;
  return retval;
}

DeclarationResult make_db_success() {
  DeclarationResult retval;
  retval.result = RESULT_SUCCESS;
  return retval;
}

std::string to_cstring(SubString &substring) {
  return std::string(substring.start, substring.length);
}

DeclarationResult struct_fill_fields(Token *struct_fields, FieldDef *&fields, int &num_fields) {
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

struct TypecheckResult {
  FileLocation loaction;
  ResultType result;
  Error error;
  TypeDef *type;
};

const TypecheckResult TYPECHECK_SUCCESS = {};

struct Expression;
//TypecheckResult typecheck_expression(Expression &expr);
TypeDef *temp_expression_token_to_typedef(Token *expression_token);


bool compare_params_types_to_value_types(TypeDef **function_param_types, Token **call_values, int num_params) {
  for(int i = 0; i < num_params; ++i) {
    TypeDef *param_type = function_param_types[i];
    Token *call_value = call_values[i];

    TypeDef *call_type = temp_expression_token_to_typedef(call_value);
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

//FunctionDef *db_function_call_lookup(const SubString &identifier, Token *function_params) {
//  assert(function_params->type == TOKEN_FUNCTION_CALL_PARAMS);
//  for(auto &function : g.db.functions) {
//    if (substring_cmp(function.identifier, identifier) == false) continue;
//    
//    Token *subtokens[MAX_PARAMS];
//    int num_params = expand_tokens(function_params, subtokens, MAX_PARAMS);
//    if (function.num_params != num_params) continue;
//
//    if (compare_params_types_to_value_types(function.param_types, subtokens, num_params) == false) {
//      continue;
//    }
//
//    return &function;
//  }
//
//  return NULL;
//};

//FunctionDef *db_operator_definition_lookup(TokenType op, Token *function_params) {
//  assert(function_params->type == TOKEN_FUNCTION_PARAMS);
//  for(auto &function : g.db.functions) {
//    if (function.op != op) continue;
//
//    Token *subtokens[MAX_PARAMS];
//    int num_params = expand_tokens(function_params, subtokens, MAX_PARAMS);
//    if (function.num_params != num_params) continue;
//
//    if (compare_params_types(function.param_types, subtokens, num_params) == false) {
//      continue;
//    }
//
//    return &function;
//  }
//
//  return NULL;
//};

//FunctionDef *db_function_definition_lookup(const SubString &identifier, Token *function_params) {
//  assert(function_params->type == TOKEN_FUNCTION_PARAMS);
//  for(auto &function : g.db.functions) {
//    if (substring_cmp(function.identifier, identifier) == false) continue;
//
//    Token *subtokens[MAX_PARAMS];
//    int num_params = expand_tokens(function_params, subtokens, MAX_PARAMS);
//    if (function.num_params != num_params) continue;
//
//    if (compare_params_types(function.param_types, subtokens, num_params) == false) {
//      continue;
//    }
//
//    return &function;
//  }
//
//  return NULL;
//};

//FunctionDef *db_unary_operator_lookup(TokenType op, TypeDef *type_lhs) {
//  for(auto &function : g.db.functions) {
//    if (function.op == op &&
//      function.num_params == 1 &&
//      function.param_types[0] == type_lhs) {
//        return &function;
//    }
//  }
//
//  return NULL;
//}

//numeric literals are of the smallest type that contains them and can be auto-upconverted
//
//typeof(1) = i8
//typeof(100000) = i16
//
//x := 1      // i8  1
//i32 y;      // i32 0
//z := x+y;   // i32 1;
//i8 w = x+y; // error, truncation
//
//
//i16 operator cast(i8 source) implicit {
//  i16 dest;
//  llvm {
//    $dest = sext $source to i16
//  }
//  return dest;
//}
//
//i8 operator cast(i16 source) {
//  i8 dest;
//  llvm {
//    $dest = trunc $source to i8
//  }
//  return dest;
//}
//
//auto cast  i32(i16)  {}
//auto cast  i64(i32)  {}
//auto cast i128(i64)  {}
//
//auto cast  f32(i8)   {}
//auto cast  f32(i16)  {}
//auto cast  f32(i32)  {}
//auto cast  f64(i64)  {}
//
//auto cast  f32(f16)  {}
//auto cast  f64(f32)  {}
//auto cast f128(f64)  {}
//
//
//i8 -> i16 -> if32 -> i32 -> if64 -> i64 -> i128
//              |              |               |
//             f32     ->     f64     ->     f128
//
//i32 y = 10;
//i32 z = 20;
//f32 x = (f32)y + (f32)z;


//f32 operator *(f32, f32) {
//  f32
//}

typedef TypeDef TypeDef2;

enum TypeDeclarationKind {
  TYPE_BASE,
  TYPE_FUNCTION,
  TYPE_POINTER,
  TYPE_ARRAY,
};
struct TypeDeclaration;

struct BaseType {
  SubString identifier;
};

struct FunctionType {
  TypeDeclaration *retval;
  TypeDeclaration *params;
  int num_params;
};

struct PointerType {
  TypeDeclaration *sub_type;
};

struct ArrayType {
  TypeDeclaration *sub_type;
  Expression *count_expression;
};

struct TypeDeclaration {
  TypeDeclarationKind kind;

  union {
    BaseType base_type;
    FunctionType function_type;
    PointerType pointer_type;
    ArrayType array_type;
  };

  TypeDef *type;
};


struct VariableDefinition {
  //Token *tok_type;
  //TypeDef2  *type;
  TypeDeclaration *type;

  SubString identifier;

  //Token *tok_initial_value;
  Expression *initial_value;

  llvm::Value *llvm_value;
};

enum ParamDefinitionType {
  PD_VARIABLE,
  PD_VARARGS,
};

struct ParamDefinition {
  ParamDefinitionType type;
  VariableDefinition variable;
};

struct FunctionStatement;

enum IntrinsicOperationType {
  OP_NONE,

  OP_POINTER_ADD,
  //OP_POINTER_SUBTRACT,

  OP_POINTER_DEREFERENCE,
  OP_ARRAY_DEREFERENCE,

  OP_ADDRESS_OF,

  //OP_CAST,
  OP_EXTEND_INTEGER,
  OP_EXTEND_FLOAT,
  OP_CAST_POINTER,
  OP_CAST_ARRAY_TO_POINTER,

  //OP_MEMBER_DEREFERENCE,

  // OP_CAST
  // OP_TRUNC
  // OP_SEXT
  // OP_UEXT
  // OP_FEXT
};

//struct IntrinsicOperation {
//  TypeDef2 *retval_type;
//  Expression *params;
//  int num_params;
//};

//struct TypeCast {
//  TypeDef2 *target_type;
//  Expression *source;
//};

enum TypecheckState {
  NEVER_TYPECHECKED,
  BEING_TYPECHECKED,
  FAILED_TYPECHECK,
  SUCCESS_TYPECHECK,
};

struct FunctionDefinition {
  TypecheckState been_typechecked;
  SubString identifier;
  TokenType op;

  //Token *tok_retval_type;
  //TypeDef2 *retval_type;
  TypeDeclaration *retval_type;

  //Token *tok_params;
  ParamDefinition *params;
  int num_params;

  //Token *tok_body;
  FunctionStatement *body;
  int num_statements;

  llvm::Function *llvm_function;
};

struct FunctionCall {
  SubString identifier;
  TokenType op;

  //Token *tok_params;
  Expression *params;
  int num_params;

  FunctionDefinition        *function;
  ExternFunctionDeclaration *extern_function;
  //IntrinsicOperation  intrinsic;
  IntrinsicOperationType    intrinsic;
  TypeDef                   *dest_type;

};

struct VariableReference {
  SubString identifier;

  VariableDefinition *variable;

  //VaraibleDefinition *definition;
  
  //VariableDef *variable;
  //TypeDef2 *type;
};

struct StringLiteral  {
  SubString literal;
  TypeDef *type;
};

struct NumericLiteral {
  SubString literal;
  TypeDef *type;
};

struct FieldDereference {
  Expression *object;
  SubString field_identifier;
  TypeDef *type;
};
//NullLiteral     
//UndefinedLiteral

enum ExpressionType {
  EXPR_NONE,
  //EXPR_OPERATOR, 
  //EXPR_INTRINSIC,
  EXPR_FUNCTION_CALL,
  EXPR_FIELD_DEREFERENCE,
  //EXPR_TYPE_CAST,
  EXPR_VARIABLE,
  EXPR_STRING_LITERAL,
  EXPR_NUMERIC_LITERAL,
  //EXPR_BINARY,

  EXPR_DEFAULT_VALUE,
};

struct OperatorExpression {
  TokenType op;
  Expression *params;
  int num_params;
};

struct Expression {
  ExpressionType type;
  Token *token;

  union {
    //OperatorExpression operator_expression; // untyped
    //IntrinsicOperation intrinsic;
    FunctionCall       function_call;
    FieldDereference   field_dereference;
    //TypeCast         type_cast;
    VariableReference  variable;
    StringLiteral      string_literal;
    NumericLiteral     numeric_literal;
    //NullLiteral       null_literal;
    //UndefinedLiteral  undefined_literal;
  };
};

TypecheckResult typecheck_expression(Expression &expr);

bool is_success(const TypecheckResult &result) {
  return result.result != RESULT_ERROR;
}

struct ExternFunctionDeclaration {
  SubString identifier;

  TypeDeclaration *retval_type;
  ParamDefinition *params;
  int num_params;

  llvm::Function *llvm_function;
};

TypeDef *expression_get_type(Expression &expr) {

  TypeDeclaration *declaration = NULL;
  switch(expr.type) {
  //case EXPR_INTRINSIC:
  //  return expr.intrinsic.retval_type;
  case EXPR_FUNCTION_CALL:
    if (expr.function_call.function) {
      declaration = expr.function_call.function->retval_type;
    } else if (expr.function_call.extern_function) {
      declaration = expr.function_call.extern_function->retval_type;
    } else {
      assert(expr.function_call.op != OP_NONE);
      switch(expr.function_call.intrinsic) {
        case OP_POINTER_ADD:
          return expression_get_type(expr.function_call.params[0]);
        case OP_POINTER_DEREFERENCE:
          return expression_get_type(expr.function_call.params[0])->pointer.base_type;
        case OP_ARRAY_DEREFERENCE:
          return expression_get_type(expr.function_call.params[0])->array.base_type;
        case OP_ADDRESS_OF:
          return db_get_pointer_type(expression_get_type(expr.function_call.params[0]));
        default:
          halt();
      }
    }
    break;
  case EXPR_VARIABLE:
    declaration = expr.variable.variable->type;
    break;
    //EXPR_TYPE_CAST,
  case EXPR_STRING_LITERAL:
    assert(expr.string_literal.type);
    return expr.string_literal.type;
  case EXPR_NUMERIC_LITERAL:
    assert(expr.numeric_literal.type);
    return expr.numeric_literal.type;
  default:
    halt();
  }

  assert(declaration);
  if(declaration->type == NULL) {
    TypecheckResult result = typecheck_expression(expr);
    assert(is_success(result));
    assert(declaration->type);
  }

  return declaration->type;
}

bool variable_exists(const SubString &identifier) {
  for(auto &var : g.db.variables) {
    if (substring_cmp(var.variable->identifier, identifier) == true) {
      return true;
    }
  }

  return false;
}

SubString to_substring(const char *str) {
  SubString retval;
  retval.start = str;
  retval.length = strlen(str);
  return retval;
}

SubString op_to_substring(TokenType op) {
  switch(op) {
  case TOKEN_ADD_OP:
  case TOKEN_POSITIVE_OP:
    return to_substring("operator +");
    break;
  case TOKEN_SUB_OP:
  case TOKEN_NEGATIVE_OP:
    return to_substring("operator -");
    break;
  case TOKEN_MUL_OP:
  case TOKEN_DEREF_OP:
    return to_substring("operator *");
    break;
  case TOKEN_DIV_OP:
    return to_substring("operator /");
    break;
  case TOKEN_BITWISE_AND_OP:
  case TOKEN_ADDRESS_OP:
    return to_substring("operator &");
    break;
  case TOKEN_BITWISE_OR_OP:
    return to_substring("operator |");
    break;
  case TOKEN_BITWISE_NOT_OP:
    return to_substring("operator ~");
    break;
  case TOKEN_LOGICAL_NOT_OP:
    return to_substring("operator !");
    break;
  case TOKEN_BITWISE_XOR_OP:
    return to_substring("operator ^");
    break;
  case TOKEN_REM_OP:
    return to_substring("operator %");
    break;
  default:
    halt();
  }

  return to_substring("error");
}

bool is_success(const DeclarationResult &result) {
  return result.result == RESULT_SUCCESS;
}

TypecheckResult make_error(const FileLocation &location, const char *format_str, ...) {
  TypecheckResult  result;
  result.result = RESULT_ERROR;
  result.error.location = location;
  result.error.error_string = format_str;
  return result;
}

llvm::Type *llvm_emit_type(TypeDef *type);
llvm::Type *llvm_get_type(TypeDef *type) {
  if (type->llvm_type == NULL) {
    return llvm_emit_type(type);
  }

  return type->llvm_type;
}

enum ProgramStatementType {
  PS_NONE,
  PS_IMPORT,
  PS_EXTERN_FUNCTION,
  PS_FUNCTION,
  PS_VARIABLE,
  PS_STRUCT,
  PS_LLVM_TYPE,
};

enum FunctionStatementType {
  FS_EXTERN_FUNCTION,
  FS_FUNCTION,
  FS_EXPRESSION,
  FS_VARIABLE,
  FS_STRUCT,
  FS_LLVM_TYPE,
  FS_RETURN,
  FS_LLVM,
  FS_ASSIGNMENT,
  //FS_IF,
  //FS_FOR,
};

typedef VariableDefinition FieldDefinition;

struct StructDefinition {
  SubString identifier;

  //Token *tok_fields;
  FieldDefinition *fields;
  int num_fields;
};

struct LLVMTypeDefinition {
  SubString identifier;
  SubString raw_llvm;
};

struct ReturnStatement {
  Expression *retval;
};

struct LLVMStatement {
  SubString raw_llvm;
};

enum AssignmentType {

};

//rule: if a function type only takes a numeric of a specific type, a numeric literal is interpreted as that type, any required truncation is reported as an error
//rule: if a function type takes multiple numeric types, the numeric type is determined by the RHS of the equation
//rule: if there is no LHS or the LHS type is inferred, the type is the smallest size that contains the number, but will report an error if any auto casting occurs in the expression
//
//      f32  x = eitherType(40) + onlyDouble(50) + onlyFloat(60);	// either type is f32, error: truncation of lhs
//      f64  y = eitherType(40) + onlyDouble(50) + onlyFloat(60);	// either type is f64
//      auto z = eitherType(40) + onlyDouble(50) + onlyFloat(60);	// error: ambiguous automatic casting of a numeric literal, specifiy the type of z explicitly or resolve the type of '40' in eitherType
//      eitherType(40) + onlyDouble(50) + onlyFloat(60);			// error: automatic casting of ambiguous type, resolve the type of '40' in eitherType
//      eitherType(40) + onlyFloat(60);								// f32
//      eitherType(40) + onlyDouble(60);							// error: automatic casting of ambiguous type, resolve the type of '40' in eitherType
//      eitherType(40);												// f32
//      eitherType(1e400);										// f64
//      eitherType((f64)40);									// f64
//      f32 x = (f32)eitherType((f64)40);     // calls the f64 version of either type and truncates it to f32


struct AssignmentStatement {
  AssignmentType op;
  Expression *lhs;
  Expression *rhs;
};

struct FunctionStatement {
  FunctionStatementType type;

  union {
    ExternFunctionDeclaration extern_function;
    FunctionDefinition function;
    VariableDefinition varaible;
    StructDefinition struct_def;
    LLVMTypeDefinition llvm_type;
    Expression *expression;
    ReturnStatement return_statement;
    LLVMStatement llvm;
    AssignmentStatement assignment;
    //IfStatement if;
    //ForStatement for;
  };
};

struct ImportStatement {
  SubString file;
};

struct ProgramStatement {
  Token *token;
  ProgramStatementType type;
  union {
    ImportStatement import;
    ExternFunctionDeclaration extern_function;
    FunctionDefinition function;
    VariableDefinition varaible;
    StructDefinition struct_def;
    LLVMTypeDefinition llvm_type;
  };
};

struct Program {
  ProgramStatement *statement;
  int num_statements;
};

bool is_void_pointer(TypeDef *type) {
  if (type->kind != POINTER_TYPE) return false;
  llvm::Type *llvm_type = llvm_get_type(type->pointer.base_type);
  if (llvm_type->isVoidTy()) return true;
  return false;
}

bool is_pointer(TypeDef *type) {
  return type->kind == POINTER_TYPE;
}

bool is_float(TypeDef *type) {
  if (type->kind != LLVM_TYPE) return false;
  llvm::Type *llvm_type = llvm_get_type(type);
  return llvm_type->isFloatTy();
  // check against llvm type for integerness
}

bool is_integer(TypeDef *type) {
  if (type->kind == ARBITRARY_INTEGER) return true;
  if (type->kind != LLVM_TYPE) return false;
  llvm::Type *llvm_type = llvm_get_type(type);
  return llvm_type->isIntegerTy();
  // check against llvm type for integerness
}

bool types_can_be_auto_cast(TypeDef *target, TypeDef *source) {
  if (source->kind == ARRAY_TYPE && target->kind == POINTER_TYPE && source->array.base_type == target->pointer.base_type) {
    return true;
  }

  return source == target;
}

TypecheckResult make_result(const TypecheckResult &result_lhs, const TypecheckResult &result_rhs) {
  if (result_lhs.type->kind == ARBITRARY_INTEGER) {
    if (is_integer(result_rhs.type)) return TYPECHECK_SUCCESS;
    return make_error(result_lhs.loaction, "Type mismatch)");
  } else if (result_rhs.type->kind == ARBITRARY_INTEGER) {
    if (is_integer(result_rhs.type)) return TYPECHECK_SUCCESS;
    return make_error(result_lhs.loaction, "Type mismatch)");
  }

  bool can_convert = types_can_be_auto_cast(result_lhs.type, result_rhs.type);

  if (can_convert == false) {
    return make_error(result_lhs.loaction, "Type mismatch"); //: %s, %s", to_string(result_lhs), to_string(result_rhs));
  }
  return TYPECHECK_SUCCESS;
}
 
struct LLVMTypeResult {
  ResultType result;
  llvm::Type *type;
  Error error;
};

llvm::Type *llvm_get_type(TypeDef *type);

bool is_boolean(TypeDef *type) {
  if (type->kind != LLVM_TYPE) return false;
  llvm::Type *llvm_type = llvm_get_type(type);

  return llvm_type ->isIntegerTy() &&
    llvm_type->getScalarSizeInBits() == 1;
}
//
//
bool is_void(TypeDef *type) {
  if (type->kind != LLVM_TYPE) return false;
  llvm::Type *llvm_type = llvm_get_type(type);
  return llvm_type->isVoidTy();
  // check against llvm type for integerness
}

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

struct DigestResult {
  ResultType result;
  Error error;
};

DigestResult DIGEST_SUCCESS = {};

bool is_success(const DigestResult &result) {
  return result.result == RESULT_SUCCESS;
}

TypeDeclaration *type_decl_alloc(int num_decls) {
  return (TypeDeclaration *)calloc(sizeof(TypeDeclaration) * num_decls, 1);
}

DigestResult digest_type_declaration(Token *token, TypeDeclaration &decl);
///*
DigestResult digest_pointer_type(Token *declaration, TypeDeclaration &type) {
  Token *tok_subtype;
  expand_tokens(declaration, tok_subtype);

  type.kind = TYPE_POINTER;
  type.pointer_type.sub_type = type_decl_alloc(1);
  DigestResult result = digest_type_declaration(tok_subtype, *type.pointer_type.sub_type);
  if (is_success(result)) return result;

  return DIGEST_SUCCESS;
}

Expression *expressions_alloc(int num_params) {
  if (num_params == 0) return NULL;
  return (Expression *)calloc(sizeof(Expression) * num_params, 1);
}

DigestResult digest_expression(Token *expression, Expression &expr);

DigestResult digest_array_type(Token *declaration, TypeDeclaration &type) {
  Token *tok_subtype, *tok_count;
  expand_tokens(declaration, tok_subtype, tok_count);

  type.kind = TYPE_ARRAY;
  type.array_type.sub_type = type_decl_alloc(1);
  DigestResult subtype_result = digest_type_declaration(tok_subtype, *type.array_type.sub_type);
  if (is_success(subtype_result) == false) return subtype_result;
  
  type.array_type.count_expression = expressions_alloc(1);
  DigestResult count_result = digest_expression(tok_count, *type.array_type.count_expression);
  if (is_success(count_result) == false) return count_result;

  return DIGEST_SUCCESS;
}

DigestResult digest_function_type(Token *declaration, TypeDeclaration &type) {
  Token *tok_retval, *tok_params;
  expand_tokens(declaration, tok_retval, tok_params);
  //Token *subtokens[2];
  //int num_subtokens = expand_tokens(declaration, subtokens, 2);

  type.kind = TYPE_FUNCTION;
  type.function_type.num_params = count_subtokens(tok_params);
  type.function_type.params = type_decl_alloc(type.function_type.num_params);

  int i = 0;
  for(Token *param = tok_params->start; param != NULL; param = param->next) {
    TypeDeclaration &param_decl = type.function_type.params[i++];
    DigestResult result = digest_type_declaration(param, param_decl);
    if (is_success(result) == false) return result;
  }

  return DIGEST_SUCCESS;
}

TypeDef *emit_base_type(Token *type_identifier) {
  TypeDef *retval = db_lookup_type_by_identifier(type_identifier->substring);
  if (retval == NULL) {
    halt();
  }

  return retval;
}

DigestResult digest_base_type(Token *type_identifier, TypeDeclaration &type) {
  type.kind = TYPE_BASE;
  type.base_type.identifier = type_identifier->substring;
  return DIGEST_SUCCESS;
  //TypeDef *retval = db_lookup_type_by_identifier(type_identifier->substring);
  //if (retval == NULL) {
  //  return make_error(UNKNOWN_LOCATION, "Unknown type");
  //}

  //return DIGEST_SUCCESS;
}

TypeDef *emit_type_declaration(Token *declaration) {
  switch(declaration->type) {
  case TOKEN_IDENTIFIER:
    return emit_base_type(declaration);
  case TOKEN_POINTER_TYPE:
    return NULL; //emit_pointer_type(declaration);
  case TOKEN_ARRAY_TYPE:
    return NULL; //emit_array_type(declaration);
  case TOKEN_FUNCTION_TYPE:
    return NULL; //emit_function_type(declaration);
  //case TOKEN_PARAMETERIZED_TYPE:
  //  return emit_pointer_type(declaration);
  default:
    halt();
  }

  return NULL;
}
//*/
TypecheckResult typecheck_variable_definition(VariableDefinition &variable, bool add_variable);

enum ConversionType {
  array_to_pointer,
  up_convert_integer,
  up_convert_float,
  pointer_to_void_pointer,
};

const int MAX_CONVERSIONS = 8;

struct ConversionChain {
  int tier;
  ConversionType chain[MAX_CONVERSIONS];
  int num_conversions;
};

TypeDef *apply_conversion(ConversionType type, TypeDef *source, TypeDef *dest= NULL) {
  switch(type) {
  case array_to_pointer: 
    return db_get_pointer_type(source->array.base_type);
  case up_convert_integer:
    return dest;
  case up_convert_float:
    return dest;
  case pointer_to_void_pointer:
    return dest;
  default:
    halt();
  }

  return NULL;
}

int type_size(TypeDef *type) {
  if (type->kind == LLVM_TYPE) {
    llvm::Type *llvm_type = llvm_get_type(type);
    uint32_t size = llvm_type->getScalarSizeInBits();
    return (int)size;
  }

  halt();
  return 0;
}

bool is_array(TypeDef *type) {
  return type->kind == ARRAY_TYPE;
}

bool can_convert(TypeDef *source, TypeDef *dest, ConversionChain &conversion) {
  conversion.num_conversions = 0;

  if (is_array(source) && is_pointer(dest)) {
    conversion.chain[conversion.num_conversions++] = array_to_pointer;
    source = apply_conversion(array_to_pointer, source);
  }

  if (is_integer(source) && is_integer(dest) && type_size(source) < type_size(dest)) {
    conversion.chain[conversion.num_conversions++] = up_convert_integer;
    source = apply_conversion(up_convert_integer, source, dest);
  }

  if (is_float(source) && is_float(dest) && type_size(source) < type_size(dest)) {
    conversion.chain[conversion.num_conversions++] = up_convert_float;
    source = apply_conversion(up_convert_float, source, dest);
  }

  if (is_pointer(source) && is_void_pointer(dest)) {
    conversion.chain[conversion.num_conversions++] = pointer_to_void_pointer;
    source = apply_conversion(pointer_to_void_pointer, source);
  }

  // TODO: user defined

  if (source == dest) return true;
  return false;
}

bool compare_function_param_types(Expression *call_params, ParamDefinition *function_params, int num_params, ConversionChain *conversions) {
  for(int i = 0; i < num_params; ++i) {
    assert(function_params[i].type != PD_VARARGS);
    TypeDef *t0 = expression_get_type(call_params[i]);
    TypeDef *t1 = function_params[i].variable.type->type;

    assert(t0);

    if (t1 == NULL) {
      typecheck_variable_definition(function_params[i].variable, false);
      t1 = function_params[i].variable.type->type;
    }
    assert(t1);

    if (can_convert(t0, t1, conversions[i]) == false) {
      return false;
    }
  }

  return true;
}

const FileLocation UNKNOWN_LOCATION = {};

TypeDef *pointer_type_get(TypeDef *base_type) {
  for(auto &type : g.db.types) {
    if (type.kind == POINTER_TYPE && type.pointer.base_type == base_type) {
      return &type;
    }
  }

  TypeDef new_type = {};
  new_type.kind = POINTER_TYPE;
  new_type.pointer.base_type = base_type;
  g.db.types.push_back(new_type);
  return &g.db.types.back();
}

TypeDef *array_type_get(TypeDef *base_type, int count) {
  for(auto &type : g.db.types) {
    if (type.kind == ARRAY_TYPE && type.array.base_type == base_type && type.array.count == count) {
      return &type;
    }
  }

  TypeDef new_type = {};
  new_type.kind = ARRAY_TYPE;
  new_type.array.base_type = base_type;
  new_type.array.count = count;
  g.db.types.push_back(new_type);
  return &g.db.types.back();
}

enum FunctionLookupResultType {
  LOOKUP_NONE,
  LOOKUP_ERROR,
  LOOKUP_FUNCTION,
  LOOKUP_EXTERN_FUNCTION,
  LOOKUP_INTRINSIC,
};

struct FunctionLookupResult {
  FunctionLookupResultType type;

  union {
    ExternFunctionDeclaration *extern_function;
    FunctionDefinition      *function;
    IntrinsicOperationType  intrinsic;
  };
};

//TypeDef *apply_conversion(ConversionType type, Expression *params) {
//}

const int MAX_MATCHES = 256;

struct FunctionMatch {
  FunctionLookupResult result;
  ConversionChain conversions[MAX_PARAMS];
};

#define Max(x, y) (((x) > (y)) ? (x) : (y))

bool compare_conversions(ConversionChain *conversions0, ConversionChain *conversions1, int num_params) {
  int max0Conversion = 0;
  int max1Conversion = 0;

  for(int i = 0; i < num_params; ++i) {
    max0Conversion = Max(max0Conversion, conversions0[i].tier);
    max1Conversion = Max(max1Conversion, conversions1[i].tier);
  }

  if (max0Conversion > max1Conversion) return true;
  return false;
}

void function_match_add(FunctionMatch &best_match, const FunctionLookupResult &result, ConversionChain *conversions, int num_params) {
  if (best_match.result.type == LOOKUP_NONE || compare_conversions(best_match.conversions, conversions, num_params)) {
    best_match.result = result;
    memcpy(best_match.conversions, conversions, sizeof(ConversionChain) * MAX_PARAMS);
  }
}

void create_conversion_expression(ConversionType type, Expression &expression, Expression &original_expression, TypeDef *dest_type) {
  switch(type) {
  case array_to_pointer: 
    expression.type = EXPR_FUNCTION_CALL;
    expression.function_call.intrinsic = OP_CAST_ARRAY_TO_POINTER;
    expression.function_call.params = &original_expression;
    expression.function_call.num_params = 1;
    break;
  case up_convert_integer:
    expression.type = EXPR_FUNCTION_CALL;
    expression.function_call.intrinsic = OP_EXTEND_INTEGER;
    expression.function_call.params = &original_expression;
    expression.function_call.num_params = 1;
    expression.function_call.dest_type = dest_type;
    break;
  case up_convert_float:
    expression.type = EXPR_FUNCTION_CALL;
    expression.function_call.intrinsic = OP_EXTEND_FLOAT;
    expression.function_call.params = &original_expression;
    expression.function_call.num_params = 1;
    expression.function_call.dest_type = dest_type;
    break;
  case pointer_to_void_pointer:
    expression.type = EXPR_FUNCTION_CALL;
    expression.function_call.intrinsic = OP_EXTEND_FLOAT;
    expression.function_call.params = &original_expression;
    expression.function_call.num_params = 1;
    expression.function_call.dest_type = dest_type;
    break;
  default:
    halt();
  }
}

void apply_conversion(const ConversionChain &conversion, Expression &expression, ParamDefinition &dest_param) {
  if (conversion.num_conversions == 0) return;
  
  for(int i = 0; i < conversion.num_conversions; ++i) {
    Expression *new_expression = expressions_alloc(1);
    *new_expression = expression;

    assert(dest_param.type == PD_VARIABLE);
    TypeDef *dest_type = dest_param.variable.type->type;
    create_conversion_expression(conversion.chain[i], expression, *new_expression, dest_type);
  }
}

void apply_conversions(ConversionChain *conversions, Expression *call_params, ParamDefinition *function_params, int num_params) {
  for(int i = 0; i < num_params; ++i) {
    apply_conversion(conversions[i], call_params[i], function_params[i]);
  }
}


FunctionLookupResult db_function_call_lookup(const SubString &identifier, TokenType op, Expression *params, int num_params) {
  FunctionLookupResult error = {LOOKUP_ERROR};
  FunctionMatch best_match = {};
  ConversionChain conversions[MAX_PARAMS] = {};

  for(FunctionDefinitionDBEntry &entry : g.db.functions) {
    if (entry.function) {
      if (entry.function->num_params && entry.function->params[entry.function->num_params-1].type == PD_VARARGS) {
        if (substring_cmp(entry.function->identifier, identifier) && entry.function->num_params-1 <= num_params) {
          if (compare_function_param_types(params, entry.function->params, entry.function->num_params-1, conversions) == true) {
            FunctionLookupResult retval;
            retval.type = LOOKUP_FUNCTION;
            retval.function = entry.function;
            function_match_add(best_match, retval, conversions, num_params);
          }
        }
      } else {
        if (substring_cmp(entry.function->identifier, identifier) && entry.function->num_params == num_params) {
          if (compare_function_param_types(params, entry.function->params, num_params, conversions) == true) {
            FunctionLookupResult retval;
            retval.type = LOOKUP_FUNCTION;
            retval.function = entry.function;
            //return retval;
            function_match_add(best_match, retval, conversions, num_params);
          }
        }
      }
    } else {
      assert(entry.extern_function);
      if (entry.extern_function->num_params && entry.extern_function->params[entry.extern_function->num_params-1].type == PD_VARARGS) {
        if (substring_cmp(entry.extern_function->identifier, identifier) && entry.extern_function->num_params-1 <= num_params) {
          if (compare_function_param_types(params, entry.extern_function->params, entry.extern_function->num_params-1, conversions) == true) {
            FunctionLookupResult retval;
            retval.type = LOOKUP_EXTERN_FUNCTION;
            retval.extern_function = entry.extern_function;
            //return retval;
            function_match_add(best_match, retval, conversions, num_params);
          }
        }
      }
      else {
        if (substring_cmp(entry.extern_function->identifier, identifier) && entry.extern_function->num_params == num_params) {
          if (compare_function_param_types(params, entry.extern_function->params, num_params, conversions) == true) {
            FunctionLookupResult retval;
            retval.type = LOOKUP_EXTERN_FUNCTION;
            retval.extern_function = entry.extern_function;
            //return retval;
            function_match_add(best_match, retval, conversions, num_params);
          }
        }
      }
    }
  }

  if (best_match.result.type != LOOKUP_NONE) {
    ParamDefinition *function_params;
    if (best_match.result.type == LOOKUP_EXTERN_FUNCTION) {
      function_params = best_match.result.extern_function->params;
    } else {
      function_params = best_match.result.function->params;
    }

    apply_conversions(best_match.conversions, params, function_params, num_params);
    return best_match.result;
  }

  if (op == TOKEN_NONE) {
    return error;
  }

  //OP_POINTER_ADD,
  //OP_POINTER_DEREFERENCE,
  //OP_ARRAY_DEREFERENCE,

  if (num_params == 1) {
    TypeDef *type0 = expression_get_type(params[0]);

    if (op == TOKEN_DEREF_OP && is_pointer(type0)) {
      FunctionLookupResult retval;
      retval.type = LOOKUP_INTRINSIC;
      retval.intrinsic = OP_POINTER_DEREFERENCE;
      return retval;
    } else if (op == TOKEN_ADDRESS_OP) {
      FunctionLookupResult retval;
      retval.type = LOOKUP_INTRINSIC;
      retval.intrinsic = OP_ADDRESS_OF;
      return retval;
    } else {
      return error;
    }
  } else if (num_params == 2) {
    TypeDef *type0 = expression_get_type(params[0]);
    TypeDef *type1 = expression_get_type(params[1]);

    if (op == TOKEN_ADD_OP && is_pointer(type0) && is_integer(type1))  {
      FunctionLookupResult retval;
      retval.type = LOOKUP_INTRINSIC;
      retval.intrinsic = OP_POINTER_ADD;
      return retval;
    } else if (op == TOKEN_OP_ARRAY_INDEX) {
      FunctionLookupResult retval;
      retval.type = LOOKUP_INTRINSIC;
      retval.intrinsic = OP_ARRAY_DEREFERENCE;
      return retval;
    } else {
      return error;
    }
  } else {
    return error;
  }


}

TypeDef *db_lookup_type(const SubString &identifier) {
  for(TypeDef &type : g.db.types) {
    if (substring_cmp(type.identifier, identifier)) {
      return &type;
    }
  }

  return NULL;
}

bool compare_type_definition(TypeDef *type0, TypeDef *type1) {
  return type0 == type1;
}

TypecheckResult typecheck_type_declaration(TypeDeclaration &decl);

TypecheckResult compare_type_declaration(TypeDeclaration &decl0, TypeDeclaration &decl1, bool &result) {
  TypecheckResult r0 = typecheck_type_declaration(decl0);
  if (is_success(r0) == false) return r0;

  TypecheckResult r1 = typecheck_type_declaration(decl1);
  if (is_success(r1) == false) return r1;

  result = compare_type_definition(decl1.type, decl1.type);
  return TYPECHECK_SUCCESS;
}

TypecheckResult compare_function_params(ParamDefinition *function_params, ParamDefinition *params, int num_params, bool &types_equal) {
  types_equal = true;

  for(int i = 0; i < num_params; ++i) {
    ParamDefinition &function_param = function_params[i];
    ParamDefinition &param = params[i];

    assert(function_param.type == PD_VARIABLE);
    assert(param.type == PD_VARIABLE);

    TypecheckResult result = compare_type_declaration(*param.variable.type, *function_param.variable.type, types_equal);
    if (is_success(result)) return result;

    if (types_equal == false) return TYPECHECK_SUCCESS;
  }

  return TYPECHECK_SUCCESS;
}

SubString dbfunction_get_identifier(FunctionDefinitionDBEntry &new_function) {
  if (new_function.extern_function) return new_function.extern_function->identifier;
  else return new_function.function->identifier;
}

int dbfunction_get_num_params(FunctionDefinitionDBEntry &new_function) {
  if (new_function.extern_function) return new_function.extern_function->num_params;
  else return new_function.function->num_params;
}

ParamDefinition *dbfunction_get_params(FunctionDefinitionDBEntry &new_function) {
  if (new_function.extern_function) return new_function.extern_function->params;
  else return new_function.function->params;
}


TypecheckResult db_lookup_function(const SubString &identifier, ParamDefinition *params, int num_params, FunctionDefinitionDBEntry *&result) {
  for(FunctionDefinitionDBEntry &function : g.db.functions) {
    SubString function_identifier = dbfunction_get_identifier(function);
    if (substring_cmp(function_identifier, identifier)) {
      int function_num_params = dbfunction_get_num_params(function);
      ParamDefinition *function_params = dbfunction_get_params(function);
      if (num_params != function_num_params) continue;

      bool types_equal;
      TypecheckResult param_result = compare_function_params(function_params, params, num_params, types_equal);
      if (is_success(param_result) == false) return param_result;

      if (types_equal == false) continue;
      result = &function;
      return TYPECHECK_SUCCESS;
    }
  }

  result = NULL;
  return TYPECHECK_SUCCESS;
}

VariableDefinition *db_lookup_variable(const SubString &identifier) {
  for(auto &varaible : g.db.variables) {
    if (substring_cmp(varaible.variable->identifier, identifier)) {
      return varaible.variable;
    }
  }
  
  return NULL;
}

TypecheckResult typecheck_function_definition(FunctionDefinition &function);

TypecheckResult typecheck_function_call_expression(FunctionCall &call) {
  for(int i = 0; i < call.num_params; ++i)
  {
    Expression &call_param = call.params[i];
    TypecheckResult result = typecheck_expression(call_param);
    if (is_success(result) == false) return result;
  }

  FunctionLookupResult lookup_result = db_function_call_lookup(call.identifier, call.op, call.params, call.num_params);

  switch(lookup_result.type) {
  case LOOKUP_ERROR:
    return make_error(UNKNOWN_LOCATION, "Undefined function");
  case LOOKUP_FUNCTION:
    call.function   = lookup_result.function;
    if (call.function->retval_type->type == NULL) {
      TypecheckResult result = typecheck_type_declaration(*call.function->retval_type);
      if (is_success(result) == false) return result;
    }
    break;
  case LOOKUP_EXTERN_FUNCTION:
    call.extern_function = lookup_result.extern_function;
    if (call.extern_function->retval_type->type == NULL) {
      TypecheckResult result = typecheck_type_declaration(*call.extern_function->retval_type);
      if (is_success(result) == false) return result;
    }
    break;
  case LOOKUP_INTRINSIC:
    call.intrinsic  = lookup_result.intrinsic;
    break;
  default:
    halt();
  }

  return TYPECHECK_SUCCESS;
}

bool is_compile_time(TypeDef *) {
  // TODO
  return true;
}

bool is_compile_time_integer(TypeDef *type) {
  return is_compile_time(type) && is_integer(type);
}

TypecheckResult typecheck_array_dereference_expression(FunctionCall &expr){
  assert(expr.op == OP_POINTER_DEREFERENCE);
  assert(expr.num_params == 2);

  TypecheckResult result_base = typecheck_expression(expr.params[0]);
  if (is_success(result_base) == false) return result_base;
  TypeDef *type_base = expression_get_type(expr.params[0]);

  if (is_array(type_base) == false &&
    is_pointer(type_base) == false) {
    return make_error(UNKNOWN_LOCATION, "Expected array or pointer");
  }

  TypecheckResult result_index = typecheck_expression(expr.params[1]);
  if (is_success(result_index) == false) return result_index;
  TypeDef *type_index = expression_get_type(expr.params[1]);

  if (is_compile_time_integer(type_index) == false) {
    return make_error(UNKNOWN_LOCATION, "Expected compile time integer");
  }

  return TYPECHECK_SUCCESS;
}

TypeDef *db_lookup_integer_type(int numBits) {
  for(auto &type : g.db.types) {
    if (type.kind == LLVM_TYPE && substring_cmp(type.llvm_def.type_definition.identifier, "i32") == true) {
        return &type;
    }
  }

  return NULL;
}

TypeDef *db_lookup_arbitrary_integer() {
  for(auto &type : g.db.types) {
    if (type.kind == ARBITRARY_INTEGER) {
      return &type;
    }
  }

  return NULL;
}

TypeDef *integer_type_lookup(const char *str) {
  for(auto &type : g.db.types) {
    if (type.kind == LLVM_TYPE && substring_cmp(type.token->start->substring, str)) {
      return &type;
    }
  }

  return NULL;
};

TypeDef *char_type_lookup() {
  TypeDef *retval = integer_type_lookup("char");
  assert(retval);
  return retval;
}

TypeDef *calc_string_literal_type(SubString &str) {
  TypeDef *char_type  = char_type_lookup();
  TypeDef *array_type = array_type_get(char_type, str.length + 1);
  return array_type;
}

TypecheckResult typecheck_string_literal(StringLiteral &literal) {
  if (literal.type == NULL) {
    literal.type = calc_string_literal_type(literal.literal);
  }

  return TYPECHECK_SUCCESS;
}

TypecheckResult typecheck_integer_literal(NumericLiteral &literal) {
  //llvm::Type *integer_type = llvm::IntegerType(g.llvm.context, 32);
  //TypeDef *type = db_lookup_integer_type(32);
  //TypeDef *type = db_lookup_arbitrary_integer();
  //Type *type = new Type();
  //type->kind = BASE_TYPE;
  //type->llvm_type = integer_type;

  if (literal.type == NULL) {
    // TODO
    literal.type = integer_type_lookup("i32");
  }

  return TYPECHECK_SUCCESS;

  //TypeDef *type = calc_integer_literal_type(literal);
  //return make_result(literal->location, type); //make_result(integer_type);
}

TypecheckResult typecheck_identifier(VariableReference &variable) {
  if (variable.variable == NULL) {
    variable.variable = db_lookup_variable(variable.identifier);
    assert(variable.variable);
  }

  return TYPECHECK_SUCCESS;
}

TypecheckResult typecheck_expression(Expression &expr) {
  switch(expr.type) {
  //case EXPR_OPERATOR:
  //  return typecheck_integer_literal(expr);
  case EXPR_FUNCTION_CALL:
    return typecheck_function_call_expression(expr.function_call);
  case EXPR_NUMERIC_LITERAL:
    return typecheck_integer_literal(expr.numeric_literal);
  case EXPR_STRING_LITERAL:
    return typecheck_string_literal(expr.string_literal);
  case EXPR_VARIABLE:
    return typecheck_identifier(expr.variable);
  default:
    halt();
  }

  return make_error(UNKNOWN_LOCATION, "Unknown expression");
}

TypecheckResult typecheck_base_type(TypeDeclaration &decl) {
  decl.type = db_lookup_type(decl.base_type.identifier);
  if (decl.type == NULL) {
    return make_error(UNKNOWN_LOCATION, "Unknown type");
  }

  return TYPECHECK_SUCCESS;
}

int64_t eval_constexpr(Expression &expression) {
  assert(expression.type == EXPR_NUMERIC_LITERAL); // TODO more of these
  return (int64_t)substring_to_uint64(expression.numeric_literal.literal);
}

TypecheckResult typecheck_pointer_type(TypeDeclaration &decl) {
  TypecheckResult result = typecheck_type_declaration(*decl.pointer_type.sub_type);
  if (is_success(result) == false) return result;

  decl.type = db_get_pointer_type(decl.pointer_type.sub_type->type);

  return TYPECHECK_SUCCESS;
}

TypecheckResult typecheck_array_type(TypeDeclaration &decl) {
  TypecheckResult result = typecheck_type_declaration(*decl.array_type.sub_type);
  if (is_success(result) == false) return result;

  TypecheckResult result_count = typecheck_expression(*decl.array_type.count_expression);
  if (is_success(result_count) == false) return result_count;

  TypeDef *count_type = expression_get_type(*decl.array_type.count_expression);
  if (is_compile_time_integer(count_type) == false) {
    return make_error(UNKNOWN_LOCATION, "Type mismatch: Array count must be a constant integer");
  }

  int64_t count = eval_constexpr(*decl.array_type.count_expression);

  decl.type = db_get_array_type(decl.array_type.sub_type->type, count);

  return TYPECHECK_SUCCESS;
}

TypeDef *db_get_function_type(TypeDeclaration *retval, TypeDeclaration *params, int num_params) {
  halt();
  return NULL;
}

TypecheckResult typecheck_function_type(TypeDeclaration &decl) {
  TypecheckResult result = typecheck_type_declaration(*decl.function_type.retval);
  if (is_success(result) == false) return result;

  for(int i = 0; i < decl.function_type.num_params; ++i) {
    TypecheckResult result = typecheck_type_declaration(decl.function_type.params[i]);
  }

  decl.type = db_get_function_type(decl.function_type.retval, decl.function_type.params, decl.function_type.num_params);

  return TYPECHECK_SUCCESS;
}

TypeDef *TYPE_DEF_ACTIVE = (TypeDef *)-1;

TypecheckResult typecheck_type_declaration(TypeDeclaration &decl) {

  if (decl.type == TYPE_DEF_ACTIVE) {
    return make_error(UNKNOWN_LOCATION, "Recursive type definition");
  } else if (decl.type != NULL) {
    return TYPECHECK_SUCCESS;
  }

  decl.type = TYPE_DEF_ACTIVE;

  switch(decl.kind) {
  case TYPE_BASE:
    return typecheck_base_type(decl);
  case TYPE_POINTER:
    return typecheck_pointer_type(decl);
  case TYPE_ARRAY:
    return typecheck_array_type(decl);
  case TYPE_FUNCTION:
    return typecheck_function_type(decl);
  default:
    halt();
  }

  return TYPECHECK_SUCCESS;
}

bool is_auto_type(TypeDef *type) {
  return false;
}

TypecheckResult typecheck_assignment(TypeDef *type, Expression *&expression);

TypecheckResult db_try_add_variable(VaraibleDefinitionDBEntry &new_variable) {
  VariableDefinition *existing_variable = db_lookup_variable(new_variable.variable->identifier);
  if (existing_variable) {
    return make_error(UNKNOWN_LOCATION, "Variable redefinition");
  }

  g.db.variables.push_back(new_variable);

  return TYPECHECK_SUCCESS;
}

TypecheckResult db_add_variable_declaration(VariableDefinition &variable) {
  VaraibleDefinitionDBEntry new_entry = {};
  new_entry.scope = g.db.scope;
  new_entry.variable = &variable;

  return db_try_add_variable(new_entry);
}

TypecheckResult typecheck_variable_definition(VariableDefinition &variable, bool add_variable) {
  //assert(variable.type);

  if (g.db.scope != 0 && add_variable) {
    TypecheckResult result_add = db_add_variable_declaration(variable);
    if (is_success(result_add) == false) return result_add;
  }

  TypecheckResult result_type = typecheck_type_declaration(*variable.type);
  if (is_success(result_type) == false) return result_type;

  if (variable.initial_value) {
    if (variable.initial_value->type == EXPR_DEFAULT_VALUE) {
      if (is_auto_type(variable.type->type)) {
        return make_error(UNKNOWN_LOCATION, "Unintialized auto deduced variable");
      } else {
        return TYPECHECK_SUCCESS;
      }
    }

    TypecheckResult result = typecheck_expression(*variable.initial_value);
    if (is_success(result) == false) return result;

    return typecheck_assignment(variable.type->type, variable.initial_value);
  }

  return TYPECHECK_SUCCESS;
}

FieldDefinition *fields_alloc(int num_params) {
  return (FieldDefinition *)malloc(sizeof(FieldDefinition) * num_params);
}

TypecheckResult typecheck_struct_definition(StructDefinition &struct_def) {

  for(int i = 0; i < struct_def.num_fields; ++i) {
    VariableDefinition &field_def = struct_def.fields[i];
    TypecheckResult result = typecheck_variable_definition(field_def, false);
    if (is_success(result)) return result;
  }

  return TYPECHECK_SUCCESS;
}

TypecheckResult typecheck_function_statement(TypeDef *retval_type, FunctionStatement &statement);

// rules for type conversion:
// int small can go to int big
// float small can go to float big
// array[size] can go to pointer
// any pointer can go to void *
// null can go to any pointer


bool try_auto_cast(TypeDef *rhs_type, TypeDef *lhs_type, Expression *&expression) {
  if (is_pointer(rhs_type) && is_array(lhs_type) &&
      rhs_type->pointer.base_type == lhs_type->pointer.base_type)
  {
    Expression *cast = expressions_alloc(1);
    cast->type = EXPR_FUNCTION_CALL;
    cast->function_call.intrinsic = OP_CAST_ARRAY_TO_POINTER;
    cast->function_call.params = expression;
    cast->function_call.num_params = 1;
    cast->function_call.identifier = to_substring("auto_cast");
    expression = cast;
    return true;
  }
  else {
    return false;
  }

}


TypecheckResult typecheck_assignment(TypeDef *type, Expression *&expression) {
  TypeDef *expression_type = expression_get_type(*expression);
  if (expression_type != type) {
    // TODO auto-casting

    if (try_auto_cast(type, expression_type, expression)) {
      return TYPECHECK_SUCCESS;
    }

    return make_error(UNKNOWN_LOCATION, "Type mismatch");
  }

  return TYPECHECK_SUCCESS;
}

TypecheckResult typecheck_return_statement(TypeDef *retval_type, ReturnStatement &statement) {
  TypecheckResult result_rhs = typecheck_expression(*statement.retval);
  if (is_success(result_rhs) == false) return result_rhs;

  return typecheck_assignment(retval_type, statement.retval);

  //TypecheckResult result = typecheck_assigment(statement->location, retval_type, expression_get_type(*fs.return_statement.retval));
  //return result;
}

TypecheckResult typecheck_assignment_statement(AssignmentStatement &statement) {

  TypecheckResult result_lhs = typecheck_expression(*statement.rhs);
  if (is_success(result_lhs) == false) return result_lhs;

  TypecheckResult result_rhs = typecheck_expression(*statement.lhs);
  if (is_success(result_rhs) == false) return result_rhs;

  TypeDef *lhs_type = expression_get_type(*statement.lhs);
  
  return typecheck_assignment(lhs_type, statement.rhs);

  //TypecheckResult result = typecheck_assigment(statement->location, retval_type, expression_get_type(*fs.return_statement.retval));
  //return result;
}

Expression *call_params_alloc(int num_params) {
  if (num_params == 0) return NULL;
  return expressions_alloc(num_params);
  //return (Expression *)malloc(sizeof(Expression) * num_params);
}

ParamDefinition *params_alloc(int num_params) {
  if (num_params == 0) return NULL;
  return (ParamDefinition *)calloc(sizeof(ParamDefinition) * num_params, 1);
}

FunctionStatement *function_body_alloc(int num_params) {
  return (FunctionStatement *)calloc(sizeof(FunctionStatement) * num_params, 1);
}

TypecheckResult typecheck_function_params(ParamDefinition *params, int num_params) {
  for(int i = 0; i < num_params; ++i) {
    if (params[i].type == PD_VARARGS) {
      assert(i == num_params-1);
      return TYPECHECK_SUCCESS;
    }

    TypecheckResult result = typecheck_variable_definition(params[i].variable, true);
    if (is_success(result) == false) return result;
  }

  return TYPECHECK_SUCCESS;
}

TypecheckResult typecheck_function_statement(TypeDef *retval_type, FunctionStatement &statement) {
  switch(statement.type) {
    //case TOKEN_IF_STATEMENT:
    //  return typecheck_if_statement(statement_token, retval_type);
    //case TOKEN_FOR_STATEMENT:
    //  return typecheck_for_statement(statement, result_retval);
    //case TOKEN_SWITCH_STATEMENT:
    //  return typecheck_switch_statement(statement, result_retval);
  case FS_RETURN:
    return typecheck_return_statement(retval_type, statement.return_statement);
  case FS_EXPRESSION:
    return typecheck_expression(*statement.expression);
  case FS_ASSIGNMENT:
    return typecheck_assignment_statement(statement.assignment);
  case FS_VARIABLE:
    return typecheck_variable_definition(statement.varaible, true);
  case FS_LLVM:
    return TYPECHECK_SUCCESS;
  default:
    halt();
  }

  //return make_error(statement_token->location, "Unknown statement.");

  return TYPECHECK_SUCCESS;
}

TypecheckResult typecheck_function_body(TypeDef *retval_type, FunctionStatement *body, int num_statements) {

  bool return_statement_verified = false;
  int i = 0;
  for(int i = 0; i < num_statements; ++i) {
    FunctionStatement &statement = body[i];
    TypecheckResult result = typecheck_function_statement(retval_type, statement);
    if (is_success(result) == false) return result;

    if (statement.type == FS_RETURN) {
      // todo handle multiple returns, "all paths much return"
      return_statement_verified = true; 
    }
  }

  if (return_statement_verified == false && !is_void(retval_type)) {
    return make_error(UNKNOWN_LOCATION, "Missing return statement");
  }

  return TYPECHECK_SUCCESS;
}

DigestResult digest_type_declaration(Token *token, TypeDeclaration &decl) {

  switch(token->type) {
  case TOKEN_IDENTIFIER:
    return digest_base_type(token, decl);
  case TOKEN_POINTER_TYPE:
    return digest_pointer_type(token, decl);
  case TOKEN_ARRAY_TYPE:
    return digest_array_type(token, decl);
  case TOKEN_FUNCTION_TYPE:
    return digest_function_type(token, decl);
    //case TOKEN_PARAMETERIZED_TYPE:
    //  return emit_pointer_type(declaration);
  default:
    halt();
  }

  return DIGEST_SUCCESS;
}

TypecheckResult db_try_add_function(FunctionDefinitionDBEntry &new_function) {
  SubString identifier    = dbfunction_get_identifier(new_function);
  int num_params          = dbfunction_get_num_params(new_function);
  ParamDefinition *params = dbfunction_get_params(new_function);

  assert(identifier.length > 0);

  FunctionDefinitionDBEntry *existing_fn;
  TypecheckResult result = db_lookup_function(identifier, params, num_params, existing_fn);

  if (is_success(result) == false) {
    return result;
  }

  if (existing_fn) {
    return make_error(UNKNOWN_LOCATION, "Function redefinition");
  }

  g.db.functions.push_back(new_function);
  return TYPECHECK_SUCCESS;
}

TypecheckResult db_add_function_declaration(FunctionDefinition &function) {
  FunctionDefinitionDBEntry new_entry = {};
  new_entry.scope = g.db.scope;
  new_entry.function = &function;
  return db_try_add_function(new_entry);
}

TypecheckResult typecheck_function_definition(FunctionDefinition &function) {
  TypecheckResult result_retval = typecheck_type_declaration(*function.retval_type);
  if (is_success(result_retval) == false) return result_retval;

  db_push_scope();

  TypecheckResult result_params = typecheck_function_params(function.params, function.num_params);
  if (is_success(result_params) == false) return result_params;

  TypecheckResult result_body = typecheck_function_body(function.retval_type->type, function.body, function.num_statements);
  if (is_success(result_body) == false) return result_body;

  db_pop_scope();

  if (g.db.scope != 0) {
    //DeclarationResult result = db_add_function_definition(program_statement);
    TypecheckResult result = db_add_function_declaration(function);
    if (is_success(result) == false) return result;
  }

  return TYPECHECK_SUCCESS;
}

TypecheckResult db_add_external_function_declaration(ExternFunctionDeclaration &extern_function) {
  FunctionDefinitionDBEntry new_entry = {};
  new_entry.scope = g.db.scope;
  new_entry.extern_function = &extern_function;

  TypecheckResult result = db_try_add_function(new_entry);
  if (is_success(result) == false) return result;

  return TYPECHECK_SUCCESS;
}

ProgramStatement *program_statment_alloc(int num_statements) {
  return (ProgramStatement *)calloc(sizeof(ProgramStatement) * num_statements, 1);
}

TypecheckResult typecheck_extern_function_declaration(ExternFunctionDeclaration &function) {
  TypecheckResult result_retval = typecheck_type_declaration(*function.retval_type);
  if (is_success(result_retval) == false) return result_retval;

  db_push_scope();

  TypecheckResult result_params = typecheck_function_params(function.params, function.num_params);
  if (is_success(result_params) == false) return result_params;

  //TypecheckResult result_body = typecheck_function_body(function.retval_type->type, function.body, function.num_statements);
  //if (is_success(result_body) == false) return result_body;

  db_pop_scope();

  if (g.db.scope != 0) {
    //DeclarationResult result = db_add_function_definition(program_statement);
    TypecheckResult result = db_add_external_function_declaration(function);
    if (is_success(result) == false) return result;
  }

  return TYPECHECK_SUCCESS;
}

TypecheckResult typecheck_program(Program &program) {
  for(int i = 0; i < program.num_statements; ++i) {
    ProgramStatement &cur_statement = program.statement[i];

    switch(cur_statement.type) {
    case PS_EXTERN_FUNCTION: {
      TypecheckResult result = typecheck_extern_function_declaration(cur_statement.extern_function);
      if (is_success(result) == false) return result;
    }  break;
    //case TOKEN_TYPEDEF_DEFINITION:
    case PS_LLVM_TYPE: {
      //TypecheckResult result = typecheck_variable_definition(program_statement);
      //if (is_success(result) == false) return result;
    }  break;
    case PS_VARIABLE: {
      TypecheckResult result = typecheck_variable_definition(cur_statement.varaible, false);
      if (is_success(result) == false) return result;
    }  break;
    case PS_STRUCT: {
      TypecheckResult result = typecheck_struct_definition(cur_statement.struct_def);
      if (is_success(result) == false) return result;
    } break;
    //case TOKEN_ENUM_DEFINITION:
    //  typecheck_enum_definition(program_statement);
    //  break;
    case PS_FUNCTION: {
      TypecheckResult result = typecheck_function_definition(cur_statement.function);
      if (is_success(result) == false) return result;
    } break;

    case PS_IMPORT:
      break;
    default:
      halt();
    }
  }

  return TYPECHECK_SUCCESS;
}

llvm::StringRef to_string_ref(const SubString &substring) {
  return llvm::StringRef(substring.start, substring.length);
}

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

LLVMTypeResult make_success(llvm::Type *type) {
  LLVMTypeResult retval;
  retval.result = RESULT_SUCCESS;
  retval.type = type;
  return retval;
}

LLVMTypeResult make_llvmtype_error(const FileLocation &location, const char *fmt, ...){
  LLVMTypeResult retval;
  retval.result = RESULT_ERROR;
  retval.error.location = location;
  FORMAT_ERROR_STRING(retval.error.error_string, fmt);
  return retval;
}

bool is_numeric(const SubString &digits) {
  for(int i = 0; i < digits.length; ++i) {
    if (isDigit(digits.start[i]) == false) return false;
  }

  return true;
}

LLVMTypeResult llvm_emit_llvm_type(Token *type_definition) {
  assert(type_definition->type == TOKEN_LLVM_TYPE_DEFINITION);
  // todo vector types...

  Token *raw_llvm = type_definition->end;
  assert(raw_llvm->type == TOKEN_RAW_LLVM_TYPE);
  //assert(type_definition->substring.length > 0);
  //assert(type_definition->substring.start != NULL);

  SubString trimmed_content = substring_trim(raw_llvm->substring);

  assert(trimmed_content.length > 0);
  assert(trimmed_content.start != NULL);

  switch(trimmed_content.start[0]) {
  case 'u':
  case 'i': {
    SubString digits = trimmed_content;
    digits.start++;
    digits.length--;

    if (is_numeric(digits) == false) {
      return make_llvmtype_error(type_definition->location, ("Invalid integer size '" + to_cstring(digits) + "'").c_str()); 
    }

    uint32_t size = (uint32_t)substring_to_uint64(digits);
    return make_success(llvm::IntegerType::get(*g.llvm.context, size));
  }
  default:
    if (substring_cmp(trimmed_content, "void")) {
      return make_success(llvm::Type::getVoidTy(*g.llvm.context));
    } else if (substring_cmp(trimmed_content, "half")) {
      return make_success(llvm::Type::getHalfTy(*g.llvm.context));
    } else if (substring_cmp(trimmed_content, "float")) {
      return make_success(llvm::Type::getFloatTy(*g.llvm.context));
    } else if (substring_cmp(trimmed_content, "double")) {
      return make_success(llvm::Type::getDoubleTy(*g.llvm.context));
    } else if (substring_cmp(trimmed_content, "fp128")) {
      return make_success(llvm::Type::getFP128Ty(*g.llvm.context));
    } else {
      return make_llvmtype_error(type_definition->location, "Unknown llvm type '%s'", to_cstring(trimmed_content).c_str());
    }
    break;
  }

  //return make_llvmtype_error(type_definition->location, "Uknown");
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


LLVMTypeResult llvm_emit_struct_type(Token *type_definition) {
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
  return make_success(type);
}

bool is_success(const LLVMTypeResult &result) {
  return (result.result != RESULT_ERROR);
}

LLVMTypeResult llvm_emit_base_type(TypeDef *type) {
  assert(type->llvm_type == NULL);
  //assert(type->kind == BASE_TYPE);

  LLVMTypeResult result;
  switch(type->kind) {
  case ARBITRARY_INTEGER:
  //case ARBITRARY_FLOAT:
    result = make_success((llvm::Type *)NULL);
    break;
  case LLVM_TYPE:
    result = llvm_emit_llvm_type(type->token);
    break;
  //case TOKEN_TYPEDEF_DEFINITION:
  //  return llvm_emit_typedef_type(type->base.type_definition);
  case STRUCT_TYPE:
    result = llvm_emit_struct_type(type->token);
    break;
  //case TOKEN_ENUM_DEFINITION:
  //  return llvm_emit_enum_type(type->base.type_definition);
    break;
  default:
    halt();
    result = make_llvmtype_error(type->token->location, "Unknown llvm type error.");
  }

  if (is_success(result)) {
    type->llvm_type = result.type;
  }
  return result;
}

llvm::Type *llvm_emit_pointer_type(TypeDef *pointer_type) {
  //assert(type_token->type == TOKEN_POINTER_TYPE);
  //Token *subtokens[1];
  //int num_subtokens = expand_tokens(type_token, subtokens, 1);
  assert(pointer_type->kind == POINTER_TYPE);
  //Token *base_type_declaration = ;
  //Type *base_type = emit_type_declaration(base_type_declaration);
  llvm::Type *base_llvm_type = llvm_emit_type(pointer_type->pointer.base_type);
  return llvm::PointerType::get(base_llvm_type, 0);
}

llvm::Type *llvm_emit_array_type(TypeDef *array_type) {
  //assert(type_token.token == TOKEN_COMPOUND_ARRAY_TYPE);
  //assert(type_token.num_subtokens == 2);
  //const GGToken &base_type_token = type_token.subtokens[0];
  //const GGToken &array_size = type_token.subtokens[1];
  assert(array_type->kind == ARRAY_TYPE);

  llvm::Type *base_type = llvm_emit_type(array_type->array.base_type);
  //uint64_t num_elements = eval_constexpr(array_size);
  return llvm::ArrayType::get(base_type, array_type->array.count);
}

int get_function_type_param_llvm_types(Token *param_list, llvm::Type **types, int max_params) {
  int num_params = 0;
  for(Token *param = param_list->start; param != NULL; param = param->next) {
    TypeDef *type_def = emit_type_declaration(param->start);
    types[num_params++] = llvm_emit_type(type_def);
  }
  return num_params;
}

void get_function_param_llvm_types(ParamDefinition *params, llvm::Type **types, int num_params) {
  //int num_params = 0;
  //for(Token *token = param_list->start; token != NULL; token = token->next) {
  //assert(max_params >= function->num_params);
  for(int i = 0; i < num_params; ++i) {
    assert(params[i].type == PD_VARIABLE);
    types[i] = llvm_get_type(params[i].variable.type->type);
    assert(types[i]);
  }

  //return function->num_params;
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

  llvm::Type *retval_type = llvm_emit_type(function_type->function.retval_type);

  llvm::Type *param_types[MAX_PARAMS];
  int num_params = get_function_type_param_llvm_types(function_type->function.param_list, param_types, MAX_PARAMS);
  llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, num_params);

  return llvm::FunctionType::get(retval_type, args, FIXED_ARGS);
}

//LLVMTypeResult llvm_emit_llvm_type(TypeDef *type) {
//  //assert(type->llvm_type == NULL);
//  if (type->llvm_type != NULL) {
//    return make_success(type->llvm_type);
//  }
//
//  return llvm_emit_base_type(type);
//}

llvm::Type *llvm_emit_type(TypeDef *type) {
  //assert(type->llvm_type == NULL);
  if (type->llvm_type != NULL) {
    return type->llvm_type;
  }

  llvm::Type *result;
  switch(type->kind) {
  case STRUCT_TYPE:
  case LLVM_TYPE: {
    LLVMTypeResult type_result = llvm_emit_base_type(type);
    result = type_result.type;
  } break;
    //halt();
    //assert(type->llvm_type != NULL);
  //  return type->llvm_type;
  case POINTER_TYPE:
    result = llvm_emit_pointer_type(type);
    break;
  case ARRAY_TYPE:
    result = llvm_emit_array_type(type);
    break;
  case FUNCTION_TYPE:
    result = llvm_emit_function_type(type);
    break;
  default:
    halt();
  }
  //Token *type_definition = type->type_definition;
  //llvm::Type *llvm_type = llvm_emit_type_defintiion(type_definition);
  //if (is_success(result)) {
  type->llvm_type = result;
  //}

  return result;
}

llvm::Value *CreateAlloca(const SubString &identifier, TypeDef *type) {
  llvm::StringRef name = to_string_ref(identifier);
  //GetType *type = emit_type_declaration(token_type);
  llvm::Type *llvm_type = llvm_get_type(type);
  llvm::Value *llvm_value = g.llvm.builder->CreateAlloca(llvm_type, 0, name);
  //Value *value = new Value;
  //value->type = type;
  //value->llvm_value = llvm_value;
  return llvm_value;
}

void CreateStore(llvm::Value *val, llvm::Value *address) //llvm.builder->CreateStore(retval, lvalue);
{
  g.llvm.builder->CreateStore(val, address);
}

static const bool SIGNED = true;

std::string to_llvm_type_str(llvm::Type *type)
{
  std::string data;
  llvm::raw_string_ostream stream(data);
  //type->getContainedType(0)->print(stream);
  type->print(stream);
  return stream.str();
}

llvm::Value *emit_one(/*TypeDef *type*/) {
  return llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 1, SIGNED);
}

llvm::Value *emit_zero() {
  return llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 0, SIGNED);
}

//llvm::Value *emit_rvalue_expression(Token *expression);
llvm::Value *emit_rvalue_expression(Expression &expression);

llvm::Value *emit_lvalue_function_call(FunctionCall &call) {
  halt();
  return NULL;
}

llvm::Value *emit_lvalue_expression(Expression &expr);

llvm::Value *CreateLoad(llvm::Value *rhs_value) {
  return g.llvm.builder->CreateLoad(rhs_value);
}

llvm::Value *emit_lvalue_array_dereference(FunctionCall &call) {
  //Token *subtokens[2];
  //expand_tokens(token, subtokens, 2);
  //Token *array_pointer = subtokens[0];
  //Token *index_expr = subtokens[1];
  //assert(token.num_subtokens == 2);
  //const GGToken &array_pointer = token.subtokens[0];
  //const GGToken &index_expr = token.subtokens[1];

  assert(call.num_params == 2);

  //TypeDef *base_type = temp_expression_token_to_typedef(array_pointer);
  TypeDef *base_type = expression_get_type(call.params[0]);

  llvm::Value *lhsVal = emit_lvalue_expression(call.params[0]);
  assert(lhsVal);

  llvm::Value *indexVal = emit_rvalue_expression(call.params[1]);
  assert(indexVal);

  //return llvm.builder->CreateGEP(lhsVal, indexVal);

  if (is_pointer(base_type)) {
    llvm::Value *zero = emit_zero();
    llvm::Value *idxs[] = { indexVal };
    int num_idxs = 1; //ARRAYSIZE(idxs);
    llvm::ArrayRef<llvm::Value *> array_ref = to_array_ref(idxs, num_idxs);
    llvm::Value *arrayElement = CreateLoad(lhsVal); //llvm.builder->CreateLoad(address);
    return g.llvm.builder->CreateGEP(arrayElement, array_ref);
  } else {
    llvm::Value *zero = emit_zero();
    llvm::Value *idxs[] = { zero, indexVal };
    int num_idxs = 2; //ARRAYSIZE(idxs);
    llvm::ArrayRef<llvm::Value *> array_ref = to_array_ref(idxs, num_idxs);
    llvm::Value *arrayElement = g.llvm.builder->CreateGEP(lhsVal, array_ref);
    return arrayElement;
  }

}

llvm::Value *emit_lvalue_intrinsic_function_call(FunctionCall &call) {
  //Token *subtokens[2];
  //expand_tokens(token, subtokens, 2);
  //Token *op_token = subtokens[0];
  //Token *lhs_token = subtokens[1];

  switch(call.intrinsic) {
  case OP_POINTER_DEREFERENCE: {
    return emit_rvalue_expression(call.params[0]);
  }
  case OP_ARRAY_DEREFERENCE: {
    return emit_lvalue_array_dereference(call);
  }
  default:
    halt();
  } 

  return NULL;

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

llvm::Value *field_index(TypeDef *type, const SubString &identifier) {
  int field_idx = field_idx_lookup(type, identifier);
  return llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), field_idx, SIGNED);
}

llvm::Value *emit_lvalue_member_identifier(FieldDereference &field_deref) {
  //Token *subtokens[2];
  //expand_tokens(token, subtokens, 2);
  //Token *basePointerExpr = subtokens[0];
  //Token *fieldIdentifier = subtokens[1];
  //assert(token.num_subtokens == 2);
  //const GGToken &basePointerExpr = token.subtokens[0];
  //const GGToken &fieldIdentifier = token.subtokens[1];

  llvm::Value *basePointer = emit_lvalue_expression(*field_deref.object);
  assert(basePointer);

  TypeDef *basePointerType = expression_get_type(*field_deref.object);
  //TypeDef *basePointerType = temp_expression_token_to_typedef(basePointerExpr);
  //assert(basePointerType->isPointerTy());
  //llvm::Type *baseValueType = basePointerType->getContainedType(0);

  llvm::Value *fieldIndex = field_index(basePointerType, field_deref.field_identifier);
  llvm::Value *zero = llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 0, SIGNED);

  llvm::Value *idxs[] = {zero, fieldIndex};
  const int num_idxs = 2;

  llvm::Value *fieldPointer = g.llvm.builder->CreateGEP(basePointer, to_array_ref(idxs, num_idxs));

  return fieldPointer;
}

llvm::Value *emit_lvalue_identifier(VariableReference &variable) {
  //assert(token->start == NULL);
  //assert(token.num_subtokens == 0);
  //llvm::Value *retval = old_db_lookup_variable(token->substring)->llvm_value;
  //assert(retval);
  //return retval;
  return variable.variable->llvm_value;
}

llvm::Value *emit_lvalue_expression(Expression &expression) {
  switch(expression.type) {
  //case TOKEN_UNARY_EXPRESSION:
  //  return emit_lvalue_unary_op(expression);
  case EXPR_FUNCTION_CALL:
    //return emit_lvalue_array_dereference(expression);
    if (expression.function_call.function) {
      return emit_lvalue_function_call(expression.function_call);
    } else {
      return emit_lvalue_intrinsic_function_call(expression.function_call);
    }
  case EXPR_FIELD_DEREFERENCE:
    //case TOKEN_OP_MEMBER:
    return emit_lvalue_member_identifier(expression.field_dereference);
  case EXPR_VARIABLE:
//  case TOKEN_IDENTIFIER:
    return emit_lvalue_identifier(expression.variable);
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

//llvm::Value *emit_rvalue_unary_op(Token *token) {
//  Token *subtokens[2];
//  expand_tokens(token, subtokens, 2);
//  Token *op_token = subtokens[0];
//  Token *lhs_token = subtokens[1];
//
//  //assert(token.num_subtokens == 2);
//  //const GGToken &op_token = token.subtokens[0];
//  //const GGToken &lhsToken = token.subtokens[1];
//  
//  switch(op_token->type) {
//  case TOKEN_PRE_INC_OP: {
//      llvm::Value *lvalue = emit_lvalue_expression(lhs_token);
//      llvm::Value *rvalue = emit_rvalue_expression(lhs_token);
//      llvm::Value *one    = emit_one(); // llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 1, SIGNED);
//      llvm::Value *retval = CreateAdd(rvalue, one); // llvm.builder->CreateAdd(rvalue, one);
//      CreateStore(retval, lvalue); //llvm.builder->CreateStore(retval, lvalue);
//      return retval;
//  }
//  case TOKEN_POSITIVE_OP: {
//      return emit_rvalue_expression(lhs_token);
//  }
//  case TOKEN_PRE_DEC_OP: {
//      llvm::Value *lvalue = emit_lvalue_expression(lhs_token);
//      llvm::Value *rvalue = emit_rvalue_expression(lhs_token);
//      llvm::Value *one   = emit_one(); //llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 1, SIGNED);
//      llvm::Value *retval = CreateSub(rvalue, one); //llvm.builder->CreateSub(rvalue, one);
//      CreateStore(retval, lvalue); //llvm.builder->CreateStore(retval, lvalue);
//      return retval;
//    } 
//  case TOKEN_NEGATIVE_OP: {
//      llvm::Value *value = emit_rvalue_expression(lhs_token);
//      llvm::Value *zero = emit_zero(); //llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 0, SIGNED);
//      return CreateSub(zero, value);
//      //return llvm.builder->CreateSub(zero, value);
//    }
//  case TOKEN_ADDRESS_OP: {
//      return emit_lvalue_expression(lhs_token);
//    }
//  case TOKEN_DEREF_OP: {
//      llvm::Value *address = emit_rvalue_expression(lhs_token);
//      llvm::Value *value = CreateLoad(address); //llvm.builder->CreateLoad(address);
//      return value;
//    }
//  default:
//    halt();
//  }
//
//  return NULL;
//}

void emit_function_declaration(FunctionDefinition &function);


//llvm::Value *emit_rvalue_binary_op(Token *token) {
//  Token *subtokens[3];
//  expand_tokens(token, subtokens, 3);
//  Token *lhs_token = subtokens[0];
//  Token *op_token = subtokens[1];
//  Token *rhs_token = subtokens[2];
//  //assert(token.num_subtokens == 3);
//  //const GGToken &lhsToken = token.subtokens[0];
//  //const GGToken &op_token = token.subtokens[1];
//  //const GGToken &rhsToken = token.subtokens[2];
//
//  llvm::Value *lhs = emit_rvalue_expression(lhs_token);
//  llvm::Value *rhs = emit_rvalue_expression(rhs_token);
//
//  TypeDef *lhs_type = temp_expression_token_to_typedef(lhs_token);
//  TypeDef *rhs_type = temp_expression_token_to_typedef(rhs_token);
//
//  //GGSubString function_identifier = op_token.substring;
//  //switch(*op_token.substring.start) {
//  //case '+': {
//  //  GGToken replacementToken;
//  //  token.substring.start = "op_++";
//  //  token.substring.length
//  //  return llvm.builder->CreateAdd(lhs, rhs, "addtmp");
//  //          }
//  //case '-': {
//  //  return llvm.builder->CreateSub(lhs, rhs, "subtmp");
//  //          }
//  //case '*': {
//  //  return llvm.builder->CreateMul(lhs, rhs, "multmp");
//  //          }
//  //case '/': {
//  //  return llvm.builder->CreateSDiv(lhs, rhs, "divtmp");
//  //          }
//  //case '%': {
//  //  return llvm.builder->CreateSRem(lhs, rhs, "remtmp");
//  //          }
//  //          //case OP_BINARY_AND:
//  //          //case OP_BINARY_OR:
//  //          //case OP_BINARY_XOR:
//  //          //case OP_LOGICAL_AND:
//  //          //case OP_LOGICAL_OR:
//  //          //case OP_COMPARE_EQUAL:
//  //          //case OP_COMPARE_NOT_EQUAL:
//  //          //case OP_COMPARE_LESS_THAN:
//  //          //case OP_COMPARE_LESS_THAN_EQUAL:
//  //          //case OP_COMPARE_GREATER_THAN:
//  //          //case OP_COMPARE_GREATER_THAN_EQUAL:
//  //default:
//  //  halt();
//  //}
//
//  //Value *param_expressions[2];
//  //param_expressions[0] = lhs;
//  //param_expressions[1] = rhs;
//
//  //llvm::StringRef name = to_string_ref(function_identifier);
//
//  if (op_token->type == TOKEN_ADD_OP) {
//    //llvm::StringRef stringRef(string_literal->substring.start, string_literal->substring.length);
//    //llvm::Value *array_value = g.llvm.builder->CreateGlobalString(stringRef);
//
//    llvm::Value *index_value;
//    llvm::Value *array_value;
//    if (is_pointer(lhs_type) && is_integer(rhs_type)) {
//      array_value = lhs;
//      index_value = rhs;
//    } else if (is_pointer(rhs_type) && is_integer(lhs_type)) {
//      array_value = rhs;
//      index_value = lhs;
//    } else {
//      goto LOOKUP;
//    }
//    llvm::Value *zero = llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 0, SIGNED);
//    llvm::Value *zeros[] = {index_value};
//    llvm::Value *result = g.llvm.builder->CreateGEP(array_value, to_array_ref(zeros, 1));
//    return result;
//  } 
//
//LOOKUP:
//
//  FunctionDef *callee = db_binary_operator_lookup(op_token->type, lhs_type, rhs_type);
//  llvm::Value *llvm_param_expressions[2];
//  llvm_param_expressions[0] = lhs;
//  llvm_param_expressions[1] = rhs;
//  llvm::ArrayRef<llvm::Value *> ref_params = to_array_ref(llvm_param_expressions, 2);
//  if (callee->llvm_function == NULL) {
//    emit_function_declaration(callee);
//  }
//  llvm::Value *result = g.llvm.builder->CreateCall(callee->llvm_function, ref_params);
//  return result;
//}
//
llvm::Value *emit_rvalue_array_dereference(FunctionCall &call) {
  llvm::Value *lvalue = emit_lvalue_array_dereference(call);
  llvm::Value *rvalue = CreateLoad(lvalue);
  return rvalue;
}
//
//llvm::Value *emit_rvalue_unary_post_op(Token *token, TokenType opType) {
//  Token *subtokens[2];
//  expand_tokens(token, subtokens, 2);
//  Token *value_expr = subtokens[0];
//  Token *op_expr = subtokens[1];
//  //assert(token.num_subtokens == 2);
//  //const GGToken &value_expr = token.subtokens[0];
//  //const GGToken &op_expr= token.subtokens[1];
//
//  llvm::Value *lvalue = emit_lvalue_expression(value_expr);
//  llvm::Value *const1 = emit_one();
//  llvm::Value *value = CreateLoad(lvalue);
//
//  llvm::Value *newValue;
//  switch(opType) {
//  case TOKEN_OP_POSTFIX_INC:
//    newValue = CreateAdd(value, const1);
//    break;
//  case TOKEN_OP_POSTFIX_DEC:
//    newValue = CreateSub(value, const1);
//    break;
//  default:
//    halt();
//  }
//
//  CreateStore(newValue, lvalue);
//  return value;
//}

llvm::Value *emit_rvalue_member_identifier(FieldDereference &field_dereference) {
  llvm::Value *lvalue = emit_lvalue_member_identifier(field_dereference);
  llvm::Value *rvalue = CreateLoad(lvalue);
  return rvalue;
}

void emit_function_call_params(Expression *params, llvm::Value **values, int num_params)
{
  //assert(params.num_subtokens <= maxParams);
  //for(int i = 0; i< params.num_subtokens; ++i) {
  //for(Token *param = params->start; param != NULL; param = param->next) {
  for(int i = 0; i < num_params; ++i) {
    llvm::Value *value = emit_rvalue_expression(params[i]);
    assert(value);
    values[i] = value;
  }
}

llvm::Value *emit_rvalue_pointer_add(Expression &pointer, Expression &integer) {
  //llvm::Value *zero = llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 0, SIGNED);

  llvm::Value *pvalue = emit_rvalue_expression(pointer);
  llvm::Value *ivalue = emit_rvalue_expression(integer);

  llvm::Value *offset[] = {ivalue};
  llvm::Value *result = g.llvm.builder->CreateGEP(pvalue, to_array_ref(offset, 1));

  return result;
}

llvm::Value *emit_rvalue_intrinsic_function_call(FunctionCall &call) {
  switch(call.intrinsic) {
  case OP_EXTEND_INTEGER: {
    llvm::Value *rhs = emit_rvalue_expression(call.params[0]);
    llvm::Type  *dest_type = llvm_get_type(call.dest_type);
      return g.llvm.builder->CreateSExt(rhs, dest_type);
  }
  case OP_EXTEND_FLOAT: {
    llvm::Value *rhs = emit_rvalue_expression(call.params[0]);
    llvm::Type  *dest_type = llvm_get_type(call.dest_type);
    return g.llvm.builder->CreateFPExt(rhs, dest_type);
  }
  case OP_CAST_POINTER: {
    llvm::Value *rhs = emit_rvalue_expression(call.params[0]);
    llvm::Type  *dest_type = llvm_get_type(call.dest_type);
    return g.llvm.builder->CreateBitCast(rhs, dest_type);
  }
  case OP_CAST_ARRAY_TO_POINTER: {
    llvm::Value *rhs = emit_rvalue_expression(call.params[0]);
    llvm::Value *zero = llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 0, SIGNED);
    llvm::Value *zeros[] = {zero, zero};
    llvm::Value *pointer = g.llvm.builder->CreateGEP(rhs, to_array_ref(zeros, 2));
    return pointer;      
  }
  case OP_POINTER_ADD:
      return emit_rvalue_pointer_add(call.params[0], call.params[1]);
    //case OP_POINTER_SUBTRACT,
    //  return emit_rvalue_pointer_add(call.params[0], call.params[1]);
  case OP_POINTER_DEREFERENCE: {
      llvm::Value *address = emit_rvalue_expression(call.params[0]);
      llvm::Value *value = CreateLoad(address); //llvm.builder->CreateLoad(address);
      return value;
    }
  case OP_ARRAY_DEREFERENCE:
      //return emit_rvalue_member_identifier()
    return emit_rvalue_array_dereference(call);
  case OP_ADDRESS_OF:
    return emit_lvalue_expression(call.params[0]);
  //case OP_MEMBER_DEREFERENCE:
  //    return emit_rvalue_member_identifier(call.);
    default:
      halt();
  }

  return NULL;
}

llvm::Value *emit_rvalue_extern_function_call(FunctionCall &call) {
  //Token *subtokens[2];
  //expand_tokens(function_call, subtokens, 2);
  //Token *function_identifier = subtokens[0];
  //Token *function_params = subtokens[1];

  ////llvm::Function *callee = llvm.module->getFunction(name);
  //FunctionDef *callee = db_function_call_lookup(function_identifier->substring, function_params);
  llvm::StringRef name = to_string_ref(call.identifier);
  llvm::Function *llvm_function = call.function->llvm_function;

  llvm::Value *retval = NULL;
  if (call.num_params == 0) {
    retval = g.llvm.builder->CreateCall(call.function->llvm_function);
  } else {
    llvm::Value *param_expressions[MAX_PARAMS];
    assert(call.num_params < MAX_PARAMS);
    emit_function_call_params(call.params, param_expressions, call.num_params);

    llvm::ArrayRef<llvm::Value *> ref_params = to_array_ref(param_expressions, call.num_params);
    retval = g.llvm.builder->CreateCall(llvm_function, ref_params);
    //assert(retval);
  }

  return retval;
}

llvm::Value *emit_rvalue_function_call(FunctionCall &call) {
  //Token *subtokens[2];
  //expand_tokens(function_call, subtokens, 2);
  //Token *function_identifier = subtokens[0];
  //Token *function_params = subtokens[1];

  ////llvm::Function *callee = llvm.module->getFunction(name);
  //FunctionDef *callee = db_function_call_lookup(function_identifier->substring, function_params);
  llvm::StringRef name = to_string_ref(call.identifier);

  llvm::Function *llvm_function;
  if (call.function) {
    llvm_function = call.function->llvm_function;
    if (llvm_function == NULL) {
      emit_function_declaration(*call.function);
      llvm_function = call.function->llvm_function;
    }
  } else if (call.extern_function) {
    llvm_function = call.extern_function->llvm_function;
  } else {
    halt();
  }


  assert(llvm_function);


  llvm::Value *retval = NULL;
  if (call.num_params == 0) {
    retval = g.llvm.builder->CreateCall(llvm_function);
  } else {
    llvm::Value *param_expressions[MAX_PARAMS];
    assert(call.num_params < MAX_PARAMS);
    emit_function_call_params(call.params, param_expressions, call.num_params);

    llvm::ArrayRef<llvm::Value *> ref_params = to_array_ref(param_expressions, call.num_params);
    retval = g.llvm.builder->CreateCall(llvm_function, ref_params);
    //assert(retval);
  }

  return retval;
}

int sizeof_type(TypeDef *type) {
  assert(type->llvm_type);
  return type->llvm_type->getPrimitiveSizeInBits();
}

llvm::Value *emit_rvalue_integer_literal(NumericLiteral &literal) {
  //assert(integer_literal.token == TOKEN_LITERAL_INTEGER);
  //int num_bits = 32; //get_integer_type_num_bits(integer_literal);

  TypeDef *type = integer_type_lookup("i32"); //calc_integer_literal_type(integer_literal);
  int num_bits = sizeof_type(type);
  llvm::IntegerType *llvm_type = llvm::IntegerType::get(*g.llvm.context, num_bits);
  assert(type->llvm_type == (llvm::Type *)llvm_type);

  llvm::StringRef str = to_string_ref(literal.literal);

  const int RADIX = 10;
  llvm::Value *retval = llvm::ConstantInt::get(llvm_type, str, RADIX);
  return retval;
}

llvm::Value *emit_rvalue_float_literal(Token *float_literal) {
  //assert(float_literal.token == TOKEN_LITERAL_FLOAT);
  llvm::Type *type = llvm::Type::getFloatTy(*g.llvm.context);
  llvm::StringRef str = to_string_ref(float_literal->substring);
  llvm::Value *retval = llvm::ConstantFP::get(type, str);
  return retval;
}

llvm::Value *emit_rvalue_string_literal(StringLiteral &literal) {
  llvm::StringRef stringRef = to_string_ref(literal.literal);
  llvm::Value *array_value = g.llvm.builder->CreateGlobalString(stringRef);
  return array_value;
}

llvm::Value *emit_rvalue_identifier(VariableReference &varaible) {
  llvm::Value *lvalue = emit_lvalue_identifier(varaible);
  llvm::Value *rvalue = CreateLoad(lvalue);
  return rvalue;
}

llvm::Value *emit_rvalue_expression(Expression &expr) {
  switch(expr.type) {
  case EXPR_FUNCTION_CALL:
    if (expr.function_call.function || expr.function_call.extern_function) {
      return emit_rvalue_function_call(expr.function_call);
    } else {
      return emit_rvalue_intrinsic_function_call(expr.function_call);
    }
  case EXPR_NUMERIC_LITERAL:
    return emit_rvalue_integer_literal(expr.numeric_literal);
  case EXPR_STRING_LITERAL:
    return emit_rvalue_string_literal(expr.string_literal);
  case EXPR_VARIABLE:
    return emit_rvalue_identifier(expr.variable);
  case EXPR_FIELD_DEREFERENCE:
    return emit_rvalue_member_identifier(expr.field_dereference);
  default:
    halt();
  }

  return NULL;
}

llvm::Value *emit_autocasts(llvm::Value *lhs, llvm::Value *rhs, TypeDef *lhs_type, TypeDef *rhs_type) {
  if (lhs_type == rhs_type) {
    return rhs;
  }

  if (lhs_type->kind == POINTER_TYPE && rhs_type->kind == ARRAY_TYPE && lhs_type->array.base_type == rhs_type->pointer.base_type) {
    llvm::Value *zero = llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 0, SIGNED);
    llvm::Value *zeros[] = {zero, zero};
    llvm::Value *pointer = g.llvm.builder->CreateGEP(rhs, to_array_ref(zeros, 2));
    return pointer;
  }

  halt();
  return NULL;
}

EmitResult emit_variable_initalization(VariableDefinition &variable) {
  llvm::Value *value_variable = CreateAlloca(variable.identifier, variable.type->type);
  llvm::Value *value_initaizlier;
  switch(variable.initial_value->type) {
  case EXPR_DEFAULT_VALUE: {
      value_initaizlier = llvm::Constant::getNullValue(variable.type->type->llvm_type);
    } break;
  default: {
      value_initaizlier = emit_rvalue_expression(*variable.initial_value);
    } break;
  }
  CreateStore(value_initaizlier, value_variable);
  variable.llvm_value = value_variable;
  return EMIT_SUCCESS; //make_success(value_variable);
}

EmitResult emit_local_variable_definition(VariableDefinition &variable) {
  //DeclarationResult result = db_add_variable_definition(variable);
  db_add_variable_declaration(variable);
  return emit_variable_initalization(variable);
}

uint64_t eval_constexpr(Token *initalizer_value) {
  switch(initalizer_value->type) {
  case TOKEN_INTEGER_LITERAL:
    return substring_to_uint64(initalizer_value->substring);
  default:
    halt();
  }

  return 0;
}

EmitResult emit_global_constant_initialization(VariableDefinition &variable) {
  TypeDef *type = variable.type->type;
  llvm::Type *llvm_type = llvm_emit_type(type);

  llvm::Constant *constant;

  switch(variable.initial_value->type) {
  case EXPR_DEFAULT_VALUE: {
      constant = llvm::Constant::getNullValue(llvm_type);
    } break;
  default: {
      int64_t val64 = eval_constexpr(*variable.initial_value);
      constant = llvm::ConstantInt::get(llvm_type, (uint64_t)val64);
    } break;
  }

  llvm::StringRef name = to_string_ref(variable.identifier);
  llvm::Value *value = new llvm::GlobalVariable(*g.llvm.module, type->llvm_type, false, llvm::GlobalVariable::ExternalLinkage, constant, name);
  variable.llvm_value = value;

  return EMIT_SUCCESS;
}

void emit_assignment_statement(AssignmentStatement &statement) {
  llvm::Value *lhs = emit_lvalue_expression(*statement.lhs);
  llvm::Value *rhs = emit_rvalue_expression(*statement.rhs);

  switch(statement.op) {
  case TOKEN_ASSIGNMENT_OP: {
    CreateStore(rhs, lhs);
  } break;
  case TOKEN_ADD_ASSIGNMENT_OP: {
    llvm::Value *r_lhs = emit_rvalue_expression(*statement.lhs);
    llvm::Value *newVal = CreateAdd(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  case TOKEN_SUB_ASSIGNMENT_OP: {
    llvm::Value *r_lhs = emit_rvalue_expression(*statement.lhs);
    llvm::Value *newVal = CreateSub(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  case TOKEN_MUL_ASSIGNMENT_OP: {
    llvm::Value *r_lhs = emit_rvalue_expression(*statement.lhs);
    llvm::Value *newVal = CreateMul(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  case TOKEN_DIV_ASSIGNMENT_OP: {
    llvm::Value *r_lhs = emit_rvalue_expression(*statement.lhs);
    llvm::Value *newVal = CreateSDiv(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  case TOKEN_REM_ASSIGNMENT_OP: {
    llvm::Value *r_lhs = emit_rvalue_expression(*statement.lhs);
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

void emit_return_statement(ReturnStatement &return_statement) {
  //Token *subtokens[1];
  //int num_subtokens = expand_tokens(return_statement, subtokens, 1);
  //Token *return_expression = subtokens[0];
  //assert(return_statement.token == TOKEN_COMPOUND_RETURN_STATEMENT);
  //assert(return_statement.num_subtokens == 1);
  //GGToken &return_expression = return_statement.subtokens[0];
  //return_statement.retval

  llvm::Value *retval = emit_rvalue_expression(*return_statement.retval);
  g.llvm.builder->CreateRet(retval);
}

//void LinesRemove(GGSubString *lines, int num_lines, int line_to_remove) {
//  std::vector<std::string> lines2;
//  int i = 0;
//  lines2.erase(&lines2[i]);
//  lines2.insert(&lines2[i], 
//
//}

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
      VariableDefinition *variable = db_lookup_variable(token_substr);
      TypeDef *ggtype = variable->type->type;
      llvm::Type *type = ggtype->llvm_type;
      std::string type_str = to_llvm_type_str(type);
      std::string new_line = string_format("%s%%%s%d%s", old_lhs.c_str(), token_str.c_str(), temp_n, old_rhs.c_str());
      std::string new_store = string_format("store %s %%%s%d, %s* %%%s", type_str.c_str(), token_str.c_str(), temp_n, type_str.c_str(), token_str.c_str());

      lines.erase(lines.begin() + i); //lines.remove(i); //line);
      lines.insert(lines.begin() + i, new_line);
      lines.insert(lines.begin() + i+1, new_store);
      //next = i;
                                      } break;
    case LLVM_REPLACEMENT_FIRST_EXPRESSION: 
    case LLVM_REPLACEMENT_EXPRESSION: {
      VariableDefinition *variable = db_lookup_variable(token_substr);
      TypeDef *ggtype = variable->type->type;
      llvm::Type *type = ggtype->llvm_type;
      std::string type_str = to_llvm_type_str(type);
      std::string new_load  = string_format("%%%s%d = load %s* %%%s", token_str.c_str(), temp_n, type_str.c_str(), token_str.c_str());
      std::string new_line        = string_format("%s%s %%%s%d%s", old_lhs.c_str(), type_str.c_str(), token_str.c_str(), temp_n, old_rhs.c_str());
      std::string new_line_no_type= string_format("%s%%%s%d%s", old_lhs.c_str(), token_str.c_str(), temp_n, old_rhs.c_str());
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

void replace_inline_llvm_bindings(SubString &raw_llvm, char replaced_llvm_buffer[1024]) 
{
  //GGSubString lines[MAX_LLVM_LINES];
  Lines lines;
  substring_to_lines(raw_llvm, lines);
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

DeclarationResult make_declaration_error(const char *error_fmt, ...) {
  DeclarationResult retval;
  retval.result = RESULT_ERROR;
  //retval.error.location = location;
  FORMAT_ERROR_STRING(retval.error.error_string, error_fmt);
  return retval;
}

DeclarationResult db_try_add_type(TypeDef &new_type) {
  TypeDef *existing_type = db_lookup_type(new_type.identifier);
  if (existing_type) {
    return make_declaration_error("Type redefinition");
  }

  g.db.types.push_back(new_type);
  return DECLARATION_SUCCESS;
}

void emit_inline_llvm(llvm::Function *function, LLVMStatement &llvm_statement) {
  //Token *subtokens[1];
  //int num_subtokens = expand_tokens(inline_llvm, subtokens, 1);
  //Token *raw_llvm = subtokens[0];

  g.llvm.builder->GetInsertPoint();

  char replaced_llvm_buffer[1024];
  replace_inline_llvm_bindings(llvm_statement.raw_llvm, replaced_llvm_buffer);

  llvm::StringRef llvmAssembly(replaced_llvm_buffer);
  llvm::MemoryBuffer *memory = llvm::MemoryBuffer::getMemBuffer(llvmAssembly, "<string>", true);
  llvm::SMDiagnostic error;
  g.llvm.module->dump();
  bool retval = ParseAssembly2(memory, *g.llvm.module, error, function, g.llvm.builder->GetInsertBlock());
  assert(retval);
}

void emit_function_declaration(FunctionDefinition &function) {
  //if (function_body->start == NULL) {}
  //  function = db_function_lookup(function_identifier, NULL);
  //} else {
  //  llvm::Type *param_types[MAX_PARAMS];
  //  int num_params = get_param_types(llvm, function_params, param_types, MAX_PARAMS);
  //  function = db_lookup_function_declaration(llvm, function_identifier, param_types, num_params);
  //}

  llvm::Type *retval_type = llvm_get_type(function.retval_type->type);

  llvm::FunctionType *functionType;
  if (function.num_params == 0) 
  {
    functionType = llvm::FunctionType::get(retval_type, FIXED_ARGS);
  }
  else
  {
    llvm::Type *param_types[MAX_PARAMS];
    assert(function.num_params <= MAX_PARAMS);

    if (function.params[function.num_params-1].type == PD_VARARGS) {
      get_function_param_llvm_types(function.params, param_types, function.num_params-1);
      llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function.num_params-1);
      functionType = llvm::FunctionType::get(retval_type, args, VAR_ARGS);
    } else {
      get_function_param_llvm_types(function.params, param_types, function.num_params);
      llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function.num_params);
      functionType = llvm::FunctionType::get(retval_type, args, FIXED_ARGS);
    }


    //emit_paramater_bindings(llvm, function_params);
  }

  llvm::StringRef name = to_string_ref(function.identifier);
  function.llvm_function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, g.llvm.module);
  assert(function.llvm_function);
}

EmitResult emit_function_definition(FunctionDefinition &function) {
  db_push_scope();

  if (function.llvm_function == NULL) {
    emit_function_declaration(function);
  }

  llvm::BasicBlock *entry = llvm::BasicBlock::Create(*g.llvm.context, "", function.llvm_function);
  g.llvm.builder->SetInsertPoint(entry);

  int i = 0;
  llvm::Function::arg_iterator arg_iterator = function.llvm_function->arg_begin();
  for(int i = 0; i < function.num_params; ++i, arg_iterator++) {
    assert(function.params[i].type == PD_VARIABLE);
    VariableDefinition &param = function.params[i].variable;

    TypeDef *type = param.type->type;

    if (variable_exists(function.params[i].variable.identifier)) {
      return make_emit_error("Duplicate variable %s", to_cstring(param.identifier).c_str());
    }

    llvm::Argument *AI = arg_iterator;
    llvm::Value *alloca = CreateAlloca(param.identifier, type);
    CreateStore(AI, alloca);
    param.llvm_value = alloca;

    db_add_variable_declaration(param);
  }

  for(int i = 0; i < function.num_statements; ++i) {
    FunctionStatement &statement = function.body[i];
    switch(statement.type) {
    case FS_VARIABLE:
      emit_local_variable_definition(statement.varaible);
      break;
    case FS_ASSIGNMENT:
      emit_assignment_statement(statement.assignment);
      break;
    case FS_RETURN:
      emit_return_statement(statement.return_statement);
      break;
    case FS_EXPRESSION:
      //assert(subtoken.num_subtokens == 1);
      emit_rvalue_expression(*statement.expression);
      break;
    case FS_LLVM:
      emit_inline_llvm(function.llvm_function, statement.llvm);
      break;
    default: 
      halt();
    }
  }
  db_pop_scope();

  if (is_void(function.retval_type->type)) {
    g.llvm.builder->CreateRetVoid();
  }

  return EMIT_SUCCESS;
}

EmitResult emit_external_function_declaration(ExternFunctionDeclaration &function) {
  db_push_scope();

  llvm::Type *retval_type = llvm_get_type(function.retval_type->type);

  llvm::FunctionType *functionType;
  if (function.num_params == 0) 
  {
    functionType = llvm::FunctionType::get(retval_type, FIXED_ARGS);
  }
  else
  {
    llvm::Type *param_types[MAX_PARAMS];
    assert(function.num_params <= MAX_PARAMS);

    if (function.params[function.num_params-1].type == PD_VARARGS) {
      get_function_param_llvm_types(function.params, param_types, function.num_params-1);
      llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function.num_params-1);
      functionType = llvm::FunctionType::get(retval_type, args, VAR_ARGS);
    } else {
      get_function_param_llvm_types(function.params, param_types, function.num_params);
      llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function.num_params);
      functionType = llvm::FunctionType::get(retval_type, args, FIXED_ARGS);
    }

    //get_function_param_llvm_types(function.params, param_types, function.num_params);
    //llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function.num_params);
    //functionType = llvm::FunctionType::get(retval_type, args, FIXED_ARGS);
  }

  llvm::StringRef name = to_string_ref(function.identifier);
  function.llvm_function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, g.llvm.module);

  return EMIT_SUCCESS;
}

EmitResult emit_program_statement(ProgramStatement &statement) {
    switch(statement.type) {
    case PS_STRUCT:
    case PS_LLVM_TYPE:
      // case PS_TYPEDEF:
      break;
    case PS_VARIABLE:
      return emit_global_constant_initialization(statement.varaible);
    case PS_FUNCTION:
      return emit_function_definition(statement.function);
    case PS_EXTERN_FUNCTION:
      return emit_external_function_declaration(statement.extern_function);
    case PS_IMPORT:
      break;
    default:
      halt();
    }

  return EMIT_SUCCESS;
}

bool is_success(const EmitResult &result) {
  return true;
}

EmitResult emit_program(Program &program, const char *dest_file) {
  for(int i = 0; i < program.num_statements; ++i) {
    EmitResult result = emit_program_statement(program.statement[i]);
    if(is_success(result) == false) return result;
  }

  g.llvm.module->dump();

  return EMIT_SUCCESS;
}

CompileResult make_result(const LLVMTypeResult &result) {
  CompileResult retval;
  retval.result = result.result;

  const std::string &file = g.parser.files[result.error.location.file_index];
  int line = result.error.location.line;
  int col = result.error.location.column;
  retval.error.error_string = string_format("%s(%d:%d): %s\n", file.c_str(), line, col, result.error.error_string.c_str());

  return retval;
}

CompileResult make_result(const ParseResult &parse_result) {
  CompileResult retval;
  retval.result = parse_result.result;

  const std::string &file = g.parser.files[parse_result.error.location.file_index];
  int line = parse_result.error.location.line;
  int col = parse_result.error.location.column;
  retval.error.error_string = string_format("%s(%d:%d): %s\n", file.c_str(), line, col, parse_result.error.error_string.c_str());

  return retval;
}

TypeDef *temp_expression_token_to_typedef(Token *expression_token) {
  Expression expr = {};
  DigestResult result_digest = digest_expression(expression_token, expr);
  assert(is_success(result_digest));
  TypecheckResult result_type = typecheck_expression(expr);
  assert(is_success(result_type));

  return expression_get_type(expr);
}


CompileResult make_result(const DeclarationResult &result) {
  CompileResult retval;
  retval.result = result.result;
  //retval.error = typecheck_result.error;
  const std::string &file = g.parser.files[result.error.location.file_index];
  int line = result.error.location.line;
  int col = result.error.location.column;
  retval.error.error_string = string_format("%s(%d:%d): %s\n", file.c_str(), line, col, result.error.error_string.c_str());
  return retval;
}

CompileResult make_result(const DigestResult &result) {
  CompileResult retval;
  retval.result = result.result;
  //retval.error = typecheck_result.error;
  const std::string &file = g.parser.files[result.error.location.file_index];
  int line = result.error.location.line;
  int col = result.error.location.column;
  retval.error.error_string = string_format("%s(%d:%d): %s\n", file.c_str(), line, col, result.error.error_string.c_str());
  return retval;
}

CompileResult make_result(const TypecheckResult &typecheck_result) {
  CompileResult retval;
  retval.result = typecheck_result.result;
  //retval.error = typecheck_result.error;
  const std::string &file = g.parser.files[typecheck_result.error.location.file_index];
  int line = typecheck_result.error.location.line;
  int col = typecheck_result.error.location.column;
  retval.error.error_string = string_format("%s(%d:%d): %s\n", file.c_str(), line, col, typecheck_result.error.error_string.c_str());
  return retval;
}

CompileResult make_result(const EmitResult &emit_result) {
  CompileResult retval;
  retval.result = emit_result.result;
  //retval.error = emit_result.error;
  const std::string &file = g.parser.files[emit_result.error.location.file_index];
  int line = emit_result.error.location.line;
  int col = emit_result.error.location.column;
  retval.error.error_string = string_format("%s(%d:%d): %s\n", file.c_str(), line, col, emit_result.error.error_string.c_str());
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

void add_base_type(TypeKind kind) {
  TypeDef type = {};
  type.kind = kind;
  g.db.types.push_back(type);
}

const CompileResult COMPILE_SUCCESS = {};

void IRCompile(LLVMState &llvm);

DigestResult digest_default_value_expression(Expression &expr) {
  expr.type = EXPR_DEFAULT_VALUE;
  return DIGEST_SUCCESS;
}

DigestResult digest_variable_definition(Token *variable_declaration, VariableDefinition &variable) {
  int num_subtokens = count_subtokens(variable_declaration);

  if (num_subtokens == 1) {
    Token *tok_type;
    expand_tokens(variable_declaration, tok_type);

    variable.type = type_decl_alloc(1);
    DigestResult result_type = digest_type_declaration(tok_type, *variable.type);
    if (is_success(result_type) == false) return result_type;
  } else if (num_subtokens == 2) {
    Token *tok_type, *tok_identifier;
    expand_tokens(variable_declaration, tok_type, tok_identifier);

    variable.type = type_decl_alloc(1);
    DigestResult result_type = digest_type_declaration(tok_type, *variable.type);
    if (is_success(result_type) == false) return result_type;

    variable.identifier = tok_identifier->substring;

    variable.initial_value = expressions_alloc(1);
    DigestResult result = digest_default_value_expression(*variable.initial_value);
    if (is_success(result) == false) return result;
  } else if (num_subtokens == 3) {
    Token *tok_type, *tok_identifier, *tok_initial_value;
    expand_tokens(variable_declaration, tok_type, tok_identifier, tok_initial_value);

    variable.type = type_decl_alloc(1);
    DigestResult result_type = digest_type_declaration(tok_type, *variable.type);
    if (is_success(result_type) == false) return result_type;

    variable.identifier = tok_identifier->substring;

    variable.initial_value = expressions_alloc(1);
    DigestResult result = digest_expression(tok_initial_value, *variable.initial_value);
    if (is_success(result) == false) return result;
  } else {
    halt();
  }

  return DIGEST_SUCCESS;
}

DigestResult digest_field_definition(Token *field_definition, VariableDefinition &field_def) {
  return digest_variable_definition(field_definition, field_def);
}

DigestResult digest_struct_definition(Token *struct_declaration, ProgramStatement &statement) {
  Token *type_identifier, *tok_fields;
  expand_tokens(struct_declaration, type_identifier, tok_fields);

  statement.type = PS_STRUCT;
  statement.struct_def.identifier = type_identifier->substring;

  statement.struct_def.num_fields = count_subtokens(tok_fields);
  statement.struct_def.fields = fields_alloc(statement.struct_def.num_fields);

  int i = 0;
  for(Token *field_definition = tok_fields; field_definition != NULL; field_definition = field_definition->next) {
    VariableDefinition &field_def = statement.struct_def.fields[i];
    DigestResult result = digest_field_definition(field_definition, field_def);
    if (is_success(result) == false) return result;
  }

  return DIGEST_SUCCESS;
}

DigestResult digest_llvm_type_definition(Token *llvm_type_declaration, ProgramStatement &statement) {
    statement.type = PS_LLVM_TYPE;
    Token *type_identifier, *raw_llvm;
    expand_tokens(llvm_type_declaration, type_identifier, raw_llvm);
    statement.llvm_type.identifier = type_identifier->substring;
    statement.llvm_type.raw_llvm = raw_llvm->substring;

    return DIGEST_SUCCESS;
}

DigestResult digest_binary_expression(Token *expression, Expression &expr) {
  Token *lhs, *op, *rhs;
  expand_tokens(expression, lhs, op, rhs);

  expr.type = EXPR_FUNCTION_CALL;
  expr.function_call.op = op->type;
  expr.function_call.identifier = op_to_substring(expr.function_call.op);
  expr.function_call.params = call_params_alloc(2);
  expr.function_call.num_params = 2;

  DigestResult lhs_result = digest_expression(lhs, expr.function_call.params[0]);
  if (is_success(lhs_result) == false) return lhs_result;

  DigestResult rhs_result = digest_expression(rhs, expr.function_call.params[1]);
  if (is_success(rhs_result) == false) return rhs_result;

  return DIGEST_SUCCESS;
}

DigestResult digest_unary_expression(Token *expression, Expression &expr) {
  Token *lhs, *op;
  expand_tokens(expression, op, lhs);

  expr.type = EXPR_FUNCTION_CALL;
  expr.function_call.op = op->type;
  expr.function_call.identifier = op_to_substring(expr.function_call.op);
  expr.function_call.params = call_params_alloc(1);
  expr.function_call.num_params = 1;

  DigestResult lhs_result = digest_expression(lhs, expr.function_call.params[0]);
  if (is_success(lhs_result) == false) return lhs_result;

  return DIGEST_SUCCESS;
}

DigestResult digest_postfix_inc(Token *expression, Expression &expr) {
  Token *lhs, *op;
  expand_tokens(expression, op, lhs);

  expr.type = EXPR_FUNCTION_CALL;
  expr.function_call.op = TOKEN_OP_POSTFIX_INC;
  expr.function_call.identifier = op_to_substring(expr.function_call.op);
  expr.function_call.params = call_params_alloc(1);
  expr.function_call.num_params = 1;

  DigestResult lhs_result = digest_expression(lhs, expr.function_call.params[0]);
  if (is_success(lhs_result) == false) return lhs_result;

  return DIGEST_SUCCESS;
}

DigestResult digest_postfix_dec(Token *expression, Expression &expr) {
  Token *lhs;
  expand_tokens(expression, lhs);

  expr.type = EXPR_FUNCTION_CALL;
  expr.function_call.op = TOKEN_OP_POSTFIX_DEC;
  expr.function_call.identifier = op_to_substring(expr.function_call.op);
  expr.function_call.params = call_params_alloc(1);
  expr.function_call.num_params = 1;

  DigestResult lhs_result = digest_expression(lhs, expr.function_call.params[0]);
  if (is_success(lhs_result) == false) return lhs_result;

  return DIGEST_SUCCESS;
}

DigestResult digest_array_index(Token *expression, Expression &expr) {
  Token *array, *index;
  expand_tokens(expression, array, index);

  expr.type = EXPR_FUNCTION_CALL;
  expr.function_call.op = TOKEN_OP_ARRAY_INDEX;
  expr.function_call.params = call_params_alloc(2);
  expr.function_call.num_params = 2;

  DigestResult array_result = digest_expression(array, expr.function_call.params[0]);
  if (is_success(array_result) == false) return array_result;

  DigestResult index_result = digest_expression(index, expr.function_call.params[1]);
  if (is_success(index_result) == false) return index_result;

  return DIGEST_SUCCESS;
}

DigestResult digest_function_call(Token *expression, Expression &expr) {
  Token *function_identifier, *function_call_params;
  expand_tokens(expression, function_identifier, function_call_params);

  expr.type = EXPR_FUNCTION_CALL;
  expr.function_call.op = TOKEN_NONE;
  expr.function_call.num_params = count_subtokens(function_call_params);
  expr.function_call.params = call_params_alloc(expr.function_call.num_params);
  expr.function_call.identifier = function_identifier->substring;

  int i = 0;
  for(Token *call_param = function_call_params->start; call_param != NULL; call_param = call_param->next) {
    DigestResult result = digest_expression(call_param, expr.function_call.params[i++]);
    if (is_success(result) == false) return result;
  }

  return DIGEST_SUCCESS;
}

DigestResult digest_member(Token *expression, Expression &expr) {
  Token *object, *field_identifier;
  expand_tokens(expression, object, field_identifier);

  expr.type = EXPR_FIELD_DEREFERENCE;
  expr.field_dereference.field_identifier = field_identifier->substring;

  expr.field_dereference.object = call_params_alloc(1);
  DigestResult obj_result = digest_expression(object, *expr.field_dereference.object);
  if (is_success(obj_result) == false) return obj_result;


  return DIGEST_SUCCESS;
}

DigestResult digest_integer_literal(Token *expression, Expression &expr) {
  expr.type = EXPR_NUMERIC_LITERAL;
  expr.numeric_literal.literal = expression->substring;
  return DIGEST_SUCCESS;
}

DigestResult digest_string_literal(Token *expression, Expression &expr) {
  expr.type = EXPR_STRING_LITERAL;
  expr.string_literal.literal = expression->substring;
  return DIGEST_SUCCESS;
}

DigestResult digest_identifier(Token *expression, Expression &expr) {
  expr.type = EXPR_VARIABLE;
  expr.variable.identifier = expression->substring;
  expr.variable.variable = NULL;
  return DIGEST_SUCCESS;
}

DigestResult digest_expression(Token *expression, Expression &expr) {
  switch(expression->type) {
  case TOKEN_BINARY_EXPRESSION:
    return digest_binary_expression(expression, expr);
  case TOKEN_UNARY_EXPRESSION:
    return digest_unary_expression(expression, expr);
  case TOKEN_OP_POSTFIX_INC:
    return digest_postfix_inc(expression, expr);
  case TOKEN_OP_POSTFIX_DEC:
    return digest_postfix_dec(expression, expr);
  case TOKEN_OP_ARRAY_INDEX:
    return digest_array_index(expression, expr);
  case TOKEN_OP_FUNCTION_CALL:
    return digest_function_call(expression, expr);
  case TOKEN_OP_MEMBER:
    return digest_member(expression, expr);
  case TOKEN_INTEGER_LITERAL:
    return digest_integer_literal(expression, expr);
  case TOKEN_STRING_LITERAL:
    return digest_string_literal(expression, expr);
  case TOKEN_IDENTIFIER:
    return digest_identifier(expression, expr);
  default:
    halt();
  }

  return DIGEST_SUCCESS;
}

AssignmentType to_assignment_type(TokenType op_type) {
  return (AssignmentType)op_type;
}

DigestResult digest_assignment_statement(Token *assignment_statement, FunctionStatement &statement) {
  Token *lhs, *op, *rhs;
  expand_tokens(assignment_statement, lhs, op, rhs);

  statement.type = FS_ASSIGNMENT;
  statement.assignment.op = to_assignment_type(op->type);

  statement.assignment.lhs = call_params_alloc(1);
  DigestResult lhs_result = digest_expression(lhs, *statement.assignment.lhs);
  if (is_success(lhs_result) == false) return lhs_result;

  statement.assignment.rhs = call_params_alloc(1);
  DigestResult rhs_result = digest_expression(rhs, *statement.assignment.rhs);
  if (is_success(rhs_result) == false) return rhs_result;

  return DIGEST_SUCCESS;
}

DigestResult digest_return_statement(Token *assignment_statement, FunctionStatement &statement) {
  Token *tok_expr;
  expand_tokens(assignment_statement, tok_expr);

  statement.type = FS_RETURN;
  statement.return_statement.retval = expressions_alloc(1);
  DigestResult result = digest_expression(tok_expr, *statement.return_statement.retval);
  if (!is_success(result)) return result;

  return DIGEST_SUCCESS;
}

DigestResult digest_inline_llvm(Token *inline_llvm, FunctionStatement &statement) {
  Token *raw_llvm;
  expand_tokens(inline_llvm, raw_llvm);

  statement.type = FS_LLVM;
  statement.llvm.raw_llvm = raw_llvm->substring;

  return DIGEST_SUCCESS;
}

DigestResult digest_function_statement(Token *tok_function_statement, FunctionStatement &fn_statement) {
  switch(tok_function_statement->type) {
  case TOKEN_VARIABLE_DEFINITION:
    fn_statement.type = FS_VARIABLE;
    return digest_variable_definition(tok_function_statement, fn_statement.varaible);
  case TOKEN_ASSIGMENT_STATEMENT:
    return digest_assignment_statement(tok_function_statement, fn_statement);
  case TOKEN_RETURN_STATEMENT:
    return digest_return_statement(tok_function_statement, fn_statement);
  case TOKEN_EXPRESSION_STATEMENT:
    assert(tok_function_statement->start == tok_function_statement->end);
    fn_statement.type = FS_EXPRESSION;
    fn_statement.expression = expressions_alloc(1);
    return digest_expression(tok_function_statement->start, *fn_statement.expression);
  case TOKEN_INLINE_LLVM:
    return digest_inline_llvm(tok_function_statement, fn_statement);
  //case TOKEN_FUNCTION_DEFINITION:
  //case TOKEN_LLVM_TYPE_DEFINITION:
  //case TOKEN_STRUCT_DEFINITION:
  // ...
  default: 
    halt();
  }

  return DIGEST_SUCCESS;
}

DigestResult digest_function_param_definition(Token *tok_param, ParamDefinition &param) {
  if (tok_param->type == TOKEN_VARARG_PARAM) {
    param.type = PD_VARARGS;
    return DIGEST_SUCCESS;
  } else {
    param.type = PD_VARIABLE;
    return digest_variable_definition(tok_param, param.variable);
  }
}

DigestResult make_digest_error(const FileLocation &location, const char *format_str, ...) {
  DigestResult result;
  result.result = RESULT_ERROR;
  result.error.location = location;
  result.error.error_string = format_str;
  return result;
}

DigestResult digest_function_definition(Token *function_definition, ProgramStatement &statement) {
  Token *tok_retval_type, *tok_identifier, *tok_params, *tok_body;
  expand_tokens(function_definition, tok_retval_type, tok_identifier, tok_params, tok_body);

  statement.type = PS_FUNCTION;
  statement.function.retval_type = type_decl_alloc(1);
  DigestResult retval_result = digest_type_declaration(tok_retval_type, *statement.function.retval_type);
  if (is_success(retval_result) == false) return retval_result;

  switch(tok_identifier->type) {
  case TOKEN_IDENTIFIER: {
    statement.function.identifier = tok_identifier->substring;
  } break;
  case TOKEN_OPERATOR_IDENTIFIER: {
    assert(tok_identifier->start == tok_identifier->end);
    statement.function.op = tok_identifier->start->type;
    statement.function.identifier = op_to_substring(statement.function.op);
  } break;
  default:
    halt();
  }

  assert(statement.function.identifier.length > 0);

  statement.function.num_params = count_subtokens(tok_params);
  statement.function.params = params_alloc(statement.function.num_params);
  int i = 0;
  for(Token *tok_param = tok_params->start; tok_param != NULL; tok_param = tok_param->next) {
    ParamDefinition &fn_param = statement.function.params[i++];
    DigestResult result = digest_function_param_definition(tok_param, fn_param);
    if (is_success(result) == false) return result;
    if (fn_param.type == PD_VARARGS && i < statement.function.num_params) {
      return make_digest_error(tok_param->location, "Varargs must be last parameter");
    }
  }

  statement.function.num_statements = count_subtokens(tok_body);
  statement.function.body = function_body_alloc(statement.function.num_statements);
  int j = 0;
  for(Token *tok_function_statement = tok_body->start; tok_function_statement != NULL; tok_function_statement = tok_function_statement->next) {
    FunctionStatement &fn_statement = statement.function.body[j++];
    DigestResult result = digest_function_statement(tok_function_statement, fn_statement);
    if (is_success(result) == false) return result;
  }

  return DIGEST_SUCCESS;
}

DigestResult digest_extern_function_declaration(Token *extern_function_declaration, ProgramStatement &statement) {
  Token *tok_retval_type, *tok_identifier, *tok_params;
  expand_tokens(extern_function_declaration, tok_retval_type, tok_identifier, tok_params);

  statement.type = PS_EXTERN_FUNCTION;
  statement.extern_function.retval_type = type_decl_alloc(1);
  DigestResult retval_result = digest_type_declaration(tok_retval_type, *statement.extern_function.retval_type);
  if (is_success(retval_result) == false) return retval_result;

  statement.extern_function.identifier = tok_identifier->substring;

  statement.extern_function.num_params = count_subtokens(tok_params);
  statement.extern_function.params = params_alloc(statement.extern_function.num_params);
  int i = 0;
  for(Token *tok_param = tok_params->start; tok_param != NULL; tok_param = tok_param->next) {
    ParamDefinition &fn_param = statement.extern_function.params[i++];
    DigestResult result = digest_function_param_definition(tok_param, fn_param);
    if (is_success(result) == false) return result;
    if (fn_param.type == PD_VARARGS && i < statement.function.num_params) {
      return make_digest_error(tok_param->location, "Varargs must be last parameter");
    }
  }

  return DIGEST_SUCCESS;
}

//
//
//if (is_success(result) == false) return result;
//
DigestResult digest_program(Token *program_token, Program &program) {
  program.num_statements = count_subtokens(program_token);
  program.statement = program_statment_alloc(program.num_statements);

  int i = 0;
  for(Token *program_statement = program_token->start; program_statement != NULL; program_statement = program_statement->next) {
    ProgramStatement &cur_statement = program.statement[i++];
    cur_statement.token = program_statement;

    switch(program_statement->type) {
    case TOKEN_EXTERNAL_FUNCTION_DECLARATION: {
      DigestResult result = digest_extern_function_declaration(program_statement, cur_statement);
      if (is_success(result) == false) return result;
    }  break;
    //case TOKEN_TYPEDEF_DEFINITION:
    //  DigestResult result = digest_variable_definition(program_statement, cur_statement);
    //  if (is_success(result) == false) return result;
    //}  break;
    case TOKEN_LLVM_TYPE_DEFINITION: {
      DigestResult result = digest_llvm_type_definition(program_statement, cur_statement);
      if (is_success(result) == false) return result;
    }  break;
    case TOKEN_VARIABLE_DEFINITION: {
      cur_statement.type = PS_VARIABLE;
      DigestResult result = digest_variable_definition(program_statement, cur_statement.varaible);
      if (is_success(result) == false) return result;
    }  break;
    case TOKEN_STRUCT_DEFINITION: {
      DigestResult result = digest_struct_definition(program_statement, cur_statement);
      if (is_success(result) == false) return result;
    } break;
    //case TOKEN_ENUM_DEFINITION:
    //  typecheck_enum_definition(program_statement);
    //  break;
    case TOKEN_FUNCTION_DEFINITION: {
      DigestResult result = digest_function_definition(program_statement, cur_statement);
      if (is_success(result) == false) return result;
    } break;
    case TOKEN_IMPORT_STATEMENT:
      cur_statement.type = PS_IMPORT;
      cur_statement.import.file = program_statement->start->substring;
      //cur_statement.import.
      break;
    default:
      halt();
    }

    assert(cur_statement.type);
  }

  return DIGEST_SUCCESS;
};

DeclarationResult db_add_global_types(const Program &program) {
  for(int i = 0; i < program.num_statements; ++i) {
    ProgramStatement &statement = program.statement[i];

    switch(statement.type) {
    case PS_IMPORT:
    case PS_EXTERN_FUNCTION:
    case PS_VARIABLE:
    case PS_FUNCTION: 
      break;
    case PS_LLVM_TYPE: {
      TypeDef new_type = {};
      new_type.kind = LLVM_TYPE;
      new_type.identifier = statement.llvm_type.identifier;
      new_type.llvm_def.new_type_definition = &statement.llvm_type;
      new_type.token = statement.token;
      DeclarationResult result = db_try_add_type(new_type);
      if (is_success(result) == false) return result;
    } break;
    case PS_STRUCT: {
      TypeDef new_type = {};
      new_type.kind = STRUCT_TYPE;
      new_type.identifier = statement.struct_def.identifier;
      new_type.struct_def.new_type_definition = &statement.struct_def;
      new_type.token = statement.token;
      DeclarationResult result = db_try_add_type(new_type);
      if (is_success(result) == false) return result;
    } break;
    //case PS_TYPEDEF:
    //  emit_base_types()
    //  break;
    default:
      halt();
    }
  }

  return DECLARATION_SUCCESS;
}

TypecheckResult db_add_global_declarations(const Program &program) {
  for(int i = 0; i < program.num_statements; ++i) {
    ProgramStatement &statement = program.statement[i];

    switch(statement.type) {
    case PS_EXTERN_FUNCTION: {
      TypecheckResult result = db_add_external_function_declaration(statement.extern_function);
      if (is_success(result) == false) return result;
    } break;
    case PS_VARIABLE: {
      TypecheckResult result = db_add_variable_declaration(statement.varaible);
      if (is_success(result) == false) return result;
    } break;
    case PS_FUNCTION: {
      TypecheckResult result = db_add_function_declaration(statement.function);
      if (is_success(result) == false) return result;
    } break;
    case PS_IMPORT:
    case PS_LLVM_TYPE:
    case PS_STRUCT:
      break;
    default:
      halt();
    }
  }

  return TYPECHECK_SUCCESS;
}

CompileResult compile_program(const char *source_file) {
  std::string dest_file = to_dest_file(source_file);
  LLVMInit(dest_file.c_str());

  ParseResult parse_result = parse_program(source_file);
  if (is_success(parse_result) == false) return make_result(parse_result);

  Program program;
  DigestResult result_digest = digest_program(parse_result.start, program);
  if (is_success(result_digest) == false) return make_result(result_digest);

  DeclarationResult result_types = db_add_global_types(program);
  if (is_success(result_types) == false) return make_result(result_types);

  TypecheckResult result_declarations = db_add_global_declarations(program);
  if (is_success(result_declarations) == false) return make_result(result_declarations);

  TypecheckResult typecheck_result = typecheck_program(program);
  if (is_success(typecheck_result) == false) return make_result(typecheck_result);

  EmitResult emit_result = emit_program(program, dest_file.c_str());
  if (is_success(emit_result) == false) return make_result(typecheck_result);
  
  g.llvm.module->dump();
  IRCompile(g.llvm);

  return COMPILE_SUCCESS;
}

bool is_success(const CompileResult &result) {
  return (result.result != RESULT_ERROR);
}


