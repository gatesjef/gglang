// GGParser.cpp

#include "Precompile.h"
#include "GGParser.h"
#include "GGUtils.h"
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

  TOKEN_STATEMENT_DEFINITION,
  TOKEN_STATEMENT_PARAMS,
  TOKEN_STATEMENT_PARAM,
  TOKEN_STATEMENT_BODY,
  TOKEN_STATEMENT_CALL,
  TOKEN_STATEMENT_CALL_PARAM,
    TOKEN_STATEMENT_CALL_PARAM_IDENTIFIER,
    TOKEN_STATEMENT_CALL_PARAM_EXPRESSION,
  TOKEN_STATEMENT_CALL_BODY,
  TOKEN_YIELD_STATEMENT,

  TOKEN_CONTINUE_STATEMENT,
  TOKEN_BREAK_STATEMENT,
  //TOKEN_IF_STATEMENT,
  //TOKEN_FOR_STATEMENT,
  //TOKEN_SWITCH_STATEMENT,
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

  char *file_data;

  const char *current_line_start;
  int current_line_number;
  const char *current_char;

  std::string filepath;
  std::string current_directory;

  //SubString current_filename;

  Token program;
  Error error;

  //TokenPool token_pool;
  Token token_pool[MAX_TOKENS];
  int num_tokens;

  //std::vector<std::string> files;
  //std::vector<char *> file_data;
  //int current_file_idx;
};

enum TypeDeclarationKind {
  //BASE_TYPE,
  //ARBITRARY_FLOAT,
  //TYPEDEF_TYPE,

  //ARBITRARY_INTEGER,

  TYPE_BASE,
  //TYPE_LLVM,
  //TYPE_STRUCT,
  TYPE_ARRAY,
  TYPE_POINTER,
  TYPE_FUNCTION,
};

struct StructDef;

//struct LLVMTypeDef {
//  SubString identifier;
//  Token *raw_llvm;
//  // size?
//
//  llvm::Type *llvm_type;
//  Token *token;
//};


struct TypeDeclaration;
struct Expression;

//enum TypeDeclarationKind {
//  TYPE_BASE,
//  TYPE_FUNCTION,
//  TYPE_POINTER,
//  TYPE_ARRAY,
//};

//struct TypeInstance {
//}

struct BaseTypeDeclaration {
  SubString identifier;
};

struct FunctionTypeDeclaration {
  TypeDeclaration *retval;
  TypeDeclaration *params;
  int num_params;
};

struct PointerTypeDeclaration {
  TypeDeclaration *sub_type;
};

struct ArrayTypeDeclaration {
  TypeDeclaration *sub_type;
  Expression *count_expression;
  uint64_t count;
};

struct TypeDeclaration {
  TypeDeclarationKind kind;

  union {
    BaseTypeDeclaration base_type;
    FunctionTypeDeclaration function_type;
    PointerTypeDeclaration pointer_type;
    ArrayTypeDeclaration array_type;
  };

  //TypeDef *type;
};

enum TypeDefintitionKind {
  TYPE_LLVM,
  TYPE_STRUCT,
  // TYPE_TYPEDEF,
};

struct LLVMTypeDefinition {
  SubString identifier;
  SubString raw_llvm;
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

typedef VariableDefinition FieldDefinition;

struct StructDefinition {
  SubString identifier;

  //Token *tok_fields;
  FieldDefinition *fields;
  int num_fields;
};

struct TypeDefinition {
  TypeDefintitionKind type;
  union {
    LLVMTypeDefinition *llvm_type_definition;
    StructDefinition *struct_definition;
    // typedef
  };

  TypeDeclaration *decl;
  llvm::Type      *llvm_type;
};

enum Linkage {
  LINKAGE_INTERNAL,
  LINKAGE_EXTERNAL,
};

#include "list"
#define Table std::list

struct VariableDefinition;
struct FunctionDefinition;
struct ExternFunctionDeclaration;
struct StatementDefinition;
struct StatementCallParam;

struct TypeDefinitionDBEntry {
  int scope;
  TypeDefinition *type;

 //TypeDef *varaible;
};

struct StatementDefinitionDBEntry {
  int scope;
  StatementDefinition *statement;
};

enum VariableType {
  VT_VARIABLE,
  VT_STATEMENT_PARAM,
};

struct VaraibleDefinitionDBEntry {
  int scope;
  SubString identifier;
  VariableType type;

  VariableDefinition *variable;
  StatementCallParam *statement_param;
};

struct FunctionDefinitionDBEntry {
  int scope;
  FunctionDefinition        *function;
  ExternFunctionDeclaration *extern_function;
};

struct SymbolDatabase {
  Table<FunctionDefinitionDBEntry> functions;
  Table<VaraibleDefinitionDBEntry> variables;
  Table<TypeDefinitionDBEntry> types;
  Table<StatementDefinitionDBEntry> statements;

  int scope;
};

enum ParamDefinitionType {
  PD_VARIABLE,
  PD_VARARGS,
};

struct ParamDefinition {
  ParamDefinitionType type;
  VariableDefinition variable;
};

struct StatementParam {
  TypeDeclaration type;
  SubString identifier;
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
  OP_EXTEND_INT_TO_FLOAT,
  OP_CAST_POINTER,
  OP_CAST_ARRAY_TO_POINTER,

  //OP_MEMBER_DEREFERENCE,

  // OP_CAST
  // OP_TRUNC
  // OP_SEXT
  // OP_UEXT
  // OP_FEXT
};

struct FunctionDefinition {
  SubString identifier;
  TokenType op;

  TypeDeclaration *retval_type;

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

  Expression *params;
  int num_params;

  FunctionDefinition        *function;
  ExternFunctionDeclaration *extern_function;
  IntrinsicOperationType    intrinsic;
  TypeDeclaration           *dest_type;

};

struct VariableReference {
  SubString identifier;

  VariableDefinition *variable;
};

struct StringLiteral  {
  SubString literal;
  TypeDeclaration *type;
};

struct NumericLiteral {
  SubString literal;
  TypeDeclaration *type;
};

struct FieldDereference {
  Expression *object;
  SubString field_identifier;
  int field_idx;
  //TypeDef *type;
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
  EXPR_FLOAT_LITERAL,
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
  //Token *token;

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

enum DependancyType {
  DEPENDANCY_TYPE,
  DEPENDANCY_FUNCTION,
  DEPENDANCY_VARIABLE,
  DEPENDANCY_STATEMENT,
};

struct CompilationDependancy {
  DependancyType type;
  SubString identifier;
};

struct PipelineResult {
  FileLocation location;
  ResultType result;
  Error error;
  CompilationDependancy dependancy;
  //TypeDef *type;
};

const PipelineResult PIPELINE_SUCCESS = {};

PipelineResult typecheck_expression(Expression &expr, TypeDeclaration *&decl);

enum ProgramStatementType {
  PS_NONE,
  PS_IMPORT,
  PS_EXTERN_FUNCTION,
  PS_FUNCTION,
  PS_VARIABLE,
  PS_STATEMENT,
  PS_STRUCT,
  PS_LLVM_TYPE,
};

enum FunctionStatementType {
  FS_STATEMENT_CALL,
  FS_EXTERN_FUNCTION,
  //FS_FUNCTION,
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

struct ExternFunctionDeclaration {
  SubString identifier;

  TypeDeclaration *retval_type;
  ParamDefinition *params;
  int num_params;

  llvm::Function *llvm_function;
};

enum StatementCallParamType {
  SCP_IDENTIFIER,
  SCP_EXPRESSION,
};

struct StatementCallParam {
  StatementCallParamType type;

  union {
    SubString identifier;
  };
};

struct StatementCall {
  SubString identifier;
  StatementCallParam *params;
  int num_params;

  FunctionStatement *body;
  int num_statement_body;
};

struct FunctionStatement {
  FunctionStatementType type;

  union {
    ExternFunctionDeclaration extern_function;
    FunctionDefinition function;
    VariableDefinition varaible;
    StatementCall statement_call;
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
  ParserState *parser;
};

//struct StatementParam {
//};

//struct StatementBody {
//};

struct StatementYieldParam {
  SubString keyword;
  SubString identifier;
};

struct StatementDefinition {
  SubString identifier;

  StatementParam *statement_params;
  int num_statement_params;

  StatementYieldParam *statement_yield_params;
  int num_statement_yield_params;

  FunctionStatement *body;
  int num_body_statements;
};

struct ProgramStatement {
  Token *token;
  ProgramStatementType type;
  union {
    ImportStatement import;
    ExternFunctionDeclaration extern_function;
    FunctionDefinition function;
    VariableDefinition varaible;
    StatementDefinition statement;
    StructDefinition struct_def;
    LLVMTypeDefinition llvm_type;
  };
};

struct Program {
  ProgramStatement *statement;
  int num_statements;
};

struct G {
  //ParserState parser;
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
  llvm::InitializeNativeTarget();

  g.llvm.context = &llvm::getGlobalContext();
  g.llvm.builder = new llvm::IRBuilder<>(*g.llvm.context);
  g.llvm.module = new llvm::Module(dest_file, *g.llvm.context);

  static std::string ErrStr;
  g.llvm.engine =
    llvm::EngineBuilder(g.llvm.module)
    .setErrorStr(&ErrStr)
    //.setMCJITMemoryManager(std::unique_ptr<HelpingMemoryManager>(
    //new HelpingMemoryManager(this)))
    .create();
  if (g.llvm.engine == NULL) {
    halt();
  }

  //return llvm;
}

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

//typedef PipelineResult PipelineResult;
//struct EmitResult {
//  ResultType result;
//  Error error;
//};

const PipelineResult EMIT_SUCCESS = {};

PipelineResult make_emit_error(const char *error_fmt, ...) {
  PipelineResult retval;
  retval.result = RESULT_ERROR;
  return retval;
}

ParseResult make_sequence_error(ParserState &parser, int i) {
  ParseResult retval;
  retval.result = RESULT_ERROR;
  //retval.error.location.file_index = parser.current_file_idx;
  retval.error.location.parser = &parser;
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
  //retval.error.location.file_index = parser.current_file_idx;
  retval.error.location.parser = &parser;
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

ParseResult parse_statement_exact(ParserState &parser) {
  return parse_exact(parser, "statement");
}

ParseResult parse_semicolon(ParserState &parser) {
  return parse_exact_error(parser, ";", "Missing semicolon.");
}

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
  //new_token->location.file_index = parser.current_file_idx;
  new_token->location.parser = &parser;
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
    new_token->location.parser = &parser;
    new_token->location = result_subtokens.start->location;
  } else {
    //new_token->location.file_index = parser.current_file_idx;
    new_token->location.parser = &parser;
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
    new_token->location.parser = &parser;
    new_token->location = subtoken->location;
  } else {
    //new_token->location.file_index = parser.current_file_idx;
    new_token->location.parser = &parser;
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

  //new_token->location.file_index = parser.current_file_idx;
  new_token->location.parser = &parser;
  new_token->location.line = parser.current_line_number;
  new_token->location.column = 0;

  return make_success(new_token);
}

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
        if (*c == 'n') {
          break;
        } else {
          return make_error(parser, "Unknown escape sequence");
        }

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

typedef ParseResult (*ParseFn)(ParserState&);

//void halt();

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
  //if (retval.result == RESULT_SUCCESS) {
  //  ParseResult import_result = emit_import_statement(parser, retval);
  //  if (is_success(import_result) == false) return import_result;
  //  return retval;
  //}

  return retval;

  //if (retval.sequence_index == 0) {
  //  return PARSE_NONE;
  //} else {
  //  return make_error(parser, "Invalid import statement");
  //}
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

//ParseResult parse_constexpr_integer(ParserState &parser) {
//  return parse_expression(parser);
//  // TODO;
//  //return parse_integer_literal(parser);
//}

ParseResult parse_expression(ParserState &parser);
ParseResult parse_constexpr(ParserState &parser) {
  return parse_expression(parser);
}

ParseResult parse_array_type(ParserState &parser, Token *base_type) {
  ParseResult result = parse_exact(parser, "[");
  if (is_success(result) == false) return result;

  ParseResult size = parse_constexpr(parser);
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

ParseResult parse_statement_param(ParserState &parser) {
  static const ParseFn fns[] = { 
    parse_type_declaration,
    parse_identifier,
  };
  static const int num = ARRAYSIZE(fns);
  ParseResult result = parse_sequence(parser, fns, num);
  return make_result(parser, TOKEN_STATEMENT_PARAM, result);
}

ParseResult parse_statement_params(ParserState &parser) {
  ParseResult result = parse_zero_or_more_separated(parser, parse_statement_param, ",");
  if (is_success(result) == false) return result;
  return make_result(parser, TOKEN_STATEMENT_PARAMS, result);
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

ParseResult parse_statement_identifier(ParserState &parser) {
  return parse_identifier(parser);
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
  ParseResult result_op = parse_exact(parser, ".");
  if (is_success(result_op) == false) return result_op;

  ParseResult member = parse_member_identifier(parser);
  if (is_success(member) == false) {
    return make_error(parser, "Invalid member identifier");
  }

  ParseResult result = make_result(parser, TOKEN_OP_MEMBER, lhs);
  if (is_success(result) == false) return result;
  append_result(result.start, member);
  return result;
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

ParseResult parse_2char_assignment_operator(ParserState &parser) {
  static const ParseTable table[] = {
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

    //"&",  TOKEN_BITWISE_AND_OP, 10,
    //"|",  TOKEN_BITWISE_OR_OP,  11,
    "^",  TOKEN_BITWISE_XOR_OP, 12,
    "&&", TOKEN_LOGICAL_AND_OP, 13,
    "||", TOKEN_LOGICAL_OR_OP,  14,
  };

  ParseResult assignment_result = peek(parser, parse_2char_assignment_operator);
  if (is_success(assignment_result)) return PARSE_NONE;

  static const int num_entries = ARRAYSIZE(table);
  return parse_first_table(parser, table, num_entries);
}

ParseResult parse_any_operator(ParserState &parser) {
  static const ParseTable table[] = {
    "&&", TOKEN_LOGICAL_AND_OP, 13,
    "||", TOKEN_LOGICAL_OR_OP,  14,
    "++", TOKEN_PRE_INC_OP,     3,
    "--", TOKEN_PRE_DEC_OP,     3,
    "<<", TOKEN_LSHIFT_OP,  7,
    ">>", TOKEN_RSHIFT_OP,  7,
    "<=", TOKEN_CMP_LTE_OP, 8,
    ">=", TOKEN_CMP_GTE_OP, 8,
    "==", TOKEN_CMP_EQ_OP,  9,
    "!=", TOKEN_CMP_NEQ_OP, 9,

    "*",  TOKEN_MUL_OP, 5,
    "/",  TOKEN_DIV_OP, 5,
    "%",  TOKEN_REM_OP, 5,
    "+",  TOKEN_ADD_OP, 6,
    "-",  TOKEN_SUB_OP, 6,
    "<",  TOKEN_CMP_LT_OP,  8,
    ">",  TOKEN_CMP_GT_OP,  8,
    "&",  TOKEN_BITWISE_AND_OP, 10,
    "|",  TOKEN_BITWISE_OR_OP,  11,
    "^",  TOKEN_BITWISE_XOR_OP, 12,
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
  ParseResult lparen = parse_left_paren(parser);
  if (is_success(lparen)) {
    ParseResult expr   = parse_expression(parser);
    if (!is_success(expr)) return expr;

    ParseResult rparen = parse_right_paren(parser);
    if (!is_success(rparen)) return make_error(parser, "expected ')'");
    return expr;
  }

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


ParseResult parse_binary_expression(ParserState &parser, int minimum_precedence, const ParseResult &arg_lhs) {
  ParseResult lhs = arg_lhs;

  while(1) {
    int precedence;
    {
      ParseResult op = peek(parser, parse_binary_operator);
      if (is_error(op)) return op;
      if (is_none(op)) {
        return lhs;
      }

      precedence = get_precedence(op);
      if (precedence < minimum_precedence) {
        return lhs;
      }
    }

    ParseResult op = parse_binary_operator(parser);
    ParseResult rhs = parse_unary_expression(parser);
    assert(rhs.result >= 0 && rhs.result <= 3);
    if (is_success(rhs) == false) {
      return make_error(parser, "Invalid right hand side in unary expression ");
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

//unary_expr = unary_op expr
//
//expr = (expr)
//       unary_expr
//       unary_expr bin_op expr

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

ParseResult parse_yield_exact(ParserState &parser) {
  return parse_exact(parser, "yield");
}

ParseResult parse_yield_statement(ParserState &parser) {
  ParseResult yield_result = parse_yield_exact(parser);
  if (is_success(yield_result) == false) return yield_result;

  ParseResult identifier_result = parse_identifier(parser);
  if (is_error(identifier_result)) return identifier_result;

  ParseResult semicolon_result = parse_semicolon(parser);
  if (is_success(semicolon_result) == false) {
    make_error(parser, "Expected ';' in yield statement");
  }

  return make_result(parser, TOKEN_YIELD_STATEMENT, identifier_result);
}

ParseResult parse_statement(ParserState &parser);

ParseResult parse_statement_statement(ParserState &parser) {
  ParseResult parse_yield = parse_yield_statement(parser);
  if (is_success(parse_yield)) return parse_yield;
  if (is_error(parse_yield)) return parse_yield;

  return parse_statement(parser);
}

ParseResult parse_statement_body(ParserState &parser) {
  ParseResult result = parse_zero_or_more(parser, parse_statement_statement);
  return make_result(parser, TOKEN_STATEMENT_BODY, result);
}

ParseResult parse_left_brace(ParserState &parser) {
  return parse_exact(parser, "{");
}

ParseResult parse_right_brace(ParserState &parser) {
  return parse_exact(parser, "}");
}

ParseResult parse_statement_definition(ParserState &parser) {
  ParseFn statments[] = {
    parse_statement_exact,
    parse_statement_identifier,
    parse_left_paren,
    parse_statement_params,
    parse_right_paren,
    parse_left_brace,
    parse_statement_body,
    parse_right_brace,
  };
  static const int num_statements = ARRAYSIZE(statments);

  ParseResult result = parse_sequence(parser, statments, num_statements);
  return make_result(parser, TOKEN_STATEMENT_DEFINITION, result);
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

ParseResult parse_variable_definition_part_2(ParserState &parser, ParseResult &partial_results) {
  if (is_success(parse_declaration_assignment_operator(parser))) {
    ParseResult expr = parse_expression(parser);
    if (is_success(expr) == false) {
      return make_error(parser, "Invalid expression for variable initilizer");
    }
    append_result_old(partial_results, expr);
  } 

  if (is_success(parse_semicolon(parser)) == false) {
      return make_error(parser, "Missing semicolon");
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

//ParseResult parse_if_statement (ParserState &parser) {
//  static const ParseFn statements[] = {
//    parse_if_exact,
//    parse_left_paren,
//    parse_expression,
//    parse_right_paren,
//    parse_statement,
//  };
//  static const int num_statements = ARRAYSIZE(statements);
//
//  ParseResult result = parse_sequence(parser, statements, num_statements);
//  if (is_success(result) == false) return result;
//
//  if (is_success(parse_exact(parser, "else"))) {
//    ParseResult else_statement = parse_statement(parser);
//    if (is_error(result)) return result;
//    else if (is_none(result)) {
//      return make_error(parser, "Missing statement for else clause");
//    }
//    append_result(result.start, else_statement);
//  }
//
//  return make_result(parser, TOKEN_IF_STATEMENT, result);
//}

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

ParseResult parse_statement_call_param_identifier(ParserState &parser) {
  ParseResult result = parse_identifier(parser);
  if (is_success(result) == false) return result;
  
  return make_result(parser, TOKEN_STATEMENT_CALL_PARAM_IDENTIFIER, result);
}


ParseResult parse_statement_call_params(ParserState &parser) {
  static const ParseFn statements[] = {
    //parse_expression,
    parse_statement_call_param_identifier, 
    //varaible_declaration,
    //statement,
  };
  static const int num_statements = ARRAYSIZE(statements);

  ParseResult result = parse_first_of_deep(parser, statements, num_statements);
  return make_result(parser, TOKEN_STATEMENT_CALL_PARAM, result);
}

ParseResult parse_statement_call_body(ParserState &parser) {
  ParseResult result = parse_zero_or_more(parser, parse_statement);
  return make_result(parser, TOKEN_STATEMENT_CALL_BODY, result);
}

ParseResult parse_statement_call(ParserState &parser) {
  static const ParseFn statements[] = {
    parse_identifier,
    parse_left_paren,
    parse_statement_call_params,
    parse_right_paren,
    parse_left_brace,
    parse_statement_call_body,
    parse_right_brace,
  };
  static const int num_statements = ARRAYSIZE(statements);

  ParseResult result = parse_sequence(parser, statements, num_statements);
  return make_result(parser, TOKEN_STATEMENT_CALL, result);
}

ParseResult parse_function_definition(ParserState &parser);

ParseResult parse_variable_or_function_definition(ParserState &parser);

ParseResult parse_statement(ParserState &parser) {
  {
    static const ParseFn statements[] = {
      parse_block_statement,
      parse_return_statement,
      // parse_for_statement,
      // parse_if_statement,
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
      parse_statement_call,
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
    parse_statement_definition,         // 
    parse_variable_or_function_definition,
  };
  static const int num_statements = ARRAYSIZE(statements);
  ParseResult result = parse_first_of(parser, statements, num_statements);
  return result;
}

void path_split_directory_filename(const char *path, std::string &directory, SubString &filename) {
  int length = strlen(path);
  for(int i = length - 1; i >= 0; --i) {
    if (path[i] == '\\') {
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

void parser_init(ParserState &parser, std::string &filepath) {
  SubString filename;
  parser.filepath = filepath;
  path_split_directory_filename(filepath.c_str(), parser.current_directory, filename);
  parser.num_tokens = 0;
}

std::string make_full_path(const std::string &directory, const SubString &relative_path) {
  // TODO: handle ..\ etc
  std::string retval = directory;
  //if (directory.length > 0) {
  //  retval.append(directory.start, directory.length);
  //}
  if (relative_path.length > 0) {
    retval.append(relative_path.start, relative_path.length);
  }

  char buffer[FILENAME_MAX];
  char *result = _fullpath(buffer, retval.c_str(), FILENAME_MAX); 
  assert(result != NULL);
  return std::string(result);
  //return retval;
}

//bool parser_lookup_file(ParserState &parser, const std::string &path) {
//  for(auto file : parser.files) {
//    if (file == path) return true;
//  }
//
//  return false;
//}

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

SubString make_substring(const char *str) {
  SubString retval;
  retval.start = str;
  retval.length = strlen(str);
  return retval;
}

ParseResult parse_program(ParserState &parser, const char *source_file) {
  //parser_init(g.parser);
  //ParseResult result = parser_add_file(g.parser, make_substring(source_file));
  //if (is_success(result) == false) return result;

  ParseResult retval = PARSE_EMPTY;
  //for(int i = 0; i < (int)g.parser.files.size(); ++i) {
    parser.file_data = file_read_all(source_file);
    if (parser.file_data == NULL) {
      return make_error(parser, "cannot open file %s", source_file);
    }
    parser.current_char = parser.file_data;
    parser.current_line_start = parser.current_char;
    parser.current_line_number = 0;


    //parser_set_file_index(g.parser, i);
    consume_whitespace(parser);
    ParseResult result = parse_zero_or_more(parser, parse_program_statements);
    if (is_error(result)) return result;
    if (*parser.current_char != 0) {
      return make_error(parser, "Unexpected token - couldn't parse program statement.");
    }
    append_result_old(retval, result);
  //}

  ParseResult program = make_result(parser, TOKEN_PROGRAM, retval);
  return program;
}

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////

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

PipelineResult emit_type_declaration(TypeDeclaration &declaration, llvm::Type *&llvm_type);

//struct DeclarationResult {
//  ResultType result;
//  Error error;
//};
//
//const DeclarationResult DECLARATION_SUCCESS = {};
//
//DeclarationResult make_db_error(const char *error, const std::string &substr) {
//  DeclarationResult retval;
//  retval.result = RESULT_ERROR;
//  retval.error.error_string = error + substr;
//  return retval;
//}
//
//DeclarationResult make_db_success() {
//  DeclarationResult retval;
//  retval.result = RESULT_SUCCESS;
//  return retval;
//}

std::string to_cstring(SubString &substring) {
  return std::string(substring.start, substring.length);
}

TypeDefinition *db_lookup_type_by_identifier(const SubString &identifier) {
  for(TypeDefinitionDBEntry &entry: g.db.types) {
    if (entry.type->type == TYPE_LLVM && substring_cmp(entry.type->llvm_type_definition->identifier, identifier) == true) {
      return entry.type;
    }
    else if (entry.type->type == TYPE_STRUCT && substring_cmp(entry.type->struct_definition->identifier, identifier) == true) {
      return entry.type;
    }
    //else if (type.kind == TYPEDEF_TYPE) {

    //}
  }

  return NULL;
};

bool db_type_exists(SubString &identifier) {
  TypeDefinition *type = db_lookup_type_by_identifier(identifier);
  if (type == NULL) return false;
  return true;
}

const int MAX_PARAMS = 128;

bool is_success(const PipelineResult &result) {
  return result.result == RESULT_SUCCESS;
}

FieldDefinition *struct_lookup_field(TypeDefinition &type, const SubString &identifier, int &field_idx) {
  assert(type.type == TYPE_STRUCT);
  StructDefinition &struct_def = *type.struct_definition;
  for(field_idx = 0; field_idx < struct_def.num_fields; ++field_idx) {
    FieldDefinition &field = struct_def.fields[field_idx];
    if (substring_cmp(field.identifier, identifier))
      return &field;
  }

  return NULL;
}

TypeDeclaration *make_array_type(TypeDeclaration *base_type, uint64_t count) {
  TypeDeclaration *new_type = new TypeDeclaration;
  new_type->kind= TYPE_ARRAY;
  new_type->array_type.sub_type = base_type;
  new_type->array_type.count_expression = NULL;
  new_type->array_type.count = count;
  return new_type;
}

TypeDeclaration *make_pointer_type(TypeDeclaration *base_type) {
  TypeDeclaration *pointer_type = new TypeDeclaration;
  pointer_type->kind= TYPE_POINTER;
  pointer_type->pointer_type.sub_type = base_type;
  return pointer_type;
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
  case TOKEN_CMP_NEQ_OP:
    return to_substring("operator !=");
    break;
  case TOKEN_CMP_EQ_OP:
    return to_substring("operator ==");
    break;
  case TOKEN_CMP_LTE_OP:
    return to_substring("operator <=");
    break;
  case TOKEN_CMP_GTE_OP:
    return to_substring("operator >=");
    break;
  case TOKEN_CMP_LT_OP:
    return to_substring("operator <");
    break;
  case TOKEN_CMP_GT_OP:
    return to_substring("operator >");
    break;
  case TOKEN_LOGICAL_AND_OP:
    return to_substring("operator &&");
    break;
  case TOKEN_LOGICAL_OR_OP:
    return to_substring("operator ||");
    break;
  default:
    halt();
  }

  return to_substring("error");
}

//bool is_success(const DeclarationResult &result) {
//  return result.result == RESULT_SUCCESS;
//}

PipelineResult make_error(const FileLocation &location, const char *format_str, ...) {
  PipelineResult  result;
  result.result = RESULT_ERROR;
  result.error.location = location;
  FORMAT_ERROR_STRING(result.error.error_string, format_str);
  return result;
}


//struct LLVMTypeResult {
//  ResultType result;
//  llvm::Type *type;
//  Error error;
//};

//llvm::Type *llvm_get_type(TypeDef *type);

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

typedef PipelineResult DigestResult;

DigestResult DIGEST_SUCCESS = {};

//bool is_success(const DigestResult &result) {
//  return result.result == RESULT_SUCCESS;
//}

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


DigestResult digest_base_type(Token *type_identifier, TypeDeclaration &type) {
  type.kind = TYPE_BASE;
  type.base_type.identifier = type_identifier->substring;
  return DIGEST_SUCCESS;
}

enum ConversionType {
  array_to_pointer,
  up_convert_integer,
  up_convert_float,
  up_convert_int_to_float,
  pointer_to_void_pointer,
};

const int MAX_CONVERSIONS = 8;

struct ConversionChain {
  int tier;
  ConversionType chain[MAX_CONVERSIONS];
  int num_conversions;
};

TypeDeclaration *apply_conversion(ConversionType type, TypeDeclaration *source, TypeDeclaration *dest= NULL) {
  switch(type) {
  case array_to_pointer: 
    return make_pointer_type(source->array_type.sub_type);
  case up_convert_integer:
    return dest;
  case up_convert_float:
    return dest;
  case up_convert_int_to_float:
    return dest;
  case pointer_to_void_pointer:
    return dest;
  default:
    halt();
  }

  return NULL;
}

int type_size(TypeDeclaration *type) {
  llvm::Type *llvm_type;
  PipelineResult result = emit_type_declaration(*type, llvm_type);
  assert(is_success(result));

  switch(type->kind) {
  case TYPE_ARRAY:
    // TODO 
    halt();
    break;
  case TYPE_POINTER:
    return g.llvm.module->getDataLayout()->getPointerSizeInBits();
  case TYPE_BASE: {
    TypeDefinition *decl = db_lookup_type_by_identifier(type->base_type.identifier);
    switch(decl->type) {
    case TYPE_LLVM: {
      uint32_t size = llvm_type->getPrimitiveSizeInBits();
      return size;
    }
    case TYPE_STRUCT: {
      llvm::StructType* llvm_struct_type = llvm::dyn_cast<llvm::StructType>(llvm_type);
      const llvm::StructLayout *layout = g.llvm.module->getDataLayout()->getStructLayout(llvm_struct_type);
      return (int)layout->getSizeInBits();
    } 
    default:
      halt();
    }
  } break;
  case TYPE_FUNCTION:
    halt();
  default:
    halt();
  }

  halt();
  return 0;
}

bool is_void(TypeDeclaration *type) {
  llvm::Type *llvm_type;
  PipelineResult result = emit_type_declaration(*type, llvm_type);
  assert(is_success(result));
  return llvm_type->isVoidTy();
}

bool is_integer(TypeDeclaration *type) {
  llvm::Type *llvm_type;
  PipelineResult result = emit_type_declaration(*type, llvm_type);
  assert(is_success(result));
  return llvm_type->isIntegerTy();
}

bool is_float(TypeDeclaration *type) {
  llvm::Type *llvm_type;
  PipelineResult result = emit_type_declaration(*type, llvm_type);
  assert(is_success(result));
  return llvm_type->isFloatTy();
}

bool is_pointer(TypeDeclaration *type) {
  return type->kind == TYPE_POINTER;
}

bool is_void_pointer(TypeDeclaration *type) {
  if (type->kind != TYPE_POINTER) return false;
  return is_void(type->pointer_type.sub_type);
}

bool is_array(TypeDeclaration *type) {
  return type->kind == TYPE_ARRAY;
}

bool can_convert(TypeDeclaration *source, TypeDeclaration *dest, ConversionChain &conversion) {
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

  if (is_integer(source) && is_float(dest) && type_size(source) <= type_size(dest)) {
    conversion.chain[conversion.num_conversions++] = up_convert_int_to_float;
    source = apply_conversion(up_convert_int_to_float, source, dest);
  }

  if (is_pointer(source) && is_void_pointer(dest)) {
    conversion.chain[conversion.num_conversions++] = pointer_to_void_pointer;
    source = apply_conversion(pointer_to_void_pointer, source);
  }

  // TODO: user defined
  llvm::Type *llvm_source;
  llvm::Type *llvm_dest;
  PipelineResult result_source = emit_type_declaration(*source, llvm_source);
  //if (is_success(result_source) == false) return result_source;
  assert(is_success(result_source));

  PipelineResult result_dest   = emit_type_declaration(*dest, llvm_dest);
  //if (is_success(result_dest) == false) return result_dest;
  assert(is_success(result_dest));

  return (llvm_source == llvm_dest);;
}

bool compare_function_param_types(TypeDeclaration **call_param_types, ParamDefinition *function_params, int num_params, ConversionChain *conversions) {
  for(int i = 0; i < num_params; ++i) {
    assert(function_params[i].type != PD_VARARGS);
    TypeDeclaration *t0 = call_param_types[i];
    TypeDeclaration *t1 = function_params[i].variable.type;
    assert(t0);
    assert(t1);

    if (can_convert(t0, t1, conversions[i]) == false) {
      return false;
    }
  }

  return true;
}

const FileLocation UNKNOWN_LOCATION = {};

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

void create_conversion_expression(ConversionType type, Expression &expression, Expression &original_expression, TypeDeclaration *dest_type) {
  switch(type) {
  case array_to_pointer: 
    expression.type = EXPR_FUNCTION_CALL;
    expression.function_call.intrinsic = OP_CAST_ARRAY_TO_POINTER;
    expression.function_call.params = &original_expression;
    expression.function_call.num_params = 1;
    expression.function_call.dest_type = dest_type;
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
  case up_convert_int_to_float:
    expression.type = EXPR_FUNCTION_CALL;
    expression.function_call.intrinsic = OP_EXTEND_INT_TO_FLOAT;
    expression.function_call.params = &original_expression;
    expression.function_call.num_params = 1;
    expression.function_call.dest_type = dest_type;
    break;
  case pointer_to_void_pointer:
    expression.type = EXPR_FUNCTION_CALL;
    expression.function_call.intrinsic = OP_CAST_POINTER;
    expression.function_call.params = &original_expression;
    expression.function_call.num_params = 1;
    expression.function_call.dest_type = dest_type;
    break;
  default:
    halt();
  }
}

void apply_conversion(const ConversionChain &conversion, Expression &expression, TypeDeclaration *dest_type) {
  if (conversion.num_conversions == 0) return;
  
  for(int i = 0; i < conversion.num_conversions; ++i) {
    Expression *new_expression = expressions_alloc(1);
    *new_expression = expression;

    create_conversion_expression(conversion.chain[i], expression, *new_expression, dest_type);
  }
}

void apply_auto_varargs_conversion(TypeDeclaration *type, Expression &param) {
  if (is_integer(type) && type_size(type) < 32) {
    TypeDeclaration *i32_type = db_lookup_type_by_identifier(to_substring("i32"))->decl;
    ConversionChain chain;
    chain.chain[0] = up_convert_integer;
    chain.num_conversions = 1;
    apply_conversion(chain, param, i32_type);
  }
  if (is_float(type) && type_size(type) < 64) {
    TypeDeclaration *f64_type = db_lookup_type_by_identifier(to_substring("f64"))->decl;
    ConversionChain chain;
    chain.chain[0] = up_convert_float;
    chain.num_conversions = 1;
    apply_conversion(chain, param, f64_type);
  }
}

void apply_conversions(ConversionChain *conversions, TypeDeclaration **call_param_types, Expression *call_params, ParamDefinition *function_params, int num_params, int num_function_params) {
  for(int i = 0; i < num_params; ++i) {

    if (i >= num_function_params || (i == num_function_params-1 && function_params[i].type == PD_VARARGS)) {
      apply_auto_varargs_conversion(call_param_types[i], call_params[i]);
      continue;
    } else if (conversions[i].num_conversions == 0) continue;

    assert(function_params[i].type == PD_VARIABLE);
    TypeDeclaration *dest_type = function_params[i].variable.type;
    apply_conversion(conversions[i], call_params[i], dest_type);
  }
}


FunctionLookupResult db_function_call_lookup(const SubString &identifier, TokenType op, TypeDeclaration **call_param_types, Expression *params, int num_params) {
  FunctionLookupResult error = {LOOKUP_ERROR};
  FunctionMatch best_match = {};
  ConversionChain conversions[MAX_PARAMS] = {};
  int num_function_params;
  
  for(FunctionDefinitionDBEntry &entry : g.db.functions) {
    if (entry.function) {
      if (entry.function->num_params && entry.function->params[entry.function->num_params-1].type == PD_VARARGS) {
        if (substring_cmp(entry.function->identifier, identifier) && entry.function->num_params-1 <= num_params) {
          if (compare_function_param_types(call_param_types, entry.function->params, entry.function->num_params-1, conversions) == true) {
            FunctionLookupResult retval;
            retval.type = LOOKUP_FUNCTION;
            retval.function = entry.function;
            num_function_params = entry.function->num_params;
            function_match_add(best_match, retval, conversions, num_params);
          }
        }
      } else {
        if (substring_cmp(entry.function->identifier, identifier) && entry.function->num_params == num_params) {
          if (compare_function_param_types(call_param_types, entry.function->params, num_params, conversions) == true) {
            FunctionLookupResult retval;
            retval.type = LOOKUP_FUNCTION;
            retval.function = entry.function;
            num_function_params = entry.function->num_params;
            //return retval;
            function_match_add(best_match, retval, conversions, num_params);
          }
        }
      }
    } else {
      assert(entry.extern_function);
      if (entry.extern_function->num_params && entry.extern_function->params[entry.extern_function->num_params-1].type == PD_VARARGS) {
        if (substring_cmp(entry.extern_function->identifier, identifier) && entry.extern_function->num_params-1 <= num_params) {
          if (compare_function_param_types(call_param_types, entry.extern_function->params, entry.extern_function->num_params-1, conversions) == true) {
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
          if (compare_function_param_types(call_param_types, entry.extern_function->params, num_params, conversions) == true) {
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
    int num_function_params;
    if (best_match.result.type == LOOKUP_EXTERN_FUNCTION) {
      function_params = best_match.result.extern_function->params;
      num_function_params = best_match.result.extern_function->num_params;
    } else {
      function_params = best_match.result.function->params;
      num_function_params = best_match.result.function->num_params;
    }

    apply_conversions(best_match.conversions, call_param_types, params, function_params, num_params, num_function_params);
    return best_match.result;
  }

  if (op == TOKEN_NONE) {
    return error;
  }

  //OP_POINTER_ADD,
  //OP_POINTER_DEREFERENCE,
  //OP_ARRAY_DEREFERENCE,

  if (num_params == 1) {
    TypeDeclaration *type0 = call_param_types[0];

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
    TypeDeclaration *type0 = call_param_types[0];
    TypeDeclaration *type1 = call_param_types[1];

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

bool compare_type_declaration(TypeDeclaration *type0, TypeDeclaration *type1) {
  return type0 == type1;
}

PipelineResult compare_type_declaration(TypeDeclaration &decl0, TypeDeclaration &decl1, bool &result) {
  // TODO

  llvm::Type *llvm0;
  PipelineResult result0 = emit_type_declaration(decl0, llvm0);
  if (is_success(result0) == false) return result0;

  llvm::Type *llvm1;
  PipelineResult result1 = emit_type_declaration(decl1, llvm1);
  if (is_success(result1) == false) return result1;

  result = llvm0 == llvm1;
  return PIPELINE_SUCCESS;
}

PipelineResult compare_function_params(ParamDefinition *function_params, ParamDefinition *params, int num_params, bool &types_equal) {
  types_equal = true;

  for(int i = 0; i < num_params; ++i) {
    ParamDefinition &function_param = function_params[i];
    ParamDefinition &param = params[i];

    assert(function_param.type == PD_VARIABLE);
    assert(param.type == PD_VARIABLE);

    PipelineResult result = compare_type_declaration(*param.variable.type, *function_param.variable.type, types_equal);
    if (is_success(result)) return result;

    if (types_equal == false) return PIPELINE_SUCCESS;
  }

  return PIPELINE_SUCCESS;
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


PipelineResult db_lookup_function(const SubString &identifier, ParamDefinition *params, int num_params, FunctionDefinitionDBEntry *&result) {
  for(FunctionDefinitionDBEntry &function : g.db.functions) {
    SubString function_identifier = dbfunction_get_identifier(function);
    if (substring_cmp(function_identifier, identifier)) {
      int function_num_params = dbfunction_get_num_params(function);
      ParamDefinition *function_params = dbfunction_get_params(function);
      if (num_params != function_num_params) continue;

      bool types_equal;
      PipelineResult param_result = compare_function_params(function_params, params, num_params, types_equal);
      if (is_success(param_result) == false) return param_result;

      if (types_equal == false) continue;
      result = &function;
      return PIPELINE_SUCCESS;
    }
  }

  result = NULL;
  return PIPELINE_SUCCESS;
}

VaraibleDefinitionDBEntry *db_lookup_variable(const SubString &identifier) {
  for(auto &varaible : g.db.variables) {
    if (substring_cmp(varaible.identifier, identifier)) {
      return &varaible;
    }
  }
  
  return NULL;
}

PipelineResult make_function_dependancy(SubString &identifier) {
  PipelineResult result;
  result.result = RESULT_DEPENDANCY;
  result.dependancy.type = DEPENDANCY_FUNCTION;
  result.dependancy.identifier = identifier;
  printf("dependancy %s\n", to_cstring(identifier).c_str());
  return result;
}

PipelineResult make_type_dependancy(SubString &identifier) {
  PipelineResult result;
  result.result = RESULT_DEPENDANCY;
  result.dependancy.type = DEPENDANCY_TYPE;
  result.dependancy.identifier = identifier;
  printf("dependancy %s\n", to_cstring(identifier).c_str());
  return result;
}

PipelineResult make_variable_dependancy(SubString &identifier) {
  PipelineResult result;
  result.result = RESULT_DEPENDANCY;
  result.dependancy.type = DEPENDANCY_VARIABLE;
  result.dependancy.identifier = identifier;
  printf("dependancy %s\n", to_cstring(identifier).c_str());
  return result;
}

PipelineResult make_statement_dependancy(SubString &identifier) {
  PipelineResult result;
  result.result = RESULT_DEPENDANCY;
  result.dependancy.type = DEPENDANCY_STATEMENT;
  result.dependancy.identifier = identifier;
  printf("dependancy %s\n", to_cstring(identifier).c_str());
  return result;
}

PipelineResult typecheck_function_definition(FunctionDefinition &function);

PipelineResult typecheck_function_call_expression(FunctionCall &call, TypeDeclaration *&type) {
  if (call.intrinsic != OP_NONE) { 
    type = call.dest_type;
    assert(type);
    return PIPELINE_SUCCESS;
  }

  TypeDeclaration *call_param_types[MAX_PARAMS];

  for(int i = 0; i < call.num_params; ++i)
  {
    Expression &call_param = call.params[i];
    PipelineResult result = typecheck_expression(call_param, call_param_types[i]);

    if (is_success(result) == false) return result;
  }

  // TODO use call_param_types rather than params;
  FunctionLookupResult lookup_result = db_function_call_lookup(call.identifier, call.op, call_param_types, call.params, call.num_params);

  switch(lookup_result.type) {
  case LOOKUP_ERROR:
    //return make_error(UNKNOWN_LOCATION, "Undefined function");
    return make_function_dependancy(call.identifier);
  case LOOKUP_FUNCTION:
    call.function   = lookup_result.function;
    type = call.function->retval_type;
    //if (call.function->retval_type->type == NULL) {
    //  TypecheckResult result = typecheck_type_declaration(*call.function->retval_type);
    //  if (is_success(result) == false) return result;
    //}
    break;
  case LOOKUP_EXTERN_FUNCTION:
    call.extern_function = lookup_result.extern_function;
    type = call.extern_function->retval_type;
    //if (call.extern_function->retval_type->type == NULL) {
    //  TypecheckResult result = typecheck_type_declaration(*call.extern_function->retval_type);
    //  if (is_success(result) == false) return result;
    //}
    break;
  case LOOKUP_INTRINSIC:
    call.intrinsic  = lookup_result.intrinsic;
    switch(lookup_result.intrinsic) {
    case OP_POINTER_DEREFERENCE:
      type = call_param_types[0]->pointer_type.sub_type;
      call.dest_type = type;
      break;
    case OP_ADDRESS_OF:
      type = make_pointer_type(call_param_types[0]);
      call.dest_type = type;
      break;
    case OP_POINTER_ADD:
      type = call_param_types[0];
      call.dest_type = type;
      break;
    case OP_ARRAY_DEREFERENCE:
      type = call_param_types[0]->array_type.sub_type;
      call.dest_type = type;
      break;
    default:
      halt();
    } break;
  default:
    halt();
  }

  return PIPELINE_SUCCESS;
}

bool is_compile_time(TypeDeclaration *) {
  // TODO
  return true;
}

bool is_compile_time_integer(TypeDeclaration *type) {
  return is_compile_time(type) && is_integer(type);
}

PipelineResult typecheck_array_dereference_expression(FunctionCall &expr){
  assert(expr.op == OP_POINTER_DEREFERENCE);
  assert(expr.num_params == 2);

  TypeDeclaration *type_base;
  PipelineResult result_base = typecheck_expression(expr.params[0], type_base);
  if (is_success(result_base) == false) return result_base;
  //expression_get_type(expr.params[0]);

  if (is_array(type_base) == false &&
    is_pointer(type_base) == false) {
    return make_error(UNKNOWN_LOCATION, "Expected array or pointer");
  }

  TypeDeclaration *type_index;
  PipelineResult result_index = typecheck_expression(expr.params[1], type_index);
  if (is_success(result_index) == false) return result_index;
  // = expression_get_type(expr.params[1]);

  if (is_compile_time_integer(type_index) == false) {
    return make_error(UNKNOWN_LOCATION, "Expected compile time integer");
  }

  return PIPELINE_SUCCESS;
}

TypeDeclaration *integer_type_lookup(const char *str) {
  TypeDefinition *definition = db_lookup_type_by_identifier(to_substring(str));
  if (definition == NULL) return NULL;
  assert(definition->decl);
  return definition->decl;
};

PipelineResult char_type_lookup(TypeDeclaration *&decl) {
  decl = integer_type_lookup("char");
  if (decl == NULL) {
    return make_type_dependancy(to_substring("char"));
  }

  return PIPELINE_SUCCESS;
}

PipelineResult calc_string_literal_type(SubString &str, TypeDeclaration *&array_type) {
  TypeDeclaration *char_type;
  PipelineResult result = char_type_lookup(char_type);
  if (is_success(result) == false) return result;
  
  array_type = make_array_type(char_type, str.length + 1);
  return PIPELINE_SUCCESS;
}

PipelineResult typecheck_string_literal(StringLiteral &literal, TypeDeclaration *&type) {
  return calc_string_literal_type(literal.literal, type);
  //return TYPECHECK_SUCCESS;
}


PipelineResult typecheck_float_literal(NumericLiteral &literal, TypeDeclaration *&type) {
  // TODO
  type = integer_type_lookup("f32");
  if (type == NULL) {
    return make_type_dependancy(to_substring("f32"));
  }

  return PIPELINE_SUCCESS;
}

PipelineResult typecheck_integer_literal(NumericLiteral &literal, TypeDeclaration *&type) {
  // TODO
  type = integer_type_lookup("i32");
  if (type == NULL) {
    return make_type_dependancy(to_substring("i32"));
  }

  return PIPELINE_SUCCESS;
}

PipelineResult typecheck_identifier(VariableReference &variable, TypeDeclaration *&type) {
  if (variable.variable == NULL) {
    VaraibleDefinitionDBEntry *entry = db_lookup_variable(variable.identifier);
    if (entry == NULL) {
      return make_variable_dependancy(variable.identifier);
    }

    switch(entry->type) {
    case VT_VARIABLE:
      variable.variable = entry->variable;
      break;
    case VT_STATEMENT_PARAM:
      halt();
      break;
    default:
      halt();
    }
  }

  type = variable.variable->type;
  assert(variable.variable);
  return PIPELINE_SUCCESS;
}

PipelineResult typecheck_field_dereference(FieldDereference &field_deref, TypeDeclaration *&type) {

  TypeDeclaration *type_obj;
  PipelineResult result_object = typecheck_expression(*field_deref.object, type_obj);
  if (is_success(result_object) == false) {
    return result_object;
  }

  // allow field deref via pointer.  ie turn . into ->
  if (is_pointer(type_obj)) {
    type_obj = type_obj->pointer_type.sub_type;
  }

  if (type_obj->kind != TYPE_BASE) {
    return make_error(UNKNOWN_LOCATION, "Expected struct type");
  }

  TypeDefinition *type_def = db_lookup_type_by_identifier(type_obj->base_type.identifier);

  if (type_def->type != TYPE_STRUCT) {
    return make_error(UNKNOWN_LOCATION, "Expected struct type");
  }

  FieldDefinition *field_def = struct_lookup_field(*type_def, field_deref.field_identifier, field_deref.field_idx);
  if (field_def == NULL) {
    return make_error(UNKNOWN_LOCATION, "Unknown field");
  }


  type = field_def->type;
  //field_deref.type = type_obj->struct_def.new_type_definition->fields[index].type->type;
  //assert(field_deref.type);

  return PIPELINE_SUCCESS;
}

PipelineResult typecheck_expression(Expression &expr, TypeDeclaration *&type) {
  switch(expr.type) {
  //case EXPR_OPERATOR:
  //  return typecheck_integer_literal(expr);
  case EXPR_FUNCTION_CALL:
    return typecheck_function_call_expression(expr.function_call, type);
  case EXPR_NUMERIC_LITERAL:
    return typecheck_integer_literal(expr.numeric_literal, type);
  case EXPR_FLOAT_LITERAL:
    return typecheck_float_literal(expr.numeric_literal, type);
  case EXPR_STRING_LITERAL:
    return typecheck_string_literal(expr.string_literal, type);
  case EXPR_VARIABLE:
    return typecheck_identifier(expr.variable, type);
  case EXPR_FIELD_DEREFERENCE:
    return typecheck_field_dereference(expr.field_dereference, type);
  default:
    halt();
  }

  return make_error(UNKNOWN_LOCATION, "Unknown expression");
}

enum {
  FIXED_ARGS = false,
  VAR_ARGS = true,
};

bool is_structure(TypeDeclaration *type) {
  if (type->kind != TYPE_BASE) return false;
  TypeDefinition *type_def = db_lookup_type_by_identifier(type->base_type.identifier);
  if (type_def->type == TYPE_STRUCT) return true;
  return false;
}

void *ptr_add(void *ptr, int offsetBytes) {
  uint8_t *ptr8 = (uint8_t *)ptr;
  ptr8 += offsetBytes;
  return ptr8;
}

int field_offset(TypeDefinition *type_struct, int i) {
  llvm::StructType* llvm_struct_type = llvm::dyn_cast<llvm::StructType>(type_struct->llvm_type);
  const llvm::StructLayout *layout = g.llvm.module->getDataLayout()->getStructLayout(llvm_struct_type);
  return (int)(layout->getElementOffsetInBits(i) / 8);
}

TypeDeclaration *field_type(TypeDefinition *type_struct, int i) {
  return type_struct->struct_definition->fields[i].type;
}

llvm::Constant *marshall_retval_into_constant(TypeDeclaration *type, void *retval);


llvm::Constant *marshall_retval_into_base_constant(TypeDeclaration *type, void *retval) {
  if (is_integer(type)) {
    int size = type_size(type);
    int words = size+7 / 8;
    llvm::APInt value(size, words, (uint64_t*) retval);
    llvm::Type *llvm_type;
    PipelineResult result = emit_type_declaration(*type, llvm_type);
    assert(is_success(result));
    return llvm::ConstantInt::get(llvm_type, value);
  } else if (is_float(type)) {
    int size = type_size(type);
    //int words = size+7 / 8;
    llvm::Type *llvm_type;
    PipelineResult result = emit_type_declaration(*type, llvm_type);
    assert(is_success(result));
    if (size == 32) {
      float f = *(float *)retval;
      return llvm::ConstantFP::get(llvm_type, (double)f);
    } else if (size == 64) {
      return llvm::ConstantFP::get(llvm_type, *(double *)retval);
    } else {
      halt();
    }

    //llvm::APInt value(size, words, (uint64_t*) retval);
    //llvm::Type *llvm_type;
    //PipelineResult result = emit_type_declaration(*type, llvm_type);
    //assert(is_success(result));
  } else if (is_structure(type)) {
    llvm::Type *llvm_type;
    PipelineResult result = emit_type_declaration(*type, llvm_type);
    assert(is_success(result));

    TypeDefinition *type_def = db_lookup_type_by_identifier(type->base_type.identifier);
    assert(type_def->type == TYPE_STRUCT);

    const int MAX_FIELDS = 256;
    assert(type_def->struct_definition->num_fields < MAX_FIELDS);
    llvm::Constant *field_constants[MAX_FIELDS];

    for(int i = 0; i < type_def->struct_definition->num_fields; ++i) {
      int child_offset = field_offset(type_def, i);
      TypeDeclaration *type_field = field_type(type_def, i);
      void *child_data = ptr_add(retval, child_offset);
      field_constants[i] = marshall_retval_into_constant(type_field, child_data);
    }

    llvm::ArrayRef<llvm::Constant *> elts(field_constants, type_def->struct_definition->num_fields);
    llvm::StructType *struct_type = llvm::dyn_cast<llvm::StructType>(type_def->llvm_type);
    llvm::Constant *constant = llvm::ConstantStruct::get(struct_type, elts);
    return constant;
  } else {
    halt();
  }

  return NULL;
}

llvm::Constant *marshall_retval_into_struct_constant(TypeDeclaration *type, void *retval) {
  // todo
  halt();
  return NULL;
}

llvm::Constant *marshall_retval_into_array_constant(TypeDeclaration *type, void *retval) {
  // todo
  halt();
  return NULL;
}

llvm::Constant *marshall_retval_into_pointer_constant(TypeDeclaration *type, void *retval) {
  // todo
  halt();
  return NULL;
}

llvm::Constant *marshall_retval_into_constant(TypeDeclaration *type, void *retval) {
  switch(type->kind) {
  case TYPE_BASE:
    return marshall_retval_into_base_constant(type, retval);
  case TYPE_ARRAY:
    return marshall_retval_into_array_constant(type, retval);
  case TYPE_POINTER:
    return marshall_retval_into_pointer_constant(type, retval);
  case TYPE_FUNCTION:
    halt();
  default:
    halt();
  }

  return NULL;
}

enum CompilationStage {
  STAGE_DECLARATION,
  STAGE_TYPECHECK,
  STAGE_EMIT,
  STAGE_COMPLETE,
};

struct CompilationUnit {
  ProgramStatement *statement;
  CompilationStage stage;
  CompilationDependancy dependancy;
};

struct CompilationSet {
  std::vector<CompilationUnit> units;
};

//typedef PipelineResult PipelineResult;

// new function
// emit all variables referenced by expression
// emit all variables referenced by references...
// emit retval expression
// eval function


PipelineResult emit_rvalue_expression(Expression &expression, llvm::Value *&result);
PipelineResult emit_local_variable_definition(VariableDefinition &variable);
PipelineResult emit_variable_initalization(VariableDefinition &variable);

PipelineResult emit_dependant_varaibles(Expression &expression) {
  switch(expression.type) {
  case EXPR_FUNCTION_CALL: {
    for(int i = 0; i < expression.function_call.num_params; ++i) {
      PipelineResult result = emit_dependant_varaibles(expression.function_call.params[i]);
      if (is_success(result) == false) return result;
    } 
    return PIPELINE_SUCCESS;
  } 
  case EXPR_VARIABLE: {
    PipelineResult result = emit_dependant_varaibles(*expression.variable.variable->initial_value);
    if (is_success(result) == false) return result;
    //return emit_local_variable_definition(*expression.variable.variable);
    return emit_variable_initalization(*expression.variable.variable);
  } 
  case EXPR_FLOAT_LITERAL:
  case EXPR_NUMERIC_LITERAL:
  case EXPR_FIELD_DEREFERENCE:
  case EXPR_STRING_LITERAL:
  case EXPR_DEFAULT_VALUE:
    return PIPELINE_SUCCESS;
  default:
    halt();
  }

  return make_error(UNKNOWN_LOCATION, "unknown dependent variable statement");
}

#include "llvm/ExecutionEngine/GenericValue.h"

static llvm::ArrayRef<llvm::Value *> to_array_ref(llvm::Value **values, int num_values) {
  return llvm::ArrayRef<llvm::Value *>(values, num_values);
}

static llvm::ArrayRef<llvm::Type *> to_array_ref(llvm::Type **types, int num_types) {
  return llvm::ArrayRef<llvm::Type *>(types, num_types);
}

void CreateStore(llvm::Value *val, llvm::Value *address) //llvm.builder->CreateStore(retval, lvalue);
{
  g.llvm.builder->CreateStore(val, address);
}

PipelineResult emit_compile_time_expression(Expression &expression, llvm::Constant *&constant) {
  auto ip = g.llvm.builder->saveIP();

  // TODO
  //DBAlternateScope scope; 

  TypeDeclaration *type;
  PipelineResult result_type = typecheck_expression(expression, type);
  if (is_success(result_type) == false) return result_type;

  llvm::Type *llvm_type;
  PipelineResult result_llvm_type = emit_type_declaration(*type, llvm_type);
  if (is_success(result_llvm_type) == false) return result_llvm_type;

  //g.llvm.module->dump();

  if (is_structure(type)) {
    llvm::Type *param_types[MAX_PARAMS];
    param_types[0] = llvm::PointerType::get(llvm_type, 0);

    llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, 1);
    llvm::FunctionType *llvm_function_type = llvm::FunctionType::get(llvm::Type::getVoidTy(*g.llvm.context), args, FIXED_ARGS);
    assert(llvm_function_type);
    llvm::Function *llvm_function = llvm::Function::Create(llvm_function_type, llvm::Function::InternalLinkage, "constant_struct_expression", g.llvm.module);

    llvm_function->addAttribute(1, llvm::Attribute::StructRet);
    llvm_function->addAttribute(1, llvm::Attribute::NoAlias);

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(*g.llvm.context, "", llvm_function);
    g.llvm.builder->SetInsertPoint(entry);

    // TODO
    //PipelineResult result_varaibles = emit_dependant_varaibles(expression);
    //if (is_success(result_varaibles) == false) {
    //  llvm_function->removeFromParent();
    //  delete llvm_function;
    //  return result_varaibles;
    //}

    llvm::Value *retval_expr;
    PipelineResult result = emit_rvalue_expression(expression, retval_expr);
    if (is_success(result) == false) {
      llvm_function->removeFromParent();
      delete llvm_function;
      return result;
    }

    //g.llvm.module->dump();

    llvm::Function::arg_iterator arg_iterator = llvm_function->arg_begin();
    llvm::Value *retval_param = arg_iterator;
    CreateStore(retval_expr, retval_param);
    g.llvm.builder->CreateRetVoid();

    //g.llvm.module->dump();
    //llvm::StringRef name = to_string_ref(function.identifier);
    //function.llvm_function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, g.llvm.module);
    //assert(function.llvm_function);

    void *void_function_pointer = g.llvm.engine->getPointerToFunction(llvm_function);
    typedef void (*FnPointer)(void *);
    FnPointer function_pointer = (FnPointer)void_function_pointer;

    int size = type_size(type);
    void *retval = malloc(size);
    function_pointer(retval);
    constant = marshall_retval_into_constant(type, retval);
    free(retval);

    llvm_function->removeFromParent();
    delete llvm_function;
  } else {
    llvm::FunctionType *llvm_function_type = llvm::FunctionType::get(llvm_type, FIXED_ARGS);
    assert(llvm_function_type);
    llvm::Function *llvm_function = llvm::Function::Create(llvm_function_type, llvm::Function::InternalLinkage, "constant_expressionmars", g.llvm.module);
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(*g.llvm.context, "", llvm_function);
    g.llvm.builder->SetInsertPoint(entry);

    // TODO
    //PipelineResult result_varaibles = emit_dependant_varaibles(expression);
    //if (is_success(result_varaibles) == false) {
    //  llvm_function->removeFromParent();
    //  delete llvm_function;
    //  return result_varaibles;
    //}

    llvm::Value *retval_expr;
    PipelineResult result = emit_rvalue_expression(expression, retval_expr);
    if (is_success(result) == false) {
      llvm_function->removeFromParent();
      delete llvm_function;
      return result;
    }

    g.llvm.builder->CreateRet(retval_expr);



    void *void_function_pointer = g.llvm.engine->getPointerToFunction(llvm_function);

    typedef uint64_t (*FnPointer)();
    FnPointer function_pointer = (FnPointer)void_function_pointer;
    uint64_t retval = function_pointer();
    constant = marshall_retval_into_constant(type, &retval);
    //constant = llvm::Constant::getNullValue(llvm_type);

    llvm_function->removeFromParent();
    delete llvm_function;
  }

  g.llvm.builder->restoreIP(ip);


  return EMIT_SUCCESS;
}

bool is_auto_type(TypeDeclaration *type) {
  return false;
}

PipelineResult typecheck_assignment(TypeDeclaration *type, Expression &expression);

PipelineResult db_try_add_variable(VaraibleDefinitionDBEntry &new_variable, const SubString &identifier) {
  new_variable.identifier = identifier;

  VaraibleDefinitionDBEntry *entry = db_lookup_variable(identifier);
  if (entry) {
    return make_error(UNKNOWN_LOCATION, "Variable redefinition");
  }

  g.db.variables.push_back(new_variable);

  return PIPELINE_SUCCESS;
}

PipelineResult db_add_variable_declaration(VariableDefinition &variable) {
  VaraibleDefinitionDBEntry new_entry = {};
  new_entry.scope = g.db.scope;
  new_entry.variable = &variable;

  return db_try_add_variable(new_entry, variable.identifier);
}

PipelineResult typecheck_variable_definition(VariableDefinition &variable) {
  //assert(variable.type);

  //if (add_variable) {
  
  //}

  //TypecheckResult result_type = typecheck_type_declaration(*variable.type);
  //if (is_success(result_type) == false) return result_type;

  if (variable.initial_value) {
    if (variable.initial_value->type == EXPR_DEFAULT_VALUE) {
      if (is_auto_type(variable.type)) {
        return make_error(UNKNOWN_LOCATION, "Unintialized auto deduced variable");
      } else {
        // ok
        //PipelineResult result_type = typecheck_default_assignment(*variable.type);
        //if (is_success(result_type) == false) return result_type;
      }
    }
    else {
      PipelineResult result = typecheck_assignment(variable.type, *variable.initial_value);
      if (is_success(result) == false) return result;
    }
  }

  if (g.db.scope != 0) {
    PipelineResult result_add = db_add_variable_declaration(variable);
    if (is_success(result_add) == false) {
      return result_add;
    }
  }

  return PIPELINE_SUCCESS;
}

FieldDefinition *fields_alloc(int num_params) {
  return (FieldDefinition *)malloc(sizeof(FieldDefinition) * num_params);
}

PipelineResult typecheck_struct_definition(StructDefinition &struct_def) {

  for(int i = 0; i < struct_def.num_fields; ++i) {
    VariableDefinition &field_def = struct_def.fields[i];
    PipelineResult result = typecheck_variable_definition(field_def);
    if (is_success(result) == false) return result;
  }

  return PIPELINE_SUCCESS;
}

PipelineResult typecheck_function_statement(TypeDeclaration *retval_type, FunctionStatement &statement);

// rules for type conversion:
// int small can go to int big
// float small can go to float big
// array[size] can go to pointer
// any pointer can go to void *
// null can go to any pointer

PipelineResult typecheck_assignment(TypeDeclaration *type, Expression &expression) {
  //TypeDeclaration *expression_type = expression_get_type(expression);

  TypeDeclaration *expression_type;
  PipelineResult result = typecheck_expression(expression, expression_type);
  if (is_success(result) == false) return result;

  if (expression_type != type) {

    ConversionChain conversions = {};
    if (can_convert(expression_type, type, conversions)) {
      apply_conversion(conversions, expression, type);
      return PIPELINE_SUCCESS;
    }

    return make_error(UNKNOWN_LOCATION, "Type mismatch");
  }

  return PIPELINE_SUCCESS;
}

PipelineResult typecheck_return_statement(TypeDeclaration *retval_type, ReturnStatement &statement) {
  //TypecheckResult result_rhs = typecheck_expression(*statement.retval);
  //if (is_success(result_rhs) == false) return result_rhs;

  return typecheck_assignment(retval_type, *statement.retval);

  //TypecheckResult result = typecheck_assigment(statement->location, retval_type, expression_get_type(*fs.return_statement.retval));
  //return result;
}

PipelineResult typecheck_assignment_statement(AssignmentStatement &statement) {
  TypeDeclaration *lhs_type;
  PipelineResult result_lhs = typecheck_expression(*statement.lhs, lhs_type);
  if (is_success(result_lhs) == false) return result_lhs;

  //TypecheckResult result_rhs = typecheck_expression(*statement.lhs);
  //if (is_success(result_rhs) == false) return result_rhs;
  //= expression_get_type(*statement.lhs);
  
  return typecheck_assignment(lhs_type, *statement.rhs);

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

StatementParam *statement_params_alloc(int num_params) {
  if (num_params == 0) return NULL;
  return (StatementParam *)calloc(sizeof(StatementParam) * num_params, 1);
}

FunctionStatement *function_body_alloc(int num_params) {
  return (FunctionStatement *)calloc(sizeof(FunctionStatement) * num_params, 1);
}

PipelineResult typecheck_function_params(ParamDefinition *params, int num_params) {
  for(int i = 0; i < num_params; ++i) {
    if (params[i].type == PD_VARARGS) {
      assert(i == num_params-1);
      return PIPELINE_SUCCESS;
    }

    PipelineResult result = typecheck_variable_definition(params[i].variable);
    if (is_success(result) == false) return result;
  }

  return PIPELINE_SUCCESS;
}

StatementDefinition *db_lookup_statement(const SubString &identifier) {
  for(auto &statement : g.db.statements) {
    if (substring_cmp(statement.statement->identifier, identifier)) {
      return statement.statement;
    }
  }
  
  return NULL;
}

PipelineResult typecheck_statement_call(StatementCall  &call) {
  StatementDefinition *def = db_lookup_statement(call.identifier);
  if (def == NULL) return make_statement_dependancy(call.identifier);

  // TODO, check params

  return PIPELINE_SUCCESS;
}

//PipelineResult emit_statement_statement(FunctionStatement &body_statement, StatementParam *params, int num_params) {
//  switch(body_statement.type) {
//    case FS_LLVM:
//      emit_inline_llvm();
//      break;
//    default:
//      halt();
//  }
//}

PipelineResult emit_statement_call_param(StatementCallParam &call_param, StatementParam &statement_param) {
  VaraibleDefinitionDBEntry entry = {};
  entry.scope = g.db.scope;
  entry.type = VT_STATEMENT_PARAM;
  entry.statement_param = &call_param;
  return db_try_add_variable(entry, statement_param.identifier);
}

class DBScope {
public:
  DBScope() {
    db_push_scope();
  }

  ~DBScope() {
    db_pop_scope();
  }
};

PipelineResult emit_function_statement(llvm::Function *function, FunctionStatement &statement, llvm::Value *retval_param);


PipelineResult emit_statement_call(llvm::Function *function, StatementCall &call, llvm::Value *retval_param) {
  StatementDefinition *def = db_lookup_statement(call.identifier);
  assert(def);

  {
    DBScope scope;

    for(int i = 0; i < call.num_params; ++i) {
      PipelineResult result = emit_statement_call_param(call.params[i], def->statement_params[i]);
      if (is_success(result) == false) return result;
    }

    for(int i = 0; i < def->num_body_statements; ++i) {
      FunctionStatement &body_statement = def->body[i];
      PipelineResult result = emit_function_statement(function, body_statement, retval_param);
      if (is_success(result) == false) return result;
    }
  }


  return PIPELINE_SUCCESS;
  

  //halt();
  //return make_error(UNKNOWN_LOCATION, "todo");
}

PipelineResult db_add_statement_declaration(StatementDefinition &def) {
  StatementDefinitionDBEntry new_entry = {};
  new_entry.scope = g.db.scope;
  new_entry.statement = &def;

  StatementDefinition *existing_variable = db_lookup_statement(def.identifier);
  if (existing_variable) {
    return make_error(UNKNOWN_LOCATION, "Statement redefinition");
  }

  g.db.statements.push_back(new_entry);

  return PIPELINE_SUCCESS;
}

PipelineResult emit_statement_declaration(StatementDefinition &statement) {
  return db_add_statement_declaration(statement);
  //halt();
  //return make_error(UNKNOWN_LOCATION, "todo");
}

PipelineResult emit_statement_definition(StatementDefinition &statement) {
  //halt();
  //return make_error(UNKNOWN_LOCATION, "todo");
  return PIPELINE_SUCCESS;
}

PipelineResult typecheck_statement_param(StatementParam &param) {
  return PIPELINE_SUCCESS;
}

PipelineResult typecheck_statement_statement(FunctionStatement &statement) {
  return typecheck_function_statement(NULL, statement);
}

PipelineResult typecheck_statement_definition(StatementDefinition &statement) {

  for(int i = 0; i < statement.num_statement_params; ++i) {
    PipelineResult result = typecheck_statement_param(statement.statement_params[i]);
    if (is_success(result) == false) return result;
  }

  for(int i = 0; i < statement.num_statement_yield_params; ++i) {
    //typecheck_yield_param();
    // TODO
  }

  for(int i = 0; i < statement.num_body_statements; ++i) {
    PipelineResult result = typecheck_statement_statement(statement.body[i]);
    if (is_success(result) == false) return result;
  }

  return PIPELINE_SUCCESS;
}

PipelineResult typecheck_function_statement(TypeDeclaration *retval_type, FunctionStatement &statement) {
  switch(statement.type) {
    //case TOKEN_IF_STATEMENT:
    //  return typecheck_if_statement(statement_token, retval_type);
    //case TOKEN_FOR_STATEMENT:
    //  return typecheck_for_statement(statement, result_retval);
    //case TOKEN_SWITCH_STATEMENT:
    //  return typecheck_switch_statement(statement, result_retval);
  case FS_STATEMENT_CALL:
    return typecheck_statement_call(statement.statement_call);
  case FS_RETURN:
    return typecheck_return_statement(retval_type, statement.return_statement);
  case FS_EXPRESSION: {
    TypeDeclaration *unused;
    return typecheck_expression(*statement.expression, unused);
  }
  case FS_ASSIGNMENT:
    return typecheck_assignment_statement(statement.assignment);
  case FS_VARIABLE:
    return typecheck_variable_definition(statement.varaible);
  case FS_LLVM:
    return PIPELINE_SUCCESS;
  default:
    halt();
  }

  //return make_error(statement_token->location, "Unknown statement.");

  return PIPELINE_SUCCESS;
}

PipelineResult typecheck_function_body(TypeDeclaration *retval_type, FunctionStatement *body, int num_statements) {

  bool return_statement_verified = false;
  int i = 0;
  for(int i = 0; i < num_statements; ++i) {
    FunctionStatement &statement = body[i];
    PipelineResult result = typecheck_function_statement(retval_type, statement);
    if (is_success(result) == false) return result;

    if (statement.type == FS_RETURN) {
      // todo handle multiple returns, "all paths much return"
      return_statement_verified = true; 
    }
  }

  if (return_statement_verified == false && !is_void(retval_type)) {
    return make_error(UNKNOWN_LOCATION, "Missing return statement");
  }

  return PIPELINE_SUCCESS;
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

PipelineResult db_try_add_function(FunctionDefinitionDBEntry &new_function) {
  SubString identifier    = dbfunction_get_identifier(new_function);
  int num_params          = dbfunction_get_num_params(new_function);
  ParamDefinition *params = dbfunction_get_params(new_function);

  assert(identifier.length > 0);

  FunctionDefinitionDBEntry *existing_fn;
  PipelineResult result = db_lookup_function(identifier, params, num_params, existing_fn);

  if (is_success(result) == false) {
    return result;
  }

  if (existing_fn) {
    return make_error(UNKNOWN_LOCATION, "Function redefinition");
  }

  g.db.functions.push_back(new_function);
  return PIPELINE_SUCCESS;
}

PipelineResult db_add_function_declaration(FunctionDefinition &function) {
  FunctionDefinitionDBEntry new_entry = {};
  new_entry.scope = g.db.scope;
  new_entry.function = &function;
  return db_try_add_function(new_entry);
}

PipelineResult typecheck_function_definition(FunctionDefinition &function) {
  //TypecheckResult result_retval = typecheck_type_declaration(*function.retval_type);
  //if (is_success(result_retval) == false) return result_retval;

  {
    DBScope scope;
    PipelineResult result_params = typecheck_function_params(function.params, function.num_params);
    if (is_success(result_params) == false) return result_params;

    PipelineResult result_body = typecheck_function_body(function.retval_type, function.body, function.num_statements);
    if (is_success(result_body) == false) return result_body;
  }


  //if (g.db.scope != 0) {
    //DeclarationResult result = db_add_function_definition(program_statement);
  PipelineResult result = db_add_function_declaration(function);
  if (is_success(result) == false) return result;
  //}

  return PIPELINE_SUCCESS;
}

PipelineResult db_add_external_function_declaration(ExternFunctionDeclaration &extern_function) {
  FunctionDefinitionDBEntry new_entry = {};
  new_entry.scope = g.db.scope;
  new_entry.extern_function = &extern_function;

  PipelineResult result = db_try_add_function(new_entry);
  if (is_success(result) == false) return result;

  return PIPELINE_SUCCESS;
}

ProgramStatement *program_statment_alloc(int num_statements) {
  return (ProgramStatement *)calloc(sizeof(ProgramStatement) * num_statements, 1);
}

PipelineResult typecheck_extern_function_declaration(ExternFunctionDeclaration &function) {
  //TypecheckResult result_retval = typecheck_type_declaration(*function.retval_type);
  //if (is_success(result_retval) == false) return result_retval;

  //db_push_scope();

  {
    DBScope scope;
    PipelineResult result_params = typecheck_function_params(function.params, function.num_params);
    if (is_success(result_params) == false) return result_params;

  //TypecheckResult result_body = typecheck_function_body(function.retval_type->type, function.body, function.num_statements);
  //if (is_success(result_body) == false) return result_body;
  }

  //if (g.db.scope != 0) {
    //DeclarationResult result = db_add_function_definition(program_statement);
    PipelineResult result = db_add_external_function_declaration(function);
    if (is_success(result) == false) return result;
  //}

  return PIPELINE_SUCCESS;
}

//PipelineResult typecheck_program(Program &program) {
//  for(int i = 0; i < program.num_statements; ++i) {
//    ProgramStatement &cur_statement = program.statement[i];
//
//    switch(cur_statement.type) {
//    case PS_EXTERN_FUNCTION: {
//      PipelineResult result = typecheck_extern_function_declaration(cur_statement.extern_function);
//      if (is_success(result) == false) return result;
//    }  break;
//    //case TOKEN_TYPEDEF_DEFINITION:
//    case PS_LLVM_TYPE: {
//      //TypecheckResult result = typecheck_variable_definition(program_statement);
//      //if (is_success(result) == false) return result;
//    }  break;
//    case PS_VARIABLE: {
//      PipelineResult result = typecheck_variable_definition(cur_statement.varaible);
//      if (is_success(result) == false) return result;
//    }  break;
//    case PS_STRUCT: {
//      PipelineResult result = typecheck_struct_definition(cur_statement.struct_def);
//      if (is_success(result) == false) return result;
//    } break;
//    //case TOKEN_ENUM_DEFINITION:
//    //  typecheck_enum_definition(program_statement);
//    //  break;
//    case PS_FUNCTION: {
//      PipelineResult result = typecheck_function_definition(cur_statement.function);
//      if (is_success(result) == false) return result;
//    } break;
//    case PS_STATEMENT: {
//      PipelineResult result = typecheck_statment_definition(cur_statement.function);
//      if (is_success(result) == false) return result;
//    } break;
//
//    case PS_IMPORT:
//      break;
//    default:
//      halt();
//    }
//  }
//
//  return PIPELINE_SUCCESS;
//}

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

//LLVMTypeResult make_success(llvm::Type *type) {
//  LLVMTypeResult retval;
//  retval.result = RESULT_SUCCESS;
//  retval.type = type;
//  return retval;
//}
//
//LLVMTypeResult make_llvmtype_error(const FileLocation &location, const char *fmt, ...){
//  LLVMTypeResult retval;
//  retval.result = RESULT_ERROR;
//  retval.error.location = location;
//  FORMAT_ERROR_STRING(retval.error.error_string, fmt);
//  return retval;
//}

bool is_numeric(const SubString &digits) {
  for(int i = 0; i < digits.length; ++i) {
    if (isDigit(digits.start[i]) == false) return false;
  }

  return true;
}

PipelineResult emit_llvm_type_definition(LLVMTypeDefinition &llvm_def) {
  SubString trimmed_content = substring_trim(llvm_def.raw_llvm);

  TypeDefinition *type_def = db_lookup_type_by_identifier(llvm_def.identifier);
  assert(type_def->type == TYPE_LLVM);
  llvm::Type *&type = type_def->llvm_type;
  assert(type == NULL);

  assert(trimmed_content.length > 0);
  assert(trimmed_content.start != NULL);

  switch(trimmed_content.start[0]) {
  case 'u':
  case 'i': {
    SubString digits = trimmed_content;
    digits.start++;
    digits.length--;

    if (is_numeric(digits) == false) {
      return make_error(UNKNOWN_LOCATION, ("Invalid integer size '" + to_cstring(digits) + "'").c_str()); 
    }

    uint32_t size = (uint32_t)substring_to_uint64(digits);
    type = llvm::IntegerType::get(*g.llvm.context, size);
    //return TYPECHECK_SUCCESS;
  } break;
  default:
    if (substring_cmp(trimmed_content, "void")) {
      type = llvm::Type::getVoidTy(*g.llvm.context);
      break; // return TYPECHECK_SUCCESS;
    } else if (substring_cmp(trimmed_content, "half")) {
      type = llvm::Type::getHalfTy(*g.llvm.context);
      break; // return TYPECHECK_SUCCESS;
    } else if (substring_cmp(trimmed_content, "float")) {
      type = llvm::Type::getFloatTy(*g.llvm.context);
      break; // return TYPECHECK_SUCCESS;
    } else if (substring_cmp(trimmed_content, "double")) {
      type = llvm::Type::getDoubleTy(*g.llvm.context);
      break; // return TYPECHECK_SUCCESS;
    } else if (substring_cmp(trimmed_content, "fp128")) {
      type = llvm::Type::getFP128Ty(*g.llvm.context);
      break; // return TYPECHECK_SUCCESS;
    } else {
      return make_error(UNKNOWN_LOCATION, "Unknown llvm type '%s'", to_cstring(trimmed_content).c_str());
    }
    break;
  }

  printf("emit type: %s\n", to_cstring(llvm_def.identifier).c_str());

  assert(type != NULL);
  return PIPELINE_SUCCESS;
  //return make_llvmtype_error(type_definition->location, "Uknown");
}

const int IS_PACKED = true;


PipelineResult emit_struct_type_definition(StructDefinition &struct_def) {
  TypeDefinition *type_def = db_lookup_type_by_identifier(struct_def.identifier);
  assert(type_def->type == TYPE_STRUCT);
  assert(type_def->llvm_type == NULL);

  llvm::Type *fields[MAX_PARAMS];
  for(int i = 0; i < struct_def.num_fields; ++i) {
    TypeDeclaration *type = struct_def.fields[i].type;
    llvm::Type *field_type;
    PipelineResult result = emit_type_declaration(*type, field_type);
    if (is_success(result) == false) return result;
    fields[i] = field_type;
  }

  llvm::StringRef name = to_string_ref(struct_def.identifier);
  llvm::StructType *type = llvm::StructType::create(*g.llvm.context, name);
  llvm::ArrayRef<llvm::Type *> ref_fields = to_array_ref(fields, struct_def.num_fields);
  type->setBody(ref_fields, IS_PACKED); 
  type_def->llvm_type = type;
  assert(type_def->llvm_type != NULL);

  return PIPELINE_SUCCESS;
}

//bool is_success(const LLVMTypeResult &result) {
//  return (result.result != RESULT_ERROR);
//}

PipelineResult emit_pointer_type_declaration(PointerTypeDeclaration &declaration, llvm::Type *&type) {
  llvm::Type *base_llvm_type;
  PipelineResult result = emit_type_declaration(*declaration.sub_type, base_llvm_type);
  if (is_success(result) == false) return result;

  type = llvm::PointerType::get(base_llvm_type, 0);
  return PIPELINE_SUCCESS;
}

PipelineResult emit_array_type_declaration(ArrayTypeDeclaration &array_type, llvm::Type *&type) {
  //assert(array_type->kind == TYPE_ARRAY);

  llvm::Type *base_type;
  PipelineResult result = emit_type_declaration(*array_type.sub_type, base_type);
  if (is_success(result) == false) return result;


  uint64_t uval;

  if (array_type.count_expression) {

    if (array_type.count_expression->type == EXPR_NUMERIC_LITERAL) {
      //llvm::ConstantInt *constant = array_type.count_expression->numeric_literal.literal
      const llvm::APInt value(array_type.count_expression->numeric_literal.literal.length * 4, to_string_ref(array_type.count_expression->numeric_literal.literal), 10);

      if (value.getActiveBits() > 64) {
        return make_error(UNKNOWN_LOCATION, "Array count exceeds 2^64");
      }

      uval = value.getZExtValue();
    } else {
      llvm::Constant *constant;
      PipelineResult index_result = emit_compile_time_expression(*array_type.count_expression, constant);
      if (is_success(index_result) == false) return make_error(UNKNOWN_LOCATION, "TODO");

      llvm::ConstantInt *constant_int = llvm::dyn_cast<llvm::ConstantInt>(constant);
      if (constant_int == NULL) {
        return make_error(UNKNOWN_LOCATION, "expected integer");
      }
      const llvm::APInt &value = constant_int->getValue();

      if (value.getActiveBits() > 64) {
        return make_error(UNKNOWN_LOCATION, "Array count exceeds 2^64");
      }

      uval = value.getZExtValue();
    }

  } else {
    uval = array_type.count;
  }

  type = llvm::ArrayType::get(base_type, uval);
  return PIPELINE_SUCCESS;
}

PipelineResult get_function_param_llvm_types(TypeDeclaration *params, llvm::Type **types, int num_params) {
  for(int i = 0; i < num_params; ++i) {
    PipelineResult result = emit_type_declaration(params[i], types[i]);
    if (is_success(result) == false) return result;
    assert(types[i]);
  }

  return PIPELINE_SUCCESS;
}

PipelineResult get_function_param_llvm_types(ParamDefinition *params, llvm::Type **types, int num_params) {
  for(int i = 0; i < num_params; ++i) {
    assert(params[i].type == PD_VARIABLE);
    PipelineResult result = emit_type_declaration(*params[i].variable.type, types[i]);
    if (is_success(result) == false) return result;
    assert(types[i]);
  }

  return PIPELINE_SUCCESS;
}

PipelineResult emit_function_type_declaration(FunctionTypeDeclaration &function_type, llvm::Type *&type) {
  //assert(array_type->kind == TYPE_ARRAY);

  llvm::Type *retval_type;
  PipelineResult result_retval = emit_type_declaration(*function_type.retval, retval_type);
  if (is_success(result_retval) == false) return result_retval;

  llvm::Type* param_types[MAX_PARAMS];
  assert(function_type.num_params <= MAX_PARAMS);
  PipelineResult result_params = get_function_param_llvm_types(function_type.params, param_types, function_type.num_params);
  llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function_type.num_params);

  type = llvm::FunctionType::get(retval_type, args, FIXED_ARGS);
  return PIPELINE_SUCCESS;
}

PipelineResult emit_type_declaration(TypeDeclaration &type, llvm::Type *&llvm_type) {
  //assert(type->llvm_type == NULL);
  //if (type->llvm_type != NULL) {
  //  return type->llvm_type;
  //}

  switch(type.kind) {
  case TYPE_BASE: {
    //TypecheckResult result = db_lookup_type_definition(); TODO
    //LLVMTypeResult type_result = llvm_emit_base_type(type);
    //result = type_result.type;
    TypeDefinition *definition = db_lookup_type_by_identifier(type.base_type.identifier);
    if (definition == NULL) {
      return make_type_dependancy(type.base_type.identifier);
    } else {
      llvm_type = definition->llvm_type;
      return PIPELINE_SUCCESS;
    }
  } break;
  case TYPE_POINTER:
    return emit_pointer_type_declaration(type.pointer_type, llvm_type);
  case TYPE_ARRAY:
    return emit_array_type_declaration(type.array_type, llvm_type);
  case TYPE_FUNCTION:
    return emit_function_type_declaration(type.function_type, llvm_type);
  default:
    halt();
  }

  return make_error(UNKNOWN_LOCATION, "unknown type declaration");
  //Token *type_definition = type->type_definition;
  //llvm::Type *llvm_type = llvm_emit_type_defintiion(type_definition);
  //if (is_success(result)) {
  //type->llvm_type = result;
  //}
  //return result;
}

PipelineResult CreateAlloca(const SubString &identifier, TypeDeclaration &type, llvm::Value *&value) {
  llvm::StringRef name = to_string_ref(identifier);
  llvm::Type *llvm_type;
  PipelineResult result = emit_type_declaration(type, llvm_type);
  if (is_success(result) == false) return result;
  value = g.llvm.builder->CreateAlloca(llvm_type, 0, name);
  return PIPELINE_SUCCESS;
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

PipelineResult emit_lvalue_function_call(FunctionCall &call, llvm::Value *&value) {
  halt();
  return make_emit_error("lvalue function call - cant do until references");
}

PipelineResult emit_lvalue_expression(Expression &expr, llvm::Value *&value);

llvm::Value *CreateLoad(llvm::Value *rhs_value) {
  return g.llvm.builder->CreateLoad(rhs_value);
}

PipelineResult emit_lvalue_array_dereference(FunctionCall &call, llvm::Value *&value) {
  assert(call.num_params == 2);

  //TypeDef *base_type = expression_get_type(call.params[0]);
  //EmitResult result_base = emit_expr

  llvm::Value *lhsVal;
  PipelineResult lhs_result = emit_lvalue_expression(call.params[0], lhsVal);
  if (is_success(lhs_result) == false) return lhs_result;

  llvm::Value *indexVal;
  PipelineResult index_result = emit_rvalue_expression(call.params[1], indexVal);
  if (is_success(index_result) == false) return index_result;

  //if (is_pointer(base_type)) {
  if (lhsVal->getType()->getContainedType(0)->isArrayTy()) { // TODO
    llvm::Value *zero = emit_zero();
    llvm::Value *idxs[] = { zero, indexVal };
    int num_idxs = 2; //ARRAYSIZE(idxs);
    llvm::ArrayRef<llvm::Value *> array_ref = to_array_ref(idxs, num_idxs);
    llvm::Value *arrayElement = g.llvm.builder->CreateGEP(lhsVal, array_ref);
    value = arrayElement;
    return EMIT_SUCCESS;
  } else {
    llvm::Value *zero = emit_zero();
    llvm::Value *idxs[] = { indexVal };
    int num_idxs = 1; //ARRAYSIZE(idxs);
    llvm::ArrayRef<llvm::Value *> array_ref = to_array_ref(idxs, num_idxs);
    llvm::Value *arrayElement = CreateLoad(lhsVal); //llvm.builder->CreateLoad(address);
    value = g.llvm.builder->CreateGEP(arrayElement, array_ref);
    return EMIT_SUCCESS;
  }

}

PipelineResult emit_lvalue_intrinsic_function_call(FunctionCall &call, llvm::Value *&value) {
  //Token *subtokens[2];
  //expand_tokens(token, subtokens, 2);
  //Token *op_token = subtokens[0];
  //Token *lhs_token = subtokens[1];

  switch(call.intrinsic) {
  case OP_POINTER_DEREFERENCE: {
    return emit_rvalue_expression(call.params[0], value);
  }
  case OP_ARRAY_DEREFERENCE: {
    return emit_lvalue_array_dereference(call, value);
  }
  default:
    halt();
  } 

  return make_emit_error("unknown intrinsic");

}

llvm::Value *field_index(FieldDereference &field) {
  //struct_lookup_field_idx(
  //int field_idx = field_idx_lookup(type, identifier);
  return llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), field.field_idx, SIGNED);
}

PipelineResult emit_lvalue_member_identifier(FieldDereference &field_deref, llvm::Value *&value) {
  llvm::Value *basePointer;
  PipelineResult result = emit_lvalue_expression(*field_deref.object, basePointer);
  if (is_success(result) == false) return result;
  assert(basePointer);

  //TypeDef *basePointerType = expression_get_type(*field_deref.object);

  llvm::Value *fieldIndex = field_index(field_deref);
  llvm::Value *zero = llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 0, SIGNED);

  llvm::Value *idxs[] = {zero, fieldIndex};
  const int num_idxs = 2;

  value = g.llvm.builder->CreateGEP(basePointer, to_array_ref(idxs, num_idxs));
  return EMIT_SUCCESS;
}

PipelineResult emit_lvalue_identifier(VariableReference &variable, llvm::Value *&value) {
  //assert(token->start == NULL);
  //assert(token.num_subtokens == 0);
  //llvm::Value *retval = old_db_lookup_variable(token->substring)->llvm_value;
  //assert(retval);
  //return retval;
  if (variable.variable->llvm_value == NULL) {
    return make_variable_dependancy(variable.identifier);
  }
  assert(variable.variable->llvm_value);
  value = variable.variable->llvm_value; 
  return EMIT_SUCCESS;
}

PipelineResult emit_lvalue_expression(Expression &expression, llvm::Value *&value) {
  switch(expression.type) {
  //case TOKEN_UNARY_EXPRESSION:
  //  return emit_lvalue_unary_op(expression);
  case EXPR_FUNCTION_CALL:
    //return emit_lvalue_array_dereference(expression);
    if (expression.function_call.function) {
      return emit_lvalue_function_call(expression.function_call, value);
    } else {
      return emit_lvalue_intrinsic_function_call(expression.function_call, value);
    }
  case EXPR_FIELD_DEREFERENCE:
    //case TOKEN_OP_MEMBER:
    return emit_lvalue_member_identifier(expression.field_dereference, value);
  case EXPR_VARIABLE:
//  case TOKEN_IDENTIFIER:
    return emit_lvalue_identifier(expression.variable, value);
  //case TOKEN_COMPOUND_BINARY_OPERATION:
  //case TOKEN_COMPOUND_UNARY_POST_OPERATION:
  //case TOKEN_COMPOUND_FUNCTION_CALL:
  //case TOKEN_LITERAL_INTEGER:
  default:
    halt();
  }

  return make_emit_error("unknown lvalue expression type");
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


PipelineResult emit_function_declaration(FunctionDefinition &function);
PipelineResult emit_external_function_declaration(ExternFunctionDeclaration &function);

PipelineResult emit_rvalue_array_dereference(FunctionCall &call, llvm::Value *&value) {
  llvm::Value *lvalue;
  PipelineResult result = emit_lvalue_array_dereference(call, lvalue);
  if (is_success(result) == false) return result;
  value = CreateLoad(lvalue);
  return EMIT_SUCCESS;
}

PipelineResult emit_rvalue_member_identifier(FieldDereference &field_dereference, llvm::Value *&value) {
  llvm::Value *lvalue;
  PipelineResult result = emit_lvalue_member_identifier(field_dereference, lvalue);
  if (is_success(result) == false) return result;
  value = CreateLoad(lvalue);
  return EMIT_SUCCESS;
}

PipelineResult emit_function_call_params(Expression *params, llvm::Value **values, int num_params) {
  for(int i = 0; i < num_params; ++i) {
    llvm::Value *value;
    PipelineResult result = emit_rvalue_expression(params[i], value);
    if (is_success(result) == false) return result;
    assert(value);
    values[i] = value;
  }

  return EMIT_SUCCESS;
}

PipelineResult emit_rvalue_pointer_add(Expression &pointer, Expression &integer, llvm::Value *&value) {
  llvm::Value *pvalue;
  PipelineResult presult = emit_rvalue_expression(pointer, pvalue);
  if (is_success(presult) == false) return presult;

  llvm::Value *ivalue;
  PipelineResult iresult = emit_rvalue_expression(integer, ivalue);
  if (is_success(iresult) == false) return iresult;

  llvm::Value *offset[] = {ivalue};
  value = g.llvm.builder->CreateGEP(pvalue, to_array_ref(offset, 1));
  return EMIT_SUCCESS;
}

PipelineResult emit_rvalue_intrinsic_function_call(FunctionCall &call, llvm::Value *&value) {
  switch(call.intrinsic) {
  case OP_EXTEND_INTEGER: {
    llvm::Value *rhs;
    PipelineResult result = emit_rvalue_expression(call.params[0], rhs);
    if (is_success(result) == false) return result;
    llvm::Type  *dest_type;
    PipelineResult result_type = emit_type_declaration(*call.dest_type, dest_type);
    if (is_success(result_type) == false) return result_type;
    value = g.llvm.builder->CreateSExt(rhs, dest_type);
    return EMIT_SUCCESS;
  }
  case OP_EXTEND_FLOAT: {
    llvm::Value *rhs;
    PipelineResult result = emit_rvalue_expression(call.params[0], rhs);
    if (is_success(result) == false) return result;
    llvm::Type  *dest_type;
    PipelineResult result_type = emit_type_declaration(*call.dest_type, dest_type);
    if (is_success(result_type) == false) return result_type;
    value = g.llvm.builder->CreateFPExt(rhs, dest_type);
    return EMIT_SUCCESS;
  }
  case OP_EXTEND_INT_TO_FLOAT: {
    llvm::Value *rhs;
    PipelineResult result = emit_rvalue_expression(call.params[0], rhs);
    if (is_success(result) == false) return result;
    llvm::Type  *dest_type;
    PipelineResult result_type = emit_type_declaration(*call.dest_type, dest_type);
    if (is_success(result_type) == false) return result_type;
    value = g.llvm.builder->CreateSIToFP(rhs, dest_type);
    return EMIT_SUCCESS;
  }
  case OP_CAST_POINTER: {
    llvm::Value *rhs;
    PipelineResult result = emit_rvalue_expression(call.params[0], rhs);
    if (is_success(result) == false) return result;
    llvm::Type  *dest_type;
    PipelineResult result_type = emit_type_declaration(*call.dest_type, dest_type);
    if (is_success(result_type) == false) return result_type;
    value = g.llvm.builder->CreateBitCast(rhs, dest_type);
    return EMIT_SUCCESS;
  }
  case OP_CAST_ARRAY_TO_POINTER: {
    llvm::Value *rhs;
    PipelineResult result = emit_rvalue_expression(call.params[0], rhs);
    if (is_success(result) == false) return result;
    llvm::Value *zero = llvm::ConstantInt::get(llvm::IntegerType::get(*g.llvm.context, 32), 0, SIGNED);
    llvm::Value *zeros[] = {zero, zero};
    value = g.llvm.builder->CreateGEP(rhs, to_array_ref(zeros, 2));
    return EMIT_SUCCESS;
  }
  case OP_POINTER_ADD:
    return emit_rvalue_pointer_add(call.params[0], call.params[1], value);
  //case OP_POINTER_SUBTRACT,
  //  return emit_rvalue_pointer_add(call.params[0], call.params[1]);
  case OP_POINTER_DEREFERENCE: {
      llvm::Value *address;
      PipelineResult result = emit_rvalue_expression(call.params[0], address);
    if (is_success(result) == false) return result;
      value = CreateLoad(address); //llvm.builder->CreateLoad(address);
      return EMIT_SUCCESS;
    }
  case OP_ARRAY_DEREFERENCE:
      //return emit_rvalue_member_identifier()
    return emit_rvalue_array_dereference(call, value);
  case OP_ADDRESS_OF:
    return emit_lvalue_expression(call.params[0], value);
  //case OP_MEMBER_DEREFERENCE:
  //    return emit_rvalue_member_identifier(call.);
    default:
      halt();
  }

  return make_emit_error("unknown rvalue intrinsic");
}

llvm::Value *emit_rvalue_extern_function_call(FunctionCall &call) {
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

PipelineResult emit_rvalue_function_call(FunctionCall &call, llvm::Value *&value) {
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
    if (llvm_function == NULL) {
      emit_external_function_declaration(*call.extern_function);
      llvm_function = call.extern_function->llvm_function;
    }
  } else {
    halt();
  }

  assert(llvm_function);

  //assert(call.function);
  TypeDeclaration *retval_type = call.function ? call.function->retval_type : call.extern_function->retval_type;
  if (is_structure(retval_type)) {
    if (call.num_params == 0) {
      PipelineResult result = CreateAlloca(to_substring("retval"), *retval_type, value);
      assert(is_success(result));
      llvm::Value *param_expressions[1];
      param_expressions[0] = value;

      llvm::ArrayRef<llvm::Value *> ref_params = to_array_ref(param_expressions, 1);
      g.llvm.builder->CreateCall(llvm_function, ref_params);
      value = CreateLoad(value);
    } else {
      llvm::Value *param_expressions[MAX_PARAMS+1];
      assert(call.num_params < MAX_PARAMS);
      PipelineResult result = CreateAlloca(to_substring("retval"), *retval_type, value);
      assert(is_success(result));
      param_expressions[0] = value;
      PipelineResult result_params = emit_function_call_params(call.params, param_expressions+1, call.num_params);
      if (is_success(result_params) == false) return result_params;

      llvm::ArrayRef<llvm::Value *> ref_params = to_array_ref(param_expressions, call.num_params+1);
      g.llvm.builder->CreateCall(llvm_function, ref_params);
      value = CreateLoad(value);
      //g.llvm.module->dump();
    }
  } else {
    if (call.num_params == 0) {
      value = g.llvm.builder->CreateCall(llvm_function);
    } else {
      llvm::Value *param_expressions[MAX_PARAMS];
      assert(call.num_params < MAX_PARAMS);
      PipelineResult result = emit_function_call_params(call.params, param_expressions, call.num_params);
      if (is_success(result) == false) return result;

      llvm::ArrayRef<llvm::Value *> ref_params = to_array_ref(param_expressions, call.num_params);
      value = g.llvm.builder->CreateCall(llvm_function, ref_params);
    }
  }

  return EMIT_SUCCESS;
}

PipelineResult emit_rvalue_float_literal(NumericLiteral &literal, llvm::Value *&value) {
  TypeDeclaration *type = integer_type_lookup("f32"); //calc_integer_literal_type(integer_literal);
  llvm::Type *llvm_type;
  PipelineResult result = emit_type_declaration(*type, llvm_type);
  if (is_success(result) == false) return result;
  llvm::StringRef str = to_string_ref(literal.literal);
  value = llvm::ConstantFP::get(llvm_type, str);
  return EMIT_SUCCESS;
}

PipelineResult emit_rvalue_integer_literal(NumericLiteral &literal, llvm::Value *&value) {
  TypeDeclaration *type = integer_type_lookup("i32"); //calc_integer_literal_type(integer_literal);
  int num_bits = type_size(type);
  llvm::IntegerType *llvm_type = llvm::IntegerType::get(*g.llvm.context, num_bits);
  //assert(type->llvm_type == (llvm::Type *)llvm_type);

  llvm::StringRef str = to_string_ref(literal.literal);

  const int RADIX = 10;
  value = llvm::ConstantInt::get(llvm_type, str, RADIX);
  return EMIT_SUCCESS;
}

PipelineResult emit_rvalue_float_literal(Token *float_literal, llvm::Value *&value) {
  //assert(float_literal.token == TOKEN_LITERAL_FLOAT);
  llvm::Type *type = llvm::Type::getFloatTy(*g.llvm.context);
  llvm::StringRef str = to_string_ref(float_literal->substring);
  value = llvm::ConstantFP::get(type, str);
  return EMIT_SUCCESS;
}

std::string emit_string_literal(SubString &literal) {
  std::string str;
  str.reserve(literal.length);

  for(int i = 0; i < literal.length; ++i) {
    if (literal.start[i] == '\\') {
      assert(i+1 < literal.length);
      switch(literal.start[i+1]) {
      case 'n':
        i++;
        str += '\n';
        break;
      default:
        halt();
      }
    } else {
      str += literal.start[i];
    }

  }

  return str;
}

PipelineResult emit_rvalue_string_literal(StringLiteral &literal, llvm::Value *&value) {
  std::string str = emit_string_literal(literal.literal);
  llvm::StringRef stringRef = str;
  value = g.llvm.builder->CreateGlobalString(stringRef);
  return EMIT_SUCCESS;
}

PipelineResult emit_rvalue_identifier(VariableReference &varaible, llvm::Value *&value) {
  llvm::Value *lvalue;
  PipelineResult result = emit_lvalue_identifier(varaible, lvalue);
  if (lvalue == 0) {
    int i = 0; ++i;
  }
  if (is_success(result) == false) return result;
  value = CreateLoad(lvalue);
  return EMIT_SUCCESS;
}

PipelineResult emit_rvalue_expression(Expression &expr, llvm::Value *&value) {
  switch(expr.type) {
  case EXPR_FUNCTION_CALL:
    if (expr.function_call.function || expr.function_call.extern_function) {
      return emit_rvalue_function_call(expr.function_call, value);
    } else {
      return emit_rvalue_intrinsic_function_call(expr.function_call, value);
    }
  case EXPR_FLOAT_LITERAL:
    return emit_rvalue_float_literal(expr.numeric_literal, value);
  case EXPR_NUMERIC_LITERAL:
    return emit_rvalue_integer_literal(expr.numeric_literal, value);
  case EXPR_STRING_LITERAL:
    return emit_rvalue_string_literal(expr.string_literal, value);
  case EXPR_VARIABLE:
    return emit_rvalue_identifier(expr.variable, value);
  case EXPR_FIELD_DEREFERENCE:
    return emit_rvalue_member_identifier(expr.field_dereference, value);
  default:
    halt();
  }

  return make_emit_error("unknown rvalue expression");
}

PipelineResult emit_variable_initalization(VariableDefinition &variable) {
  llvm::Value *value_variable;
  PipelineResult result = CreateAlloca(variable.identifier, *variable.type, value_variable);
  if (is_success(result) == false) return result;

  llvm::Value *value_initaizlier = 0;
  switch(variable.initial_value->type) {
  case EXPR_DEFAULT_VALUE: {
      llvm::Type *llvm_type;
      PipelineResult result = emit_type_declaration(*variable.type, llvm_type);
      if (is_success(result) == false) return result;
      value_initaizlier = llvm::Constant::getNullValue(llvm_type);
    } break;
  default: {
      PipelineResult result = emit_rvalue_expression(*variable.initial_value, value_initaizlier);
      if (is_success(result) == false) return result;
    } break;
  }
  assert(value_initaizlier);
  CreateStore(value_initaizlier, value_variable);
  variable.llvm_value = value_variable;
  return EMIT_SUCCESS; //make_success(value_variable);
}

PipelineResult emit_local_variable_definition(VariableDefinition &variable) {
  //DeclarationResult result = db_add_variable_definition(variable);
  PipelineResult result = db_add_variable_declaration(variable);
  if (is_success(result) == false) return result;
  return emit_variable_initalization(variable);
}

PipelineResult emit_global_variable_initialization(VariableDefinition &variable) {
  //TypeDef *type = variable.type->type;
  llvm::Type *llvm_type;
  PipelineResult result = emit_type_declaration(*variable.type, llvm_type);

  llvm::Constant *constant;

  switch(variable.initial_value->type) {
  case EXPR_DEFAULT_VALUE: {
      constant = llvm::Constant::getNullValue(llvm_type);
    } break;
  default: {
      PipelineResult result = emit_compile_time_expression(*variable.initial_value, constant);
      if (is_success(result) == false) return result;
    } break;
  }

  llvm::StringRef name = to_string_ref(variable.identifier);
  llvm::Value *value = new llvm::GlobalVariable(*g.llvm.module, llvm_type, false, llvm::GlobalVariable::ExternalLinkage, constant, name);
  variable.llvm_value = value;

  return EMIT_SUCCESS;
}

PipelineResult emit_assignment_statement(AssignmentStatement &statement) {
  llvm::Value *lhs;
  PipelineResult lhs_result = emit_lvalue_expression(*statement.lhs, lhs);
  if (is_success(lhs_result) == false) return lhs_result;

  llvm::Value *rhs;
  PipelineResult rhs_result = emit_rvalue_expression(*statement.rhs, rhs);
  if (is_success(rhs_result) == false) return rhs_result;

  switch(statement.op) {
  case TOKEN_ASSIGNMENT_OP: {
    CreateStore(rhs, lhs);
  } break;
  case TOKEN_ADD_ASSIGNMENT_OP: {
    llvm::Value *r_lhs;
    PipelineResult result = emit_rvalue_expression(*statement.lhs, r_lhs);
    if (is_success(result) == false) return result;
    llvm::Value *newVal = CreateAdd(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  case TOKEN_SUB_ASSIGNMENT_OP: {
    llvm::Value *r_lhs;
    PipelineResult result = emit_rvalue_expression(*statement.lhs, r_lhs);
    if (is_success(result) == false) return result;
    llvm::Value *newVal = CreateSub(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  case TOKEN_MUL_ASSIGNMENT_OP: {
    llvm::Value *r_lhs;
    PipelineResult result = emit_rvalue_expression(*statement.lhs, r_lhs);
    if (is_success(result) == false) return result;
    llvm::Value *newVal = CreateMul(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  case TOKEN_DIV_ASSIGNMENT_OP: {
    llvm::Value *r_lhs;
    PipelineResult result = emit_rvalue_expression(*statement.lhs, r_lhs);
    if (is_success(result) == false) return result;
    llvm::Value *newVal = CreateSDiv(r_lhs, rhs);
    CreateStore(newVal, lhs);
  } break;
  case TOKEN_REM_ASSIGNMENT_OP: {
    llvm::Value *r_lhs;
    PipelineResult result = emit_rvalue_expression(*statement.lhs, r_lhs);
    if (is_success(result) == false) return result;
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

  return EMIT_SUCCESS;
}

PipelineResult emit_return_statement(ReturnStatement &return_statement, llvm::Value *retval_param) {
  if (retval_param) {
    llvm::Value *retval;
    PipelineResult result = emit_rvalue_expression(*return_statement.retval, retval);
    if (is_success(result) == false) return result;
    CreateStore(retval, retval_param);
    g.llvm.builder->CreateRetVoid();
    //g.llvm.module->dump();
  } else {
    llvm::Value *retval;
    PipelineResult result = emit_rvalue_expression(*return_statement.retval, retval);
    if (is_success(result) == false) return result;
    g.llvm.builder->CreateRet(retval);
  }


  return EMIT_SUCCESS;
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

  const char *space = find_any_of(replacement, end, ":, \t\r\n\0");
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
    //assert(equals);
    if (equals == NULL) return LLVM_REPLACEMENT_FIRST_EXPRESSION;

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
      //VariableDefinition *variable = db_lookup_variable(token_substr);
      VaraibleDefinitionDBEntry *entry = db_lookup_variable(token_substr);
      assert(entry->type == VT_VARIABLE);
      VariableDefinition *variable = entry->variable;
      llvm::Type *type = variable->llvm_value->getType()->getContainedType(0);
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
      VaraibleDefinitionDBEntry *entry = db_lookup_variable(token_substr);

      if (entry->type == VT_STATEMENT_PARAM) {
        assert(entry->statement_param->type == SCP_IDENTIFIER);
        std::string replacement = to_cstring(entry->statement_param->identifier);
        std::string new_line        = string_format("%s%s%s", old_lhs.c_str(), replacement.c_str(), old_rhs.c_str());
        lines.erase(lines.begin() + i);
        lines.insert(lines.begin() + i, new_line);
      } else {
        assert(entry->type == VT_VARIABLE);
        VariableDefinition *variable = entry->variable;
        llvm::Type *type = variable->llvm_value->getType()->getContainedType(0);
        std::string type_str = to_llvm_type_str(type);
        std::string new_load  = string_format("%%%s%d = load %s* %%%s", token_str.c_str(), temp_n, type_str.c_str(), token_str.c_str());
        std::string new_line        = string_format("%s%s %%%s%d%s", old_lhs.c_str(), type_str.c_str(), token_str.c_str(), temp_n, old_rhs.c_str());
        std::string new_line_no_type= string_format("%s%%%s%d%s", old_lhs.c_str(), token_str.c_str(), temp_n, old_rhs.c_str());
        if (replacement == LLVM_REPLACEMENT_EXPRESSION) new_line = new_line_no_type;
        lines.erase(lines.begin() + i); //lines.remove(line);
        lines.insert(lines.begin() + i, new_load); //lines.insert(new_load);
        lines.insert(lines.begin() + i+1, new_line); //lines.insert(new_line);
      }

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

static bool ParseAssembly2(llvm::MemoryBuffer *F, llvm::Module &M, llvm::SMDiagnostic &Err, llvm::Function *Function, llvm::BasicBlock *&BB) 
{
  llvm::SourceMgr SM;
  //std::unique_ptr<MemoryBuffer> Buf = llvm::MemoryBuffer::getMemBuffer(F, false);
  //SM.AddNewSourceBuffer(std::move(Buf), SMLoc());
  return llvm::LLParser2(F, SM, Err, &M).RunSubFunction(Function, BB);
}

//DeclarationResult make_declaration_error(const char *error_fmt, ...) {
//  DeclarationResult retval;
//  retval.result = RESULT_ERROR;
//  //retval.error.location = location;
//  FORMAT_ERROR_STRING(retval.error.error_string, error_fmt);
//  return retval;
//}

PipelineResult emit_inline_llvm(llvm::Function *function, LLVMStatement &llvm_statement) {
  char replaced_llvm_buffer[1024];
  replace_inline_llvm_bindings(llvm_statement.raw_llvm, replaced_llvm_buffer);

  llvm::StringRef llvmAssembly(replaced_llvm_buffer);
  llvm::MemoryBuffer *memory = llvm::MemoryBuffer::getMemBuffer(llvmAssembly, "<string>", true);
  llvm::SMDiagnostic error;
  llvm::BasicBlock *block = g.llvm.builder->GetInsertBlock();
  llvm::BasicBlock *old_block = block;

  //g.llvm.module->dump();
  bool retval = ParseAssembly2(memory, *g.llvm.module, error, function, block);

  if (retval == false) {
    return make_emit_error("error parsing inline assembly");
  }

  if (block != old_block) {
    g.llvm.builder->SetInsertPoint(block);
  }
  //assert(retval);

  return EMIT_SUCCESS;
}

PipelineResult emit_function_declaration(FunctionDefinition &function) {
  llvm::Type *retval_type;
  PipelineResult result = emit_type_declaration(*function.retval_type, retval_type);
  if (is_success(result) == false) return result;

  llvm::FunctionType *functionType;

  //int size = type_size(function.retval_type);
  if (is_structure(function.retval_type)) {

    llvm::Type *param_types[MAX_PARAMS+1];
    assert(function.num_params <= MAX_PARAMS);

    //assert(retval_type->isPointerTy());
    param_types[0] = llvm::PointerType::get(retval_type, 0);

    if (function.params[function.num_params-1].type == PD_VARARGS) {
      PipelineResult result = get_function_param_llvm_types(function.params, param_types + 1, function.num_params-1);
      if (is_success(result) == false) return result;
      llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function.num_params-1 + 1);
      functionType = llvm::FunctionType::get(llvm::Type::getVoidTy(*g.llvm.context), args, VAR_ARGS);
    } else {
      PipelineResult result = get_function_param_llvm_types(function.params, param_types + 1, function.num_params);
      if (is_success(result) == false) return result;
      llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function.num_params + 1);
      functionType = llvm::FunctionType::get(llvm::Type::getVoidTy(*g.llvm.context), args, FIXED_ARGS);
    }

    llvm::StringRef name = to_string_ref(function.identifier);
    function.llvm_function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, g.llvm.module);
    assert(function.llvm_function);

    function.llvm_function->addAttribute(1, llvm::Attribute::StructRet);
    function.llvm_function->addAttribute(1, llvm::Attribute::NoAlias);

    //g.llvm.module->dump();
  } else {
    if (function.num_params == 0) 
    {
      functionType = llvm::FunctionType::get(retval_type, FIXED_ARGS);
    }
    else
    {
      llvm::Type *param_types[MAX_PARAMS];
      assert(function.num_params <= MAX_PARAMS);

      if (function.params[function.num_params-1].type == PD_VARARGS) {
        PipelineResult result = get_function_param_llvm_types(function.params, param_types, function.num_params-1);
        if (is_success(result) == false) return result;
        llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function.num_params-1);
        functionType = llvm::FunctionType::get(retval_type, args, VAR_ARGS);
      } else {
        PipelineResult result = get_function_param_llvm_types(function.params, param_types, function.num_params);
        if (is_success(result) == false) return result;
        llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function.num_params);
        functionType = llvm::FunctionType::get(retval_type, args, FIXED_ARGS);
      }
    }

    llvm::StringRef name = to_string_ref(function.identifier);
    function.llvm_function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, g.llvm.module);
    assert(function.llvm_function);
  }

  return PIPELINE_SUCCESS;
}

PipelineResult emit_function_statement(llvm::Function *function, FunctionStatement &statement, llvm::Value *retval_param)
{
  switch(statement.type) {
  case FS_VARIABLE: {
    PipelineResult result = emit_local_variable_definition(statement.varaible);
    if (is_success(result) == false) { 
      return result;
    }
  } break;
  case FS_ASSIGNMENT: {
    PipelineResult result = emit_assignment_statement(statement.assignment);
    if (is_success(result) == false) {
      return result;
    }
  } break;
  case FS_STATEMENT_CALL: {
    PipelineResult result = emit_statement_call(function, statement.statement_call, retval_param);
    if (is_success(result) == false) {
      return result;
    }
  } break;
  case FS_RETURN: {
    PipelineResult result = emit_return_statement(statement.return_statement, retval_param);
    if (is_success(result) == false) {
      return result;
    }
  } break;
  case FS_EXPRESSION: {
    //assert(subtoken.num_subtokens == 1);
    llvm::Value *unused;
    PipelineResult result = emit_rvalue_expression(*statement.expression, unused);
    if (is_success(result) == false) {
      return result;
    }
  } break;
  case FS_LLVM: {
    PipelineResult result = emit_inline_llvm(function, statement.llvm);
    if (is_success(result) == false) {
      return result;
    }
  } break;
  default: 
    halt();
  }

  return PIPELINE_SUCCESS;
}

bool variable_exists(const SubString &identifier) {
  return db_lookup_variable(identifier) != NULL;
}

PipelineResult emit_function_definition(FunctionDefinition &function) {
  //db_push_scope();

  {
    DBScope scope;

    if (function.llvm_function == NULL) {
      PipelineResult result = emit_function_declaration(function);
      if (is_success(result) == false) {
        return result;
      }
    }

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(*g.llvm.context, "", function.llvm_function);
    g.llvm.builder->SetInsertPoint(entry);

    llvm::Value *retval_param = NULL;
    if (is_structure(function.retval_type)) {
      llvm::Function::arg_iterator arg_iterator = function.llvm_function->arg_begin();
      for(int i = 0; i < function.num_params+1; ++i, arg_iterator++) {
        if (i == 0) {
          llvm::Argument *AI = arg_iterator;
          retval_param = AI;
        } else {
          assert(function.params[i-1].type == PD_VARIABLE);
          VariableDefinition &param = function.params[i-1].variable;

          if (variable_exists(function.params[i-1].variable.identifier)) {
            return make_emit_error("Duplicate variable %s", to_cstring(param.identifier).c_str());
          }

          TypeDeclaration *type = param.type;
          llvm::Argument *AI = arg_iterator;
          llvm::Value *alloca;
          PipelineResult result = CreateAlloca(param.identifier, *type, alloca);
          if (is_success(result) == false) {
            return result;
          }
          CreateStore(AI, alloca);
          param.llvm_value = alloca;

          PipelineResult result_add = db_add_variable_declaration(param);
          if (is_success(result_add) == false) return result_add;
        }
      }
    } else {
      llvm::Function::arg_iterator arg_iterator = function.llvm_function->arg_begin();
      for(int i = 0; i < function.num_params; ++i, arg_iterator++) {
        assert(function.params[i].type == PD_VARIABLE);
        VariableDefinition &param = function.params[i].variable;

        if (variable_exists(function.params[i].variable.identifier)) {
          return make_emit_error("Duplicate variable %s", to_cstring(param.identifier).c_str());
        }

        TypeDeclaration *type = param.type;
        llvm::Argument *AI = arg_iterator;
        llvm::Value *alloca;
        PipelineResult result = CreateAlloca(param.identifier, *type, alloca);
        if (is_success(result) == false) {
          return result;
        }
        CreateStore(AI, alloca);
        param.llvm_value = alloca;

        PipelineResult result_add = db_add_variable_declaration(param);
        if (is_success(result_add) == false) return result_add;
      }
    }

    for(int i = 0; i < function.num_statements; ++i) {
      FunctionStatement &statement = function.body[i];
      PipelineResult result = emit_function_statement(function.llvm_function, statement, retval_param);
      if (is_success(result) == false) return result;
    }
  }

  if (is_void(function.retval_type)) {
    g.llvm.builder->CreateRetVoid();
  }

  //function.llvm_function->Verify(()
  if (llvm::verifyFunction(*function.llvm_function)) {
    g.llvm.module->dump();
    //printf("bad function\n");
    return make_error(UNKNOWN_LOCATION, "bad function");
  } else {
    printf("emit function %s\n", to_cstring(function.identifier).c_str());
  }

  return EMIT_SUCCESS;
}

PipelineResult emit_external_function_declaration(ExternFunctionDeclaration &function) {
  //if (function.llvm_function != NULL) {
  //  return EMIT_SUCCESS;
  //}

  {
    DBScope scope;
    llvm::Type *retval_type;
    PipelineResult result = emit_type_declaration(*function.retval_type, retval_type);
    if (is_success(result) == false) return result;

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
        PipelineResult result = get_function_param_llvm_types(function.params, param_types, function.num_params-1);
        if (is_success(result) == false) return result;
        llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function.num_params-1);
        functionType = llvm::FunctionType::get(retval_type, args, VAR_ARGS);
      } else {
        PipelineResult result = get_function_param_llvm_types(function.params, param_types, function.num_params);
        if (is_success(result) == false) return result;
        llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function.num_params);
        functionType = llvm::FunctionType::get(retval_type, args, FIXED_ARGS);
      }

      //get_function_param_llvm_types(function.params, param_types, function.num_params);
      //llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, function.num_params);
      //functionType = llvm::FunctionType::get(retval_type, args, FIXED_ARGS);
    }

    printf("emit extern function %s\n", to_cstring(function.identifier).c_str());

    llvm::StringRef name = to_string_ref(function.identifier);
    function.llvm_function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, g.llvm.module);
  }


  return EMIT_SUCCESS;
}

//PipelineResult emit_program_statement(ProgramStatement &statement) {
//    switch(statement.type) {
//    case PS_STRUCT:
//    case PS_LLVM_TYPE:
//      // case PS_TYPEDEF:
//      break;
//    case PS_VARIABLE:
//      return emit_global_variable_initialization(statement.varaible);
//    case PS_FUNCTION:
//      return emit_function_definition(statement.function);
//    case PS_EXTERN_FUNCTION:
//      return emit_external_function_declaration(statement.extern_function);
//    case PS_IMPORT:
//      break;
//    default:
//      halt();
//    }
//
//  return EMIT_SUCCESS;
//}
//
//PipelineResult emit_program(Program &program, const char *dest_file) {
//  for(int i = 0; i < program.num_statements; ++i) {
//    PipelineResult result = emit_program_statement(program.statement[i]);
//    if(is_success(result) == false) return result;
//  }
//
//  //g.llvm.module->dump();
//
//  return EMIT_SUCCESS;
//}

//CompileResult make_result(const LLVMTypeResult &result) {
//  CompileResult retval;
//  retval.result = result.result;
//
//  //const std::string &file = g.parser.files[result.error.location.file_index];
//  const std::string &file = result.error.location.parser->filepath;
//  int line = result.error.location.line;
//  int col = result.error.location.column;
//  retval.error.error_string = string_format("%s(%d:%d): %s\n", file.c_str(), line, col, result.error.error_string.c_str());
//
//  return retval;
//}

CompileResult make_result(const ParseResult &parse_result) {
  CompileResult retval;
  retval.result = parse_result.result;

  //const std::string &file = g.parser.files[parse_result.error.location.file_index];
  const std::string &file = parse_result.error.location.parser->filepath;
  int line = parse_result.error.location.line;
  int col = parse_result.error.location.column;
  retval.error.error_string = string_format("%s(%d:%d): %s\n", file.c_str(), line, col, parse_result.error.error_string.c_str());

  return retval;
}

//CompileResult make_result(const DeclarationResult &result) {
//  CompileResult retval;
//  retval.result = result.result;
//  //retval.error = typecheck_result.error;
//  const std::string &file = result.error.location.parser->filepath;
//  //const std::string &file = g.parser.files[result.error.location.file_index];
//  int line = result.error.location.line;
//  int col = result.error.location.column;
//  retval.error.error_string = string_format("%s(%d:%d): %s\n", file.c_str(), line, col, result.error.error_string.c_str());
//  return retval;
//}

//CompileResult make_result(const DigestResult &result) {
//  CompileResult retval;
//  retval.result = result.result;
//  //retval.error = typecheck_result.error;
//  std::string file = "unknown";
//  //const std::string &file = g.parser.files[result.error.location.file_index];
//  int line = result.error.location.line;
//  int col = result.error.location.column;
//  retval.error.error_string = string_format("%s(%d:%d): %s\n", file.c_str(), line, col, result.error.error_string.c_str());
//  return retval;
//}

CompileResult make_result(const PipelineResult &typecheck_result) {
  CompileResult retval;
  retval.result = typecheck_result.result;
  //retval.error = typecheck_result.error;
  const std::string &file = typecheck_result.error.location.parser->filepath;
  //const std::string &file = g.parser.files[typecheck_result.error.location.file_index];
  int line = typecheck_result.error.location.line;
  int col = typecheck_result.error.location.column;
  retval.error.error_string = string_format("%s(%d:%d): %s\n", file.c_str(), line, col, typecheck_result.error.error_string.c_str());
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
  for(Token *field_definition = tok_fields->start; field_definition != NULL; field_definition = field_definition->next) {
    VariableDefinition &field_def = statement.struct_def.fields[i++];
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

DigestResult digest_float_literal(Token *expression, Expression &expr) {
  expr.type = EXPR_FLOAT_LITERAL;
  expr.numeric_literal.literal = expression->substring;
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
  case TOKEN_FLOAT_LITERAL:
    return digest_float_literal(expression, expr);
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

StatementCallParam *alloc_statement_call_params(int num_params) {
  return (StatementCallParam *)calloc(num_params, sizeof(StatementCallParam));
}

DigestResult digest_statement_call_param(Token *tok_param, StatementCallParam &param) {
  switch(tok_param->type) {
  case TOKEN_STATEMENT_CALL_PARAM_IDENTIFIER:
    param.type = SCP_IDENTIFIER;
    param.identifier = tok_param->start->substring;
    break;
  case TOKEN_STATEMENT_CALL_PARAM_EXPRESSION:
    param.type = SCP_IDENTIFIER;
    param.identifier = tok_param->start->substring;
    break;
  default:
    halt();
  }

  return DIGEST_SUCCESS;
}

DigestResult digest_function_statement(Token *tok_function_statement, FunctionStatement &fn_statement);


DigestResult digest_statement_call(Token *tok_statement, FunctionStatement &statement) {
  Token *tok_identifier, *tok_params, *tok_body;
  expand_tokens(tok_statement, tok_identifier, tok_params, tok_body);

  statement.type = FS_STATEMENT_CALL;
  statement.statement_call.identifier = tok_identifier->substring;
  statement.statement_call.num_params = count_subtokens(tok_params);
  statement.statement_call.params = alloc_statement_call_params(statement.statement_call.num_params);
  int i = 0;
  for(Token *tok_param = tok_params->start; tok_param != NULL; tok_param = tok_param->next) {
    StatementCallParam &param = statement.statement_call.params[i++];
    DigestResult param_result = digest_statement_call_param(tok_param, param);
    if (is_success(param_result) == false) return param_result;
  }

  statement.statement_call.num_statement_body = count_subtokens(tok_body);
  statement.statement_call.body = function_body_alloc(statement.statement_call.num_statement_body);
  int j = 0;
  for(Token *tok_fn_statement = tok_body->start; tok_fn_statement != NULL; tok_fn_statement = tok_fn_statement->next) {
    FunctionStatement &fn_statement = statement.statement_call.body[j++];
    DigestResult result = digest_function_statement(tok_fn_statement, fn_statement);
    if (is_success(result) == false) return result;
  }

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
  case TOKEN_STATEMENT_CALL:
    return digest_statement_call(tok_function_statement, fn_statement);
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

DigestResult digest_statement_param_definition(Token *tok_param, StatementParam &param) {
  Token *tok_type, *tok_identifier;
  expand_tokens(tok_param, tok_type, tok_identifier);

  DigestResult result = digest_type_declaration(tok_type, param.type);
  if (is_success(result) == false) return result;
  
  param.identifier = tok_identifier->substring;
  return DIGEST_SUCCESS;
}

DigestResult digest_statement_definition(Token *tok_statement, ProgramStatement &statement) {
  Token *tok_identifier, *tok_params, *tok_body;
  expand_tokens(tok_statement, tok_identifier, tok_params, tok_body);

  statement.type = PS_STATEMENT;
  statement.statement.identifier = tok_identifier->substring;

  statement.statement.num_statement_params = count_subtokens(tok_params);
  statement.statement.statement_params = statement_params_alloc(statement.statement.num_statement_params);
  int i = 0;
  for(Token *tok_param = tok_params->start; tok_param != NULL; tok_param = tok_param->next) {
    StatementParam &param = statement.statement.statement_params[i++];
    DigestResult result = digest_statement_param_definition(tok_param, param);
    if (is_success(result) == false) return result;
  }

  // TODO: multi-statements
  statement.statement.num_statement_yield_params = 0;

  statement.statement.num_body_statements = count_subtokens(tok_body);
  statement.statement.body = function_body_alloc(statement.statement.num_body_statements);
  int j = 0;
  for(Token *tok_statement = tok_body->start; tok_statement != NULL; tok_statement = tok_statement->next) {
    FunctionStatement &stmt = statement.statement.body[j++];
    DigestResult result = digest_function_statement(tok_statement, stmt);
    if (is_success(result) == false) return result;
  }

  return DIGEST_SUCCESS;
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
DigestResult digest_program(ParserState &parser, Token *program_token, Program &program) {
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
    case TOKEN_STATEMENT_DEFINITION: {
      DigestResult result = digest_statement_definition(program_statement, cur_statement);
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
      cur_statement.import.file      = program_statement->start->substring;
      cur_statement.import.parser    = &parser;
      //cur_statement.import.
      break;
    default:
      halt();
    }

    assert(cur_statement.type);
  }

  return DIGEST_SUCCESS;
};

struct ScopeDB {
  ScopeDB *parent;
  Table<TypeDefinitionDBEntry> types;
  Table<VaraibleDefinitionDBEntry> variables;
  Table<FunctionDefinitionDBEntry> functions;
};

bool has_parsed_file(SubString &filename) {
  return false;
}

struct CompilationFile {
  ParserState parser;
  Program program;
  std::string file;
};

struct CompilationProject {
  std::list<CompilationFile *>files;
};

CompilationFile &alloc_compliation_file(CompilationProject &project, const std::string &full_path) {
  //CompilationFile *file = (CompilationFile *)calloc(1, sizeof(CompilationFile));
  CompilationFile *file = new CompilationFile;

  file->file = full_path;
  parser_init(file->parser, file->file);
  //file->program;

  return *file;
}

PipelineResult compile_set_try_add_file(CompilationProject &project, CompilationSet &active_set, std::string &directory, SubString &relative_file) {
  std::string full_filename = make_full_path(directory, relative_file);

  for(CompilationFile *file : project.files) {
    if (file->file == full_filename) {
      return PIPELINE_SUCCESS;
    }
  }

  CompilationFile &comp_file = alloc_compliation_file(project, full_filename);

  ParseResult presult = parse_program(comp_file.parser, full_filename.c_str());
  if (is_success(presult) == false) {
    PipelineResult result;
    result.result = presult.result;
    result.error = presult.error;
    return result;
  }

  DigestResult dresult = digest_program(comp_file.parser, presult.start, comp_file.program);
  if (is_success(dresult) == false) return dresult;

  for(int i = comp_file.program.num_statements-1; i >= 0; --i) {
    CompilationUnit unit = {};
    unit.statement = &comp_file.program.statement[i];
    active_set.units.push_back(unit);
  }

  return PIPELINE_SUCCESS;
}

PipelineResult emit_import_statement(ImportStatement &import, CompilationProject &project, CompilationSet &active_set) {
  PipelineResult result = compile_set_try_add_file(project, active_set, import.parser->current_directory, import.file);
  if (is_success(result) == false) return result;
  printf("emit import %s\n", to_cstring(import.file).c_str());
  return PIPELINE_SUCCESS;
}

//statement while(bool expr) {
//label Test;
//  if (expr) {
//    yield;
//    goto Test;
//  }
//}
//
//for (statement initial, bool expr, statement inc) {
//  initial;
//  while(expr) {
//    yield;
//    inc;
//  }
//}
//
//statement if (bool expr) {
//  llvm {
//    %i1 = trunc $expr to i1
//    br i1 %i1, label %Then, label %Else
//  }
//label Then;
//  yield;
//label Else;
//}
//
//statement goto (identifier lbl) {
//  llvm {
//    br label $lbl
//  }
//};
//
//statement label (identifier label) {
//  llvm {
//$label:
//  }
//};
//
//statement if (bool expr) clauseThen else clauseElse {
//  if (expr) {
//    yield clauseThen;
//    goto End;
//  } 
//  yield clauseElse;
//label End;
//}
//
//statement for(identifier x, identifier arr) {
//  for(int i = 0, i < arr.length, ++i) {
//    auto x = arr[i];
//    yield;
//  }
//}


PipelineResult pipelined_emit(ProgramStatement &statement, CompilationProject &project, CompilationSet &active_set) {
  switch(statement.type) {
  case PS_STATEMENT:
    return emit_statement_definition(statement.statement);
  case PS_FUNCTION:
    return emit_function_definition(statement.function);
  case PS_EXTERN_FUNCTION:
    break;
  case PS_LLVM_TYPE:
    return emit_llvm_type_definition(statement.llvm_type);
    //llvm_emit_llvm_type(*type->llvm_def.new_type_definition);
  case PS_STRUCT:
    return emit_struct_type_definition(statement.struct_def);
  case PS_VARIABLE:
    //return emit_variable_initalization(statement.varaible);
    return emit_global_variable_initialization(statement.varaible);
  case PS_IMPORT: 
    return emit_import_statement(statement.import, project, active_set);
  default:
    halt();
  }

  return PIPELINE_SUCCESS;
}


PipelineResult emit_llvm_type_declaration(LLVMTypeDefinition &definition) {
  TypeDefinitionDBEntry new_entry;
  new_entry.scope = g.db.scope;
  new_entry.type = (TypeDefinition *)calloc(1, sizeof(TypeDefinition));
  new_entry.type->type = TYPE_LLVM;
  new_entry.type->llvm_type_definition = &definition;
  new_entry.type->decl = (TypeDeclaration *)calloc(1, sizeof(TypeDeclaration));
  new_entry.type->decl->kind = TYPE_BASE;
  new_entry.type->decl->base_type.identifier = definition.identifier;
  g.db.types.push_back(new_entry);
  //db_add_type(type.identifier);

  return PIPELINE_SUCCESS;
}

PipelineResult emit_struct_declaration(StructDefinition &definition) {
  TypeDefinitionDBEntry new_entry = {};
  new_entry.scope = g.db.scope;
  new_entry.type = (TypeDefinition *)calloc(1, sizeof(TypeDefinition));
  new_entry.type->type = TYPE_STRUCT;
  new_entry.type->struct_definition = &definition;
  new_entry.type->decl = (TypeDeclaration *)calloc(1, sizeof(TypeDeclaration));
  new_entry.type->decl->kind = TYPE_BASE;
  new_entry.type->decl->base_type.identifier = definition.identifier;
  g.db.types.push_back(new_entry);
  //db_add_type(type.identifier);

  return PIPELINE_SUCCESS;
}

PipelineResult emit_variable_declaration(VariableDefinition &definition) {
  PipelineResult result = db_add_variable_declaration(definition);
  if (is_success(result) == false) return result;
  return PIPELINE_SUCCESS;
}

PipelineResult pipelined_declaration(ProgramStatement &statement) {
  switch(statement.type) {
  case PS_STATEMENT:
    return emit_statement_declaration(statement.statement);
  case PS_FUNCTION:
    return emit_function_declaration(statement.function);
  case PS_EXTERN_FUNCTION:
    return emit_external_function_declaration(statement.extern_function);
  case PS_LLVM_TYPE:
    return emit_llvm_type_declaration(statement.llvm_type);
  case PS_STRUCT:
    return emit_struct_declaration(statement.struct_def);
  case PS_VARIABLE:
    return emit_variable_declaration(statement.varaible);
  case PS_IMPORT:
    break;
  default:
    halt();
  }

  return PIPELINE_SUCCESS;
}

PipelineResult pipelined_typecheck(ProgramStatement &statement) {
  switch(statement.type) {
  case PS_STATEMENT:
    return typecheck_statement_definition(statement.statement);
  case PS_FUNCTION:
    return typecheck_function_definition(statement.function);
  case PS_EXTERN_FUNCTION:
    return typecheck_extern_function_declaration(statement.extern_function);
  case PS_LLVM_TYPE:
    break;
  case PS_STRUCT:
    return typecheck_struct_definition(statement.struct_def);
  case PS_VARIABLE:
    return typecheck_variable_definition(statement.varaible);
  case PS_IMPORT:
    break;
  default:
    halt();
  }

  return PIPELINE_SUCCESS;
}

bool is_dependancy(PipelineResult &result) {
  return result.result == RESULT_DEPENDANCY;
}

void add_dependancy(CompilationSet &defered_set, ProgramStatement &statement, CompilationStage stage, CompilationDependancy &dependancy) {
  CompilationUnit unit;
  unit.statement = &statement;
  unit.stage = stage;
  unit.dependancy = dependancy;
  defered_set.units.push_back(unit);
}

bool resolves_dependancy(CompilationDependancy &dependancy, ProgramStatement &statement) {

  switch(dependancy.type) {
  case DEPENDANCY_FUNCTION:
    if (statement.type == PS_FUNCTION && substring_cmp(statement.function.identifier, dependancy.identifier)) return true;
    else if (statement.type == PS_EXTERN_FUNCTION && substring_cmp(statement.extern_function.identifier, dependancy.identifier)) return true;
    else return false;
  case DEPENDANCY_VARIABLE:
    if (statement.type == PS_VARIABLE && substring_cmp(statement.varaible.identifier, dependancy.identifier)) return true;
    else return false;
  case DEPENDANCY_TYPE:
    if (statement.type == PS_LLVM_TYPE && substring_cmp(statement.struct_def.identifier, dependancy.identifier)) return true;
    else if (statement.type == PS_STRUCT && substring_cmp(statement.llvm_type.identifier, dependancy.identifier)) return true;
    else return false;
  default:
    halt();
  }
  return false;
}

bool is_error(PipelineResult &result) {
  return result.result == RESULT_ERROR;
}

PipelineResult resume_pipelined_compilation(CompilationProject &project, ProgramStatement &statement, CompilationStage stage, CompilationSet &active_set, CompilationSet &defered_set) {
  PipelineResult result;
  while(1) {

    switch(stage) {
      //case STAGE_PARSE:
      //  result = pipelined_parse(statement);
      //  if (is_success(result) == false) return result;
      //  break;
      case STAGE_DECLARATION:
        result = pipelined_declaration(statement);
        if (is_error(result)) return result;
        if (is_success(result)) stage = STAGE_TYPECHECK;
        break;
      case STAGE_TYPECHECK:
        result = pipelined_typecheck(statement);
        if (is_error(result)) return result;
        if (is_success(result)) stage = STAGE_EMIT;
        break;
      case STAGE_EMIT:
        result = pipelined_emit(statement, project, active_set);
        if (is_error(result)) return result;
        if (is_success(result)) stage = STAGE_COMPLETE;
        break;
      case STAGE_COMPLETE: {
        auto it = defered_set.units.begin();
        auto result = it;
        auto end = defered_set.units.end();
        while(it != end) {
          CompilationUnit &unit = *it;
          if (resolves_dependancy(unit.dependancy, statement)) {
            active_set.units.push_back(unit);
          } else {
            *result = *it;
            ++result;
          }
          ++it;
        }

        if (result != end) {
          defered_set.units.erase(result, end);
        }

        return PIPELINE_SUCCESS;
      }
      default:
        halt();
    }

    if (is_success(result)) {
      continue;
    } else if (is_dependancy(result)) {
      add_dependancy(defered_set, statement, stage, result.dependancy);
      return PIPELINE_SUCCESS;
    } else {
      return result;
    }

  }

  halt();
}

#include <stdio.h>  /* defines FILENAME_MAX */
#ifdef WIN32
    #include <direct.h>
    #define GetCurrentDir _getcwd
#else
    #include <unistd.h>
    #define GetCurrentDir getcwd
 #endif

std::string get_current_directory() {
   char cwd[FILENAME_MAX];
   if (GetCurrentDir(cwd, sizeof(cwd)) == NULL) {
       halt();
   }

   return std::string(cwd) + '\\';
}

bool type_exists(SubString &identifier) {
  TypeDefinition *type = db_lookup_type_by_identifier(identifier);
  if (type == NULL) return false;
  return true;
}

bool function_exists(SubString &identifier) {
  return false;
}

bool is_defined(CompilationDependancy &dependancy) {
  switch(dependancy.type) {
  case DEPENDANCY_TYPE:
    return type_exists(dependancy.identifier);
  case DEPENDANCY_FUNCTION:
    return function_exists(dependancy.identifier);
  case DEPENDANCY_VARIABLE:
    return variable_exists(dependancy.identifier);
  default:
    halt();
  }

  return false;
}

PipelineResult compile_pipelined(const char* source_file, const char *dest_file) {
  CompilationProject project;
  CompilationSet active_set;
  CompilationSet defered_set;

  std::string current_directory = get_current_directory();
  PipelineResult result = compile_set_try_add_file(project, active_set, current_directory, to_substring(source_file));
  if (is_success(result) == false) return result;

  while(!active_set.units.empty()) {
    CompilationUnit unit = active_set.units.back();
    active_set.units.pop_back();

    PipelineResult result = resume_pipelined_compilation(project, *unit.statement, unit.stage, active_set, defered_set);
    if (!is_success(result)) return result;
  }

  if (!defered_set.units.empty()) {
    CompilationDependancy &dependancy = defered_set.units.front().dependancy;
    if(is_defined(dependancy)) {
      return make_error(UNKNOWN_LOCATION, "cyclic dependancy");
    } else {
      return make_error(UNKNOWN_LOCATION, "unknown dependancy: %s", to_cstring(dependancy.identifier).c_str());
    }
  }

  return PIPELINE_SUCCESS;
}

std::string path_remove_extension(std::string source_file) {
  std::string directory;
  SubString filename;
  path_split_directory_filename(source_file.c_str(), directory, filename);
  std::string retval = directory.append(filename.start, filename.length);
  size_t endPos = retval.find_last_of(".");
  retval = retval.substr(0, endPos);
  return retval;
}

std::string to_dest_file(std::string source_file) {
  return path_remove_extension(source_file) + ".out";
}

std::string to_exe_file(std::string source_file) {
  return path_remove_extension(source_file) + ".exe";
}

std::string to_obj_file(std::string source_file) {
  return path_remove_extension(source_file) + ".obj";
}

CompileResult compile_program(std::string source_file, std::string exe_file) {
  std::string dest_file = to_dest_file(source_file);
  LLVMInit(dest_file.c_str());

  PipelineResult result = compile_pipelined(source_file.c_str(), dest_file.c_str());
  if (is_success(result) == false) return make_result(result);

  std::string obj_file = to_obj_file(exe_file);

  IRCompile(g.llvm, obj_file.c_str(), exe_file.c_str());

  return COMPILE_SUCCESS;
}

bool is_success(const CompileResult &result) {
  return (result.result != RESULT_ERROR);
}


