// gg_compiler.cpp

#include "precompile.h"
#include "GGCompiler.h"
//#include "gg_llvm.h"

void halt() {
  assert(0);
}

struct GGParseInput {
  const char *data;
  GGParseInfo info;
};

///////////////////////////////////
/*

_program              = [_program_statement]

_program_statement    = _variable_definition
| _function_definition
| _type_definition

_variable_definition  = _type_identifier _variable_identifier ; 
| _type_identifier _variable_identifier = _variable_initializer ;
| _variable_identifier = _variable_initializer ;

_variable_initializer = { [_variable_intializer], }
| _expression;

_function_definition  = _type_identifier _function_identifier ( _function_params ) { _function_body }

_function_body        = [_statement]

_statement            = _variable_definition
| _function_definition
| _type_definition
| _return_statement
| _if_statement
| _for_statement
| _switch_statement
| { [_statement] }

_if_statement         = if (_expression) _statement
| if (_expression) _statement else _statement

_function_params      = [_type_identifier _variable_identifier [= _expression]],

_type_definition      = _struct_definition,
| _enum_definition,
| _typdedef definition

_type_identifier      = _identifier
_variable_identifier  = _identifier
_function_identifier  = _identifier

_identifier           = [Alpha][Alpha,Digit,_]

_literal              = _numeric_literal
| _string_literal

_numeric_literal      = [digit][digit,e,.,-][_type_suffix]

_string_literal       = "[any]"[_type_suffix]

_type_suffix          = _identifier

_expression           = ( _expression )
| _expression _binary_operator _expression
| _unary_operator _expression
| _literal                              
| _variable_identifier                            
| _function_call 
| _cast 

_function_call        = _function_identifier ( _function_call_params )  

_function_call_params = [_expression,]

_cast                 = _type_identifer : _expression                   

*/
///////////////////////////////////

const char *next_end_of_line(const char *data) {
  while (1) {
    if (*data == '\0') {
      return data;
    }

    if (*data == '\n') {
      return data;
    }

    if (*data == '\r') {
      return data;
    }

    data++;
  }

  return data;
}

const GGToken PARSE_FALSE = {};
GGToken parse_comment(const GGParseInput &input) {
  if (input.data[0] == '/' &&	input.data[1] == '/') {
    const char *eol = next_end_of_line(input.data);

    GGToken result = {};
    result.token = TOKEN_COMMENT;
    result.info = input.info;
    result.substring.start = input.data;
    result.substring.length = eol - input.data;
    result.next = eol;
    return result;
  } else {
    return PARSE_FALSE;
  }
}

bool iswhitespace(char data) {
  if (data == ' ' || data == '\t') {
    return true;
  }

  return false;
}

GGToken parse_whitespace_chars(const GGParseInput &input) {
  const char* data = input.data;
  while(iswhitespace(*data)) {
    data++;
  }

  if (data == input.data) {
    return PARSE_FALSE;
  }
  else {
    GGToken result = {};
    result.token = TOKEN_WHITESPACE;
    result.info = input.info;
    result.substring.start = input.data;
    result.substring.length = data - input.data;
    result.next = data;
    return result;
  }
}

GGToken parse_endoffile(const GGParseInput &input) {
  if (input.data[0] == 0) {
    GGToken result = {};
    result.token = TOKEN_EOF;
    return result;
  } else {
    return PARSE_FALSE;
  }
}

GGToken parse_endofline(const GGParseInput &input) {
  if (input.data[0] == '\n' || input.data[0] == '\r') {
    int eol_length = 1;
    if ((input.data[1] == '\n' || input.data[1] == '\r') && input.data[0] != input.data[1]) {
      eol_length = 2;
    }

    GGToken result = {};
    result.token = TOKEN_END_OF_LINE;
    result.info = input.info;
    result.substring.start = input.data;
    result.substring.length = eol_length;
    result.next = result.substring.start + result.substring.length;
    return result;
  } else {
    return PARSE_FALSE;
  }
}

typedef GGToken (*ParseFn)(const GGParseInput &);

void ParseOutputFree(GGToken &total_output) {
  assert(total_output.token >= TOKEN_COMPOUND);
  //printf("free: %d (%d)\n", total_output.subtokens, total_output.num_subtokens);
  free(total_output.subtokens);
  total_output.subtokens = NULL;
}

GGToken ParseOutputAlloc(GGTokenType token_type, int num_sequence) {
  assert(token_type >= TOKEN_COMPOUND);
  GGToken retval = {};
  retval.token = token_type;
  retval.subtokens = (GGToken *)malloc(sizeof(GGToken) * num_sequence);
  //printf("alloc: %d (%d)\n", retval.subtokens, num_sequence);
  return retval;
}

bool ParseOuputIsFalse(const GGToken &output) {
  return output.token == TOKEN_NONE;
}

void ParseOutputAppend(GGToken &output, const GGToken &newOutput) {
  const int MAX_SUBTOKENS = 1024;
  if (output.token == TOKEN_NONE) {
    output = newOutput;
  } else if (output.token >= TOKEN_COMPOUND) {
    assert(output.num_subtokens < MAX_SUBTOKENS);
    output.subtokens[output.num_subtokens++] = newOutput;
  } else {
    //printf("alloc append: %d\n", output.subtokens);
    output.subtokens = (GGToken *)malloc(sizeof(GGToken) * MAX_SUBTOKENS);
    output.subtokens[0] = output;
    output.subtokens[1] = newOutput;
    output.num_subtokens = 2;
    output.token = TOKEN_COMPOUND;
  }
}

#define ARRAYSIZE(ar) (sizeof((ar))/sizeof((ar)[0]))

GGToken parse_first_of(const ParseFn fn[], int numFns, const GGParseInput &input) {
  for(int i = 0; i < numFns; ++i) {
    GGToken output = fn[i](input);
    if (!ParseOuputIsFalse(output)) {
      return output;
    } 
  }

  return PARSE_FALSE;
}

GGToken parse_nonsyntax_tokens(const GGParseInput &input) {
  static const ParseFn fns[] = { parse_whitespace_chars, parse_endofline, parse_comment, parse_endoffile };
  static const int num_fns = ARRAYSIZE(fns);

  return parse_first_of(fns, num_fns, input);
}

void continue_parse_whitespace_separated_sequence(GGToken &total_output, const ParseFn sequence[], int num_sequence, const GGParseInput &input) {
  assert(num_sequence > 1);

  GGParseInput cur_input = input;

  int i = 0;
  for(; i < num_sequence; ++i) {

    {
      GGToken output = sequence[i](cur_input);
      if (ParseOuputIsFalse(output)) {
        break;
      } else if (output.token == TOKEN_DISCARD) {
        cur_input.data = output.next;
        cur_input.info = output.info;
      } else {
        assert(total_output.num_subtokens < num_sequence);
        ParseOutputAppend(total_output, output);
        cur_input.data = output.next;
        cur_input.info = output.info;
      }
    }

    if (i != num_sequence-1) {
      while(1) {
        GGToken discard = parse_nonsyntax_tokens(cur_input);
        if (ParseOuputIsFalse(discard)) {
          break;
        } else if (discard.token == TOKEN_EOF) {
          //ParseOutputAppend(total_output, output);
          break;
        } else if (discard.token == TOKEN_END_OF_LINE) {
          //ParseOutputAppend(total_output, output);
          cur_input.data = discard.next;
          cur_input.info.line_number = discard.info.line_number + 1;
          cur_input.info.col_number = 0;
        } else {
          // discard
          cur_input.data = discard.next;
          cur_input.info = discard.info;
        }
      }
    }
  }

  if (i != num_sequence) {
    ParseOutputFree(total_output);
    total_output = PARSE_FALSE;
  } else {
    total_output.info = cur_input.info;
    total_output.next = cur_input.data;
  }
}

GGToken parse_whitespace_separated_sequence(const ParseFn sequence[], int num_sequence, GGTokenType token_type, const GGParseInput &input) {
  GGToken retval = ParseOutputAlloc(token_type, num_sequence);
  continue_parse_whitespace_separated_sequence(retval, sequence, num_sequence, input);
  return retval;
}

bool ischar(char c0, char c1)
{
  return c0 == c1;
};

GGToken parse_identifier(const GGParseInput &input) {
  const char* data = input.data;

  if (isalpha(data[0]) || ischar('_', data[0])) {
    data++;
  } else {
    return PARSE_FALSE;
  }

  while(isalpha(data[0]) || ischar('_', data[0]) || isdigit(data[0])) {
    data++;
  }

  GGToken result = {};
  result.token = TOKEN_IDENTIFIER;
  result.info = input.info;
  result.substring.start = input.data;
  result.substring.length = data - input.data;
  result.next = data;
  return result;
}

GGToken parse_type_identifier(const GGParseInput &input) {
  GGToken retval = parse_identifier(input);
  if (ParseOuputIsFalse(retval)) return PARSE_FALSE;

  retval.token = TOKEN_TYPE_IDENTIFIER;
  return retval;
}

GGToken parse_variable_identifier(const GGParseInput &input) {
  return parse_identifier(input);
}

GGToken parse_function_identifier(const GGParseInput &input) {
  return parse_identifier(input);
}

GGToken parse_member_identifier(const GGParseInput &input) {
  return parse_identifier(input);
}

typedef bool (*CharPredicate)(char c0);

GGToken parse_zero_or_more_until_pred(CharPredicate pred, GGTokenType tokenType, const GGParseInput &input) {
  const char *data = input.data;
  while(data) {
    if (pred(*data) == true) break;
    data++;
  }

  GGToken result = {};
  result.token = tokenType;
  result.info = input.info;
  result.substring.start = input.data;
  result.substring.length = data - input.data;
  result.next = data;
  return result;
}

GGToken parse_one_or_more_pred(CharPredicate pred, GGTokenType tokenType, const GGParseInput &input) {
  const char *data = input.data;
  while(data) {
    if (pred(*data) == false) break;
    data++;
  }

  if (data == input.data) return PARSE_FALSE;

  GGToken result = {};
  result.token = tokenType;
  result.info = input.info;
  result.substring.start = input.data;
  result.substring.length = data - input.data;
  result.next = data;
  return result;
}

bool consume_whitespace(GGParseInput &cur_input) {
  while(1) {
    GGToken discard = parse_nonsyntax_tokens(cur_input);
    if (ParseOuputIsFalse(discard)) {
      return true;
    } else if (discard.token == TOKEN_EOF) {
      //ParseOutputAppend(total_output, output);
      return false;
    } else if (discard.token == TOKEN_END_OF_LINE) {
      //ParseOutputAppend(total_output, output);
      cur_input.data = discard.next;
      cur_input.info.line_number = discard.info.line_number + 1;
      cur_input.info.col_number = 0;
    } else {
      // discard
      cur_input.data = discard.next;
      cur_input.info = discard.info;
    }
  }
}

bool isDigit(char c) {
  return isdigit(c) != 0;
}

void continue_parse_numeric_literal(GGToken &retval, const GGParseInput &input){
  GGParseInput cur_input = input;
  while(isdigit(*cur_input.data)) {
    cur_input.data++;
  }

  retval.substring.length = cur_input.data - retval.substring.start;
  retval.next = cur_input.data;
}

GGToken parse_integer_literal(const GGParseInput &input) {
  return parse_one_or_more_pred(isDigit, TOKEN_LITERAL_INTEGER, input);
};

//GGToken parse_float_literal(const GGParseInput &input) {
//  return parse_one_or_more_pred(isDigit, TOKEN_LITERAL_FLOAT, input);
//};

GGToken parse_numeric_literal(const GGParseInput &input) {
  GGToken retval = parse_integer_literal(input);
  if (ParseOuputIsFalse(retval)) {
    return PARSE_FALSE;
  }

  GGParseInput cur_input = input;
  cur_input.data = retval.next;
  if (*retval.next == '.')
  {
    cur_input.data++;
    retval.token = TOKEN_LITERAL_FLOAT;
    continue_parse_numeric_literal(retval, cur_input);
  }

  cur_input.data = retval.next;
  if (*retval.next == 'e') 
  {
    cur_input.data++;
    retval.token = TOKEN_LITERAL_FLOAT;
    continue_parse_numeric_literal(retval, cur_input);
  }

  if (!consume_whitespace(cur_input)) return PARSE_FALSE;

  return retval;

  //static const ParseFn numeric_literals[] = { 
  //  parse_float_literal, 
  //  parse_integer_literal, 
  //  //TODO parse_hex_literal,
  //  //parse_octal_literal,
  //  //parse_binary_literal,
  //};
  //static const int num_literals = ARRAYSIZE(numeric_literals);

  //return parse_first_of(numeric_literals, num_literals, input);
}

GGToken parse_string_literal(const GGParseInput &input) {
  GGParseInput cur_input = input;
  if (*cur_input.data != '\"') return PARSE_FALSE;
  cur_input.data++;

  while(1){
    char c = *cur_input.data;
    if (c == 0) {
      return PARSE_FALSE;
    } else if (c == '\"') {
      GGToken retval = {};
      retval.token = TOKEN_LITERAL_STRING;
      retval.substring.start = input.data + 1;
      retval.substring.length = cur_input.data - (retval.substring.start);
      retval.next = cur_input.data + 1;
      retval.info = cur_input.info;
      return retval;
    } else if (c == '\\') {
      if (isDigit(cur_input.data[1])) {
        cur_input.data += 2;
        while (isDigit(*cur_input.data)) {
          cur_input.data++;
        }
      } else {
        cur_input.data++;
      }
    } else {
      cur_input.data++;
    }
  }

}

GGToken parse_literal(const GGParseInput &input)
{
  static const ParseFn literals[] = { 
    parse_numeric_literal, 
    parse_string_literal, 
    //TODO parse_char_literal, 
  };
  static const int num_literals = ARRAYSIZE(literals);
  return parse_first_of(literals, num_literals, input);
}

bool inline_parse_exact_discard(const char *str, GGToken *&token, int& num_remaining, GGParseInput &input) {
  const char *c = str;
  const char *&data = input.data;
  while(*c) {
    if (*c != *data) return false;
    c++;
    data++;
  }
  
  return true;
}

GGToken parse_exact(const char *str, GGTokenType tokenType, const GGParseInput &input) {
  const char *c = str;
  const char *data = input.data;
  while(*c) {
    if (*c != *data) return PARSE_FALSE;
    c++;
    data++;
  }

  GGToken result = {};
  result.token = tokenType;
  result.info = input.info;
  result.substring.start = input.data;
  result.substring.length = data - input.data;
  result.next = data;
  return result;
}

GGToken parse_semicolon(const GGParseInput &input) {
  return parse_exact(";", TOKEN_DISCARD, input);
}

GGToken parse_declaration_assignment_operator(const GGParseInput &input) {
  return parse_exact("=", TOKEN_DISCARD, input);
}


enum Associativity {
  LEFT_TO_RIGHT,
  RIGHT_TO_LEFT,
};

struct OperatorDef {
  const char *symbol;
  int precidence;
  Associativity associativity;
};

const OperatorDef POSTFIX_OPERATORS[] = {
  { "[", 2, LEFT_TO_RIGHT },   // array subscript []
  { "(", 2, LEFT_TO_RIGHT },   // function call   ()
  { ".",  2, LEFT_TO_RIGHT },   // field selection
  { "++", 2, LEFT_TO_RIGHT },   // post increment
  { "--", 2, LEFT_TO_RIGHT },   // post decrement
};

enum POSTFIX_OPERATOR_TYPE {
  ARRAY_INDEX_OP = 0,
  FUNCTION_CALL_OP = 1,
  MEMBER_OP = 2,
  POST_INC_OP = 3,
  POST_DEC_OP = 4,

  NUM_POSTFIX_OPERATORS,
};

const OperatorDef UNARY_OPERATORS[] = {
  { "++", 3, RIGHT_TO_LEFT},    // pre increment
  { "--", 3, RIGHT_TO_LEFT},    // pre decrement
  { "-",  3, RIGHT_TO_LEFT},    // unary minus
  { "+",  3, RIGHT_TO_LEFT},    // unary plus
  { "!",  3, RIGHT_TO_LEFT},    // logical NOT
  { "~",  3, RIGHT_TO_LEFT},    // bitwise NOT
  { "&",  3, RIGHT_TO_LEFT},    // address of
  { "*",  3, RIGHT_TO_LEFT},    // dereference
};
const int NUM_UNARY_OPERATORS = ARRAYSIZE(UNARY_OPERATORS);

const OperatorDef BINARY_OPERATORS[] = {
  //{ "()", 2, LEFT_TO_RIGHT },   // function call
  //{ "[]", 2, LEFT_TO_RIGHT },   // array subscript
  //{ ".",  2, LEFT_TO_RIGHT },   // field selection

  { "*",  5, LEFT_TO_RIGHT},    // multiplication
  { "/",  5, LEFT_TO_RIGHT},    // division
  { "%",  5, LEFT_TO_RIGHT},    // remainder

  { "+",  6, LEFT_TO_RIGHT},    // addition
  { "-",  6, LEFT_TO_RIGHT},    // subtraction

  { "<<", 7, LEFT_TO_RIGHT},    // bitwise left shift
  { ">>", 7, LEFT_TO_RIGHT},    // bitwise right shift

  { ">=", 8, LEFT_TO_RIGHT},    // gte comparison
  { "<=", 8, LEFT_TO_RIGHT},    // lte comparison
  { ">",  8, LEFT_TO_RIGHT},    // gt comparison
  { "<",  8, LEFT_TO_RIGHT},    // lt comparison

  { "==", 9, LEFT_TO_RIGHT},    // eq comparison
  { "!=", 9, LEFT_TO_RIGHT},    // neq comparison

  { "&",  10, LEFT_TO_RIGHT},   // bitwise and
  { "^",  11, LEFT_TO_RIGHT},   // bitwise xor
  { "|",  12, LEFT_TO_RIGHT},   // bitwise or
  { "&&", 13, LEFT_TO_RIGHT},   // boolean and
  { "||", 14, LEFT_TO_RIGHT},   // boolean or
};
const int NUM_BINARY_OPERATORS = ARRAYSIZE(BINARY_OPERATORS);


const OperatorDef ASSIGNMENT_OPERATORS[] = {
  { "=",  16, RIGHT_TO_LEFT},   // assignment
  { "+=", 16, RIGHT_TO_LEFT},   // compound assignment
  { "-=", 16, RIGHT_TO_LEFT},   // 
  { "*=", 16, RIGHT_TO_LEFT},   // 
  { "/=", 16, RIGHT_TO_LEFT},   // 
  { "%=", 16, RIGHT_TO_LEFT},   // 
  { "|=", 16, RIGHT_TO_LEFT},   // 
  { "&=", 16, RIGHT_TO_LEFT},   // 
  { "^=", 16, RIGHT_TO_LEFT},   // 
};
const int NUM_ASSIGNMENT_OPERATORS = ARRAYSIZE(ASSIGNMENT_OPERATORS);

//string
//OSString 
//DisplayString 
//
//
//struct String
//{
//  u8  *data;
//  i32 string_length;
//  i32 allocation_size;
//  i32 refcount;
//};
//
//String(string_literal s) {
//  String retval;
//  retval.data = s;
//  retval.string_length = s.length;
//  retval.allocation_size = 0;
//}
//
//char[20]        str;
//Rc<char*>       str;
//char[]<'static> str;


GGToken parse_expression(const GGParseInput &input);
GGToken parse_type_declaration(const GGParseInput &input);

GGToken parse_function_param(const GGParseInput &input) {
  static const ParseFn sequence[] = { parse_type_declaration, parse_variable_identifier };
  static const int num_sequence = ARRAYSIZE(sequence);
  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_FUNCTION_PARAM, input);
}

GGToken parse_zero_or_more_separated(ParseFn fn, char separator, GGTokenType token_type, const GGParseInput &input) {
  GGParseInput cur_input = input;
  GGToken total_output = ParseOutputAlloc(token_type, 1024);

  while(1) {
    {
      GGToken output = fn(cur_input);
      if (ParseOuputIsFalse(output)) {
        break;
      } else {
        ParseOutputAppend(total_output, output);
        cur_input.data = output.next;
        cur_input.info = output.info;
      }
    }

    {
      while(1) {
        GGToken discard = parse_nonsyntax_tokens(cur_input);
        if (ParseOuputIsFalse(discard)) {
          break;
        } else if (discard.token == TOKEN_EOF) {
          //ParseOutputAppend(total_output, output);
          break;
        } else if (discard.token == TOKEN_END_OF_LINE) {
          //ParseOutputAppend(total_output, output);
          cur_input.data = discard.next;
          cur_input.info.line_number = discard.info.line_number + 1;
          cur_input.info.col_number = 0;
        } else {
          // discard
          cur_input.data = discard.next;
          cur_input.info = discard.info;
        }
      }
    }

    {
      const char str[2] = {separator, 0};
      GGToken discard = parse_exact(str, TOKEN_DISCARD, cur_input);
      if (ParseOuputIsFalse(discard)) {
        break;
      } else if (discard.token == TOKEN_EOF) {
        break;
      } else if (discard.token == TOKEN_END_OF_LINE) {
        //ParseOutputAppend(total_output, output);
        cur_input.data = discard.next;
        cur_input.info.line_number = discard.info.line_number + 1;
        cur_input.info.col_number = 0;
      } else {
        // discard
        cur_input.data = discard.next;
        cur_input.info = discard.info;
      }
    }

    {
      while(1) {
        GGToken discard = parse_nonsyntax_tokens(cur_input);
        if (ParseOuputIsFalse(discard)) {
          break;
        } else if (discard.token == TOKEN_EOF) {
          //ParseOutputAppend(total_output, output);
          break;
        } else if (discard.token == TOKEN_END_OF_LINE) {
          //ParseOutputAppend(total_output, output);
          cur_input.data = discard.next;
          cur_input.info.line_number = discard.info.line_number + 1;
          cur_input.info.col_number = 0;
        } else {
          // discard
          cur_input.data = discard.next;
          cur_input.info = discard.info;
        }
      }
    }
  }

  total_output.info = cur_input.info;
  total_output.next = cur_input.data;
  return total_output;
}

GGToken parse_function_call_params(const GGParseInput &input) {
  return parse_zero_or_more_separated(parse_expression, ',', TOKEN_COMPOUND_FUNCTION_CALL_PARAMS, input);
}

GGToken parse_for_exact(const GGParseInput &input) {
  return parse_exact("for", TOKEN_DISCARD, input);
}

GGToken parse_in_exact(const GGParseInput &input) {
  return parse_exact("in", TOKEN_DISCARD, input);
}

GGToken parse_if_exact(const GGParseInput &input) {
  return parse_exact("if", TOKEN_DISCARD, input);
}

GGToken parse_else_exact(const GGParseInput &input) {
  return parse_exact("else", TOKEN_DISCARD, input);
}

GGToken parse_left_bracket(const GGParseInput &input) {
  return parse_exact("[", TOKEN_DISCARD, input);
}

GGToken parse_right_bracket(const GGParseInput &input) {
  return parse_exact("]", TOKEN_DISCARD, input);
}

GGToken parse_left_paren(const GGParseInput &input) {
  return parse_exact("(", TOKEN_DISCARD, input);
}

GGToken parse_right_paren(const GGParseInput &input) {
  return parse_exact(")", TOKEN_DISCARD, input);
}

GGToken parse_left_brace(const GGParseInput &input) {
  return parse_exact("{", TOKEN_DISCARD, input);
}

GGToken parse_right_brace(const GGParseInput &input) {
  return parse_exact("}", TOKEN_DISCARD, input);
}

GGToken parse_llvm_exact(const GGParseInput &input) {
  return parse_exact("llvm", TOKEN_DISCARD, input);
}

//GGToken parse_function_call(const GGParseInput &input) {
//  static const ParseFn sequence[] = {parse_identifier, parse_left_paren, parse_function_call_params, parse_right_paren};
//  static const int num_sequence = ARRAYSIZE(sequence);
//
//  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_FUNCTION_CALL, input);
//}

GGToken parse_atomic_expression(const GGParseInput &input) {
  // aexpr :
  //    literal
  //	variable_identifier
  static const ParseFn expressions[] = {
    parse_literal, 
    parse_variable_identifier,
    //parse_function_call,
    //parse_paren_expression,
  };
  static const int num_expressions = ARRAYSIZE(expressions);

  return parse_first_of(expressions, num_expressions, input);
}

//GGToken parse_unary_expression(const GGParseInput &input) {
//  // uexpr : uop aexpr
//  GGToken uop = parse_unary_operator(cur_input);
//
//  // whitespace
//
//  if (ParseOutputIsFalse(uop)) {
//	  GGToken aexp = parse_unary_expression(input);
//	  // whitespace
//	  return aexp
//  } 
//
//}
//
//parse_primary_expression() {
//	ParseFirstOf(parse_unary_expression, parse_atomic_expression)
//}

GGToken parse_op_symbol(const GGParseInput &input, GGTokenType token, const OperatorDef *ops, int num_ops) {
  for(int i = 0; i < num_ops; ++i) {
    const OperatorDef &opdef = ops[i];
    int len = strlen(opdef.symbol);
    if (strncmp(opdef.symbol, input.data, len) == 0) {
      GGToken retval = {};
      retval.info = input.info;
      retval.token = token;
      retval.num_subtokens = i;
      retval.substring.start = input.data;
      retval.substring.length = len;
      retval.next = input.data + len;
      return retval;
    }
  }

  return PARSE_FALSE;
  //return parse_one_or_more_pred(isBinaryOperator, TOKEN_BINARY_OPERATOR, input);
};

GGToken parse_postfix_op_symbol(const GGParseInput &input) {
  return parse_op_symbol(input, TOKEN_POSTFIX_OPERATOR, POSTFIX_OPERATORS, NUM_POSTFIX_OPERATORS);
}


GGToken parse_binary_op_symbol(const GGParseInput &input) {
  return parse_op_symbol(input, TOKEN_BINARY_OPERATOR, BINARY_OPERATORS, NUM_BINARY_OPERATORS);
}

GGToken parse_unary_op_symbol(const GGParseInput &input) {
  return parse_op_symbol(input, TOKEN_UNARY_OPERATOR, UNARY_OPERATORS, NUM_UNARY_OPERATORS);
}

GGToken parse_assignment_op_symbol(const GGParseInput &input) {
  return parse_op_symbol(input, TOKEN_ASSIGNMENT_OPERATOR, ASSIGNMENT_OPERATORS, NUM_ASSIGNMENT_OPERATORS);
}



//bool isBinaryOperator(char c) {
//  //const char *operator_symbols =  "+-*/%"   // arithmatic
//  //  "&|"      // boolean
//  //  "<>=!";   // comparison
//
//  //return strchr(operator_symbols, c) != NULL;
//}

int binary_operator_get_precedence(const GGToken &op) {
  return BINARY_OPERATORS[op.num_subtokens].precidence;
}

GGToken parse_unary_expression(const GGParseInput &input);

GGToken parse_binary_operation(int minimum_precidence, const GGToken &lhs_input, const GGParseInput &input) {
  GGParseInput cur_input = input;
  GGToken LHS = lhs_input;

  while(1) {
    GGToken op = parse_binary_op_symbol(cur_input);
    if (ParseOuputIsFalse(op)) {
      return LHS;
    }

    int op_precedence = binary_operator_get_precedence(op);
    if (op_precedence < minimum_precidence) {
      return LHS;
    }

    cur_input.data = op.next;
    cur_input.info = op.info;

    // whitespace
    if (!consume_whitespace(cur_input)) return PARSE_FALSE;

    GGToken rhs = parse_unary_expression(cur_input);
    if (ParseOuputIsFalse(rhs)) {
      return PARSE_FALSE;
    }

    cur_input.data = rhs.next;
    cur_input.info = rhs.info;

    // whitespace
    if (!consume_whitespace(cur_input)) return PARSE_FALSE;

    GGToken next_op = parse_binary_op_symbol(cur_input);
    if (!ParseOuputIsFalse(next_op)) {
      int next_op_precedence = binary_operator_get_precedence(next_op);

      if (op_precedence < next_op_precedence) {
        GGToken rhs = parse_binary_operation(op_precedence + 1, rhs, cur_input);
        if (ParseOuputIsFalse(rhs)) {
          return PARSE_FALSE;
        }

        cur_input.data = rhs.next;
        cur_input.info = rhs.info;

        // whitespace
        if (!consume_whitespace(cur_input)) return PARSE_FALSE;
      }
    }

    GGToken newLHS = ParseOutputAlloc(TOKEN_COMPOUND_BINARY_OPERATION, 3);
    newLHS.subtokens[0] = LHS;
    newLHS.subtokens[1] = op;
    newLHS.subtokens[2] = rhs;
    newLHS.num_subtokens = 3;
    newLHS.next = cur_input.data;
    newLHS.info = cur_input.info;
    LHS = newLHS;
  }
}

bool consume_whitespace_and_terminator(char c, GGParseInput &cur_input) {
  while(1) {
    if (*cur_input.data == c) {
      cur_input.data++;
      return true;
    }

    GGToken discard = parse_nonsyntax_tokens(cur_input);
    if (ParseOuputIsFalse(discard)) {
      return false;
    } else if (discard.token == TOKEN_EOF) {
      //ParseOutputAppend(total_output, output);
      return false;
    } else if (discard.token == TOKEN_END_OF_LINE) {
      //ParseOutputAppend(total_output, output);
      cur_input.data = discard.next;
      cur_input.info.line_number = discard.info.line_number + 1;
      cur_input.info.col_number = 0;
    } else {
      // discard
      cur_input.data = discard.next;
      cur_input.info = discard.info;
    }
  }
}

GGToken parse_postfix_expression(const GGParseInput &input) {
  GGParseInput cur_input = input;
  GGToken lhs = parse_atomic_expression(cur_input);
  if (ParseOuputIsFalse(lhs)) {
    return PARSE_FALSE;
  }

  // whitespace
  cur_input.data = lhs.next;
  cur_input.info = lhs.info;
  if (!consume_whitespace(cur_input)) return PARSE_FALSE;

  while(1) {
    GGToken op = parse_postfix_op_symbol(cur_input);
    if (ParseOuputIsFalse(op)) {
      return lhs;
    } 

    // whitespace
    cur_input.data = op.next;
    cur_input.info = op.info;
    if (!consume_whitespace(cur_input)) return PARSE_FALSE;

    GGToken newLHS = ParseOutputAlloc(TOKEN_COMPOUND, 2);
    newLHS.info = op.info;
    newLHS.next = op.next;

    switch(op.num_subtokens) {
    case ARRAY_INDEX_OP: {
      GGToken array_expression = parse_expression(cur_input);
      if (ParseOuputIsFalse(op)) {
        return PARSE_FALSE;
      } 

      newLHS.token = TOKEN_COMPOUND_ARRAY_INDEX;
      newLHS.subtokens[0] = lhs;
      newLHS.subtokens[1] = array_expression;
      newLHS.num_subtokens = 2;

      cur_input.data = array_expression.next;
      cur_input.info = array_expression.info;
      if (!consume_whitespace_and_terminator(']', cur_input)) return PARSE_FALSE;
                         } break;
    case FUNCTION_CALL_OP: {
      GGToken function_params = parse_function_call_params(cur_input);
      if (ParseOuputIsFalse(op)) {
        return PARSE_FALSE;
      } 
      newLHS.token = TOKEN_COMPOUND_FUNCTION_CALL;
      newLHS.subtokens[0] = lhs;
      newLHS.subtokens[1] = function_params;
      newLHS.num_subtokens = 2;

      cur_input.data = function_params.next;
      cur_input.info = function_params.info;
      if (!consume_whitespace_and_terminator(')', cur_input)) return PARSE_FALSE;
                           } break;
    case MEMBER_OP: {
      GGToken member = parse_member_identifier(cur_input);
      if (ParseOuputIsFalse(op)) {
        return PARSE_FALSE;
      } 
      newLHS.token = TOKEN_COMPOUND_MEMBER_IDENTIFIER;
      newLHS.subtokens[0] = lhs;  // fn identifier
      newLHS.subtokens[1] = member;
      newLHS.num_subtokens = 2;

      cur_input.data = member.next;
      cur_input.info = member.info;
                    } break;
    case POST_INC_OP:
    case POST_DEC_OP: {
      newLHS.token = TOKEN_COMPOUND_UNARY_POST_OPERATION;
      newLHS.subtokens[0] = lhs;
      newLHS.subtokens[1] = op;
      newLHS.num_subtokens = 2;
                      } break;
    default:
      halt();
    }

    newLHS.info = cur_input.info;
    newLHS.next = cur_input.data;
    lhs = newLHS;

    //// whitespace
    //cur_input.data = op.next;
    //cur_input.info = op.info;
    //if (!consume_whitespace(cur_input)) return PARSE_FALSE;
  }
}

GGToken parse_unary_expression(const GGParseInput &input) {
  GGParseInput cur_input = input;
  GGToken op = parse_unary_op_symbol(cur_input);
  if (ParseOuputIsFalse(op)) {
    GGToken expr = parse_postfix_expression(cur_input);
    if (ParseOuputIsFalse(expr)) {
      return PARSE_FALSE;
    }
    return expr;
  }

  // whitespace
  cur_input.data = op.next;
  cur_input.info = op.info;
  if (!consume_whitespace(cur_input)) return PARSE_FALSE;

  GGToken sub_expression = parse_unary_expression(cur_input);
  if (ParseOuputIsFalse(sub_expression)) {
    return PARSE_FALSE;
  }

  GGToken retval = ParseOutputAlloc(TOKEN_COMPOUND_UNARY_OPERATION, 2);
  retval.subtokens[0] = op;
  retval.subtokens[1] = sub_expression;
  retval.num_subtokens = 2;
  retval.next = sub_expression.next;
  retval.info = sub_expression.info;
  return retval;
}

//GGToken parse_unary_expression(const GGParseInput &input) {
//  GGParseInput cur_input = input;
//  GGToken op = parse_unary_op_symbol(cur_input);
//  if (ParseOuputIsFalse(op)) {
//      return PARSE_FALSE;
//  }
//
//
//  while(1) {
//	GGToken next_op = parse_unary_op_symbol(cur_input);
//	if (ParseOuputIsFalse(next_op)) {
//		GGToken expr = parse_atomic_expression(cur_input);
//		if (ParseOuputIsFalse(expr)) {
//			return PARSE_FALSE;
//		}
//
//		GGToken newLHS = ParseOutputAlloc(TOKEN_COMPOUND_BINARY_OPERATION, 2);
//		newLHS.subtokens[0] = op;
//		newLHS.subtokens[1] = expr;
//		newLHS.next = cur_input.data;
//		newLHS.info = cur_input.info;
//		return newLHS;
//	} else {
//      int op_precedence = operator_get_precedence(op);
//	  int next_op_precedence = operator_get_precedence(next_op);
//      if (op_precedence < next_op_precedence) {
//        //GGToken rhs = parse_binary_operation(op_precedence + 1, rhs, cur_input);
//        //if (ParseOuputIsFalse(rhs)) {
//	  }	else {
//		GGToken newLHS = ParseOutputAlloc(TOKEN_COMPOUND_BINARY_OPERATION, 2);
//		newLHS.subtokens[0] = op;
//		newLHS.subtokens[1] = expr;
//		newLHS.next = cur_input.data;
//		newLHS.info = cur_input.info;
//		return newLHS;
//	  }
//	}
//
//
//
//
//    GGToken rhs = parse_atomic_expression(cur_input);
//    if (ParseOuputIsFalse(rhs)) {
//      return PARSE_FALSE;
//    }
//
//    cur_input.data = rhs.next;
//    cur_input.info = rhs.info;
//
//    // whitespace
//    if (!consume_whitespace(cur_input)) return PARSE_FALSE;
//
//    if (!ParseOuputIsFalse(op)) {
//
//          return PARSE_FALSE;
//        }
//
//        cur_input.data = rhs.next;
//        cur_input.info = rhs.info;
//
//        // whitespace
//        if (!consume_whitespace(cur_input)) return PARSE_FALSE;
//      }
//    }
//
//    GGToken newLHS = ParseOutputAlloc(TOKEN_COMPOUND_BINARY_OPERATION, 3);
//    newLHS.subtokens[0] = LHS;
//    newLHS.subtokens[1] = op;
//    newLHS.subtokens[2] = rhs;
//    newLHS.next = cur_input.data;
//    newLHS.info = cur_input.info;
//    LHS = newLHS;
//  }
//}



GGToken parse_expression(const GGParseInput &input) {
  //static const ParseFn parse_aexrps[] = {parse_atomic_expression, parse_unary_expression};
  //static const int numFns = ARRAYSIZE(parse_aexrps);
  //GGToken lhs = parse_first_of(parse_aexrps, numFns, input);
  GGToken lhs = parse_unary_expression(input);
  if (ParseOuputIsFalse(lhs)) 
  {
    return PARSE_FALSE;
  }

  // whitespace
  GGParseInput cur_input;
  cur_input.data = lhs.next;
  cur_input.info = lhs.info;
  if (!consume_whitespace(cur_input)) return PARSE_FALSE;

  return parse_binary_operation(0, lhs, cur_input);
}

//GGToken parse_expression(lhs, int min_precedence) {
//
//  while(1) {
//    next_token = 
//    next_token_precidence = 
//    rhs = parse_primary();
//    while (1) {
//      next_next_token = 
//      next_next_token_precidence = 
//      rhs = parse_expression(rhs, next_token_precidence);
//    }
//  }
//}


//GGToken parse_binary_operation(const GGParseInput &input) {
//  static const ParseFn sequence[] = { parse_expression, parse_binary_op_symbol, parse_expression};
//  static const int num_sequence = ARRAYSIZE(sequence);
//  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_BINARY_OPERATION, input);
//}

//GGToken parse_expression(const GGParseInput &input) {
//  // expr :
//  //		literal
//  //		unary_operation
//  //		binary_operation
//  //		function_call
//  //		variable_identifier
//  //		paren_expression
//  static const ParseFn expressions[] = {
//    parse_literal, 
//    //parse_unary_op, 
//    parse_binary_op, 
//    //parse_function_call, 
//    parse_variable_identifier,
//    //parse_paren_expression,
//  };
//  static const int num_expressions = ARRAYSIZE(expressions);
//
//  return parse_first_of(expressions, num_expressions, input);
//}


//GGToken parse_zero_or_more_terminated(ParseFn fn, char separator, GGTokenType token_type, const GGParseInput &input) {
//  return parse_zero_or_more_separated(fn, separator, token_type, input);
//}

GGToken parse_zero_or_more(ParseFn fn, GGTokenType token_type, const GGParseInput &input) {
  GGParseInput cur_input = input;
  //GGToken total_output = {};
  GGToken total_output = ParseOutputAlloc(token_type, 1024);

  while(1) {
    GGToken output = fn(cur_input);
    if (ParseOuputIsFalse(output)) {
      break;
    } else if (output.token == TOKEN_EOF) {
      //ParseOutputAppend(total_output, output);
      break;
    } else if (output.token == TOKEN_END_OF_LINE) {
      //ParseOutputAppend(total_output, output);
      cur_input.data = output.next;
      cur_input.info.line_number = output.info.line_number + 1;
      cur_input.info.col_number = 0;
    } else {
      ParseOutputAppend(total_output, output);
      cur_input.data = output.next;
      cur_input.info = output.info;
    }

    {
      while(1) {
        GGToken discard = parse_nonsyntax_tokens(cur_input);
        if (ParseOuputIsFalse(discard)) {
          break;
        } else if (discard.token == TOKEN_EOF) {
          //ParseOutputAppend(total_output, output);
          break;
        } else if (discard.token == TOKEN_END_OF_LINE) {
          //ParseOutputAppend(total_output, output);
          cur_input.data = discard.next;
          cur_input.info.line_number = discard.info.line_number + 1;
          cur_input.info.col_number = 0;
        } else {
          // discard
          cur_input.data = discard.next;
          cur_input.info = discard.info;
        }
      }
    }
  }

  total_output.info = cur_input.info;
  total_output.next = cur_input.data;
  return total_output;
}

GGToken parse_function_params(const GGParseInput &input) {
  return parse_zero_or_more_separated(parse_function_param, ',', TOKEN_COMPOUND_FUNCTION_PARAMS, input);
}

//GGParseOutput &parse_function_identifier(const ParseInput &input) {
//}
//
//GGParseOutput &parse_variable_identifier(const ParseInput &input) {
//}
//
//GGParseOutput &parse_variable_identifier(const ParseInput &input) {
//}

bool isRightBrace(char c) {
  return c == '}';
}

GGToken parse_anything_until_right_brace(const GGParseInput &input, GGTokenType token_type) {
  return parse_zero_or_more_until_pred(isRightBrace, token_type, input);
}

GGToken parse_llvm_content(const GGParseInput &input)  {
  return parse_anything_until_right_brace(input, TOKEN_LLVM_CONTENT);
};

GGToken parse_inline_llvm(const GGParseInput &input) {
  static const ParseFn sequence[] = { parse_llvm_exact, parse_left_brace, parse_llvm_content, parse_right_brace, };
  static const int num_sequence = ARRAYSIZE(sequence);
  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_INLINE_LLVM, input);
}

//GGToken parse_variable(const GGParseInput &input) {
//  // identifier := expression;
//  static const ParseFn sequence[] = {parse_identifier, parse_declaration_assignment_operator, parse_expression};
//  static const int num_sequence = ARRAYSIZE(sequence);
//
//  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_VARIABLE, input);
//};

GGToken parse_typedef_exact(const GGParseInput &input) {
  return parse_exact("typedef", TOKEN_DISCARD, input);
}

GGToken parse_llvm_type_exact(const GGParseInput &input) {
  return parse_exact("llvm_type", TOKEN_DISCARD, input);
}

GGToken parse_struct_exact(const GGParseInput &input) {
  return parse_exact("struct", TOKEN_DISCARD, input);
}

GGToken parse_enum_exact(const GGParseInput &input) {
  return parse_exact("enum", TOKEN_DISCARD, input);
}

GGToken parse_extern_exact(const GGParseInput &input) {
  return parse_exact("extern", TOKEN_DISCARD, input);
}

GGToken parse_import_exact(const GGParseInput &input) {
  return parse_exact("import", TOKEN_DISCARD, input);
}

bool inline_parse_import_exact(GGToken *&token, int& num_remaining, GGParseInput &input) {
  return inline_parse_exact_discard("import", token, num_remaining, input);
}

bool inline_parse_semicolon(GGToken *&token, int& num_remaining, GGParseInput &input) {
  return inline_parse_exact_discard(";", token, num_remaining, input);
}


//T[,,] = tuple(T*, int size1, int stride1, int size2, int stride2, int size3)
//  T[,5] = tuple(T*, int size1) // stride = 5*sizeof(T)
//  T[5,] = tuple(T*, int stride1, int size2)
//  T[5] = tuple(T*)
//  T[5,5] = tuple(T*)
//rule:
//removing the first element adds a size
//  removing any other element adds a size for that element and a stride for the previous elements
//  er, element

GGToken parse_constexpr(const GGParseInput &input) {
  return parse_integer_literal(input);
}

GGToken parse_param_type_list(const GGParseInput &input) {
  return parse_zero_or_more_separated(parse_type_declaration, ',', TOKEN_COMPOUND_PARAM_TYPE_LIST, input); 
}

//GGToken parse_base_type_declaration(const GGParseInput &input) {
//}

//GGToken parse_anonymous_struct(const GGParseInput &input) {}
//GGToken parse_anonymous_enum(const GGParseInput &input) {}
//GGToken parse_anonymous_llvm(const GGParseInput &input) {}

GGToken parse_type_declaration(const GGParseInput &input) {
  // struct
  // enum
  // llvm_type
  // identifier

  const static ParseFn fns[] = { 
    //parse_anonymous_struct, 
    //parse_anonymous_enum, 
    //parse_anonymous_llvm_type, 
    parse_type_identifier 
  };
  const static int num_fns = ARRAYSIZE(fns);

  GGParseInput cur_input = input;
  GGToken lhs = parse_first_of(fns, num_fns, cur_input);
  if (ParseOuputIsFalse(lhs)) {
    return PARSE_FALSE;
  } 

  //lhs.token = TOKEN_TYPE_IDENTIFIER;
  cur_input.data = lhs.next;
  cur_input.info = lhs.info;
  if (!consume_whitespace(cur_input)) return PARSE_FALSE;

  bool hadALeftParen = false;
  while(1)
  {
    //const static ParseFn fns[] = { parse_asterix, parse_left_bracket, parse_left_paren };
    //   const static int num_fns = ARRAYSIZE(fns);
    //   GGOutput op = parse_one_of(fns, num_fns);
    //   if (op is false) {
    //  return lhs;
    //   }

    char c = *cur_input.data;
    switch(c) {
    case '*': {
      //GGOutput pointer_symbol = parse_pointer_symbol();
      GGToken newLhs = ParseOutputAlloc(TOKEN_COMPOUND_POINTER_TYPE, 1);
      cur_input.data++;
      newLhs.subtokens[0] = lhs;
      newLhs.num_subtokens = 1;
      newLhs.info = lhs.info;
      newLhs.next = cur_input.data;
      lhs = newLhs;
              } break;
    case '[': {
      static const ParseFn sequence[] = { parse_left_bracket, parse_constexpr, parse_right_bracket };
      static const int num_sequence = ARRAYSIZE(sequence);
      GGToken array_subscript = parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND, cur_input);
      if (ParseOuputIsFalse(array_subscript )) return PARSE_FALSE;
      GGToken newLhs = ParseOutputAlloc(TOKEN_COMPOUND_ARRAY_TYPE, 2);
      newLhs.subtokens[0] = lhs;
      newLhs.subtokens[1] = array_subscript.subtokens[0];
      newLhs.num_subtokens = 2;
      cur_input.data = array_subscript.next;
      cur_input.info = array_subscript.info;
      newLhs.next = array_subscript.next;
      newLhs.info = array_subscript.info;
      lhs = newLhs;
              } break;
    case '(': {
      if (hadALeftParen == true) {
        return PARSE_FALSE;
      }
      hadALeftParen = true;
      static const ParseFn sequence[] = { parse_left_paren, parse_param_type_list, parse_right_paren };
      static const int num_sequence = ARRAYSIZE(sequence);
      GGToken param_type_list = parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND, cur_input);
      if (ParseOuputIsFalse(param_type_list)) return PARSE_FALSE;
      GGToken newLhs = ParseOutputAlloc(TOKEN_COMPOUND_FUNCTION_TYPE, 2);
      newLhs.subtokens[0] = lhs;
      newLhs.subtokens[1] = param_type_list.subtokens[0];
      newLhs.num_subtokens = 2;
      cur_input.data = param_type_list.next;
      cur_input.info = param_type_list.info;
      newLhs.next = param_type_list.next;
      newLhs.info = param_type_list.info;
      lhs = newLhs;
              } break;
    default:
      return lhs;
    }

    if (!consume_whitespace(cur_input)) return PARSE_FALSE;
  }

}

//() x = 10;

GGToken parse_typedef_definition(const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_typedef_exact, parse_type_identifier, parse_declaration_assignment_operator, parse_type_declaration};
  static const int num_sequence = ARRAYSIZE(sequence);
  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_TYPEDEF_DEFINITION, input);
}

GGToken parse_struct_field(const GGParseInput &input) {
  static const ParseFn declaration_part_sequence[] = { parse_type_declaration, parse_variable_identifier };
  static const int num_declaration_part = ARRAYSIZE(declaration_part_sequence);

  GGToken declaration_token = ParseOutputAlloc(TOKEN_COMPOUND_VARIABLE_DEFINITION, 3);
  continue_parse_whitespace_separated_sequence(declaration_token, declaration_part_sequence, num_declaration_part, input);
  if (ParseOuputIsFalse(declaration_token)) {
    return PARSE_FALSE;
  }

  GGParseInput cur_input;
  cur_input.data = declaration_token.next;
  cur_input.info = declaration_token.info;
  consume_whitespace(cur_input);

  if (*cur_input.data == ';') {
    declaration_token.next = cur_input.data + 1;
  } else {
    static const ParseFn assigment_part_sequence[] = { parse_declaration_assignment_operator, parse_expression, parse_semicolon };
    static const int num_assigment_part = ARRAYSIZE(assigment_part_sequence);
    continue_parse_whitespace_separated_sequence(declaration_token, assigment_part_sequence, num_assigment_part, cur_input);
    if (ParseOuputIsFalse(declaration_token)) {
      return PARSE_FALSE;
    }
  }

  cur_input.data = declaration_token.next;
  cur_input.info = declaration_token.info;
  consume_whitespace(cur_input);

  declaration_token.next = cur_input.data;
  return declaration_token;
}

GGToken parse_variable_definition(const GGParseInput &input) {
  return parse_struct_field(input);

  // identifier := expression;
  //static const ParseFn sequence[] = {parse_type_declaration, parse_variable_identifier, parse_declaration_assignment_operator, parse_expression, parse_semicolon};
  //static const int num_sequence = ARRAYSIZE(sequence);
  //return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_VARIABLE_DEFINITION, input);
};



GGToken parse_struct_fields(const GGParseInput &input) {
  return parse_zero_or_more(parse_struct_field, TOKEN_COMPOUND_STRUCT_FIELDS, input);
}

GGToken parse_enum_field(const GGParseInput &input) {
  return parse_variable_definition(input);
}

GGToken parse_enum_fields(const GGParseInput &input) {
  return parse_zero_or_more(parse_enum_field, TOKEN_COMPOUND_ENUM_FIELDS, input);
}

//GGToken parse_raw_llvm(const GGParseInput &input) {
//  parse_until
//};

GGToken parse_llvm_type_definition(const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_llvm_type_exact, parse_type_identifier, parse_left_brace, parse_llvm_content, parse_right_brace};
  static const int num_sequence = ARRAYSIZE(sequence);
  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_LLVM_TYPE_DEFINITION, input);
}

GGToken parse_struct_definition(const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_struct_exact, parse_type_identifier, parse_left_brace, parse_struct_fields, parse_right_brace};
  static const int num_sequence = ARRAYSIZE(sequence);
  GGToken retval = parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_STRUCT_DEFINITION, input);
  return retval;
}

GGToken parse_enum_definition(const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_enum_exact, parse_type_identifier, parse_left_brace, parse_enum_fields, parse_right_brace};
  static const int num_sequence = ARRAYSIZE(sequence);
  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_ENUM_DEFINITION, input);
}

bool inline_parse_1_to_3_strings(GGToken *&token, int &remainingTokens, GGParseInput &cur_input) {
  assert(remainingTokens >= 3);

  int num_strings = 0;
  for(int i = 0; i < 3; ++i) {
    GGToken &subtoken = *token++;
    subtoken = parse_string_literal(cur_input);
    if (ParseOuputIsFalse(subtoken)) {
      break;
    } else {
      num_strings++;
      cur_input.info = subtoken.info;
      cur_input.data = subtoken.next;
      if (consume_whitespace(cur_input) == false) break;
    }
  }

  remainingTokens -= num_strings;
  return num_strings != 0;
}

typedef bool (*ParseFn2)(GGToken *&token, int &num_remaining, GGParseInput &);

bool inline_parse_whitespace_separated_sequence(const ParseFn2 *parse_fns, int num_functions, GGToken *&token, int &remainingTokens, GGParseInput &cur_input) {
  for(int i = 0; i < num_functions; ++i) {
    bool result = parse_fns[i](token, remainingTokens, cur_input);
    if (result == false) return false;
    if (consume_whitespace(cur_input) == false) return false;
  }

  return true;
}

const int MAX_FILES = 1024;
const int MAX_STACK = 1024;
struct FileList {
  GGSubString directory_stack[MAX_STACK];
  int stack_depth;

  GGSubString files[MAX_FILES];
  int num_files;
} gFileList;

bool substring_cmp(const GGSubString &substring, const GGSubString &substring0);

bool FileListLookup(const FileList &files, const GGSubString &new_file) {
  for(int i = 0; i < files.num_files; ++i) {
    if (substring_cmp(files.files[i], new_file)) return true;
  }

  return false;
}

const int MAX_FILENAME = 1024;

GGSubString filepath_concat(const GGSubString &dir, const GGSubString &file) {
  if (dir.length == 0) {
    return file;
  }
  
  GGSubString retval;
  retval.length = dir.length + file.length;
  char *new_str = (char *)malloc(retval.length * sizeof(char));
  strncpy(new_str, dir.start, dir.length);
  strncpy(new_str + dir.length, file.start, file.length);
  retval.start = new_str;

  return retval;
}

void MakeFileName(const FileList &files, const GGSubString &file, GGSubString &project_relative_filename) {
  if (files.stack_depth == 0) {
    project_relative_filename = file;
    return;
  }

  const GGSubString &dir = files.directory_stack[files.stack_depth-1];
  project_relative_filename = filepath_concat(dir, file);
}

void FileListAdd(FileList &files, const GGSubString &new_file) {
  GGSubString project_relative_filename;
  MakeFileName(files, new_file, project_relative_filename);

  if (FileListLookup(files, project_relative_filename) == true) return;

  files.files[files.num_files++] = project_relative_filename;
}

void FileListAdd(FileList &files, const char *new_file) {
  GGSubString substring;
  substring.start = new_file;
  substring.length = strlen(new_file);
  FileListAdd(files, substring);
}

void emit_import_statement(FileList &list, const GGToken &import_statement) {
  assert(import_statement.num_subtokens >= 1);
  assert(import_statement.num_subtokens <= 3);

  switch(import_statement.num_subtokens) {
  case 1:
    FileListAdd(list, import_statement.subtokens[0].substring);
    break;
  case 2:
  case 3:
    // TODO
  default:
    halt();
  }
}


GGToken parse_import_statement(const GGParseInput &input) {
  // identifier := expression;
  
  static const ParseFn2 statements[] = {
    inline_parse_import_exact,
    inline_parse_1_to_3_strings,
    inline_parse_semicolon,
  };
  static const int num_statements = ARRAYSIZE(statements);

  GGToken retval = ParseOutputAlloc(TOKEN_COMPOUND_IMPORT_STATEMENT, 3);
  int num_remaining = 3;
  GGParseInput cur_input = input;
  GGToken *subtokens = retval.subtokens;
  bool result = inline_parse_whitespace_separated_sequence(statements, num_statements, subtokens, num_remaining, cur_input);
  if (result == false) {
    ParseOutputFree(retval);
    return PARSE_FALSE;
  }

  retval.num_subtokens = 3 - num_remaining;
  retval.info = input.info;
  retval.next = cur_input.data;

  emit_import_statement(gFileList, retval);

  return retval;
};

GGToken parse_extern_function_declaration(const GGParseInput &input) {
  // identifier := expression;
  static const ParseFn statements[] = {
    parse_extern_exact,
    parse_type_declaration,
    parse_function_identifier,
    parse_left_paren,
    parse_function_params,
    parse_right_paren,
    parse_semicolon,
  };
  static const int num_statements = ARRAYSIZE(statements);
  GGToken retval = parse_whitespace_separated_sequence(statements, num_statements, TOKEN_COMPOUND_EXTERNAL_FUNCTION_DECLARATION, input);
  return retval;
};

GGToken parse_type_definition(const GGParseInput &input) {
  // identifier := expression;
  static const ParseFn statements[] = {
    parse_typedef_definition,
    parse_struct_definition,
    parse_enum_definition,
    parse_llvm_type_definition,
  };
  static const int num_statements = ARRAYSIZE(statements);
  return parse_first_of(statements, num_statements, input);
};

GGToken parse_return_exact(const GGParseInput &input) {
  return parse_exact("return", TOKEN_DISCARD, input);
}

GGToken parse_return_statement(const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_return_exact, parse_expression, parse_semicolon};
  static const int num_sequence = ARRAYSIZE(sequence);

  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_RETURN_STATEMENT, input);
}

GGToken parse_expression_statement(const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_expression, parse_semicolon};
  static const int num_sequence = ARRAYSIZE(sequence);

  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_EXPRESSION_STATEMENT, input);
}

GGToken parse_statement(const GGParseInput &input);
GGToken parse_block_statement (const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_left_brace, parse_statement, parse_right_brace};
  static const int num_sequence = ARRAYSIZE(sequence);

  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_BLOCK_STATEMENT, input);
}

//GGToken parse_function_call_statement(const GGParseInput &input) {
//  static const ParseFn sequence[] = {parse_identifier, parse_left_paren, parse_function_call_params, parse_right_paren, parse_semicolon};
//  static const int num_sequence = ARRAYSIZE(sequence);
//
//  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_FUNCTION_CALL, input);
//}

GGToken parse_for_statement(const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_for_exact, parse_left_paren, parse_variable_identifier, parse_in_exact, parse_expression, parse_right_paren, parse_right_paren, parse_statement};
  static const int num_sequence = ARRAYSIZE(sequence);
  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_FOR_STATEMENT, input);
}

GGToken parse_if_statement(const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_if_exact, parse_left_paren, parse_expression, parse_right_paren, parse_statement};
  static const int num_sequence = ARRAYSIZE(sequence);
  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_IF_STATEMENT, input);
}

GGToken parse_assignment_statement(const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_unary_expression, parse_assignment_op_symbol, parse_expression, parse_semicolon};
  static const int num_sequence = ARRAYSIZE(sequence);
  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_ASSIGNMENT_STATEMENT, input);
}

GGToken parse_function_definition(const GGParseInput &input);

GGToken parse_statement(const GGParseInput &input) {
  static const ParseFn statements[] = {
    parse_block_statement,
    parse_return_statement,
    parse_for_statement,
    parse_if_statement,
    // parse_switch_statement,
    parse_inline_llvm,

    parse_expression_statement,
    parse_assignment_statement,

    //parse_type_definition,
    parse_variable_definition,
    parse_function_definition,
  };

  static const int num_statements = ARRAYSIZE(statements);
  return parse_first_of(statements, num_statements, input);
}

GGToken parse_function_body(const GGParseInput &input) {
  return parse_zero_or_more(parse_statement, TOKEN_COMPOUND_FUNCTION_BODY,  input);
}

GGToken parse_function_definition(const GGParseInput &input) {
  // identifier := expression;
  static const ParseFn sequence[] = {parse_type_declaration, parse_function_identifier, parse_left_paren, parse_function_params, parse_right_paren, parse_left_brace, parse_function_body, parse_right_brace};
  static const int num_sequence = ARRAYSIZE(sequence);

  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_FUNCTION_DEFINITION, input);
};


//parse_int() {
//	if (isdigit(input.data[0]) == false) {
//		return false;
//	}
//
//	const char *next = input.data + 1;
//	while(isdigit(*next)) {
//		next++;
//	}
//
//	result.token = TOKEN_INTEGER;
//	result.substring.length = next - result.substring.start;
//}
//
//parse_number() {
//
//	parse_fns[] = {
//		parse_int,
//		//parse_decimal,
//		//parse_scientific,
//		//parse_hex,
//		//parse_octal,
//		//parse_binary,
//	}
//}
//
//
//
//parse_scientific
//parse_hex
////todo parse_binary, parse_octal
//
//
//parse_output parse_number(parse_input &input) {
//		if (isDigit(input.data[0]) == false) {
//			return false;
//		}
//		
//		const char *start_digit = input.data;
//
//		// 
//}
//
//parse_string() {
//}
//
//parse_endofline() {
//}

//parse_whitespace() {
//}

//
//GGParseOutput parse_sequence() {
//}

GGToken parse_program_statement(const GGParseInput &input) {
  static const ParseFn fns[] = { 
    //parse_nonsyntax_tokens,
    parse_import_statement, 
    parse_extern_function_declaration, 
    parse_type_definition, 
    parse_variable_definition, 
    parse_function_definition, 
  };
  static const int num_fns = ARRAYSIZE(fns);

  return parse_first_of(fns, num_fns, input);
}

GGToken parse_program(const GGParseInput &input) {
  GGParseInput cur_input = input;
  consume_whitespace(cur_input);
  return parse_zero_or_more(parse_program_statement, TOKEN_COMPOUND_PROGRAM, cur_input);
}


bool inline_parse_zero_or_more(GGToken *&tokens, int &num_remamining, ParseFn fn, GGParseInput &input) {
  int num_tokens = 0;
  while(1) {
    GGToken output = fn(input);
    if (ParseOuputIsFalse(output)) {
      break;
    } else {
      assert(num_tokens < num_remamining);
      tokens[num_tokens++] = output;
      input.data = output.next;
    }

    if (consume_whitespace(input) == false) break;
  }

  tokens += num_tokens;
  num_remamining -= num_tokens;
  return true;
}
bool inline_parse_program(GGToken *&tokens, int &num_remaining, GGParseInput &input) {
  consume_whitespace(input);
  return inline_parse_zero_or_more(tokens, num_remaining, parse_program_statement, input);
}

bool inline_ggcompile(const char *file_data, GGToken *&tokens, int &num_remaining) {
  GGParseInput input = {};
  input.data = file_data;
  bool result = inline_parse_program(tokens, num_remaining, input);
  return result;
}

//GGToken GGCompile(const char *file_data) {
//  GGParseInput input = {};
//  input.data = file_data;
//  GGToken output = ParseOutputAlloc(TOKEN_COMPOUND_PROGRAM, 1024);
//  int num_remaining = 1024;
//  inline_parse_program(output.subtokens, num_remaining, input);
//  output.num_subtokens = 1024 - num_remaining;
//  return output;
//}

void error(const char *format, ...) {
  exit(-1);
}

int fsize(FILE *file) {
  fseek(file, 0, SEEK_END);
  int size = ftell (file);
  rewind(file);
  return size;
}

char *FileRead(const GGSubString &filename) {
  char input_filename[MAX_FILENAME];
  assert(filename.length < MAX_FILENAME - 1);

  strncpy(input_filename, filename.start, filename.length);
  input_filename[filename.length] = 0;
  
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

  return file_data;
}

//GGToken GGCompile(const FileList &files) {
//  GGToken output = ParseOutputAlloc(TOKEN_COMPOUND_PROGRAM, 1024);
//  int num_remaining = 1024;
//  for(int i = 0; i < files.num_files; ++i) {
//    char *file_data = FileRead(files.files[i]);
//    inline_ggcompile(file_data, output.subtokens, num_remaining);
//  }
//  output.num_subtokens = 1024 - num_remaining;
//  return output;
//}

void FileNameParse(const GGSubString &source_path, GGSubString &filename, GGSubString &directory) {
  for(int i = source_path.length - 1; i >= 0; --i) {
    if (source_path.start[i] == '/') {
      directory.start = source_path.start;
      directory.length = i+1;
      filename.start = source_path.start + i + 1;
      filename.length = source_path.length - i - 1;
      return;
    }
  }

  directory.start = 0;
  directory.length = 0;
  filename = source_path;
}

void FileListSetDirectory(FileList &list, const GGSubString &directory) {
  list.directory_stack[list.stack_depth++] = directory;
}

void FileListPopDirectory(FileList &list) {
  list.stack_depth--;
}

GGToken GGCompile(const char * source_file) {
  GGToken output = ParseOutputAlloc(TOKEN_COMPOUND_PROGRAM, 1024);

  //FileList files;
  FileListAdd(gFileList, source_file); // get from argv;
  int num_remaining = 1024;
  GGToken *subtokens = output.subtokens;
  for(int i = 0; i < gFileList.num_files; ++i) {
    GGSubString filename;
    GGSubString directory;
    FileNameParse(gFileList.files[i], filename, directory);
    FileListSetDirectory(gFileList, directory);
    char *file_data = FileRead(gFileList.files[i]);
    inline_ggcompile(file_data, subtokens, num_remaining);
    FileListPopDirectory(gFileList);
  }
  output.num_subtokens = 1024 - num_remaining;
  return output;
}

//void substring_printf(const char *format, ...) {
//};

//void emit_global_variable(const GGParseOutput &tree) {
//	assert(tree.token == TOKEN_COMPOUND_GLOBAL_VARIABLE);
//	GGParseOutput &type = tree.subtokens[0];
//	GGParseOutput &identifier = tree.subtokens[1];
//	GGParseOutput &value = tree.subtokens[2];
//	const GGSubString &llvm_typename = lookup_llvm_typename(type);
//
//	//if (isLiteral(val)) {
//	//	substring_printf("@%x = weak global %x %x", identifier.substring, llvm_typename, value.substring);
//	//} else {
//	//	???
//	//}
//}
//
//void emit_local_variable(const GGParseOutput &tree) {
//	assert(tree.token == TOKEN_COMPOUND_VARIABLE);
//	GGParseOutput &type = tree.subtokens[0];
//	GGParseOutput &identifier = tree.subtokens[1];
//	GGParseOutput &value = tree.subtokens[2];
//	const GGSubString &llvm_typename = lookup_llvm_typename(type);
//	substring_printf("@%x = alloca %x\n", identifier.substring)
//	substring_printf("@%x = alloca %x\n", identifier.substring)
//}
//
//void emit_global_function_param(const GGParseOutput &tree) {
//	assert(tree.token == TOKEN_COMPOUND_FUNCTION_PARAM);
//	GGParseOutput &variable_type = tree.subtokens[0];
//	GGParseOutput &variable_identifier = tree.subtokens[1];
//
//	substring_printf("%x \%%x", variable_type.substring, variable_identifier.substring);
//}
//
//void emit_global_function_params(const GGParseOutput &tree) {
//	assert(tree.token == TOKEN_COMPOUND_FUNCTION_PARAMS);
//	for(int i = 0; i < tree.num_subtokens; ++i) {
//		emit_global_function_param(tree.subtokens[i]);
//		if (i == tree.num_subtokens - 1) {
//			break;
//		}
//
//		substring_printf(", ");
//	}
//}
//
//void emit_global_function(const GGParseOutput &tree) {
//	assert(tree.token == TOKEN_COMPOUND_FUNCTION);
//	GGParseOutput &return_type = tree.subtokens[0];
//	GGParseOutput &function_identifier = tree.subtokens[1];
//	GGParseOutput &function_params = tree.subtokens[2];
//	GGParseOutput &function_body = tree.subtokens[3];
//
//	substring_printf("define %x @%x(", return_type.substring, function_identifier.substring);
//	emit_function_params(function_params);
//	substring_printf(") {\n");
//	emit_function_body(function_body);
//	substring_printf("\n}\n");
//}
//
//void emit_comment(const GGParseOutput &tree) {
//	substring_printf("\t\t;%x", tree.substring);
//}
//
//void emit_subtoken(const GGParseOutput &tree) {
//	switch(tree.token) {
//	case TOKEN_COMMENT: emit_comment(tree);	break;
//	default:
//		halt();
//	}
//}
//
//
// let parse = {
//	 let stream = new stream;
//	 assign new parse(stream);
// }
//
//
//parse(pass_by_consumption stream);
//
//
//
//void emit_cr() {
//	printf("\n");
//}

//void gg_emit(const GGParseOutput &tree)
//{
//  llvm_emit_program(tree);
//  //assert(tree.token == TOKEN_COMMAND) {
//  //	for(int i = 0; i < tree.subtokens) {
//  //		gg_emit_subtoken(tree.subtokens[i]);
//  //		gg_emit_cr();
//  //	}
//  //}
//}

