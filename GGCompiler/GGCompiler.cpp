// gg_compiler.cpp

#include "precompile.h"
#include "GGCompiler.h"
//#include "gg_llvm.h"

struct GGParseInput {
  const char *data;
  GGParseInfo info;
};

///////////////////////////////////
/*

_program              = [_program_statement]

_program_statement    = _variable_definition
                      | _function_definition
                      | _type_definition;

_variable_definition  = _type_identifier _variable_identifier ; 
                      | [_type_identifier] _variable_identifier = _expression ;

_function_definition  = _type_identifier _function_identifier ( _function_params ) { _function_body }

_function_params      = [_type_identifier _variable_identifier [= _expression]],

_type_definition      = _type_identifier = _type_expression ;

_type_identifier      = _identifier;
_variable_identifier  = _identifier;
_function_identifier  = _identifier;

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
  if (input.data[0] == '/' &&
    input.data[1] == '/') {
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
  if (data == ' ' ||
    data == '\t') {
      return true;
  }

  return false;
}

GGToken parse_whitespace_chars(const GGParseInput &input) 
{
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

GGToken parse_endoffile(const GGParseInput &input) 
{
  if (input.data[0] == 0) {
    GGToken result = {};
    result.token = TOKEN_EOF;
    return result;
  } else {
    return PARSE_FALSE;
  }
}

GGToken parse_endofline(const GGParseInput &input) 
{
  if (input.data[0] == '\n' || input.data[0] == '\r') {
    int eol_length = 1;
    if ((input.data[1] == '\n' ||
      input.data[1] == '\r') && 
      input.data[0] != input.data[1]) {
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

void ParseOutputFree(GGToken &total_output)
{
  assert(total_output.token >= TOKEN_COMPOUND);
  delete[] total_output.subtokens;
}

GGToken ParseOutputAlloc(GGTokenType token_type, int num_sequence) {
  assert(token_type > TOKEN_COMPOUND);
  GGToken retval = {};
  retval.token = token_type;
  retval.subtokens = new GGToken[num_sequence];
  return retval;
}

bool ParseOuputIsFalse(const GGToken &output) {
  return output.token == TOKEN_NONE;
}

void ParseOutputAppend(GGToken &output, const GGToken &newOutput) {
  const int MAX_SUBTOKENS = 1024;
  if (output.token == TOKEN_NONE) {
    output = newOutput;
  }	else if (output.token >= TOKEN_COMPOUND) {
    assert(output.num_subtokens < MAX_SUBTOKENS);
    output.subtokens[output.num_subtokens++] = newOutput;
  } else {
    output.subtokens = new GGToken[MAX_SUBTOKENS];
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

GGToken parse_whitespace_separated_sequence(const ParseFn sequence[], int num_sequence, GGTokenType token_type, const GGParseInput &input) 
{
  assert(num_sequence > 1);

  GGParseInput cur_input = input;
  GGToken total_output = ParseOutputAlloc(token_type, num_sequence);

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
    return PARSE_FALSE;
  }
  else {
    total_output.info = cur_input.info;
    total_output.next = cur_input.data;
    return total_output;
  }
}

bool ischar(char c0, char c1)
{
  return c0 == c1;
};

GGToken parse_identifier(const GGParseInput &input) {
  const char* data = input.data;

  if (isalpha(data[0]) ||
    ischar('_', data[0])) 
  {
    data++;
  }
  else 
  {
    return PARSE_FALSE;
  }

  while(isalpha(data[0]) ||
    ischar('_', data[0]) ||
    isdigit(data[0])) 
  {
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
  return parse_identifier(input);
}

GGToken parse_variable_identifier(const GGParseInput &input) {
  return parse_identifier(input);
}

GGToken parse_function_identifier(const GGParseInput &input) {
  return parse_identifier(input);
}

typedef bool (*CharPredicate)(char c0);

GGToken parse_zero_or_more_pred(CharPredicate pred, GGTokenType tokenType, const GGParseInput &input)
{
  const char *data = input.data;
  while(data) {
    if (pred(*data) == false) break;
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

GGToken parse_one_or_more_pred(CharPredicate pred, GGTokenType tokenType, const GGParseInput &input)
{
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

bool isDigit(char c) {
  return isdigit(c) != 0;
}

GGToken parse_integer_literal(const GGParseInput &input) {
  return parse_one_or_more_pred(isDigit, TOKEN_LITERAL_INTEGER, input);
};

GGToken parse_numeric_literal(const GGParseInput &input)
{
  static const ParseFn numeric_literals[] = { 
    parse_integer_literal, 
    //TODO parse_float_literal, 
    //TODO parse_hex_literal,
    //parse_octal_literal,
    //parse_binary_literal,
  };
  static const int num_literals = ARRAYSIZE(numeric_literals);

  return parse_first_of(numeric_literals, num_literals, input);
}

GGToken parse_literal(const GGParseInput &input)
{
  static const ParseFn literals[] = { 
    parse_numeric_literal, 
    //TODO parse_string_literal, 
    //TODO parse_char_literal, 
  };
  static const int num_literals = ARRAYSIZE(literals);

  return parse_first_of(literals, num_literals, input);
}


GGToken parse_exact(const char *str, GGTokenType tokenType, const GGParseInput &input)
{
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


GGToken parse_expression(const GGParseInput &input) {
  // expr :
  //		literal
  //		unary_operation
  //		binary_operation
  //		function_call
  //		variable_identifier
  //		paren_expression
  static const ParseFn expressions[] = {
    parse_literal, 
    //parse_unary_op, 
    //parse_binary_op, 
    //parse_function_call, 
    parse_identifier, //parse_variable_identifier,
    //parse_paren_expression,
  };
  static const int num_expressions = ARRAYSIZE(expressions);

  return parse_first_of(expressions, num_expressions, input);
}

GGToken parse_function_param(const GGParseInput &input) {
  static const ParseFn sequence[] = { parse_type_identifier, parse_variable_identifier };
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

//GGToken parse_zero_or_more_terminated(ParseFn fn, char separator, GGTokenType token_type, const GGParseInput &input) {
//  return parse_zero_or_more_separated(fn, separator, token_type, input);
//}

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

bool isRightBrace(char c) {
  return c == '}';
}

GGToken parse_anything_until_right_brace(const GGParseInput &input, GGTokenType token_type) {
  return parse_zero_or_more_pred(isRightBrace, token_type, input);
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

GGToken parse_variable_definition(const GGParseInput &input) {
  // identifier := expression;
  static const ParseFn sequence[] = {parse_type_identifier, parse_variable_identifier, parse_declaration_assignment_operator, parse_expression, parse_semicolon};
  static const int num_sequence = ARRAYSIZE(sequence);

  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_VARIABLE, input);
};

GGToken parse_return_exact(const GGParseInput &input) {
  return parse_exact("return", TOKEN_DISCARD, input);
}

GGToken parse_return_statement(const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_return_exact, parse_expression, parse_semicolon};
  static const int num_sequence = ARRAYSIZE(sequence);

  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_RETURN, input);
}

GGToken parse_function_call_params(const GGParseInput &input) {
  return parse_zero_or_more_separated(parse_expression, ';', TOKEN_COMPOUND_FUNCTION_CALL_PARAMS, input);
}

GGToken parse_function_call(const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_identifier, parse_left_paren, parse_function_call_params, parse_right_paren};
  static const int num_sequence = ARRAYSIZE(sequence);

  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_FUNCTION_CALL, input);
}

GGToken parse_function_call_statement(const GGParseInput &input) {
  static const ParseFn sequence[] = {parse_identifier, parse_left_paren, parse_function_call_params, parse_right_paren, parse_semicolon};
  static const int num_sequence = ARRAYSIZE(sequence);

  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_FUNCTION_CALL, input);
}

GGToken parse_statement(const GGParseInput &input) {
  static const ParseFn statements[] = {
    parse_variable_definition,
    parse_function_call_statement,
    parse_return_statement,
    parse_inline_llvm,
    // parse_for_statement,
    // parse_if_statement,
    // parse_switch_statement,
    // parse_scope_block,  { [statement] }
    // ...
  };
  static const int num_statements = ARRAYSIZE(statements);
  return parse_first_of(statements, num_statements, input);
}

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

GGToken parse_function_body(const GGParseInput &input) {
  return parse_zero_or_more(parse_statement, TOKEN_COMPOUND_FUNCTION_BODY,  input);
}

GGToken parse_function(const GGParseInput &input) {
  // identifier := expression;
  static const ParseFn sequence[] = {parse_type_identifier, parse_function_identifier, parse_left_paren, parse_function_params, parse_right_paren, parse_left_brace, parse_function_body, parse_right_brace};
  static const int num_sequence = ARRAYSIZE(sequence);

  return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_FUNCTION, input);
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
    parse_nonsyntax_tokens,
    parse_variable_definition, 
    parse_function, 
  };
  static const int num_fns = ARRAYSIZE(fns);

  return parse_first_of(fns, num_fns, input);
}

GGToken parse_program(GGParseInput &input) {
  return parse_zero_or_more(parse_program_statement, TOKEN_COMPOUND_PROGRAM, input);
}

GGToken GGCompile(const char *file_data) 
{
  GGParseInput input = {};
  input.data = file_data;
  GGToken output = parse_program(input);
  return output;
}

void halt() {
  assert(0);
}

void substring_printf(const char *format, ...) {

};

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

