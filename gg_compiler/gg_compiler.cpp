// gg_compiler.cpp

#include "precompile.h"
#include "gg.h"
#include "gg_llvm.h"

//void assert(bool assertion) 
//{
//	if (assertion == false) {
//		int i = 0; ++i;
//	}
//}

enum IdentifierType {
	IDENTIFIER_FUNCTION,
	IDENTIFIER_VARIABLE,
	IDENTIFIER_DATATYPE,

	IDENTIFIER_DATATYPE_TYPE,  // { struct, enum, typeset, typedef, llvm_type }
};

enum IdentifierScope {
	SCOPE_GLOBAL,
	SCOPE_LOCAL,
};

typedef unsigned long uint32;


struct Identifier	 {
	uint32 name_hash;
	SubString name;
	IdentifierType identifier_type;
	IdentifierScope identifier_scope;
	Identifier *data_type;
	Identifier *function_params;
	int num_function_params;
};

const int MAX_IDENTIFIERS = 1024;

struct IdentifierDB {
	Identifier identifiers[MAX_IDENTIFIERS];
	int num_identifiers;
};

uint32 crc32(const void *bytes, int lenght) {
	return 0;
}

uint32 substring_hash(const SubString &str) {
	return crc32(str.start, str.length);
}

bool param_list_equal(Identifier *param_list, int num_params, Identifier *param_list_2, int num_params_2)
{
	if (num_params != num_params_2) return false;
	for(int i = 0; i < num_params; ++i) {
		if (param_list[i].data_type != param_list_2[i].data_type) return false;
	}

	return true;
}

Identifier *IdentifierDBLookupFunction(IdentifierDB &db, const SubString &name, Identifier *param_list, int num_params)
{
	uint32 hash_name = substring_hash(name);
	for(int i = 0; i < db.num_identifiers; ++i) {
		Identifier &identifier = db.identifiers[i];
		if (identifier.name_hash == hash_name &&
			name.length == identifier.name.length &&
			strncmp(name.start, identifier.name.start, name.length) == 0) 
		{
			if (identifier.identifier_type != IDENTIFIER_FUNCTION) return &identifier;
			if (param_list_equal(param_list, num_params, identifier.function_params, identifier.num_function_params))

			return &identifier;
		}
	}

	return NULL;
}


Identifier *IdentifierDBLookup(IdentifierDB &db, const SubString &name)
{
	uint32 hash_name = substring_hash(name);
	for(int i = 0; i < db.num_identifiers; ++i) {
		Identifier &identifier = db.identifiers[i];
		if (identifier.name_hash == hash_name &&
			name.length == identifier.name.length &&
			strncmp(name.start, identifier.name.start, name.length) == 0) 
		{
			return &identifier;
		}
	}

	return NULL;
}

Identifier *IdentifierDBAlloc(IdentifierDB &db) {
	assert(db.num_identifiers < MAX_IDENTIFIERS);
	return &db.identifiers[db.num_identifiers++];
}


struct ErrorRetval {
	char error_string[1024];
};

void error(const char *error_string, ...);

bool isDataTypeType(const Identifier *identifier) {
	return identifier->identifier_type == IDENTIFIER_DATATYPE_TYPE;
}

bool isDataType(const Identifier *identifier) {
	return identifier->identifier_type == IDENTIFIER_DATATYPE;
}

void IdentifierDBAddDataTypeType(IdentifierDB &db, const char *type) {
	Identifier *identifier = IdentifierDBAlloc(db);
	identifier->identifier_type = IDENTIFIER_DATATYPE_TYPE;
	identifier->name.start = type;
	identifier->name.length = strlen(type);
	identifier->name_hash = substring_hash(identifier->name);
}

void IdentifierDBInit(IdentifierDB &db) {
	IdentifierDBAddDataTypeType(db, "struct");
	IdentifierDBAddDataTypeType(db, "enum");
	IdentifierDBAddDataTypeType(db, "llvm_type");
	IdentifierDBAddDataTypeType(db, "typedef");
}

void IdentifierDBAddUnknownType(IdentifierDB &db, const ParseOutput &name) {
	Identifier *identifier = IdentifierDBLookup(db, name.substring);
	assert(identifier == NULL);

	Identifier *newIdentifier = IdentifierDBAlloc(db);
	newIdentifier->name_hash = substring_hash(name.substring);
	newIdentifier->identifier_type = IDENTIFIER_DATATYPE;
	//newIdentifier->identifier_scope = lexical_scope_depth > 0 ? LOCAL : GLOBAL;
	newIdentifier->data_type;
}

void IdentifierDBAddType(IdentifierDB &db, const ParseOutput &name, const ParseOutput &type) {
	Identifier *identifier = IdentifierDBLookup(db, name.substring);
	if (identifier != NULL) {
		return error("duplicate identifier %s, first defined as %s");
	}

	Identifier *type_identifier = IdentifierDBLookup(db, type.substring);
	if (type_identifier == NULL) {
		return error("unknown datatype type for %s");
	}
	else if (!isDataTypeType(type_identifier)) {
		return error("invalid datatype type for %s, first defined as %s");
	}

	Identifier *newIdentifier = IdentifierDBAlloc(db);
	newIdentifier->name_hash = substring_hash(name.substring);
	newIdentifier->identifier_type = IDENTIFIER_DATATYPE;
	//newIdentifier->identifier_scope = lexical_scope_depth > 0 ? LOCAL : GLOBAL;
	newIdentifier->data_type = type_identifier;
}

void IdentifierDBAddVariable(IdentifierDB &db, const ParseOutput &name, const ParseOutput &type) {
	Identifier *identifier = IdentifierDBLookup(db, name.substring);
	if (identifier != NULL) {
		return error("duplicate identifier %s, first defined as %s");
	}

	Identifier *type_identifier = IdentifierDBLookup(db, type.substring);
	if (type_identifier == NULL) {
		IdentifierDBAddUnknownType(db, type);
	} else if (!isDataType(type_identifier)) {
		return error("invalid datatype for %s, first defined as %s");
	}

	Identifier *newIdentifier = IdentifierDBAlloc(db);
	newIdentifier->name_hash = substring_hash(name.substring);
	newIdentifier->identifier_type = IDENTIFIER_VARIABLE;
	//newIdentifier->identifier_scope = lexical_scope_depth > 0 ? LOCAL : GLOBAL;
	newIdentifier->data_type = type_identifier;
}

Identifier *ParamListToIdentifiers(IdentifierDB &db, const ParseOutput &param_list) {
	Identifier *retval = db.identifiers+db.num_identifiers;

	for(int i = 0; i < param_list.num_subtokens; ++i) {
		Identifier *param_identifier = IdentifierDBAlloc(db);
		ParseOutput &param = param_list.subtokens[i];
		assert(param.token == TOKEN_COMPOUND_FUNCTION_PARAM);
		assert(param.num_subtokens == 2);
		ParseOutput &type = param.subtokens[0];
		ParseOutput &name = param.subtokens[1];
		IdentifierDBAddVariable(db, name, type);
	}

	return retval;
}

void IdentifierDBAddFunction(IdentifierDB &db, const ParseOutput &name, const ParseOutput &param_list, const ParseOutput &retval) {
	Identifier *param_identifiers = ParamListToIdentifiers(db, param_list);
	Identifier *identifier = IdentifierDBLookupFunction(db, name.substring, param_identifiers, param_list.num_subtokens);
	if (identifier != NULL) {
		return error("duplicate identifier %s, first defined as %s");
	}

	Identifier *type_identifier = IdentifierDBLookup(db, retval.substring);
	if (type_identifier == NULL) {
		IdentifierDBAddUnknownType(db, retval);
	} else if (!isDataType(type_identifier)) {
		return error("invalid return type identifier  %s, first defined as %s");
	}

	Identifier *newIdentifier = IdentifierDBAlloc(db);
	newIdentifier->name_hash = substring_hash(name.substring);
	newIdentifier->identifier_type = IDENTIFIER_FUNCTION;
	//newIdentifier->identifier_scope = lexical_scope_depth > 0 ? LOCAL : GLOBAL;
	newIdentifier->function_params = param_identifiers;
	newIdentifier->num_function_params = param_list.num_subtokens;
}

enum CompileErrorCode
{
	COMPILE_ERROR_NONE,
};

struct CompileError 
{
	CompileErrorCode error_code;
	char error_string[1048];
};

struct TreeStatement 
{
	TokenType token_type;
	SubString substring;
};

struct SyntaxTree
{
	TreeStatement statements[1024];
};

struct CompileResults
{
	CompileError error;
	SyntaxTree tree;
};

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

// parse :: (line, column, const char *) -> (line, column, const char *, parse_error, parse_result)

//struct ParseInfo {
//	int line_number;
//	int col_number;
//};
//
//struct ParseInput {
//	const char *data;
//	ParseInfo info;
//};

//enum ParseOutputType {
//	PO_OK,
//	PO_FALSE, 
//};

//const ParseOutput PARSE_FALSE = { PO_FALSE };

//struct ParseOutputSubtoken {
//	ParseInfo info;
//	TokenType token;
//	SubString substring;
//}

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

const ParseOutput PARSE_FALSE = {};
ParseOutput parse_comment(const ParseInput &input) {
	if (input.data[0] == '/' &&
		input.data[1] == '/') {
		const char *eol = next_end_of_line(input.data);

		ParseOutput result = {};
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

ParseOutput parse_whitespace_chars(const ParseInput &input) 
{
	const char* data = input.data;
	while(iswhitespace(*data)) {
		data++;
	}

	if (data == input.data) {
		return PARSE_FALSE;
	}
	else {
		ParseOutput result = {};
		result.token = TOKEN_WHITESPACE;
		result.info = input.info;
		result.substring.start = input.data;
		result.substring.length = data - input.data;
		result.next = data;
		return result;
	}
}

ParseOutput parse_endoffile(const ParseInput &input) 
{
	if (input.data[0] == 0) {
			ParseOutput result = {};
			result.token = TOKEN_EOF;
			return result;
	} else {
		return PARSE_FALSE;
	}
}

ParseOutput parse_endofline(const ParseInput &input) 
{
	if (input.data[0] == '\n' || input.data[0] == '\r') {
		int eol_length = 1;
		if ((input.data[1] == '\n' ||
			input.data[1] == '\r') && 
			input.data[0] != input.data[1]) {
			eol_length = 2;
		}

		ParseOutput result = {};
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

typedef ParseOutput (*ParseFn)(const ParseInput &);

void ParseOutputFree(ParseOutput &total_output)
{
	assert(total_output.token >= TOKEN_COMPOUND);
	delete[] total_output.subtokens;
}

ParseOutput ParseOutputAlloc(TokenType token_type, int num_sequence) {
	assert(token_type > TOKEN_COMPOUND);
	ParseOutput retval = {};
	retval.token = token_type;
	retval.subtokens = new ParseOutput[num_sequence];
	return retval;
}

bool ParseOuputIsFalse(const ParseOutput &output) {
	return output.token == TOKEN_NONE;
}

void ParseOutputAppend(ParseOutput &output, const ParseOutput &newOutput) {
	const int MAX_SUBTOKENS = 1024;
	if (output.token == TOKEN_NONE) {
		output = newOutput;
	}	else if (output.token >= TOKEN_COMPOUND) {
		assert(output.num_subtokens < MAX_SUBTOKENS);
		output.subtokens[output.num_subtokens++] = newOutput;
	} else {
		output.subtokens = new ParseOutput[MAX_SUBTOKENS];
		output.subtokens[0] = output;
		output.subtokens[1] = newOutput;
		output.num_subtokens = 2;
		output.token = TOKEN_COMPOUND;
	}
}

#define ARRAYSIZE(ar) (sizeof((ar))/sizeof((ar)[0]))

ParseOutput parse_first_of(const ParseFn fn[], int numFns, const ParseInput &input) {
	for(int i = 0; i < numFns; ++i) {
		ParseOutput output = fn[i](input);
		if (!ParseOuputIsFalse(output)) {
			return output;
		} 
	}

	return PARSE_FALSE;
}

ParseOutput parse_nonsyntax_tokens(const ParseInput &input) {

	static const ParseFn fns[] = { parse_whitespace_chars, parse_endofline, parse_comment, parse_endoffile };
	static const int num_fns = ARRAYSIZE(fns);

	return parse_first_of(fns, num_fns, input);
}

ParseOutput parse_whitespace_separated_sequence(const ParseFn sequence[], int num_sequence, TokenType token_type, const ParseInput &input) 
{
	assert(num_sequence > 1);

	ParseInput cur_input = input;
	ParseOutput total_output = ParseOutputAlloc(token_type, num_sequence);

	int i = 0;
	for(; i < num_sequence; ++i) {

		{
			ParseOutput output = sequence[i](cur_input);
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
				ParseOutput discard = parse_nonsyntax_tokens(cur_input);
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

ParseOutput parse_identifier(const ParseInput &input) {
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

	ParseOutput result = {};
	result.token = TOKEN_IDENTIFIER;
	result.info = input.info;
	result.substring.start = input.data;
	result.substring.length = data - input.data;
	result.next = data;
	return result;
}

ParseOutput parse_type_identifier(const ParseInput &input) {
	return parse_identifier(input);
}

typedef bool (*CharPredicate)(char c0);

ParseOutput parse_zero_or_more_pred(CharPredicate pred, TokenType tokenType, const ParseInput &input)
{
	const char *data = input.data;
	while(data) {
		if (pred(*data) == false) break;
		data++;
	}

	ParseOutput result = {};
	result.token = tokenType;
	result.info = input.info;
	result.substring.start = input.data;
	result.substring.length = data - input.data;
	result.next = data;
	return result;
}

ParseOutput parse_one_or_more_pred(CharPredicate pred, TokenType tokenType, const ParseInput &input)
{
	const char *data = input.data;
	while(data) {
		if (pred(*data) == false) break;
		data++;
	}

	if (data == input.data) return PARSE_FALSE;

	ParseOutput result = {};
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

ParseOutput parse_integer_literal(const ParseInput &input) {
	return parse_one_or_more_pred(isDigit, TOKEN_LITERAL_INTEGER, input);
};

ParseOutput parse_numeric_literal(const ParseInput &input)
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

ParseOutput parse_literal(const ParseInput &input)
{
	static const ParseFn literals[] = { 
		parse_numeric_literal, 
		//TODO parse_string_literal, 
		//TODO parse_char_literal, 
	};
	static const int num_literals = ARRAYSIZE(literals);

	return parse_first_of(literals, num_literals, input);
}


ParseOutput parse_exact(const char *str, TokenType tokenType, const ParseInput &input)
{
	const char *c = str;
	const char *data = input.data;
	while(*c) {
		if (*c != *data) return PARSE_FALSE;
		c++;
		data++;
	}

	ParseOutput result = {};
	result.token = tokenType;
	result.info = input.info;
	result.substring.start = input.data;
	result.substring.length = data - input.data;
	result.next = data;
	return result;
}

ParseOutput parse_semicolon(const ParseInput &input) {
	return parse_exact(";", TOKEN_DISCARD, input);
}

ParseOutput parse_declaration_assignment_operator(const ParseInput &input) {
	return parse_exact(":=", TOKEN_DECLARATION_ASSIGNMENT, input);
}


ParseOutput parse_expression(const ParseInput &input) {
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

ParseOutput parse_function_param(const ParseInput &input) {
	static const ParseFn sequence[] = {
		parse_identifier, // type		//parse_type_identifier
		parse_identifier, // variable	//parse_variable_identifier
	};
	static const int num_sequence = ARRAYSIZE(sequence);

	return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_FUNCTION_PARAM, input);
}

ParseOutput parse_zero_or_more_separated(ParseFn fn, char separator, TokenType token_type, const ParseInput &input) {
	ParseInput cur_input = input;
	ParseOutput total_output = ParseOutputAlloc(token_type, 1024);

	while(1) {
		{
			ParseOutput output = fn(cur_input);
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
				ParseOutput discard = parse_nonsyntax_tokens(cur_input);
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
			ParseOutput discard = parse_exact(str, TOKEN_DISCARD, cur_input);
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
				ParseOutput discard = parse_nonsyntax_tokens(cur_input);
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

ParseOutput parse_function_params(const ParseInput &input) {
	return parse_zero_or_more_separated(parse_function_param, ',', TOKEN_COMPOUND_FUNCTION_PARAMS, input);
}

//ParseOutput &parse_function_identifier(const ParseInput &input) {
//}
//
//ParseOutput &parse_variable_identifier(const ParseInput &input) {
//}
//
//ParseOutput &parse_variable_identifier(const ParseInput &input) {
//}

ParseOutput parse_left_paren(const ParseInput &input) {
	return parse_exact("(", TOKEN_DISCARD, input);
}

ParseOutput parse_right_paren(const ParseInput &input) {
	return parse_exact(")", TOKEN_DISCARD, input);
}

ParseOutput parse_left_brace(const ParseInput &input) {
	return parse_exact("{", TOKEN_DISCARD, input);
}

ParseOutput parse_right_brace(const ParseInput &input) {
	return parse_exact("}", TOKEN_DISCARD, input);
}

ParseOutput parse_llvm_exact(const ParseInput &input) {
	return parse_exact("llvm", TOKEN_DISCARD, input);
}

bool isRightBrace(char c) {
	return c == '}';
}

ParseOutput parse_anything_until_right_brace(const ParseInput &input, TokenType token_type) {
	return parse_zero_or_more_pred(isRightBrace, token_type, input);
}

ParseOutput parse_llvm_content(const ParseInput &input)  {
	return parse_anything_until_right_brace(input, TOKEN_LLVM_CONTENT);
};

ParseOutput parse_inline_llvm(const ParseInput &input) {
	static const ParseFn sequence[] = { parse_llvm_exact, parse_left_brace, parse_llvm_content, parse_right_brace, };
	static const int num_sequence = ARRAYSIZE(sequence);
	return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_INLINE_LLVM, input);
}

ParseOutput parse_variable(const ParseInput &input) {
	// identifier := expression;
	static const ParseFn sequence[] = {parse_identifier, parse_declaration_assignment_operator, parse_expression};
	static const int num_sequence = ARRAYSIZE(sequence);

	return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_VARIABLE, input);
};

ParseOutput parse_variable_w_semicolon(const ParseInput &input) {
	// identifier := expression;
	static const ParseFn sequence[] = {parse_identifier, parse_declaration_assignment_operator, parse_expression, parse_semicolon};
	static const int num_sequence = ARRAYSIZE(sequence);

	return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_VARIABLE, input);
};

ParseOutput parse_return_exact(const ParseInput &input) {
	return parse_exact("return", TOKEN_DISCARD, input);
}

ParseOutput parse_return_statement(const ParseInput &input) {
	static const ParseFn sequence[] = {parse_return_exact, parse_expression, parse_semicolon};
	static const int num_sequence = ARRAYSIZE(sequence);

	return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_RETURN, input);
}

ParseOutput parse_function_call_params(const ParseInput &input) {
	return parse_zero_or_more_separated(parse_expression, ';', TOKEN_COMPOUND_FUNCTION_CALL_PARAMS, input);
}

ParseOutput parse_function_call(const ParseInput &input) {
	static const ParseFn sequence[] = {parse_identifier, parse_left_paren, parse_function_call_params, parse_right_paren};
	static const int num_sequence = ARRAYSIZE(sequence);

	return parse_whitespace_separated_sequence(sequence, num_sequence, TOKEN_COMPOUND_FUNCTION_CALL, input);
}

ParseOutput parse_statement(const ParseInput &input) {
	static const ParseFn statements[] = {
		parse_variable,
		parse_function_call,
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

ParseOutput parse_function_body(const ParseInput &input) {
	return parse_zero_or_more_separated(parse_statement, ';', TOKEN_COMPOUND_FUNCTION_BODY, input);
}

ParseOutput parse_function(const ParseInput &input) {
	// identifier := expression;
	static const ParseFn sequence[] = {parse_type_identifier, parse_identifier, parse_left_paren, parse_function_params, parse_right_paren, parse_left_brace, parse_function_body, parse_right_brace};
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
//ParseOutput parse_sequence() {
//}

ParseOutput parse_zero_or_more(ParseFn fn, const ParseInput &input) {
	ParseInput cur_input = input;
	ParseOutput total_output = {};
	while(1) {
		ParseOutput output = fn(cur_input);
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
			continue;
		} else {
			ParseOutputAppend(total_output, output);
			cur_input.data = output.next;
			cur_input.info = output.info;
			continue;
		}
	}

	return total_output;
}

ParseOutput parse_element(const ParseInput &input) {

	static const ParseFn fns[] = { 
		parse_nonsyntax_tokens,
		parse_variable_w_semicolon, 
		parse_function, 
	};
	static const int num_fns = ARRAYSIZE(fns);

	return parse_first_of(fns, num_fns, input);
}

ParseOutput parse_program(ParseInput &input) {
	return parse_zero_or_more(parse_element, input);
}

ParseOutput gg_compile(const char *file_data) 
{
	ParseInput input = {};
	input.data = file_data;
	ParseOutput output = parse_program(input);
	return output;
}

void halt() {
	assert(0);
}

void substring_printf(const char *format, ...) {

};

//void emit_global_variable(const ParseOutput &tree) {
//	assert(tree.token == TOKEN_COMPOUND_GLOBAL_VARIABLE);
//	ParseOutput &type = tree.subtokens[0];
//	ParseOutput &identifier = tree.subtokens[1];
//	ParseOutput &value = tree.subtokens[2];
//	const SubString &llvm_typename = lookup_llvm_typename(type);
//
//	//if (isLiteral(val)) {
//	//	substring_printf("@%x = weak global %x %x", identifier.substring, llvm_typename, value.substring);
//	//} else {
//	//	???
//	//}
//}
//
//void emit_local_variable(const ParseOutput &tree) {
//	assert(tree.token == TOKEN_COMPOUND_VARIABLE);
//	ParseOutput &type = tree.subtokens[0];
//	ParseOutput &identifier = tree.subtokens[1];
//	ParseOutput &value = tree.subtokens[2];
//	const SubString &llvm_typename = lookup_llvm_typename(type);
//	substring_printf("@%x = alloca %x\n", identifier.substring)
//	substring_printf("@%x = alloca %x\n", identifier.substring)
//}
//
//void emit_global_function_param(const ParseOutput &tree) {
//	assert(tree.token == TOKEN_COMPOUND_FUNCTION_PARAM);
//	ParseOutput &variable_type = tree.subtokens[0];
//	ParseOutput &variable_identifier = tree.subtokens[1];
//
//	substring_printf("%x \%%x", variable_type.substring, variable_identifier.substring);
//}
//
//void emit_global_function_params(const ParseOutput &tree) {
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
//void emit_global_function(const ParseOutput &tree) {
//	assert(tree.token == TOKEN_COMPOUND_FUNCTION);
//	ParseOutput &return_type = tree.subtokens[0];
//	ParseOutput &function_identifier = tree.subtokens[1];
//	ParseOutput &function_params = tree.subtokens[2];
//	ParseOutput &function_body = tree.subtokens[3];
//
//	substring_printf("define %x @%x(", return_type.substring, function_identifier.substring);
//	emit_function_params(function_params);
//	substring_printf(") {\n");
//	emit_function_body(function_body);
//	substring_printf("\n}\n");
//}
//
//void emit_comment(const ParseOutput &tree) {
//	substring_printf("\t\t;%x", tree.substring);
//}
//
//void emit_subtoken(const ParseOutput &tree) {
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

void gg_emit(const ParseOutput &tree)
{
	llvm_emit_program(tree);
	//assert(tree.token == TOKEN_COMMAND) {
	//	for(int i = 0; i < tree.subtokens) {
	//		gg_emit_subtoken(tree.subtokens[i]);
	//		gg_emit_cr();
	//	}
	//}
}


int main(int argc, char* argv[])
{
	const char *input_filename = "../../main.gg";
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

	ParseOutput compile_results = gg_compile(file_data);

	//if (ParseOutput.!= COMPILE_ERROR_NONE) {
	//	error(compile_results.error.error_string);
	//}

	gg_emit(compile_results);

	return 0;
}

