// gg.h
#pragma once

enum TokenType {
	TOKEN_NONE,

	TOKEN_DISCARD,
	TOKEN_EOF,
	TOKEN_END_OF_LINE,
	TOKEN_WHITESPACE,
	TOKEN_COMMENT,
	TOKEN_TYPE_IDENTIFIER,
	TOKEN_IDENTIFIER,
	TOKEN_LLVM_CONTENT,

	TOKEN_SEMICOLON,
	TOKEN_DECLARATION_ASSIGNMENT,

	TOKEN_LITERAL_INTEGER,

	TOKEN_COMPOUND,

	TOKEN_COMPOUND_VARIABLE,
	TOKEN_COMPOUND_GLOBAL_VARIABLE,

	//TOKEN_COMPOUND_LITERAL_INTEGER,

	TOKEN_COMPOUND_FUNCTION,
	TOKEN_COMPOUND_FUNCTION_PARAMS,
	TOKEN_COMPOUND_FUNCTION_PARAM,
	TOKEN_COMPOUND_FUNCTION_BODY,

	TOKEN_COMPOUND_INLINE_LLVM,

	TOKEN_COMPOUND_FUNCTION_CALL,
	TOKEN_COMPOUND_FUNCTION_CALL_PARAMS,

	TOKEN_COMPOUND_RETURN,
};

//void assert(bool assertion);

struct ParseInfo {
	int line_number;
	int col_number;
};

struct ParseInput {
	const char *data;
	ParseInfo info;
};

struct SubString {
	const char *start;
	int length;
};

struct ParseOutput {
	ParseInfo info;
	TokenType token;
	SubString substring;

	ParseOutput *subtokens;
	int num_subtokens;

	const char *next;
};

void halt();