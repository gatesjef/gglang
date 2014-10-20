// gg_llvm.gg

#include "precompile.h"
#include "GGCompiler.h"
#include "GGError.h"

//llvm_emit_global(LLVM &llvm, Substring type, Substring name, Substring value)
//llvm_emit_local_variable() {
//	
//}

const int MAX_PARAMS = 128;

enum GGIdentifierType {
	IDENTIFIER_TYPE,
	IDENTIFIER_FUNCTION,
	IDENTIFIER_VARIABLE,
};

struct DBItem {
	GGIdentifierType itemType;
	GGSubString identifier;
	union {
	llvm::Type *type;
	llvm::Function *function;
	llvm::Value *value;
	};
};

const int MAX_DB_ITEMS = 1024;

struct LLVM {
	llvm::Module *module;
	llvm::IRBuilder<> *builder;
	llvm::LLVMContext *context;

	DBItem db_items[MAX_DB_ITEMS];
	int num_db_items;
};

llvm::StringRef to_string_ref(const GGSubString &substring) {
	return llvm::StringRef(substring.start, substring.length);
}

llvm::ArrayRef<llvm::Value *> to_array_ref(llvm::Value **values, int num_values) {
	return llvm::ArrayRef<llvm::Value *>(values, num_values);
}

llvm::ArrayRef<llvm::Type *> to_array_ref(llvm::Type **values, int num_values) {
	return llvm::ArrayRef<llvm::Type *>(values, num_values);
}

llvm::ArrayRef<char> to_array_ref(char *values, int num_values) {
	return llvm::ArrayRef<char>(values, num_values);
}


//llvm::Value *IdentifierDBLookup(LLVM &llvm, const GGToken &token) {
//}

bool substring_cmp(const GGSubString &substring, const char *str) {
	return strncmp(substring.start, str, substring.length) == 0;
}

bool substring_cmp(const GGSubString &substring, const GGSubString &substring0) {
	if (substring.length != substring0.length) return false;
	return strncmp(substring.start, substring0.start, substring0.length) == 0;
}

void *db_lookup_any(LLVM &llvm, const GGToken &token) {
	for(int i = 0; i < llvm.num_db_items; ++i) {
	DBItem &item = llvm.db_items[i];
	if (substring_cmp(item.identifier, token.substring)) {
		return item.value;
	}
	}
	return NULL;
}

llvm::Value *db_lookup_variable(LLVM &llvm, const GGToken &token)
{
	for(int i = 0; i < llvm.num_db_items; ++i) {
	DBItem &item = llvm.db_items[i];
	if (substring_cmp(item.identifier, token.substring)) {
		assert(item.itemType == IDENTIFIER_VARIABLE);
		return item.value;
	}
	}
	halt();
	return NULL;
}

llvm::Type *db_lookup_type(LLVM &llvm, const GGToken &token)
{
	for(int i = 0; i < llvm.num_db_items; ++i) {
	DBItem &item = llvm.db_items[i];
	if (substring_cmp(item.identifier, token.substring)) {
		assert(item.itemType == IDENTIFIER_TYPE);
		return item.type;
	}
	}
	halt();
	return NULL;
}

llvm::Function *db_lookup_function(LLVM &llvm, const GGToken &token)
{
	for(int i = 0; i < llvm.num_db_items; ++i) {
	DBItem &item = llvm.db_items[i];
	if (substring_cmp(item.identifier, token.substring)) {
		assert(item.itemType == IDENTIFIER_FUNCTION);
		return item.function;
	}
	}
	halt();
	return NULL;
}

void db_push_scope(LLVM &llvm)
{
	// TODO
}

void db_pop_scope(LLVM &llvm)
{
	// TODO
}

void db_add_type(LLVM &llvm, const GGToken &identifier, llvm::Type *type) {
	void *existing_type = db_lookup_any(llvm, identifier);
	assert(existing_type == NULL);
	assert(llvm.num_db_items < MAX_DB_ITEMS);
	DBItem &item = llvm.db_items[llvm.num_db_items++];
	item.itemType = IDENTIFIER_TYPE;
	item.identifier = identifier.substring;
	item.type = type;
}

//void db_add_global_variable(LLVM &llvm, const GGToken &identifier, llvm::Value *) {
//  // TODO
//}

void db_add_variable(LLVM &llvm, const GGToken &identifier, llvm::Value *value) {
	void *existing = db_lookup_any(llvm, identifier);
	assert(existing == NULL);
	assert(llvm.num_db_items < MAX_DB_ITEMS);
	DBItem &item = llvm.db_items[llvm.num_db_items++];
	item.itemType = IDENTIFIER_VARIABLE;
	item.identifier = identifier.substring;
	item.value = value;
}

void db_add_function(LLVM &llvm, const GGToken &identifier, llvm::Function *function) {
	void *existing = db_lookup_any(llvm, identifier);
	assert(existing == NULL);
	assert(llvm.num_db_items < MAX_DB_ITEMS);
	DBItem &item = llvm.db_items[llvm.num_db_items++];
	item.itemType = IDENTIFIER_FUNCTION;
	item.identifier = identifier.substring;
	item.function = function;
}

//llvm::Value *emit_expression(LLVM &llvm, const GGToken &expression);
llvm::Value *emit_lvalue_expression(LLVM &llvm, const GGToken &expression);
llvm::Value *emit_rvalue_expression(LLVM &llvm, const GGToken &expression);


int emit_function_call_params(LLVM &llvm, const GGToken &params, llvm::Value **values, int maxParams)
{
	assert(params.num_subtokens < maxParams);
	for(int i = 0; i< params.num_subtokens; ++i) {
	values[i] = emit_rvalue_expression(llvm, params.subtokens[i]);
	}

	return params.num_subtokens;
}

llvm::Value *emit_rvalue_function_call(LLVM &llvm, const GGToken &function_call) {
	assert(function_call.token == TOKEN_COMPOUND_FUNCTION_CALL);
	assert(function_call.num_subtokens == 2);
	GGToken &function_identifier = function_call.subtokens[0];
	GGToken &function_params = function_call.subtokens[1];

	llvm::StringRef name = to_string_ref(function_identifier.substring);
	llvm::Function *callee = llvm.module->getFunction(name);
	//llvm::Function *callee = db_lookup_function(llvm, function_identifier);

	assert(function_params.num_subtokens == callee->arg_size());

	llvm::Value *retval = NULL;
	if (function_params.num_subtokens == 0)
	{
	retval = llvm.builder->CreateCall(callee);
	//assert(retval);
	}
	else
	{
	llvm::Value *param_expressions[MAX_PARAMS];
	int num_params = emit_function_call_params(llvm, function_params, param_expressions, MAX_PARAMS);

	llvm::ArrayRef<llvm::Value *> ref_params = to_array_ref(param_expressions, num_params);
	retval = llvm.builder->CreateCall(callee, ref_params);
	//assert(retval);
	}

	return retval;
}

//llvm::Value *emit_lvalue_function_call(LLVM &llvm, const GGToken &function_call) 
// DOES NOT EXIST UNTIL return by reference



//void emit_local_function_call(LLVM &llvm, GGToken &function_call) {
//  assert(function_call.token == TOKEN_COMPOUND_FUNCTION_CALL);
//  assert(function_call.num_subtokens == 2);
//  GGToken &function_identifier = function_call.subtokens[0];
//  GGToken &function_params = function_call.subtokens[1];
//
//  llvm::StringRef name = to_string_ref(function_identifier.substring);
//  llvm::Function *callee = llvm.module->getFunction(name);
//
//  assert(function_params.num_subtokens == callee->arg_size());
//
//  llvm::Value *param_expressions[MAX_PARAMS];
//  int num_params = emit_function_call_params(llvm, function_params, param_expressions, MAX_PARAMS);
//
//  if (num_params == 0)
//  {
//    llvm::Value *retval = llvm.builder->CreateCall(callee, "calltmp");
//    assert(retval != NULL);
//  }
//  else
//  {
//    llvm::ArrayRef<llvm::Value *> ref_params = to_array_ref(param_expressions, num_params);
//    llvm::Value *retval = llvm.builder->CreateCall(callee, ref_params, "calltmp");
//    assert(retval != NULL);
//  }
//}

int get_integer_type_num_bits(const GGToken &integer_type) {
	//TODO
	return 32;
}

//llvm::Value *emit_integer_literal(LLVM &llvm, const GGParseOutput &integer_literal) {
//	assert(integer_literal.token == TOKEN_COMPOUND_LITERAL_INTEGER);
//	assert(integer_literal.num_subtokens == 2);
//	GGParseOutput &integer_value = integer_literal.subtokens[0];
//	GGParseOutput &integer_type  = integer_literal.subtokens[1];
//	int num_bits = get_integer_type_num_bits(integer_type);
//
//	llvm::IntegerType *type = llvm::IntegerType::get(*llvm.context, num_bits);
//	llvm::StringRef str = to_string_ref(integer_value.substring);
//
//	llvm::Value *retval = llvm::ConstantInt::get(type, str, 10);
//	return retval;
//}

int string_literal_to_char_data(char *data, const GGSubString &substring) {
	int n = 0;
	for(int i = 0; i < substring.length; ++i) {
		if (substring.start[i] == '\\') {
			++i;
			switch(substring.start[i]) {
			case '0':	
				data[n++] = '\0';
				break;
			case '\\':	
			case '\"':
			case '\'':
				data[n++] = substring.start[i];
				break;
			case 'n':
				data[n++] = '\n';
				break;
			case 'r':
				data[n++] = '\r';
				break;
			case 'b':
				data[n++] = '\b';
				break;
			case 't':
				data[n++] = '\t';
				break;
			case 'f':
				data[n++] = '\f';
				break;
			case 'a':
				data[n++] = '\a';
				break;
			default:
				// TODO, octal and hex values
				halt();
			//case 'x': {
			//	const char *end = first_non_hex()
			//} break;
			//default:
			//	if ('isdigit(')
			}
			++i;
		} else {
			data[n++] = substring.start[i];
		}
	}

	data[n++] = 0;
	return n;
}

static const bool SIGNED = true;

llvm::Value *emit_rvalue_string_literal(LLVM &llvm, const GGToken &string_literal) {
	assert(string_literal.token == TOKEN_LITERAL_STRING);

	//llvm::Type *type = get_type(llvm, type_token);
	//llvm::StringRef name = to_string_ref(identifier.substring);

	const int MAX_STRING_LENGTH = 1024;
	assert(MAX_STRING_LENGTH - 1 > string_literal.substring.length);
	char data[MAX_STRING_LENGTH];
	int lenWithNull = string_literal_to_char_data(data, string_literal.substring);

	//llvm::Type *i8Type = llvm::IntegerType::get(*llvm.context, 8);
	//llvm::ArrayType *i8ArrayType = llvm::ArrayType::get(i8Type, string_literal.substring.length + 1);

	GGSubString str_data;
	str_data.start = data;
	str_data.length = lenWithNull - 1;
	//llvm::ArrayRef<uint8_t> elts = to_array_ref(data, lenWithNull);
	//llvm::Constant *initializer = llvm::ConstantDataArray::getString(*llvm.context, to_string_ref(str_data), false);
	
	//llvm::Value *global_string = llvm.builder->CreateGlobalString(to_string_ref(str_data));
	//llvm::Value *

	llvm::Value *array_value = llvm.builder->CreateGlobalString(to_string_ref(str_data));
	llvm::Value *zero = llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 0, SIGNED);
	llvm::Value *zeros[] = {zero, zero};
	llvm::Value *i8pointer = llvm.builder->CreateGEP(array_value, to_array_ref(zeros, 2));

	//llvm::Value *lvalue = emit_lvalue_identifier(llvm, identifier);
	//llvm::Value *rvalue = llvm.builder->CreateLoad(i8pointer);
	return i8pointer;

	//llvm::ConstantArray::get(i8ArrayType, data);
	//llvm::Value *value = new llvm::GlobalVariable(*llvm.module, i8ArrayType, true, llvm::GlobalVariable::LinkOnceAnyLinkage, initializer);

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

llvm::Value *emit_rvalue_integer_literal(LLVM &llvm, const GGToken &integer_literal) {
	assert(integer_literal.token == TOKEN_LITERAL_INTEGER);
	int num_bits = get_integer_type_num_bits(integer_literal);

	llvm::IntegerType *type = llvm::IntegerType::get(*llvm.context, num_bits);
	llvm::StringRef str = to_string_ref(integer_literal.substring);

	const int RADIX = 10;
	llvm::Value *retval = llvm::ConstantInt::get(type, str, RADIX);
	return retval;
}

llvm::Value *emit_lvalue_unary_op(LLVM &llvm, const GGToken &token) {
	assert(token.num_subtokens == 2);
	const GGToken &op_token = token.subtokens[0];
	const GGToken &lhsToken = token.subtokens[1];

	switch(*op_token.substring.start) {
	case '+': {
		//llvm::Value *lhsVal = emit_rvalue_expression(llvm, lhsToken);
		//return lhsVal;
		halt();
	}
	case '-': {
		//llvm::Value *lhsVal = emit_rvalue_expression(llvm, lhsToken);
		//return Builder.CreateMul(neg1, lhs, "multmp");
		halt();
	}
	case '&': {
		halt();
		//llvm::Value *retval = alloca(type *)
		//llvm::Value *lhsVal = emit_lvalue_expression(llvm, lhsToken);
		//store(lhsVal, retval);
		//return retval;
	}
	case '*': {
		llvm::Value *lhsVal = emit_rvalue_expression(llvm, lhsToken);
		return lhsVal;
	}
	default:
		halt();
	}

	return NULL;
}

llvm::Value *emit_rvalue_unary_op(LLVM &llvm, const GGToken &token) {
	assert(token.num_subtokens == 2);
	const GGToken &op_token = token.subtokens[0];
	const GGToken &lhsToken = token.subtokens[1];

	switch(*op_token.substring.start) {
	case '+': {
		if (op_token.substring.length == 2) {
			llvm::Value *lvalue = emit_lvalue_expression(llvm, lhsToken);
			llvm::Value *rvalue = emit_rvalue_expression(llvm, lhsToken);
			llvm::Value *one   = llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 1, SIGNED);
			llvm::Value *retval = llvm.builder->CreateAdd(rvalue, one);
			llvm.builder->CreateStore(retval, lvalue);
			return retval;
		} else {
			return emit_rvalue_expression(llvm, lhsToken);
		}
	}
	case '-': {
		if (op_token.substring.length == 2) {
			llvm::Value *lvalue = emit_lvalue_expression(llvm, lhsToken);
			llvm::Value *rvalue = emit_rvalue_expression(llvm, lhsToken);
			llvm::Value *one   = llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 1, SIGNED);
			llvm::Value *retval = llvm.builder->CreateSub(rvalue, one);
			llvm.builder->CreateStore(retval, lvalue);
			return retval;
		} else {
			llvm::Value *value = emit_rvalue_expression(llvm, lhsToken);
			llvm::Value *zero = llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 0, SIGNED);
			return llvm.builder->CreateSub(zero, value);
		}
	}
	case '&': {
		return emit_lvalue_expression(llvm, lhsToken);
	}
	case '*': {
		llvm::Value *address = emit_rvalue_expression(llvm, lhsToken);
		llvm::Value *value = llvm.builder->CreateLoad(address);
		return value;
	}
	default:
		halt();
	}

	return NULL;
}

//rvalue_unary_op(expr, op) {
//	switch(op) {
//	case op_plus: return rvalue(expr);
//	case op_minus: return create_mul(neg1, rvalue(expr));
//	case op_address_of:
//		return lvalue(expr);
//	case op_deref:
//		value *retval = alloca(type);
//		store(rvalue(expr), retval);
//		return retval;
//	}
//}

llvm::Value *emit_rvalue_binary_op(LLVM &llvm, const GGToken &token) {
	assert(token.num_subtokens == 3);
	const GGToken &lhsToken = token.subtokens[0];
	const GGToken &op_token = token.subtokens[1];
	const GGToken &rhsToken = token.subtokens[2];

	llvm::Value *lhs = emit_rvalue_expression(llvm, lhsToken);
	llvm::Value *rhs = emit_rvalue_expression(llvm, rhsToken);

	switch(*op_token.substring.start) {
	case '+': {
		return llvm.builder->CreateAdd(lhs, rhs, "addtmp");
	}
	case '-': {
		return llvm.builder->CreateSub(lhs, rhs, "subtmp");
	}
	case '*': {
		return llvm.builder->CreateMul(lhs, rhs, "multmp");
	}
	case '/': {
		return llvm.builder->CreateSDiv(lhs, rhs, "divtmp");
	}
	case '%': {
		return llvm.builder->CreateSRem(lhs, rhs, "remtmp");
	}
	//case OP_BINARY_AND:
	//case OP_BINARY_OR:
	//case OP_BINARY_XOR:
	//case OP_LOGICAL_AND:
	//case OP_LOGICAL_OR:
	//case OP_COMPARE_EQUAL:
	//case OP_COMPARE_NOT_EQUAL:
	//case OP_COMPARE_LESS_THAN:
	//case OP_COMPARE_LESS_THAN_EQUAL:
	//case OP_COMPARE_GREATER_THAN:
	//case OP_COMPARE_GREATER_THAN_EQUAL:
	default:
		halt();
	}

	return NULL;
}

llvm::Value *emit_lvalue_array_dereference(LLVM &llvm, const GGToken &token) {
	assert(token.num_subtokens == 2);
	const GGToken &array_pointer = token.subtokens[0];
	const GGToken &index_expr = token.subtokens[1];

	llvm::Value *lhsVal = emit_rvalue_expression(llvm, array_pointer);
	assert(lhsVal);

	llvm::Value *indexVal = emit_rvalue_expression(llvm, index_expr);
	assert(indexVal);

	//Value *idxs[] = { &indexValue, 1 };
	//int num_idxs = ARRAYSIZE(idxs);
	//llvm::ArrayRef<Value *> array_ref = to_array_ref(idxs, num_idxs);

	return llvm.builder->CreateGEP(lhsVal, indexVal);
}

llvm::Value *emit_rvalue_array_dereference(LLVM &llvm, const GGToken &array_dereference) {
	llvm::Value *lvalue = emit_lvalue_array_dereference(llvm, array_dereference);
	llvm::Value *rvalue = llvm.builder->CreateLoad(lvalue);
	return rvalue;
}

llvm::Value *emit_rvalue_unary_post_op(LLVM &llvm, const GGToken &token) {
	assert(token.num_subtokens == 2);
	const GGToken &value_expr = token.subtokens[0];
	const GGToken &op_expr= token.subtokens[1];

	llvm::Value *lvalue = emit_lvalue_expression(llvm, value_expr);
	llvm::Value *const1 = llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 11, SIGNED);
	llvm::Value *value = llvm.builder->CreateLoad(lvalue);

	llvm::Value *newValue;
	switch(*op_expr.substring.start) {
	case '+':
		newValue = llvm.builder->CreateAdd(value, const1);
		break;
	case '-':
		newValue = llvm.builder->CreateSub(value, const1);
		break;
	default:
		halt();
	}

	llvm.builder->CreateStore(newValue, lvalue);
	return value;
}

//llvm::Value *emit_lvalue_expression_unary_post_op(LLVM &llvm, const GGToken &unary_post_expression) {
// DOES NOT EXIST

llvm::Value *field_index(LLVM &llvm, const GGToken &token) {
	// TODO
	return llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 0, SIGNED);
}

llvm::Value *emit_lvalue_identifier(LLVM &llvm, const GGToken &token) {
	assert(token.num_subtokens == 0);
		return db_lookup_variable(llvm, token);
		//assert(token.subtokens->value);
		//return (llvm::Value *)token.subtokens->next;
	// TODO
	//return IdentifierDBLookup(token.substring);
	//return NULL;
}

llvm::Value *emit_rvalue_identifier(LLVM &llvm, const GGToken &identifier) {
	llvm::Value *lvalue = emit_lvalue_identifier(llvm, identifier);
	llvm::Value *rvalue = llvm.builder->CreateLoad(lvalue);
	return rvalue;
}

llvm::Value *emit_lvalue_member_identifier(LLVM &llvm, const GGToken &token) {
	assert(token.num_subtokens == 2);
	const GGToken &basePointerExpr = token.subtokens[0];
	const GGToken &fieldIdentifier = token.subtokens[1];

	llvm::Value *basePointer = emit_lvalue_expression(llvm, basePointerExpr);
	assert(basePointer);

	llvm::Value *fieldIndex = field_index(llvm, fieldIdentifier);
	llvm::Value *zero = llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), 0, SIGNED);
	//llvm::Value *indexVal = emit_expression(llvm, lhsToken);
	//assert(indexVal);

	llvm::Value *idxs[] = {zero, fieldIndex};
	const int num_idxs = 2;
	
	llvm::Value *fieldPointer = llvm.builder->CreateGEP(basePointer, to_array_ref(idxs, num_idxs));
	return fieldPointer;
}

llvm::Value *emit_rvalue_member_identifier(LLVM &llvm, const GGToken &member_identifier) {
	llvm::Value *lvalue = emit_lvalue_member_identifier(llvm, member_identifier);
	llvm::Value *rvalue = llvm.builder->CreateLoad(lvalue);
	return rvalue;
}

llvm::Value *emit_lvalue_expression(LLVM &llvm, const GGToken &expression) {
	switch(expression.token) {
	case TOKEN_COMPOUND_UNARY_OPERATION:
		return emit_lvalue_unary_op(llvm, expression);
	case TOKEN_COMPOUND_ARRAY_INDEX:
		return emit_lvalue_array_dereference(llvm, expression);
	case TOKEN_COMPOUND_MEMBER_IDENTIFIER:
		return emit_lvalue_member_identifier(llvm, expression);
	case TOKEN_IDENTIFIER:
		return emit_lvalue_identifier(llvm, expression);
	case TOKEN_COMPOUND_BINARY_OPERATION:
	//return emit_expression_binary_op(llvm, expression);
	case TOKEN_COMPOUND_UNARY_POST_OPERATION:
	//return emit_expression_unary_post_op(llvm, expression);
	case TOKEN_COMPOUND_FUNCTION_CALL:
	//return emit_lvalue_expression_function_call(llvm, expression);
	case TOKEN_LITERAL_INTEGER:
	//return emit_integer_literal(llvm, expression);
	default:
		halt();
	}

	return NULL;
}

llvm::Value *emit_rvalue_expression(LLVM &llvm, const GGToken &expression) {
	switch(expression.token) {
	case TOKEN_COMPOUND_UNARY_OPERATION:
		return emit_rvalue_unary_op(llvm, expression);
	case TOKEN_COMPOUND_BINARY_OPERATION:
		return emit_rvalue_binary_op(llvm, expression);
	case TOKEN_COMPOUND_ARRAY_INDEX:
		return emit_rvalue_array_dereference(llvm, expression);
	case TOKEN_COMPOUND_UNARY_POST_OPERATION:
		return emit_rvalue_unary_post_op(llvm, expression);
	case TOKEN_COMPOUND_MEMBER_IDENTIFIER:
		return emit_rvalue_member_identifier(llvm, expression);
	case TOKEN_COMPOUND_FUNCTION_CALL:
		return emit_rvalue_function_call(llvm, expression);
	case TOKEN_LITERAL_INTEGER:
		return emit_rvalue_integer_literal(llvm, expression);
	case TOKEN_LITERAL_STRING:
		return emit_rvalue_string_literal(llvm, expression);
	case TOKEN_IDENTIFIER:
		return emit_rvalue_identifier(llvm, expression);

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

void llvm_emit_local_return(LLVM &llvm, GGToken &return_statement) {
	assert(return_statement.token == TOKEN_COMPOUND_RETURN_STATEMENT);
	assert(return_statement.num_subtokens == 1);
	GGToken &return_expression = return_statement.subtokens[0];

	llvm::Value *retval = emit_rvalue_expression(llvm, return_expression);
	llvm.builder->CreateRet(retval);
}

//llvm::Type *type_get(LLVM &llvm, const GGToken &type) {
//  //TODO
//  return llvm::IntegerType::get(*llvm.context, 32);
//}

llvm::Type* get_type(LLVM &llvm, const GGToken &type);

static llvm::AllocaInst *CreateAlloca(LLVM &llvm, llvm::Function *TheFunction, const GGToken &identifier, const GGToken &type) {
//  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
//
	llvm::StringRef name = to_string_ref(identifier.substring);
//
	llvm::Type *llvm_type = get_type(llvm, type);
//
//  return TmpB.CreateAlloca(llvm_type, 0, name);
	return llvm.builder->CreateAlloca(llvm_type, 0, name);
}

void llvm_emit_local_variable(LLVM &llvm, llvm::Function *function, const GGToken &local_variable) {
	assert(local_variable.token == TOKEN_COMPOUND_VARIABLE_DEFINITION);
	assert(local_variable.num_subtokens == 3);
	GGToken &type				= local_variable.subtokens[0];
	GGToken &identifier			= local_variable.subtokens[1];
	GGToken &value_expression	= local_variable.subtokens[2];

	llvm::Value *value = emit_rvalue_expression(llvm, value_expression);

	llvm::AllocaInst *alloca = CreateAlloca(llvm, function, identifier, type);
	llvm.builder->CreateStore(value, alloca);

	db_add_variable(llvm, identifier, alloca);
}

//void llvm_emit_function_body(LLVM &llvm, llvm::Function *function, const GGToken &function_body) {
//  llvm::BasicBlock *entry = llvm::BasicBlock::Create(*llvm.context, "entrypoint", function);
//  llvm.builder->SetInsertPoint(entry);
//  //llvm::BasicBlock *BB = BasicBlock::Create(*llvm.context, "entry", function);
//  //llvm.builder->SetInsertPoint(BB);
//
//  for(int i = 0; i < function_body.num_subtokens; ++i) {
//    GGToken &subtoken = function_body.subtokens[i];
//    switch(subtoken.token) {
//    case TOKEN_COMPOUND_VARIABLE_DEFINITION:
//      llvm_emit_local_variable(llvm, function, subtoken);
//      break;
//    //case TOKEN_COMPOUND_FUNCTION_CALL:
//    //  emit_local_function_call(llvm, subtoken);
//    //  break;
//    case TOKEN_COMPOUND_RETURN_STATEMENT:
//      llvm_emit_local_return(llvm, subtoken);
//      break;
//    default: 
//      halt();
//    }
//  }
//
//  //verifyFunction(*function);
//}

enum {
	FIXED_ARGS = false,
	VAR_ARGS = true,
};

llvm::Type *emit_param_type(LLVM &llvm, const GGToken &param)
{
	assert(param.token == TOKEN_COMPOUND_FUNCTION_PARAM);
	assert(param.num_subtokens == 2);
	const GGToken &param_type = param.subtokens[0];
	const GGToken &param_identifier = param.subtokens[1];
	return get_type(llvm, param_type);
}

int get_param_types(LLVM &llvm, const GGToken &params, llvm::Type **types, int maxParams)
{
	assert(params.num_subtokens < maxParams);
	for(int i = 0; i< params.num_subtokens; ++i) {
		types[i] = emit_param_type(llvm, params.subtokens[i]);
	}

	return params.num_subtokens;
}


//llvm::Function *llvm_emit_function_prototype(LLVM &llvm, const GGToken &function_definition) {
//  assert(function_definition.token == TOKEN_COMPOUND_FUNCTION_DEFINITION);
//  assert(function_definition.num_subtokens == 4);
//  const GGToken &function_return_type = function_definition.subtokens[0];
//  const GGToken &function_identifier  = function_definition.subtokens[1];
//  const GGToken &function_params      = function_definition.subtokens[2];
//
//  llvm::Type *retval_type = get_type(llvm, function_return_type);
//
//  llvm::FunctionType *functionType;
//  if (function_params.num_subtokens == 0) 
//  {
//    functionType = llvm::FunctionType::get(retval_type, FIXED_ARGS);
//  }
//  else
//  {
//    llvm::Type *param_types[MAX_PARAMS];
//    int num_params = get_param_types(llvm, function_params, param_types, MAX_PARAMS);
//
//    llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, num_params);
//    functionType = llvm::FunctionType::get(retval_type, args, FIXED_ARGS);
//  }
//
//  llvm::StringRef name = to_string_ref(function_identifier.substring);
//
//  //llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, llvm.module);
//  llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, llvm.module);
//  assert(function);
//  return function;
//}

// lookup_function(function_definition);

void llvm_emit_assignment_statement(LLVM &llvm, const GGToken &assigment) {
	assert(assigment.token == TOKEN_COMPOUND_ASSIGNMENT_STATEMENT);
	assert(assigment.num_subtokens == 3);
	const GGToken &lhs_expr = assigment.subtokens[0];
	const GGToken &assigment_op = assigment.subtokens[1];
	const GGToken &rhs_expr = assigment.subtokens[2];

	llvm::Value *lhs = emit_lvalue_expression(llvm, lhs_expr);
	llvm::Value *r_lhs = emit_rvalue_expression(llvm, lhs_expr);
	llvm::Value *rhs = emit_rvalue_expression(llvm, rhs_expr);

	switch(assigment_op.substring.start[0]) {
	case '=': {
		llvm.builder->CreateStore(rhs, lhs);
	} break;
	case '+': {
		llvm::Value *newVal = llvm.builder->CreateAdd(r_lhs, rhs);
		llvm.builder->CreateStore(newVal, lhs);
	} break;
	case '-': {
		llvm::Value *newVal = llvm.builder->CreateSub(r_lhs, rhs);
		llvm.builder->CreateStore(newVal, lhs);
	} break;
	case '*': {
		llvm::Value *newVal = llvm.builder->CreateMul(r_lhs, rhs);
		llvm.builder->CreateStore(newVal, lhs);
	} break;
	case '/': {
		llvm::Value *newVal = llvm.builder->CreateSDiv(r_lhs, rhs);
		llvm.builder->CreateStore(newVal, lhs);
	} break;
	case '%': {
		llvm::Value *newVal = llvm.builder->CreateSRem(r_lhs, rhs);
		llvm.builder->CreateStore(newVal, lhs);
	} break;
	default:
		halt();
	}
}

void llvm_emit_external_function_declaration(LLVM &llvm, const GGToken &function_declaration) {
	assert(function_declaration.token == TOKEN_COMPOUND_EXTERNAL_FUNCTION_DECLARATION);
	assert(function_declaration.num_subtokens == 3);
	const GGToken &function_return_type = function_declaration.subtokens[0];
	const GGToken &function_identifier  = function_declaration.subtokens[1];
	const GGToken &function_param_types = function_declaration.subtokens[2];

	llvm::Type *retval_type = get_type(llvm, function_return_type);

	llvm::FunctionType *functionType;
	if (function_param_types.num_subtokens == 0) 
	{
		functionType = llvm::FunctionType::get(retval_type, FIXED_ARGS);
	}
	else
	{
		llvm::Type *param_types[MAX_PARAMS];
		int num_params = get_param_types(llvm, function_param_types, param_types, MAX_PARAMS);

		llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, num_params);
		functionType = llvm::FunctionType::get(retval_type, args, FIXED_ARGS);

		//emit_paramater_bindings(llvm, function_params);
	}

	llvm::StringRef name = to_string_ref(function_identifier.substring);

	llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, llvm.module);
	assert(function);

	//function->addFnAttr("nounwind");

	db_add_function(llvm, function_identifier, function);
}

void llvm_emit_function_definition(LLVM &llvm, const GGToken &function_definition) {
	assert(function_definition.token == TOKEN_COMPOUND_FUNCTION_DEFINITION);
	assert(function_definition.num_subtokens == 4);
	const GGToken &function_return_type = function_definition.subtokens[0];
	const GGToken &function_identifier  = function_definition.subtokens[1];
	const GGToken &function_params      = function_definition.subtokens[2];
	const GGToken &function_body        = function_definition.subtokens[3];

	llvm::Type *retval_type = get_type(llvm, function_return_type);
	llvm::FunctionType *functionType;
	if (function_params.num_subtokens == 0) 
	{
		functionType = llvm::FunctionType::get(retval_type, FIXED_ARGS);
	}
	else
	{
		llvm::Type *param_types[MAX_PARAMS];
		int num_params = get_param_types(llvm, function_params, param_types, MAX_PARAMS);

		llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, num_params);
		functionType = llvm::FunctionType::get(retval_type, args, FIXED_ARGS);

		//emit_paramater_bindings(llvm, function_params);
	}

	llvm::StringRef name = to_string_ref(function_identifier.substring);

	//llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, llvm.module);
	llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, llvm.module);
	assert(function);

	llvm::BasicBlock *entry = llvm::BasicBlock::Create(*llvm.context, "", function);
	llvm.builder->SetInsertPoint(entry);

	int i = 0;
	for (llvm::Function::arg_iterator AI = function->arg_begin(); i < function_params.num_subtokens; ++AI, ++i) {
		const GGToken &param_type = function_params.subtokens[i].subtokens[0];
		const GGToken &param_identifier = function_params.subtokens[i].subtokens[1];
		//AI->setName(name);

		llvm::StringRef name = to_string_ref(param_identifier.substring);

		llvm::AllocaInst *alloca = CreateAlloca(llvm, function, param_identifier, param_type);
		llvm.builder->CreateStore(AI, alloca);

		db_add_variable(llvm, param_identifier, alloca);
	}

	//llvm::BasicBlock *BB = BasicBlock::Create(*llvm.context, "entry", function);
	//llvm.builder->SetInsertPoint(BB);

	for(int i = 0; i < function_body.num_subtokens; ++i) {
		GGToken &subtoken = function_body.subtokens[i];
		switch(subtoken.token) {
		case TOKEN_COMPOUND_VARIABLE_DEFINITION:
			llvm_emit_local_variable(llvm, function, subtoken);
			break;
		case TOKEN_COMPOUND_ASSIGNMENT_STATEMENT:
			llvm_emit_assignment_statement(llvm, subtoken);
			break;
		case TOKEN_COMPOUND_RETURN_STATEMENT:
			llvm_emit_local_return(llvm, subtoken);
			break;
		case TOKEN_COMPOUND_EXPRESSION_STATEMENT:
			assert(subtoken.num_subtokens == 1);
			emit_rvalue_expression(llvm, subtoken.subtokens[0]);
			break;
		default: 
			halt();
		}
	}

	//verifyFunction(*function);

	//llvm::Function *function = lookup_function(function_definition);
	//llvm_emit_function_body(llvm, function, function_body);
	//function->addFnAttr("nounwind");

	db_add_function(llvm, function_identifier, function);
}

//void emit_global_functions_declarations(LLVM &llvm, const GGToken &program) {
//  for(int i = 0; i < program.num_subtokens; ++i) {
//    const GGToken &subtoken = program.subtokens[i];
//    if (subtoken.token == TOKEN_COMPOUND_FUNCTION) {
//      llvm_emit_function_prototype(llvm, subtoken);
//    }
//  }
//}

uint64_t substring_to_uint64(const GGSubString &substring) {
	//const char *digit = substring.start;
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

bool isWhitespace(char c) {
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

GGSubString substring_trim(const GGSubString &substring) {

	GGSubString retval = substring;

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

llvm::Type *get_llvm_type(LLVM &llvm, const GGToken &type) {
	// todo vector types...
	assert(type.substring.length > 0);
	assert(type.substring.start != NULL);

	GGSubString trimmed_content = substring_trim(type.substring);

	assert(trimmed_content.length > 0);
	assert(trimmed_content.start != NULL);

	switch(trimmed_content.start[0]) {
	case 'i': {
	GGSubString digits = trimmed_content;
	digits.start++;
	digits.length--;

	uint32_t size = (uint32_t)substring_to_uint64(digits);
	return llvm::IntegerType::get(*llvm.context, size);
	}
	default:
	if (substring_cmp(trimmed_content, "void")) {
		return llvm::Type::getVoidTy(*llvm.context);
	} else if (substring_cmp(trimmed_content, "half")) {
		return llvm::Type::getHalfTy(*llvm.context);
	} else if (substring_cmp(trimmed_content, "float")) {
		return llvm::Type::getFloatTy(*llvm.context);
	} else if (substring_cmp(trimmed_content, "double")) {
		return llvm::Type::getDoubleTy(*llvm.context);
	} else if (substring_cmp(trimmed_content, "fp128")) {
		return llvm::Type::getFP128Ty(*llvm.context);
	} else {
		halt();
	}
	break;
	}

	return NULL;
}

uint64_t eval_constexpr(const GGToken &constexpr_token) {
	switch(constexpr_token.token) {
	case TOKEN_LITERAL_INTEGER:
		return substring_to_uint64(constexpr_token.substring);
	default:
		halt();
	}

	return NULL;
}


llvm::Type *get_type_identifier(LLVM &llvm, const GGToken &type_token) {
	assert(type_token.token == TOKEN_TYPE_IDENTIFIER);
	llvm::Type *type = db_lookup_type(llvm, type_token);
	assert(type);
	return type;
}

llvm::Type *get_pointer_type(LLVM &llvm, const GGToken &type_token) {
	assert(type_token.token == TOKEN_COMPOUND_POINTER_TYPE);
	assert(type_token.num_subtokens == 1);
	const GGToken &base_type_token = type_token.subtokens[0];

	llvm::Type *base_type = get_type(llvm, base_type_token);
	return llvm::PointerType::get(base_type, 0);
}

llvm::Type *get_array_type(LLVM &llvm, const GGToken &type_token) {
	assert(type_token.token == TOKEN_COMPOUND_ARRAY_TYPE);
	assert(type_token.num_subtokens == 2);
	const GGToken &base_type_token = type_token.subtokens[0];
	const GGToken &array_size = type_token.subtokens[1];

	llvm::Type *base_type = get_type(llvm, base_type_token);
	uint64_t num_elements = eval_constexpr(array_size);
	return llvm::ArrayType::get(base_type, num_elements);
}

llvm::Type *get_function_type(LLVM &llvm, const GGToken &type_token) {
	assert(type_token.token == TOKEN_COMPOUND_FUNCTION_TYPE);
	assert(type_token.num_subtokens == 2);
	const GGToken &return_type_token = type_token.subtokens[0];
	const GGToken &param_type_tokens = type_token.subtokens[1];

	llvm::Type *retval_type = get_type(llvm, return_type_token);
	llvm::Type *param_types[MAX_PARAMS];
	int num_params = get_param_types(llvm, param_type_tokens, param_types, MAX_PARAMS);
	llvm::ArrayRef<llvm::Type *> args = to_array_ref(param_types, num_params);

	return llvm::FunctionType::get(retval_type, args, FIXED_ARGS);
}

llvm::Type* get_type(LLVM &llvm, const GGToken &type) {
	switch(type.token) {
	case TOKEN_TYPE_IDENTIFIER:
		return get_type_identifier(llvm, type);
	case TOKEN_LLVM_CONTENT:
		return get_llvm_type(llvm, type);
	case TOKEN_COMPOUND_POINTER_TYPE:
		return get_pointer_type(llvm, type);
	case TOKEN_COMPOUND_ARRAY_TYPE:
		return get_array_type(llvm, type);
	case TOKEN_COMPOUND_FUNCTION_TYPE:
		return get_function_type(llvm, type);
	default:
		halt();
	}

	return NULL;
}

void llvm_emit_global_variable(LLVM &llvm, const GGToken &variable_declaraion) {
	assert(variable_declaraion.num_subtokens == 3);
	const GGToken &type_token = variable_declaraion.subtokens[0];
	const GGToken &identifier = variable_declaraion.subtokens[1];
	const GGToken &expression = variable_declaraion.subtokens[2];
	
	llvm::Type *type = get_type(llvm, type_token);
	llvm::StringRef name = to_string_ref(identifier.substring);
	llvm::Value *value = new llvm::GlobalVariable(*llvm.module, type, false, llvm::GlobalVariable::LinkOnceAnyLinkage, NULL, name);

	//llvm.module->getOrInsertGlobal(
	//  
	//  ) GlobalList.push_back(value);
	db_add_variable(llvm, identifier, value);
}

//void llvm_emit_local_variable(LLVM &llvm, const GGToken &variable_declaraion) {
//  assert(variable_declaraion.num_subtokens == 3);
//  const GGToken &type_token = variable_declaraion.subtokens[0];
//  const GGToken &identifier = variable_declaraion.subtokens[1];
//  const GGToken &expression = variable_declaraion.subtokens[2];
//
//  llvm::Type *type = get_type(type_token);
//  llvm::AllocaInst *alloca = CreateEntryBlockAlloca(llvm, function, identifier, type);
//  //llvm::Value *value = Alloca(llvm, function, type);
//  db_add_scoped_variable(identifier, value);
//}

llvm::Type *llvm_emit_llvm_type_definition(LLVM &llvm, const GGToken &type_def) {
	assert(type_def.num_subtokens == 2);
	const GGToken &identifier = type_def.subtokens[0];
	const GGToken &type_decl = type_def.subtokens[1];

	llvm::Type *type = get_type(llvm, type_decl);
	db_add_type(llvm, identifier, type);
	return type;
}

llvm::Type *llvm_emit_type_definition(LLVM &llvm, const GGToken &type_def) {
	assert(type_def.num_subtokens == 2);
	const GGToken &identifier = type_def.subtokens[0];
	const GGToken &type_decl = type_def.subtokens[1];

	llvm::Type *type = get_type(llvm, type_decl);
	db_add_type(llvm, identifier, type);
	return type;
}

llvm::Type *llvm_emit_struct_field(LLVM &llvm, const GGToken &struct_field) {
	//assert(struct_field.token == TOKEN_COMPOUND_STRUCT_FIELD);
	assert(struct_field.num_subtokens >= 2);
	const GGToken &type_decl  = struct_field.subtokens[0];
	//const GGToken &identifier = struct_field.subtokens[1];
	return get_type(llvm, type_decl);
}

llvm::Type *llvm_emit_struct_definition(LLVM &llvm, const GGToken &struct_def) {
	assert(struct_def.num_subtokens == 2);
	const GGToken &identifier = struct_def.subtokens[0];
	const GGToken &struct_body = struct_def.subtokens[1];

	assert(struct_body.num_subtokens < MAX_PARAMS);
	llvm::Type *fields[MAX_PARAMS];
	for(int i = 0; i < struct_body.num_subtokens; ++i) {
		llvm::Type *field = llvm_emit_struct_field(llvm, struct_body.subtokens[i]);
		fields[i] = field;
	}

	llvm::ArrayRef<llvm::Type *> ref_fields = to_array_ref(fields, struct_body.num_subtokens);

	static const int IS_PACKED = true;
	llvm::Type *type = llvm::StructType::get(*llvm.context, ref_fields, IS_PACKED);
	db_add_type(llvm, identifier, type);
	return type;
}

void llvm_emit_global_definitions(LLVM &llvm, const GGToken &program) {
	for(int i = 0; i < program.num_subtokens; ++i) {
		const GGToken &subtoken = program.subtokens[i];
		switch(subtoken.token) {
		case TOKEN_COMPOUND_VARIABLE_DEFINITION:
			llvm_emit_global_variable(llvm, subtoken);
			break;
		case TOKEN_COMPOUND_FUNCTION_DEFINITION:
			llvm_emit_function_definition(llvm, subtoken);
			break;
		case TOKEN_COMPOUND_STRUCT_DEFINITION:
			llvm_emit_struct_definition(llvm, subtoken);
			break;
		case TOKEN_COMPOUND_TYPEDEF_DEFINITION:
			llvm_emit_type_definition(llvm, subtoken);
			break;
		case TOKEN_COMPOUND_LLVM_TYPE_DEFINITION:
			llvm_emit_llvm_type_definition(llvm, subtoken);
			break;
		case TOKEN_COMPOUND_EXTERNAL_FUNCTION_DECLARATION:
			llvm_emit_external_function_declaration(llvm, subtoken);
		case TOKEN_COMMENT:
			break;
		default:
			halt();
		}
	}
}

LLVM GGLLVMInit() {
	LLVM llvm = {};
	llvm.context = &llvm::getGlobalContext();
	llvm.builder = new llvm::IRBuilder<>(*llvm.context);
	llvm.module = new llvm::Module("gg", *llvm.context);

	return llvm;
}

void IRCompile(LLVM &llvm);

void GGLLVMEmitProgram(const GGToken &program) {
	LLVM llvm = GGLLVMInit();

	//emit_global_functions_declarations(llvm, program);
	llvm_emit_global_definitions(llvm, program);

	llvm.module->dump();

	IRCompile(llvm);


}


//clang++ -g -O3 toy.cpp `llvm-config --cppflags --ldflags --libs core` -o toy
//-->
//	-IC:/ax_git/gg/llvm/include -IC:/ax_git/gg/llvm/include    /MP -D_CRT_SECURE_NO_DEPRECATE -D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE -D_CRT_NONSTDC_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE -D_SCL_SECURE_NO_WARNINGS 
//		-wd4146 -wd4180	-wd4244 -wd4267 -wd4291 -wd4345 -wd4351 -wd4355 -wd4503 -wd4624 -wd4722 -wd4800	-w14062 -we4238 -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS 
//		-LC:/ax_git/gg/llvm/Debug/lib -lLLVMCore -lLLVMSupport


//#include "llvm/ADT/ArrayRef.h"
//#include "llvm/LLVMContext.h"
//#include "llvm/Module.h"
//#include "llvm/Function.h"
//#include "llvm/BasicBlock.h"
//#include "llvm/Support/IRBuilder.h"
//#include <vector>
//#include <string>
//
//int main()
//{
//  llvm::LLVMContext & context = llvm::getGlobalContext();
//  llvm::Module *module = new llvm::Module("asdf", context);
//  llvm::IRBuilder<> builder(context);
//
//  llvm::FunctionType *funcType = llvm::FunctionType::get(builder.getVoidTy(), false);
//  llvm::Function *mainFunc = 
//    llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "main", module);
//  llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entrypoint", mainFunc);
//  builder.SetInsertPoint(entry);
//
//  llvm::Value *helloWorld = builder.CreateGlobalStringPtr("hello world!\n");
//
//  std::vector<llvm::Type *> putsArgs;
//  putsArgs.push_back(builder.getInt8Ty()->getPointerTo());
//  llvm::ArrayRef<llvm::Type*>  argsRef(putsArgs);
//
//  llvm::FunctionType *putsType = 
//    llvm::FunctionType::get(builder.getInt32Ty(), argsRef, false);
//  llvm::Constant *putsFunc = module->getOrInsertFunction("puts", putsType);
//
//  builder.CreateCall(putsFunc, helloWorld);
//  builder.CreateRetVoid();
//  module->dump();
//}

using namespace llvm;

// General options for llc.  Other pass-specific options are specified
// within the corresponding llc passes, and target-specific options
// and back-end code generation options are specified with the target machine.
//
static cl::opt<std::string>
	InputFilename(cl::Positional, cl::desc("<input bitcode>"), cl::init("-"));

static cl::opt<std::string>
	OutputFilename("o", cl::desc("Output filename"), cl::value_desc("filename"));

static cl::opt<unsigned>
	TimeCompilations("time-compilations", cl::Hidden, cl::init(1u),
	cl::value_desc("N"),
	cl::desc("Repeat compilation N times for timing"));

static cl::opt<bool>
	NoIntegratedAssembler("no-integrated-as", cl::Hidden,
	cl::desc("Disable integrated assembler"));

// Determine optimization level.
static cl::opt<char>
	OptLevel("O",
	cl::desc("Optimization level. [-O0, -O1, -O2, or -O3] "
	"(default = '-O2')"),
	cl::Prefix,
	cl::ZeroOrMore,
	cl::init(' '));

static cl::opt<std::string>
	TargetTriple("mtriple", cl::desc("Override target triple for module"));

static cl::opt<bool> NoVerify("disable-verify", cl::Hidden,
							  cl::desc("Do not verify input module"));

static cl::opt<bool> DisableSimplifyLibCalls("disable-simplify-libcalls",
											 cl::desc("Disable simplify-libcalls"));

static cl::opt<bool> ShowMCEncoding("show-mc-encoding", cl::Hidden,
									cl::desc("Show encoding in .s output"));

static cl::opt<bool> EnableDwarfDirectory(
	"enable-dwarf-directory", cl::Hidden,
	cl::desc("Use .file directives with an explicit directory."));

static cl::opt<bool> AsmVerbose("asm-verbose",
								cl::desc("Add comments to directives."),
								cl::init(true));

// GetFileNameRoot - Helper function to get the basename of a filename.
static inline std::string
	GetFileNameRoot(const std::string &InputFilename) {
		std::string IFN = InputFilename;
		std::string outputFilename;
		int Len = IFN.length();
		if ((Len > 2) &&
			IFN[Len-3] == '.' &&
			((IFN[Len-2] == 'b' && IFN[Len-1] == 'c') ||
			(IFN[Len-2] == 'l' && IFN[Len-1] == 'l'))) {
				outputFilename = std::string(IFN.begin(), IFN.end()-3); // s/.bc/.s/
		} else {
			outputFilename = IFN;
		}
		return outputFilename;
}

#include "llvm/CodeGen/CommandFlags.h"

static tool_output_file *GetOutputStream(const char *TargetName,
	Triple::OSType OS,
	const char *ProgName) 
{
	// If we don't yet have an output filename, make one.
	if (OutputFilename.empty()) {
		if (InputFilename == "-")
			OutputFilename = "-";
		else {
			OutputFilename = GetFileNameRoot(InputFilename);

			switch (FileType) {
			case TargetMachine::CGFT_AssemblyFile:
				if (TargetName[0] == 'c') {
					if (TargetName[1] == 0)
						OutputFilename += ".cbe.c";
					else if (TargetName[1] == 'p' && TargetName[2] == 'p')
						OutputFilename += ".cpp";
					else
						OutputFilename += ".s";
				} else
					OutputFilename += ".s";
				break;
			case TargetMachine::CGFT_ObjectFile:
				if (OS == Triple::Win32)
					OutputFilename += ".obj";
				else
					OutputFilename += ".o";
				break;
			case TargetMachine::CGFT_Null:
				OutputFilename += ".null";
				break;
			}
		}
	}

	// Decide if we need "binary" output.
	bool Binary = false;
	switch (FileType) {
	case TargetMachine::CGFT_AssemblyFile:
		break;
	case TargetMachine::CGFT_ObjectFile:
	case TargetMachine::CGFT_Null:
		Binary = true;
		break;
	}

	// Open the file.
	std::string error;
	sys::fs::OpenFlags OpenFlags = sys::fs::F_None;
	if (!Binary)
		OpenFlags |= sys::fs::F_Text;
	tool_output_file *FDOut = new tool_output_file(OutputFilename.c_str(), error,
		OpenFlags);
	if (!error.empty()) {
		errs() << error << '\n';
		delete FDOut;
		return nullptr;
	}

	return FDOut;
}

//// main - Entry point for the llc compiler.
////
//int main(int argc, char **argv) {
//	sys::PrintStackTraceOnErrorSignal();
//	PrettyStackTraceProgram X(argc, argv);
//
//	// Enable debug stream buffering.
//	EnableDebugBuffering = true;
//
//	LLVMContext &Context = getGlobalContext();
//	llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.
//
//	// Initialize targets first, so that --version shows registered targets.
//	InitializeAllTargets();
//	InitializeAllTargetMCs();
//	InitializeAllAsmPrinters();
//	InitializeAllAsmParsers();
//
//	// Initialize codegen and IR passes used by llc so that the -print-after,
//	// -print-before, and -stop-after options work.
//	PassRegistry *Registry = PassRegistry::getPassRegistry();
//	initializeCore(*Registry);
//	initializeCodeGen(*Registry);
//	initializeLoopStrengthReducePass(*Registry);
//	initializeLowerIntrinsicsPass(*Registry);
//	initializeUnreachableBlockElimPass(*Registry);
//
//	// Register the target printer for --version.
//	cl::AddExtraVersionPrinter(TargetRegistry::printRegisteredTargetsForVersion);
//
//	cl::ParseCommandLineOptions(argc, argv, "llvm system compiler\n");
//
//	// Compile the module TimeCompilations times to give better compile time
//	// metrics.
//	for (unsigned I = TimeCompilations; I; --I)
//		if (int RetVal = compileModule(argv, Context))
//			return RetVal;
//	return 0;
//}

static int compileModule(char **argv, LLVMContext &Context, Module *mod) {
	// Load the module to be compiled...
	SMDiagnostic Err;
	//std::unique_ptr<Module> M;
	//Module *mod = nullptr;
	Triple TheTriple;

	//bool SkipModule = MCPU == "help" ||
	//	(!MAttrs.empty() && MAttrs.front() == "help");

	// If user asked for the 'native' CPU, autodetect here. If autodection fails,
	// this will set the CPU to an empty string which tells the target to
	// pick a basic default.
	if (MCPU == "native" || MCPU == "")
		MCPU = sys::getHostCPUName();

	//// If user just wants to list available options, skip module loading
	//if (!SkipModule) {
	//	M.reset(ParseIRFile(InputFilename, Err, Context));
	//	mod = M.get();
	//	if (mod == nullptr) {
	//		Err.print(argv[0], errs());
	//		return 1;
	//	}

	// If we are supposed to override the target triple, do so now.
	if (!TargetTriple.empty())
		mod->setTargetTriple(Triple::normalize(TargetTriple));
	TheTriple = Triple(mod->getTargetTriple());
	//} else {
	//	TheTriple = Triple(Triple::normalize(TargetTriple));
	//}

	if (TheTriple.getTriple().empty())
		TheTriple.setTriple(sys::getDefaultTargetTriple());


	// Get the target specific parser.
	std::string Error;
	const Target *TheTarget = TargetRegistry::lookupTarget(MArch, TheTriple,
		Error);
	if (!TheTarget) {
		errs() << argv[0] << ": " << Error;
		return 1;
	}

	// Package up features to be passed to target/subtarget
	std::string FeaturesStr;
	if (MAttrs.size()) {
		SubtargetFeatures Features;
		for (unsigned i = 0; i != MAttrs.size(); ++i)
			Features.AddFeature(MAttrs[i]);
		FeaturesStr = Features.getString();
	}

	CodeGenOpt::Level OLvl = CodeGenOpt::None;
	switch (OptLevel) {
	default:
		errs() << argv[0] << ": invalid optimization level.\n";
		return 1;
	case ' ': break;
	case '0': OLvl = CodeGenOpt::None; break;
	case '1': OLvl = CodeGenOpt::Less; break;
	case '2': OLvl = CodeGenOpt::Default; break;
	case '3': OLvl = CodeGenOpt::Aggressive; break;
	}

	TargetOptions Options = InitTargetOptionsFromCodeGenFlags();
	Options.DisableIntegratedAS = NoIntegratedAssembler;
	Options.MCOptions.ShowMCEncoding = ShowMCEncoding;
	Options.MCOptions.MCUseDwarfDirectory = EnableDwarfDirectory;
	Options.MCOptions.AsmVerbose = AsmVerbose;

	std::unique_ptr<TargetMachine> target(
		TheTarget->createTargetMachine(TheTriple.getTriple(), MCPU, FeaturesStr,
		Options, RelocModel, CMModel, OLvl));
	assert(target.get() && "Could not allocate target machine!");

	// If we don't have a module then just exit now. We do this down
	// here since the CPU/Feature help is underneath the target machine
	// creation.
	//if (SkipModule)
	//	return 0;

	assert(mod && "Should have exited if we didn't have a module!");
	TargetMachine &Target = *target.get();

	if (GenerateSoftFloatCalls)
		FloatABIForCalls = FloatABI::Soft;

	// Figure out where we are going to send the output.
	std::unique_ptr<tool_output_file> Out(
		GetOutputStream(TheTarget->getName(), TheTriple.getOS(), argv[0]));
	if (!Out) return 1;

	// Build up all of the passes that we want to do to the module.
	PassManager PM;

	// Add an appropriate TargetLibraryInfo pass for the module's triple.
	TargetLibraryInfo *TLI = new TargetLibraryInfo(TheTriple);
	if (DisableSimplifyLibCalls)
		TLI->disableAllFunctions();
	PM.add(TLI);

	// Add the target data from the target machine, if it exists, or the module.
	if (const DataLayout *DL = Target.getDataLayout())
		mod->setDataLayout(DL);
	PM.add(new DataLayoutPass(mod));

	if (RelaxAll.getNumOccurrences() > 0 &&
		FileType != TargetMachine::CGFT_ObjectFile)
		errs() << argv[0]
	<< ": warning: ignoring -mc-relax-all because filetype != obj";

	{
		formatted_raw_ostream FOS(Out->os());

		AnalysisID StartAfterID = nullptr;
		AnalysisID StopAfterID = nullptr;
		const PassRegistry *PR = PassRegistry::getPassRegistry();
		if (!StartAfter.empty()) {
			const PassInfo *PI = PR->getPassInfo(StartAfter);
			if (!PI) {
				errs() << argv[0] << ": start-after pass is not registered.\n";
				return 1;
			}
			StartAfterID = PI->getTypeInfo();
		}
		if (!StopAfter.empty()) {
			const PassInfo *PI = PR->getPassInfo(StopAfter);
			if (!PI) {
				errs() << argv[0] << ": stop-after pass is not registered.\n";
				return 1;
			}
			StopAfterID = PI->getTypeInfo();
		}

		// Ask the target to add backend passes as necessary.
		if (Target.addPassesToEmitFile(PM, FOS, FileType, NoVerify,
			StartAfterID, StopAfterID)) {
				errs() << argv[0] << ": target does not support generation of this"
					<< " file type!\n";
				return 1;
		}

		// Before executing passes, print the final values of the LLVM options.
		cl::PrintOptionValues();

		PM.run(*mod);
	}

	// Declare success.
	Out->keep();


	return 0;
}

std::string exec(char* cmd) {
	FILE* pipe = _popen(cmd, "r");
	if (!pipe) return "ERROR";
	char buffer[128];
	std::string result = "";
	while(!feof(pipe)) {
		if(fgets(buffer, 128, pipe) != NULL)
			result += buffer;
	}
	_pclose(pipe);
	return result;
}
void IRCompile(LLVM &llvm)
{
	sys::PrintStackTraceOnErrorSignal();
	//PrettyStackTraceProgram X(argc, argv);

	// Enable debug stream buffering.
	EnableDebugBuffering = true;

	LLVMContext &Context = getGlobalContext();
	//llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.

	// Initialize targets first, so that --version shows registered targets.
	//InitializeAllTargets();
	LLVMInitializeNativeTarget();
	//InitializeAllTargetMCs();
	LLVMInitializeX86TargetMC();
	//InitializeAllAsmPrinters();
	LLVMInitializeNativeAsmPrinter();
	//InitializeAllAsmParsers();
	LLVMInitializeNativeAsmParser();

	// Initialize codegen and IR passes used by llc so that the -print-after,
	// -print-before, and -stop-after options work.
	PassRegistry *Registry = PassRegistry::getPassRegistry();
	initializeCore(*Registry);
	initializeCodeGen(*Registry);
	initializeLoopStrengthReducePass(*Registry);
	initializeLowerIntrinsicsPass(*Registry);
	initializeUnreachableBlockElimPass(*Registry);

	// Register the target printer for --version.
	cl::AddExtraVersionPrinter(TargetRegistry::printRegisteredTargetsForVersion);

	//cl::ParseCommandLineOptions(argc, argv, "llvm system compiler\n");
	//StartAfter = 

	FileType = TargetMachine::CGFT_ObjectFile;
	OutputFilename = "../me.obj";

	const char *name = "gg.exe";
	compileModule((char **)&name, *llvm.context, llvm.module);

	//system("link me.obj /ENTRY:main");
	system("..\\mylink.bat");

	//exec("vcvars32.bat");
	//exec("link me.obj /ENTRY:main");
	//exec("vcvars32.bat");
}