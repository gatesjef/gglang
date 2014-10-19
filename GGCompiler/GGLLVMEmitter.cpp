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

static const bool SIGNED = true;

llvm::Value *emit_rvalue_unary_op(LLVM &llvm, const GGToken &token) {
	assert(token.num_subtokens == 2);
	const GGToken &op_token = token.subtokens[0];
	const GGToken &lhsToken = token.subtokens[1];

	switch(*op_token.substring.start) {
	case '+': {
		return emit_rvalue_expression(llvm, lhsToken);
	}
	case '-': {
		llvm::Value *value = emit_rvalue_expression(llvm, lhsToken);
		llvm::Value *neg1 = llvm::ConstantInt::get(llvm::IntegerType::get(*llvm.context, 32), -1, SIGNED);
		return llvm.builder->CreateMul(value, neg1);
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

static llvm::AllocaInst *CreateEntryBlockAlloca(LLVM &llvm, llvm::Function *TheFunction, const GGToken &identifier, const GGToken &type) {
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());

  llvm::StringRef name = to_string_ref(identifier.substring);

  llvm::Type *llvm_type = get_type(llvm, type);

  return TmpB.CreateAlloca(llvm_type, 0, name);
}

void llvm_emit_local_variable(LLVM &llvm, llvm::Function *function, const GGToken &local_variable) {
  assert(local_variable.token == TOKEN_COMPOUND_VARIABLE_DEFINITION);
  assert(local_variable.num_subtokens == 3);
  GGToken &type				= local_variable.subtokens[0];
  GGToken &identifier			= local_variable.subtokens[1];
  GGToken &value_expression	= local_variable.subtokens[2];

  llvm::Value *value = emit_rvalue_expression(llvm, value_expression);

  llvm::AllocaInst *alloca = CreateEntryBlockAlloca(llvm, function, identifier, type);
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

void emit_paramater_bindings(LLVM &llvm, const GGToken &function_params) {

}


void llvm_emit_function_definition(LLVM &llvm, const GGToken &function_definition) {
  assert(function_definition.token == TOKEN_COMPOUND_FUNCTION_DEFINITION);
  assert(function_definition.num_subtokens == 4);
  const GGToken &function_body      = function_definition.subtokens[3];
  //llvm::Function *function = llvm_emit_function_prototype(llvm, function_definition);

  assert(function_definition.token == TOKEN_COMPOUND_FUNCTION_DEFINITION);
  assert(function_definition.num_subtokens == 4);
  const GGToken &function_return_type = function_definition.subtokens[0];
  const GGToken &function_identifier  = function_definition.subtokens[1];
  const GGToken &function_params      = function_definition.subtokens[2];

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

  llvm::BasicBlock *entry = llvm::BasicBlock::Create(*llvm.context, "entrypoint", function);
  llvm.builder->SetInsertPoint(entry);

  int i = 0;
  for (llvm::Function::arg_iterator AI = function->arg_begin(); i < function_params.num_subtokens; ++AI, ++i) {
    const GGToken &param_type = function_params.subtokens[i].subtokens[0];
    const GGToken &param_identifier = function_params.subtokens[i].subtokens[1];
    //AI->setName(name);

    llvm::StringRef name = to_string_ref(param_identifier.substring);

    llvm::AllocaInst *alloca = CreateEntryBlockAlloca(llvm, function, param_identifier, param_type);
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
  llvm::Value *value = new llvm::GlobalVariable(type, false, llvm::GlobalValue::CommonLinkage);
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

void GGLLVMEmitProgram(const GGToken &program) {
  LLVM llvm = GGLLVMInit();

  //emit_global_functions_declarations(llvm, program);
  llvm_emit_global_definitions(llvm, program);

  llvm.module->dump();
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