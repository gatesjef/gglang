// gg_llvm.gg

#include "precompile.h"
#include "gg.h"



//llvm_emit_global(LLVM &llvm, Substring type, Substring name, Substring value)
//llvm_emit_local_variable() {
//	
//}

const int MAX_PARAMS = 128;

struct LLVM {
	llvm::Module *module;
	llvm::IRBuilder<> *builder;
	llvm::LLVMContext *context;
};

llvm::StringRef to_string_ref(const SubString &substring) {
	return llvm::StringRef(substring.start, substring.length);
}

llvm::ArrayRef<llvm::Value *> to_array_ref(llvm::Value **values, int num_values) {
	return llvm::ArrayRef<llvm::Value *>(values, num_values);
}

llvm::ArrayRef<llvm::Type *> to_array_ref(llvm::Type **values, int num_values) {
	return llvm::ArrayRef<llvm::Type *>(values, num_values);
}

llvm::Value *emit_expression(LLVM &llvm, const ParseOutput &expression);

int emit_function_call_params(LLVM &llvm, const ParseOutput &params, llvm::Value **values, int maxParams)
{
	assert(params.num_subtokens < maxParams);
	for(int i = 0; i< params.num_subtokens; ++i) {
		values[i] = emit_expression(llvm, params.subtokens[i]);
	}

	return params.num_subtokens;
}

llvm::Value *emit_expression_function_call(LLVM &llvm, ParseOutput &function_call) {
	assert(function_call.token == TOKEN_COMPOUND_FUNCTION_CALL);
	assert(function_call.num_subtokens == 2);
	ParseOutput &function_identifier = function_call.subtokens[0];
	ParseOutput &function_params = function_call.subtokens[1];

	llvm::StringRef name = to_string_ref(function_identifier.substring);
	llvm::Function *callee = llvm.module->getFunction(name);

	assert(function_params.num_subtokens == callee->arg_size());

	llvm::Value *param_expressions[MAX_PARAMS];
	int num_params = emit_function_call_params(llvm, function_params, param_expressions, MAX_PARAMS);

	llvm::ArrayRef<llvm::Value *> ref_params = to_array_ref(param_expressions, num_params);
	llvm::Value *retval = llvm.builder->CreateCall(callee, ref_params, "calltmp");
	assert(retval);
	return retval;
}

void emit_local_function_call(LLVM &llvm, ParseOutput &function_call) {
	assert(function_call.token == TOKEN_COMPOUND_FUNCTION_CALL);
	assert(function_call.num_subtokens == 2);
	ParseOutput &function_identifier = function_call.subtokens[0];
	ParseOutput &function_params = function_call.subtokens[1];

	llvm::StringRef name = to_string_ref(function_identifier.substring);
	llvm::Function *callee = llvm.module->getFunction(name);

	assert(function_params.num_subtokens == callee->arg_size());

	llvm::Value *param_expressions[MAX_PARAMS];
	int num_params = emit_function_call_params(llvm, function_params, param_expressions, MAX_PARAMS);

	if (num_params == 0)
	{
		llvm::Value *retval = llvm.builder->CreateCall(callee, "calltmp");
		assert(retval != NULL);
	}
	else
	{
		llvm::ArrayRef<llvm::Value *> ref_params = to_array_ref(param_expressions, num_params);
		llvm::Value *retval = llvm.builder->CreateCall(callee, ref_params, "calltmp");
		assert(retval != NULL);
	}
}

int get_integer_type_num_bits(const ParseOutput &integer_type) {
	//TODO
	return 32;
}

//llvm::Value *emit_integer_literal(LLVM &llvm, const ParseOutput &integer_literal) {
//	assert(integer_literal.token == TOKEN_COMPOUND_LITERAL_INTEGER);
//	assert(integer_literal.num_subtokens == 2);
//	ParseOutput &integer_value = integer_literal.subtokens[0];
//	ParseOutput &integer_type  = integer_literal.subtokens[1];
//	int num_bits = get_integer_type_num_bits(integer_type);
//
//	llvm::IntegerType *type = llvm::IntegerType::get(*llvm.context, num_bits);
//	llvm::StringRef str = to_string_ref(integer_value.substring);
//
//	llvm::Value *retval = llvm::ConstantInt::get(type, str, 10);
//	return retval;
//}

llvm::Value *emit_integer_literal(LLVM &llvm, const ParseOutput &integer_literal) {
	assert(integer_literal.token == TOKEN_LITERAL_INTEGER);
	int num_bits = get_integer_type_num_bits(integer_literal);

	llvm::IntegerType *type = llvm::IntegerType::get(*llvm.context, num_bits);
	llvm::StringRef str = to_string_ref(integer_literal.substring);
	
	llvm::Value *retval = llvm::ConstantInt::get(type, str, 10);
	return retval;
}

llvm::Value *emit_expression(LLVM &llvm, const ParseOutput &expression) {
	switch(expression.token) {
		//case EXPRESSION_BINARY_OP;
		//case EXPRESSION_UNARY_OP;
		//case EXPRESSION_FUNCTION_CALL;
	//case TOKEN_COMPOUND_LITERAL_INTEGER:
	//case TOKEN_IDENTIFIER:
	//	return emit_variable(llvm, expression);
	case TOKEN_LITERAL_INTEGER:
		return emit_integer_literal(llvm, expression);
		//case EXPRESSION_STRING_LITERAL;
		//case EXPRESSION_FLOAT_LITERAL;
	default:
		halt();
	}

	return NULL;
}

void emit_local_return(LLVM &llvm, ParseOutput &return_statement) {
	assert(return_statement.token == TOKEN_COMPOUND_RETURN);
	assert(return_statement.num_subtokens == 1);
	ParseOutput &return_expression = return_statement.subtokens[0];

	llvm::Value *retval = emit_expression(llvm, return_expression);
	llvm.builder->CreateRet(retval);
}

llvm::Type *type_get(LLVM &llvm, const ParseOutput &type) {
	//TODO
	return llvm::IntegerType::get(*llvm.context, 32);
}

static llvm::AllocaInst *CreateEntryBlockAlloca(LLVM &llvm, llvm::Function *TheFunction, const ParseOutput &identifier, const ParseOutput &type) {
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());

  llvm::StringRef name = to_string_ref(identifier.substring);

  llvm::Type *llvm_type = type_get(llvm, type);

  return TmpB.CreateAlloca(llvm_type, 0, name);
}

void emit_local_variable(LLVM &llvm, llvm::Function *function, const ParseOutput &local_variable) {
	assert(local_variable.token == TOKEN_COMPOUND_VARIABLE);
	assert(local_variable.num_subtokens == 3);
	ParseOutput &identifier			= local_variable.subtokens[0];
	ParseOutput &type				= local_variable.subtokens[1];
	ParseOutput &value_expression	= local_variable.subtokens[2];

	llvm::Value *value = emit_expression(llvm, value_expression);

	llvm::AllocaInst *alloca = CreateEntryBlockAlloca(llvm, function, identifier, type);
	llvm.builder->CreateStore(value, alloca);
}

void llvm_emit_function_body(LLVM &llvm, llvm::Function *function, const ParseOutput &function_body) {
	llvm::BasicBlock *entry = llvm::BasicBlock::Create(*llvm.context, "entrypoint", function);
	llvm.builder->SetInsertPoint(entry);
	//llvm::BasicBlock *BB = BasicBlock::Create(*llvm.context, "entry", function);
	//llvm.builder->SetInsertPoint(BB);

	for(int i = 0; i < function_body.num_subtokens; ++i) {
		ParseOutput &subtoken = function_body.subtokens[i];
		switch(subtoken.token) {
			case TOKEN_COMPOUND_VARIABLE:
				emit_local_variable(llvm, function, subtoken);
				break;
			case TOKEN_COMPOUND_FUNCTION_CALL:
				emit_local_function_call(llvm, subtoken);
				break;
			case TOKEN_COMPOUND_RETURN:
				emit_local_return(llvm, subtoken);
				break;
			default: 
				halt();
		}
	}

	//verifyFunction(*function);
}

enum {
	FIXED_ARGS = false,
	VAR_ARGS = true,
};

llvm::Type *emit_param_type(LLVM &llvm, const ParseOutput &param)
{
	assert(param.token == TOKEN_COMPOUND_FUNCTION_PARAM);
	assert(param.num_subtokens == 2);
	const ParseOutput &param_type = param.subtokens[0];
	const ParseOutput &param_identifier = param.subtokens[1];
	return type_get(llvm, param_type);
}

int get_param_types(LLVM &llvm, const ParseOutput &params, llvm::Type **types, int maxParams)
{
	assert(params.num_subtokens < maxParams);
	for(int i = 0; i< params.num_subtokens; ++i) {
		types[i] = emit_param_type(llvm, params.subtokens[i]);
	}

	return params.num_subtokens;
}


llvm::Function *llvm_emit_function_prototype(LLVM &llvm, const ParseOutput &function_definition) {
	assert(function_definition.token == TOKEN_COMPOUND_FUNCTION);
	assert(function_definition.num_subtokens == 4);
	const ParseOutput &function_return_type = function_definition.subtokens[0];
	const ParseOutput &function_identifier  = function_definition.subtokens[1];
	const ParseOutput &function_params      = function_definition.subtokens[2];

	llvm::Type *retval_type = type_get(llvm, function_return_type);

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
	}

	llvm::StringRef name = to_string_ref(function_identifier.substring);

	//llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, llvm.module);
	llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, llvm.module);
	assert(function);
	return function;
}

// lookup_function(function_definition);

void llvm_emit_function(LLVM &llvm, const ParseOutput &function_definition) {
	assert(function_definition.token == TOKEN_COMPOUND_FUNCTION);
	assert(function_definition.num_subtokens == 4);
	const ParseOutput &function_body      = function_definition.subtokens[3];
	llvm::Function *function = llvm_emit_function_prototype(llvm, function_definition);
	//llvm::Function *function = lookup_function(function_definition);
	llvm_emit_function_body(llvm, function, function_body);
}

void emit_global_functions_declarations(LLVM &llvm, const ParseOutput &program) {
	for(int i = 0; i < program.num_subtokens; ++i) {
		const ParseOutput &subtoken = program.subtokens[i];
		if (subtoken.token == TOKEN_COMPOUND_FUNCTION) {
			llvm_emit_function_prototype(llvm, subtoken);
		}
	}
}

void emit_global_definitions(LLVM &llvm, const ParseOutput &program) {
	for(int i = 0; i < program.num_subtokens; ++i) {
		const ParseOutput &subtoken = program.subtokens[i];
		if (subtoken.token == TOKEN_COMPOUND_VARIABLE) {
			//llvm_emit_global_variable(llvm, subtoken);
		} else if (subtoken.token == TOKEN_COMPOUND_FUNCTION) {
			llvm_emit_function(llvm, subtoken);
		} else if (subtoken.token == TOKEN_COMMENT) {
			//...
		} else {
			halt();
		}
	}
}

LLVM llvm_init() {
	LLVM llvm = {};
	llvm.context = &llvm::getGlobalContext();
	llvm.builder = new llvm::IRBuilder<>(*llvm.context);
	llvm.module = new llvm::Module("gg", *llvm.context);

	return llvm;
}

void llvm_emit_program(const ParseOutput &program) {
	LLVM llvm = llvm_init();

	emit_global_functions_declarations(llvm, program);
	emit_global_definitions(llvm, program);

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