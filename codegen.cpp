#include "node.h"
#include "codegen.h"
#include "parser.hpp"

#define DEBUG true
#define EXIT false
#define OPTIMIZE true

using namespace std;

// HELPERS

void printType(Value* v) {
	std::string type_str;
	llvm::raw_string_ostream rso(type_str);
	v->getType()->print(rso);
	std::cout << "[TYPE]: " << rso.str() << "\n";
}

CodeGenBlock* findLocals(std::stack<CodeGenBlock*> &blocks, std::string& name) {
	if(blocks.empty())
        return NULL;
    CodeGenBlock* b = blocks.top();
    if(b->locals.find(name) != b->locals.end())
		return b;
    blocks.pop();
    CodeGenBlock* f = findLocals(blocks, name);
    blocks.push(b);
	return f;
}

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock& root)
{
	#if DEBUG == true
	printf("Generating code...\n");
	#endif

	/* Create the top level interpreter function to call as entry */
	vector<Type*> argTypes;
	FunctionType *ftype = FunctionType::get(Type::getVoidTy(MyContext), makeArrayRef(argTypes), false);
	mainFunction = Function::Create(ftype, GlobalValue::InternalLinkage, "main", module);
	BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", mainFunction, 0);
	
	/* Push a new variable/block context */
	pushBlock(bblock);
	root.codeGen(*this); /* emit bytecode for the toplevel block */
	ReturnInst::Create(MyContext, this->currentBlock());
	popBlock();
	
	/* Print the bytecode in a human-readable format 
	   to see if our program compiled properly
	 */
	#if DEBUG == true
	printf("Code is generated.\n");
	#endif
	// module->dump();

	legacy::PassManager pm;
	
	#if OPTIMIZE == true
	pm.add(createPromoteMemoryToRegisterPass());
	pm.add(createInstructionCombiningPass());
	pm.add(createReassociatePass());
	pm.add(createGVNPass());
	pm.add(createCFGSimplificationPass());
	#endif
	pm.add(createPrintModulePass(outs()));
	pm.run(*module);
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
	#if DEBUG == true
	printf("Running code...\n");
	#endif

	std::string* out;

	ExecutionEngine *ee = EngineBuilder( unique_ptr<Module>(module) ).create();
	
	ee->finalizeObject();
	
	if (ee->hasError()) {
		std::cout << ee->getErrorMessage();
		GenericValue v;
		return v;
	}

	vector<GenericValue> noargs;
	GenericValue v = ee->runFunction(mainFunction, noargs);
	#if DEBUG == true
	printf("Code was run.\n");
	#endif
	return v;
}

/* Compiles the AST by starting at the main function */
int CodeGenContext::compileCode() {
	#if DEBUG == true
	printf("Compiling code...\n");
	#endif

	auto TargetTriple = LLVMGetDefaultTargetTriple();
	module->setTargetTriple(TargetTriple);

	std::string Error;
	auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);
	
	#if DEBUG == true
	std::cout << "Target: " << TargetTriple << "\n";
	#endif

	// Print an error and exit if we couldn't find the requested target.
	// This generally occurs if we've forgotten to initialise the
	// TargetRegistry or we have a bogus target triple.
	if (!Target) {
		errs() << Error;
		return 1;
	}

	auto CPU = "generic";
	auto Features = "";

	TargetOptions opt;
	auto RM = Optional<Reloc::Model>();
	auto TargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

	module->setDataLayout(TargetMachine->createDataLayout());
	module->setTargetTriple(TargetTriple);

	auto Filename = "output.o";
	std::error_code EC;
	raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

	if (EC) {
		errs() << "Could not open file: " << EC.message();
		return 1;
	}

	legacy::PassManager pass;
	auto FileType = CGFT_ObjectFile;

	if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
		errs() << "TargetMachine can't emit a file of this type";
		return 1;
	}

	//auto MC = new MCContext(Target, );

	/*if (TargetMachine->addPassesToEmitMC(pass, MC, dest)) {
		errs() << "TargetMachine can't emit a file of this type";
		return 1;
	}*/

	pass.run(*module);
	dest.flush();

	#if DEBUG == true
	printf("Code was compiled.\n");
	#endif

	return 0;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(const NIdentifier& type) 
{
	if (type.name.compare("void") == 0) {
		return Type::getVoidTy(MyContext);
	} 
	else if (type.name.compare("int") == 0) {
		return Type::getInt64Ty(MyContext);
	}
	else if (type.name.compare("double") == 0) {
		return Type::getDoubleTy(MyContext);
	}
	else if (type.name.compare("string") == 0) {
		return Type::getInt64Ty(MyContext);
	}
	else if (type.name.compare("bool") == 0) {
		return Type::getInt1Ty(MyContext);
	}
	printf("\x1B[91mFAILURE\033[0m\n");
	std::cerr << "[\x1B[91mERROR\033[0m]: nonexistent type " << type.name << endl;
	#if EXIT == true
	exit(-1);
	#endif
	return Type::getVoidTy(MyContext);
}

/* -- Code Generation -- */

Value* NInteger::codeGen(CodeGenContext& context)
{
	#if DEBUG == true
	std::cout << "Creating integer: " << value << endl;
	#endif
	return ConstantInt::get(Type::getInt64Ty(MyContext), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context)
{
	#if DEBUG == true
	std::cout << "Creating double: " << value << endl;
	#endif
	return ConstantFP::get(Type::getDoubleTy(MyContext), value);
}

Value* NString::codeGen(CodeGenContext& context)
{
	#if DEBUG == true
	std::cout << "Creating string: " << value << endl;
	#endif
	return ConstantInt::get(Type::getInt64Ty(MyContext), 0, true);
}

Value* NBool::codeGen(CodeGenContext& context)
{
	#if DEBUG == true
	std::cout << "Creating bool: " << value << endl;
	#endif
	return ConstantInt::get(Type::getInt1Ty(MyContext), value, true);
}

Value* NIdentifier::codeGen(CodeGenContext& context)
{
	#if DEBUG == true
	std::cout << "Creating identifier reference: " << name << endl;
	#endif

	CodeGenBlock* b = findLocals(context.blocks, name);
	
	if (b == NULL) {
		printf("\x1B[91mFAILURE\033[0m\n");
		std::cerr << "[\x1B[91mERROR\033[0m]: undeclared variable " << name << endl;
		#if EXIT == true
		exit(-1);
		#endif
		return NULL;
	}

	return new LoadInst(b->ltypes[name], b->locals[name], name, false, context.currentBlock());
}

Value* NMethodCall::codeGen(CodeGenContext& context)
{
	Function *function = context.module->getFunction(id.name.c_str());
	if (function == NULL) {
		printf("\x1B[91mFAILURE\033[0m\n");
		std::cerr << "[\x1B[91mERROR\033[0m]: no such function " << id.name << endl;
		#if EXIT == true
		exit(-1);
		#endif
	}
	std::vector<Value*> args;
	ExpressionList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		args.push_back((**it).codeGen(context));
	}
	CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
	#if DEBUG == true
	std::cout << "Creating method call: " << id.name << endl;
	#endif
	return call;
}

Value* NBinaryOperator::codeGen(CodeGenContext& context)
{
	#if DEBUG == true
	std::cout << "Creating binary operation " << op << endl;
	#endif
	Instruction::BinaryOps instr;
	CmpInst::Predicate pred;

	switch (op) {
		case PLUS: 		instr = Instruction::Add; goto math;
		case MINUS: 	instr = Instruction::Sub; goto math;
		case MUL: 		instr = Instruction::Mul; goto math;
		case DIV: 		instr = Instruction::SDiv; goto math;

		case CEQ:		pred = CmpInst::ICMP_EQ; goto comp;
		case CNE:		pred = CmpInst::ICMP_NE; goto comp;
		case CLT:		pred = CmpInst::ICMP_SLT; goto comp;
		case CLE:		pred = CmpInst::ICMP_SLE; goto comp;
		case CGT:		pred = CmpInst::ICMP_SGT; goto comp;
		case CGE:		pred = CmpInst::ICMP_SGE; goto comp;
	}
	return NULL;
math:
	return BinaryOperator::Create(instr, lhs.codeGen(context), rhs.codeGen(context), "", context.currentBlock());
comp:
	return CmpInst::Create(AddrSpaceCastInst::OtherOps::ICmp, pred, lhs.codeGen(context), rhs.codeGen(context), "", context.currentBlock());
}

Value* NUnaryOperator::codeGen(CodeGenContext& context)
{
	#if DEBUG == true
	std::cout << "Creating unary operation " << op << endl;
	#endif

	switch (op) {
		case MINUS:
			return BinaryOperator::CreateNeg(expr.codeGen(context), "", context.currentBlock());
		case NOT:
			return BinaryOperator::CreateNot(expr.codeGen(context), "", context.currentBlock());
		default:
			return NULL;
	}
}

Value* NAssignment::codeGen(CodeGenContext& context)
{
	#if DEBUG == true
	std::cout << "Creating assignment for " << lhs.name << endl;
	#endif

	CodeGenBlock* b = findLocals(context.blocks, lhs.name);

	if (b == NULL) {
		printf("\x1B[91mFAILURE\033[0m\n");
		std::cerr << "[\x1B[91mERROR\033[0m]: undeclared variable " << lhs.name << endl;
		#if EXIT == true
		exit(-1);
		#endif
		return NULL;
	}

	Instruction::BinaryOps instr;
	switch (op) {
		case PLUSASN: 		instr = Instruction::Add; goto math;
		case MINUSASN:	 	instr = Instruction::Sub; goto math;
		case MULASN: 		instr = Instruction::Mul; goto math;
		case DIVASN: 		instr = Instruction::SDiv; goto math;
				
		default: 			return new StoreInst(rhs.codeGen(context), b->locals[lhs.name], false, context.currentBlock());
	}
	return NULL;
math:
	return new StoreInst(BinaryOperator::Create(instr, lhs.codeGen(context), rhs.codeGen(context), "", context.currentBlock()), b->locals[lhs.name], false, context.currentBlock());
}

Value* NBlock::codeGen(CodeGenContext& context)
{
	StatementList::const_iterator it;
	Value *last = NULL;
	for (it = statements.begin(); it != statements.end(); it++) {
		#if DEBUG == true
		auto type = **it;
		std::cout << "Generating code for " << typeid(type).name() << endl;
		#endif
		last = (**it).codeGen(context);
	}
	#if DEBUG == true
	std::cout << "Creating block" << endl;
	#endif
	return last;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context)
{
	#if DEBUG == true
	std::cout << "Generating code for " << typeid(expression).name() << endl;
	#endif
	return expression.codeGen(context);
}

Value* NReturnStatement::codeGen(CodeGenContext& context)
{
	#if DEBUG == true
	std::cout << "Generating return code for " << typeid(expression).name() << endl;
	#endif
	Value *returnValue = expression.codeGen(context);
	context.setCurrentReturnValue(returnValue);
	return returnValue;
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context)
{
	#if DEBUG == true
	std::cout << "Creating variable declaration " << type.name << " " << id.name << endl;
	#endif
	if (context.locals().find(id.name) != context.locals().end()) {
		printf("\x1B[91mFAILURE\033[0m\n");
		std::cerr << "[\x1B[91mERROR\033[0m]: variable already declared " << id.name << endl;
		#if EXIT == true
		exit(-1);
		#endif
		return NULL;
	}

	context.ltypes()[id.name] = typeOf(type);
	AllocaInst *alloc = new AllocaInst(context.ltypes()[id.name], 0, id.name.c_str(), context.currentBlock());
	context.locals()[id.name] = alloc;

	if (assignmentExpr != NULL) {
		NAssignment assn(id, *assignmentExpr);
		assn.codeGen(context);
	}
	return alloc;
}

Value* NExternDeclaration::codeGen(CodeGenContext& context)
{
    vector<Type*> argTypes;
    VariableList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(typeOf((**it).type));
    }
    FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.name.c_str(), context.module);
    return function;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context)
{
	vector<Type*> argTypes;
	VariableList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		argTypes.push_back(typeOf((**it).type));
	}
	FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
	Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, id.name.c_str(), context.module);
	BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", function, 0);

	context.pushBlock(bblock);

	Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;

	for (it = arguments.begin(); it != arguments.end(); it++) {
		(**it).codeGen(context);
		
		argumentValue = &*argsValues++;
		argumentValue->setName((*it)->id.name.c_str());
		StoreInst *inst = new StoreInst(argumentValue, context.locals()[(*it)->id.name], false, bblock);
	}
	
	block.codeGen(context);
	ReturnInst::Create(MyContext, context.getCurrentReturnValue(), bblock);

	context.popBlock();
	#if DEBUG == true
	std::cout << "Creating function: " << id.name << endl;
	#endif
	return function;
}

Value* NConditional::codeGen(CodeGenContext& context)
{
	BasicBlock *Then = BasicBlock::Create(MyContext, "then", context.currentBlock()->getParent());
	BasicBlock *Else = BasicBlock::Create(MyContext, "else", context.currentBlock()->getParent());
	BasicBlock *Continue = BasicBlock::Create(MyContext, "continue", context.currentBlock()->getParent());

	BranchInst::Create(Then, Else, condition.codeGen(context), context.currentBlock());

	context.pushBlock(Then);
	thenblock.codeGen(context);
	BranchInst::Create(Continue, context.currentBlock());

	Else->moveAfter(context.currentBlock());
	context.pushBlock(Else);
	elseblock.codeGen(context);
	BranchInst::Create(Continue, context.currentBlock());

	Continue->moveAfter(context.currentBlock());
	context.pushBlock(Continue);

	return NULL;
}

Value* NLoop::codeGen(CodeGenContext& context)
{
	BasicBlock *Loop = BasicBlock::Create(MyContext, "loop", context.currentBlock()->getParent());
	BasicBlock *Continue = BasicBlock::Create(MyContext, "continue", context.currentBlock()->getParent());

	BranchInst::Create(Loop, Continue, condition.codeGen(context), context.currentBlock());

	context.pushBlock(Loop);
	block.codeGen(context);
	BranchInst::Create(Loop, Continue, condition.codeGen(context), context.currentBlock());

	Continue->moveAfter(context.currentBlock());
	context.pushBlock(Continue);

	return NULL;
}