#include "node.h"
#include "codegen.h"
#include "parser.hpp"

using namespace std;

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock& root)
{
	std::cout << "Generating code...\n";
	
	/* Create the top level interpreter function to call as entry */
	vector<Type*> argTypes;
	FunctionType *ftype = FunctionType::get(Type::getVoidTy(TheContext), makeArrayRef(argTypes), false);
	mainFunction = Function::Create(ftype, GlobalValue::InternalLinkage, "main", module);
	BasicBlock *bblock = BasicBlock::Create(TheContext, "entry", mainFunction, 0);
	
	/* Push a new variable/block context */
	pushBlock(bblock);
	root.codeGen(*this); /* emit bytecode for the toplevel block */
	ReturnInst::Create(TheContext, bblock);
	popBlock();
	
	/* Print the bytecode in a human-readable format 
	   to see if our program compiled properly
	 */
	std::cout << "Code is generated.\n";
	PassManager<Module> pm;
        AnalysisManager<Module>* am = new AnalysisManager<Module>;
	pm.addPass(PrintModulePass(outs()));
	pm.run(*module, *am);
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
	std::cout << "Running code...\n";
	ExecutionEngine *ee = EngineBuilder( unique_ptr<Module>(module) ).create();
	ee->finalizeObject();
	vector<GenericValue> noargs;
	GenericValue v = ee->runFunction(mainFunction, noargs);
	std::cout << "Code was run.\n";
	return v;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(const NIdentifier& type, CodeGenContext& context)
{
	if (type.name.compare("int") == 0) {
		return Type::getInt64Ty(context.TheContext);
	}
	else if (type.name.compare("double") == 0) {
            return Type::getDoubleTy(context.TheContext);
        } else if (type.name.compare("void") == 0) {
            return Type::getVoidTy(context.TheContext);
        } else if (context.structs.find(type.name) != context.structs.end()) {
            return context.structs[type.name].type;
        }
        std::cerr<<"ERROR: no type found: "<< type.name << "\n";
	return Type::getVoidTy(context.TheContext);
}

/* -- Code Generation -- */

Value* NInteger::codeGen(CodeGenContext& context)
{
	std::cout << "Creating integer: " << value << endl;
	return ConstantInt::get(Type::getInt64Ty(context.TheContext), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context)
{
	std::cout << "Creating double: " << value << endl;
	return ConstantFP::get(Type::getDoubleTy(context.TheContext), value);
}

Value *CodeGenContext::findValue(Value *parent, const std::string &name)
{
    return locals()[name];
}

Value* NIdentifier::codeGen(CodeGenContext& context)
{
    std::function<Value *(const NIdentifier *)> value = [&context, &value](const NIdentifier *ident) -> Value * {

        std::cout<<"PPP "<<ident->name<<ident->parent<<"\n";
        if (ident->parent) {
            Value *parentV = value(ident->parent);
//             auto *v = new LoadInst(parentV, "", false, context.currentBlock());
            Type *t = parentV->getType();
            t = t->getPointerElementType();

            t->dump();
            if (t->isStructTy()) {
                StructType *st = static_cast<StructType *>(t);
                Struct &str = context.structs[st->getName()];

                            std::cout<<"    "<<st->getName().str()<<"\n";

                int id = 0;
                for (int i = 0; i < str.fields.size(); ++i) {
                    if (str.fields[i] == ident->name) {
                        id = i;
                        break;
                    }
                }

                std::cout<<"fout id in struct "<<id<<"\n";

                auto id1 = ConstantInt::get(context.TheContext, llvm::APInt(32, 0, false));
                auto id2 = ConstantInt::get(context.TheContext, llvm::APInt(32, id, false));

                return GetElementPtrInst::CreateInBounds(parentV, {id1, id2}, "bla", context.currentBlock());
//                  return parentV;
            }
        }
        return context.locals()[ident->name];
    };

    Value *v = value(this);

    if (parent) {
//         return v;
    }


	std::cout << "Creating identifier reference: " << name << endl;
	if (context.locals().find(name) == context.locals().end()) {
		std::cerr << "undeclared variable " << name << endl;
//                 exit(1);
// 		return NULL;
	}
// 	return v;
	return new LoadInst(v, "", false, context.currentBlock());
}

Value* NMethodCall::codeGen(CodeGenContext& context)
{
	Function *function = context.module->getFunction(id.name.c_str());
	if (function == NULL) {
		std::cerr << "no such function " << id.name << endl;
	}
	std::vector<Value*> args;
	ExpressionList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		args.push_back((**it).codeGen(context));
	}
	CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
	std::cout << "Creating method call: " << id.name << endl;
	return call;
}

Value* NBinaryOperator::codeGen(CodeGenContext& context)
{
	std::cout << "Creating binary operation " << op << endl;
	Instruction::BinaryOps instr;
	switch (op) {
		case TPLUS: 	instr = Instruction::Add; goto math;
		case TMINUS: 	instr = Instruction::Sub; goto math;
		case TMUL: 		instr = Instruction::Mul; goto math;
		case TDIV: 		instr = Instruction::SDiv; goto math;
				
		/* TODO comparison */
	}

	return NULL;
math:
	return BinaryOperator::Create(instr, lhs.codeGen(context), 
		rhs.codeGen(context), "", context.currentBlock());
}

Value* NAssignment::codeGen(CodeGenContext& context)
{
	std::cout << "Creating assignment for " << lhs.name << endl;
	if (context.locals().find(lhs.name) == context.locals().end()) {
		std::cerr << "undeclared variable " << lhs.name << endl;
		return NULL;
	}
	return new StoreInst(rhs.codeGen(context), context.locals()[lhs.name], false, context.currentBlock());
}

Value* NBlock::codeGen(CodeGenContext& context)
{
	StatementList::const_iterator it;
	Value *last = NULL;
	for (it = statements.begin(); it != statements.end(); it++) {
		std::cout << "Generating code for " << typeid(**it).name() << endl;
		last = (**it).codeGen(context);
	}
	std::cout << "Creating block" << endl;
	return last;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating code for " << typeid(expression).name() << endl;
	return expression.codeGen(context);
}

Value* NReturnStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating return code for " << typeid(expression).name() << endl;
	Value *returnValue = expression.codeGen(context);
	context.setCurrentReturnValue(returnValue);
	return returnValue;
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context)
{
	std::cout << "Creating variable declaration " << type.name << " " << id.name << endl;
	AllocaInst *alloc = new AllocaInst(typeOf(type, context), id.name.c_str(), context.currentBlock());
	context.locals()[id.name] = alloc;
	if (!expressions.empty()) {
		NAssignment assn(id, *expressions.front());
		assn.codeGen(context);
	}
	return alloc;
}

Value* NExternDeclaration::codeGen(CodeGenContext& context)
{
    vector<Type*> argTypes;
    VariableList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(typeOf((**it).type, context));
    }
    FunctionType *ftype = FunctionType::get(typeOf(type, context), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.name.c_str(), context.module);
    return function;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context)
{
	vector<Type*> argTypes;
	VariableList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		argTypes.push_back(typeOf((**it).type, context));
	}
	FunctionType *ftype = FunctionType::get(typeOf(type, context), makeArrayRef(argTypes), false);
	Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, id.name.c_str(), context.module);
	BasicBlock *bblock = BasicBlock::Create(context.TheContext, "entry", function, 0);

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
	ReturnInst::Create(context.TheContext, context.getCurrentReturnValue(), bblock);

	context.popBlock();
	std::cout << "Creating function: " << id.name << endl;
	return function;
}

Value* NStructDeclaration::codeGen(CodeGenContext& context)
{
    std::cout << "Creating struct declaration " << " " << id.name << endl;
    vector<Type *> argTypes;
    for (auto it = elements.begin(); it != elements.end(); it++) {
        argTypes.push_back(typeOf((**it).type, context));
        std::cout<<"    with arg " << (*it)->type.name << " " <<(*it)->id.name<<"\n";
    }
    StructType *type = StructType::create(context.TheContext, argTypes, id.name.c_str());
    Struct &str = context.structs[id.name];
    str.type = type;
    str.fields.reserve(elements.size());
    for (auto &&el: elements) {
        str.fields.push_back(el->id.name);
    }
    return 0;
}
