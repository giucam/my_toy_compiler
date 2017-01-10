#include <fstream>

#include "llvm/Support/raw_os_ostream.h"

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
	BasicBlock *bblock = BasicBlock::Create(TheContext, "entry", 0, 0);
	
	/* Push a new variable/block context */
	pushBlock(bblock);
	root.codeGen(*this); /* emit bytecode for the toplevel block */
	ReturnInst::Create(TheContext, bblock);
	popBlock();

        mainFunction = module->getFunction("main");
	
	/* Print the bytecode in a human-readable format 
	   to see if our program compiled properly
	 */
	std::cout << "Code is generated.\n";
	PassManager<Module> pm;
        AnalysisManager<Module>* am = new AnalysisManager<Module>;
	pm.addPass(PrintModulePass(outs()));
	pm.run(*module, *am);

        std::ofstream StdOutputFile("out.ll");
        raw_os_ostream OutputFile(StdOutputFile);
        module->print(OutputFile, nullptr);
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
	if (type.name.compare("i32") == 0) {
		return Type::getInt32Ty(context.TheContext);
	}
	else if (type.name.compare("double") == 0) {
            return Type::getDoubleTy(context.TheContext);
        } else if (type.name.compare("void") == 0) {
            return Type::getVoidTy(context.TheContext);
        } else if (type.name.compare("string") == 0) {
            return Type::getInt8PtrTy(context.TheContext);
        } else if (context.structs.find(type.name) != context.structs.end()) {
            return context.structs[type.name].type;
        }
        std::cerr<<"error: type '" << type.name << "' was not declared in this scope.\n";
        exit(1);
}

Value *CodeGenContext::findValue(const NIdentifier &ident, const NIdentifier *parent)
{
    if (parent) {
        Value *parentV = findValue(*parent, parent->parent);
        Type *t = parentV->getType();
        t = t->getPointerElementType();

        t->dump();
        if (t->isStructTy()) {
            StructType *st = static_cast<StructType *>(t);
            Struct &str = structs[st->getName()];

            int id = -1;
            for (int i = 0; i < str.fields.size(); ++i) {
                if (str.fields[i] == ident.name) {
                    id = i;
                    break;
                }
            }
            if (id == -1) {
                std::cerr << "error: struct '" << st->getName().str() << "' has no member named '" << ident.name << "'.\n";
                exit(1);
            }

            std::cout<<"fout id in struct "<<id<<"\n";

            auto id1 = ConstantInt::get(TheContext, llvm::APInt(32, 0, false));
            auto id2 = ConstantInt::get(TheContext, llvm::APInt(32, id, false));

            return GetElementPtrInst::CreateInBounds(parentV, {id1, id2}, "", currentBlock()->block);
        }
    }

    auto it = locals().find(ident.name);
    if (it == locals().end()) {
        std::cerr << "error: '" << ident.name << "' was not declared in this scope.\n";
        exit(1);
    }
    return it->second;
}

/* -- Code Generation -- */

Value* NInteger::codeGen(CodeGenContext& context)
{
	std::cout << "Creating integer: " << value << endl;
	return ConstantInt::get(Type::getInt32Ty(context.TheContext), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context)
{
	std::cout << "Creating double: " << value << endl;
	return ConstantFP::get(Type::getDoubleTy(context.TheContext), value);
}

Value* NString::codeGen(CodeGenContext& context)
{
    std::cout << "Creating string: \"" << value << "\", (";
    for (auto it = value.begin(); it != value.end();) {
        char c = *it;
        if (c == '\n') std::cout << "\\n";
        else std::cout << c;
        if (++it != value.end()) {
            std::cout << ", ";
        } else {
            break;
        }
    }
    std::cout << ")\n";
    auto constant = ConstantDataArray::getString(context.TheContext, value);
    auto var = new GlobalVariable(*context.module, ArrayType::get(Type::getInt8Ty(context.TheContext), value.length() + 1), true, llvm::GlobalValue::PrivateLinkage, constant, ".str");

    auto id1 = ConstantInt::get(context.TheContext, llvm::APInt(32, 0, false));
    auto id2 = ConstantInt::get(context.TheContext, llvm::APInt(32, 0, false));

    return GetElementPtrInst::CreateInBounds(var, {id1, id2}, "", context.currentBlock()->block);
}

Value* NIdentifier::codeGen(CodeGenContext& context)
{
    return new LoadInst(context.findValue(*this, parent), "", false, context.currentBlock()->block);
}

Value* NMethodCall::codeGen(CodeGenContext& context)
{
	Function *function = context.module->getFunction(id.name.c_str());
	if (function == NULL) {
		std::cerr << "no such function " << id.name << endl;
	}

	std::vector<Value*> args;
        args.resize(function->getArgumentList().size());

        auto &funcList = function->getArgumentList();
        auto findId = [&](const NIdentifier &id) -> int {
            for (auto &&a: funcList) {
                if (a.getName() == id.name) {
                    return a.getArgNo();
                }
            }
            return -1;
        };

	for (auto it = arguments.begin(); it != arguments.end(); it++) {
            int index = findId((*it)->lhs);
            if (index == -1) {
                std::cerr << "Unexpected argument '" << (*it)->lhs.name << "' when calling function '" << id.name << "(";
                for (auto i = funcList.begin(); i != funcList.end();) {
                    auto &&a = *i;

                    std::cerr << a.getName().str();
                    if (++i != funcList.end()) {
                        std::cerr << ", ";
                    }
                }
                std::cerr << ")'.\n";
                exit(1);
            }

            args[index] = (*it)->rhs.codeGen(context);
	}
	CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock()->block);
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
		rhs.codeGen(context), "", context.currentBlock()->block);
}

Value* NAssignment::codeGen(CodeGenContext& context)
{
    std::cout << "Creating assignment for " << lhs.name << endl;
    auto parent = context.currentBlock()->currentId;
    return new StoreInst(genRhs(context), context.findValue(lhs, parent), false, context.currentBlock()->block);
}

Value *NAssignment::genRhs(CodeGenContext &context)
{
    if (!rhsValue) {
        rhsValue = rhs.codeGen(context);
    }
    return rhsValue;
}

Value* NBlock::codeGen(CodeGenContext& context)
{
	StatementList::const_iterator it;
	Value *last = NULL;
	for (it = statements.begin(); it != statements.end(); it++) {
		std::cout << "block: Generating code for " << typeid(**it).name() << endl;
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
    const int numExpressions = expressions.size();
    std::cout << "Creating variable declaration " << id.name << endl;

    Type *t = [&]() {
        if (!type) {
            if (numExpressions == 0) {
                std::cerr << "error: missing type or initializer when declaring variable '" << id.name <<"'.\n";
                exit(1);
            }

            auto value = expressions.front()->genRhs(context);
            return value->getType();
        }
        return typeOf(*type, context);
    }();

    AllocaInst *alloc = new AllocaInst(t, id.name.c_str(), context.currentBlock()->block);
    context.locals()[id.name] = alloc;

    if (t->isStructTy()) {
        Struct &str = context.structs[type->name];
        if (numExpressions != str.fields.size()) {
            std::cerr << "error: wrong number of initializers passed when declaring variable '" << id.name << "' of type '" << type->name << "'.\n";
            exit(1);
        }

        context.currentBlock()->currentId = &id;
        for (int i = 0; i < expressions.size(); ++i) {
            expressions[i]->codeGen(context);
        }
        context.currentBlock()->currentId = nullptr;
    } else if (numExpressions == 1) {
        expressions.front()->codeGen(context);
    } else if (numExpressions != 0) {
        std::cerr << "error: wrong number of initializers passed when declaring variable '" << id.name << "' of type '" << type->name << "'.\n";
        exit(1);
    }
    return alloc;
}

Value* NExternDeclaration::codeGen(CodeGenContext& context)
{
    vector<Type*> argTypes;
    VariableList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(typeOf(*(**it).type, context));
    }
    FunctionType *ftype = FunctionType::get(typeOf(type, context), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.name.c_str(), context.module);

    Value *argumentValue;
    Function::arg_iterator argsValues = function->arg_begin();
    for (auto it = arguments.begin(); it != arguments.end(); it++) {
        argumentValue = &*argsValues++;
        argumentValue->setName((*it)->id.name.c_str());
    }

    return function;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context)
{
	vector<Type*> argTypes;
	VariableList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		argTypes.push_back(typeOf(*(**it).type, context));
	}
	FunctionType *ftype = FunctionType::get(typeOf(type, context), makeArrayRef(argTypes), false);
	Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.name.c_str(), context.module);
	BasicBlock *bblock = BasicBlock::Create(context.TheContext, "entry", function, 0);

	context.pushBlock(bblock);

	Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;

	for (it = arguments.begin(); it != arguments.end(); it++) {
		argumentValue = &*argsValues++;
		argumentValue->setName((*it)->id.name.c_str());

                (**it).codeGen(context);
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
        argTypes.push_back(typeOf(*(**it).type, context));
        std::cout<<"    with arg " << (*it)->type->name << " " <<(*it)->id.name<<"\n";
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
