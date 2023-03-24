#include <stack>
#include <typeinfo>
#include <llvm/Pass.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Bitstream/BitstreamReader.h>
#include <llvm/Bitstream/BitstreamWriter.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>

using namespace llvm;

class NBlock;

static LLVMContext MyContext;

class CodeGenBlock {
public:
    BasicBlock *block;
    Value *returnValue;
    std::map<std::string, Value*> locals;
    std::map<std::string, Type*> ltypes;
};

class CodeGenContext {
    Function *mainFunction;

public:

    std::stack<CodeGenBlock *> blocks;
    Module *module;

    CodeGenContext() { 
        module = new Module("main", MyContext);
    }
    
    void generateCode(NBlock& root);
    GenericValue runCode();
    int compileCode();
    std::map<std::string, Value*>& locals() { return blocks.top()->locals; }
    std::map<std::string, Type*>& ltypes() { return blocks.top()->ltypes; }
    BasicBlock *currentBlock() { return blocks.top()->block; }
    void pushBlock(BasicBlock *block) { blocks.push(new CodeGenBlock()); blocks.top()->returnValue = NULL; blocks.top()->block = block; }
    void popBlock() { CodeGenBlock *top = blocks.top(); blocks.pop(); delete top; }
    void setCurrentReturnValue(Value *value) { blocks.top()->returnValue = value; }
    Value* getCurrentReturnValue() { return blocks.top()->returnValue; }
};
