#include <iostream>
#include "codegen.h"
#include "node.h"

using namespace std;

bool JIT = false;

extern int yyparse();
extern NBlock* programBlock;

void createCoreFunctions(CodeGenContext& context);

int main(int argc, char **argv)
{
	FILE* fp;

	if (argc > 1) {
		if (!strcmp(argv[1], "run")) {
			JIT = true;
			fp = freopen(argv[2], "r", stdin);
		} else {
			fp = freopen(argv[1], "r", stdin);
		}
	}

	printf("[\x1B[94mACID\033[0m]: Parsing Code...        ");
	yyparse();
	printf("\x1B[92mSUCCESS\033[0m\n");

	#if DEBUG == true
	cout << programBlock << endl;
	#endif
	
	printf("[\x1B[94mACID\033[0m]: Generating Bytecode... ");

	InitializeAllTargetInfos();
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmParsers();
	InitializeAllAsmPrinters();

	CodeGenContext context;
	createCoreFunctions(context);
	context.generateCode(*programBlock);

	printf("\x1B[92mSUCCESS\033[0m\n");

	if (JIT) {
		printf("[\x1B[94mACID\033[0m]: Running Code\n");
		context.runCode();
		printf("[\x1B[94mACID\033[0m]: Code Finished\n");
	} else {
		printf("[\x1B[94mACID\033[0m]: Compiling Objects...   ");
		context.compileCode();
		printf("\x1B[92mSUCCESS\033[0m\n");
	}
	
	printf("[\x1B[94mACID\033[0m]: \x1B[95mExiting\033[0m\n");

	fclose(fp);

	return 0;
}

