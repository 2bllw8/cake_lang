#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "include/chunk.h"
#include "include/common.h"
#include "include/debug.h"
#include "include/io.h"
#include "include/vm.h"

static void repl()
{
	char line[1024];
	while (1) {
		printf("> ");
		if (!fgets(line, sizeof(line), stdin)) {
			printf("\n");
			break;
		}
		interpret(line);
	}
}

static void runFile(const char *path)
{
	char *source = readFile(path);
	InterpretResult result = interpret(source);
	free(source);

	if (result == INTERPRET_COMPILE_ERROR)
		exit(65);
	if (result == INTERPRET_RUNTIME_ERROR)
		exit(70);
}

int main(int argc, const char *argv[])
{
	initVM();

	switch (argc) {
	case 1:
		repl();
		break;
	case 2:
		runFile(argv[1]);
		break;
	default:
		fprintf(stderr, "Usage: ccake [path]\n");
	}

	freeVM();
	return 0;
}