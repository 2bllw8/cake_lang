#ifndef cake_debug_h
#define cake_debug_h

#include "chunk.h"

#define PRINT_EXEC_LINE fprintf(stderr, "\t>> %d\n", __LINE__)

void disassembleChunk(Chunk *chunk, const char *name);
int disassembleInstruction(Chunk *chunk, int offset);

#endif