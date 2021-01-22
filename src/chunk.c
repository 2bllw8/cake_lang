#include "include/chunk.h"
#include "include/memory.h"
#include "include/value.h"
#include "include/vm.h"

void initChunk(Chunk *chunk)
{
	chunk->capacity = 0;
	chunk->count = 0;
	chunk->lines = 0;
	chunk->code = 0;
	initValueArray(&chunk->constants);
}

void freeChunk(Chunk *chunk)
{
	FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
	FREE_ARRAY(uint8_t, chunk->lines, chunk->capacity);
	freeValueArray(&chunk->constants);
	initChunk(chunk);
}

void writeChunk(Chunk *chunk, uint8_t byte, int line)
{
	if (chunk->capacity < chunk->count + 1) {
		int old_capacity = chunk->capacity;
		chunk->capacity = GROW_CAPACITY(old_capacity);
		chunk->code =
			GROW_ARRAY(uint8_t, chunk->code, old_capacity, chunk->capacity);
		chunk->lines =
			GROW_ARRAY(int, chunk->lines, old_capacity, chunk->capacity);
	}

	chunk->code[chunk->count] = byte;
	chunk->lines[chunk->count] = line;
	chunk->count++;
}

int addConstant(Chunk *chunk, Value value)
{
	// Push (and the pop) on the stack for GC
	push(value);
	writeValueArray(&chunk->constants, value);
	pop();
	return chunk->constants.count - 1;
}