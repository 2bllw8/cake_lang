#ifndef cake_vm_h
#define cake_vm_h

#include "chunk.h"
#include "common.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
	ObjClosure *closure;
	uint8_t *ip;
	Value *slots;
} CallFrame;

typedef struct {
	CallFrame frames[FRAMES_MAX];
	int frame_count;
	Value stack[STACK_MAX];
	Value *tos;

	Table globals;
	Table strings;
	ObjString *init_string;
	ObjUpvalue *open_upvalues;
	Obj *objects;

	size_t allocated_bytes;
	size_t next_gc;

	int grey_count;
	int grey_capacity;
	Obj **grey_stack;
} VM;

typedef enum {
	INTERPRET_OK,
	INTERPRET_COMPILE_ERROR,
	INTERPRET_RUNTIME_ERROR,
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();

InterpretResult interpret(const char *source);
void push(Value value);
bool pushNonNil(Value value);
Value pop();

#endif