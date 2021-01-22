#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "include/chunk.h"
#include "include/common.h"
#include "include/compiler.h"
#include "include/memory.h"
#include "include/object.h"
#include "include/value.h"
#include "include/vm.h"
#include "include/stdlib.h"
#include "include/table.h"

#ifdef DEBUG_TRACE_EXECUTION
#include "include/debug.h"
#endif

VM vm;

static Value peek(int distance)
{
	return vm.tos[-1 - distance];
}

static bool isFalsey(Value value)
{
	return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value)) ||
		   (IS_FLOAT(value) && !AS_FLOAT(value)) ||
		   (IS_INT(value) && !AS_INT(value));
}

static void concatenate()
{
	// Use peek and later pop instead of pop for GC
	ObjString *b = AS_STRING(peek(0));
	ObjString *a = AS_STRING(peek(1));

	int length = a->length + b->length;
	char *chars = ALLOCATE(char, length + 1);
	memcpy(chars, a->chars, a->length);
	memcpy(chars + a->length, b->chars, b->length);
	chars[length] = '\0';

	ObjString *result = takeString(chars, length);
	pop();
	pop();
	push(OBJ_VAL(result));
}

static void resetStack()
{
	vm.tos = vm.stack;
	vm.frame_count = 0;
	vm.open_upvalues = 0;
}

static void runtimeError(const char *format, ...)
{
	va_list args;
	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	fputs("\n", stderr);

	// Stack trace
	for (int i = vm.frame_count - 1; i >= 0; i--) {
		CallFrame *frame = &vm.frames[i];
		ObjFunction *function = frame->closure->function;
		// -1 because the IP is sitting on the next instruction to be executed
		size_t instruction = frame->ip - function->chunk.code - 1;
		fprintf(stderr, "[line %d] in %s\n", function->chunk.lines[instruction],
				function->name ? function->name->chars : "script");
	}

	CallFrame *frame = &vm.frames[vm.frame_count - 1];
	size_t instruction = frame->ip - frame->closure->function->chunk.code - 1;
	int line = frame->closure->function->chunk.lines[instruction];
	fprintf(stderr, "[line %d] in script\n", line);

	resetStack();
}

static void defineNative(const char *name, NativeFn function)
{
	// Using the stack for the GC instead of simply passing to tableSet()
	push(OBJ_VAL(copyString(name, (int)strlen(name))));
	push(OBJ_VAL(newNative(function)));
	tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
	pop();
	pop();
}

static bool bindMethod(ObjClass *clazz, ObjString *name)
{
	Value method;
	if (!tableGet(&clazz->methods, name, &method)) {
		runtimeError("Undefined property '%s'", name->chars);
		return false;
	}

	ObjBoundMethod *bound = newBoundMethod(peek(0), AS_CLOSURE(method));
	pop();
	push(OBJ_VAL(bound));
	return true;
}

static bool call(ObjClosure *closure, int argc)
{
	if (argc != closure->function->arity) {
		runtimeError("Expected %d arguments, got %d", closure->function->arity,
					 argc);
		return false;
	}
	if (vm.frame_count == FRAMES_MAX) {
		runtimeError("Stack overflow");
		return false;
	}

	CallFrame *frame = &vm.frames[vm.frame_count++];
	frame->closure = closure;
	frame->ip = closure->function->chunk.code;

	// Skip local slot 0, which contains the function that's being called
	frame->slots = vm.tos - argc - 1;
	return true;
}

static bool callNative(NativeFn function, int argc)
{
	Value result = function(argc, vm.tos - argc);
	vm.tos -= (argc + 1);
	push(result);
	return true;
}

static bool callValue(Value callee, int argc)
{
	if (IS_OBJ(callee)) {
		switch (OBJ_TYPE(callee)) {
		case OBJ_BOUND_METHOD: {
			ObjBoundMethod *bound = AS_BOUND_METHOD(callee);
			vm.tos[-argc - 1] = bound->receiver;
			return call(bound->method, argc);
		}
		case OBJ_CLASS: {
			ObjClass *clazz = AS_CLASS(callee);
			vm.tos[-argc - 1] = OBJ_VAL(newInstance(clazz));
			Value initializer;
			if (tableGet(&clazz->methods, vm.init_string, &initializer)) {
				return call(AS_CLOSURE(initializer), argc);
			} else if (argc) {
				runtimeError("Expected 0 arguments, got %d", argc);
				return false;
			}
			return true;
		}
		case OBJ_CLOSURE:
			return call(AS_CLOSURE(callee), argc);
		case OBJ_NATIVE: {
			return callNative(AS_NATIVE(callee), argc);
		}
		default:; // Fall through
		}
	}

	runtimeError("Can only call functions and classess");
	return false;
}

static ObjUpvalue *captureUpvalue(Value *local)
{
	ObjUpvalue *prev_upvalue = 0;
	ObjUpvalue *upvalue = vm.open_upvalues;
	while (upvalue && upvalue->location > local) {
		prev_upvalue = upvalue;
		upvalue = upvalue->next;
	}
	if (upvalue && upvalue->location == local) {
		return upvalue;
	}

	ObjUpvalue *created = newUpvalue(local);
	created->next = upvalue;
	if (prev_upvalue) {
		prev_upvalue->next = created;
	} else {
		vm.open_upvalues = created;
	}
	return created;
}

static void closeUpvalues(Value *last)
{
	while (vm.open_upvalues && vm.open_upvalues->location >= last) {
		ObjUpvalue *upvalue = vm.open_upvalues;
		upvalue->closed = *upvalue->location;
		upvalue->location = &upvalue->closed;
		vm.open_upvalues = upvalue->next;
	}
}

static void defineMethod(ObjString *name)
{
	Value method = peek(0);
	ObjClass *clazz = AS_CLASS(peek(1));
	tableSet(&clazz->methods, name, method);
	pop();
}

static bool invokeFromClass(ObjClass *clazz, ObjString *name, int argc)
{
	Value method;
	if (!tableGet(&clazz->methods, name, &method)) {
		runtimeError("Undefined property '%s'", name->chars);
		return false;
	}
	return call(AS_CLOSURE(method), argc);
}

static bool invoke(ObjString *name, int argc)
{
	Value receiver = peek(argc);
	if (!IS_INSTANCE(receiver)) {
		runtimeError("Only instances have methods that can be invoked");
		return false;
	}
	ObjInstance *instance = AS_INSTANCE(receiver);

	Value value;
	if (tableGet(&instance->fields, name, &value)) {
		vm.tos[-argc - 1] = value;
		return callValue(value, argc);
	}
	return invokeFromClass(instance->clazz, name, argc);
}

static bool runOpAdd()
{
	Value b = peek(0);
	Value a = peek(1);
	if (IS_STRING(a) && IS_STRING(b)) {
		concatenate();
	} else if (IS_FLOAT(a) && IS_FLOAT(b)) {
		double b_num = AS_FLOAT(pop());
		double a_num = AS_FLOAT(pop());
		push(FLOAT_VAL(a_num + b_num));
	} else if (IS_INT(a) && IS_INT(b)) {
		long b_num = AS_INT(pop());
		long a_num = AS_INT(pop());
		push(INT_VAL(a_num + b_num));
	} else {
		runtimeError("Operands must be two numbers or two strings");
		return false;
	}
	return true;
}

static bool runOpSubtract()
{
	Value b = peek(0);
	Value a = peek(1);
	if (IS_FLOAT(a) && IS_FLOAT(b)) {
		double b_num = AS_FLOAT(pop());
		double a_num = AS_FLOAT(pop());
		push(FLOAT_VAL(a_num - b_num));
	} else if (IS_INT(a) && IS_INT(b)) {
		long b_num = AS_INT(pop());
		long a_num = AS_INT(pop());
		push(INT_VAL(a_num - b_num));
	} else {
		runtimeError("Operands must be two numbers of the same type");
		return false;
	}
	return true;
}

static bool runOpMultiply()
{
	Value b = peek(0);
	Value a = peek(1);
	if (IS_FLOAT(a) && IS_FLOAT(b)) {
		double b_num = AS_FLOAT(pop());
		double a_num = AS_FLOAT(pop());
		push(FLOAT_VAL(a_num * b_num));
	} else if (IS_INT(a) && IS_INT(b)) {
		long b_num = AS_INT(pop());
		long a_num = AS_INT(pop());
		push(INT_VAL(a_num * b_num));
	} else {
		runtimeError("Operands must be two numbers of the same type");
		return false;
	}
	return true;
}

static bool runOpDivide()
{
	Value b = peek(0);
	Value a = peek(1);
	if (IS_FLOAT(a) && IS_FLOAT(b)) {
		double b_num = AS_FLOAT(pop());
		double a_num = AS_FLOAT(pop());
		push(FLOAT_VAL(a_num / b_num));
	} else if (IS_INT(a) && IS_INT(b)) {
		long b_num = AS_INT(pop());
		long a_num = AS_INT(pop());
		push(INT_VAL(a_num / b_num));
	} else {
		runtimeError("Operands must be two numbers of the same type");
		return false;
	}
	return true;
}

static bool runOpGreater()
{
	Value b = peek(0);
	Value a = peek(1);
	if (IS_FLOAT(a) && IS_FLOAT(b)) {
		double b_num = AS_FLOAT(pop());
		double a_num = AS_FLOAT(pop());
		push(BOOL_VAL(a_num > b_num));
	} else if (IS_INT(a) && IS_INT(b)) {
		long b_num = AS_INT(pop());
		long a_num = AS_INT(pop());
		push(BOOL_VAL(a_num > b_num));
	} else {
		runtimeError("Operands must be two numbers of the same type");
		return false;
	}
	return true;
}

static bool runOpLess()
{
	Value b = peek(0);
	Value a = peek(1);
	if (IS_FLOAT(a) && IS_FLOAT(b)) {
		double b_num = AS_FLOAT(pop());
		double a_num = AS_FLOAT(pop());
		push(BOOL_VAL(a_num < b_num));
	} else if (IS_INT(a) && IS_INT(b)) {
		long b_num = AS_INT(pop());
		long a_num = AS_INT(pop());
		push(BOOL_VAL(a_num < b_num));
	} else {
		runtimeError("Operands must be two numbers of the same type");
		return false;
	}
	return true;
}

static void trace_execution(CallFrame *frame)
{
#ifdef DEBUG_TRACE_EXECUTION
	printf("        | ");
	for (Value *slot = vm.stack; slot < vm.tos; slot++) {
		printf("[ ");
		printValue(*slot);
		printf(" ]");
	}
	printf("\n");
	Chunk chunk = frame->closure->function->chunk;
	disassembleInstruction(&chunk, (int)(frame->ip - chunk.code));
#endif
}

static InterpretResult run()
{
	CallFrame *frame = &vm.frames[vm.frame_count - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_CONSTANT()                                                        \
	(frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_SHORT()                                                           \
	(frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_STRING() AS_STRING(READ_CONSTANT())

	while (1) {
		trace_execution(frame);
		uint8_t instruction;
		switch (instruction = READ_BYTE()) {
		case OP_CONSTANT: {
			Value constant = READ_CONSTANT();
			push(constant);
			break;
		}
		case OP_NIL:
			push(NIL_VAL);
			break;
		case OP_TRUE:
			push(BOOL_VAL(true));
			break;
		case OP_FALSE:
			push(BOOL_VAL(false));
			break;
		case OP_POP:
			pop();
			break;
		case OP_DEFINE_GLOBAL: {
			ObjString *name = READ_STRING();
			tableSet(&vm.globals, name, peek(0));
			pop();
			break;
		}
		case OP_GET_GLOBAL: {
			ObjString *name = READ_STRING();
			Value value;
			if (!tableGet(&vm.globals, name, &value)) {
				runtimeError("Undefined variable '%s'", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			push(value);
			break;
		}
		case OP_SET_GLOBAL: {
			ObjString *name = READ_STRING();
			if (tableSet(&vm.globals, name, peek(0))) {
				tableDelete(&vm.globals, name);
				runtimeError("Undefined variable '%s'", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_GET_LOCAL: {
			uint8_t slot = READ_BYTE();
			push(frame->slots[slot]);
			break;
		}
		case OP_SET_LOCAL: {
			uint8_t slot = READ_BYTE();
			frame->slots[slot] = peek(0);
			break;
		}
		case OP_GET_UPVALUE: {
			uint8_t slot = READ_BYTE();
			push(*frame->closure->upvalues[slot]->location);
			break;
		}
		case OP_SET_UPVALUE: {
			uint8_t slot = READ_BYTE();
			*frame->closure->upvalues[slot]->location = peek(0);
			break;
		}
		case OP_GET_PROPERTY: {
			if (!IS_INSTANCE(peek(0))) {
				runtimeError("Only instances can have properties");
				return INTERPRET_RUNTIME_ERROR;
			}
			ObjInstance *instance = AS_INSTANCE(peek(0));
			ObjString *name = READ_STRING();
			Value value;
			if (tableGet(&instance->fields, name, &value)) {
				pop();
				push(value);
				break;
			}
			if (!bindMethod(instance->clazz, name)) {
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_SET_PROPERTY: {
			ObjInstance *instance = AS_INSTANCE(peek(1));
			tableSet(&instance->fields, READ_STRING(), peek(0));
			Value value = pop();
			// Remove instance name from the stack
			pop();
			push(value);
			break;
		}
		case OP_GET_SUPER: {
			ObjString *name = READ_STRING();
			ObjClass *super_class = AS_CLASS(pop());
			if (!bindMethod(super_class, name)) {
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_EQUAL: {
			Value b = pop();
			Value a = pop();
			push(BOOL_VAL(valuesEqual(a, b)));
			break;
		}
		case OP_GREATER:
			if (!runOpGreater())
				return INTERPRET_RUNTIME_ERROR;
			break;
		case OP_LESS:
			if (!runOpLess())
				return INTERPRET_RUNTIME_ERROR;
			break;
		case OP_ADD:
			if (!runOpAdd())
				return INTERPRET_RUNTIME_ERROR;
			break;
		case OP_SUBTRACT:
			if (!runOpSubtract())
				return INTERPRET_RUNTIME_ERROR;
			break;
		case OP_MULTIPLY:
			if (!runOpMultiply())
				return INTERPRET_RUNTIME_ERROR;
			break;
		case OP_DIVIDE:
			if (!runOpDivide())
				return INTERPRET_RUNTIME_ERROR;
			break;
		case OP_NEGATE: {
			Value value = peek(0);
			if (IS_FLOAT(value)) {
				push(FLOAT_VAL(-AS_FLOAT(value)));
			} else if (IS_INT(value)) {
				push(INT_VAL(-AS_INT(value)));
			} else {
				runtimeError("Operand must be a number");
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_NOT:
			push(BOOL_VAL(isFalsey(pop())));
			break;
		case OP_PRINT:
			printValue(pop());
			printf("\n");
			break;
		case OP_TO_FLOAT: {
			Value value = pop();
			if (IS_BOOL(value)) {
				push(FLOAT_VAL(AS_BOOL(value) ? 1.0 : 0.0));
			} else if (IS_INT(value)) {
				push(FLOAT_VAL((double)AS_INT(value)));
			} else {
				runtimeError("Can only convert bool and int to float");
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_TO_INT: {
			Value value = pop();
			if (IS_BOOL(value)) {
				push(INT_VAL(AS_BOOL(value) ? 1 : 0));
			} else if (IS_FLOAT(value)) {
				push(INT_VAL((long)AS_FLOAT(value)));
			} else {
				runtimeError("Can only convert bool and int to float");
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_JUMP: {
			uint16_t offset = READ_SHORT();
			frame->ip += offset;
			break;
		}
		case OP_JUMP_IF_FALSE: {
			uint16_t offset = READ_SHORT();
			if (isFalsey(peek(0)))
				frame->ip += offset;
			break;
		}
		case OP_LOOP: {
			uint16_t offset = READ_SHORT();
			frame->ip -= offset;
			break;
		}
		case OP_CALL: {
			int argc = READ_BYTE();
			if (!callValue(peek(argc), argc)) {
				return INTERPRET_RUNTIME_ERROR;
			}
			frame = &vm.frames[vm.frame_count - 1];
			break;
		}
		case OP_INVOKE: {
			ObjString *method = READ_STRING();
			int argc = READ_BYTE();
			if (!invoke(method, argc)) {
				return INTERPRET_RUNTIME_ERROR;
			}
			frame = &vm.frames[vm.frame_count - 1];
			break;
		}
		case OP_SUPER_INVOKE: {
			ObjString *method = READ_STRING();
			int argc = READ_BYTE();
			ObjClass *super_class = AS_CLASS(pop());
			if (!invokeFromClass(super_class, method, argc)) {
				return INTERPRET_RUNTIME_ERROR;
			}
			frame = &vm.frames[vm.frame_count - 1];
			break;
		}
		case OP_CLOSURE: {
			ObjFunction *function = AS_FUNCTION(READ_CONSTANT());
			ObjClosure *closure = newClosure(function);
			push(OBJ_VAL(closure));
			for (int i = 0; i < closure->upvalue_count; i++) {
				uint8_t is_local = READ_BYTE();
				uint8_t index = READ_BYTE();
				if (is_local) {
					closure->upvalues[i] = captureUpvalue(frame->slots + index);
				} else {
					closure->upvalues[i] = frame->closure->upvalues[index];
				}
			}
			break;
		}
		case OP_CLOSE_UPVALUE:
			closeUpvalues(frame->slots);
			pop();
			break;
		case OP_RETURN: {
			Value result = pop();
			vm.frame_count--;
			if (vm.frame_count == 0) {
				pop();
				return INTERPRET_OK;
			}

			vm.tos = frame->slots;
			push(result);
			frame = &vm.frames[vm.frame_count - 1];
			break;
		}
		case OP_CLASS:
			push(OBJ_VAL(newClass(READ_STRING())));
			break;
		case OP_INHERIT: {
			Value super_class = peek(1);
			if (!IS_CLASS(super_class)) {
				runtimeError("The given superclass is not a class");
				return INTERPRET_RUNTIME_ERROR;
			}
			ObjClass *sub_clazz = AS_CLASS(peek(0));
			tableAddAll(&AS_CLASS(super_class)->methods, &sub_clazz->methods);
			pop();
			break;
		}
		case OP_METHOD: {
			defineMethod(READ_STRING());
			break;
		}
		}
	}

#undef BINARY_OP
#undef READ_STRING
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_BYTE
}

void initVM()
{
	resetStack();
	vm.objects = 0;

	vm.allocated_bytes = 0;
	vm.next_gc = 1024 * 1024;

	vm.grey_count = 0;
	vm.grey_capacity = 0;
	vm.grey_stack = 0;

	initTable(&vm.globals);
	initTable(&vm.strings);

	vm.init_string = 0;
	vm.init_string = copyString("init", 4);

	installStdLib(defineNative);
}

void freeVM()
{
	freeTable(&vm.globals);
	freeTable(&vm.strings);
	vm.init_string = 0;
	freeObjects();
}

InterpretResult interpret(const char *source)
{
	ObjFunction *function = compile(source);
	if (!function)
		return INTERPRET_COMPILE_ERROR;

	// Push for GC
	push(OBJ_VAL(function));

	ObjClosure *closure = newClosure(function);
	pop();
	push(OBJ_VAL(closure));
	callValue(OBJ_VAL(closure), 0);

	return run();
}

void push(Value value)
{
	*vm.tos = value;
	vm.tos++;
}

bool pushNonNil(Value value)
{
	if (value.type == VAL_NIL)
		return false;
	push(value);
	return true;
}

Value pop()
{
	vm.tos--;
	return *vm.tos;
}
