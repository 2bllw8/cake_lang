#include <stdlib.h>

#include "include/compiler.h"
#include "include/memory.h"
#include "include/object.h"
#include "include/value.h"
#include "include/vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "include/debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

static void freeObject(Obj *object)
{
	switch (object->type) {
	case OBJ_BOUND_METHOD:
		FREE(ObjBoundMethod, object);
		break;
	case OBJ_CLASS: {
		ObjClass *clazz = (ObjClass *)object;
		freeTable(&clazz->methods);
		FREE(ObjClass, object);
		break;
	}
	case OBJ_CLOSURE: {
		ObjClosure *closure = (ObjClosure *)object;
		FREE_ARRAY(ObjUpvalue *, closure->upvalues, closure->upvalue_count);
		FREE(ObjClosure, object);
		break;
	}
	case OBJ_FUNCTION: {
		ObjFunction *function = (ObjFunction *)object;
		freeChunk(&function->chunk);
		FREE(ObjFunction, object);
		break;
	}
	case OBJ_INSTANCE: {
		ObjInstance *instance = (ObjInstance *)object;
		freeTable(&instance->fields);
		FREE(ObjInstance, instance);
		break;
	}
	case OBJ_NATIVE: {
		FREE(ObjNative, object);
		break;
	}
	case OBJ_STRING: {
		ObjString *string = (ObjString *)object;
		FREE_ARRAY(char, string->chars, string->length + 1);
		FREE(ObjString, object);
		break;
	}
	case OBJ_UPVALUE: {
		FREE(ObjUpvalue, object);
		break;
	}
	}
}

static void markArray(ValueArray *array)
{
	for (int i = 0; i < array->count; i++) {
		markValue(array->values[i]);
	}
}

static void markRoots()
{
	for (Value *slot = vm.stack; slot < vm.tos; slot++) {
		markValue(*slot);
	}
	for (int i = 0; i < vm.frame_count; i++) {
		markObject((Obj *)vm.frames[i].closure);
	}
	for (ObjUpvalue *upvalue = vm.open_upvalues; upvalue;
		 upvalue = upvalue->next) {
		markObject((Obj *)upvalue);
	}
	markTable(&vm.globals);
	markCompilerRoots();
	markObject((Obj *)vm.init_string);
}

static void blackenObject(Obj *object)
{
#ifdef DEBUG_LOG_GC
	printf("%p blacken ", (void *)object);
	printValue(OBJ_VAL(object));
	printf("\n");
#endif

	switch (object->type) {
	case OBJ_BOUND_METHOD: {
		ObjBoundMethod *bound = (ObjBoundMethod *)object;
		markValue(bound->receiver);
		markObject((Obj *)bound->method);
		break;
	}
	case OBJ_CLASS: {
		ObjClass *class = (ObjClass *)object;
		markObject((Obj *)class->name);
		markTable(&class->methods);
		break;
	}
	case OBJ_CLOSURE: {
		ObjClosure *closure = (ObjClosure *)object;
		markObject((Obj *)closure->function);
		for (int i = 0; i < closure->upvalue_count; i++) {
			markObject((Obj *)closure->upvalues[i]);
		}
		break;
	case OBJ_FUNCTION: {
		ObjFunction *function = (ObjFunction *)object;
		markObject((Obj *)function->name);
		markArray(&function->chunk.constants);
		break;
	}
	case OBJ_INSTANCE: {
		ObjInstance *instance = (ObjInstance *)object;
		markObject((Obj *)instance->clazz);
		markTable(&instance->fields);
		break;
	}
	case OBJ_NATIVE:
	case OBJ_STRING:
		break;
	case OBJ_UPVALUE:
		markValue(((ObjUpvalue *)object)->closed);
		break;
	}
	}
}

static void traceReferences()
{
	while (vm.grey_count > 0) {
		Obj *object = vm.grey_stack[--vm.grey_count];
		blackenObject(object);
	}
}

static void sweep()
{
	Obj *previous = 0;
	Obj *object = vm.objects;
	while (object) {
		if (object->is_marked) {
			object->is_marked = false;
			previous = object;
			object = object->next;
		} else {
			Obj *unreached = object;
			object = object->next;
			if (previous) {
				previous->next = object;
			} else {
				vm.objects = object;
			}
			freeObject(unreached);
		}
	}
}

void *reallocate(void *pointer, size_t old_size, size_t new_size)
{
	vm.allocated_bytes += new_size - old_size;

	if (new_size > old_size) {
#ifdef DEBUG_STRESS_GC
		collectGarbage();
#endif
		if (vm.allocated_bytes > vm.next_gc) {
			collectGarbage();
		}
	}
	if (new_size == 0) {
		free(pointer);
		return 0;
	}

	void *result = realloc(pointer, new_size);
	if (!result)
		exit(1);
	return result;
}

void collectGarbage()
{
#ifdef DEBUG_LOG_GC
	printf("-- gc begin\n");
	size_t before = vm.allocated_bytes;
#endif

	markRoots();
	traceReferences();
	tableRemoveWhite(&vm.strings);
	sweep();

	vm.next_gc = vm.allocated_bytes * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
	printf("-- gc end. \n   Collected %ld bytes (%ld to %ld). Next at %ld\n",
		   before - vm.allocated_bytes, before, vm.allocated_bytes, vm.next_gc);
#endif
}

void markObject(Obj *object)
{
	if (!object || object->is_marked)
		return;

#ifdef DEBUG_LOG_GC
	printf("%p mark ", (void *)object);
	printValue(OBJ_VAL(object));
	printf("\n");
#endif

	object->is_marked = true;

	if (vm.grey_capacity < vm.grey_count + 1) {
		vm.grey_capacity = GROW_CAPACITY(vm.grey_capacity);
		vm.grey_stack =
			realloc(vm.grey_stack, sizeof(Obj *) * vm.grey_capacity);
		if (!vm.grey_stack) {
			// Out of memory
			exit(1);
		}
	}
	vm.grey_stack[vm.grey_count++] = object;
}

void markValue(Value value)
{
	if (!IS_OBJ(value))
		return;
	markObject(AS_OBJ(value));
}

void freeObjects()
{
	Obj *object = vm.objects;
	while (object) {
#ifdef DEBUG_LOG_GC
		printf("%p free type %d\n", (void *)object, object->type);
#endif
		Obj *next = object->next;
		freeObject(object);
		object = next;
	}

	free(vm.grey_stack);
}