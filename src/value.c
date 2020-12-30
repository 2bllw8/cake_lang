#include <stdio.h>
#include <string.h>

#include "include/memory.h"
#include "include/object.h"
#include "include/value.h"

bool valuesEqual(Value a, Value b)
{
	if (a.type != b.type)
		return false;
	switch (a.type) {
	case VAL_BOOL:
		return AS_BOOL(a) == AS_BOOL(b);
	case VAL_FLOAT:
		return AS_FLOAT(a) == AS_FLOAT(b);
	case VAL_INT:
		return AS_INT(a) == AS_INT(b);
	case VAL_NIL:
		return true;
	case VAL_OBJ: {
		return AS_OBJ(a) == AS_OBJ(b);
	}
	default:
		return false;
	}
}

void initValueArray(ValueArray *array)
{
	array->capacity = 0;
	array->count = 0;
	array->values = 0;
}

void writeValueArray(ValueArray *array, Value value)
{
	if (array->capacity < array->count + 1) {
		int old_capacity = array->capacity;
		array->capacity = GROW_CAPACITY(old_capacity);
		array->values =
			GROW_ARRAY(Value, array->values, old_capacity, array->capacity);
	}

	array->values[array->count++] = value;
}

void freeValueArray(ValueArray *array)
{
	FREE_ARRAY(Value, array->values, array->capacity);
	initValueArray(array);
}

void printValue(Value value)
{
	switch (value.type) {
	case VAL_BOOL:
		printf("%s", AS_BOOL(value) ? "true" : "false");
		break;
	case VAL_FLOAT:
		printf("%f", AS_FLOAT(value));
		break;
	case VAL_INT:
		printf("%ld", AS_INT(value));
		break;
	case VAL_NIL:
		printf("nil");
		break;
	case VAL_OBJ:
		printObject(value);
		break;
	default:
		return;
	}
}