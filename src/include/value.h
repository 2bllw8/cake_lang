#ifndef cake_value_h
#define cake_value_h

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef enum {
	VAL_BOOL,
	VAL_FLOAT,
	VAL_INT,
	VAL_NIL,
	VAL_OBJ,
} ValueType;

typedef struct {
	ValueType type;
	union {
		bool boolean;
		double float_number;
		long int_number;
		Obj *obj;
	} as;
} Value;

#define AS_BOOL(value) ((value).as.boolean)
#define AS_FLOAT(value) ((value).as.float_number)
#define AS_INT(value) ((value).as.int_number)
#define AS_OBJ(value) ((value).as.obj)

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_FLOAT(value) ((value).type == VAL_FLOAT)
#define IS_INT(value) ((value).type == VAL_INT)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_OBJ(value) ((value).type == VAL_OBJ)

#define BOOL_VAL(value) ((Value){ VAL_BOOL, { .boolean = value } })
#define FLOAT_VAL(value) ((Value){ VAL_FLOAT, { .float_number = value } })
#define INT_VAL(value) ((Value){ VAL_INT, { .int_number = value } })
#define NIL_VAL ((Value){ VAL_NIL, { .int_number = 0 } })
#define OBJ_VAL(object) ((Value){ VAL_OBJ, { .obj = (Obj *)object } })

typedef struct {
	int capacity;
	int count;
	Value *values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray *array);
void writeValueArray(ValueArray *array, Value value);
void freeValueArray(ValueArray *array);

void printValue(Value value);

#endif