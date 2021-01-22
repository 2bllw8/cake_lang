#ifndef cake_stdlib_h
#define cake_stdlib_h

#include "object.h"

void installStdLib(void (*install)(const char *, NativeFn));

#endif