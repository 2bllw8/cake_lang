#ifndef cake_compiler_h
#define cake_compiler_h

#include "vm.h"

ObjFunction *compile(const char *source);

void markCompilerRoots();

#endif