#ifndef cake_scanner_h
#define cake_scanner_h

#include "token.h"

void initScanner(const char *source);
Token scanToken();

#endif