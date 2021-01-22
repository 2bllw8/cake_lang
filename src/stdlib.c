#include <time.h>

#include "include/stdlib.h"
#include "include/object.h"
#include "include/value.h"

static Value _clock()
{
	return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

void installStdLib(void (*install)(const char *, NativeFn))
{
	install("clock", _clock);
}