#ifndef cake_io_h
#define cake_io_h

#include <stdio.h>
#include <stdlib.h>

static char *readFile(const char *path)
{
	FILE *file = fopen(path, "rb");
	if (!file) {
		fprintf(stderr, "Could not open file \'%s\'\n", path);
		exit(74);
	}

	fseek(file, 0L, SEEK_END);
	size_t file_size = ftell(file);
	rewind(file);

	char *buffer = (char *)malloc(file_size + 1);
	if (!buffer) {
		fprintf(stderr, "Not enough memory to read \'%s\'\n", path);
		exit(74);
	}

	size_t read = fread(buffer, sizeof(char), file_size, file);
	if (read < file_size) {
		fprintf(stderr, "Failed to read \'%s\'\n", path);
		exit(74);
	}
	buffer[read] = '\0';

	fclose(file);
	return buffer;
}

#endif