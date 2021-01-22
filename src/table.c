#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "include/memory.h"
#include "include/object.h"
#include "include/table.h"
#include "include/value.h"

static Entry *findEntry(Entry *entries, int capacity, ObjString *key)
{
	uint32_t index = key->hash & capacity;
	Entry *tombstone = 0;

	while (1) {
		Entry *entry = &entries[index];
		if (!entry->key) {
			if (IS_NIL(entry->value)) {
				return tombstone ? tombstone : entry;
			} else {
				if (!tombstone)
					tombstone = entry;
			}
		} else if (entry->key == key) {
			// Found
			return entry;
		}

		index = (index + 1) & capacity;
	}
}

static void adjustCapacity(Table *table, int capacity)
{
	Entry *entries = ALLOCATE(Entry, capacity + 1);
	for (int i = 0; i <= capacity; i++) {
		entries[i].key = 0;
		entries[i].value = NIL_VAL;
	}

	table->count = 0;
	for (int i = 0; i <= table->capacity_mask; i++) {
		Entry *entry = &table->entries[i];
		if (!entry->key) {
			continue;
		}

		Entry *dest = findEntry(entries, capacity, entry->key);
		dest->key = entry->key;
		dest->value = entry->value;
		table->count++;
	}

	FREE_ARRAY(Entry, table->entries, table->capacity_mask + 1);
	table->entries = entries;
	table->capacity_mask = capacity;
}

void initTable(Table *table)
{
	table->count = 0;
	table->capacity_mask = -1;
	table->entries = 0;
}

void freeTable(Table *table)
{
	FREE_ARRAY(Entry, table->entries, table->capacity_mask + 1);
	initTable(table);
}

bool tableSet(Table *table, ObjString *key, Value value)
{
	if (table->count + 1 > (table->capacity_mask + 1) * TABLE_MAX_LOAD) {
		int capacity = GROW_CAPACITY(table->capacity_mask + 1) - 1;
		adjustCapacity(table, capacity);
	}

	Entry *entry = findEntry(table->entries, table->capacity_mask, key);

	bool is_new = !entry->key;
	// Increase only if we're not taking a tombstone
	if (is_new && IS_NIL(entry->value))
		table->count++;

	entry->key = key;
	entry->value = value;
	return is_new;
}

bool tableGet(Table *table, ObjString *key, Value *value)
{
	if (!table->count)
		return false;

	Entry *entry = findEntry(table->entries, table->capacity_mask, key);
	if (!entry->key)
		return false;

	*value = entry->value;
	return true;
}

bool tableDelete(Table *table, ObjString *key)
{
	if (!table->count)
		return false;

	Entry *entry = findEntry(table->entries, table->capacity_mask, key);
	if (!entry->key)
		return false;

	// tombstone
	entry->key = 0;
	entry->value = BOOL_VAL(true);
	return true;
}

void tableAddAll(Table *from, Table *to)
{
	for (int i = 0; i <= from->capacity_mask; i++) {
		Entry *entry = &from->entries[i];
		if (entry->key) {
			tableSet(to, entry->key, entry->value);
		}
	}
}

ObjString *tableFindString(Table *table, const char *chars, int length,
						   uint32_t hash)
{
	if (!table->count)
		return 0;

	uint32_t index = hash & table->capacity_mask;
	while (1) {
		Entry *entry = &table->entries[index];

		if (!entry->key) {
			// Stop if we find an empty non-tombstone entry.
			if (IS_NIL(entry->value))
				return 0;
		} else if (entry->key->length == length && entry->key->hash == hash &&
				   memcmp(entry->key->chars, chars, length) == 0) {
			// Found
			return entry->key;
		}
		index = (index + 1) & table->capacity_mask;
	}
}

void markTable(Table *table)
{
	for (int i = 0; i <= table->capacity_mask; i++) {
		Entry *entry = &table->entries[i];
		markObject((Obj *)entry->key);
		markValue(entry->value);
	}
}

void tableRemoveWhite(Table *table)
{
	for (int i = 0; i <= table->capacity_mask; i++) {
		Entry *entry = &table->entries[i];
		if (entry->key && !entry->key->obj.is_marked) {
			tableDelete(table, entry->key);
		}
	}
}