/**
 * This is special case of hashmap. While it conforms hashmap properties,
 * it uses no hash function and works with array directly, using integer as key.
 * For current task we know that index is positive integer
 * not greater than 100_000
 * */

#include "hashmap.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

hashmap_int_t *init_hashmap(int size, int default_v) {
  int *arr = malloc(sizeof(int) * size);
  if (arr == NULL) {
    perror("Hashmap arr alloc");
    exit(1);
  }

  memset(arr, default_v, sizeof(int) * size);
  hashmap_int_t *m = malloc(sizeof(hashmap_int_t));
  if (m == NULL) {
    perror("Hashmap alloc");
    exit(1);
  }

  m->_values = arr;
  m->_size = size;
  return m;
}

void set_kv(hashmap_int_t *m, int key, int value) {
  int h = hash(key, m->_size);
  m->_values[h] = value;
}

int get_v(hashmap_int_t *m, int key) {
  int h = hash(key, m->_size);
  return m->_values[h];
}

void destroy_hashmap(hashmap_int_t **h) {
  free((*h)->_values);
  free(*h);
  *h = NULL;
}

/** Placeholder function */
static int hash(int i, int size) { return i; }
