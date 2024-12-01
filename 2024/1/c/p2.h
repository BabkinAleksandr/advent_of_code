#ifndef P2_H
#define P2_H

#include "hashmap.h"

#define CORRECT_ANSWER 25574739

// capacities are known in advance
#define LIST_CAPACITY 1000
#define HASHMAP_CAPACITY 100000

typedef struct List {
  /** Number of elements */
  int count;
  /** Number of elements memory is allocated for */
  int _allocated;
  unsigned int *nums;
} list_t;

/** Parses two integers in line. Put first one in *l list
 * and second to hashmap *h */
void parse_line(const char *s, list_t *l, hashmap_int_t *h);
/** Inits list with allocation assertions */
list_t *init_list();
/** Deallocates memory and nulling pointer */
void destroy_list(list_t **l);

#endif
