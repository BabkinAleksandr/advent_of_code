#ifndef P1_H
#define P1_H

#define CORRECT_ANSWER 1603498
#define CAPACITY 1000 // list size is known in advance

typedef struct List {
  /** Number of elements */
  int count;
  /** Number of elements memory is allocated for */
  int _allocated;
  unsigned int *nums;
} list_t;

/** Parses two integers in line. Put them in *l and *r lists respectively */
void parse_line(const char *s, list_t *l, list_t *r);
/** Inits list with allocation assertions */
list_t *init_list();
/** Compare function for sorting in ascending order */
int comp_asc(const void *l, const void *r);
/** Deallocates memory and nulling pointer */
void destroy_list(list_t **l);

#endif
