#ifndef HASHMAP_H
#define HASHMAP_H

/** Integer -> Integer hash map */
typedef struct HashMapInt {
  int *_values;
  /** Actual number of integers memory is allocated for */
  int _size;
} hashmap_int_t;

static int hash(int i, int size);

/** Inits hashmap with designated size and default values,
 * which is returned if key is not found */
hashmap_int_t *init_hashmap(int size, int default_v);
/** Sets value */
void set_kv(hashmap_int_t *m, int key, int value);
/** Get value from hashmap or detaulf value that was used in [init_hashmap]
 * function */
int get_v(hashmap_int_t *m, int key);
/** Frees memory and nulling pointer */
void destroy_hashmap(hashmap_int_t **);

#endif
