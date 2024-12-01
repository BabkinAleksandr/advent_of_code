#include "p2.h"
#include "hashmap.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  int sum = 0;
  char buffer[50];

  list_t *l = init_list();
  // fill with zeroes by default
  hashmap_int_t *h = init_hashmap(HASHMAP_CAPACITY, 0);

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    parse_line((char *)&buffer, l, h);
  }

  for (int i = 0; i < l->count; i++) {
    sum += l->nums[i] * get_v(h, l->nums[i]);
  }

  assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d1/P2 result: %d\n", sum);

  destroy_list(&l);
  destroy_hashmap(&h);

  return 0;
}

void parse_line(const char *s, list_t *l, hashmap_int_t *h) {
  // integers to parse
  unsigned int n1 = 0, n2 = 0;
  // pointer to currently parsed integer
  unsigned int *n = &n1;

  do {
    if (isdigit(*s)) {
      *n = *n * 10 + (*s - '0');
    } else {
      // change currently parsed integer if non-digit is encountered
      n = &n2;
    }
  } while (*(++s) != '\0' && *s != '\n');

  l->nums[l->count++] = n1;
  set_kv(h, n2, get_v(h, n2) + 1);
}

list_t *init_list() {
  unsigned int *nums = malloc(sizeof(int) * LIST_CAPACITY);
  if (nums == NULL) {
    perror("Nums malloc");
    exit(1);
  }
  list_t *l = malloc(sizeof(list_t));
  if (l == NULL) {
    perror("List malloc");
    exit(1);
  }

  l->count = 0;
  l->_allocated = LIST_CAPACITY;
  l->nums = nums;

  return l;
}

void destroy_list(list_t **l) {
  free((*l)->nums);
  free((*l));
  *l = NULL;
}
