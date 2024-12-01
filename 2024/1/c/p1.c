#include "p1.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  int sum = 0;
  char buffer[50];

  list_t *l = init_list();
  list_t *r = init_list();

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    parse_line((char *)&buffer, l, r);
  }

  assert(l->count == r->count);
  // could be solved with min heap, but optimization profit is not convincing:
  // using sorting complexity: O(2n * log(n))
  // using min heap complexity: O(2 * log(n))
  qsort(l->nums, l->count, sizeof(int), &comp_asc);
  qsort(r->nums, r->count, sizeof(int), &comp_asc);

  for (int i = 0; i < l->count; i++) {
    sum += abs((int)l->nums[i] - (int)r->nums[i]);
  }

  assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d1/P1 result: %d\n", sum);

  destroy_list(&l);
  destroy_list(&r);

  return 0;
}

void parse_line(const char *s, list_t *l, list_t *r) {
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
  r->nums[r->count++] = n2;
}

list_t *init_list() {
  unsigned int *nums = malloc(sizeof(int) * CAPACITY);
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
  l->_allocated = CAPACITY;
  l->nums = nums;

  return l;
}

int comp_asc(const void *l, const void *r) {
  int lnum = *(unsigned int *)l;
  int rnum = *(unsigned int *)r;

  return lnum - rnum;
}

void destroy_list(list_t **l) {
  free((*l)->nums);
  free((*l));
  *l = NULL;
}
