#include "p1.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
  int sum = 0;
  char buffer[1000];

  row_t *rows[] = {init_row(), init_row()};
  uint cr = 0;
  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    clean_row(rows[cr]);
    parse_line((char *)&buffer, rows[cr]);

    if (rows[!cr]->count > 0)
      sum += get_row_sum(rows[!cr], rows[cr]);

    // negating gives interchanging of `0` and `1`
    cr = !cr;
  }

  destroy_row(&rows[0]);
  destroy_row(&rows[1]);

  assert(sum == CORRECT_ANSWER);
  printf("\n/2023/d3/P1 result: %d\n", sum);
  return 0;
}

row_t *init_row() {
  detail_t *details = malloc(sizeof(detail_t) * CAPACITY);
  assert_ptr(details, "Details malloc");

  // to not allocate memory for every integer for `details`
  // allocate whole buffer at once and use it to store distinct integers
  uint *_buffer = calloc(CAPACITY, sizeof(int));
  assert_ptr(_buffer, "Buffer calloc");

  row_t *row = malloc(sizeof(row_t));
  assert_ptr(row, "Row malloc");

  row->details = details;
  row->_buffer = _buffer;
  row->count = 0;

  return row;
}

void clean_row(row_t *r) {
  memset(r->_buffer, EMPTY, sizeof(uint) * CAPACITY);
  memset(r->details, EMPTY, sizeof(uint) * CAPACITY);
  r->count = 0;
}

/** Parses line and puts int *pointer for every parsed item
 * If integer is parsed, the same pointer to that integer is used for
 * several consequtive items in `details` array
 * */
void parse_line(const char *s, row_t *r) {
  detail_t *d = r->details;
  uint *b = r->_buffer;
  uint i = 0;

  for (; s[i] != '\0' && s[i] != '\n'; i++) {
    // put parsed integer to the same pointer
    if (isdigit(s[i])) {
      d[i] = b;
      *b = *b * 10 + (s[i] - '0');
      continue;
    }
    // if integer was written to current pointer, this will add `1` to buffer
    // pointer and will result in pointer to next empty slot in buffer
    b += !!*b;
    d[i] = b++;
    if (s[i] != '.')
      *d[i] = SYMBOL;
  }

  r->count = i;
}

int get_row_sum(const row_t *prev, const row_t *cur) {
  assert(prev != NULL && cur != NULL);
  assert(prev->count == cur->count);

  uint empty = EMPTY;
  int i, sum = 0;

  uint *cp; // current row, prev elem (i - 1)
  uint *ci; // current row, current elem
  uint *cn; // current row, next elem (i + 1)
  uint *pp; // prev row, prev elem (i - 1)
  uint *pi; // prev row, current elem
  uint *pn; // prev row, next elem (i + 1)

  for (i = 0; i < cur->count; i++) {
    // assign vars for easier access
    // using &empty placeholder pointer to not tackle with NULL pointers
    cp = i > 0 ? cur->details[i - 1] : &empty;
    ci = cur->details[i];
    cn = i < cur->count - 1 ? cur->details[i + 1] : &empty;
    pp = i > 0 ? prev->details[i - 1] : &empty;
    pi = prev->details[i];
    pn = i < cur->count - 1 ? prev->details[i + 1] : &empty;

    // checking current element in current row
    switch (*ci) {

      // skip EMPTY
    case EMPTY:
      continue;

      // current elem is SYMBOL
    case SYMBOL: {
      // checking all adjasent elements
      uint *ptrs[] = {cp, cn, pp, pi, pn, NULL};
      int **cur = (int **)&ptrs;
      do {
        // if adjasent element is number, add it to sum and erase value behind
        // pointer, so even if we access the same pointer again, it will be
        // already empty
        if (**cur != SYMBOL && **cur != EMPTY) {
          sum += **cur;
          **cur = EMPTY;
        }
      } while (*++cur != NULL);
      break;
    }

    default: {
      // current elem is number, so try to find SYMBOL on the previous row
      if (*pp == SYMBOL || *pi == SYMBOL || *pn == SYMBOL) {
        sum += *ci;
        *ci = EMPTY;
      }
    }
    }
  }

  return sum;
}

void destroy_row(row_t **r) {
  free((*r)->details);
  free((*r)->_buffer);
  free(*r);
  *r = NULL;
}
