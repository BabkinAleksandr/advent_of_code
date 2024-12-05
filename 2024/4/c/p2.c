#include "p2.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
  int sum = 0;
  char buffer[200];

  matrix_t *matrix = alloc_matrix();

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    parse_line((char *)&buffer, matrix);
    check_matrix_capacity(matrix);
  }

  sum += init_dfs(matrix);

  destroy_matrix(&matrix);

  assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d4/P2 result: %d\n", sum);

  return 0;
}

void parse_line(const char *s, matrix_t *m) {
  int len = strlen(s);
  char *b = malloc(sizeof(char) * len - 1); // strip out EOL
  if (b == NULL) {
    perror("Line malloc");
    exit(1);
  }
  memcpy(b, s, sizeof(char) * len - 1);
  m->items[m->count++] = b;
}

void perform_dft(matrix_t *m, int i, int j, int *acc);
int init_dfs(matrix_t *m) {
  int sum = 0;

  for (int i = 0; i < m->count; i++) {
    for (int j = 0; m->items[i][j] != '\0'; j++) {
      if (m->items[i][j] == 'A') {
        perform_dft(m, i, j, &sum);
      }
    }
  }

  return sum;
}

void perform_dft(matrix_t *m, int i, int j, int *acc) {
  if (m->items[i][j] != 'A') {
    return;
  }

  // check if we've reached edges of matrix
  if (i < 1 || i == m->count - 1) {
    return;
  }

  if (j < 1 || j == strlen(m->items[i])) {
    return;
  }

  int tl = m->items[i - 1][j - 1]; // top left
  int tr = m->items[i - 1][j + 1]; // top right
  int bl = m->items[i + 1][j - 1]; // bottom left
  int br = m->items[i + 1][j + 1]; // bottom right

  // check two diagonals, the should have exactly two 'M' and 'S' symbols
  int left_matches = (tl == 'M' && br == 'S') || (tl == 'S' && br == 'M');
  int right_matches = (tr == 'M' && bl == 'S') || (tr == 'S' && bl == 'M');

  if (left_matches && right_matches) {
    *acc += 1;
  }
}

matrix_t *alloc_matrix() {
  matrix_t *matrix = malloc(sizeof(matrix_t));
  if (matrix == NULL) {
    perror("Matrix malloc");
    exit(1);
  }
  char **items = malloc(sizeof(char *) * CAPACITY);
  if (items == NULL) {
    perror("Matrix malloc");
    exit(1);
  }
  matrix->_allocated = CAPACITY;
  matrix->items = items;

  return matrix;
}

void check_matrix_capacity(matrix_t *m) {
  if (m->count == m->_allocated - 1) {
    m->items = realloc(m->items, sizeof(char *) * (m->_allocated + CAPACITY));
    if (m->items == NULL) {
      perror("Items realloc");
      exit(1);
    }
    m->_allocated += CAPACITY;
  }
}

void destroy_matrix(matrix_t **m) {
  for (int i = 0; i < (*m)->count; i++) {
    free((*m)->items[i]);
  }
  free((*m)->items);
  free(*m);
  *m = NULL;
}
