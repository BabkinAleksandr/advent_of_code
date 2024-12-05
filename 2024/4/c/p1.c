#include "p1.h"
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
  printf("\n/2024/d4/P1 result: %d\n", sum);

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

int init_dfs(matrix_t *m) {
  int sum = 0;

  for (int i = 0; i < m->count; i++) {
    for (int j = 0; m->items[i][j] != '\0'; j++) {
      if (m->items[i][j] == PATTERN[0]) {
        // start scanning from current element of pattern in all directions
        perform_dfs(m, i, j, 1, TOP_LEFT, &sum);
        perform_dfs(m, i, j, 1, TOP, &sum);
        perform_dfs(m, i, j, 1, TOP_RIGHT, &sum);
        perform_dfs(m, i, j, 1, LEFT, &sum);
        perform_dfs(m, i, j, 1, RIGHT, &sum);
        perform_dfs(m, i, j, 1, BOTTOM_LEFT, &sum);
        perform_dfs(m, i, j, 1, BOTTOM, &sum);
        perform_dfs(m, i, j, 1, BOTTOM_RIGHT, &sum);
      }
    }
  }

  return sum;
}

void perform_dfs(matrix_t *m, int i, int j, int pi, direction_t d, int *acc) {
  // found PATTERN
  if (PATTERN[pi] == '\0') {
    *acc += 1;
    return;
  }

  int len = strlen(m->items[i]);

  // Checking next element according to direction, also verifying edges of
  // matrix. If they're reached -- stop search

  switch (d) {
  case TOP_LEFT:
    if (i > 0 && j > 0 && m->items[i - 1][j - 1] == PATTERN[pi])
      perform_dfs(m, i - 1, j - 1, pi + 1, d, acc);
    break;

  case TOP:
    if (i > 0 && m->items[i - 1][j] == PATTERN[pi])
      perform_dfs(m, i - 1, j, pi + 1, d, acc);
    break;

  case TOP_RIGHT:
    if (i > 0 && j < len - 1 && m->items[i - 1][j + 1] == PATTERN[pi])
      perform_dfs(m, i - 1, j + 1, pi + 1, d, acc);
    break;

  case LEFT:
    if (j > 0 && m->items[i][j - 1] == PATTERN[pi])
      perform_dfs(m, i, j - 1, pi + 1, d, acc);
    break;

  case RIGHT:
    if (j < len - 1 && m->items[i][j + 1] == PATTERN[pi])
      perform_dfs(m, i, j + 1, pi + 1, d, acc);
    break;

  case BOTTOM_LEFT:
    if (i < m->count - 1 && j > 0 && m->items[i + 1][j - 1] == PATTERN[pi])
      perform_dfs(m, i + 1, j - 1, pi + 1, d, acc);
    break;

  case BOTTOM:
    if (i < m->count - 1 && m->items[i + 1][j] == PATTERN[pi])
      perform_dfs(m, i + 1, j, pi + 1, d, acc);
    break;

  case BOTTOM_RIGHT:
    if (i < m->count - 1 && j < len - 1 &&
        m->items[i + 1][j + 1] == PATTERN[pi])
      perform_dfs(m, i + 1, j + 1, pi + 1, d, acc);
    break;
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
