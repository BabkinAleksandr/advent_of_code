#include "p1.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
  // Position that is step on is counted. It means initial position is left
  // uncounted For that reason start with 1
  int sum = 1;
  char buffer[200];

  map_t *map = alloc_map();

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    parse_map(buffer, map);
  }

  traverse_map(map, &sum);
  // print_map(map);
  destroy_map(&map);

  assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d6/P1 result: %d\n", sum);

  return 0;
}

void parse_map(const char *s, map_t *m) {
  // copy provided s in to map
  int len = strlen(s) - 1; // strip out EOL
  if (m->row_len != 0 && len != m->row_len) {
    perror("Inconsistent row length");
    exit(1);
  }

  m->row_len = len;
  memcpy(m->rows[m->count++], s, sizeof(char) * len);

  int guard = -1;
  // search for guard in line
  // if it's found, save it's position and assign enum value
  for (int i = 0; i < len; i++) {
    switch (s[i]) {
    case GUARD_UP_STR:
      guard = GUARD_UP;
      break;
    case GUARD_RIGHT_STR:
      guard = GUARD_RIGHT;
      break;
    case GUARD_DOWN_STR:
      guard = GUARD_DOWN;
      break;
    case GUARD_LEFT_STR:
      guard = GUARD_LEFT;
      break;
    }

    if (guard != -1) {
      // should be exactly one line with single guard,
      // otherwise input is invalid
      if (m->pos[0] != -1 || m->pos[1] != -1) {
        printf("Position already defined\n");
        exit(1);
      }

      m->pos[0] = i;
      m->pos[1] = m->count - 1;
      m->rows[m->count - 1][i] = guard;
      break;
    }
  }
}

void traverse_map(map_t *m, int *acc) {
  // it's a valid check before traversing
  // but the same condition could be met during traversing
  if (m->pos[0] == -1 || m->pos[1] == -1) {
    perror("Undefined start point");
    exit(1);
  }

  if (m->count == 0) {
    perror("Empty map");
    exit(1);
  }

  // position
  int *x = &(m->pos[0]);
  int *y = &(m->pos[1]);
  int prev_x = 0, prev_y = 0;

  // velocity by axises
  int x_vel = 0;
  int y_vel = 0;
  set_velocity(m->rows[*y][*x], &x_vel, &y_vel);

  while (*x >= 0 && *x < m->row_len && *y >= 0 && *y < m->count) {
    // make step
    prev_x = *x;
    prev_y = *y;
    *y += y_vel;
    *x += x_vel;

    // check bounds first
    if (*x < 0 || *x >= m->row_len || *y < 0 || *y >= m->count) {
      // mark last one to be able to print the final state correctly
      m->rows[prev_y][prev_x] = VISITED;
      continue;
    }

    // if hit obstacle, step back and turn clockwise
    if (m->rows[*y][*x] == OBSTACLE) {
      *y += y_vel * -1;
      *x += x_vel * -1;
      // leverage serial int enum values to calculate next guard
      m->rows[*y][*x] += 1;
      m->rows[*y][*x] %= 4;
      set_velocity(m->rows[*y][*x], &x_vel, &y_vel);
      continue;
    }

    // count if not visited
    *acc += m->rows[*y][*x] != VISITED;

    // if step is correct, mark previous as visited and move guard to new
    // position
    m->rows[*y][*x] = m->rows[prev_y][prev_x];
    m->rows[prev_y][prev_x] = VISITED;
  }
}

static void set_velocity(guard_t guard, int *x_vel, int *y_vel) {
  switch (guard) {
  case GUARD_UP:
    *x_vel = 0;
    *y_vel = -1;
    break;
  case GUARD_RIGHT:
    *x_vel = 1;
    *y_vel = 0;
    break;
  case GUARD_DOWN:
    *x_vel = 0;
    *y_vel = 1;
    break;
  case GUARD_LEFT:
    *x_vel = -1;
    *y_vel = 0;
    break;
  default:
    printf("Illegal guard: %d\n", guard);
    exit(1);
  }
}

map_t *alloc_map() {
  map_t *map = malloc(sizeof(map_t));
  if (map == NULL) {
    perror("Map alloc");
    exit(1);
  }

  map->rows = malloc(sizeof(char *) * CAPACITY);
  if (map->rows == NULL) {
    perror("Rows malloc");
    exit(1);
  }

  // allocate cells for the whole matrix at once and just assign segments of
  // allocated memory
  char *cells = malloc(sizeof(char) * CAPACITY * CAPACITY);
  if (cells == NULL) {
    perror("Map cells malloc");
    exit(1);
  }
  for (int i = 0; i < CAPACITY; i++) {
    map->rows[i] = cells + (CAPACITY * i);
  }

  map->pos[0] = -1;
  map->pos[1] = -1;
  map->count = 0;
  map->row_len = 0;

  return map;
}

void destroy_map(map_t **m) {
  free((*m)->rows[0]);
  free((*m)->rows);
  free(*m);
  *m = NULL;
}

static void print_map(map_t *m) {
  printf("Map:\n");
  for (int i = 0; i < m->count; i++) {
    printf("\t");
    for (int j = 0; j < m->row_len; j++) {
      switch (m->rows[i][j]) {
      case GUARD_UP:
        if (j < m->row_len - 1) {
          printf("^");
        }
        break;
      case GUARD_RIGHT:
        printf(">");
        break;
      case GUARD_DOWN:
        printf("v");
        break;
      case GUARD_LEFT:
        printf("<");
        break;
      default:
        printf("%c", m->rows[i][j]);
      }
    }
    printf("\n");
  }
}
