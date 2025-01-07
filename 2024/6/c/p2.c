#include "p2.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
  int sum = 0;
  char buffer[200];

  int visited[CAPACITY * CAPACITY];
  int vlen = 0;

  map_t *orig_map = alloc_map();
  /** Temporal map used for storing traversals progress */
  map_t *traversal_map = alloc_map();

  // firstly parse map
  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    parse_map(buffer, orig_map);
  }

  // copy original map and traverse it until went outside
  // than gather all visited cells in array
  copy_map(orig_map, traversal_map);
  traverse_map(traversal_map);
  get_visited(traversal_map, visited, &vlen);

  // set obstacle on one of visited cells and simulate until went outside or
  // detect a loop.
  // there is no point to try set obstacle on all of the empty
  // cells, because robot would never reach them anyway
  sum = try_set_obstacle(orig_map, traversal_map, visited, vlen);

  // print_map(map);
  destroy_map(&traversal_map);
  destroy_map(&orig_map);

  assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d6/P2 result: %d\n", sum);

  return 0;
}

void parse_map(const char *s, map_t *m) {
  // copy provided s in to map
  int len = strlen(s) - 1;
  if (m->row_len != 0 && len != m->row_len) {
    perror("Inconsistent row length");
    exit(1);
  }

  m->row_len = len;

  unsigned int val = 0;
  char guard_set = 0;
  for (int i = 0; i < len; i++) {
    switch (s[i]) {
    case GUARD_UP_STR:
      val = GUARD_UP;
      guard_set = 1;
      break;
    case GUARD_RIGHT_STR:
      val = GUARD_RIGHT;
      guard_set = 1;
      break;
    case GUARD_DOWN_STR:
      val = GUARD_DOWN;
      guard_set = 1;
      break;
    case GUARD_LEFT_STR:
      val = GUARD_LEFT;
      guard_set = 1;
      break;
    case OBSTACLE_STR:
      val = OBSTACLE;
      break;
    case EMPTY_STR:
      val = EMPTY;
      break;
    default:
      printf("Invalid symbol: %c\n", s[i]);
      exit(1);
    }

    m->rows[m->count][i] = val;

    if (guard_set) {
      // should be exactly one line with single guard,
      // otherwise input is invalid
      if (m->pos[0] != -1 || m->pos[1] != -1) {
        printf("Position already defined\n");
        exit(1);
      }

      m->pos[0] = i;
      m->pos[1] = m->count;
      guard_set = 0;
    }
  }

  m->count += 1;
}

int try_set_obstacle(map_t *m, map_t *simmap, int *visited, int vlen) {
  int sum = 0;
  int x = 0;
  int y = 0;

  for (int i = 0; i < vlen; i++) {
    // erase simulation map firstly
    copy_map(m, simmap);

    x = visited[i] / simmap->row_len;
    y = visited[i] % simmap->row_len;

    // then set obstacle and run simulation
    if (simmap->rows[y][x] == EMPTY) {
      simmap->rows[y][x] = OBSTACLE;
      sum += traverse_map(simmap);
    }
  }

  return sum;
}

int traverse_map(map_t *m) {
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
  int x_vel = 0, y_vel = 0;
  unsigned int guard = m->rows[*y][*x] & GUARD_MASK;
  set_velocity(guard, &x_vel, &y_vel);

  while (*x >= 0 && *x < m->row_len && *y >= 0 && *y < m->count) {
    // get current guard bit
    guard = m->rows[*y][*x] & GUARD_MASK;
    // make step
    prev_x = *x;
    prev_y = *y;
    *y += y_vel;
    *x += x_vel;

    // check bounds first
    if (*x < 0 || *x >= m->row_len || *y < 0 || *y >= m->count) {
      return 0;
    }

    // if already visited with same direction guard - it's a loop
    // we have 4 bits for guard which correspond to same visited bits shifted to
    // the left: 'guard << 4' will give corresponding 'visited' bit
    if ((m->rows[*y][*x] & VISITED_MASK) & (guard << 4)) {
      return 1;
    }

    // if hit obstacle, step back and turn clockwise
    if (m->rows[*y][*x] & OBSTACLE) {
      *y += y_vel * -1;
      *x += x_vel * -1;
      // record visited state
      m->rows[*y][*x] |= guard << 4;

      turn_clockwise(m);
      set_velocity(m->rows[*y][*x] & GUARD_MASK, &x_vel, &y_vel);
      continue;
    }

    // if step is correct, mark previous as visited and move guard to new
    // position
    m->rows[*y][*x] |= guard;
    m->rows[prev_y][prev_x] ^= guard;
    m->rows[prev_y][prev_x] |= guard << 4;
  }

  return 0;
}

static void turn_clockwise(map_t *m) {
  int x = m->pos[0];
  int y = m->pos[1];
  // get guard bit
  int guard = m->rows[y][x] & GUARD_MASK;
  // move it left to get next guard or zero if we overflowed
  int calculated_guard = ((m->rows[y][x] & GUARD_MASK) << 1) & GUARD_MASK;
  // get final next guard: bit moved to left or min guard bit
  int next_guard = calculated_guard ? calculated_guard : MIN_GUARD;

  // remove prev guard from current position
  m->rows[y][x] ^= guard;
  // add next guard to next position
  m->rows[y][x] |= next_guard;
}

static void set_velocity(unsigned int guard, int *x_vel, int *y_vel) {
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

void get_visited(map_t *m, int *visited, int *vlen) {
  for (int i = 0; i < m->count; i++) {
    for (int j = 0; j < m->row_len; j++) {
      if (m->rows[i][j] & VISITED_MASK) {
        // calc unique index for cell coordinates
        // it could be reversed than to get original coords
        visited[*vlen] = j * m->row_len + i;
        *vlen += 1;
      }
    }
  }
}

map_t *alloc_map() {
  map_t *map = malloc(sizeof(map_t));
  if (map == NULL) {
    perror("Map alloc");
    exit(1);
  }

  map->rows = malloc(sizeof(unsigned int *) * CAPACITY);
  if (map->rows == NULL) {
    perror("Rows malloc");
    exit(1);
  }

  // allocate cells for the whole matrix at once and just assign segments of
  // allocated memory
  unsigned int *cells = malloc(sizeof(unsigned int) * CAPACITY * CAPACITY);
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

void copy_map(map_t const *s, map_t *t) {
  t->count = s->count;
  t->row_len = s->row_len;
  t->pos[0] = s->pos[0];
  t->pos[1] = s->pos[1];

  memcpy(t->rows[0], s->rows[0], sizeof(unsigned int) * CAPACITY * CAPACITY);

  for (int i = 0; i < s->count; i++) {
    for (int j = 0; j < s->row_len; j++) {
      t->rows[i][j] = s->rows[i][j];
    }
  }
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
      if (m->rows[i][j] & OBSTACLE) {
        printf("#");
      } else if (m->rows[i][j] & GUARD_UP) {
        printf("^");
      } else if (m->rows[i][j] & GUARD_RIGHT) {
        printf(">");
      } else if (m->rows[i][j] & GUARD_DOWN) {
        printf("v");
      } else if (m->rows[i][j] & GUARD_LEFT) {
        printf("<");
      } else if (m->rows[i][j] & VISITED_MASK) {
        printf("x");
      } else {
        printf(".");
      }
    }
    printf("\n");
  }
}
