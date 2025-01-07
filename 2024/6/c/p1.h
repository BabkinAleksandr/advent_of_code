#ifndef P1_H
#define P1_H

#define CORRECT_ANSWER 5305
#define CAPACITY 200

#define EMPTY '.'
#define VISITED 'x'

#define GUARD_UP_STR '^'
#define GUARD_RIGHT_STR '>'
#define GUARD_DOWN_STR 'V'
#define GUARD_LEFT_STR '<'
#define OBSTACLE '#'

typedef enum {
  GUARD_UP = 0,
  GUARD_RIGHT = 1,
  GUARD_DOWN = 2,
  GUARD_LEFT = 3
} guard_t;

#define NO_VALUE -1

typedef struct Map {
  int count;
  int row_len;
  char **rows;
  int pos[2]; // guard position, [X,Y]
} map_t;

map_t *alloc_map();
void destroy_map(map_t **m);

void parse_map(const char *s, map_t *m);
void traverse_map(map_t *m, int *acc);

static void set_velocity(guard_t guard, int *x_vel, int *y_vel);
static void print_map(map_t *m);

#endif
