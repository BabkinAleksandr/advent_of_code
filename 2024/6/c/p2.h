#ifndef P2_H
#define P2_H

#define CORRECT_ANSWER 2143
#define CAPACITY 200

#define EMPTY_STR '.'
#define GUARD_UP_STR '^'
#define GUARD_RIGHT_STR '>'
#define GUARD_DOWN_STR 'v'
#define GUARD_LEFT_STR '<'
#define OBSTACLE_STR '#'

// store state of a cell as bitmask
#define EMPTY 0b00000
#define OBSTACLE 0b00001

#define GUARD_UP 0b00010
#define GUARD_RIGHT 0b00100
#define GUARD_DOWN 0b01000
#define GUARD_LEFT 0b10000
#define MIN_GUARD GUARD_UP

#define VISITED_UP (GUARD_UP << 4)
#define VISITED_RIGHT (GUARD_RIGHT << 4)
#define VISITED_DOWN (GUARD_DOWN << 4)
#define VISITED_LEFT (GUARD_LEFT << 4)

#define GUARD_MASK (GUARD_UP | GUARD_RIGHT | GUARD_DOWN | GUARD_LEFT)
#define VISITED_MASK (GUARD_MASK << 4)

#define NO_VALUE -1

typedef struct Map {
  int count;
  int row_len;
  unsigned int **rows;
  int pos[2]; // guard position, [X,Y]
} map_t;

/** Allocates new map */
map_t *alloc_map();
/** Copies Source map to Target */
void copy_map(map_t const *s, map_t *t);
/** Deallocates memory for map */
void destroy_map(map_t **m);

void parse_map(const char *s, map_t *m);
/** Takes original map and simulation map. Tries to set obstacle for every cell
 * in 'visited' and then runs traversal on simmap.
 * Returns count of loops detected
 */
int try_set_obstacle(map_t *m, map_t *simmap, int *visited, int vlen);
/** Takes a map and makes traversal. Returns '1' if loop detected, '0' --
 * otherwise
 */
int traverse_map(map_t *m);
/** Gathers all visited cells in from map */
void get_visited(map_t *m, int *visited, int *vlen);

static void turn_clockwise(map_t *m);
static void set_velocity(unsigned int guard, int *x_vel, int *y_vel);
static void print_map(map_t *m);

#endif
