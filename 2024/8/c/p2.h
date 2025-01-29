#ifndef P2_H
#define P2_H

#define CORRECT_ANSWER 1134

#define CAPACITY 50
#define ANTENNAS_COUNT 250 // input gives no more than that
#define NO_VALUE -1

typedef struct {
  char antenna;
  int x, y;
  int is_antinode;
} node_t;

/** Collection of same type antennas nodes */
typedef struct {
  char type;
  int count;
  node_t **nodes;
} antennas_t;

typedef struct {
  int rowlen;
  int len;
  int alen;
  node_t **rows;
  antennas_t *antennas;
} map_t;

void parse(const char *s, map_t *m);
/** Puts empty cell in to map */
static void put_empty(map_t *m, int i);
/** Puts antenna in to map, populates antennas collection */
static void put_antenna(map_t *m, const char *s, int i);

/** Evaluates all antinodes and marks them */
void set_antinodes(const map_t *m);
/** Count all antinodes */
int count_antinodes(const map_t *m);

map_t *alloc_map();
void destroy_map(map_t **m);

#endif
