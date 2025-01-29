#include "p2.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
  int sum = 0;
  char buffer[200];
  map_t *map = alloc_map();

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    parse(buffer, map);
  }

  set_antinodes(map);

  sum = count_antinodes(map);
  destroy_map(&map);

  assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d8/P2 result: %d\n", sum);

  return 0;
}

void parse(const char *s, map_t *m) {
  antennas_t *collection = NULL;
  int i;

  for (i = 0; s[i] != '\0' && s[i] != '\n'; i++) {
    if (s[i] == '.') {
      put_empty(m, i);
    } else {
      put_antenna(m, s, i);
    }
  }

  if (m->rowlen != 0 && m->rowlen != i) {
    printf("Inconsistent row length\n");
    exit(1);
  }

  m->len += 1;
  m->rowlen = i;
}

static void put_empty(map_t *m, int i) {
  m->rows[m->len][i].antenna = NO_VALUE;
  m->rows[m->len][i].is_antinode = 0;
}

static void put_antenna(map_t *m, const char *s, int i) {
  antennas_t *collection = NULL;
  int j;

  // set node values first
  m->rows[m->len][i].antenna = s[i] - '0';
  m->rows[m->len][i].x = i;
  m->rows[m->len][i].y = m->len;
  m->rows[m->len][i].is_antinode = 0;

  // then put antenna's node in to antennas collection:
  // existing one, or a new one
  for (j = 0; j < m->alen; j++) {
    if (m->antennas[j].type == s[i] - '0') {
      collection = &m->antennas[j];
      break;
    }
  }

  if (collection == NULL) {
    collection = &m->antennas[m->alen++];
  }

  collection->type = s[i] - '0';
  collection->nodes[collection->count++] = &m->rows[m->len][i];
}

void set_antinodes(const map_t *m) {
  node_t *cur, *next;
  int diff_x, diff_y, ax, ay, bx, by;
  int out, h_level;

  // traverse through collections of same-kind antennas
  for (int i = 0; i < m->alen; i++) {
    // take one and compare to others
    for (int j = 0; j < m->antennas[i].count - 1; j++) {
      cur = m->antennas[i].nodes[j];

      for (int k = j + 1; k < m->antennas[i].count; k++) {
        next = m->antennas[i].nodes[k];
        // if there are more than 1 of antennas of this kind -
        // all of them are antinodes
        cur->is_antinode = 1;
        next->is_antinode = 1;

        // antinodes would lie on the straight line, which two nodes create
        // so we evaluate coordinates from both sides of two given nodes - it's
        // antinodes
        diff_x = cur->x - next->x;
        diff_y = cur->y - next->y;

        // set default harmonics level and out of bounds count
        h_level = 1;
        out = 0;

        // searching until all of the potential antinodes are out of bounds
        while (out < 2) {
          out = 0;

          ax = cur->x + diff_x * h_level;
          ay = cur->y + diff_y * h_level;

          bx = next->x - diff_x * h_level;
          by = next->y - diff_y * h_level;

          // and if coordinates are within map - mark it as antinode
          if (ax >= 0 && ax < m->rowlen && ay >= 0 && ay < m->len) {
            m->rows[ay][ax].is_antinode = 1;
          } else {
            out += 1;
          }

          if (bx >= 0 && bx < m->rowlen && by >= 0 && by < m->len) {
            m->rows[by][bx].is_antinode = 1;
          } else {
            out += 1;
          }

          h_level += 1;
        }
      }
    }
  }
}

int count_antinodes(const map_t *m) {
  int sum = 0;
  for (int i = 0; i < m->len; i++) {
    for (int j = 0; j < m->rowlen; j++) {
      sum += m->rows[i][j].is_antinode;
    }
  }
  return sum;
}

map_t *alloc_map() {
  map_t *m = malloc(sizeof(map_t));
  if (m == NULL) {
    perror("Map alloc");
    exit(1);
  }
  m->rowlen = 0;
  m->len = 0;
  m->alen = 0;

  m->rows = malloc(sizeof(node_t *) * CAPACITY);
  if (m->rows == NULL) {
    perror("Rows malloc");
    exit(1);
  }

  node_t *nodes = malloc(sizeof(node_t) * CAPACITY * CAPACITY);
  if (nodes == NULL) {
    perror("Map cells malloc");
    exit(1);
  }
  for (int i = 0; i < CAPACITY; i++) {
    m->rows[i] = nodes + (CAPACITY * i);
  }

  m->antennas = malloc(sizeof(antennas_t) * 62); // size of alphabet
  if (m->antennas == NULL) {
    perror("Antennas malloc");
    exit(1);
  }

  node_t **nodes_p = malloc(sizeof(node_t *) * 62 * CAPACITY * CAPACITY);
  if (nodes_p == NULL) {
    perror("Antennas coll malloc");
    exit(1);
  }

  for (int i = 0; i < 62; i++) {
    m->antennas[i].count = 0;
    m->antennas[i].type = -1;
    m->antennas[i].nodes = nodes_p + (i * CAPACITY * CAPACITY);
  }

  return m;
}

void destroy_map(map_t **m) {
  // destroy inner antennas collection
  free((*m)->antennas->nodes);
  // destroy antennas
  free((*m)->antennas);
  // destroy nodes
  free((*m)->rows[0]);
  // destroy rows
  free((*m)->rows);
  // destroy map
  free(*m);
  *m = NULL;
}
