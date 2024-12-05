#ifndef P1_H
#define P1_H

#define CORRECT_ANSWER 2406
#define CAPACITY 100
#define PATTERN "XMAS"

typedef struct Matrix {
  int count;
  int _allocated;
  char **items;
} matrix_t;

typedef enum Direction {
  TOP_LEFT,
  TOP,
  TOP_RIGHT,
  LEFT,
  RIGHT,
  BOTTOM_LEFT,
  BOTTOM,
  BOTTOM_RIGHT
} direction_t;

void parse_line(const char *s, matrix_t *m);
/** Checks currently allocated memory and allocate more if needed. We assume
 * dynamic data size */
void check_matrix_capacity(matrix_t *m);
/** Starts pattern search. Return number of pattern matches */
int init_dfs(matrix_t *m);
/** Peforms one step in finding pattern, where:
 * @m: matrix to search within
 * @i, j: current row and column
 * @pi: current index of pattern, or current symbol of pattern we're searching
 * @d: direction in which we search
 * @acc: integer pointer which is used for accumulating results */
void perform_dfs(matrix_t *m, int i, int j, int pi, direction_t d, int *acc);
matrix_t *alloc_matrix();
void destroy_matrix(matrix_t **m);

#endif
