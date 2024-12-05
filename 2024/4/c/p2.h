#ifndef P2_H
#define P2_H

#define CORRECT_ANSWER 1807
#define CAPACITY 100

typedef struct Matrix {
  int count;
  int _allocated;
  char **items;
} matrix_t;

void parse_line(const char *s, matrix_t *m);
/** Checks currently allocated memory and allocate more if needed. We assume
 * dynamic data size */
void check_matrix_capacity(matrix_t *m);

/** Starts pattern search. Return number of pattern matches */
int init_dfs(matrix_t *m);

/** Peforms one step in finding pattern, where:
 * @m: matrix to search within
 * @i, j: current row and column
 * @acc: integer pointer which is used for accumulating results */
void perform_dft(matrix_t *m, int i, int j, int *acc);

matrix_t *alloc_matrix();
void destroy_matrix(matrix_t **m);

#endif
