#ifndef P1_H
#define P1_H

#include <sys/types.h>
#define MAX_ITERATIONS_COUNT 100000
#define CORRECT_ANSWER 521515

#define CAPACITY 200
#define SYMBOL -1
#define EMPTY 0

/** Accepts pointer and a message. If pointer is NULL, exits a program */
#define assert_ptr(ptr, message)                                               \
  {                                                                            \
    if (ptr == NULL) {                                                         \
      perror(message);                                                         \
      exit(1);                                                                 \
    }                                                                          \
  }

typedef uint *detail_t;

typedef struct Row {
  uint count;
  detail_t *details;
  /** Buffer for storing all details data. Not for direct access */
  uint *_buffer;
} row_t;

row_t *init_row();
void clean_row(row_t *r);
void parse_line(const char *, row_t *);
int get_row_sum(const row_t *prev, const row_t *cur);
void destroy_row(row_t **);

#endif
