#ifndef P2_H
#define P2_H

#define MAX_ITERATIONS_COUNT 100000
#define CORRECT_ANSWER 54885

typedef struct Substitute {
  char *source;
  char c;
} substitute_t;

int replace(char *str, const char *source, const char target);
void replace_all(char *str, int len, substitute_t const *);
int get_calibration_value(char *, int *);

#endif
