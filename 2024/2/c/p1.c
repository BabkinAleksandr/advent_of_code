#include "p1.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#define NO_VALUE -1
typedef enum { INCREASE, DECREASE, UNDEFINED } direction_t;

int parse_line(char *s);

int main(int argc, char **argv) {
  int sum = 0;
  char buffer[50];

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    sum += parse_line((char *)&buffer);
  }

  assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d2/P1 result: %d\n", sum);

  return 0;
}

int parse_line(char *s) {
  int prev = NO_VALUE;
  int diff = 0;
  int n = 0;

  // determine direction
  do {
    if (isdigit(*s++)) {
      n = n * 10 + (*s - '0');
      continue;
    }

    if (prev == NO_VALUE) {
      prev = n;
      n = 0;
      continue;
    }

    diff = n - prev;

    if (diff == 0) {
      // we already know this line is unsafe
      return 0;
    }
  } while (!diff);

  do {
    if (isdigit(*s)) {
      n = n * 10 + (*s - '0');
      continue;
    }

    if (abs(n - prev) == 0 || abs(n - prev) > 3) {
      return 0;
    }

    if (n - prev > 0 && diff < 0) {
      return 0;
    }

    if (n - prev < 0 && diff > 0) {
      return 0;
    }

    diff = n - prev;
    prev = n;
    n = 0;
  } while (*(++s) != '\0' && *(++s) != '\n');

  return 1;
}
