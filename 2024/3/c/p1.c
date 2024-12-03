#include "p1.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>

#define PREFIX "mul("
#define POSTFIX ')'

int main(int argc, char **argv) {
  int sum = 0;

  int n1 = 0;
  int n2 = 0;
  int *n = &n1;

  char c;

  int i = 0;
  int prefix_match = 0;
  while ((c = fgetc(stdin)) != EOF) {
    if (c == PREFIX[i]) {
      i++;
      if (PREFIX[i] == '\0') {
        prefix_match = 1;
        continue;
      }
    }
    if (prefix_match) {
      if (isdigit(c)) {
        *n = *n * 10 + (c - '0');
        continue;
      } else if (c == ',') {
        n = &n2;
        continue;
      } else if (c == POSTFIX) {
        sum += n1 * n2;
        n1 = 0;
        n2 = 0;
        n = &n1;
        prefix_match = 0;
        i = 0;
        continue;
      } else {
        n1 = 0;
        n2 = 0;
        n = &n1;
        prefix_match = 0;
        i = 0;
      }
    }
  }

  // assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d3/P1 result: %d\n", sum);

  return 0;
}
