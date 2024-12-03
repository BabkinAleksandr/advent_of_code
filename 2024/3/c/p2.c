#include "p2.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>

#define PREFIX "mul("
#define POSTFIX ')'
#define DO "do()"
#define DONT "don't()"

int main(int argc, char **argv) {
  int sum = 0;

  int n1 = 0;
  int n2 = 0;
  int *n = &n1;

  char c;

  // track prefix
  int i = 0;
  int prefix_match = 0;

  int j = 0; // track instruction
  int enabled = 1;

  while ((c = fgetc(stdin)) != EOF) {
    // parse instruction
    if (c == DO[j]) {
      j++;
      if (DO[j] == '\0') {
        enabled = 1;
        j = 0;
        continue;
      }
      continue;
    } else if (DONT[j] == c) {
      j++;
      if (DONT[j] == '\0') {
        enabled = 0;
        j = 0;
        continue;
      }
      continue;
    }

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
        if (enabled) {
          sum += n1 * n2;
        }
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
