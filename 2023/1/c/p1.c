#include "p1.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  int sum = 0;
  int cv;
  char buffer[1000];
  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    if (get_calibration_value((char *)&buffer, &cv) != 0) {
      perror("Error while parsing calibration value\n");
      return 1;
    }
    sum += cv;
  }

  assert(sum == CORRECT_ANSWER);
  printf("\n/2023/d1/P1 result: %d\n", sum);

  return 0;
}

int get_calibration_value(char *s, int *cv) {
  int i;
  short d1 = -1, d2 = -1;

  // iterating over line
  for (i = 0; (s[i] != '\0') && (i <= MAX_ITERATIONS_COUNT); i++) {
    if (!isdigit(s[i])) {
      continue;
    }

    // set the first digit as both d1 and d2
    if (d1 == -1) {
      d1 = s[i] - '0';
      d2 = s[i] - '0';
    } else {
      // last digit overwrites d2
      d2 = s[i] - '0';
    }
  }

  if (i >= MAX_ITERATIONS_COUNT) {
    return 1;
  }

  if (d1 == -1 || d2 == -1) {
    return 1;
  }

  *cv = d1 * 10 + d2;

  return 0;
}
