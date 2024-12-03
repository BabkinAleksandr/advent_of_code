#include "p2.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#define TERMINATOR -1
typedef enum { INCREASE, DECREASE, UNDEFINED } direction_t;

int *parse_line(const char *s);
int check_safety_dampener(int *nums);
int check_safety(int *);

int main(int argc, char **argv) {
  int sum = 0;
  char buffer[50];

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    int *nums = parse_line((char *)&buffer);
    sum += check_safety_dampener(nums);
  }

  // assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d2/P1 result: %d\n", sum);

  return 0;
}

int *parse_line(const char *s) {
  int *nums = calloc(20, sizeof(int));
  if (nums == 0) {
    perror("Nums malloc");
    exit(1);
  }

  int *cur = nums;

  do {
    if (isdigit(*s)) {
      *cur = *cur * 10 + (*s - '0');
    } else {
      cur++;
    }
  } while (*(++s) != '\0' && *s != '\n');

  *(++cur) = TERMINATOR;

  return nums;
}

int check_safety_dampener(int *nums) {
  if (check_safety(nums)) {
    return 1;
  }

  int *n = calloc(20, sizeof(int));
  if (nums == 0) {
    perror("Nums malloc");
    exit(1);
  }

  for (int skip = 0; nums[skip] != TERMINATOR; skip++) {
    int c = 0;

    for (int i = 0; nums[i] != TERMINATOR; i++) {
      if (i == skip) {
        continue;
      }
      n[c] = nums[i];
      c++;
    }

    n[c] = TERMINATOR;

    if (check_safety(n)) {
      return 1;
    }
  }

  return 0;
}

int check_safety(int *nums) {
  int direction = nums[1] - nums[0] > 0 ? INCREASE : DECREASE;

  for (int i = 1; nums[i] != TERMINATOR; i++) {
    int diff = nums[i] - nums[i - 1];

    if (abs(diff) == 0 || abs(diff) > 3) {
      // printf(">1\n");
      return 0;
    }

    if (diff > 0 && direction == DECREASE) {
      // printf(">2\n");
      return 0;
    }

    if (diff < 0 && direction == INCREASE) {
      // printf(">3\n");
      return 0;
    }
  }

  return 1;
}
