#include "p2.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
  int sum = 0;
  char buffer[50];
  int *nums = alloc_nums();

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    memset(nums, 0, sizeof(int) * CAPACITY);
    parse_line((char *)&buffer, nums);
    sum += is_safe_with_dampener(nums);
  }

  free(nums);
  nums = NULL;

  assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d2/P1 result: %d\n", sum);

  return 0;
}

int *alloc_nums() {
  int *nums = calloc(CAPACITY, sizeof(int));
  if (nums == 0) {
    perror("Nums malloc");
    exit(1);
  }
  return nums;
}

void parse_line(const char *s, int *nums) {
  do {
    if (isdigit(*s)) {
      *nums = *nums * 10 + (*s - '0');
    } else {
      nums++;
    }
  } while (*(++s) != '\0' && *s != '\n');

  *(++nums) = TERMINATOR;
}

int is_safe(int *nums) {
  int diff = nums[1] - nums[0];
  int prev_diff = diff;

  for (int i = 1; nums[i] != TERMINATOR; i++) {
    diff = nums[i] - nums[i - 1];

    if (abs(diff) == 0 || abs(diff) > 3) {
      return 0;
    }

    if (diff > 0 && prev_diff < 0) {
      return 0;
    }

    if (diff < 0 && prev_diff > 0) {
      return 0;
    }
  }

  return 1;
}

/** Simple bruteforce. Constructing new slice removing elements one by one,
 * until it's 'safe' or we have no more variants */
int is_safe_with_dampener(int *nums) {
  // check base case first
  if (is_safe(nums)) {
    return 1;
  }

  // allocate array on stack to hold intermidiate nums
  int n[CAPACITY];

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

    if (is_safe(n)) {
      return 1;
    }
  }

  return 0;
}
