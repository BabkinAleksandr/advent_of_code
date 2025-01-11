#include "p2.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
  unsigned long sum = 0;
  char buffer[200];

  unsigned long calibration_v = 0;
  int nums[CAPACITY] = {0};

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    calibration_v = 0;
    memset(nums, TERMINATOR, sizeof(int) * CAPACITY);
    parse_eq(buffer, &calibration_v, nums);

    sum += search_sum(nums, 1, calibration_v, nums[0]);
  }

  assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d7/P2 result: %lu\n", sum);

  return 0;
}

void parse_eq(const char *s, unsigned long *calibration_v, int *nums) {
  int cv_parsing = 1;
  int *n;

  do {
    if (*s == ':') {
      cv_parsing = 0;
      continue;
    }
    if (*s == ' ') {
      n = nums++;
      continue;
    }

    if (cv_parsing) {
      *calibration_v = *calibration_v * 10 + (*s - '0');
    } else {
      *n = *n * 10 + (*s - '0');
    }

  } while (*(++s) != '\n' && *s != '\0');
}

unsigned long search_sum(int *nums, int i, unsigned long c,
                         unsigned long ires) {
  if (nums[i] == TERMINATOR) {
    return c == ires ? c : 0;
  }

  unsigned long with_sum = search_sum(nums, i + 1, c, ires + nums[i]);
  unsigned long with_mult = search_sum(nums, i + 1, c, ires * nums[i]);
  unsigned long with_concat =
      search_sum(nums, i + 1, c, concatl(ires, nums[i]));

  if (with_sum != 0) {
    return with_sum;
  }

  if (with_mult != 0) {
    return with_mult;
  }

  if (with_concat != 0) {
    return with_concat;
  }

  return 0;
}

static unsigned long concatl(unsigned long num, int n) {
  int tmp = n;
  int radix = 1;

  while (tmp) {
    radix *= 10;
    tmp /= 10;
  }

  // concatenation is nothing more than 'num' multiplied by n's radix + n
  return num * radix + n;
}
