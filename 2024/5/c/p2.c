#include "p2.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
  int sum = 0;
  char buffer[100];
  int step = 0;

  int **rules = alloc_rules();

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    if (strlen(buffer) <= 1) {
      step++;
      continue;
    }
    if (step == 0)
      parse_rules(buffer, rules);

    if (step == 1) {
      sum += check_rules(buffer, rules);
    }
  }

  destroy_rules(rules);

  assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d5/P2 result: %d\n", sum);

  return 0;
}

void parse_rules(const char *s, int **rules) {
  int n1 = 0, n2 = 0, *n = &n1;

  do {
    if (*s == '|') {
      n = &n2;
      continue;
    }
    *n = *n * 10 + (*s - '0');
  } while (*(++s) != '\0' && *s != '\n');

  // put them in `rules` slice
  // same integer could appear in several rules, so put all appearances in slice
  // as well, so we can find all rules for number by accessing it: rules[number]
  for (int i = 0; i < CAPACITY; i++) {
    if (rules[n1][i] == 0) {
      rules[n1][i] = n2;
      break;
    }
  }
}

int check_rules(const char *s, int **rules) {
  int nums[CAPACITY];
  int len = 0;
  int i = 0, j = 0, k = 0;
  int tmp, is_invalid = 0;

  // using 0-terminated array to avoid scanning the whole array every time
  memset(nums, 0, sizeof(int) * CAPACITY);

  // parse numbers firstly
  do {
    if (*s == ',') {
      len++;
      continue;
    }
    nums[len] = nums[len] * 10 + (*s - '0');
  } while (*(++s) != '\0' && *s != '\n');
  len++;

  // check pairs of numbers againts the rules
start:
  for (i = 0; i < len; i++) {       // <- get first number
    for (j = i + 1; j < len; j++) { // <- get second number
                                    // checking all rules under rules[n]
      for (k = 0; rules[nums[j]][k] != 0; k++) {
        // flip numbers and check if rule exist
        // if yes, it means current row violates it

        // we mark it as invalid (which is what we're searching for): flip
        // violating pair in array and run checks again (there could be more
        // than 1 violations, so do it until row becomes valid)
        if (rules[nums[j]][k] == nums[i]) {
          is_invalid = 1;
          int temp = nums[i];
          nums[i] = nums[j];
          nums[j] = temp;
          goto start; // oh no: it's GOTO!
        }
      }
    }
  }

  // here we're want to return middle number only for initially `invalid` array
  return is_invalid ? nums[len / 2] : 0;
}

int **alloc_rules() {
  int **rules = malloc(sizeof(int *) * CAPACITY);
  if (rules == NULL) {
    perror("Rules malloc");
    exit(1);
  }

  for (int i = 0; i < CAPACITY; i++) {
    rules[i] = calloc(CAPACITY, sizeof(int) * CAPACITY);
    if (rules[i] == NULL) {
      perror("Rules[i] calloc");
      exit(1);
    }
  }

  return rules;
}

void destroy_rules(int **rules) {
  for (int i = 0; i < CAPACITY; i++) {
    free(rules[i]);
  }
  free(rules);
}
