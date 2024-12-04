#include "p2.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>

int main(int argc, char **argv) {
  int sum = 0;

  int n1 = 0;
  int n2 = 0;
  int *n = &n1; // pointer to currently parsed integer

  char c;

  // track prefix match
  int i = 0;
  int prefix_matched = 0;

  // track instructions match
  int j = 0;
  int enabled = 1;

  while ((c = fgetc(stdin)) != EOF) {
    // parse 'do()' instruction
    if (c == DO[j]) {
      // matched
      if (DO[++j] == '\0') {
        enabled = 1;
        j = 0;
      }
      continue;
    }

    // parse 'don't()' instruction
    if (DONT[j] == c) {
      // matched
      if (DONT[++j] == '\0') {
        enabled = 0;
        j = 0;
      }
      continue;
    }

    // start matching PREFIX
    if (c == PREFIX[i++]) {
      // if we've reached end of PREFIX, it means we've matched it;
      // setting flag
      prefix_matched = PREFIX[i] == '\0';
      continue;
    } else {
      i = 0;
    }

    if (!prefix_matched) {
      continue;
    }

    // start parsing numbers inside potential 'mul(X,Y)'
    // matching integer and putting in to current pointer: n1 or n2
    if (isdigit(c)) {
      *n = *n * 10 + (c - '0');
      continue;
    }

    // comma separating two arguments in (X,Y)
    // means we're going to parse second argument
    if (c == ',') {
      n = &n2;
      continue;
    }

    // we've matched whole instruction, it's time to multiply numbers
    if (c == POSTFIX && enabled) {
      sum += n1 * n2;
    }

    // reseting variables
    // works both: for invalid match or for valid one
    n1 = 0;
    n2 = 0;
    n = &n1;
    i = 0;
    prefix_matched = 0;
  }

  assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d3/P1 result: %d\n", sum);

  return 0;
}
