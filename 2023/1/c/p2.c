#include "p2.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SUBS_LEN 9
const substitute_t SUBSTITUTES[SUBS_LEN] = {
    {.source = "one", .c = '1'},   {.source = "two", .c = '2'},
    {.source = "three", .c = '3'}, {.source = "four", .c = '4'},
    {.source = "five", .c = '5'},  {.source = "six", .c = '6'},
    {.source = "seven", .c = '7'}, {.source = "eight", .c = '8'},
    {.source = "nine", .c = '9'}};

int main() {
  int sum = 0;
  int cv;
  char buffer[1000];

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    // firstly parse words and insert digits
    replace_all((char *)&buffer, SUBS_LEN, (substitute_t const *)&SUBSTITUTES);
    // get first and last digit as in p1
    if (get_calibration_value((char *)&buffer, &cv) != 0) {
      perror("Error while parsing calibration value\n");
      return 1;
    }
    sum += cv;
  }

  assert(sum == CORRECT_ANSWER);
  printf("\n/2023/d1/P2 result: %d\n", sum);

  return 0;
}

void replace_all(char *s, int len, substitute_t const *substitutes) {
  int i = 0, is_replaced = 0;
  for (i = 0; (i < len) && (i <= MAX_ITERATIONS_COUNT); i++) {
    // replace all occurrences of 'sources[i]' in 's'
    do {
      is_replaced = 0;
      is_replaced =
          replace(s, substitutes[i].source, substitutes[i].c) || is_replaced;
    } while (is_replaced);
  }
}

/* Places 'target' in the middle of found 'source' in provided 's' */
int replace(char *s, const char *source, char target) {
  int i, j, start = -1, end = -1;
  for (i = 0, j = 0;
       (s[i] != '\0') && (source[j] != '\0') && (i <= MAX_ITERATIONS_COUNT);
       i++) {
    if (source[j] == '\0') {
      end = i - 1;
      break;
    }

    if (s[i] == source[j]) {
      if (start == -1) {
        start = i;
      }
      j++;
    } else {
      if (start != -1) {
        i = start;
        start = -1;
        j = 0;
      }
    }
  }
  // if 'source' is in the end of s line
  if (start != -1 && end == -1) {
    end = i - 1;
  }

  // no 'source' found
  if (start == -1 || end == -1) {
    return 0;
  }

  // set 'target' in the middle of found 'source',
  // because 'source's in line could have clashes
  s[start + ((end - start) / 2)] = target;

  return 1;
}

int get_calibration_value(char *s, int *cv) {
  int i, d1 = -1, d2 = -1;

  for (i = 0; (s[i] != '\0') && (i <= MAX_ITERATIONS_COUNT); i++) {
    if (!isdigit(s[i])) {
      continue;
    }

    if (d1 == -1) {
      d1 = s[i] - '0';
      d2 = s[i] - '0';
    } else {
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
