#include "p2.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
  char result[RESULT_LENGTH];
  char buffer[25];

  // gates in order
  wire_t wires[1000];
  int len = 0;

  int wrong_wires[WW_LENGTH];
  int wlen = 0;

  int max_zindex = 0;
  int step = 0;

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    // empty line separates two sections
    if (strlen(buffer) == 1) {
      step++;
      continue;
    }

    if (step == 0) {
      // we can easily skip inputs parsing here
      continue;
    }
    if (step == 1) {
      parse_gates(buffer, wires, &len, &max_zindex);
    }
  }

  wlen = validate_adder(wires, len, wrong_wires, max_zindex);
  qsort(wrong_wires, WW_LENGTH, sizeof(int), cmp_ints_asc);
  collect_wrong_wires(wrong_wires, WW_LENGTH, result, RESULT_LENGTH);

  assert(strcmp(CORRECT_ANSWER, result) == 0);
  printf("\n/2024/d24/P2 result: %s\n", result);

  return 0;
}

void parse_gates(const char *s, wire_t *wires, int *wires_len,
                 int *max_zindex) {
  char c[3];
  char *cur = &c[0];

  int index_l = NO_VALUE, index_r = NO_VALUE;
  operation_t operation = NO_VALUE;

  do {
    if (*s == '-' || *s == '>')
      continue;

    if (*s == ' ') {
      if (strncmp(c, AND_STR, 3) == 0) {
        operation = AND;
      } else if (strncmp(c, OR_STR, 2) == 0) {
        operation = OR;
      } else if (strncmp(c, XOR_STR, 3) == 0) {
        operation = XOR;
      } else {
        if (index_l == NO_VALUE) {
          index_l = get_wire_index(c[0], c[1], c[2]);
        } else if (index_r == NO_VALUE) {
          index_r = get_wire_index(c[0], c[1], c[2]);
        }
      }

      cur = &c[0];
      continue;
    }

    *cur++ = *s;
  } while (*(++s) != '\0' && *s != '\n');

  int res_index = get_wire_index(c[0], c[1], c[2]);
  wires[*wires_len].idx = res_index;
  wires[*wires_len].left = index_l;
  wires[*wires_len].right = index_r;
  wires[*wires_len].operation = operation;
  *wires_len += 1;

  update_max_zindex(max_zindex, res_index);
}

int validate_adder(wire_t *wires, int len, int *wrong_wires, int max_zindex) {
  int wlen = 0;
  int idx, left_idx, right_idx;
  operation_t operation = NO_VALUE;

  for (int i = 0; i < len; i++) {
    if (wlen == WW_LENGTH + 1) {
      printf("Excessive wrong wire found\n");
      exit(1);
    }

    idx = wires[i].idx;
    left_idx = wires[i].left;
    right_idx = wires[i].right;
    operation = wires[i].operation;

    // Z wire could be calculated only using XOR operation, except the last
    // index, which is carry
    if (is_z(idx) && operation != XOR && idx != max_zindex) {
      add_dedup(wrong_wires, &wlen, idx);
    }

    // XOR is used only for operations on end wires (X, Y, Z)
    if (operation == XOR && !is_end_wire(idx) && !is_end_wire(left_idx) &&
        !is_end_wire(right_idx)) {
      add_dedup(wrong_wires, &wlen, idx);
    }

    // AND on non-X00 wire should feed into OR (which produces carry-out)
    if (operation == AND && left_idx != X00 && right_idx != X00) {
      for (int j = 0; j < len; j++) {
        if ((idx == wires[j].left || idx == wires[j].right) &&
            wires[j].operation != OR) {
          add_dedup(wrong_wires, &wlen, idx);
        }
      }
    }

    // XOR produces sum and should not feed into OR (which produces carry-out)
    if (operation == XOR) {
      for (int j = 0; j < len; j++) {
        if ((idx == wires[j].left || idx == wires[j].right) &&
            wires[j].operation == OR) {
          add_dedup(wrong_wires, &wlen, idx);
        }
      }
    }
  }

  return wlen;
}

void collect_wrong_wires(int *wrong_wires, int len, char *result, int r_len) {
  char *cur = result;
  for (int i = 0; i < len; i++) {
    reverse_wire_index(wrong_wires[i], cur);
    cur[3] = ',';
    cur += 4;
  }
  result[r_len - 1] = '\0'; // terminate symbol overwrites hanging comma
}

static int char_to_int(char c) {
  if (c >= '0' && c <= '9') {
    // '0' to '9' maps to 0-9
    return c - '0';
  }

  if (c >= 'a' && c <= 'z') {
    // 'a' to 'z' maps to 10-35
    return c - 'a' + 10;
  }

  printf("Illegal character: %c (%d)\n", c, c);
  exit(1);
}

static int get_wire_index(char c1, char c2, char c3) {
  return (char_to_int(c1) * ALPHABET_LENGTH * ALPHABET_LENGTH) +
         (char_to_int(c2) * ALPHABET_LENGTH) + char_to_int(c3);
}

static void update_max_zindex(int *max, int index) {
  if (index > *max) {
    *max = index;
  }
}

static int is_x(int index) { return index >= X00 && index <= XZZ; }
static int is_y(int index) { return index >= Y00 && index <= YZZ; }
static int is_z(int index) { return index >= Z00; }
static int is_end_wire(int index) {
  return is_x(index) || is_y(index) || is_z(index);
}

static void reverse_wire_index(int index, char *result) {
  // Calculate the characters in reverse order
  result[0] = index / (36 * 36); // Extract the first character
  result[1] = (index / 36) % 36; // Extract the second character
  result[2] = index % 36;        // Extract the third character

  // Convert numeric values back to characters
  for (int i = 0; i < 3; i++) {
    if (result[i] < 10) {
      result[i] += '0'; // Map 0-9 to '0'-'9'
    } else {
      result[i] += 'a' - 10; // Map 10-35 to 'a'-'z'
    }
  }
}

static int cmp_ints_asc(const void *a, const void *b) {
  return *(const int *)a - *(const int *)b;
}

static void add_dedup(int *wires, int *len, int wire) {
  for (int i = 0; i < *len; i++) {
    if (wires[i] == wire) {
      return;
    }
  }

  wires[*len] = wire;
  *len += 1;
}
