#include "p1.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
  unsigned long sum = 0;
  char buffer[25];

  /** Hold all wire info in array, using unique indexes */
  wire_t wires[COLLECTION_LENGTH];
  int max_zindex = 0;
  int step = 0;

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    // empty line separates two sections
    if (strlen(buffer) == 1) {
      step++;
      continue;
    }

    if (step == 0) {
      parse_inputs(buffer, wires, &max_zindex);
    }
    if (step == 1) {
      parse_gates(buffer, wires, &max_zindex);
    }
  }

  evaluate_zwires(wires, max_zindex, &sum);

  assert(sum == CORRECT_ANSWER);
  printf("\n/2024/d24/P1 result: %lu\n", sum);

  return 0;
}

void evaluate_zwires(wire_t *wires, int max_zindex, unsigned long *sum) {
  // takes highest z-number (z[00] - z[99]), evaluates its value and applies it
  // to sum using bitwise operations
  for (int i = get_zwire_from_index(max_zindex); i >= 0; i--) {
    *sum <<= 1;
    *sum |= calculate_wire(wires, get_wire_from_zwire(i));
  }
}

char calculate_wire(wire_t *wires, int wire) {
  if (wires[wire].value != NO_VALUE) {
    return wires[wire].value;
  }

  // memoize calculated values
  wires[wire].value = apply_gate(calculate_wire(wires, wires[wire].left),
                                 calculate_wire(wires, wires[wire].right),
                                 wires[wire].operation);

  return wires[wire].value;
}

void parse_inputs(const char *s, wire_t *wires, int *max_zindex) {
  int wire_index = NO_VALUE;
  // hold first three letter of wire name + 1 char for its value
  char c[4];
  char *cur = &c[0];

  do {
    if (*s == ':')
      continue;
    if (*s == ' ')
      continue;

    *cur++ = *s;
  } while (*(++s) != '\0' && *s != '\n');

  wire_index = get_wire_index(c[0], c[1], c[2]);
  // Put value but no references to other wires
  update_wire(wires, wire_index, c[3] - '0', NO_VALUE, NO_VALUE, NO_VALUE);
  update_max_zindex(max_zindex, wire_index);
}

void parse_gates(const char *s, wire_t *wires, int *max_zindex) {
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
  update_wire(wires, res_index, NO_VALUE, index_l, index_r, operation);
  update_max_zindex(max_zindex, res_index);
}

char apply_gate(char l, char r, operation_t op) {
  if (l < 0 || l > 1 || r < 0 || r > 1) {
    printf("Invalid wire value: L = %c (%d); R = %c (%d)\n", l, l, r, r);
  }

  switch (op) {
  case AND:
    return l & r;
  case OR:
    return l | r;
  case XOR:
    return l ^ r;
  default:
    printf("Illegal operation: %c (%d)\n", op, op);
    exit(1);
  }
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

static int get_zwire_from_index(int index) {
  return (((index / ALPHABET_LENGTH) % ALPHABET_LENGTH) * 10) +
         (index % ALPHABET_LENGTH);
}

static int get_wire_from_zwire(int zwire) {
  return (35 * ALPHABET_LENGTH * ALPHABET_LENGTH) +
         (zwire / 10 * ALPHABET_LENGTH) + zwire % 10;
}

static void update_max_zindex(int *max, int index) {
  if (index > *max) {
    *max = index;
  }
}

static void update_wire(wire_t *wires, int index, char value, int left,
                        int right, char operation) {
  wires[index].value = value;
  wires[index].left = left;
  wires[index].right = right;
  wires[index].operation = operation;
}
