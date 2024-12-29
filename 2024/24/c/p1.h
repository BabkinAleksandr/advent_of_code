#ifndef P1_H
#define P1_H

#define CORRECT_ANSWER 52956035802096
#define NO_VALUE -1
#define ALPHABET_LENGTH 36
/** We have a wires, which names are composited from case-insensitive
 * alpha-numeric symbols ([0-9,a-z]) and 3 symbols long (from a00 to zzz)
 * So the max calculated index (see [calculate_wire] for more) is 46656 */
#define COLLECTION_LENGTH 46657

#define AND_STR "AND"
#define OR_STR "OR"
#define XOR_STR "XOR"

typedef enum { AND, OR, XOR } operation_t;

typedef struct {
  char value;
  int left;
  int right;
  operation_t operation;
} wire_t;

/** Parses values for defined wires */
void parse_inputs(const char *s, wire_t *wires, int *max_zindex);
/** Parses gates */
void parse_gates(const char *s, wire_t *wires, int *max_zindex);
/** Gets max zindex and calculates result number */
void evaluate_zwires(wire_t *wires, int max_zindex, unsigned long *sum);
/** Applies bitwise operation on left and right */
char apply_gate(char l, char r, operation_t op);
/** Calculates wire value by reading value if it's defined, or evaluating chain
 * of gates */
char calculate_wire(wire_t *wires, int wire);

/** Converts character (0-9,a-z) to unique decimal */
static int char_to_int(char c);
/** Converts three-letter wire name to unique array index */
static int get_wire_index(char c1, char c2, char c3);
/** Get order number of z-wire (z[00] - z[99]) from array index */
static int get_zwire_from_index(int index);
/** Get array index from z-wire index (z[00] - z[99]) */
static int get_wire_from_zwire(int zwire);

/** Updates max index if needed */
static void update_max_zindex(int *max, int index);
/** Updates wire data */
static void update_wire(wire_t *wires, int index, char value, int left,
                        int right, char operation);

#endif
