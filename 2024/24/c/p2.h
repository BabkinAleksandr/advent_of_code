#ifndef P2_H
#define P2_H

#define CORRECT_ANSWER "hnv,hth,kfm,tqr,vmv,z07,z20,z28"

#define NO_VALUE -1
#define ALPHABET_LENGTH 36
#define WW_LENGTH 8
#define RESULT_LENGTH 32 // 8 3-symbols wires separated by commas

typedef enum { AND, OR, XOR } operation_t;

typedef struct {
  int idx;
  int left;
  int right;
  operation_t operation;
} wire_t;

// Wires ranges indexes
#define X00 42768
#define XZZ 44063
#define Y00 44064
#define YZZ 45359
#define Z00 45360

#define AND_STR "AND"
#define OR_STR "OR"
#define XOR_STR "XOR"

/** Parses gates */
void parse_gates(const char *s, wire_t *wires, int *gates_len, int *max_zindex);

/** Validate gates and puts wrong wires in to `wrong_wires` slice.
 * Returns number of wrong wires added */
int validate_adder(wire_t *wires, int len, int *wrong_wires, int max_zindex);
/** Gets wrong wires slice of length,
 * converts wires' indexes back in to 3-symbols representation
 * and put it to result string separating with comma */
void collect_wrong_wires(int *wrong_wires, int len, char *result, int r_len);

// Some misc functions

/** Converts character (0-9,a-z) to unique decimal */
static int char_to_int(char c);
/** Converts three-letter wire name to unique array index */
static int get_wire_index(char c1, char c2, char c3);
/** Updates max index if needed */
static void update_max_zindex(int *max, int index);
/** Adds element in to collection, skipping duplicates */
static void add_dedup(int *wires, int *len, int wire);
/** Converts wire index back in to 3-symbols representation */
static void reverse_wire_index(int index, char *result);
/** Check if it's an X wire by index */
static int is_x(int index);
/** Check if it's an Y wire by index */
static int is_y(int index);
/** Check if it's an Z wire by index */
static int is_z(int index);
/** Check if it's an end wire (X, Y or Z) by index */
static int is_end_wire(int index);
/** Compare function for sorting */
static int cmp_ints_asc(const void *a, const void *b);

#endif
