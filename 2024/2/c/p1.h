#ifndef P1_H
#define P1_H

#define CORRECT_ANSWER 639
#define TERMINATOR -1 // designates end of integers row
#define CAPACITY 20   // pre-allocated integers slice length

/** Parses integers in line and put them with nums, terminating with TERMINATOR
 */
void parse_line(const char *, int *);
/** Allocates slice of integers */
int *alloc_nums();
/** Check whether integers row is safe. Returns 0 or 1 */
int is_safe(int *);

#endif
