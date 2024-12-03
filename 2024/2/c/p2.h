#ifndef P2_H
#define P2_H

#define CORRECT_ANSWER 674
#define TERMINATOR -1 // designates end of integers row
#define CAPACITY 20   // pre-allocated integers slice length

/** Parses integers in line and put them with nums, terminating with TERMINATOR
 */
void parse_line(const char *, int *);
/** Allocates slice of integers */
int *alloc_nums();
/** Check whether integers row is safe. Returns 0 or 1 */
int is_safe(int *);
/** Check whether integers row is safe. Returns 0 or 1
 * Unlike [is_safe] allows one anomaly integer, which excluded, makes row safe
 */
int is_safe_with_dampener(int *nums);

#endif
