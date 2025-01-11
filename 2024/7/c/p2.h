#ifndef P2_H
#define P2_H

#define CORRECT_ANSWER 162987117690649

#define TERMINATOR 0
#define CAPACITY 20

/** Parses equations */
void parse_eq(const char *s, unsigned long *calibration_v, int *nums);
/** Searches calibration value */
unsigned long search_sum(int *nums, int i, unsigned long cv, unsigned long res);

/** Concatenates num and n (num on the left, n on the right) */
static unsigned long concatl(unsigned long num, int n);

#endif
