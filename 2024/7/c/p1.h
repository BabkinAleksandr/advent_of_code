#ifndef P1_H
#define P1_H

#define CORRECT_ANSWER 2437272016585

#define TERMINATOR 0
#define CAPACITY 20

/** Parses equations */
void parse_eq(const char *s, unsigned long *calibration_v, int *nums);
/** Searches calibration value */
unsigned long search_sum(int *nums, int i, unsigned long cv, unsigned long res);

#endif
