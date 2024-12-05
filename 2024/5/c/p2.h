#ifndef P2_H
#define P2_H

#define CORRECT_ANSWER 5900
#define CAPACITY 100

/** ALlocating and freeing memory */
int **alloc_rules();
void destroy_rules(int **);

/** Parsing first part of input: rules */
void parse_rules(const char *s, int **rules);
/** Parsing second part of input, updates, and validating them against
 * previously parsed rules */
int check_rules(const char *s, int **rules);

#endif