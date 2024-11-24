#ifndef P1_H
#define P1_H

#define MAX_ITERATIONS_COUNT 100000
#define CORRECT_ANSWER 72422

typedef struct Collection {
  int red;
  int green;
  int blue;
} collection_t;

typedef struct Game {
  int id;
  collection_t *max_cubes;
} game_t;

/** Allocates memory for a game */
game_t *init_game();
/** Frees memory for a game */
void destroy_game(game_t **);
/** Resets values in game. Does not do any allocations */
void reset_game(game_t *);
void parse_game(char *s, game_t *g);
int get_game_power(game_t *);

#endif
