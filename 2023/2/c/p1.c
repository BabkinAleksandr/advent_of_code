#include "p1.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

const collection_t CUBES_AVAILABLE = {.red = 12, .green = 13, .blue = 14};

int main(int argc, char **argv) {
  int sum = 0;
  char buffer[1000];
  game_t *g = init_game();
  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    reset_game(g);
    parse_game(buffer, g);
    if (is_game_possible(&CUBES_AVAILABLE, g->max_cubes)) {
      sum += g->id;
    }
  }

  destroy_game(&g);

  assert(sum == CORRECT_ANSWER);
  printf("\n/2023/d2/P1 result: %d\n", sum);
  return 0;
}

game_t *init_game() {
  collection_t *c = malloc(sizeof(collection_t));
  game_t *g = malloc(sizeof(game_t));

  g->id = -1;
  g->max_cubes = c;
  g->max_cubes->red = 0;
  g->max_cubes->blue = 0;
  g->max_cubes->green = 0;
  return g;
}

void destroy_game(game_t **g) {
  free((*g)->max_cubes);
  free(*g);
  *g = NULL;
}

void reset_game(game_t *g) {
  g->id = -1;
  g->max_cubes->red = 0;
  g->max_cubes->blue = 0;
  g->max_cubes->green = 0;
}

/* Returns a game with max count of cubes shown simultaneously */
void parse_game(char *s, game_t *g) {
  int i;

  // parse game id
  char buffer[4];
  for (i = 5; s[i] != '\0' && s[i] != ':'; i++) {
    buffer[i - 5] = s[i];
  }
  buffer[i - 5] = '\0';
  g->id = atoi(buffer);

  int tmp = 0;
  for (; s[i] != '\0'; i++) {
    // parsing integer
    if (isdigit(s[i])) {
      tmp = tmp * 10 + (s[i] - '0');
      continue;
    }

    if (s[i - 1] == ' ' && isdigit(s[i - 2])) {
      switch (s[i]) {
      case 'g': {
        if (tmp > g->max_cubes->green) {
          g->max_cubes->green = tmp;
        }
        break;
      }
      case 'b': {
        if (tmp > g->max_cubes->blue) {
          g->max_cubes->blue = tmp;
        }
        break;
      }
      case 'r': {
        if (tmp > g->max_cubes->red) {
          g->max_cubes->red = tmp;
        }
        break;
      }
      }
      tmp = 0;
    }
  }
}

int is_game_possible(collection_t const *available, collection_t *game) {
  return (game->red <= available->red) && (game->green <= available->green) &&
         (game->blue <= available->blue);
}
