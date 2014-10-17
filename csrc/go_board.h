#ifndef GO_BOARD_H
#define GO_BOARD_H

#include <stdint.h>


const uint8_t MAX_DIM = 19;
const uint8_t BOARD_SIZE = 91;
const uint8_t EMPTY = 0x00;
const uint8_t BLACK = 0x01;
const uint8_t WHITE = 0x02;


void board_empty(uint8_t dim, uint8_t *b);
uint8_t board_dim(const uint8_t* b);
uint8_t board_get(const uint8_t* b, uint8_t y, uint8_t x);
void board_set(const uint8_t* b0, uint8_t y, uint8_t x, uint8_t s, uint8_t* b1);
void board_print(const uint8_t* b);

#endif
