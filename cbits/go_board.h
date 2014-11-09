#ifndef GO_BOARD_H
#define GO_BOARD_H

#include <stdint.h>


#define EMPTY   0x00
#define BLACK   0x01
#define WHITE   0x02


int sizeof_board_by_dim(uint8_t dim);
void board_empty(uint8_t dim, uint8_t *b);
uint8_t board_dim(const uint8_t* b);
uint8_t board_get(const uint8_t* b, uint8_t y, uint8_t x);
void board_set(const uint8_t* b0, uint8_t y, uint8_t x, uint8_t s, uint8_t* b1);
void board_print(const uint8_t* b);

#endif
