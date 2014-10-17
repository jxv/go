#include "go_board.h"

#include <stdio.h>
#include <memory.h>


void board_empty(uint8_t dim, uint8_t *b)
{
        uint16_t size = (((uint16_t)dim * (uint16_t)dim) >> 2) + 2;
        memset(b, 0x00, size);
        b[0] = dim;
}


uint8_t board_dim(const uint8_t* b)
{
        return b[0];
}


uint8_t board_get(const uint8_t* b, uint8_t y, uint8_t x)
{
        const uint16_t i = (uint16_t)y * (uint16_t)board_dim(b) + (uint16_t)x;
        const uint16_t ofs = (i >> 2) + 1;
        const uint16_t k = (i & 0x03) << 1;
        return 0x03 & (b[ofs] >> k);
}


void board_set(const uint8_t* b0, uint8_t y, uint8_t x, uint8_t s, uint8_t* b1)
{
        *b1 = *b0; 
        const uint16_t i = (uint16_t)y * (uint16_t)board_dim(b0) + (uint16_t)x;
        const uint16_t ofs = (i >> 2) + 1;
        const uint16_t k = (i & 0x03) << 1;
        b1[ofs] &= ~(0x03 << k); // clear previous value
        b1[ofs] |= (0x03 & s) << k; // set new value
}


void board_print(const uint8_t* b)
{
        uint8_t y = 0;
        uint8_t x = 0;
        uint8_t val = 0x00;
        char s = ' ';
        uint8_t dim = board_dim(b);
        printf("[%dx%d]\n", dim, dim);
        putchar('+');
        for (x = 0; x < dim; x++) { putchar('-'); }
        puts("+");
        for (y = 0; y < dim; y++) {
                putchar('|');
                for (x = 0; x < dim; x++) {
                        val = board_get(b, y, x);
                        s = val == BLACK ? 'b'
                          : val == WHITE ? 'w' 
                          : ' ';
                        putchar(s);
                }
                puts("|");
        }
        putchar('+');
        for (x = 0; x < dim; x++) { putchar('-'); }
        puts("+");
}

/*
int main()
{
        uint8_t b[1 + 19 * 19];
        board_empty(99, b);
        board_set(b, 2, 2, BLACK, b);
        board_set(b, 1, 2, WHITE, b);
        board_set(b, 1, 1, BLACK, b);
        board_set(b, 2, 1, WHITE, b);
        board_set(b, 3, 1, BLACK, b);
        board_set(b, 2, 3, WHITE, b);
        board_set(b, 2, 0, BLACK, b); board_set(b, 2, 1, EMPTY, b);
        board_set(b, 3, 2, WHITE, b);
        board_set(b, 4, 2, BLACK, b);
        board_set(b, 2, 1, WHITE, b); board_set(b, 2, 2, EMPTY, b);
        board_print(b);
        return 0;
}
*/
