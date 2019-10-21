#include <stdint.h>
#include <stdlib.h>
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef float f32;
typedef double f64;
typedef unsigned char bool;
typedef struct { void *data; u64 n; } slice_;
#define false ((bool)0)
#define true ((bool)1)


/* declarations */
void puti(i64 x);
i64 factorial(i64 x);
void main__(void);
/* code */
int main() {
	main__();
	return 0;
}

#include <stdio.h>
#define kasfdhkjasdfhjk ;
void puti(i64 x) {

	printf("%ld\n", (long)x);
}


i64 factorial(i64 x) {

	slice_ numbers; {
	slice_ expr__; slice_ a0_; a0_.data = calloc(x, sizeof(i64)); a0_.n = x;expr__ = a0_;numbers = expr__;}
	i64 i; {
	i64 expr__; expr__ = 0;i = expr__;}
	while ((i<x)) {
		(((i64(*))(numbers.data))[i]) = (i+1);;
		i = (i+1);;
	};
	i64 product; {
	i64 expr__; expr__ = 1;product = expr__;}
	i = 0;;
	while ((i<x)) {
		product = (product*(((i64(*))(numbers.data))[i]));;
		i = (i+1);;
	};
	free(numbers.data);
	return product;
}


void main__(void) {

	i64( a342[120]) = {0}; 
	(puti((factorial(10))));
}


