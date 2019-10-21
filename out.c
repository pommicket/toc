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
i64 foo(i64 x);
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


i64 foo(i64 x) {

	slice_ C; {
	slice_ expr__; slice_ a0_; a0_.data = calloc(x, sizeof(i64)); a0_.n = x;expr__ = a0_;C = expr__;}
	i64 i; {
	i64 expr__; expr__ = 0;i = expr__;}
	while ((i<x)) {
		(((i64(*))(C.data))[i]) = i;;
		i = (i+1);;
	};
	i64 total; {
	i64 expr__; expr__ = 0;total = expr__;}
	i = 0;;
	while ((i<x)) {
		total = (total+(((i64(*))(C.data))[i]));;
		i = (i+1);;
	};
	return total;
}


void main__(void) {

	i64( A[45]) = {0}; 
	i64( B[4950]) = {0}; 
}


