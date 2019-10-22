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
static slice_ mkslice_(void *data, u64 n) { slice_ ret; ret.data = data; ret.n = n; return ret; }
#define false ((bool)0)
#define true ((bool)1)


/* declarations */
void puti(i64 x);
void putf(f32 x);
i64 foo(void);
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


void putf(f32 x) {

	printf("%f\n", (double)x);
}


i64 foo(void) {

	i64 N; {
	i64 expr__; expr__ = 10;N = expr__;}
	slice_ numbers; {
	slice_ expr__; expr__ = mkslice_(calloc(N, sizeof(i64)), N);numbers = expr__;}
	i64 i; {
	i64 expr__; expr__ = 0;i = expr__;}
	while ((i<N)) {
		(((i64(*))(numbers.data))[i]) = i;;
		i = (i+1);;
	};
	slice_ a1_; { slice_ of__ = numbers; u64 a2_ = 5; a1_.data = (i64(*))(of__.data) + a2_; a1_.n = 7 - a2_; }
	slice_ a3_; { slice_ of__ = numbers; u64 a4_ = 2; a3_.data = (i64(*))(of__.data) + a4_; a3_.n = of__.n - 1 - a4_; }
	slice_ a5_; { slice_ of__ = numbers; u64 a6_ = 0; a5_.data = (i64(*))(of__.data) + a6_; a5_.n = 6 - a6_; }
	return (((((i64(*))(a1_.data))[1])+(((i64(*))(a3_.data))[0]))+(((i64(*))(a5_.data))[3]));
}


void main__(void) {

	i64 N = 5;
	(puti(N));
	f32 M = 1.4320000410079956;
	(putf(M));
	i64( x[11]) = {0}; 
	(puti((foo())));
}


