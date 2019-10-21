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


i64 foo(void) {

	i64 N; {
	i64 expr__; expr__ = 10;N = expr__;}
	slice_ numbers; {
	slice_ expr__; slice_ a0_; a0_.data = calloc(N, sizeof(i64)); a0_.n = N;expr__ = a0_;numbers = expr__;}
	i64 i; {
	i64 expr__; expr__ = 0;i = expr__;}
	while ((i<N)) {
		(((i64(*))(numbers.data))[i]) = i;;
		i = (i+1);;
	};
	slice_ a2_; { slice_ of__ = numbers; u64 a3_ = 5; a2_.data = (i64(*))(of__.data) + a3_; a2_.n = 7 - a3_; }
	slice_ a4_; { slice_ of__ = numbers; u64 a5_ = 2; a4_.data = (i64(*))(of__.data) + a5_; a4_.n = of__.n - 1 - a5_; }
	slice_ a6_; { slice_ of__ = numbers; u64 a7_ = 0; a6_.data = (i64(*))(of__.data) + a7_; a6_.n = 6 - a7_; }
	return (((((i64(*))(a2_.data))[1])+(((i64(*))(a4_.data))[0]))+(((i64(*))(a6_.data))[3]));
}


void main__(void) {

	i64( x[11]) = {0}; 
	(puti((foo())));
}


