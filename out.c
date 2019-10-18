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
#define false ((bool)0)
#define true ((bool)1)


/* declarations */
void puti(i64 x);
void main__(void);
/* code */
int main() {
	main__();
	return 0;
}

#include <stdio.h>
;
void puti(i64 x) {
	{
	printf("%ld\n", (long)x);
}}


void main__(void) {
	{
	i64 a0_0_; i64 a0_1_; if (0) {
		(a0_0_) = 3;(a0_1_) = 5;
	} else {
		(a0_0_) = 4;(a0_1_) = 6;
	}i64 A; i64 B; (A) = a0_0_; (B) = a0_1_; 
	(puti(A));
	(puti(B));
