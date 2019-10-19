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
void asdf(i64( (*x)[3]), i64 y, i64(*ret0_), i64((*ret1_)[3]));
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


void asdf(i64( (*x)[3]), i64 y, i64(*ret0_), i64((*ret1_)[3])) {
{
}(*ret0_) = y;{
size_t i;i64(*arr__in) = (*x); i64(*arr__out) = (*ret1_);
for (i = 0; i < 3; i++) arr__out[i] = arr__in[i];
}return;
}


void main__(void) {
{
	i64( a[3]) = {0}; 
	i64 b = 0; 
	(a[0]) = 17;;
	b = 5489;;
	i64 c; i64( d[3]); asdf(&a, b, &c, &d);

	(puti(c));
	(puti((d[0])));
	void (* asdfasdf)(i64((*)[3]), i64, i64(*), i64((*)[3])); {
	void (* expr__)(i64((*)[3]), i64, i64(*), i64((*)[3])); expr__ = asdf;asdfasdf = expr__;}
}}


