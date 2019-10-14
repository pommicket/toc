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
i64 puti(i64 i);
void dbl(i64((* x)[3]));
void main__();
/* code */
int main() {
	main__();
	return 0;
}

i64 puti(i64 i) {
printf("%ld\n", i);
}

void dbl(i64((* x)[3])) {
((*x)[0]) = (((*x)[0])*2);;
((*x)[1]) = (((*x)[1])*2);;
((*x)[2]) = (((*x)[2])*2);;
}

void main__() {
i64( a[3]) = {0}; 
(a[0]) = 1;;
(dbl((&a)));
(puti((a[0])));
}

