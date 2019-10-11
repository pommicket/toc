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
void puti(i64 i);
void asdf(i64 i, i64( (*ret__)[3]));
void main__();
/* code */
int main() {
	main__();
	return 0;
}

void puti(i64 i) {
printf("%ld\n", i);
}

void asdf(i64 i, i64( (*ret__)[3])) {
i64( ret[3]) = {0}; 
((ret[0])=(0*i));
((ret[1])=(1*i));
((ret[2])=(2*i));
return;
}

void main__() {
i64(* x); x = ((i64(*))calloc(1, sizeof(i64))); 
((*x)=17);
if (((*x)==0)) {
((*x)=((1+2)+(3-(5/62))));
} else {
((*x)=((4+5)+6));
};
(puti((*x)));
(free(x));
void (* fptr)(i64, i64((*)[3])); fptr = asdf; 
}

i64 foo = 5; 
char( bar[5]) = "\x48\x65\x6c\x6c\x6f"; 
i64 a = 123; i64 b = 123; 
char x = ((char)97); 
i64 sadkfj = (-1293812); 
