#include "out.h"

/* toc */
#include <stdio.h>
void foo(int64_t (*out__)[3]) {
	int64_t x[3] = {0}; 
	*out__ = x;
	return;
}
void main__(void) {
	int64_t x[3] = foo(); 
	printf("Foo: %ld\n", (long)x);
	return;
}

int main(void) {
	main__();
	return 0;
}
