#include "out.h"

/* toc */
static void foo(void);
static void a___(void);
void main__(void) {
	void (*bar)(void) = a___; 
	foo();
	a___();
}
static void foo(void) {
	void (*x)(void) = a___; 
}
static void a___(void) {
}

int main(void) {
	main__();
	return 0;
}
