#include "out.h"

/* toc */
static void a___(void);
static void a___1(void);
void main__(void) {
	void (*foo)(void)  = a___; 
	void (*bar)(void)  = a___1; 
}
static void a___(void) {
	a___1();
}
static void a___1(void) {
	a___();
}

int main(void) {
	main__();
	return 0;
}
