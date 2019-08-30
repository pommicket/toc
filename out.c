#include "out.h"

/* toc */
static void a___(void);
void main__(void) {
	void (*bar[3])(void)  = {NULL}; 
	void (*foo)(void)  = a___; 
	((bar[1])=foo);
	((bar[2])=foo);
	((bar[0])=foo);
	int64_t i = 0; 
	void (*x)(void)  = (bar[i]); 
	x();
	(i=(i+1));
	(x=(bar[i]));
	x();
	(i=(i+1));
	(x=(bar[i]));
	x();
}
static void a___(void) {
}

int main(void) {
	main__();
	return 0;
}
