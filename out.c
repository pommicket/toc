#include "out.h"

/* toc */
void main__(void) {
	void (*bar[3])(void)  = {NULL}; 
	int64_t i = 0; 
	void (*x)(void)  = (bar[i]); 
	x();
	(i=(i+1));
	(x=(bar[i]));
	x();
	(i=(i+1));
	(x=(bar[i]));
	x();
	(i=(i+1));
	(x=(bar[i]));
	x();
}

int main(void) {
	main__();
	return 0;
}
