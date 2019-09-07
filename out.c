#include "out.h"

/* toc */
int64_t N = 10; 
#include <stdio.h>
void foo(int64_t (*out__)[10][10]) {
	int64_t av___0[10][10] = {{0}};
	int64_t (*x)[10] = av___0;
	memcpy(*out__, x, 100 * sizeof(int64_t )); return;
}
void main__(void) {
	int64_t av___1[10][10];
	foo(&av___1);
	int64_t (*x)[10] = av___1;
	
for (int i = 0; i < 10; i++)
	for (int j = 0; j < 10; j++)
		printf("%ld", x[i][j]);
puts("");
	;
	return;
}

int main(void) {
	main__();
	return 0;
}
