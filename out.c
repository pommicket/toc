#include "out.h"

/* toc */
#include <stdio.h>
void foo(int64_t (*out__)[3][3]) {
	int64_t av___0[3][3] = {{0}};
	int64_t (*x)[3] = av___0;
	memcpy(*out__, x, 9 * sizeof(int64_t )); return;
}
void main__(void) {
	int64_t av___1[3][3];
	foo(&av___1);
	int64_t (*x)[3] = av___1;
	
for (int i = 0; i < 3; i++)
	for (int j = 0; j < 3; j++)
		printf("%ld", x[i][j]);
puts("");
	;
	return;
}

int main(void) {
	main__();
	return 0;
}
