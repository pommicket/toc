#include <stdio.h>
#include <stdlib.h>

int main(void) {
	int *p;
	memset(&p, 0, sizeof p);
	if (p) {
		fprintf(stderr, "You cannot run toc. Sorry.\n");
		return EXIT_FAILURE;
	}
	return EXIT_SUCCESS;
}
