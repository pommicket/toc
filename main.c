#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include "util/err.c"
#include "util/files.c"
#include "tokenizer.c"

int main(int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr, "Please specify an input file.\n");
		return EXIT_FAILURE;
	}
	
	FILE *in = fopen(argv[1], "r");
	if (!in) {
		fprintf(stderr, "Could not open file: %s.\n", argv[1]);
		return EXIT_FAILURE;
	}

	Tokenizer t = tokenize_file(in);
	
	for (size_t i = 0; i < t.ntokens; i++) {
		if (i)
			printf("    ");
		token_fprint(stdout, &t.tokens[i]);
	}
	printf("\n");

	tokenizer_free(&t);
	
	fclose(in);
}
