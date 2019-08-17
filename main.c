#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include "util/colored_text.c"
#include "util/err.c"
#include "identifiers.c"
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
	
	char *contents = err_malloc(4096);
	long contents_cap = 4096;
	long contents_len = 0;
	while (fgets(contents + contents_len, (int)(contents_cap - contents_len), in)) {
		contents_len += (long)strlen(contents + contents_len);
		
		if (contents_len >= (long)contents_cap - 1024) {
			contents_cap *= 2;
			contents = err_realloc(contents, (size_t)contents_cap);
		}
	}
	if (ferror(in)) {
		fprintf(stderr, "Error reading input file: %s.\n", argv[1]);
		return EXIT_FAILURE;
	}
	
	Tokenizer t = tokenize_string(contents);
	
	for (size_t i = 0; i < t.ntokens; i++) {
		if (i)
			printf("    ");
		token_fprint(stdout, &t.tokens[i]);
	}
	printf("\n");

	free(contents);
	tokenizer_free(&t);
	
	fclose(in);
	idents_free();
}
