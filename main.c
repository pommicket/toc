#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
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
	
	char *contents = err_malloc(4096); /* TODO:check files with >this */
	size_t contents_cap = 4096;
	size_t contents_len = 0;
	while (fgets(contents + contents_len, (int)(contents_cap - contents_len), in)) {
		contents_len += strlen(contents + contents_len);
		if (contents_len >= contents_cap - 1024) {
			contents_cap *= 2;
			contents = err_realloc(contents, contents_cap);
		}
	}
	/* TODO: check ferror */
	
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
