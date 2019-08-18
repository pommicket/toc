#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdbool.h>
#include "util/err.c"
#include "util/arr.c"
#include "identifiers.c"
#include "tokenizer.c"
#include "parse.c"

int main(int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr, "Please specify an input file.\n");
		return EXIT_FAILURE;
	}

	const char *in_filename = argv[1];
	FILE *in = fopen(in_filename, "r");
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

	err_filename = in_filename;
	Tokenizer t;
	if (!tokenize_string(&t, contents)) {
		err_fprint(TEXT_IMPORTANT("Errors occured while preprocessing.\n"));
		return EXIT_FAILURE;
	}
	
	arr_foreach(t.tokens, Token, token) {
		if (token != t.tokens.data)
			printf("    ");
		token_fprint(stdout, token);
	}
	printf("\n");
	
   	ParsedFile f;
	if (!parse_file(&f, &t)) {
		err_fprint(TEXT_IMPORTANT("Errors occured while parsing.\n"));
		return EXIT_FAILURE;
	}

	parsed_file_fprint(stdout, &f);
	
	tokr_free(&t);
	free(contents);
	
	fclose(in);
	idents_free();
}
