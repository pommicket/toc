/* 
TODO:
re-do cgen
arrs are pointers
make sure initializers for global variables are compile-time constants
allow, e.g.: x := "foo"; x[0] = 'g';
any odd number of "s for a string
 */
#include "toc.c"

int main(int argc, char **argv) {
#ifdef TOC_DEBUG
	test_all();
#endif
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
	long contents_cap = 4095;
	long contents_len = 0;
	while (fgets(contents + contents_len, (int)(contents_cap - contents_len), in)) {
		contents_len += (long)strlen(contents + contents_len);
		
		if (contents_len >= (long)contents_cap - 1024) {
			contents_cap *= 2;
			/* allocate +1 so that pointers don't overflow */
			contents = err_realloc(contents, (size_t)contents_cap + 1);
		}
	}
	if (ferror(in)) {
		fprintf(stderr, "Error reading input file: %s.\n", argv[1]);
		return EXIT_FAILURE;
	}
	fclose(in);
	Identifiers file_idents;
	idents_create(&file_idents);
	Tokenizer t;
	tokr_create(&t, &file_idents, in_filename);
	if (!tokenize_string(&t, contents)) {
		err_fprint(TEXT_IMPORTANT("Errors occured while preprocessing.\n"));
		return EXIT_FAILURE;
	}
	
	arr_foreach(t.tokens, Token, token) {
		if (token != t.tokens)
			printf("    ");
		fprint_token(stdout, token);
	}
	printf("\n");
	Parser p;
	parser_from_tokenizer(&p, &t);
   	ParsedFile f;
	if (!parse_file(&p, &f)) {
		err_fprint(TEXT_IMPORTANT("Errors occured while parsing.\n"));
		return EXIT_FAILURE;
	}
	tokr_free_tokens(&t);
	fprint_parsed_file(stdout, &f);
    
	printf("\n\n-----\n\n");
	
	Typer tr;
	Evaluator ev;
	evalr_create(&ev);
	typer_create(&tr, &ev);
	if (!types_file(&tr, &f)) {
		err_fprint(TEXT_IMPORTANT("Errors occured while determining types.\n"));
		return EXIT_FAILURE;
	}
	parse_printing_after_types = true;
	fprint_parsed_file(stdout, &f);

	FILE *out = fopen("out.c", "w");
	if (!out) {
		err_fprint(TEXT_IMPORTANT("Could not open output file (out.c).\n"));
		return EXIT_FAILURE;
	}
	CGenerator g;
	cgen_create(&g, out, &file_idents, &ev);
	cgen_file(&g, &f);
	
	tokr_free(&t);
    
	free(contents);

	parser_free(&p);
	typer_free(&tr);
	evalr_free(&ev);
	fclose(out);
	/* fclose(h_out); */
	idents_free(&file_idents);
}
