/* 
TODO:
memory leaks!
deal with unused functions
compile time arguments + out parameters (in C)
double check that val_get_ptr is being used everywhere it should be
compile-time arguments for out parameter functions
compile-time arguments for functions returning tuples
deal with x, y @= fn(x: int, y @ int){}
don't allow pointers to functions with compile-time arguments
struct parameters
don't allow while {3; 5} (once break is added)
any odd number of "s for a string
modifiable string literals
make sure futurely/currently-declared types are only used by pointer/slice
allow omission of trailing ; in foo @= fn() {}?
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
	const char *out_filename = "out.c";

	for (int i = 2; i < argc-1; i++) {
		if (strcmp(argv[i], "-o") == 0)
			out_filename = argv[i+1];
	}
	
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
	Allocator main_allocr;
	allocr_create(&main_allocr);
	ErrCtx err_ctx;
	err_ctx.filename = in_filename;
	err_ctx.enabled = true;
	tokr_create(&t, &file_idents, &err_ctx, &main_allocr);
	if (!tokenize_string(&t, contents)) {
		
		err_fprint(TEXT_IMPORTANT("Errors occured while preprocessing.\n"));
		return EXIT_FAILURE;
	}
	
	/* arr_foreach(t.tokens, Token, token) { */
	/* 	if (token != t.tokens) */
	/* 		printf("    "); */
	/* 	fprint_token(stdout, token); */
	/* } */
	/* printf("\n"); */
	Parser p;
	parser_create(&p, &t, &main_allocr);
   	ParsedFile f;
	if (!parse_file(&p, &f)) {
		
		err_fprint(TEXT_IMPORTANT("Errors occured while parsing.\n"));
		return EXIT_FAILURE;
	}
	/* fprint_parsed_file(stdout, &f); */
    
	/* printf("\n\n-----\n\n"); */
	
	tokr_free(&t);
	
	Typer tr;
	Evaluator ev;
	evalr_create(&ev, &tr, &main_allocr);
	typer_create(&tr, &ev, &main_allocr);


	if (!block_enter(NULL, f.stmts, SCOPE_CHECK_REDECL)) /* enter global scope */
		return false;

	if (!types_file(&tr, &f)) {
	    err_fprint(TEXT_IMPORTANT("Errors occured while determining types.\n"));
		return EXIT_FAILURE;
	}
	parse_printing_after_types = true;
	fprint_parsed_file(stdout, &f);

	FILE *out = fopen(out_filename, "w");
	if (!out) {
		err_fprint(TEXT_IMPORTANT("Could not open output file (out.c).\n"));
		return EXIT_FAILURE;
	}
	CGenerator g;
	cgen_create(&g, out, &file_idents, &ev, &main_allocr);
	if (!cgen_file(&g, &f)) {
		fclose(out);
		err_fprint(TEXT_IMPORTANT("Errors occured while generating C code.\n"));
		return EXIT_FAILURE;
	}
	
	block_exit(NULL, f.stmts); /* exit global scope */
	
	free(contents);
	allocr_free_all(&main_allocr);
	evalr_free(&ev);
	
	fclose(out);
	/* fclose(h_out); */
	idents_free(&file_idents);
}

