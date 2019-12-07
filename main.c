/*
  Copyright (C) 2019 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

/* 
TODO:
reduce copying (don't copy body of fn_copy unless the instance doesn't 
exist yet)

make sure you can't have a tuple parameter (check const tuple params)
check arr.toc's Arr works @ compile time
new version of copy_val for copying types??

there are probably places where we enter a function and never exit (in typing?) if there's an error
switch struct."field" to struct["field"]

packages
X ::= newtype(int); or something
don't allow while {3; 5} (once break is added)
any odd number of "s for a string
make sure futurely/currently-declared types are only used by pointer/slice
allow omission of trailing ; in foo ::= fn() {}?
 */

#ifdef __cplusplus
#define new new_
#define this this_
#endif

#include "toc.c"


int main(int argc, char **argv) {
#ifdef TOC_DEBUG
	test_all();
#endif

	const char *in_filename;
	if (argc < 2) {
#ifdef TOC_DEBUG
		in_filename = "test.toc";
#else
		fprintf(stderr, "Please specify an input file.\n");
		return EXIT_FAILURE;
#endif
	} else {
		in_filename = argv[1];
	}
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
	ErrCtx err_ctx = {0};
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

