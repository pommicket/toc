/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

/* 
   NOTE:
   Structure of the toc compiler:
   tokenizer => parser => typing (types.c) => cgen 
   (lexing)

   toc tries to continue even after the first error. 
   It will not continue during cgen, but it will during tokenization,
   parsing, and typing. If one stage fails, the following ones do not
   start.
*/

/* 
TODO:
#builtin("sizeof int")
#builtin("target sizeof int") 
etc.

allow any global declaration to be used before itself

#include
constants in structs
#if

variadic fns

---
X ::= newtype(int); or something
don't allow while {3; 5} (once break is added)
better printing of types (take was_expr into account)
any odd number of "s for a string
make sure futurely/currently-declared types are only used by pointer/slice
allow omission of trailing ; in foo ::= fn() {}?
*/


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
	for (int i = 2; i < argc-1; ++i) {
		if (strs_equal(argv[i], "-o"))
			out_filename = argv[i+1];
	}
	
	FILE *in = fopen(in_filename, "r");
	if (!in) {
		fprintf(stderr, "Could not open file: %s.\n", in_filename);
		return EXIT_FAILURE;
	}
	
	char *contents = err_malloc(4096);
	contents[0] = 0; /* put 0 byte at the start of the file. see err.c:err_print_location_text to find out why */
	contents[1] = 0; /* if fgets fails the first time */
	long contents_cap = 4095;
	long contents_len = 1;
	while (fgets(contents + contents_len, (int)(contents_cap - contents_len), in)) {
		contents_len += (long)strlen(contents + contents_len);
		
		if (contents_len >= (long)contents_cap - 1024) {
			contents_cap *= 2;
			contents = err_realloc(contents, (size_t)contents_cap + 1);
		}
	}
	++contents;
	if (ferror(in)) {
		fprintf(stderr, "Error reading input file: %s.\n", in_filename);
		return EXIT_FAILURE;
	}
	fclose(in);
	Identifiers idents;
	idents_create(&idents);
	Tokenizer t;
	Allocator main_allocr;
	allocr_create(&main_allocr);
	ErrCtx err_ctx = {0};
	err_ctx.enabled = true;
	err_ctx.color_enabled = true;
	File file = {0};
	file.filename = in_filename;
	file.contents = contents;
	file.ctx = &err_ctx;
	tokr_create(&t, &idents, &err_ctx, &main_allocr);
	if (!tokenize_file(&t, &file)) {
		err_text_important(&err_ctx, "Errors occured during preprocessing.\n");
		return EXIT_FAILURE;
	}

#if 0
	arr_foreach(t.tokens, Token, token) {
		if (token != t.tokens)
			printf("    ");
		fprint_token(stdout, token);
	}
	printf("\n");
#endif
	
	Parser p;
	parser_create(&p, &t, &main_allocr);
	ParsedFile f;
	if (!parse_file(&p, &f)) {
		err_text_important(&err_ctx, "Errors occured during parsing.\n");
		return EXIT_FAILURE;
	}
	/* fprint_parsed_file(stdout, &f); */
	/* printf("\n\n-----\n\n"); */
	
	
	Typer tr;
	Evaluator ev;
	Exporter exptr;
	evalr_create(&ev, &tr, &main_allocr);
	typer_create(&tr, &ev, &err_ctx, &main_allocr, &idents);
	tr.exptr = &exptr;
	
	if (!block_enter(NULL, f.stmts, SCOPE_CHECK_REDECL)) /* enter global scope */
		return EXIT_FAILURE;

	if (!types_file(&tr, &f)) {
		/* TODO(eventually): fix this if the error occured while exporting something */
		err_text_important(&err_ctx, "Errors occured while determining types.\n");
		return EXIT_FAILURE;
	}
#ifdef TOC_DEBUG
	/* printf("\n\n");	 */
	/* fprint_parsed_file(stdout, &f); */
#endif
	FILE *out = fopen(out_filename, "w");
	if (!out) {
		err_text_important(&err_ctx, "Could not open output file: ");
		err_fprint(&err_ctx, "%s\n", out_filename);
		return EXIT_FAILURE;
	}
	CGenerator g;
	cgen_create(&g, out, &idents, &ev, &main_allocr);
	if (!cgen_file(&g, &f)) {
		fclose(out);
		err_text_important(&err_ctx, "Errors occured while generating C code.\n");
		return EXIT_FAILURE;
	}
	
	block_exit(NULL, f.stmts); /* exit global scope */
	
	free(contents - 1); /* -1 because we put a 0 byte at the beginning */
	allocr_free_all(&main_allocr);
	evalr_free(&ev);
	typer_free(&tr);
	fclose(out);
	idents_free(&idents);
	return 0;
}

