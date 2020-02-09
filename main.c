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
change the way arrays work (a := b, where b is an array probably shouldn't copy)
struct parameters - to allow circular dependencies in types
foo, _ := bar();
nice syntax for #including something into a namespace
run stuff at compile time without assigning it to a constant
#compile_only declarations
constants in structs
#if

variadic fns
switch to / add as an alternative: libffi

---
X ::= newtype(int); or something
don't allow while {3; 5} (once break is added)
do we need was_expr? (now that, presumably, we have struct arguments)
any odd number of "s for a string
make sure futurely/currently-declared types are only used by pointer/slice
allow omission of trailing ; in foo ::= fn() {...} or foo ::= nms {...} ?
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
	Allocator main_allocr;
	allocr_create(&main_allocr);
	ErrCtx err_ctx = {0};
	err_ctx.enabled = true;
	err_ctx.color_enabled = true;

	File file = {0};
	file.filename = in_filename;
	Location file_where = {0};
	file_where.file = &file;
	file.ctx = &err_ctx;
	char *contents = read_file_contents(&main_allocr, in_filename, file_where);
	if (!contents) return EXIT_FAILURE;

	Identifiers globals;
	idents_create(&globals, &main_allocr, NULL);
	Tokenizer t;
	file.contents = contents;
	tokr_create(&t, &err_ctx, &main_allocr);
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
	parser_create(&p, &globals, &t, &main_allocr);
	ParsedFile f;
	if (!parse_file(&p, &f)) {
		err_text_important(&err_ctx, "Errors occured during parsing.\n");
		return EXIT_FAILURE;
	}
	/* fprint_parsed_file(stdout, &f); */
	/* printf("\n\n-----\n\n"); */
	
	
	Typer tr;
	Evaluator ev;
	evalr_create(&ev, &tr, &main_allocr);
	typer_create(&tr, &ev, &err_ctx, &main_allocr, &globals);
    
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
	cgen_create(&g, out, &globals, &ev, &main_allocr);
	if (!cgen_file(&g, &f)) {
		fclose(out);
		err_text_important(&err_ctx, "Errors occured while generating C code.\n");
		return EXIT_FAILURE;
	}
	
	allocr_free_all(&main_allocr);
	fclose(out);
	return 0;
}

