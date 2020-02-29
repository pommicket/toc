/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

/* see toc.c for development information */

/* 
TODO:
better #foreign system- something like f := #foreign fn (int,float, #C int, #C longlong);
make err_print and tokr_err return Status
---
constants in structs
#if
#returns_code (function/struct body is a block, to be evaluated at compile time, which returns the actual statements -- you can use this for implementation of printf)
variadic fns
switch to / add as an alternative: libffi

---
X ::= newtype(int); or something
don't allow while {3; 5} or for 0..10 { 3; 5 } (once break is added)
do we need was_expr? (now that, presumably, we have struct arguments)
any odd number of "s for a string
make sure futurely/currently-declared types are only used by pointer/slice
allow omission of trailing ; in foo ::= fn() {...} or foo ::= nms {...} or foo ::= struct { ... }?
consider- should #sizeof always take a Type? it would be more verbose, but we might not actually need
	#sizeof that much, given that we have new.
*/


#include "toc.c"


#if defined TOC_DEBUG && defined __linux__ && defined __GNU_LIBRARY__
#include <signal.h>
#include <execinfo.h>
#include <unistd.h>
static void signal_handler(int num) {
    switch (num) {
	case SIGABRT:
		fprintf(stderr, "Aborted.\n");
		break;
	case SIGSEGV:
		fprintf(stderr, "Segmentation fault.\n");
		break;
	default:
	    fprintf(stderr, "Terminated for unknown reason.\n");
		break;
	}
	fprintf(stderr, "Stack trace:\n");

	static void *addrs[30];
    int naddrs = (int)(sizeof addrs / sizeof *addrs);
    naddrs = backtrace(addrs, naddrs);
	/* char **syms = backtrace_symbols(addrs, naddrs); */
	char command[2048] = "addr2line -p -f -a -e toc ";
	for (int i = 4; i < naddrs; ++i) {
		snprintf(command + strlen(command), sizeof command - strlen(command), "%p ", addrs[i]);
	}
	system(command);
	/* free(syms); */
	
}
#endif

int main(int argc, char **argv) {
#ifdef TOC_DEBUG
	signal(SIGABRT, signal_handler);
	signal(SIGSEGV, signal_handler);
#endif
#ifdef RUN_TESTS	
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
		allocr_free_all(&main_allocr);
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
		allocr_free_all(&main_allocr);
		return EXIT_FAILURE;
	}
	/* fprint_parsed_file(stdout, &f); */
	/* printf("\n\n-----\n\n"); */
	
	
	Typer tr;
	Evaluator ev;
	evalr_create(&ev, &tr, &main_allocr);
	typer_create(&tr, &ev, &err_ctx, &main_allocr, &globals);
    
	if (!types_file(&tr, &f)) {
		err_text_important(&err_ctx, "Errors occured while determining types.\n");
		allocr_free_all(&main_allocr);
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
		allocr_free_all(&main_allocr);
		return EXIT_FAILURE;
	}
	CGenerator g;
	cgen_create(&g, out, &globals, &main_allocr);
	cgen_file(&g, &f);
	
	allocr_free_all(&main_allocr);
	fclose(out);
	return 0;
}

