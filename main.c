/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

/* see development.md for development information */

/* 
TODO:
figure out why we're not getting redecl errors
use
 - note: just keep an array of useds on the block
 - use with a decl, e.g. use p : Point;
 - make sure use works with functions and for, e.g. for use p := points
local structs should not be named in C
for some reason forgetting a ; after #include causes a misleading unrecognized expression
simplify eval macros with val_to_u/i64
&&, ||
start making a standard library... (printf; stringbuilder would be nice to have)
switch
 - #fallthrough
enums
unions
---
switch to / add as an alternative: libffi
X ::= newtype(int); or something
any odd number of "s for a string
---
make sure that floating point literals are exact as possible
	have some way of doing Infinity and s/qNaN (you can
	have them be in std/math.toc)
once you have a bunch of test code, try making more Expression members pointers
error on x ::= {return; 3}
#returns_code (struct body is a block, to be evaluated at compile time, which returns the actual statements)
	- struct varargs
macros
*/

#if defined __unix__ || (defined __APPLE__ && defined __MACH__)
#define _POSIX_C_SOURCE 200112L
#include <unistd.h>
#define UNISTD_AVAILABLE 1
#endif

#include "toc.c"

#if defined TOC_DEBUG && defined __GNU_LIBRARY__ && defined UNISTD_AVAILABLE
#define BACKTRACE
#endif
#ifdef BACKTRACE
#include <signal.h>
#include <execinfo.h>

static char *program_name;

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
	char command[2048] = "addr2line -p -f -a -e ";
	strcat(command, program_name);
	strcat(command, " ");

	for (int i = 4; i < naddrs; ++i) {
		snprintf(command + strlen(command), sizeof command - strlen(command), "%p ", addrs[i]);
	}
	system(command);
	/* free(syms); */
	
}
#endif
int main(int argc, char **argv) {
#ifdef BACKTRACE
	program_name = argv[0];
	signal(SIGABRT, signal_handler);
	signal(SIGSEGV, signal_handler);
#endif
#ifdef RUN_TESTS	
	printf("running tests...\n");
	test_all();
#endif

	const char *in_filename = NULL;
	const char *out_filename = "out.c";

	ErrCtx err_ctx = {0};
	err_ctx.enabled = true;
	bool default_color_enabled;
#if UNISTD_AVAILABLE
	#if defined _POSIX_VERSION && _POSIX_VERSION >= 200112L
	/* isatty available */
	default_color_enabled = isatty(2); /* is /dev/stderr a tty? */
	#else
	default_color_enabled = false; /* old posix version */
	#endif
#else
	default_color_enabled = false; /* probably windows */
#endif
	err_ctx.color_enabled = default_color_enabled;

	for (int i = 1; i < argc; ++i) {
		if ((i == 1 || argv[i-1][0] != '-') && argv[i][0] != '-') {
			in_filename = argv[i];
		} else if (strs_equal(argv[i], "-no-color")) {
			err_ctx.color_enabled = false;
		} else if (strs_equal(argv[i], "-color")) {
			err_ctx.color_enabled = true;
		} else if (strs_equal(argv[i], "-o")) {
			if (i == argc-1) {
				fprintf(stderr, "-o cannot be the last argument to toc.\n");
				return EXIT_FAILURE;
			}
			out_filename = argv[i+1];
			++i;
		} else {
			fprintf(stderr, "Unrecognized option: %s.\n", argv[i]);
			return EXIT_FAILURE;
		}
	}
	
	if (!in_filename) {
#ifdef TOC_DEBUG
		in_filename = "test.toc";
#else
		fprintf(stderr, "Please specify an input file.\n");
		return EXIT_FAILURE;
#endif
	}
	
	Allocator main_allocr;
	allocr_create(&main_allocr);

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
	typer_create(&tr, &ev, &file, &err_ctx, &main_allocr, &globals);
	
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

