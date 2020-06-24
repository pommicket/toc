/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

/* see development.md for development information */

/* 
@TODO:
#includes during parsing
initialization statements (maybe #init(-50), where -50 is the priority and <0 is reserved for standard library)
if we do #include "foo.toc", bar; and foo.toc fails, bar should be declared as TYPE_UNKNOWN (right now it's undeclared)
improve type_to_str:
	Foo ::= struct(t::Type) {}
	type_to_str(Foo(int))
switch
 - #fallthrough
enums
unions
bitwise operations
---
#compile_only for functions only used at compile time
switch to / add as an alternative: libffi
	- better yet, inline assembly
don't bother generating ret_ if nothing's deferred
X ::= newtype(int); or something
any odd number of "s for a string
give each warning a number; #no_warn(warning), #no_warn_throughout_this_file(warning) 
test various parse errors; see if they can be improved, e.g. if else { 3; }
use point #except x;
optional -Wshadow
format errors so that vim/emacs can jump to them
show include stack--especially for redeclarations with #include #force
stuff like __builtin_sqrt
---
make sure that floating point literals are as exact as possible
	have some way of doing Infinity and s/qNaN (you can
	have them be in std/math.toc)
once you have a bunch of test code:
- analyze memory usage by secretly passing __FILE__, __LINE__ to allocr_m/c/realloc
- try making more Expression members pointers
- try making Value.slice a pointer
- should val_stack be on the allocator? what about temporary arrays?
	-->on the contrary, should in_decls be off the allocator?
struct param inference
maybe macros are just inline functions
passing untyped expressions to macros
#returns_code (struct body is a block, to be evaluated at compile time, which returns the actual statements)
	- struct varargs
	- also use with functions for macros?
*/

#if defined __unix__ || (defined __APPLE__ && defined __MACH__)
#define _POSIX_C_SOURCE 200112L
#include <unistd.h>
#define UNISTD_AVAILABLE 1
#endif

#include "toc.c"

#if defined TOC_DEBUG && defined __GNU_LIBRARY__ && defined UNISTD_AVAILABLE && !defined BACKTRACE
#define BACKTRACE 1
#endif
#if BACKTRACE
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
	case SIGFPE:
		fprintf(stderr, "Floating point exception.\n");
		break;
	case SIGINT:
		fprintf(stderr, "Interrupted.\n");
		break;
	case SIGTERM:
		fprintf(stderr, "Terminated.\n");
		break;
	case SIGILL:
		fprintf(stderr, "Illegal instruction.\n");
		break;
	default:
		fprintf(stderr, "Terminated for unknown reason (signal %d).\n", num);
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
	signal(SIGABRT, SIG_DFL);
	abort();
}
#endif
int main(int argc, char **argv) {
#if BACKTRACE
	program_name = argv[0];
	signal(SIGABRT, signal_handler);
	signal(SIGSEGV, signal_handler);
	signal(SIGINT, signal_handler);
	signal(SIGTERM, signal_handler);
	signal(SIGILL, signal_handler);
	signal(SIGFPE, signal_handler);
#endif
#if RUN_TESTS	
	printf("running tests...\n");
	test_all();
#endif
	const char *in_filename = NULL;
	const char *out_filename = "out.c";
	
	bool verbose = false;

	ErrCtx err_ctx = {0};
	err_ctx.enabled = true;
	bool default_color_enabled;
#if UNISTD_AVAILABLE
	#if defined _POSIX_VERSION && _POSIX_VERSION >= 200112L
	/* isatty available */
	default_color_enabled = (bool)isatty(2); /* is /dev/stderr a tty? */
	#else
	default_color_enabled = false; /* old posix version */
	#endif
#else
	default_color_enabled = false; /* probably windows */
#endif
	err_ctx.color_enabled = default_color_enabled;
	
	for (int i = 1; i < argc; ++i) {
		char *arg = argv[i];
		if (streq(arg, "-no-color")) {
			err_ctx.color_enabled = false;
		} else if (streq(arg, "-color")) {
			err_ctx.color_enabled = true;
		} else if (streq(arg, "-o")) {
			if (i == argc-1) {
				fprintf(stderr, "-o cannot be the last argument to toc.\n");
				return EXIT_FAILURE;
			}
			out_filename = argv[i+1];
			++i;
		} else if (streq(arg, "-v") || streq(arg, "-verbose")) {
			printf("Verbose mode enabled\n");
			verbose = true;
		} else {
			if (arg[0] == '-') {
				fprintf(stderr, "Unrecognized option: %s.\n", argv[i]);
				return EXIT_FAILURE;
			} else {
				in_filename = arg;
			}
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
	if (verbose) printf("Reading contents of %s...\n", in_filename);
	char *contents = read_file_contents(&main_allocr, in_filename, file_where);
	if (!contents) return EXIT_FAILURE;

	Identifiers globals;
	idents_create(&globals, &main_allocr, NULL);
	Tokenizer t;
	file.contents = contents;
	if (verbose) printf("Tokenizing...\n");
	tokr_create(&t, &err_ctx, &main_allocr);
	if (!tokenize_file(&t, &file)) {
		err_text_important(&err_ctx, "Errors occured during preprocessing.\n");
		allocr_free_all(&main_allocr);
		return EXIT_FAILURE;
	}

	if (verbose) {
		printf("Tokens:\n");
		arr_foreach(t.tokens, Token, token) {
			if (token != t.tokens)
				printf("   ");
			fprint_token(stdout, token);
		}
		printf("\n-----\n");
	}

	if (verbose) printf("Parsing...\n");
	Parser p;
	parser_create(&p, &globals, &t, &main_allocr);
	ParsedFile f;
	if (!parse_file(&p, &f)) {
		err_text_important(&err_ctx, "Errors occured during parsing.\n");
		allocr_free_all(&main_allocr);
		return EXIT_FAILURE;
	}
	if (verbose) {
		printf("Parsed file:\n");
		fprint_parsed_file(stdout, &f);
		printf("\n\n-----\n\n");
	}
	
	if (verbose) printf("Determining types...\n");
	
	Typer tr;
	Evaluator ev;
	evalr_create(&ev, &tr, &main_allocr);
	typer_create(&tr, &ev, &err_ctx, &main_allocr, &globals, &file);
	
	if (!types_file(&tr, &f)) {
		err_text_important(&err_ctx, "Errors occured while determining types.\n");
		allocr_free_all(&main_allocr);
		return EXIT_FAILURE;
	}

	if (verbose) {
		printf("Typed file:\n");
		fprint_parsed_file(stdout, &f);
		printf("\n\n-----\n\n");
	}

	if (verbose) printf("Opening output file...\n");
	FILE *out = fopen(out_filename, "w");
	if (!out) {
		err_text_important(&err_ctx, "Could not open output file: ");
		err_fprint(&err_ctx, "%s\n", out_filename);
		allocr_free_all(&main_allocr);
		return EXIT_FAILURE;
	}
	
	if (verbose) printf("Generating C code...\n");
	CGenerator g;
	cgen_create(&g, out, &globals, &main_allocr);
	cgen_file(&g, &f, &tr);
	
	if (verbose) printf("Cleaning up...\n");
	fclose(out);
	allocr_free_all(&main_allocr);
	return 0;
}
