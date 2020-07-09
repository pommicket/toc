/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

/* see development.md for development information */

/* 
@TODO:
a..b should probably only go up to b-1
figure out how printf is gonna work
if we do #include "foo.toc", bar; and foo.toc fails, bar should be declared as TYPE_UNKNOWN (right now it's undeclared)
fix #foreign not at global scope - right now the cgen'd definition doesn't use the proper type
find out why file output is really slow at compile time; try to improve it
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
warn about x : u8 = 1283;
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
maybe macros are just inline functions
passing untyped expressions to macros
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

	static void *addrs[300];
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

static const char *replace_extension(Allocator *a, const char *filename, const char *new_extension, const char *dflt) {
	char *new_filename = allocr_malloc(a, strlen(filename) + strlen(new_extension) + 1);
	strcpy(new_filename, filename);
	char *dot = strchr(new_filename, '.');
	if (dot) {
		strcpy(dot, new_extension);
		return new_filename;
	} else {
		return dflt;
	}
}

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
	const char *out_filename = NULL, *c_filename = NULL;
	const char *c_compiler = "cc";
	char *env_CC = getenv("CC");
	if (env_CC) c_compiler = env_CC;
	
	bool verbose = false;
	bool debug_build = true;
	bool compile_c = true;

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
		} else if (streq(arg, "-O")) {
			if (i == argc-1) {
				fprintf(stderr, "-O cannot be the last argument to toc.\n");
				return EXIT_FAILURE;
			}
			c_filename = argv[i+1];
			++i;
		} else if (streq(arg, "-v") || streq(arg, "-verbose")) {
			printf("Verbose mode enabled\n");
			verbose = true;
		} else if (streq(arg, "-d") || streq(arg, "-debug")) {
			debug_build = true;
		} else if (streq(arg, "-r") || streq(arg, "-release")) {
			debug_build = false;
		} else if (streq(arg, "-cc")) {
			if (i == argc-1) {
				fprintf(stderr, "-cc cannot be the last argument to toc.\n");
				return EXIT_FAILURE;
			}
			c_compiler = argv[i+1];
			++i;
		} else if (streq(arg, "-c")) {
			compile_c = false;
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

	if (c_filename && out_filename && !compile_c) {
		fprintf(stderr, "You can't set both -o and -O unless you're compiling the outputted C code.");
		return EXIT_FAILURE;
	}
	if (!c_filename) {
		if (out_filename && !compile_c) {
			c_filename = out_filename;
		} else {
			c_filename = replace_extension(&main_allocr, in_filename, ".c", "out.c");
		}
	}
	if (!out_filename) {
		out_filename = replace_extension(&main_allocr, in_filename, "", "a.out");
	}

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

	GlobalCtx global_ctx = {0};
	str_hash_table_create(&global_ctx.included_files, sizeof(IncludedFile), &main_allocr);
	global_ctx.main_file = &file;
	global_ctx.err_ctx = &err_ctx;
	global_ctx.debug_build = debug_build;

	Parser p;
	parser_create(&p, &globals, &t, &main_allocr, &global_ctx);
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
	typer_create(&tr, &ev, &main_allocr, &globals, &global_ctx);
	
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

	if (verbose) printf("Opening output file %s...\n", c_filename);
	FILE *out = fopen(c_filename, "w");
	if (!out) {
		err_text_important(&err_ctx, "Could not open output file: ");
		err_fprint(&err_ctx, "%s\n", c_filename);
		allocr_free_all(&main_allocr);
		return EXIT_FAILURE;
	}
	
	if (verbose) printf("Generating C code...\n");
	CGenerator g;
	cgen_create(&g, out, &globals, &main_allocr);
	cgen_file(&g, &f, &tr);
	fclose(out);
	
	if (compile_c) {
		if (verbose) printf("Compiling C code...\n");
		char cmd[2048] = {0};
		char const *cflags = debug_build ? "-O0 -g" : "-O3 -s";
		snprintf(cmd, sizeof cmd, "%s -w %s %s -o %s", c_compiler, cflags, c_filename, out_filename);
		int ret = system(cmd);
		if (ret) {
			fprintf(stderr, "Uh oh... C compiler failed...\n");
			return EXIT_FAILURE;
		}
	}

	if (verbose) printf("Cleaning up...\n");
	allocr_free_all(&main_allocr);
	return 0;
}
