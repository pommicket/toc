#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __unix__
#include <unistd.h>
#define unix 1
#endif

static const char *tests[] = {
	"bf",
	"control_flow",
	"types",
	"init",
	"include",
	"arrs_slices",
	"ptr_arithmetic",
	"defer",
#ifndef __OpenBSD__
	"sizeof", /* OpenBSD's GCC doesn't support _Alignof */
#endif
	"new",
	"arr",
	"arr2",
	"arr3",
	"params",
	"nms",
	"varargs",
	"printf",
	"use",
	"misc"
};

#define ntests ((int)(sizeof tests / sizeof *tests))

#ifdef _WIN32
#define windows 1
#include <windows.h>
#else
#define windows 0
#endif

#if windows
static const char *compilers[] = {
	"cl /WX /nologo legacy_stdio_definitions.lib"
};
#else
static const char *compilers[] = {
	"gcc -O0 -g -Wno-parentheses-equality -Wno-builtin-declaration-mismatch -std=c99",
	"clang -O3 -s -Wno-parentheses-equality -Wno-builtin-requires-header -Wno-format-security -std=c99"
#ifdef __linux__ /* couldn't get TCC to build on BSD */
	,
	"tcc"
#endif
};
#endif
#define ncompilers ((int)(sizeof compilers / sizeof *compilers))

static char *str_dup(const char *s) {
	size_t bufsz = strlen(s)+1;
	char *ret = malloc(bufsz);
	memcpy(ret, s, bufsz);
	return ret;	
}


int main(int argc, char **argv) {
	int i, c;
	char command[256];
	int color = 0;
	int valgrind = 0;
#if unix
	color = isatty(1);
	#ifdef __linux__ /* valgrind is complaining a lot on BSD, but i think it's wrong */
	valgrind = 1;
	#endif
#endif
	char const *failed;
	if (color) {
		failed = "\x1b[91mFAILED\x1b[0m";
	} else {
		failed = "FAILED";
	}
	for (i = 0; i < ntests; ++i) {
		const char *test = tests[i];
		printf("Running test %s... ", test);
		fflush(stdout);
		if (windows) {
			sprintf(command, "..\\toc -c %s.toc -o out.c", test);
		} else if (valgrind) {
			sprintf(command, "valgrind -q --error-exitcode=1 ../toc -c %s.toc -o out.c", test);
		} else {
			sprintf(command, "../toc -c %s.toc -o out.c", test);
		}
		if (system(command)) {
			fprintf(stderr, "%s (while compiling toc).\n", failed);
			return EXIT_FAILURE;
		}
		for (c = 0; c < ncompilers; ++c) {
			const char *compiler = str_dup(compilers[c]);
			if (strcmp(test, "sizeof") == 0 && strncmp(compiler, "tcc", 3) == 0)
				continue; /* i think some versions of tcc have _Alignof but some don't */
			char *p = strchr(compiler, ' ');
			if (p) *p = 0;
			printf("on %s... ", compiler);
			if (p) *p = ' ';
			fflush(stdout);
			if (windows) {
				sprintf(command, "%s out.c", compiler);
			} else {
				sprintf(command, "%s out.c", compiler);
			}
			if (system(command)) {
				fprintf(stderr, "%s (while compiling C).\n", failed);
				return EXIT_FAILURE;
			}
			{
				int ret;
				if (windows)
					ret = system("out > got");
				else
					ret = system("./a.out > got");
				if (ret != 0) {
					fprintf(stderr, "%s (while running).\n", failed);
					return EXIT_FAILURE;
				}
			}

			{
				char filename[32];
				sprintf(filename, "%s_expected", test);
				char *expected = NULL, *got = NULL;
				FILE *expected_f = fopen(filename, "rb");
				if (!expected_f) {
					fprintf(stderr, "%s (couldn't open %s).\n", failed, filename);
					return EXIT_FAILURE;
				}
				fseek(expected_f, 0L, SEEK_END);
				size_t expected_size = (size_t)ftell(expected_f);
				fseek(expected_f, 0L, SEEK_SET);
				expected = malloc(expected_size);
				fread(expected, 1, expected_size, expected_f);
				fclose(expected_f);
				FILE *got_f = fopen("got", "rb");
				if (!got_f) {
					fprintf(stderr, "%s (couldn't open got).\n", failed);
					return EXIT_FAILURE;
				}
				fseek(got_f, 0L, SEEK_END);
				size_t got_size = (size_t)ftell(got_f);
				fseek(got_f, 0L, SEEK_SET);
				got = malloc(got_size);
				fread(got, 1, got_size, got_f);
				fclose(got_f);
				if (expected_size != got_size) {
					fprintf(stderr, "%s (mismatch between expected/got file sizes).\n", failed);
					return EXIT_FAILURE;
				}
				if (memcmp(expected, got, expected_size) != 0) {
					fprintf(stderr, "%s (mismatch between expected/got file contents).\n", failed);
					return EXIT_FAILURE;
				}
				free(expected); free(got);
			}
			if (windows)
				remove("out.exe");
		}
		if (windows) {
		} else {
			remove("a.out");
		}
		remove("out.c");
		remove("got");
		if (color) {
			printf("\x1b[92msuccess!\x1b[0m\n");
		} else {
			printf("success!\n");
		}
	}
	return 0;
}
