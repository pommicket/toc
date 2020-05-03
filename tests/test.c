#include <stdio.h>
#include <stdlib.h>
static const char *tests[] = {
	"bf",
	"control_flow",
	"types",
	"include",
	"arrs_slices",
	"ptr_arithmetic",
	"defer",
	"sizeof",
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
	"gcc -O0 -g",
	"clang -O3 -s",
	"tcc"
};
#endif
#define ncompilers ((int)(sizeof compilers / sizeof *compilers))


int main(void) {
	int i, c;
	char command[256];
#if windows	
	char path[MAX_PATH];
	GetModuleFileNameA(NULL, path, MAX_PATH);
	char *backslash = strrchr(path, '\\');
	*backslash = 0;
	puts(path);
	_chdir(path);
#endif
	for (i = 0; i < ntests; ++i) {
		const char *test = tests[i];
		printf("Running test %s... ", test);
		if (windows) {
			sprintf(command, "..\\toc %s.toc", test);
		} else {
			sprintf(command, "valgrind ../toc %s.toc", test);
		}
		if (system(command)) {
			fprintf(stderr, "FAILED (while compiling toc).\n");
			return EXIT_FAILURE;
		}
		for (c = 0; c < ncompilers; ++c) {
			const char *compiler = compilers[c];
			if (windows) {
				sprintf(command, "%s out.c", compiler);
			} else {
				sprintf(command, "valgrind %s out.c", compiler);
			}
			if (system(command)) {
				fprintf(stderr, "FAILED (while compiling C).\n");
				return EXIT_FAILURE;
			}
			{
				int ret;
				if (windows)
					ret = system("out > got");
				else
					ret = system("./a.out > got");
				if (ret != 0) {
					fprintf(stderr, "FAILED (while running).\n");
					return EXIT_FAILURE;
				}
			}

			{
				char filename[32];
				sprintf(filename, "%s_expected", test);
				char *expected = NULL, *got = NULL;
				FILE *expected_f = fopen(filename, "rb");
				if (!expected_f) {
					fprintf(stderr, "FAILED (couldn't open %s).\n", filename);
					return EXIT_FAILURE;
				}
				fseek(expected_f, 0L, SEEK_END);
				size_t expected_size = ftell(expected_f);
				fseek(expected_f, 0L, SEEK_SET);
				expected = malloc(expected_size);
				fread(expected, 1, expected_size, expected_f);
				fclose(expected_f);
				FILE *got_f = fopen("got", "rb");
				if (!got_f) {
					fprintf(stderr, "FAILED (couldn't open got).\n");
					return EXIT_FAILURE;
				}
				fseek(got_f, 0L, SEEK_END);
				size_t got_size = ftell(got_f);
				fseek(got_f, 0L, SEEK_SET);
				got = malloc(got_size);
				fread(got, 1, got_size, got_f);
				fclose(got_f);
				if (expected_size != got_size) {
					fprintf(stderr, "FAILED (mismatch between expected/got file sizes).\n");
					return EXIT_FAILURE;
				}
				if (memcmp(expected, got, expected_size) != 0) {
					fprintf(stderr, "FAILED (mismatch between expected/got file contents).\n");
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
		printf("success!\n");
	}
	return 0;
}
