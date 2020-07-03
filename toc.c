/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

/* Includes all of toc's files */
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include <inttypes.h>
#include <errno.h>

#ifndef COMPILE_TIME_FOREIGN_FN_SUPPORT
#define COMPILE_TIME_FOREIGN_FN_SUPPORT 1
#endif


#ifdef __cplusplus
#define new new_
#define this this_
#define export export_
#elif __STDC_VERSION__ < 199901
#ifndef inline
#define inline
#endif
#endif


/* use toc_alignof only for non-structs. it may be incorrect for pre-C(++)11. */
#if (__STDC_VERSION__ >= 201112 || __cplusplus >= 201103L) && !defined __TINYC__ && !defined __OpenBSD__ && !defined __FreeBSD__
#include <stdalign.h>
#define toc_alignof alignof

#ifdef __GNUC__
/* GCC supports non-string literals as the message for a static assertion */
#define possibly_static_assert(cond) static_assert(cond, "Assertion " #cond " failed.")
#else
#define possibly_static_assert(cond) static_assert(cond, "Assertion failed")
#endif


#else

#define toc_alignof sizeof

#define possibly_static_assert(cond) assert(cond)
#endif


#include "types.h"

/* forward declarations for debugging */
static void print_val(Value v, Type *t);
static void print_token(Token *t);
static void print_type(Type *t);
static void print_block(Block *b);
static void print_decl(Declaration *d);
static void print_stmt(Statement *s);
static void print_block_location(Block *b);


/* misc */
#define join3(a,b) a##b
#define join2(a,b) join3(a,b)
#define join(a,b) join2(a,b)
#define stringify3(x) #x
#define stringify2(x) stringify3(x)
#define stringify(x) stringify2(x)

#ifdef __linux__ /* see also cgen_file */
#define platform__ PLATFORM_LINUX
#elif defined _WIN32
#define platform__ PLATFORM_WINDOWS
#elif defined __APPLE__
#define platform__ PLATFORM_OSX
#elif defined __FreeBSD__
#define platform__ PLATFORM_FREEBSD
#elif defined __OpenBSD__
#define platform__ PLATFORM_OPENBSD
#elif defined __unix__
#define platform__ PLATFORM_MISC_UNIX
#else
#define platform__ PLATFORM_OTHER
#endif

static void fprint_char_literal(FILE *f, char c) {
	if (isprint(c))
		fprintf(f, "'%c'", c);
	else
		fprintf(f, "'\\x%02x'", c);
}


static inline bool type_is_builtin(Type *t, BuiltinType b) {
	return t->kind == TYPE_BUILTIN && t->builtin == b;
}
static inline bool type_is_void(Type *t) {
	return t->kind == TYPE_BUILTIN && t->builtin == BUILTIN_VOID;
}

static inline bool type_is_slicechar(Type *t) {
	return t->kind == TYPE_SLICE && type_is_builtin(t->slice, BUILTIN_CHAR);
}

/* utilities */
#include "allocator.c"
#include "misc.c"
#include "data_structures.c"
#include "err.c"
static size_t compiler_alignof(Type *t);
static size_t compiler_sizeof(Type *t);
#include "instance_table.c"

/* returns NULL on error */
static char *read_file_contents(Allocator *a, const char *filename, Location where) {
	FILE *in = fopen(filename, "r");
	
	if (!in) {
		err_print(where, "Could not open file: %s.", filename);
		return NULL;
	}
	char *contents = allocr_malloc(a, 4096);
	contents[0] = 0; /* put 0 byte at the start of the file. see err.c:err_print_location_text to find out why */
	contents[1] = 0; /* if fgets fails the first time */
	long contents_cap = 4095;
	long contents_len = 1;
	while (fgets(contents + contents_len, (int)(contents_cap - contents_len), in)) {
		contents_len += (long)strlen(contents + contents_len);
		
		if (contents_len >= (long)contents_cap - 1024) {
			size_t prev = (size_t)contents_cap + 1;
			contents_cap *= 2;
			contents = allocr_realloc(a, contents, prev, (size_t)contents_cap + 1);
		}
	}
	++contents;
	fclose(in);
	return contents;
}
	

static Location token_location(File *file, Token *t);

#include "identifiers.c"
#include "copy.c"
#include "tokenizer.c"
#include "parse.c"

#if COMPILE_TIME_FOREIGN_FN_SUPPORT
#if defined _MSC_VER
#include "foreign_msvc.c"
#elif defined __unix__ || defined __OSX__
#include "foreign_unix.c"
#endif
#else
static bool foreign_call(ForeignFnManager *ffmgr, FnExpr *fn, Type *ret_type, Type *arg_types, size_t arg_types_stride, Value *args, size_t nargs, Location call_where, Value *ret) {
	(void)ffmgr; (void)fn; (void)ret_type; (void)arg_types; (void)arg_types_stride; (void)args; (void)nargs; (void)ret;
	err_print(call_where, "You have not compiled toc with compile time foreign function support.");
	return false;
}
typedef char Library;
#endif
static void ffmgr_create(ForeignFnManager *ffmgr, Allocator *allocr) {
	ffmgr->allocr = allocr;
	str_hash_table_create(&ffmgr->libs_loaded, sizeof(Library), allocr);
}

#include "infer.c"
#include "types.c"
#include "eval.c"
#include "cgen.c"

#if RUN_TESTS
#include "tests.c"
#endif



#ifdef __cplusplus
#undef new
#undef this
#elif __STDC_VERSION__ < 199901
#undef inline
#endif
