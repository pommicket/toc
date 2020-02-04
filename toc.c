/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

/* NOTE: all stages should use the same allocator! */

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

#ifndef COMPILE_TIME_FOREIGN_FN_SUPPORT
#define COMPILE_TIME_FOREIGN_FN_SUPPORT 0
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

#if __STDC_VERSION__ >= 201112
#define possibly_static_assert(cond) static_assert(cond, "Assertion " #cond " failed.")
#else
#define possibly_static_assert assert
#endif


#include "types.h"

/* forward declarations for debugging */
static void print_val(Value v, Type *t);
static void print_token(Token *t);


/* misc */
#define STRINGIFY2(x) #x
#define STRINGIFY(x) STRINGIFY2(x)
#define join3(a,b) a##b
#define join2(a,b) join3(a,b)
#define join(a,b) join2(a,b)
#define eval(x) x

static void fprint_char_literal(FILE *f, char c) {
	if (isprint(c))
		fprintf(f, "'%c'", c);
	else
		fprintf(f, "'\\x%02x'", c);
}


static inline bool type_is_builtin(Type *t, BuiltinType b) {
	return t->kind == TYPE_BUILTIN && t->builtin == b;
}

static inline bool type_is_slicechar(Type *t) {
	return t->kind == TYPE_SLICE && type_is_builtin(t->slice, BUILTIN_CHAR);
}

/* utilities */
#include "allocator.c"
#include "misc.c"
#include "data_structures.c"
#include "location.c"
#include "err.c"
#include "blockarr.c"
static size_t compiler_alignof(Type *t);
static size_t compiler_sizeof(Type *t);
#include "instance_table.c"
#include "copy.c"

/* returns NULL on error */
static char *read_entire_file(Allocator *a, const char *filename, Location where) {
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
	return contents;
}
	


#include "identifiers.c"
#include "tokenizer.c"
#include "parse.c"
#include "scope.c"
#include "foreign.c"
#include "eval.c"
#include "infer.c"
#include "types.c"
static bool cgen_decls_file(CGenerator *g, ParsedFile *f);
static bool cgen_sdecls_file(CGenerator *g, ParsedFile *f);
#include "cgen.c"
#include "sdecls_cgen.c"
#include "decls_cgen.c"

#ifdef TOC_DEBUG
#include "tests.c"
#endif



#ifdef __cplusplus
#undef new
#undef this
#elif __STDC_VERSION__ < 199901
#undef inline
#endif
