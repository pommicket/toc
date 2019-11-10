/* Includes all of toc's files */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <inttypes.h>
#include <stdbool.h>
#include <float.h>

#include "types.h"

static Type *type_user_underlying(Type *t) {
	assert(t->kind == TYPE_USER);
	Declaration *d = t->user.decl;
	assert(d->flags & DECL_FOUND_VAL);
	return (d->type.kind == TYPE_TUPLE ? d->val.tuple[t->user.index] : d->val).type;
}

static Type *type_inner(Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	while (t->kind == TYPE_USER) {
		t = type_user_underlying(t);
	}
	return t;
}

#include "rand.c"
#include "location.c"
#include "err.c"
#include "allocator.c"
#include "arr.c"
#include "blockarr.c"
#include "str.c"
#include "hash_tables.c"

#include "identifiers.c"
#include "tokenizer.c"
#include "parse.c"
#include "scope.c"


#include "eval.c"
#include "types.c"
static bool cgen_decls_file(CGenerator *g, ParsedFile *f);
static bool typedefs_file(CGenerator *g, ParsedFile *f);
#include "cgen.c"
#include "typedefs_cgen.c"
#include "decls_cgen.c"

#ifdef TOC_DEBUG
#include "tests.c"
#endif
