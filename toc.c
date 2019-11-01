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

#include "types.h"

#include "location.c"
#include "err.c"
#include "allocator.c"
#include "arr.c"
#include "blockarr.c"
#include "str.c"
#include "identifiers.c"
#include "tokenizer.c"
#include "parse.c"
#include "scope.c"


static Type *type_inner(Type *t) {
	while (t && t->kind == TYPE_USER)
		t = ident_typeval(t->user.name);
	return t;
}

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
