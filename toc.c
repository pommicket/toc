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
#include "allocator.c"
#include "arr.c"
#include "location.c"
#include "err.c"
#include "rand.c"
#include "blockarr.c"
#include "str.c"
#include "instance_table.c"
#include "copy.c"

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
