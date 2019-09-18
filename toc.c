/* Includes all of toc's files */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdbool.h>

typedef unsigned long long UInteger;
typedef long long Integer;
typedef long double Floating; /* OPTIM: Switch to double */
#define INTEGER_MAX INT64_MAX
#define UINTEGER_FMT "%llu"
#define INTEGER_FMT "%lld"

#include "util/err.c"
#include "util/arr.c"
#include "util/blockarr.c"
#include "util/str.c"
#include "identifiers.c"
#include "tokenizer.c"
#include "util/location.c"
#include "parse.c"
#include "eval.c"
#include "types.c"
