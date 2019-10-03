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

typedef int64_t Integer;
typedef uint64_t UInteger;
typedef long double Floating; /* OPTIM: Switch to double, but make sure floating-point literals are right */

#define INTEGER_MAX INT64_MAX
#define UINTEGER_MAX UINT64_MAX
#define INTEGER_FMT "%"PRId64
#define UINTEGER_FMT "%"PRIu64

typedef uint8_t U8;
typedef uint16_t U16;
typedef uint32_t U32;
typedef uint64_t U64;

typedef int8_t I8;
typedef int16_t I16;
typedef int32_t I32;
typedef int64_t I64;

typedef float F32;
typedef double F64;


#include "location.c"
#include "err.c"
#include "allocator.c"
#include "arr.c"
#include "blockarr.c"
#include "str.c"
#include "identifiers.c"
#include "tokenizer.c"
#include "parse.c"
#include "eval.c"
#include "types.c"

#include "tests.c"
