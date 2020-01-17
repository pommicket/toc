#include <stdint.h>
#include <stdio.h>
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef float f32;
typedef double f64;
typedef u8 bool;
typedef struct { void *data; i64 n; } slice_;
#define false ((bool)0)
#define true ((bool)1)
static slice_ mkslice_(void *data, i64 n) { slice_ ret; ret.data = data; ret.n = n; return ret; }
static void free_(void *data) { extern void free(void *data); free(data); }
static void *e__calloc(size_t n, size_t sz) { extern void *calloc(size_t n, size_t size); extern void abort(void); void *ret = calloc(n, sz); if (n && sz && !ret) { fprintf(stderr, "Out of memory.\n"); abort(); } return ret; }


/* declarations */
/* code */
