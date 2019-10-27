/* OPTIM: is it faster to store void *end? */
typedef struct {
	size_t len;
	size_t cap;
	max_align_t data[];
} ArrHeader;

static inline ArrHeader *arr_hdr(void *arr) {
	ArrHeader *hdr = (ArrHeader *)((char *)arr - offsetof(ArrHeader, data));
	return hdr;
}

static inline size_t arr_len(void *arr) {
	if (arr == NULL) return 0;
	return arr_hdr(arr)->len;
}

static void arr_resv_(void **arr, size_t n, size_t item_sz) {
	if (*arr == NULL) {
		ArrHeader *hdr = err_malloc(item_sz * n + sizeof(ArrHeader) + 1); /* +1 => prevent ptr overflow */
		hdr->len = 0;
		hdr->cap = n;
		*arr = hdr->data;
	} else {
		ArrHeader *hdr = arr_hdr(*arr);
		hdr->cap = n;
		hdr = err_realloc(hdr, item_sz * n + sizeof(ArrHeader) + 1);
		if (hdr->len > hdr->cap) hdr->len = hdr->cap;
		*arr = hdr->data;
	}		
}
static void arr_resva_(void **arr, size_t n, size_t item_sz, Allocator *a) {
	if (*arr == NULL) {
		ArrHeader *hdr = allocr_malloc(a, item_sz * n + sizeof(ArrHeader));
		hdr->len = 0;
		hdr->cap = n;
		*arr = hdr->data;
	} else {
		ArrHeader *hdr = arr_hdr(*arr);
		hdr = allocr_realloc(a, hdr, item_sz * hdr->cap + sizeof(ArrHeader), item_sz * n + sizeof(ArrHeader));
		hdr->cap = n;
		if (hdr->len > hdr->cap) hdr->len = hdr->cap;
		*arr = hdr->data;
	}		
}


static void arr_set_len_(void **arr, size_t n, size_t item_sz) {
	arr_resv_(arr, n, item_sz);
	arr_hdr(*arr)->len = n;
}
static void arr_set_lena_(void **arr, size_t n, size_t item_sz, Allocator *a) {
	arr_resva_(arr, n, item_sz, a);
	arr_hdr(*arr)->len = n;
}

static void *arr_add_(void **arr, size_t item_sz) {
	ArrHeader *hdr;
    if (*arr == NULL) {
		arr_resv_(arr, 10, item_sz);
		hdr = arr_hdr(*arr);
	} else {
		hdr = arr_hdr(*arr);
		if (hdr->len >= hdr->cap) {
			arr_resv_(arr, hdr->len * 2 + 1, item_sz);
			hdr = arr_hdr(*arr);
		}
	}
	return &(((char *)hdr->data)[(hdr->len++) * item_sz]);
}
static void *arr_adda_(void **arr, size_t item_sz, Allocator *a) {
	ArrHeader *hdr;
    if (*arr == NULL) {
		arr_resva_(arr, 10, item_sz, a);
		hdr = arr_hdr(*arr);
	} else {
		hdr = arr_hdr(*arr);
		if (hdr->len >= hdr->cap) {
			arr_resva_(arr, hdr->len * 2 + 1, item_sz, a);
			hdr = arr_hdr(*arr);
		}
	}
	return &(((char *)hdr->data)[(hdr->len++) * item_sz]);
}

static void arr_clear_(void **arr) {
	if (*arr) {
		free(arr_hdr(*arr));
		*arr = NULL;
	}
}

static void *arr_last_(void *arr, size_t item_sz) {
	if (arr) {
		ArrHeader *hdr = arr_hdr(arr);
		return hdr->len == 0 ? NULL : (char *)hdr->data + (hdr->len-1) * item_sz;
	} else {
		return NULL;
	}
}

/* OPTIM: shrink array */
static void arr_remove_last_(void **arr, size_t item_sz) {
	
	assert(arr_hdr(*arr)->len);
	arr_hdr(*arr)->len--; (void)item_sz;
}

#ifdef __GNUC__
#define typeof __typeof__
#endif

#if defined(__GNUC__) || defined(__TINYC__)
#define HAS_TYPEOF 1
#endif

#if HAS_TYPEOF
/* 
this is to cast the return value of arr_add so that gcc produces a warning if you
do something like:
float *arr = NULL;
// ...
int *x = arr_add(&arr);
You shouldn't rely on this, though, e.g. by doing
*arr_add(&arr) = 17;
 */
#define arr_ptr_type(arr) __typeof__(*(arr))
#else
#define arr_ptr_type(arr) void *
#endif

#define arr_add(arr) (arr_ptr_type(arr))arr_add_((void **)(arr), sizeof **(arr))
#define arr_adda(arr, allocr) (arr_ptr_type(arr))arr_adda_((void **)(arr), sizeof **(arr), allocr)
#define arr_resv(arr, n) arr_resv_((void **)(arr), n, sizeof **(arr))
#define arr_resva(arr, n, allocr) arr_resva_((void **)(arr), n, sizeof **(arr), allocr)
#define arr_set_len(arr, n) arr_set_len_((void **)(arr), n, sizeof **(arr))
#define arr_set_lena(arr, n, a) arr_set_lena_((void **)(arr), n, sizeof **(arr), a)
#define arr_clear(arr) arr_clear_((void **)(arr))
#define arr_last(arr) arr_last_((void *)(arr), sizeof *(arr))
#define arr_foreach(arr, type, var) for (type *var = arr_len(arr) ? arr : NULL, *var##_foreach_end = arr_last(arr); var; var == var##_foreach_end ? var = NULL : var++)
#define arr_remove_last(arr) arr_remove_last_((void **)(arr), sizeof **(arr))

#ifdef TOC_DEBUG
static void arr_test(void) {
	int *foos = NULL;
	for (int i = 0; i < 1000; i++) {
		*(int *)arr_add(&foos) = i;
	}
	for (int i = 0; i < (int)arr_len(foos); i++) {
		assert(foos[i] == i);
	}
	int lastx = -1;
	arr_foreach(foos, int, x) {
		assert(*x == lastx + 1);
		lastx = *x;
	}
	arr_clear(&foos);
}
#endif
