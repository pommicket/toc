/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

/* OPTIM: is it faster to store void *end? */
typedef struct {
	size_t len;
	size_t cap;
	MaxAlign data[];
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

static void arr_clear_(void **arr) {
	if (*arr) {
		free(arr_hdr(*arr));
		*arr = NULL;
	}
}

static void arr_cleara_(void **arr, size_t size, Allocator *allocr) {
	if (*arr) {
		ArrHeader *header = arr_hdr(*arr);
		allocr_free(allocr, header, header->cap * size);
		*arr = NULL;
	}
}

static void arr_set_len_(void **arr, size_t n, size_t item_sz) {
	if (n == 0) {
		arr_clear_(arr);
		return;
	}
	arr_resv_(arr, n, item_sz);
	arr_hdr(*arr)->len = n;
}
static void arr_set_lena_(void **arr, size_t n, size_t item_sz, Allocator *a) {
	if (n == 0) {
		/* OPTIM: arr_cleara */
	}
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


static void *arr_last_(void *arr, size_t item_sz) {
	if (arr) {
		ArrHeader *hdr = arr_hdr(arr);
		return hdr->len == 0 ? NULL : (char *)hdr->data + (hdr->len-1) * item_sz;
	} else {
		return NULL;
	}
}

static void *arr_end_(void *arr, size_t item_sz) {
	if (arr) {
		ArrHeader *hdr = arr_hdr(arr);
		return hdr->len == 0 ? NULL : (char *)hdr->data + hdr->len * item_sz;
	} else {
		return NULL;
	}
}

/* OPTIM: shrink array */
static void arr_remove_last_(void **arr) {
	assert(arr_hdr(*arr)->len);
	if (--arr_hdr(*arr)->len == 0) {
		arr_clear_(arr);
	}
}

static void arr_remove_lasta_(void **arr, size_t item_sz, Allocator *a) {
	assert(arr_hdr(*arr)->len);
	if (--arr_hdr(*arr)->len == 0) {
		arr_cleara_(arr, item_sz, a);
	}
}



static void arr_copya_(void **out, void *in, size_t item_sz, Allocator *a) {
	size_t len = arr_len(in);
	arr_resva_(out, len, item_sz, a);
	memcpy(*out, in, len * item_sz);
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
#define arr_adda(arr, allocr) (arr_ptr_type(arr))arr_adda_((void **)(arr), sizeof **(arr), (allocr))
#define arr_resv(arr, n) arr_resv_((void **)(arr), n, sizeof **(arr))
#define arr_resva(arr, n, allocr) arr_resva_((void **)(arr), n, sizeof **(arr), (allocr))
#define arr_set_len(arr, n) arr_set_len_((void **)(arr), n, sizeof **(arr))
#define arr_set_lena(arr, n, a) arr_set_lena_((void **)(arr), n, sizeof **(arr), (a))
#define arr_clear(arr) arr_clear_((void **)(arr)), (void)sizeof **arr /* second part makes sure most of the time that you don't accidentally call it without taking the address */
#define arr_cleara(arr, allocr) arr_cleara_((void **)(arr), sizeof **(arr), (allocr))
#define arr_last(arr) arr_last_((void *)(arr), sizeof *(arr))
/* one past last, or NULL if empty */
#define arr_end(arr) arr_end_((void *)(arr), sizeof *(arr))
#define arr_foreach(arr, type, var) for (type *var = arr, *join(var,_foreach_end) = arr_end(arr); var < join(var,_foreach_end); ++var) /* NOTE: < is useful here because currently it's possible for var_foreach_end to be NULL but var could start out not null */
#define arr_remove_last(arr) arr_remove_last_((void **)(arr)), (void)sizeof **(arr)
#define arr_remove_lasta(arr, a) arr_remove_lasta_((void **)(arr), sizeof **(arr), (a))
#define arr_copya(out, in, a) do { assert(sizeof *(in) == sizeof **(out)); arr_copya_((void **)(out), (in), sizeof **(out), (a)); } while(0)
	
#ifdef TOC_DEBUG
static void arr_test(void) {
	int *foos = NULL;
	for (int i = 0; i < 10; ++i) {
		*(int *)arr_add(&foos) = i;
	}
	for (int i = 0; i < (int)arr_len(foos); ++i) {
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
