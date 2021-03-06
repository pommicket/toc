/*
Dynamic arrays and hash tables in C

This is free and unencumbered software released into the public domain.
Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.
In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
For more information, please refer to <http://unlicense.org/>
*/

typedef struct ArrHeader {
	U32 len;
	U32 cap;
	MaxAlign data[];
} ArrHeader;

static inline ArrHeader *arr_hdr(void *arr) {
	return (ArrHeader *)((char *)arr - offsetof(ArrHeader, data));
}

static inline size_t arr_len(void *arr) {
	if (arr == NULL) return 0;
	return arr_hdr(arr)->len;
}

static inline void arr_zero_(void *arr, size_t item_sz) {
	memset(arr, 0, item_sz * arr_len(arr));
}

static WarnUnusedResult void *arr_resv_(void *arr, size_t n, size_t item_sz) {
	ArrHeader *hdr;
	assert(n < U32_MAX);
	if (arr == NULL) {
		hdr = err_malloc(item_sz * n + sizeof(ArrHeader));
		hdr->len = 0;
		hdr->cap = (U32)n;
	} else {
		hdr = arr_hdr(arr);
		hdr->cap = (U32)n;
		hdr = err_realloc(hdr, item_sz * n + sizeof(ArrHeader));
		if (hdr->len > hdr->cap) hdr->len = hdr->cap;
	}
	return hdr->data;
}
static WarnUnusedResult void *arr_resva_(void *arr, size_t n, size_t item_sz, Allocator *a) {
	ArrHeader *hdr;
	if (arr == NULL) {
		hdr = allocr_malloc(a, item_sz * n + sizeof(ArrHeader));
		hdr->len = 0;
		hdr->cap = (U32)n;
	} else {
		hdr = arr_hdr(arr);
		hdr = allocr_realloc(a, hdr, item_sz * hdr->cap + sizeof(ArrHeader), item_sz * n + sizeof(ArrHeader));
		hdr->cap = (U32)n;
		if (hdr->len > hdr->cap) hdr->len = hdr->cap;
	}
	return hdr->data;
}


// accommodate one more element if necessary
static WarnUnusedResult void *arr_grow(void *arr, size_t item_sz) {
	ArrHeader *hdr;
	if (arr == NULL) {
		hdr = err_malloc(sizeof *hdr + item_sz);
		hdr->len = 0;
		hdr->cap = 1;
	} else {
		hdr = arr_hdr(arr);
		if (hdr->len >= hdr->cap) {
			size_t new_size = sizeof *hdr + item_sz * (hdr->cap *= 2);
			hdr = err_realloc(hdr, new_size);
		}
	}
	return hdr->data;
}

static WarnUnusedResult void *arr_growa(void *arr, size_t item_sz, Allocator *allocr) {
	ArrHeader *hdr;
	if (arr == NULL) {
		hdr = allocr_malloc(allocr, sizeof *hdr + item_sz);
		hdr->len = 0;
		hdr->cap = 1;
	} else {
		hdr = arr_hdr(arr);
		if (hdr->len >= hdr->cap) {
			size_t old_size = sizeof *hdr + item_sz * hdr->cap;
			hdr->cap *= 2;
			size_t new_size = sizeof *hdr + item_sz * hdr->cap;
			hdr = allocr_realloc(allocr, hdr, old_size, new_size);
		}
	}
	return hdr->data;
}

static WarnUnusedResult void *arr_clear_(void *arr) {
	if (arr) {
		free(arr_hdr(arr));
	}
	return NULL;
}

static WarnUnusedResult void *arr_cleara_(void *arr, size_t size, Allocator *allocr) {
	if (arr) {
		ArrHeader *header = arr_hdr(arr);
		allocr_free(allocr, header, header->cap * size);
	}
	return NULL;
}

static WarnUnusedResult void *arr_set_len_(void *arr, size_t n, size_t item_sz) {
	if (n == 0) {
		return arr_clear_(arr);
	}
	if (n > arr_len(arr)) {
		arr = arr_resv_(arr, n, item_sz);
	}
	arr_hdr(arr)->len = (U32)n;
	// @OPTIM: shrink
	return arr;
}
static WarnUnusedResult void *arr_set_lena_(void *arr, size_t n, size_t item_sz, Allocator *a) {
	if (n == 0) {
		return arr_cleara_(arr, item_sz, a);
	}
	arr = arr_resva_(arr, n, item_sz, a);
	arr_hdr(arr)->len = (U32)n;
	return arr;
}

static void *arr_add_ptr_(void **arr, size_t item_sz) {
	*arr = arr_grow(*arr, item_sz);
	ArrHeader *hdr = arr_hdr(*arr);
	return &(((char *)hdr->data)[(hdr->len++) * item_sz]);
}
static void *arr_adda_ptr_(void **arr, size_t item_sz, Allocator *a) {
	*arr = arr_growa(*arr, item_sz, a);
	ArrHeader *hdr = arr_hdr(*arr);
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

// @OPTIM: shrink array
static WarnUnusedResult void *arr_remove_last_(void *arr) {
	assert(arr_hdr(arr)->len);
	if (--arr_hdr(arr)->len == 0) {
		return arr_clear_(arr);
	}
	return arr;
}

static WarnUnusedResult void *arr_remove_lasta_(void *arr, size_t item_sz, Allocator *a) {
	assert(arr_hdr(arr)->len);
	if (--arr_hdr(arr)->len == 0) {
		return arr_cleara_(arr, item_sz, a);
	}
	return arr;
}



static WarnUnusedResult void *arr_copya_(void *out, void *in, size_t item_sz, Allocator *a) {
	size_t len = arr_len(in);
	out = arr_resva_(out, len, item_sz, a);
	memcpy(out, in, len * item_sz);
	return out;
}

#if defined(__GNUC__) || defined(__TINYC__)
#define HAS_TYPEOF 1
#define typeof __typeof__
#endif

#if HAS_TYPEOF
/*
this is to cast the return value of arr_add so that gcc produces a warning if you
do something like:
float *arr = NULL;
// ...
int *x = arr_add_ptr(&arr);
You shouldn't rely on this, though, e.g. by doing
*arr_add_ptr(&arr) = 17;
 */
#define arr_ptr_type(arr) typeof(arr)
#else
#define arr_ptr_type(arr) void *
#endif

#define arr_zero(arr) arr_zero_(arr, sizeof *(arr))
#define arr_add(arr, x) arr = arr_grow((arr), sizeof *(arr)), (arr)[arr_hdr(arr)->len++] = x
#define arr_add_ptr(arr) (arr_ptr_type(arr))arr_add_ptr_((void **)(&arr), sizeof *(arr))
#define arr_adda(arr, x, allocr) arr = arr_growa((arr), sizeof *(arr), (allocr)), (arr)[arr_hdr(arr)->len++] = x
#define arr_adda_ptr(arr, allocr) (arr_ptr_type(arr))arr_adda_ptr_((void **)(&arr), sizeof *(arr), (allocr))
#define arr_resv(arr, n) arr = arr_resv_((arr), n, sizeof *(arr))
#define arr_resva(arr, n, allocr) arr = arr_resva_((arr), n, sizeof *(arr), (allocr))
#define arr_set_len(arr, n) arr = arr_set_len_((arr), n, sizeof *(arr))
#define arr_set_lena(arr, n, allocr) arr = arr_set_lena_((arr), n, sizeof *(arr), (allocr))
#define arr_clear(arr) arr = arr_clear_(arr)
#define arr_cleara(arr, allocr) arr = arr_cleara_((arr), sizeof *(arr), (allocr))
#define arr_last(arr) arr[arr_len(arr)-1]
#define arr_last_ptr(arr) arr_last_((arr), sizeof *(arr))
// one past last, or NULL if empty
#define arr_end(arr) arr_end_((arr), sizeof *(arr))
#define arr_foreach(arr, type, var) for (type *var = (arr), *var##_foreach_end = arr_end(arr); var != var##_foreach_end; ++var)
#define arr_foreach_reversed(arr, type, var) for (type *var = arr_last_ptr(arr), *var##_foreach_last = arr; var; var = var == var##_foreach_last ? NULL : (var-1))
#define arr_remove_last(arr) arr = arr_remove_last_(arr)
#define arr_remove_lasta(arr, allocr) arr = arr_remove_lasta_((arr), sizeof *(arr), (allocr))
#define arr_copya(out, in, allocr) do { assert(sizeof *(in) == sizeof *(out)); out = arr_copya_((out), (in), sizeof *(out), (allocr)); } while(0)

#if RUN_TESTS
// @TODO(eventually): more extensive test?
static void arr_test(void) {
	int *foos = NULL;
	for (int i = 0; i < 10; ++i) {
		arr_add(foos, i);
	}
	for (int i = 0; i < (int)arr_len(foos); ++i) {
		assert(foos[i] == i);
	}
	int lastx = -1;
	arr_foreach(foos, int, x) {
		assert(*x == lastx + 1);
		lastx = *x;
	}
	arr_clear(foos);
}
#endif

// string hash table. entries are zero initialized (toc stuff depends on this!)

static U64 str_hash(const char *s, size_t len) {
	U32 x = 0xabcdef01;
	U32 y = 0x31415926;
	U64 hash = 0;
	for (size_t i = 0; i < len; ++i) {
		hash += (U64)x * (unsigned char)(*s) + y;
		x = rand_u32(x);
		y = rand_u32(y);
		++s;
	}
	return hash;
}

static inline void str_hash_table_create(StrHashTable *t, size_t data_size, Allocator *allocr) {
	t->slots = NULL;
	t->data_size = data_size;
	t->nentries = 0;
	t->allocr = allocr;
	t->rand_seed = 0xabacabad;
}

static StrHashTableSlot **str_hash_table_slot_get(StrHashTableSlot **slots, const char *s, size_t s_len, size_t i) {
	StrHashTableSlot **slot;
	size_t slots_cap = arr_len(slots);
	while (1) {
		assert(i < slots_cap);
		slot = &slots[i];
		if (!*slot) break;
		if (s && (*slot)->str &&
			s_len == (*slot)->len && memcmp(s, (*slot)->str, s_len) == 0)
			break;
		i = (i+1) % slots_cap;
	}
	return slot;
}

static void str_hash_table_grow(StrHashTable *t) {
	size_t slots_cap = arr_len(t->slots);
	if (slots_cap <= 2 * t->nentries) {
		StrHashTableSlot **new_slots = NULL;
		size_t new_slots_cap = slots_cap * 2 + 10;
		arr_set_lena(new_slots, new_slots_cap, t->allocr);
		arr_zero(new_slots);
		arr_foreach(t->slots, StrHashTableSlotPtr, slotp) {
			StrHashTableSlot *slot = *slotp;
			if (slot) {
				U64 new_hash = str_hash(slot->str, slot->len);
				StrHashTableSlot **new_slot = str_hash_table_slot_get(new_slots, slot->str, slot->len, new_hash % new_slots_cap);
				*new_slot = slot;
			}
		}
		arr_cleara(t->slots, t->allocr);
		t->slots = new_slots;
	}
}

static inline size_t str_hash_table_slot_size(StrHashTable *t) {
	return sizeof(StrHashTableSlot) + ((t->data_size + sizeof(MaxAlign) - 1) / sizeof(MaxAlign)) * sizeof(MaxAlign);
}

static StrHashTableSlot *str_hash_table_insert_(StrHashTable *t, const char *str, size_t len) {
	str_hash_table_grow(t);
	size_t slots_cap = arr_len(t->slots);
	U64 hash = str_hash(str, len);
	StrHashTableSlot **slot = str_hash_table_slot_get(t->slots, str, len, hash % slots_cap);
	if (!*slot) {
		*slot = allocr_calloc(t->allocr, 1, str_hash_table_slot_size(t));
		(*slot)->str = str;
		(*slot)->len = len;
		++t->nentries;
	}
	return *slot;
}

// use this if you don't need the slot
static inline void *str_hash_table_insert(StrHashTable *t, const char *str, size_t len) {
	return str_hash_table_insert_(t, str, len)->data;
}

static StrHashTableSlot *str_hash_table_insert_anonymous_(StrHashTable *t) {
	str_hash_table_grow(t);
	size_t slots_cap = arr_len(t->slots);
	U32 slot_idx = (U32)((t->rand_seed = rand_u32(t->rand_seed)) % slots_cap);
	StrHashTableSlot **slot = str_hash_table_slot_get(t->slots, NULL, 0, slot_idx);
	if (!*slot) {
		*slot = allocr_calloc(t->allocr, 1, str_hash_table_slot_size(t));
		++t->nentries;
	}
	return *slot;
}

// use this if you don't need the slot
static inline void *str_hash_table_insert_anonymous(StrHashTable *t) {
	return str_hash_table_insert_anonymous_(t)->data;
}


static void str_hash_table_free(StrHashTable *t) {
	arr_foreach(t->slots, StrHashTableSlotPtr, slotp) {
		allocr_free(t->allocr, *slotp, str_hash_table_slot_size(t));
	}
	arr_cleara(t->slots, t->allocr);
}

static StrHashTableSlot *str_hash_table_get_(StrHashTable *t, const char *str, size_t len) {
	size_t nslots = arr_len(t->slots);
	if (!nslots) return NULL;
	size_t slot_index = str_hash(str, len) % arr_len(t->slots);
	return *str_hash_table_slot_get(t->slots, str, len, slot_index);
}

static inline void *str_hash_table_get(StrHashTable *t, const char *str, size_t len) {
	StrHashTableSlot *slot = str_hash_table_get_(t, str, len);
	if (!slot) return NULL;
	return slot->data;
}

static void str_hash_table_fprint(StrHashTable *t, FILE *out) {
	fprintf(out, "------- Hash table @ %p\n", (void *)t);
	arr_foreach(t->slots, StrHashTableSlotPtr, slotp) {
		StrHashTableSlot *slot = *slotp;
		if (slot) {
			fprintf(out, "| ");
			fwrite(slot->str, 1, slot->len, out);
			fprintf(out, "\n");
		}
	}
	fprintf(out, "------\n");
}

static void str_hash_table_print(StrHashTable *t) {
	str_hash_table_fprint(t, stdout);
}

#if RUN_TESTS
static void str_hash_table_test(void) {
	StrHashTable t;
	str_hash_table_create(&t, sizeof(int), NULL);
	int *p = str_hash_table_insert(&t, "Hello", 5);
	*p = 182;
	int *q = str_hash_table_insert(&t, "Hello", 5);
	assert(p == q);
	assert(*q == 182);
	*q = 112;
	int *r = str_hash_table_insert(&t, "Hellop", 6);
	assert(p != r);
	assert(*r == 0);
	*r = 999;
	int *s = str_hash_table_insert_anonymous(&t);
	assert(p != s && r != s);
	*s = 123;
	int *u = str_hash_table_get(&t, "Hello", 5);
	assert(p == u);
	assert(!str_hash_table_get(&t, "Hellopf", 7));
	str_hash_table_free(&t);
}
#endif
