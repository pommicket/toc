
static void arr_create(Array *arr, size_t item_sz) {
	arr->len = arr->cap = 0;
	arr->item_sz = item_sz;
	arr->data = NULL;
}

static inline void arr_reserve(Array *arr, size_t n) {
	arr->cap = n;
	arr->data = err_realloc(arr->data, arr->item_sz * arr->cap);
}

static inline void arr_reservea(Array *arr, size_t n, Allocator *a) {
	arr->cap = n;
	arr->data = allocr_realloc(a, arr->data, arr->item_sz * arr->cap);
}

/* like arr_reserve, but sets the length of the array too */
static inline void arr_set_len(Array *arr, size_t n) {
	arr_reserve(arr, n);
	arr->len = n;
}

static inline void arr_set_lena(Array *arr, size_t n, Allocator *a) {
	arr_reservea(arr, n, a);
	arr->len = n;
}

static inline void *arr_last(Array *arr) {
	if (arr->data)
		return (void*)((char*)arr->data + arr->item_sz * (arr->len - 1));
	else
		return NULL;
}

static void *arr_add(Array *arr) {
	if (arr->len >= arr->cap) {
		arr_reserve(arr, (arr->cap + 1) * 2);
	}
	arr->len++;
	return (void*)((char*)arr->data + arr->item_sz * (arr->len - 1));
}

static void *arr_adda(Array *arr, Allocator *a) {
	if (arr->len >= arr->cap) {
		arr_reservea(arr, (arr->cap + 1) * 2, a);
	}
	arr->len++;
	return (void*)((char*)arr->data + arr->item_sz * (arr->len - 1));
}

static void arr_clear(Array *arr) {
	free(arr->data);
	arr->len = arr->cap = 0;
	arr->data = NULL;
}

static void arr_remove_last(Array *arr) {
	/* OPTIM (memory): Shorten array. */
	arr->len--;
}

static void arr_free(Array *arr) {
	free(arr->data);
}

/* NOTE: this will not work if type is a pointer! */
#define arr_foreach(arr, type, var) for (type *var = (arr)->data, *var##_foreach_last = arr_last(arr); var; var == var##_foreach_last ? var = NULL : var++)
#define arr_foreach_reverse(arr, type, var) for (type *var = arr_last(arr); var; var == (arr)->data ? var = NULL : var--)