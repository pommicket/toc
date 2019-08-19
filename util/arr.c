typedef struct {
	void *data;
	void *last;
	size_t len;
	size_t cap;
	size_t item_sz;
} Array;

void arr_create(Array *arr, size_t item_sz) {
	arr->len = arr->cap = 0;
	arr->item_sz = item_sz;
	arr->data = NULL;
	arr->last = NULL;
}

void arr_reserve(Array *arr, size_t n) {
	arr->cap = n;
	arr->data = realloc(arr->data, arr->item_sz * arr->cap);
	arr->last = (void*)((char*)arr->data + arr->item_sz * (arr->len - 1));
}

void *arr_add(Array *arr) {
	if (arr->len >= arr->cap) {
		arr_reserve(arr, (arr->cap + 2) * 2);
	}
	arr->len++;
	arr->last = (char*)arr->last + arr->item_sz;
	void *item = arr->last;
	return item;
}

void arr_free(Array *arr) {
	free(arr->data);
}

void arr_clear(Array *arr) {
	free(arr->data);
	arr->len = arr->cap = 0;
	arr->data = NULL;
	arr->last = NULL;
}

#define arr_foreach(arr, type, var) for (type *var = (arr)->data; var; var == (arr)->last ? var = NULL : var++)
