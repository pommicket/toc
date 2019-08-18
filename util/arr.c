#define arr_declaration(arr_type, type, prefix) typedef struct {	\
		type *data;													\
		size_t cap;													\
		size_t len;													\
	} arr_type;														\
	static void prefix##create(arr_type *arr) {						\
		arr->data = NULL;											\
		arr->cap = 0;												\
		arr->len = 0;												\
	}																\
	static void prefix##reserve(arr_type *arr, size_t n) {			\
		arr->data = err_realloc(arr->data, n * sizeof(*arr->data)); \
		arr->cap = n;												\
	}																\
	static void prefix##add(arr_type *arr, type *item) {			\
		if (arr->len >= arr->cap) {									\
			prefix##reserve(arr, 2 * arr->len + 2);					\
		}															\
	    arr->data[arr->len++] = *item;								\
	}																\
	static void prefix##clear(arr_type *arr) {						\
		free(arr->data);											\
		arr->data = NULL;											\
		arr->cap = 0;												\
		arr->len = 0;												\
	}

#define arr_foreach(arr, type, var) for (type *var = (arr).data, *arr_iterate_last_ = (arr).data + ((arr).len - 1); var; (var == arr_iterate_last_) ? var = NULL : var++)
