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
		ArrHeader *hdr = err_malloc(item_sz * n + sizeof(ArrHeader));
		hdr->len = 0;
		hdr->cap = n;
		*arr = hdr->data;
	} else {
		ArrHeader *hdr = arr_hdr(*arr);
		hdr->cap = n;
		hdr = realloc(hdr, item_sz * n + sizeof(ArrHeader));
		if (hdr->len > hdr->cap) hdr->len = hdr->cap;
		*arr = hdr->data;
	}
		
}

static void *arr_add_(void **arr, size_t item_sz) {
	ArrHeader *hdr;
    if (*arr == NULL) {
		arr_resv_(*arr, 10, item_sz);
		hdr = arr_hdr(*arr);
	} else {
		hdr = arr_hdr(*arr);
		if (hdr->len >= hdr->cap) {
			arr_resv_(*arr, hdr->len * 2, item_sz);
			hdr = arr_hdr(*arr);
		}
	}
	return &(((char *)hdr->data)[(hdr->len++) * item_sz]);
}

#define arr_add(arr) arr_add_((void **)arr, sizeof **arr)
