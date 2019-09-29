typedef struct Page {
	struct Page *next;
	size_t used;
	max_align_t data[];
} Page;

typedef struct {
	Page *first;
	Page *last;
	void **dyn; /* array of void*s for dynamic memory */
	size_t dyn_len;
	size_t dyn_cap;
} Allocator;

/* number of bytes a page hold, not including the header */
#define PAGE_SIZE (16384 - sizeof(Page))

static void allocr_create(Allocator *a) {
	a->first = a->last = NULL;
}

static void *allocr_malloc(Allocator *a, size_t bytes) {
	/* position in this page to return */
	size_t pos = PAGE_SIZE;
	if (a->last)
		pos = (a->last->used + sizeof(max_align_t) - 1) / sizeof(max_align_t);
	
	if (pos * sizeof(max_align_t) + bytes > PAGE_SIZE) {
		/* make a new page for this data */
		Page *page = err_malloc(sizeof *page + (bytes > PAGE_SIZE ? bytes : PAGE_SIZE));
		page->next = NULL;
		page->used = bytes;
		if (a->last)
			a->last->next = page;
		else
			a->first = page;
		a->last = page;
		return page->data;
	} else {
		a->last->used += bytes;
		return &a->last->data[pos];
	}
}

static void *allocr_calloc(Allocator *a, size_t bytes) {
	void *data = allocr_malloc(a, bytes);
	memset(data, 0, bytes);
	return data;
}

/* IMPORTANT: this can only be called with data which was originally allocated with allocr_realloc(a, NULL, x) */
static void *allocr_realloc(Allocator *a, void *data, size_t new_size) {
	return NULL;
}

static void allocr_free_all(Allocator *a) {
	for (Page *page = a->first; page;) {
		Page *next = page->next;
		free(page);
		page = next;
	}
	for (size_t i = 0; i < a->dyn_len; i++) {
		free(a->dyn[i]);
	}
}
