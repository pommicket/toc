static void *err_malloc(size_t bytes);
static void *err_calloc(size_t n, size_t sz);
static void *err_realloc(void *prev, size_t new_size);
#define NO_ALLOCATOR 1 /* useful for debugging; valgrind (maybe) checks writing past the end of a malloc, but that won't work with an allocator */
/* number of bytes a page hold, not including the header */
#define PAGE_BYTES (16384 - sizeof(Page))
#define PAGE_MAX_ALIGNS (PAGE_BYTES / sizeof(MaxAlign))

static void allocr_create(Allocator *a) {
	a->first = a->last = NULL;
}

static void *allocr_malloc(Allocator *a, size_t bytes) {
	assert(bytes);
#if NO_ALLOCATOR
	(void)a;
	return err_malloc(bytes);
#else
	/* position in this page to return */
	size_t pos = PAGE_MAX_ALIGNS;
	if (a->last)
		pos = a->last->used;
	size_t max_aligns = (bytes + sizeof(MaxAlign) - 1) / sizeof(MaxAlign);
	
	if (pos + max_aligns > PAGE_MAX_ALIGNS) {
		/* make a new page for this data */
		Page *page = err_malloc(sizeof *page + (bytes > PAGE_BYTES ? bytes : PAGE_BYTES));
		page->next = NULL;
		page->used = max_aligns;
		if (a->last)
			a->last->next = page;
		else
			a->first = page;
		a->last = page;
		return page->data;
	} else {
		a->last->used += max_aligns;
		return &a->last->data[pos];
	}
#endif
}

static void *allocr_calloc(Allocator *a, size_t n, size_t sz) {
#if NO_ALLOCATOR
	a = NULL;
#endif
	if (a == NULL) return err_calloc(n, sz);
	/* OPTIM: use calloc */
	size_t bytes = n * sz;
	void *data = allocr_malloc(a, bytes);
	memset(data, 0, bytes);
	return data;
}

/* OPTIM */
static void *allocr_realloc(Allocator *a, void *data, size_t old_size, size_t new_size) {
#if NO_ALLOCATOR
	a = NULL;
#endif
	if (a == NULL) return err_realloc(data, new_size);
    void *ret = allocr_malloc(a, new_size);
	memcpy(ret, data, old_size);
	return ret;
}

static void allocr_free(Allocator *a, void *data, size_t size) {
#if NO_ALLOCATOR
	a = NULL;
#endif
	if (a == NULL) {
		free(data);
	}
	/* OPTIM */
	(void)size;
}

static void allocr_free_all(Allocator *a) {
	for (Page *page = a->first; page;) {
		Page *next = page->next;
		free(page);
		page = next;
	}
}
