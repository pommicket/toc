
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

static void *allocr_calloc(Allocator *a, size_t n, size_t sz) {
	/* OPTIM: use calloc */
	size_t bytes = n * sz;
	void *data = allocr_malloc(a, bytes);
	memset(data, 0, bytes);
	return data;
}

/* OPTIM */
static void *allocr_realloc(Allocator *a, void *data, size_t old_size, size_t new_size) {
    void *ret = allocr_malloc(a, new_size);
	memcpy(ret, data, old_size);
	return ret;
}

static void allocr_free_all(Allocator *a) {
	for (Page *page = a->first; page;) {
		Page *next = page->next;
		free(page);
		page = next;
	}
}
