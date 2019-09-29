typedef struct Page {
	struct Page *next;
	size_t used;
	max_align_t data[];
} Page;

typedef struct {
	Page *first;
	Page *last;
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
	if (!data) {
		Page *page = err_malloc(sizeof *page + new_size);
		page->used = PAGE_SIZE;	/* pretend we used all of this page */
		page->next = NULL;
		if (a->last)
			a->last->next = page;
		else
			a->first = page;
		a->last = page;
		return page->data;
	} else {
		Page *page = (Page *)((char *)data - offsetof(Page, data));
		page = err_realloc(page, sizeof *page + new_size);
		return page->data;
	}
}

static void allocr_free_all(Allocator *a) {
	for (Page *page = a->first; page;) {
		Page *next = page->next;
		free(page);
		page = next;
	}
}

static void allocr_test(void) {
	Allocator a;
	allocr_create(&a);
	for (int x = 1000; x <= 8000; x += 1000) {
		size_t nfoos = x;
		int *foos = allocr_malloc(&a, nfoos * sizeof(int));

		for (size_t i = 0; i < nfoos; i++)
			foos[i] = (int)i;
		for (size_t i = 0; i < nfoos; i++)
			assert(foos[i] == (int)i);
		size_t nbars = x;
		int *bars = allocr_calloc(&a, nbars * sizeof(int));
		for (size_t i = 0; i < nbars; i++)
			assert(bars[i] == 0);
		for (size_t i = 0; i < nbars; i++)
			bars[i] = (int)i;
		for (size_t i = 0; i < nbars; i++)
			assert(bars[i] == (int)i);
	}
	
	allocr_free_all(&a);
}
