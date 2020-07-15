/*
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

#ifdef TOC_DEBUG
#define NO_ALLOCATOR 1 // useful for debugging; valgrind checks writing past the end of a malloc, but that won't work with an allocator
#endif
// number of bytes a page hold, not including the header
#define PAGE_BYTES (16384 - sizeof(Page))
#define PAGE_MAX_ALIGNS (PAGE_BYTES / sizeof(MaxAlign))

static void allocr_create(Allocator *a) {
	a->first = a->last = NULL;
}

static void *allocr_malloc(Allocator *a, size_t bytes) {
#if NO_ALLOCATOR
	(void)a;
	return err_malloc(bytes);
#else
	if (bytes == 0)
		return NULL;
	if (a == NULL)
		return err_malloc(bytes);
	// position in this page to return
	size_t pos = PAGE_MAX_ALIGNS;
	if (a->last)
		pos = a->last->used;
	size_t max_aligns = (bytes + sizeof(MaxAlign) - 1) / sizeof(MaxAlign);
	
	if (pos + max_aligns > PAGE_MAX_ALIGNS) {
		// make a new page for this data
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
	if (n == 0 || sz == 0) return NULL;
	if (a == NULL) return err_calloc(n, sz);
	// @OPTIM: use calloc
	size_t bytes = n * sz;
	void *data = allocr_malloc(a, bytes);
	memset(data, 0, bytes);
	return data;
}

static void allocr_free(Allocator *a, void *data, size_t size) {
#if NO_ALLOCATOR
	a = NULL;
#endif
	if (a == NULL) {
		free(data);
	}
	// @OPTIM
	(void)size;
}

// @OPTIM
static void *allocr_realloc(Allocator *a, void *data, size_t old_size, size_t new_size) {
#if NO_ALLOCATOR
	a = NULL;
#endif
	if (new_size == 0) {
		allocr_free(a, data, old_size);
		return NULL;
	}
	if (a == NULL) return err_realloc(data, new_size);
	void *ret = allocr_malloc(a, new_size);
	memcpy(ret, data, old_size);
	return ret;
}

static char *allocr_str_to_cstr(Allocator *a, String s) {
	size_t len = s.len;
	char *ret = allocr_malloc(a, len + 1);
	memcpy(ret, s.str, len);
	ret[len] = 0;
	return ret;
}

static void allocr_free_all(Allocator *a) {
	for (Page *page = a->first; page;) {
		Page *next = page->next;
		free(page);
		page = next;
	}
}

