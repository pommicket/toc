typedef uint32_t LineNo;

static void err_print(LineNo line, LineNo col, const char *fmt, ...) {
	/* TODO: Color */
	va_list args;
	fprintf(stderr, "Error at line %lu col %lu:\n", (unsigned long)line, (unsigned long)col);
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
}

static void *err_malloc(size_t size) {
	void *ret = malloc(size);
	if (!ret) {
		fprintf(stderr, "Error: Out of memory.\n");
		abort();
	}
	return ret;
}

static void *err_calloc(size_t n, size_t size) {
	void *ret = calloc(n, size);
	if (!ret) {
		fprintf(stderr, "Error: Out of memory.\n");
		abort();
	}
	return ret;
}

static void *err_realloc(void *data, size_t new_size) {
	void *ret = realloc(data, new_size);
	if (!ret) {
		fprintf(stderr, "Error: Out of memory.\n");
		abort();
	}
	return ret;
}

