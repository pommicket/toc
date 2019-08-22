#define USE_COLORED_TEXT 1

#if USE_COLORED_TEXT
#define TEXT_ERROR(x) "\x1b[91m" x "\x1b[0m"
#define TEXT_INFO(x) "\x1b[94m" x "\x1b[0m"
#define TEXT_IMPORTANT(x) "\x1b[1m" x "\x1b[0m"
#else
#define TEXT_ERROR(x) x
#define TEXT_INFO(x) x
#define TEXT_IMPORTANT(x) x
#endif

typedef uint32_t LineNo;

typedef struct {
	LineNo line;
	char *code;
} Location;

/* file name of file being processed */
static const char *err_filename;

/* Write directly to the error file */
static void err_fwrite(const void *data, size_t size, size_t n) {
	fwrite(data, size, n, stderr);
}

static void err_fprint(const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
}

static void err_vfprint(const char *fmt, va_list args) {
	vfprintf(stderr, fmt, args);
}

static void err_print_header_(LineNo line) {
	err_fprint(TEXT_ERROR("error:") " at line %lu of %s:\n", (unsigned long)line, err_filename);
}

static void info_print_header_(LineNo line) {
	err_fprint(TEXT_INFO("info:") " at line %lu of %s:\n", (unsigned long)line, err_filename);
}

static void err_print_footer_(const char *context) {
	err_fprint("\n\there --> ");
	const char *end = strchr(context, '\n');
	int has_newline = end != NULL;
	if (!has_newline)
		end = strchr(context, '\0');
	assert(end);
	err_fwrite(context, 1, (size_t)(end - context));
	if (!has_newline)
		err_fprint("<end of file>");
	err_fprint("\n");
}

/* Write nicely-formatted errors to the error file */


static void err_vprint(Location where, const char *fmt, va_list args) {
	err_print_header_(where.line);
	err_vfprint(fmt, args);
	err_print_footer_(where.code);
}

static void err_print(Location where, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	err_vprint(where, fmt, args);
	va_end(args);
}

static void info_print(Location where, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	info_print_header_(where.line);
	err_vfprint(fmt, args);
	err_print_footer_(where.code);
	va_end(args);
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

