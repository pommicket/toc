typedef uint32_t LineNo;

static void err_print(LineNo line, LineNo col, const char *fmt, ...) {
	/* TODO: Color */
	va_list args;
	fprintf(stderr, "Error at line %lu col %lu:\n", (unsigned long)line, (unsigned long)col);
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
}
