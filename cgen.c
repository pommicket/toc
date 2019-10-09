typedef struct {
	FILE *outc;
	long ident_counter;
} CGenerator;

static void cgen_create(CGenerator *g, FILE *out) {
	g->outc = out;
	g->ident_counter = 0;
}

static inline FILE *cgen_writing_to(CGenerator *g) {
	return g->outc;	/* for now */
}

static void cgen_write(CGenerator *g, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vfprintf(cgen_writing_to(g), fmt, args);
	va_end(args);
}

static void cgen_decls_file(CGenerator *g, ParsedFile *f);

static void cgen_file(CGenerator *g, ParsedFile *f) {
	cgen_decls_file(g, f);
	cgen_write(g, "/* code */\n");
}
