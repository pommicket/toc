/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

#define TEXT_ERR_START "\x1b[91m"
#define TEXT_ERR_END "\x1b[0m"

#define TEXT_INFO_START "\x1b[94m"
#define TEXT_INFO_END "\x1b[0m"
#define TEXT_WARN_START "\x1b[93m"
#define TEXT_WARN_END "\x1b[0m"
#define TEXT_IMPORTANT_START "\x1b[1m"
#define TEXT_IMPORTANT_END "\x1b[0m"

#define TEXT_INDICATOR_START "\x1b[96m"
#define TEXT_INDICATOR_END "\x1b[0m"

#if defined(TOC_DEBUG) && __STDC_VERSION__ >= 199901
#define ERR_SHOW_SOURCE_LOCATION 1
#endif


static void print_pos_highlight(FILE *out, ErrCtx *ctx, File *file, U32 start_pos, U32 end_pos) {
	char *str = file->contents;
	if (!str) {
		return;
	}
	
	char *start = str + start_pos;
	char *line_start = start;
	char *end = str + end_pos;

	// go back to last newline / 0 byte
	while (*line_start != '\0' && *line_start != '\n') --line_start;
	if (line_start < start) ++line_start;

	// skip space at start of line
	while (isspace(*line_start) && line_start < end)
		++line_start;
		
	char *line_end = strchr(start, '\n');
	int has_newline = line_end != NULL;
	if (!has_newline)
		line_end = strchr(start, '\0');
	assert(line_end);
	if (!line_start[0])
		fprintf(out, "<end of file>");
	else {
		// write up to start of error
		fwrite(line_start, 1, (size_t)(start - line_start), out);
		if (ctx->color_enabled)
			fprintf(out, TEXT_INDICATOR_START);
		if (line_end < end) {
			// write error part (only go to end of line)
			fwrite(start, 1, (size_t)(line_end - start), out);
			if (ctx->color_enabled)
				fprintf(out, TEXT_INDICATOR_END);
		} else {
			// write error part
			fwrite(start, 1, (size_t)(end - start), out);
			if (ctx->color_enabled)
				fprintf(out, TEXT_INDICATOR_END);
			// write rest of line
			fwrite(end, 1, (size_t)(line_end - end), out);
		}
	}
	fprintf(out, "\n");
}

static FILE *err_ctx_file(ErrCtx *ctx) {
	(void)ctx;
	return stderr;
}


static void err_fprint(ErrCtx *ctx, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vfprintf(err_ctx_file(ctx), fmt, args);
	va_end(args);
}

static void print_location_highlight(FILE *out, Location where) {
	assert(where.end >= where.start);
	File *f = where.file;
	ErrCtx *ctx = f->ctx;
	if (where.start == 0 && where.end == 0) { fprintf(out, "\n"); return; } // null location
	Token *first = &f->tokens[where.start];
	Token *last = &f->tokens[where.end-1];
	
	print_pos_highlight(out, ctx, f, first->pos.start, last->pos.end);
}

// for debugging
static void fprint_location(FILE *out, Location location) {
	File *f = location.file;
	if (location.start == 0 && location.end == 0) {
		fprintf(out, "In %s\n", f->filename);
	} else {
		fprintf(out, "Line %ld of %s: ", (long)f->tokens[location.start].pos.line, f->filename);
		print_location_highlight(out, location);
	}
}

static void print_location(Location location) {
	fprint_location(stdout, location);
}


static inline const char *ordinals(size_t x) {
	switch (x % 10) {
	case 1: return "st";
	case 2: return "nd";
	case 3: return "rd";
	default: return "th";
	}
}

static void err_text_err(ErrCtx *ctx, const char *s) {
	if (ctx->color_enabled)
		err_fprint(ctx, TEXT_ERR_START "%s" TEXT_ERR_END, s);
	else
		err_fprint(ctx, "%s", s);
}
static void err_text_warn(ErrCtx *ctx, const char *s) {
	if (ctx->color_enabled)
		err_fprint(ctx, TEXT_WARN_START "%s" TEXT_WARN_END, s);
	else
		err_fprint(ctx, "%s", s);
}
static void err_text_info(ErrCtx *ctx, const char *s) {
	if (ctx->color_enabled)
		err_fprint(ctx, TEXT_INFO_START "%s" TEXT_INFO_END, s);
	else
		err_fprint(ctx, "%s", s);
}
static void err_text_important(ErrCtx *ctx, const char *s) {
	if (ctx->color_enabled)
		err_fprint(ctx, TEXT_IMPORTANT_START "%s" TEXT_IMPORTANT_END, s);
	else
		err_fprint(ctx, "%s", s);
}



static void err_vfprint(ErrCtx *ctx, const char *fmt, va_list args) {
	vfprintf(err_ctx_file(ctx), fmt, args);
}

static void err_print_line_file(Location where) {
	File *f = where.file;
	ErrCtx *ctx = f->ctx;
	err_fprint(ctx, " at line %lu of %s:\n", (unsigned long)f->tokens[where.start].pos.line, f->filename);
}

static void err_print_header_(Location where) {
	ErrCtx *ctx = where.file->ctx;
	err_text_err(ctx, "error");
	if (where.start == 0 && where.end == 0) { err_fprint(ctx, ": "); return; }
	err_print_line_file(where);
}

static void info_print_header_(Location where) {
	ErrCtx *ctx = where.file->ctx;
	err_text_info(ctx, "info");
	if (where.start == 0 && where.end == 0) { err_fprint(ctx, ": "); return; }
	err_print_line_file(where);
}

static void warn_print_header_(Location where) {
	ErrCtx *ctx = where.file->ctx;
	err_text_warn(ctx, "warning");
	if (where.start == 0 && where.end == 0) { err_fprint(ctx, ": "); return; }
	err_print_line_file(where);
}

static void info_print(Location where, const char *fmt, ...);
static void err_print_footer_(Location where, bool show_ctx_stack) {
	ErrCtx *ctx = where.file->ctx;
	err_fprint(ctx, "\n\t");
	print_location_highlight(err_ctx_file(ctx), where);
	if (ctx && show_ctx_stack) {
		arr_foreach(ctx->instance_stack, Location, inst) {
			info_print(*inst, "While generating this instance of a function or struct:");
		}
	}
}

// Write nicely-formatted errors to the error file

static void err_vprint(Location where, const char *fmt, va_list args) {
	ErrCtx *ctx = where.file->ctx;
	if (!ctx->enabled) return;
	ctx->have_errored = true;
	err_print_header_(where);
	err_vfprint(ctx, fmt, args);
	err_print_footer_(where, true);
}


static void err_print_(
#if ERR_SHOW_SOURCE_LOCATION
	int line, const char *file,
#endif
	Location where, const char *fmt, ...) {
	
	va_list args;
#if ERR_SHOW_SOURCE_LOCATION
	ErrCtx *ctx = where.file->ctx;
	if (!ctx->enabled) return;
	if (file)
		err_fprint(ctx, "Generated by line %d of %s:\n", line, file);
#endif
	va_start(args, fmt);
	err_vprint(where, fmt, args);
	va_end(args);
}

#if ERR_SHOW_SOURCE_LOCATION
#define err_print(...) err_print_(__LINE__, __FILE__, __VA_ARGS__)
#else
#define err_print err_print_
#endif
	
static void info_print(Location where, const char *fmt, ...) {
	va_list args;
	ErrCtx *ctx = where.file->ctx;
	if (!ctx->enabled) return;
	va_start(args, fmt);
	info_print_header_(where);
	err_vfprint(ctx, fmt, args);
	err_print_footer_(where, false);
	va_end(args);
}

static void warn_print_(
#if ERR_SHOW_SOURCE_LOCATION
	int line, const char *file,
#endif
	Location where, const char *fmt, ...) {

	va_list args;
	ErrCtx *ctx = where.file->ctx;
	if (!ctx->enabled) return;
	if (where.file) {
		// check if there's a #no_warn directive
		U32 *no_warn_lines = where.file->no_warn_lines;
		if (bsearch_u32(no_warn_lines, arr_len(no_warn_lines), where.file->tokens[where.start].pos.line)) {
			// yes there is
			return;
		}
	}
#if ERR_SHOW_SOURCE_LOCATION
	if (file)
		err_fprint(ctx, "Generated by line %d of %s:\n", line, file);
#endif
	va_start(args, fmt);
	warn_print_header_(where);
	err_vfprint(ctx, fmt, args);
	err_print_footer_(where, true);
	va_end(args);
}

#if ERR_SHOW_SOURCE_LOCATION
#define warn_print(...) warn_print_(__LINE__, __FILE__, __VA_ARGS__)
#else
#define warn_print warn_print_
#endif

#ifdef MALLOC_TRACKER
typedef struct {
	int line;
	size_t nallocs;
	size_t amount;
} AllocTrackerEntry;
static AllocTrackerEntry eval_c[10000];
#endif

static void *err_malloc_(size_t size
#ifdef MALLOC_TRACKER
	, int line, const char *file
#endif
) {
	if (size == 0) return NULL;

	void *ret = malloc(size);
	if (!ret) {
		fprintf(stderr, "Error: Out of memory.\n");
		exit(EXIT_FAILURE);
	}
#ifdef MALLOC_TRACKER
	if (streq(file, "eval.c")) {
		AllocTrackerEntry *entry = &eval_c[line];
		entry->line = line;
		entry->amount += size;
		++entry->nallocs;
	}
#endif
#ifdef MALLOC_FILL
	memset(ret, MALLOC_FILL, size);
#endif
	return ret;
}


static void *err_calloc_(size_t n, size_t size
#ifdef MALLOC_TRACKER
	, int line, const char *file
#endif
) {
	if (n == 0 || size == 0) return NULL;
	void *ret = calloc(n, size);
	if (!ret) {
		fprintf(stderr, "Error: Out of memory.\n");
		exit(EXIT_FAILURE);
	}
#ifdef MALLOC_TRACKER
	if (streq(file, "eval.c")) {
		AllocTrackerEntry *entry = &eval_c[line];
		entry->amount += n * size;
	}
#endif
	return ret;
}

static void *err_realloc(void *data, size_t new_size) {
	if (new_size == 0) {
		free(data);
		return NULL;
	}
	void *ret = realloc(data, new_size);
	if (!ret) {
		fprintf(stderr, "Error: Out of memory.\n");
		exit(EXIT_FAILURE);
	}
	return ret;
}

