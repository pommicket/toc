/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
/* #define ERR_EMACS 1 */

#ifndef USE_COLORED_TEXT
#if ERR_EMACS
#define USE_COLORED_TEXT 0
#else
#define USE_COLORED_TEXT 1
#endif
#endif

#define TEXT_ERR_START "\x1b[91m"
#define TEXT_ERR_END "\x1b[0m"

#define TEXT_INFO_START "\x1b[94m"
#define TEXT_INFO_END "\x1b[0m"
#define TEXT_WARN_START "\x1b[93m"
#define TEXT_WARN_END "\x1b[0m"
#define TEXT_IMPORTANT_START "\x1b[1m"
#define TEXT_IMPORTANT_END "\x1b[0m"

#if defined(TOC_DEBUG) && __STDC_VERSION__ >= 199901
#define ERR_SHOW_SOURCE_LOCATION 1
#endif

static inline const char *ordinals(size_t x) {
	switch (x % 10) {
	case 1: return "st";
	case 2: return "nd";
	case 3: return "rd";
	default: return "th";
	}
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
	ErrCtx *ctx = where.file->ctx;
	if (where.start) {
		err_fprint(ctx, " at line %lu of %s:\n", (unsigned long)where.start->pos.line, where.file->filename);
	} else {
		U32 line = where.simple_location.line; 
		if (line)
			err_fprint(ctx, " at line %lu of %s:\n", (unsigned long)line, where.file->filename);
		else
			err_fprint(ctx, ":\n");
	}
	
}

static void err_print_header_(Location where) {
	ErrCtx *ctx = where.file->ctx;
	err_text_err(ctx, "error");
	err_print_line_file(where);
}

static void info_print_header_(Location where) {
	ErrCtx *ctx = where.file->ctx;
	err_text_info(ctx, "info");
	err_print_line_file(where);
}

static void warn_print_header_(Location where) {
	ErrCtx *ctx = where.file->ctx;
	err_text_warn(ctx, "warning");
	err_print_line_file(where);
}


static void err_print_footer_(Location where) {
	ErrCtx *ctx = where.file->ctx;
	err_fprint(ctx, "\n"); 
	print_location_highlight(err_ctx_file(ctx), where);
	if (ctx) {
		arr_foreach(ctx->instance_stack, Location, inst) {
			err_fprint(ctx, "While generating the instance of a function\n");
		    print_location_highlight(err_ctx_file(ctx), *inst);
		}
	}
}

/* Write nicely-formatted errors to the error file */

static void err_vprint(Location where, const char *fmt, va_list args) {
	ErrCtx *ctx = where.file->ctx;
	if (!ctx->enabled) return;
	ctx->have_errored = true;
	err_print_header_(where);
	err_vfprint(ctx, fmt, args);
	err_print_footer_(where);
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
	err_print_footer_(where);
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
#if ERR_SHOW_SOURCE_LOCATION
	if (file)
		err_fprint(ctx, "Generated by line %d of %s:\n", line, file);
#endif
	va_start(args, fmt);
	warn_print_header_(where);
	err_vfprint(ctx, fmt, args);
	err_print_footer_(where);
	va_end(args);
}

#if ERR_SHOW_SOURCE_LOCATION
#define warn_print(...) warn_print_(__LINE__, __FILE__, __VA_ARGS__)
#else
#define warn_print warn_print_
#endif

static void *err_malloc(size_t size) {
	if (size == 0) return NULL;
	void *ret = malloc(size);
	if (!ret) {
		fprintf(stderr, "Error: Out of memory.\n");
		abort();
	}
	return ret;
}

static void *err_calloc(size_t n, size_t size) {
	if (n == 0 || size == 0) return NULL;
	void *ret = calloc(n, size);
	if (!ret) {
		fprintf(stderr, "Error: Out of memory.\n");
		abort();
	}
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
		abort();
	}
	return ret;
}

