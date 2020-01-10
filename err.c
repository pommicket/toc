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

#define TEXT_INDICATOR_START "\x1b[96m"
#define TEXT_INDICATOR_END "\x1b[0m"
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

static void err_text_err(ErrCtx *ctx, const char *s) {
	if (ctx->color_enabled)
		err_fprint(TEXT_ERR_START "%s" TEXT_ERR_END, s);
	else
		err_fprint("%s", s);
}
static void err_text_warn(ErrCtx *ctx, const char *s) {
	if (ctx->color_enabled)
		err_fprint(TEXT_WARN_START "%s" TEXT_WARN_END, s);
	else
		err_fprint("%s", s);
}
static void err_text_info(ErrCtx *ctx, const char *s) {
	if (ctx->color_enabled)
		err_fprint(TEXT_INFO_START "%s" TEXT_INFO_END, s);
	else
		err_fprint("%s", s);
}
static void err_text_important(ErrCtx *ctx, const char *s) {
	if (ctx->color_enabled)
		err_fprint(TEXT_IMPORTANT_START "%s" TEXT_IMPORTANT_END, s);
	else
		err_fprint("%s", s);
}



static void err_vfprint(const char *fmt, va_list args) {
	vfprintf(stderr, fmt, args);
}

static void err_print_line_file(Location where) {
	if (where.start) {
		err_fprint(" at line %lu of %s:\n", (unsigned long)where.start->pos.line, where.start->pos.ctx->filename);
	} else if (where.simple_location) {
		err_fprint(" at line %lu of %s:\n", (unsigned long)where.simple_location->line, where.simple_location->ctx->filename);
	} else {
		err_fprint(":\n");

	}
	
}

static void err_print_header_(Location where) {
	SourcePos start_pos = where.start->pos;
	ErrCtx *ctx = start_pos.ctx;
	err_text_err(ctx, "error");
	err_print_line_file(where);
}

static void info_print_header_(Location where) {
	SourcePos start_pos = where.start->pos;
	ErrCtx *ctx = start_pos.ctx;
	err_text_info(ctx, "info");
	err_print_line_file(where);
}

static void warn_print_header_(Location where) {
	SourcePos start_pos = where.start->pos;
	ErrCtx *ctx = start_pos.ctx;
	err_text_warn(ctx, "warning");
	err_print_line_file(where);
}

static void err_print_pos_text(ErrCtx *ctx, U32 start_pos, U32 end_pos) {
	char *str = ctx->str;
	if (!str) {
		return;
	}
	
	char *start = str + start_pos;
	char *line_start = start;
	char *end = str + end_pos;

	/* go back to last newline / 0 byte */
	while (*line_start != '\0' && *line_start != '\n') --line_start;
	if (line_start < start) ++line_start;
		
	char *line_end = strchr(start, '\n');
	int has_newline = line_end != NULL;
	if (!has_newline)
		line_end = strchr(start, '\0');
	assert(line_end);
	err_fprint("\t");
	if (!line_start[0])
		err_fprint("<end of file>");
	else {
		/* write up to start of error */
		err_fwrite(line_start, 1, (size_t)(start - line_start));
		if (ctx->color_enabled)
			err_fprint(TEXT_INDICATOR_START);
		if (line_end < end) {
			/* write error part (only go to end of line) */
			err_fwrite(start, 1, (size_t)(line_end - start));
			if (ctx->color_enabled)
				err_fprint(TEXT_INDICATOR_END);
		} else {
			/* write error part */
			err_fwrite(start, 1, (size_t)(end - start));
			if (ctx->color_enabled)
				err_fprint(TEXT_INDICATOR_END);
			/* write rest of line */
			err_fwrite(end, 1, (size_t)(line_end - end));
		}
	}
	err_fprint("\n");
}

static void err_print_location_text(Location where) {
	if (where.start) {
		ErrCtx *ctx = where.start->pos.ctx;
		err_print_pos_text(ctx, where.start->pos.start, where.end[-1].pos.end);
	}
	
}

static void err_print_footer_(Location where) {
	ErrCtx *ctx = where.start->pos.ctx;
	err_fprint("\n"); 
	err_print_location_text(where);
	if (ctx) {
		arr_foreach(ctx->instance_stack, Location, inst) {
			err_fprint("While generating the instance of a function\n");
			err_print_location_text(*inst);
		}
	}
}

/* Write nicely-formatted errors to the error file */

static void err_vprint(Location where, const char *fmt, va_list args) {
	if (location_is_ctx_disabled(where)) return;
	if (where.start) {
		where.start->pos.ctx->have_errored = true;
	} else if (where.simple_location) {
		where.simple_location->ctx->have_errored = true;
	}
	err_print_header_(where);
	err_vfprint(fmt, args);
	err_print_footer_(where);
}


static void err_print_(
#if ERR_SHOW_SOURCE_LOCATION
					   int line, const char *file,
#endif
					   Location where, const char *fmt, ...) {
	va_list args;
#if ERR_SHOW_SOURCE_LOCATION
	if (location_is_ctx_disabled(where)) return;
	if (file)
		err_fprint("Generated by line %d of %s:\n", line, file);
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
	if (location_is_ctx_disabled(where)) return;
	va_start(args, fmt);
	info_print_header_(where);
	err_vfprint(fmt, args);
	err_print_footer_(where);
	va_end(args);
}

static void warn_print_(
#if ERR_SHOW_SOURCE_LOCATION
						int line, const char *file,
#endif
						Location where, const char *fmt, ...) {
	va_list args;
	if (location_is_ctx_disabled(where)) return;
#if ERR_SHOW_SOURCE_LOCATION
	if (file)
		err_fprint("Generated by line %d of %s:\n", line, file);
#endif
	va_start(args, fmt);
	warn_print_header_(where);
	err_vfprint(fmt, args);
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

