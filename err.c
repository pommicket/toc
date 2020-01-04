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

#if USE_COLORED_TEXT
#define TEXT_ERROR(x) "\x1b[91m" x "\x1b[0m"
#define TEXT_INFO(x) "\x1b[94m" x "\x1b[0m"
#define TEXT_WARN(x) "\x1b[93m" x "\x1b[0m"
#define TEXT_IMPORTANT(x) "\x1b[1m" x "\x1b[0m"
#else
#define TEXT_ERROR(x) x
#define TEXT_INFO(x) x
#define TEXT_WARN(x) x
#define TEXT_IMPORTANT(x) x
#endif

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

static void err_vfprint(const char *fmt, va_list args) {
	vfprintf(stderr, fmt, args);
}

static void err_print_header_(Location where) {
	if (!where.ctx)
		err_fprint(TEXT_ERROR("error") ":\n");
	else {
#if ERR_EMACS
		err_fprint("%s:%lu: " TEXT_ERROR("error") ":\n", where.ctx->filename, (unsigned long)where.line);
#else
		err_fprint(TEXT_ERROR("error") " at line %lu of %s:\n", (unsigned long)where.line, where.ctx->filename);
#endif
	}
}

static void info_print_header_(Location where) {
	if (!where.ctx)
		err_fprint(TEXT_INFO("info") ":\n");
	else {
#if ERR_EMACS
		err_fprint("%s:%lu: " TEXT_INFO("info") ":\n", where.ctx->filename, (unsigned long)where.line);
#else
		err_fprint(TEXT_INFO("info") " at line %lu of %s:\n", (unsigned long)where.line, where.ctx->filename);
#endif
	}
}

static void warn_print_header_(Location where) {
	if (!where.ctx)
		err_fprint(TEXT_WARN("warning") ":\n");
	else {
#if ERR_EMACS
		err_fprint("%s:%lu: " TEXT_WARN("warning") ":\n", where.ctx->filename, (unsigned long)where.line);
#else
		err_fprint(TEXT_WARN("warning") " at line %lu of %s:\n", (unsigned long)where.line, where.ctx->filename);
#endif
	}
}

static void err_print_location_text(Location where) {
	if (where.ctx) {
		const char *text = where.ctx->str + where.pos;
		const char *end = strchr(text, '\n');
		int has_newline = end != NULL;
		if (!has_newline)
			end = strchr(text, '\0');
		assert(end);
		err_fprint("\there: --> ");
		if (!text[0])
			err_fprint("<end of file>");
		else
			err_fwrite(text, 1, (size_t)(end - text));
		err_fprint("\n");
	} else {
		err_fprint("\t<no location available>");
	}
	
}

static void err_print_footer_(Location where) {
	ErrCtx *ctx = where.ctx;
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
	if (!where.ctx->enabled) return;
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
	if (where.ctx && !where.ctx->enabled) return;
#if ERR_SHOW_SOURCE_LOCATION
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
	if (where.ctx && !where.ctx->enabled) return;
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
	if (where.ctx && !where.ctx->enabled) return;
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

