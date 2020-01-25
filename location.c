/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

#define TEXT_INDICATOR_START "\x1b[96m"
#define TEXT_INDICATOR_END "\x1b[0m"

static void print_pos_highlight(FILE *out, ErrCtx *ctx, File *file, U32 start_pos, U32 end_pos) {
	char *str = file->contents;
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
	fprintf(out, "\t");
	if (!line_start[0])
		fprintf(out, "<end of file>");
	else {
		/* write up to start of error */
		fwrite(line_start, 1, (size_t)(start - line_start), out);
		if (ctx->color_enabled)
			fprintf(out, TEXT_INDICATOR_START);
		if (line_end < end) {
			/* write error part (only go to end of line) */
			fwrite(start, 1, (size_t)(line_end - start), out);
			if (ctx->color_enabled)
				fprintf(out, TEXT_INDICATOR_END);
		} else {
			/* write error part */
			fwrite(start, 1, (size_t)(end - start), out);
			if (ctx->color_enabled)
				fprintf(out, TEXT_INDICATOR_END);
			/* write rest of line */
			fwrite(end, 1, (size_t)(line_end - end), out);
		}
	}
	fprintf(out, "\n");
}

static void print_location_highlight(FILE *out, Location where) {
	if (where.start) {
		ErrCtx *ctx = where.file->ctx;
		print_pos_highlight(out, ctx, where.file, where.start->pos.start, where.end[-1].pos.end);
	}
	
}

/* for debugging */
static void fprint_location(FILE *out, Location location) {
	if (location.start) {
		fprintf(out, "Line %ld of %s: ", (long)location.start->pos.line, location.file->filename);
	} else {
		U32 line = location.simple_location.line;
		if (line)
			fprintf(out, "Line %lu of %s: ", (unsigned long)line, location.file->filename);
		else
			fprintf(out, "In file %s: ", location.file->filename);
		return;
	}
	print_location_highlight(out, location);
}

static void print_location(Location location) {
	fprint_location(stdout, location);
}
