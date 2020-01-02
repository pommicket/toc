/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static bool location_after(Location a, Location b) { /* a is after b? */
	assert(a.ctx == b.ctx);
	return a.pos > b.pos;
}

/* for debugging */
static void fprint_location(FILE *out, Location location) {
	char *str = location.ctx->str + location.pos;
	char *newline = strchr(str, '\n');
	if (newline) *newline = 0;
	fprintf(out, "Line %ld: %s\n", (long)location.line, str);
	if (newline) *newline = '\n';
}

static void print_location(Location location) {
	fprint_location(stdout, location);
}
