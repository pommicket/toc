/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

static Location const LOCATION_NONE = {0};

/* for debugging */
static void fprint_location(FILE *out, Location location) {
	if (!location.first) {
		fprintf(out, "No location available.");
		return;
	}
	/* TODO: show end */
	char *str = location.first->pos.ctx->str + location.first->pos.start;
	char *newline = strchr(str, '\n');
	if (newline) *newline = 0;
	fprintf(out, "Line %ld: %s\n", (long)location.first->pos.line, str);
	if (newline) *newline = '\n';
}

static void print_location(Location location) {
	fprint_location(stdout, location);
}


static bool location_is_ctx_disabled(Location location) {
	return location.first && !location.first->pos.ctx->enabled;
}
