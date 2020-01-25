/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

static Location const LOCATION_NONE = {0};

static void err_print_location_text(Location where);
/* for debugging */
static void fprint_location(FILE *out, Location location) {
	if (!location.start) {
		if (location.simple_location) {
			fprintf(out, "Line %lu of %s", (unsigned long)location.simple_location->line, location.simple_location->ctx->filename);
		} else {
			fprintf(out, "No location available.");
		}
		return;
	}
	fprintf(out, "Line %ld: ", (long)location.start->pos.line);
	err_print_location_text(location);
}

static void print_location(Location location) {
	fprint_location(stdout, location);
}


static bool location_is_ctx_disabled(Location location) {
	if (!location.start) return true;
	return !location.start->pos.ctx->enabled;
}
