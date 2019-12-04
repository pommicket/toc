static bool location_after(Location a, Location b) { /* a is after b? */
	assert(a.ctx == b.ctx);
	return a.code > b.code;
}

/* for debugging */
static void fprint_location(FILE *out, Location location) {
	char *newline = strchr(location.code, '\n');
	if (newline) *newline = 0;
	fprintf(out, "Line %ld: %s\n", (long)location.line, location.code);
	if (newline) *newline = '\n';
}

static void print_location(Location location) {
	fprint_location(stdout, location);
}
