typedef uint32_t LineNo;
typedef struct {
	LineNo line;
	char *code;
	const char *filename;
} Location;

static bool location_after(Location a, Location b) { /* a is after b? */
	return a.code > b.code;
}
