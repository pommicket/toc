static int fpeekc(FILE *fp) {
	int c = getc(fp);
	if (c == EOF)
		return c;
	ungetc(c, fp);
	return c;
}

#define fnextc getc /* advance to the next character */

/* NOTE: Advances and returns # of characters advanced iff prefix is found. */
static int fhasprefix(FILE *fp, const char *prefix) {
	assert(*prefix);
	long start = ftell(fp);
	if (start == -1)
		return 0;
	const char *p = prefix;
	while (*p) {
		int c = getc(fp);
		if (c != *p) {
			/* wrong character / EOF */
			fseek(fp, start, SEEK_SET);
			return 0;
		}
		p++;
	}
	return (int)(p - prefix); /* length of prefix */
}
