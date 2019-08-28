/* 
A better alternative to strncpy. dest is guaranteed to be a null-terminated string
after this function is run.
Returns the number of characters copied to dest, not including the null character.
destsz must be greater than 0.
*/
size_t str_copy(char *dest, size_t destsz, const char *src) {
	assert(destsz);
	for (size_t i = 0; i < destsz-1; i++) {
		if (!*src) {
			*dest = 0;
			return i;
		}
		*dest++ = *src++;
	}
	dest[destsz] = 0;
	return destsz-1;
}
