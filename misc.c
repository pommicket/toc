/* 
Miscellaneous C functions which toc uses.

This is free and unencumbered software released into the public domain.
Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.
In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
For more information, please refer to <http://unlicense.org/>
*/


/* 
A better alternative to strncpy. dest is guaranteed to be a null-terminated string
after this function is run.
Returns the number of characters copied to dest, not including the null character.
destsz must be greater than 0.
*/
size_t str_copy(char *dest, size_t destsz, const char *src) {
	assert(destsz);
	if (!*src) {
		*dest = 0;
		return 0;
	}
	for (size_t i = 0; i < destsz-1; ++i) {
		*dest = *src;
		if (!*src) {
			*dest = 0;
			return i;
		}
		++src; ++dest;
	}
	*dest = 0;
	return destsz-1;
}

static inline U32 rand_u32(U32 seed) {
	U64 seed64 = (U64)seed;
	return (U32)((seed64 * 0x832f0fda4e1a8642 + 0x41d49cd5459a2ab4) >> 32);
}


static inline bool strs_equal(const char *a, const char *b) {
	return strcmp(a, b) == 0;
}
