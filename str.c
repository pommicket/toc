/*
  Copyright (C) 2019 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
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
	dest[destsz-1] = 0;
	return destsz-1;
}
