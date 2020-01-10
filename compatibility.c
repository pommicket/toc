/*
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef float F32;
typedef double F64;
typedef uint32_t U32;
typedef uint64_t U64;

int main(void) {
	int *p;
	memset(&p, 0, sizeof p);
	if (p) {
		fprintf(stderr, "You cannot run toc. Sorry.\n");
		return EXIT_FAILURE;
	}


	FILE *f;
	f = tmpfile();
	/* endianness test */
	putc(0x12, f);
	putc(0x34, f);
	putc(0x56, f);
	putc(0x78, f);

	if (sizeof(float) != 4 || sizeof(double) != 8) {
		fprintf(stderr, "You may experience some problems with toc (sizeof(double) and sizeof(float) are strange).");
		goto unusual;
	} else {
		F32 flt = -12.3456f;
		fwrite(&flt, sizeof flt, 1, f);
		F64 dbl = -12.3456;
		fwrite(&dbl, sizeof dbl, 1, f);
	}
	fseek(f, 0L, SEEK_SET);
	U32 num;
	fread(&num, sizeof num, 1, f);
	if (num != 0x78563412) {
		/* not little endian */
		goto unusual;
	}
	U32 flt_rep;
	fread(&flt_rep, sizeof flt_rep, 1, f);
	if (flt_rep != 0xc1458794) goto unusual;
	U64 dbl_rep;
	fread(&dbl_rep, sizeof dbl_rep, 1, f);
	if (dbl_rep != 0xc028b0f27bb2fec5) goto unusual;
	return 0;
 unusual:
	printf("-DBINFILE_PORTABLE");
	return 0;
}
