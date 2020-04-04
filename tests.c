/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

static void allocr_test(void) {
	Allocator a;
	allocr_create(&a);
	for (int x = 1000; x <= 8000; x += 1000) {
		int nfoos = x;
		int *foos = allocr_malloc(&a, (size_t)nfoos * sizeof(int));
		int i;
		for (i = 0; i < nfoos; ++i)
			foos[i] = i;
		for (i = 0; i < nfoos; ++i)
			assert(foos[i] == i);
		int nbars = x;
		int *bars = allocr_calloc(&a, (size_t)nbars, sizeof(int));
		for (i = 0; i < nbars; ++i)
			assert(bars[i] == 0);
		for (i = 0; i < nbars; ++i)
			bars[i] = i;
		for (i = 0; i < nbars; ++i)
			assert(bars[i] == i);
	}
	allocr_free_all(&a);
}

static void test_all(void) {
	allocr_test();
	arr_test();
	str_hash_table_test();
	idents_test();
}
