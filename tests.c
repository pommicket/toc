
static void allocr_test(void) {
	Allocator a;
	allocr_create(&a);
	for (int x = 1000; x <= 8000; x += 1000) {
		int nfoos = x;
		int *foos = allocr_malloc(&a, (size_t)nfoos * sizeof(int));

		for (int i = 0; i < nfoos; i++)
			foos[i] = i;
		for (int i = 0; i < nfoos; i++)
			assert(foos[i] == i);
		int nbars = x;
		int *bars = allocr_calloc(&a, (size_t)nbars, sizeof(int));
		for (int i = 0; i < nbars; i++)
			assert(bars[i] == 0);
		for (int i = 0; i < nbars; i++)
			bars[i] = i;
		for (int i = 0; i < nbars; i++)
			assert(bars[i] == i);
	}
	allocr_free_all(&a);
}

static void test_all(void) {
	allocr_test();
	arr_test();
	block_arr_test();
	idents_test();
}
