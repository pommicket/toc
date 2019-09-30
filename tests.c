
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

	int nfoos1 = 5;
	int *foos = allocr_realloc(&a, NULL, (size_t)nfoos1 * sizeof(int));
	for (int i = 0; i < nfoos1; i++)
		foos[i] = i;
	for (int i = 0; i < nfoos1; i++)
		assert(foos[i] == i);

	int nfoos2 = 10;
	foos = allocr_realloc(&a, foos, (size_t)nfoos2 * sizeof(int));
	for (int i = nfoos1; i < nfoos2; i++)
		foos[i] = i;
	for (int i = 0; i < nfoos2; i++)
		assert(foos[i] == i);
	
	Array arr;
	arr_create(&arr, sizeof(int));
	int n = 1000;
	for (int i = 0; i < n; i++) {
		int *p = arr_adda(&arr, &a);
		*p = i;
	}
	int *arr_data = arr.data;
	for (int i = 0; i < n; i++) {
		assert(arr_data[i] == i);
	}
	
	allocr_free_all(&a);
}

static void test_all(void) {
	allocr_test();
}
