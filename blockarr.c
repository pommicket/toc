/* 
A block array is an array of blocks of T. 
They ensure that pointers to values in the array are not invalidated
when something is added to the array.
*/

/* 
Note: the block size must be a power of 2, to use right shifting instead of division
(for optimization)!
*/
static void block_arr_create(BlockArr *arr, int lg_block_sz, size_t item_sz) {
	arr->blocks = NULL;
	arr->item_sz = item_sz;
	arr->lg_block_sz = lg_block_sz;
}

static void *block_arr_add(BlockArr *arr) {
	ArrBlock *last_block;
	last_block = arr_last(arr->blocks);
	if (arr->blocks == NULL ||
		(unsigned long)last_block->n >= (1UL << arr->lg_block_sz)) {
		ArrBlock *block;
		/* no blocks yet / ran out of blocks*/
		block = arr_add(&arr->blocks);
		block->n = 1;
		size_t bytes = arr->item_sz << arr->lg_block_sz;
		block->data = err_malloc(bytes);
		block->last = block->data;
		return block->data;
	} else {
		last_block->last = (char*)last_block->last + arr->item_sz;
		last_block->n++;
		return last_block->last;
	}
}

static inline void *block_arr_get(BlockArr *arr, size_t index) {
	size_t block_index = index >> arr->lg_block_sz;
	ArrBlock *block = &arr->blocks[block_index];
	return (char*)block->data + arr->item_sz * index;
}

static void block_arr_free(BlockArr *arr) {
	arr_foreach(arr->blocks, ArrBlock, block) {
		free(block->data);
	}
	arr_clear(&arr->blocks);
}

static void block_arr_test(void) {
	BlockArr a;
	int *ps[100];
	block_arr_create(&a, 3, sizeof(int));
	for (int i = 0; i < 100; i++) {
		int *p = block_arr_add(&a);
		*p = i;
		ps[i] = p;
	}
	for (int i = 0; i < 100; i++) {
	    assert(*ps[i] == i);
	}
	block_arr_free(&a);
}
