/* 
A block array is an array of blocks of T. 
They ensure that pointers to values in the array are not invalidated
when something is added to the array.
*/

typedef struct {
	void *data;
	size_t n; /* number of things in this block so far */
    void *last; /* last one of them */
} ArrBlock;

typedef struct {
	size_t item_sz;
	int lg_block_sz;
	/* NOTE: dynamic array tends to over-allocate, so we're using our own */
	Array blocks;
} BlockArr;

/* 
Note: the block size must be a power of 2, to use right shifting instead of division
(for optimization)!
*/
static void block_arr_create(BlockArr *arr, int lg_block_sz, size_t item_sz) {
	arr_create(&arr->blocks, sizeof(ArrBlock));
	arr->item_sz = item_sz;
	arr->lg_block_sz = lg_block_sz;
}

static void *block_arr_add(BlockArr *arr) {
	ArrBlock *last_block;
	last_block = arr_last(&arr->blocks);
		
	if (arr->blocks.data == NULL ||
		(unsigned long)last_block->n >= (1UL << arr->lg_block_sz)) {
		ArrBlock *block;
		/* no blocks yet / ran out of blocks*/
		block = arr_add(&arr->blocks);
		block->data = malloc(arr->item_sz << arr->lg_block_sz);
		block->n = 1;
		block->last = block->data;
		return block->data;
	} else {
		last_block->last = (char*)last_block->last + arr->item_sz;
		return last_block->last;
	}
}

static inline void *block_arr_get(BlockArr *arr, size_t index) {
	size_t block_index = index >> arr->lg_block_sz;
	ArrBlock *block = (ArrBlock*)arr->blocks.data + block_index;
	return (char*)block->data + arr->item_sz * index;
}

static void block_arr_free(BlockArr *arr) {
	arr_foreach(&arr->blocks, ArrBlock, block) {
		free(block->data);
	}
	arr_free(&arr->blocks);
}
