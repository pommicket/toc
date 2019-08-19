/* 
A block array is an array of blocks of T. 
They ensure that pointers to values in the array are not invalidated
when something is added to the array.
*/

typedef struct {
	void *data;
	size_t n; /* number of things in this block so far */
    void *last; /* last one of them */
} Block;

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
void block_arr_create(BlockArr *arr, int lg_block_sz, size_t item_sz) {
	arr_create(&arr->blocks, sizeof(Block));
	arr->item_sz = item_sz;
	arr->lg_block_sz = lg_block_sz;
}

void *block_arr_add(BlockArr *arr) {
	if (arr->blocks.data == NULL ||
		(unsigned long)((Block*)arr->blocks.last)->n >= (1UL << arr->lg_block_sz)) {
		Block *block;
		/* no blocks yet / ran out of blocks*/
		block = arr_add(&arr->blocks);
		block->data = malloc(arr->item_sz << arr->lg_block_sz);
		block->n = 1;
		block->last = block->data;
		return block->data;
	} else {
	Block *last_block;
		last_block = arr->blocks.last;
		last_block->last = (char*)last_block->last + arr->item_sz;
		return last_block->last;
	}
}

/* Don't need this yet. */
/* void *block_arr_get(BlockArr *arr, size_t index) { */
	
/* } */

void block_arr_free(BlockArr *arr) {
	arr_foreach(&arr->blocks, Block, block) {
		free(block->data);
	}
	arr_free(&arr->blocks);
}
