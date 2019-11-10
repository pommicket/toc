static U32 rand_u32(U32 seed) {
	U64 seed64 = (U64)seed;
	return (U32)((seed64 * 0x832f0fda4e1a8642 + 0x41d49cd5459a2ab4) >> 32);
}
