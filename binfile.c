#define BINFILE_PORTABLE 1

static inline void write_u8(FILE *fp, U8 u8) {
	putc(u8, fp);
}

/* 
   note: a bit of testing seems to reveal that the portable versions for u16/32 are faster than fwrite
   (but this is not the case for u64).
 */

static inline void write_u16(FILE *fp, U16 u16) {
	putc(u16 & 0xFF, fp);
	putc(u16 >> 8, fp);
}

static inline void write_u32(FILE *fp, U32 u32) {
	write_u16(fp, u32 & 0xFFFF);
    write_u16(fp, (U16)(u32 >> 16));
}

static void write_u64(FILE *fp, U64 u64) {
#if BINFILE_PORTABLE
	write_u32(fp, u64 & 0xFFFFFFFF);
	write_u32(fp, (U32)(u64 >> 32));
#else
	fwrite(&u64, sizeof u64, 1, fp);
#endif
}


