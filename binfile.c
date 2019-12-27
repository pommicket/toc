#define BINFILE_PORTABLE 1

static inline void write_u8(FILE *fp, U8 u8) {
	putc(u8, fp);
}

static inline void write_i8(FILE *fp, I8 i8) {
	U8 byte = (U8)(i8 < 0 ? -i8 : +i8);
	if (i8 < 0) byte |= 0x80;
	putc(byte, fp);
}

/* 
   note: a bit of testing seems to reveal that the portable versions for u16/32 are faster than fwrite
   (but this is not the case for u64).
 */

static inline void write_u16(FILE *fp, U16 u16) {
	putc(u16 & 0xFF, fp);
	putc(u16 >> 8, fp);
}

static inline void write_i16(FILE *fp, I16 i16) {
    write_u8(fp, (U8)(i16 & 0xFF));
    write_i8(fp, (I8)(i16 / 0x100)); /* division is necessary here to keep sign */
}

static inline void write_u32(FILE *fp, U32 u32) {
	write_u16(fp, u32 & 0xFFFF);
    write_u16(fp, (U16)(u32 >> 16));
}

static inline void write_i32(FILE *fp, I32 i32) {
	write_u16(fp, (U16)(i32 & 0xFFFF));
	write_i16(fp, (I16)(i32 / 0x10000));
}

static void write_u64(FILE *fp, U64 u64) {
#if BINFILE_PORTABLE
	write_u32(fp, u64 & 0xFFFFFFFF);
	write_u32(fp, (U32)(u64 >> 32));
#else
	fwrite(&u64, sizeof u64, 1, fp);
#endif
}

static void write_i64(FILE *fp, I64 i64) {
#if BINFILE_PORTABLE
	write_u32(fp, (U32)(i64 & 0xFFFFFFFF));
	write_i32(fp, (I32)(i64 / 0x100000000));
#else
	fwrite(&i64, sizeof i64, 1, fp);
#endif
}

static void write_f32(FILE *fp, F32 f32) {
#if BINFILE_PORTABLE
	/* TODO */
#else
	fwrite(&f32, sizeof f32, 1, fp);
#endif
}

static void write_f64(FILE *fp, F64 f64) {
#if BINFILE_PORTABLE
	/* TODO */
#else
	fwrite(&f64, sizeof f64, 1, fp);
#endif
}
