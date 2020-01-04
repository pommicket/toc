#define BINFILE_PORTABLE 1

#ifdef TOC_DEBUG
#define BINFILE_PRINT
#endif

static inline void write_u8(FILE *fp, U8 u8) {
	putc(u8, fp);
#ifdef BINFILE_PRINT
	static int col = 0;
	printf("%02x ", u8);
	++col;
	if (col == 8) printf(" ");
	if (col == 16) {
		col = 0;
		printf("\n");
	}
	fflush(stdout);
#endif
}

#undef BINFILE_PRINT /* don't need it anymore */

static inline void write_i8(FILE *fp, I8 i8) {
	write_u8(fp, (U8)i8);
}

/* 
   note: a bit of testing seems to reveal that the portable versions for u16/32 are faster than fwrite
   (but this is not the case for u64).
 */

static inline void write_u16(FILE *fp, U16 u16) {
	write_u8(fp, (U8)(u16 & 0xFF));
	write_u8(fp, (U8)(u16 >> 8));
}

static inline void write_i16(FILE *fp, I16 i16) {
	write_u16(fp, (U16)i16);
}

static inline void write_u32(FILE *fp, U32 u32) {
	write_u16(fp, u32 & 0xFFFF);
	write_u16(fp, (U16)(u32 >> 16));
}

static inline void write_i32(FILE *fp, I32 i32) {
	write_u32(fp, (U32)i32);
}

static void write_u64(FILE *fp, U64 u64) {
#if BINFILE_PORTABLE
	write_u32(fp, u64 & 0xFFFFFFFF);
	write_u32(fp, (U32)(u64 >> 32));
#else
	fwrite(&u64, sizeof u64, 1, fp);
#endif
}

static inline void write_i64(FILE *fp, I64 i64) {
#if BINFILE_PORTABLE
	write_u64(fp, (U64)i64);
#else
	fwrite(&i64, sizeof i64, 1, fp);
#endif
}

static void write_f32(FILE *fp, F32 f32) {
#if BINFILE_PORTABLE
	/* writes as IEEE 754 32-bit floating-point number, little endian */
	/* TODO: infinity, NaN */
	U32 fraction = 0;
	U32 fraction_bit = ((U32)1) << 22;
	unsigned exponent = 127;
	unsigned sign = f32 < 0;
	if (sign) f32 = -f32;
	while (f32 < (F32)1) {
		f32 *= (F32)2;
		--exponent;
	} 
	while (f32 > (F32)2) {
		f32 /= (F32)2;
		++exponent;
	}
	if (f32 > (F32)1) --f32;
	f32 *= (F32)2;
	while (fraction_bit) {
		if (((U32)f32)) {
			assert((U32)f32 == 1);
			fraction |= fraction_bit;
			--f32;
		}
		f32 *= (F32)2;
		fraction_bit >>= 1;
	}
	write_u8(fp, fraction & 0xFF);
	write_u8(fp, (fraction & 0xFF00) >> 8);
	unsigned byte3 = (fraction & 0x7F0000) >> 16;
	byte3 |= (exponent & 1) << 7;
	write_u8(fp, (U8)byte3);
	unsigned byte4 = exponent >> 1;
	byte4 |= (sign << 7);
	write_u8(fp, (U8)byte4);
#else
	fwrite(&f32, sizeof f32, 1, fp);
#endif
}

static void write_f64(FILE *fp, F64 f64) {
#if BINFILE_PORTABLE
	U64 fraction = 0;
	U64 fraction_bit = ((U64)1) << 51;
	unsigned exponent = 1023;
	unsigned sign = f64 < 0;
	if (sign) f64 = -f64;
	while (f64 < (F64)1) {
		f64 *= (F64)2;
		--exponent;
	}
	while (f64 > (F64)2) {
		f64 /= (F64)2;
		++exponent;
	}
	if (f64 > (F64)1) --f64;
	f64 *= (F64)2;
	while (fraction_bit) {
		if (((U64)f64)) {
			assert((U64)f64 == 1);
			fraction |= fraction_bit;
			--f64;
		}
		f64 *= (F64)2;
		fraction_bit >>= 1;
	}
	write_u8(fp, fraction & 0xFF);
	write_u8(fp, (fraction & 0xFF00) >> 8);
	write_u8(fp, (fraction & 0xFF0000) >> 16);
	write_u8(fp, (fraction & 0xFF000000) >> 24);
	write_u8(fp, (fraction & 0xFF00000000) >> 32);
	write_u8(fp, (fraction & 0xFF0000000000) >> 40);
	unsigned byte7 = (fraction & 0xF000000000000) >> 48;
	byte7 |= (exponent & 0xF) << 4;
	write_u8(fp, (U8)byte7);
	unsigned byte8 = (exponent & 0x7F0) >> 4;
	byte8 |= (sign << 7);
	write_u8(fp, (U8)byte8);
#else
	fwrite(&f64, sizeof f64, 1, fp);
#endif
}

static void write_bool(FILE *fp, bool b) {
	write_u8(fp, b);
}

static void write_char(FILE *fp, char c) {
	write_u8(fp, (U8)c);
}


static void write_vlq(FILE *fp, U64 x) {
	while (x >= 0x80) {
		write_u8(fp, (U8)(x & 0x7f) | 0x80);
		x >>= 7;
	}
	write_u8(fp, (U8)x);
}
