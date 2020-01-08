#define BINFILE_PORTABLE 1

#ifdef TOC_DEBUG
#define BINFILE_PRINT
static bool binfile_printing_enabled = true;
#endif

static inline void write_u8(FILE *fp, U8 u8) {
	putc(u8, fp);
#ifdef BINFILE_PRINT
	if (binfile_printing_enabled) {
		static int col = 0;
		printf("%02x ", u8);
		++col;
		if (col == 8) printf(" ");
		if (col == 16) {
			col = 0;
			printf("\n");
		}
		fflush(stdout);
	}
#endif
}

#undef BINFILE_PRINT /* don't need it anymore */

static inline U8 read_u8(FILE *fp) {
	return (U8)getc(fp);
}

static inline void write_char(FILE *fp, char c) {
#ifdef TOC_DEBUG
	/* mayyybe this'd do the wrong thing for negative characters on systems where char is signed? */
	write_u8(fp, (U8)c);
#else
	putc(c, fp);
#endif
}

static inline char read_char(FILE *fp) {
	return (char)getc(fp);
}

static inline void write_i8(FILE *fp, I8 i8) {
	write_u8(fp, (U8)i8);
}

static inline I8 read_i8(FILE *fp) {
	U8 u8 = read_u8(fp);
	return (I8)((u8 & (1<<7)) ? -(1 + ~u8) : u8);
}

/* 
   note: a bit of testing seems to reveal that the portable versions for u16/32 are faster than fwrite
   (but this is not the case for u64).
 */

static inline void write_u16(FILE *fp, U16 u16) {
	write_u8(fp, (U8)(u16 & 0xFF));
	write_u8(fp, (U8)(u16 >> 8));
}

static inline U16 read_u16(FILE *fp) {
	U8 a = read_u8(fp);
	U8 b = read_u8(fp);
	return (U16)(a + (((U16)b) << 8));
}

static inline void write_i16(FILE *fp, I16 i16) {
	write_u16(fp, (U16)i16);
}

static inline I16 read_i16(FILE *fp) {
	U16 u16 = read_u16(fp);
	return (I16)((u16 & (1U<<15)) ? -(1 + ~u16) : u16);
}

static inline void write_u32(FILE *fp, U32 u32) {
	write_u16(fp, u32 & 0xFFFF);
	write_u16(fp, (U16)(u32 >> 16));
}

static inline U32 read_u32(FILE *fp) {
	U16 a = read_u16(fp);
	U16 b = read_u16(fp);
	return (U32)(a + (((U32)b << 16)));
}

static inline void write_i32(FILE *fp, I32 i32) {
	write_u32(fp, (U32)i32);
}

static inline I32 read_i32(FILE *fp) {
	U32 u32 = read_u32(fp);
	return (I32)((u32 & (1UL<<31)) ? -(1 + ~u32) : u32);
}

static void write_u64(FILE *fp, U64 u64) {
#if BINFILE_PORTABLE
	write_u32(fp, u64 & 0xFFFFFFFF);
	write_u32(fp, (U32)(u64 >> 32));
#else
	fwrite(&u64, sizeof u64, 1, fp);
#endif
}

static U64 read_u64(FILE *fp) {
#if BINFILE_PORTABLE
	U32 a = read_u32(fp);
	U32 b = read_u32(fp);
	return (U64)(a + (((U64)b << 32)));
#else
	U64 x;
	fread(&x, sizeof x, 1, fp);
	return x;
#endif
}

static inline void write_i64(FILE *fp, I64 i64) {
#if BINFILE_PORTABLE
	write_u64(fp, (U64)i64);
#else
	fwrite(&i64, sizeof i64, 1, fp);
#endif
}

static inline I64 read_i64(FILE *fp) {
#ifdef BINFILE_PORTABLE
	U64 u64 = read_u64(fp);
	return (I64)((u64 & ((U64)1<<63)) ? -(1 + ~u64) : u64);
#else
	I64 x;
	fread(&x, sizeof x, 1, fp);
	return x;
#endif
}

static void write_f32(FILE *fp, F32 f32) {
#if BINFILE_PORTABLE
	/* writes as IEEE 754 32-bit floating-point number, the byte order is little endian */
	/* https://en.wikipedia.org/wiki/Single_precision_floating-point_format */
	/* TODO: infinity, NaN */
	U32 fraction = 0;
	U32 fraction_bit = ((U32)1) << 22;
	U32 exponent = 127;
	U32 sign = f32 < 0;
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
	write_u32(fp, fraction | (exponent << 23) | (sign << 31));
#else
	fwrite(&f32, sizeof f32, 1, fp);
#endif
}

static F32 read_f32(FILE *fp) {
#if BINFILE_PORTABLE
	/* TODO: infinity, NaN */
	U32 u32 = read_u32(fp);
	U32 sign =     (u32 & 0x80000000);
	U32 exponent = (u32 & 0x7f800000) >> 23;
	U32 fraction = (u32 & 0x007fffff);
	F32 flt = (F32)1 + (F32)fraction / (F32)0x0800000;
	int signed_exponent = (int)exponent - 127;
	while (signed_exponent < 0) {
		++signed_exponent;
		flt /= (F32)2;
	}
	while (signed_exponent > 0) {
		--signed_exponent;
		flt *= (F32)2;
	}
	if (sign) flt = -flt;
	return flt;
#else
	F32 f32;
	fread(&f32, sizeof f32, 1, fp);
	return f32;
#endif
}

static void write_f64(FILE *fp, F64 f64) {
#if BINFILE_PORTABLE
	U64 fraction = 0;
	U64 fraction_bit = ((U64)1) << 51;
	U64 exponent = 1023;
	U64 sign = f64 < 0;
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
	write_u64(fp, fraction | (exponent << 52) | (sign << 63));
#else
	fwrite(&f64, sizeof f64, 1, fp);
#endif
}

static F64 read_f64(FILE *fp) {
#if BINFILE_PORTABLE
	/* TODO: infinity, NaN */
	U64 u64 = read_u64(fp);
	U64 sign =     (u64 & 0x8000000000000000);
	U64 exponent = (u64 & 0x7ff0000000000000) >> 52;
	U64 fraction = (u64 & 0x000fffffffffffff);
	F64 flt = (F64)fraction / (F64)0x10000000000000 + (F64)1;
	int signed_exponent = (int)exponent - 1023;
	while (signed_exponent < 0) {
		++signed_exponent;
		flt /= (F64)2;
	}
	while (signed_exponent > 0) {
		--signed_exponent;
		flt *= (F64)2;
	}
	if (sign) flt = -flt;
	return flt;
#else
	F64 f64;
	fread(&f64, sizeof f64, 1, fp);
	return f64;
#endif
}

static void write_bool(FILE *fp, bool b) {
	write_u8(fp, b);
}

static bool read_bool(FILE *fp) {
	return read_u8(fp);
}

/* 
   toc's vlq format:
   a byte whose first (most significant) bit is 0 indicates the number is done.
   a byte whose first bit is 1 indicates the number will continue.
   starts with the 7 least significant bits of the number.
*/
static void write_vlq(FILE *fp, U64 x) {
	while (x >= 0x80) {
		write_u8(fp, (U8)(x & 0x7f) | 0x80);
		x >>= 7;
	}
	write_u8(fp, (U8)x);
}

static U64 read_vlq(FILE *fp) {
    U8 byte = read_u8(fp);
	if (byte & 0x80) {
		return (read_vlq(fp) << 7) + (byte & 0x7F);
	} else {
		return byte;
	}
}

#ifdef TOC_DEBUG
static void binfile_test(void) {
	binfile_printing_enabled = false;
	FILE *fp = tmpfile();
	U64 a = 234873485734;
	write_vlq(fp, a);
	I64 b = -123981232131;
	write_i64(fp, b);
	U8 c = 12;
	write_u8(fp, c);
	F32 d = (F32)-2.323198123;
	write_f32(fp, d);
	bool e = true;
	write_bool(fp, e);
	F64 f = (F64)34.69459324823;
	write_f64(fp, f);
	fseek(fp, 0L, SEEK_SET);

#define expect(type, fmt, expr, to_be) {								\
		type exp = to_be;												\
		type got = expr;												\
		if (exp != got) {												\
			fprintf(stderr, "Expected " #expr " to be " fmt " but got " fmt "\n", exp, got); \
		    abort();													\
		}																\
	}

	expect(U64, U64_FMT, read_vlq(fp), a);
	expect(I64, I64_FMT, read_i64(fp), b);
	expect(U8, U8_FMT, read_u8(fp), c);
	expect(F32, F32_FMT, read_f32(fp), d);
	expect(bool, "%d", read_bool(fp), e);
    expect(F64, F64_FMT, read_f64(fp), f);
	fclose(fp);
	binfile_printing_enabled = true;
}
#endif
