/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
#if SIZE_MAX != U64_MAX
#error "What's going on? The 64-bit #foreign file was included, but size_t isn't 64 bits!"
#endif

#ifdef __cplusplus
#define external extern "C"
#else
#define external extern
#endif

#ifdef _WIN64

external U64 win64_call(FnPtr fn, U64 *args, I64 nargs);
external float win64_callf(FnPtr fn, U64 *args, I64 nargs);
external double win64_calld(FnPtr fn, U64 *args, I64 nargs);

static inline U64 foreign_calli(FnPtr fn, U64 *args, I64 nargs, bool *is_float) {
	(void)is_float;
	return win64_call(fn, args, nargs);
}

static inline float foreign_callf(FnPtr fn, U64 *args, I64 nargs, bool *is_float) {
	(void)is_float;
	return win64_callf(fn, args, nargs);
}

static inline double foreign_calld(FnPtr fn, U64 *args, I64 nargs, bool *is_float) {
	(void)is_float;
	return win64_calld(fn, args, nargs);
}
#else
external U64 systemv64_call(FnPtr fn, U64 *args, I64 nargs, bool *is_float);
external float systemv64_callf(FnPtr fn, U64 *args, I64 nargs, bool *is_float);
external double systemv64_calld(FnPtr fn, U64 *args, I64 nargs, bool *is_float);
static inline U64 foreign_calli(FnPtr fn, U64 *args, I64 nargs, bool *is_float) {
	return systemv64_call(fn, args, nargs, is_float);
}
static inline float foreign_callf(FnPtr fn, U64 *args, I64 nargs, bool *is_float) {
	return systemv64_callf(fn, args, nargs, is_float);
}
static inline double foreign_calld(FnPtr fn, U64 *args, I64 nargs, bool *is_float) {
	return systemv64_calld(fn, args, nargs, is_float);
}
#endif

/* disable strict aliasing warnings */
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#endif
static Status val_to_word(Value v, Type *t, Location where, U64 *w) {
	switch (t->kind) {
	case TYPE_BUILTIN:
		switch (t->builtin) {
		case BUILTIN_I8: *w = (U64)v.i8; break;
		case BUILTIN_I16: *w = (U64)v.i16; break;
		case BUILTIN_I32: *w = (U64)v.i32; break;
		case BUILTIN_U8: *w = (U64)v.u8; break;
		case BUILTIN_U16: *w = (U64)v.u16; break;
		case BUILTIN_U32: *w = (U64)v.u32; break;
		case BUILTIN_I64: *w = (U64)v.i64; break;
		case BUILTIN_U64: *w = v.u64; break;
		case BUILTIN_F32: *w = (U64)*(U32 *)&v.f32; break;
		case BUILTIN_F64: *w = *(U64 *)&v.f64; break;
		case BUILTIN_CHAR: *w = (U64)v.charv; break;
		case BUILTIN_BOOL: *w = (U64)v.boolv; break;
		case BUILTIN_TYPE:
		case BUILTIN_VARARGS:
		case BUILTIN_NMS:
		case BUILTIN_VOID:
			goto unsupported;
		}
		break;
	case TYPE_PTR:
		*w = (U64)v.ptr; break;
	default:
	unsupported: {
		/* @TODO(eventually) */
		char *s = type_to_str(t);
		err_print(where, "#foreign functions can't take arguments of type %s at compile time on Windows.", s);
		free(s);
		return false;
	}
	}
	return true;
}
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

static Status foreign_call(ForeignFnManager *ffmgr, FnExpr *fn, Type *ret_type, Type *arg_types, size_t arg_types_stride, Value *args, size_t nargs, Location call_where, Value *ret) {
	possibly_static_assert(sizeof(double) == 8); /* if either of these assertions fails, you'll need to use libffcall */
	possibly_static_assert(sizeof(float) == 4);
	FnPtr fn_ptr = foreign_get_fn_ptr(ffmgr, fn, call_where);
	if (!fn_ptr) return false;
	/* @OPTIM: use alloca/_malloca if available */
	U64 *words = err_malloc(nargs * sizeof *words);
	bool *is_float = err_malloc(nargs);
	U64 *word = words;
	char *type = (char *)arg_types;
	for (size_t i = 0; i < nargs; ++i) {
		if (!val_to_word(args[i], (Type *)type, call_where, word))
			return false;
		is_float[i] = type_is_float((Type *)type);
		type += arg_types_stride;
		++word;
	}
	int kind = 0; /* 0=>integer, 1=>f32, 2=>f64 */
	switch (ret_type->kind) {
	case TYPE_BUILTIN:
		switch (ret_type->builtin) {
		case BUILTIN_I8: 
		case BUILTIN_I16:
		case BUILTIN_I32:
		case BUILTIN_I64:
		case BUILTIN_U8: 
		case BUILTIN_U16:
		case BUILTIN_U32:
		case BUILTIN_U64:
		case BUILTIN_BOOL:
		case BUILTIN_CHAR:
		case BUILTIN_VOID:
			break;
		case BUILTIN_F32:
			kind = 1;
			break;
		case BUILTIN_F64:
			kind = 2;
			break;
		default:
			goto unsupported;
		}
		break;
	case TYPE_PTR:
		break;
	default:
	unsupported: {
		char *s = type_to_str(ret_type);
		/* @TODO(eventually) */
		err_print(call_where, "You can't call functions which return type %s at compile time on Windows.", s);
		free(s);
		return false;
	}
	}
	
	switch (kind) {
	case 0: {
		U64 r = foreign_calli(fn_ptr, words, (I64)nargs, is_float);
		switch (ret_type->kind) {
		case TYPE_BUILTIN:
			switch (ret_type->builtin) {
			case BUILTIN_I8: ret->i8 = (I8)r; break;
			case BUILTIN_I16: ret->i16 = (I16)r; break;
			case BUILTIN_I32: ret->i32 = (I32)r; break;
			case BUILTIN_I64: ret->i64 = (I64)r; break;
			case BUILTIN_U8: ret->u8 = (U8)r; break;
			case BUILTIN_U16: ret->u16 = (U16)r; break;
			case BUILTIN_U32: ret->u32 = (U32)r; break;
			case BUILTIN_U64: ret->u64 = (U64)r; break;
			case BUILTIN_BOOL: ret->boolv = (bool)r; break;
			case BUILTIN_CHAR: ret->charv = (char)r; break;
			case BUILTIN_VOID: (void)r; break;
			default: assert(0); break;
			}
			break;
		case TYPE_PTR:
			ret->ptr = (void *)r;
			break;
		default: assert(0); break;
		}
	} break;
	case 1:
		ret->f32 = foreign_callf(fn_ptr, words, (I64)nargs, is_float);
		break;
	case 2:
		ret->f64 = foreign_calld(fn_ptr, words, (I64)nargs, is_float);
		break;
	}
	free(words);
	free(is_float);
	return true;
}
