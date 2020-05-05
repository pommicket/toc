/*
this code is not standard-compliant in the slightest, and a bit dubious,
but I don't think there's any other way that doesn't involve assembly
*/

typedef size_t Word;
#if SIZE_MAX != U32_MAX
#error "What's going on? The 32-bit Windows file was included, but size_t isn't 32 bits!"
#endif

/* f64s take 2 words */
static Status val_to_words(Value v, Type *t, Location where, Word *w) {
	switch (t->kind) {
	case TYPE_BUILTIN:
		switch (t->builtin) {
		case BUILTIN_I8: *w = (Word)v.i8; break;
		case BUILTIN_I16: *w = (Word)v.i16; break;
		case BUILTIN_I32: *w = (Word)v.i32; break;
		case BUILTIN_U8: *w = (Word)v.u8; break;
		case BUILTIN_U16: *w = (Word)v.u16; break;
		case BUILTIN_U32: *w = (Word)v.u32; break;
		case BUILTIN_F32: *w = (Word)*(U32 *)&v.f32; break;
		case BUILTIN_I64:
		case BUILTIN_U64:
		case BUILTIN_F64: {
			Word *ws = (Word *)&v;
			w[0] = ws[0];
			w[1] = ws[1];
		} break;
		case BUILTIN_CHAR: *w = (Word)v.charv; break;
		case BUILTIN_BOOL: *w = (Word)v.boolv; break;
		case BUILTIN_TYPE:
		case BUILTIN_VARARGS:
		case BUILTIN_NMS:
		case BUILTIN_VOID:
			goto unsupported;
		}
		break;
	case TYPE_PTR:
		*w = (Word)v.ptr; break;
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


/* 
because of the way the MSVC "cdecl" calling convention works, the only things that affect the way a function is called are
the number of arguments and whether the function returns an integer (or pointer), floating-point number,
or struct (struct return values are not supported yet).

this means we can get away with these twenty functions--GCC/MSVC x64 pass most arguments in registers, so there would need to be
a lot more functions to support different combinations of integer and floating-point arguments (since they use different
registers)
*/

/* call function with 0 arguments, returning some sort of integer (or pointer) */
static U64 msvc_call0i(FnPtr fn, Word *w) {
	(void)w;
	return ((U64(*)(void))fn)();
}
static U64 msvc_call1i(FnPtr fn, Word *w) {
	return ((U64(*)(Word))fn)(w[0]);
}
static U64 msvc_call2i(FnPtr fn, Word *w) {
	return ((U64(*)(Word, Word))fn)(w[0], w[1]);
}
static U64 msvc_call3i(FnPtr fn, Word *w) {
	return ((U64(*)(Word, Word, Word))fn)(w[0], w[1], w[2]);
}
static U64 msvc_call4i(FnPtr fn, Word *w) {
	return ((U64(*)(Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3]);
}
static U64 msvc_call5i(FnPtr fn, Word *w) {
	return ((U64(*)(Word, Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3], w[4]);
}
static U64 msvc_call6i(FnPtr fn, Word *w) {
	return ((U64(*)(Word, Word, Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3], w[4], w[5]);
}
static U64 msvc_call7i(FnPtr fn, Word *w) {
	return ((U64(*)(Word, Word, Word, Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3], w[4], w[5], w[6]);
}
static U64 msvc_call8i(FnPtr fn, Word *w) {
	return ((U64(*)(Word, Word, Word, Word, Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3], w[4], w[5], w[6], w[7]);
}
static U64 msvc_call9i(FnPtr fn, Word *w) {
	return ((U64(*)(Word, Word, Word, Word, Word, Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3], w[4], w[5], w[6], w[7], w[8]);
}
static U64 msvc_call10i(FnPtr fn, Word *w) {
	return ((U64(*)(Word, Word, Word, Word, Word, Word, Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3], w[4], w[5], w[6], w[7], w[8], w[9]);
}
static U64 (*const msvc_calli[11])(FnPtr fn, Word *w) = {
	msvc_call0i,
	msvc_call1i,
	msvc_call2i,
	msvc_call3i,
	msvc_call4i,
	msvc_call5i,
	msvc_call6i,
	msvc_call7i,
	msvc_call8i,
	msvc_call9i,
	msvc_call10i
};


/* call function with 0 arguments, returning a float or double */
static double msvc_call0f(FnPtr fn, Word *w) {
	(void)w;
	return ((double(*)(void))fn)();
}
static double msvc_call1f(FnPtr fn, Word *w) {
	return ((double(*)(Word))fn)(w[0]);
}
static double msvc_call2f(FnPtr fn, Word *w) {
	return ((double(*)(Word, Word))fn)(w[0], w[1]);
}
static double msvc_call3f(FnPtr fn, Word *w) {
	return ((double(*)(Word, Word, Word))fn)(w[0], w[1], w[2]);
}
static double msvc_call4f(FnPtr fn, Word *w) {
	return ((double(*)(Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3]);
}
static double msvc_call5f(FnPtr fn, Word *w) {
	return ((double(*)(Word, Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3], w[4]);
}
static double msvc_call6f(FnPtr fn, Word *w) {
	return ((double(*)(Word, Word, Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3], w[4], w[5]);
}
static double msvc_call7f(FnPtr fn, Word *w) {
	return ((double(*)(Word, Word, Word, Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3], w[4], w[5], w[6]);
}
static double msvc_call8f(FnPtr fn, Word *w) {
	return ((double(*)(Word, Word, Word, Word, Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3], w[4], w[5], w[6], w[7]);
}
static double msvc_call9f(FnPtr fn, Word *w) {
	return ((double(*)(Word, Word, Word, Word, Word, Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3], w[4], w[5], w[6], w[7], w[8]);
}
static double msvc_call10f(FnPtr fn, Word *w) {
	return ((double(*)(Word, Word, Word, Word, Word, Word, Word, Word, Word, Word))fn)(w[0], w[1], w[2], w[3], w[4], w[5], w[6], w[7], w[8], w[9]);
}
static double (*const msvc_callf[11])(FnPtr fn, Word *w) = {
	msvc_call0f,
	msvc_call1f,
	msvc_call2f,
	msvc_call3f,
	msvc_call4f,
	msvc_call5f,
	msvc_call6f,
	msvc_call7f,
	msvc_call8f,
	msvc_call9f,
	msvc_call10f
};


static Status foreign_call(ForeignFnManager *ffmgr, FnExpr *fn, Type *ret_type, Type *arg_types, size_t arg_types_stride, Value *args, size_t nargs, Location call_where, Value *ret) {
	possibly_static_assert(sizeof(double) == 8);
	possibly_static_assert(sizeof(float) == 4);
	FnPtr fn_ptr = msvc_get_fn_ptr(ffmgr, fn, call_where);

	Word words[10];
	Word *word = words;
	char *type = (char *)arg_types;
	for (size_t i = 0; i < nargs; ++i) {
		int arg_words = compiler_sizeof((Type *)type) == 8 ? 2 : 1;
		if (word + arg_words > words + 10) {
			err_print(call_where, "You can only call functions with up to 40 bytes of arguments on 32-bit Windows.");
			return false;
		}
		if (!val_to_words(args[i], (Type *)type, call_where, word))
			return false;
		type += arg_types_stride;
		word += arg_words;
	}
	size_t nwords = (size_t)(word - words);
	bool is_float = false;
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
		case BUILTIN_F64:
			is_float = true;
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
	
	if (is_float) {
		double d = msvc_callf[nwords](fn_ptr, words);
		assert(ret_type->kind == TYPE_BUILTIN);
		if (ret_type->builtin == BUILTIN_F32) {
			ret->f32 = (F32)d; /* turns out functions just always return doubles */
		} else {
			assert(ret_type->builtin == BUILTIN_F64);
			ret->f64 = d;
		}
	} else {
		U64 r = msvc_calli[nwords](fn_ptr, words);
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
	}
	return true;
}
