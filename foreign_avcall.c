/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
// WARNING: In this file, you will find crazy macros and dubious usage of avcall. Beware!

#if CHAR_BIT != 8
#error "Compile-time foreign functions can only be used on systems where CHAR_BIT is 8."
#endif

// avcall has some sign conversion problems on BSD
// (the macros it defines cause problems too, which is why this is ignored for so long)

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#elif defined __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#endif
#include <avcall.h>


#if SCHAR_MAX != 127
#error "Foreign function support requires an 8-bit signed type."
#else
#define av_type_with_limit_127 schar
#endif

#if UCHAR_MAX != 255
#error "Foreign function support requires an 8-bit unsigned type."
#else
#define av_type_with_limit_255 uchar
#endif

#if SHRT_MAX != 32767
#error "Foreign function support requires a 16-bit signed type."
#else
#define av_type_with_limit_32767 short
#endif

#if USHRT_MAX != 65535
#error "Foreign function support requires an 16-bit unsigned type."
#else
#define av_type_with_limit_65535 ushort
#endif

#if INT_MAX == 2147483647
#define av_type_with_limit_2147483647 int
#elif LONG_MAX == 2147483647
#define av_type_with_limit_2147483647 long
#else
#error "Foreign function support requires an 32-bit signed type."
#endif

#if UINT_MAX == 4294967295
#define av_type_with_limit_4294967295 uint
#elif ULONG_MAX == 4294967295
#define av_type_with_limit_4294967295 ulong
#else
#error "Foreign function support requires an 32-bit unsigned type."
#endif

#if LONG_MAX == 9223372036854775807
#define av_type_with_limit_9223372036854775807 long
#elif LLONG_MAX == 9223372036854775807
#define av_type_with_limit_9223372036854775807 longlong
#else
#error "Foreign function support requires a 64-bit signed type."
#endif

#if ULONG_MAX == 18446744073709551615ULL
#define av_type_with_limit_18446744073709551615 ulong
#elif ULLONG_MAX == 18446744073709551615ULL
#define av_type_with_limit_18446744073709551615 ulonglong
#else
#error "Foreign function support requires a 64-bit unsigned type."
#endif



#define av_type_with_limit(x) join(av_type_with_limit_, x)

#define toc_av_start(limit) join(av_start_, av_type_with_limit(limit))
#define toc_av_add(limit) join(av_, av_type_with_limit(limit))

#define toc_av_start_f32 av_start_float
#define toc_av_start_f64 av_start_double
#define toc_av_f32 av_float
#define toc_av_f64 av_double

static Status arg_list_start(av_alist *arg_list, FnPtr fn, Value *return_val, Type *return_type, Location where) {
	switch (return_type->kind) {
	case TYPE_UNKNOWN:
		err_print(where, "Cannot call foreign function with unknown return type.");
		return false;
	case TYPE_TUPLE:
		err_print(where, "Cannot call foreign function with tuple return type.");
		return false;
	case TYPE_ARR:
		err_print(where, "Foreign functions cannot return arrays.");
		return false;
	case TYPE_FN:
		warn_print(where, "Foreign function returns function pointer. If it returns a C-style function pointer, it won't be called properly by toc.");
		av_start_ptr(*arg_list, fn, FnExpr *, &return_val->fn);
		break;
	case TYPE_PTR:
		av_start_ptr(*arg_list, fn, void *, &return_val->ptr);
		break;
	case TYPE_BUILTIN:
		switch (return_type->builtin) {
		case BUILTIN_I8:
			toc_av_start(127)(*arg_list, fn, &return_val->i8);
			break;
		case BUILTIN_U8:
			toc_av_start(255)(*arg_list, fn, &return_val->u8);
			break;
		case BUILTIN_I16:
			toc_av_start(32767)(*arg_list, fn, &return_val->i16);
			break;
		case BUILTIN_U16:
			toc_av_start(65535)(*arg_list, fn, &return_val->u16);
			break;
		case BUILTIN_I32:
			toc_av_start(2147483647)(*arg_list, fn, &return_val->i32);
			break;
		case BUILTIN_U32:
			toc_av_start(4294967295)(*arg_list, fn, &return_val->u32);
			break;
		case BUILTIN_I64:
			toc_av_start(9223372036854775807)(*arg_list, fn, &return_val->i64);
			break;
		case BUILTIN_U64:
			toc_av_start(18446744073709551615)(*arg_list, fn, &return_val->u64);
			break;
		case BUILTIN_BOOL:
			// bool is probably just unsigned char.... hopefully...
			av_start_uchar(*arg_list, fn, &return_val->u8);
			break;
		case BUILTIN_CHAR:
			av_start_char(*arg_list, fn, &return_val->charv);
			break;
		case BUILTIN_F32:
			toc_av_start_f32(*arg_list, fn, &return_val->f32);
			break;
		case BUILTIN_F64:
			toc_av_start_f64(*arg_list, fn, &return_val->f64);
			break;
		case BUILTIN_TYPE:
			av_start_ptr(*arg_list, fn, Type *, &return_val->type);
			break;
		case BUILTIN_NMS:
			av_start_ptr(*arg_list, fn, Namespace *, &return_val->nms);
			break;
		case BUILTIN_VOID:
			av_start_void(*arg_list, fn);
			break;
		case BUILTIN_VARARGS:
			assert(0);
			break;
		}
		break;
	case TYPE_STRUCT: {
		size_t struct_size = compiler_sizeof(return_type);
		StructDef *struc = return_type->struc;
		return_val->struc = err_calloc(1, struct_size);
		bool splittable;
		// hopefully this is right!
		if (struct_size <= sizeof(long)) {
			splittable = true;
		} else if (struct_size > 2*sizeof(long)) {
			splittable = false;
		} else if (arr_len(struc->fields) > 4) {
			splittable = false;
		} else {
			// NOTE: this warning is not because splittable is being computed incorrectly! it doesn't handle it right with *either* splittable = 0 or splittable = 1
			warn_print(where, "Dynamically calling function which returns a struct. avcall seems to not handle structs of size ~2*sizeof(long) correctly.");
			splittable = true;
			size_t word_size = sizeof(__avword);
			arr_foreach(struc->fields, Field, f) {
				if (f->offset / word_size != (f->offset + compiler_sizeof(f->type) - 1) / word_size) {
					splittable = false;
					break;
				}
			}
		}
		// getting warning on Debian stretch about splittable being set but not used
		_av_start_struct(*arg_list, fn, struct_size, splittable, return_val->struc); (void)splittable;
	} break;
	case TYPE_SLICE:
		av_start_struct(*arg_list, fn, Slice, av_word_splittable_2(I64, void *), &return_val->slice);
		break;
	case TYPE_EXPR:
		assert(0);
		return false;
	}
	return true;
}

static Status arg_list_add(av_alist *arg_list, Value val, Type *type, Location where) {
	switch (type->kind) {
	case TYPE_TUPLE:
	case TYPE_UNKNOWN:
	case TYPE_ARR: { // @TODO: maybe just pass pointer for arr?
		char *s = type_to_str(type);
		err_print(where, "Cannot pass type %s to foreign function.", s);
		free(s);
		return false;
	}
	case TYPE_PTR:
		av_ptr(*arg_list, void *, val.ptr);
		break;
	case TYPE_FN:
		warn_print(where, "Passing toc function pointer to foreign function. This will not work if the function expects a C-style function pointer.");
		av_ptr(*arg_list, FnExpr *, val.fn);
		break;
	case TYPE_BUILTIN:
		switch (type->builtin) {
		case BUILTIN_I8:
			toc_av_add(127)(*arg_list, val.i8);
			break;
		case BUILTIN_U8:
			toc_av_add(255)(*arg_list, val.u8);
			break;
		case BUILTIN_I16:
			toc_av_add(32767)(*arg_list, val.i16);
			break;
		case BUILTIN_U16:
			toc_av_add(65535)(*arg_list, val.u16);
			break;
		case BUILTIN_I32:
			toc_av_add(2147483647)(*arg_list, val.i32);
			break;
		case BUILTIN_U32:
			toc_av_add(4294967295)(*arg_list, val.u32);
			break;
		case BUILTIN_I64:
			toc_av_add(9223372036854775807)(*arg_list, val.i64);
			break;
		case BUILTIN_U64:
			toc_av_add(18446744073709551615)(*arg_list, val.u64);
			break;
		case BUILTIN_CHAR:
			av_char(*arg_list, val.charv);
			break;
		case BUILTIN_BOOL:
			av_uchar(*arg_list, val.boolv);
			break;
		case BUILTIN_F32:
			toc_av_f32(*arg_list, val.f32);
			break;
		case BUILTIN_F64:
			toc_av_f64(*arg_list, val.f64);
			break;
		case BUILTIN_TYPE:
			av_ptr(*arg_list, Type *, val.type);
			break;
		case BUILTIN_NMS:
			av_ptr(*arg_list, Namespace *, val.nms);
			break;
		case BUILTIN_VARARGS:
			arr_foreach(val.varargs, VarArg, arg) {
				if (!arg_list_add(arg_list, arg->val, arg->type, where))
					return false;
			}
			break;
		case BUILTIN_VOID:
			err_print(where, "Cannot pass type void to foreign function.");
			return false;
		}
		break;
	case TYPE_SLICE:
		av_struct(*arg_list, Slice, val.slice);
		break;
	case TYPE_STRUCT:
		_av_struct(*arg_list, compiler_sizeof(type), compiler_alignof(type), val.struc);
		break;
	case TYPE_EXPR:
		assert(0);
		return false;
	}
	return true;
}

#ifdef __clang__
#pragma clang diagnostic pop
#elif defined __GNUC__
#pragma GCC diagnostic pop
#endif

static Status foreign_call(ForeignFnManager *ffmgr, FnExpr *fn, Type *ret_type, Type *arg_types, size_t arg_types_stride, Value *args, size_t nargs, Location call_where, Value *ret) {
	FnPtr fn_ptr = foreign_get_fn_ptr(ffmgr, fn, call_where);
	if (!fn_ptr) return false;
	av_alist arg_list;
	if (!arg_list_start(&arg_list, fn_ptr, ret, ret_type, call_where))
		return false;
	char *type = (char *)arg_types;
	for (size_t i = 0; i < nargs; ++i) {
		if (!arg_list_add(&arg_list, args[i], (Type *)type, call_where))
			return false;
		type += arg_types_stride;
	}
	av_call(arg_list);
	return true;
}

