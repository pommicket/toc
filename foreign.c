/* WARNING: In this file, you will find crazy macros and dubious usage of avcall. Beware! */

#if COMPILE_TIME_FOREIGN_FN_SUPPORT
#if CHAR_BIT != 8
#error "Compile-time foreign functions can only be used on systems where CHAR_BIT is 8."
#endif
#include <avcall.h>
#include <dlfcn.h>


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

static bool arg_list_start(av_alist *arg_list, void (*fn)(), Value *return_val, Type *return_type, Location where) {
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
	case TYPE_VOID:
		av_start_void(*arg_list, fn);
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
			/* bool is probably just unsigned char.... hopefully... */
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
		}
		break;
	case TYPE_STRUCT: {
		size_t struct_size = compiler_sizeof(return_type);
		StructDef *struc = return_type->struc;
		return_val->struc = err_calloc(1, struct_size);
		bool splittable;
		/* hopefully this is right! */
		if (struct_size <= sizeof(long)) {
			splittable = true;
		} else if (struct_size > 2*sizeof(long)) {
			splittable = false;
		} else if (arr_len(struc->fields) > 4) {
			splittable = false;
		} else {
			/* NOTE: this warning is not because splittable is being computed incorrectly! it doesn't handle it right with *either* splittable = 0 or splittable = 1 */
			warn_print(where, "Dynamically calling function which returns a struct. avcall seems to not handle structs of size ~2*sizeof(long) correctly.");
			splittable = true;
			size_t word_size = sizeof(__avword);
			arr_foreach(struc->fields, Field, f) {
				if (f->offset / word_size != (f->offset + compiler_sizeof(&f->type) - 1) / word_size) {
					splittable = false;
					break;
				}
			}
		}
		_av_start_struct(*arg_list, fn, struct_size, splittable, return_val->struc);
	} break;
	case TYPE_SLICE:
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#endif	
		av_start_struct(*arg_list, fn, Slice, av_word_splittable_2(I64, void *), &return_val->slice);
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic pop
#endif
		break;
	case TYPE_EXPR:
		assert(0);
		return false;
	}
	return true;
}

static bool arg_list_add(av_alist *arg_list, Value *val, Type *type, Location where) {
	switch (type->kind) {
	case TYPE_VOID:
	case TYPE_TUPLE:
	case TYPE_UNKNOWN:
	case TYPE_ARR: { /* TODO: maybe just pass pointer for arr? */
		char *s = type_to_str(type);
		err_print(where, "Cannot pass type %s to foreign function.", s);
		free(s);
		return false;
	}
	case TYPE_PTR:
		av_ptr(*arg_list, void *, val->ptr);
		break;
	case TYPE_FN:
		warn_print(where, "Passing toc function pointer to foreign function. This will not work if the function expects a C-style function pointer.");
		av_ptr(*arg_list, FnExpr *, val->fn);
		break;
	case TYPE_BUILTIN:
		switch (type->builtin) {
		case BUILTIN_I8:
			toc_av_add(127)(*arg_list, val->i8);
			break;
		case BUILTIN_U8:
			toc_av_add(255)(*arg_list, val->u8);
			break;
		case BUILTIN_I16:
			toc_av_add(32767)(*arg_list, val->i16);
			break;
		case BUILTIN_U16:
			toc_av_add(65535)(*arg_list, val->u16);
			break;
		case BUILTIN_I32:
			toc_av_add(2147483647)(*arg_list, val->i32);
			break;
		case BUILTIN_U32:
			toc_av_add(4294967295)(*arg_list, val->u32);
			break;
		case BUILTIN_I64:
			toc_av_add(9223372036854775807)(*arg_list, val->i64);
			break;
		case BUILTIN_U64:
			toc_av_add(18446744073709551615)(*arg_list, val->u64);
			break;
		case BUILTIN_CHAR:
			av_char(*arg_list, val->charv);
			break;
		case BUILTIN_BOOL:
			av_uchar(*arg_list, val->boolv);
			break;
		case BUILTIN_F32:
			toc_av_f32(*arg_list, val->f32);
			break;
		case BUILTIN_F64:
			toc_av_f64(*arg_list, val->f64);
			break;
		case BUILTIN_TYPE:
			av_ptr(*arg_list, Type *, val->type);
			break;
		case BUILTIN_NMS:
			av_ptr(*arg_list, Namespace *, val->nms);
			break;
		}
		break;
	case TYPE_SLICE:
		av_struct(*arg_list, Slice, val->slice);
		break;
	case TYPE_STRUCT:
		_av_struct(*arg_list, compiler_sizeof(type), compiler_alignof(type), val->struc);
		break;
	case TYPE_EXPR:
		assert(0);
		return false;
	}
	return true;
}

static void ffmgr_create(ForeignFnManager *ffmgr) {
	str_hash_table_create(&ffmgr->libs_loaded, sizeof(Library), NULL);
}

static bool foreign_call(ForeignFnManager *ffmgr, FnExpr *fn, Type *fn_type, Value *args, Location call_where, Value *ret) {
	void (*fn_ptr)() = fn->foreign.fn_ptr;
	if (!fn_ptr) {
		assert(fn->flags & FN_EXPR_FOREIGN);
		const char *libname = fn->foreign.lib;
		Library *lib = str_hash_table_get(&ffmgr->libs_loaded, libname, strlen(libname));
		if (!lib) {
			/* TODO: IMPORTANT: only open libraries once */
			void *handle = dlopen(libname, RTLD_LAZY);
			if (!handle) {
				err_print(call_where, "Could not open dynamic library: %s.", libname);
				return false;
			}
			lib = str_hash_table_insert(&ffmgr->libs_loaded, libname, strlen(libname));
			lib->handle = handle;
		}
		const char *name = fn->foreign.name;
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#endif
		fn_ptr = dlsym(lib->handle, name);
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
		
		if (!fn_ptr) {
			err_print(call_where, "Could not get function from dynamic library: %s.", name);
			return false;
		}
		fn->foreign.fn_ptr = fn_ptr;		
	}

	av_alist arg_list;
	if (!arg_list_start(&arg_list, fn_ptr, ret, &fn_type->fn.types[0], call_where))
		return false;
	size_t nparams = arr_len(fn_type->fn.types)-1;
	for (size_t i = 0; i < nparams; ++i) {
		if (!arg_list_add(&arg_list, &args[i], &fn_type->fn.types[i+1], call_where))
			return false;
	}
	av_call(arg_list);
	
	(void)fn_type; (void)args;	
	return true;
}

static void ffmgr_free(ForeignFnManager *ffmgr) {
	arr_foreach(ffmgr->libs_loaded.slots, StrHashTableSlotPtr, slotp) {
		if (*slotp) {
			Library *lib = (void *)((*slotp)->data);
			dlclose(lib->handle);
		}
	}
	str_hash_table_free(&ffmgr->libs_loaded);
}

#else
static void ffmgr_create(ForeignFnManager *ffmgr) {
	(void)ffmgr;
}

static bool foreign_call(ForeignFnManager *ffmgr, FnExpr *fn, Type *fn_type, Value *args, Location call_where, Value *ret) {
	(void)ffmgr; (void)fn; (void)fn_type; (void)args; (void)ret;
	err_print(call_where, "You have not compiled toc with compile time foreign function support.");
	return false;
}

static void ffmgr_free(ForeignFnManager *ffmgr) {
	(void)ffmgr;
}
#endif

