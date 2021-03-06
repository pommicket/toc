#include <dlfcn.h>
typedef struct {
	void *handle;
} Library;

static FnPtr foreign_get_fn_ptr(ForeignFnManager *ffmgr, FnExpr *fn, Location call_where) {
	FnPtr fn_ptr = fn->foreign.fn_ptr;
	if (!fn_ptr) {
		assert(fn->flags & FN_EXPR_FOREIGN);
		const char *libname = fn->foreign.lib;
		if (!libname) {
			err_print(call_where, "Attempt to call function at compile time which does not have an associated library.");
			info_print(fn->where, "Function was declared here.");
			return NULL;
		}
		Library *lib = str_hash_table_get(&ffmgr->libs_loaded, libname, strlen(libname));
		if (!lib) {
			void *handle = dlopen(libname, RTLD_LAZY);
			if (!handle) {
				err_print(call_where, "Could not open dynamic library: %s.", libname);
				return NULL;
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
			return NULL;
		}
		fn->foreign.fn_ptr = fn_ptr;
	}
	return fn_ptr;
}

#ifdef FOREIGN_USE_AVCALL
#include "foreign_avcall.c"
#elif defined __x86_64__
#include "foreign64.c"
#else
#include "foreign_avcall.c"
#endif
