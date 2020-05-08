/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
#include <windows.h>

typedef struct {
	HMODULE handle;
} Library;

static FnPtr foreign_get_fn_ptr(ForeignFnManager *ffmgr, FnExpr *fn, Location call_where) {
	FnPtr fn_ptr = fn->foreign.fn_ptr;
	if (!fn_ptr) {
		assert(fn->flags & FN_EXPR_FOREIGN);
		char const *libname = fn->foreign.lib;
		if (!libname) {
			err_print(call_where, "Attempt to call function at compile time which does not have an associated library.");
			info_print(fn->where, "Function was declared here.");
			return NULL;
		}
		Library *lib = str_hash_table_get(&ffmgr->libs_loaded, libname, strlen(libname));
		if (!lib) {
			HMODULE handle = LoadLibraryA(libname);
			if (!handle) {
				DWORD err = GetLastError();
				err_print(call_where, "Could not open dynamic library %s (error code %ld).", libname, err);
				return NULL;
			}
			lib = str_hash_table_insert(&ffmgr->libs_loaded, libname, strlen(libname));
			lib->handle = handle;
		}
		const char *name = fn->foreign.name;
		fn_ptr = (FnPtr)GetProcAddress(lib->handle, name);
		if (!fn_ptr) {
			err_print(call_where, "Could not get function %s from dynamic library.", name);
			return NULL;
		}
		fn->foreign.fn_ptr = fn_ptr;
	}
	return fn_ptr;
}

#ifdef FOREIGN_USE_AVCALL
#include "foreign_avcall.c"
#elif defined _WIN64
#include "foreign64.c"
#else
#include "foreign_msvc32.c"
#endif
