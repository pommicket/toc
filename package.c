/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

#define TOP_FMT_VERSION 0

static bool export_decl(Exporter *ex, Declaration *d);
static bool export_block(Exporter *ex, Block *b);
static bool export_expr(Exporter *ex, Expression *e);
static bool import_footer(Importer *i);
static void import_decl(Importer *im, Declaration *d);
static void import_expr(Importer *im, Expression *e);
static void import_block(Importer *im, Block *b);
static inline Expression *import_expr_(Importer *im);


static void exptr_create(Exporter *ex, FILE *out, const char *filename, ErrCtx *err_ctx) {
	/* construct full filename */
	ex->out = out;
	ex->exporting_to.ctx = err_ctx;
	ex->exporting_to.filename = filename;
	ex->ident_id = 0;
	ex->exported_fns = NULL;
	ex->exported_structs = NULL;
	ex->exported_idents = NULL;
	ex->started = false;
	ex->code = NULL;
}

static inline void *imptr_malloc(Importer *i, size_t n) {
	return allocr_malloc(i->allocr, n);
}

static inline void *imptr_calloc(Importer *i, size_t n, size_t s) {
	return allocr_calloc(i->allocr, n, s);
}

static inline void export_u8(Exporter *ex, U8 u8) {
	write_u8(ex->out, u8);
}
static inline U8 import_u8(Importer *i) {
	return read_u8(i->in);
}
static inline void export_i8(Exporter *ex, I8 i8) {
	write_i8(ex->out, i8);
}
static inline I8 import_i8(Importer *i) {
	return read_i8(i->in);
}
static inline void export_u16(Exporter *ex, U16 u16) {
	write_u16(ex->out, u16);
}
static inline U16 import_u16(Importer *i) {
	return read_u16(i->in);
}
static inline void export_i16(Exporter *ex, I16 i16) {
	write_i16(ex->out, i16);
}
static inline I16 import_i16(Importer *i) {
	return read_i16(i->in);
}
static inline void export_u32(Exporter *ex, U32 u32) {
	write_u32(ex->out, u32);
}
static inline U32 import_u32(Importer *i) {
	return read_u32(i->in);
}
static inline void export_i32(Exporter *ex, I32 i32) {
	write_i32(ex->out, i32);
}
static inline I32 import_i32(Importer *i) {
	return read_i32(i->in);
}
static inline void export_u64(Exporter *ex, U64 u64) {
	write_u64(ex->out, u64);
}
static inline U64 import_u64(Importer *i) {
	return read_u64(i->in);
}
static inline void export_i64(Exporter *ex, I64 i64) {
	write_i64(ex->out, i64);
}
static inline I64 import_i64(Importer *i) {
	return read_i64(i->in);
}
static inline void export_f32(Exporter *ex, F32 f32) {
	write_f32(ex->out, f32);
}
static inline F32 import_f32(Importer *i) {
	return read_f32(i->in);
}
static inline void export_f64(Exporter *ex, F64 f64) {
	write_f64(ex->out, f64);
}
static inline F64 import_f64(Importer *i) {
	return read_f64(i->in);
}
static inline void export_bool(Exporter *ex, bool b) {
	write_bool(ex->out, b);
}
static inline bool import_bool(Importer *i) {
	return read_bool(i->in);
}
static inline void export_char(Exporter *ex, char c) {
	write_char(ex->out, c);
}
static inline bool import_char(Importer *i) {
	return read_char(i->in);
}
static inline void export_vlq(Exporter *ex, U64 x) {
	write_vlq(ex->out, x);
}
static inline U64 import_vlq(Importer *i) {
	return read_vlq(i->in);
}
static inline void export_len(Exporter *ex, size_t len) {
	export_vlq(ex, (U64)len);
}
static inline size_t import_len(Importer *i) {
	return (size_t)import_vlq(i);
}


static size_t import_arr_(Importer *im, void **arr, size_t sz) {
	*arr = NULL;
	size_t len = import_len(im);
	arr_set_lena_(arr, len, sz, im->allocr);
	return len;
}
/* 
reads length and allocates an array of that length
returns length 
*/
#define import_arr(im, arr) import_arr_(im, (void **)arr, sizeof **(arr))

static inline void export_str(Exporter *ex, const char *str, size_t len) {
#ifdef TOC_DEBUG
	for (size_t i = 0; i < len; ++i)
		export_char(ex, *str++);
#else
	fwrite(str, 1, len, ex->out);
#endif
}

static inline char *import_str(Importer *i, size_t len) {
	char *str = imptr_malloc(i, len+1);
	str[len] = 0;
	fread(str, 1, len, i->in);
	return str;
}

static inline void export_cstr(Exporter *ex, const char *str) {
	size_t len = strlen(str);
	export_len(ex, len);
	export_str(ex, str, len);
}
							   
static inline char *import_cstr(Importer *i) {
	size_t len = import_len(i);
	return import_str(i, len);
}


static void export_location(Exporter *ex, Location where) {
	/* for now, we only export the line */
	export_vlq(ex, (U64)where.start->pos.line);
}
static Location import_location(Importer *im) {
	Location l;
	l.file = im->importing_from;
	l.start = NULL;
	l.simple_location.line = (U32)import_vlq(im);
	return l;
}

/* handles NULL */
static inline void export_ident(Exporter *ex, Identifier i) {
	if (!i) {
		export_vlq(ex, 0);
		return;
	}
	
	if (!i->export_id) {
		i->export_id = ++ex->ident_id;
	}
	export_vlq(ex, i->export_id);
}
static inline Identifier import_ident(Importer *im) {
	U64 id = import_vlq(im);
	assert(id <= im->max_ident_id);
	return im->ident_map[id];
}

static const U8 toc_package_indicator[3] = {116, 111, 112};

/* writes the header */
static void exptr_start(Exporter *ex, const char *pkg_name, size_t pkg_name_len) {
	const char *code = ex->code;
	ex->started = true;
	export_u8(ex, toc_package_indicator[0]);
	export_u8(ex, toc_package_indicator[1]);
	export_u8(ex, toc_package_indicator[2]);
	export_u32(ex, TOP_FMT_VERSION);
	assert(ftell(ex->out) == 7L);
	export_u64(ex, 0); /* placeholder for footer offset in file */
	export_len(ex, pkg_name_len);
	export_str(ex, pkg_name, pkg_name_len);
	
	bool has_code = code != NULL;
	export_bool(ex, has_code);
	if (has_code) {
		size_t len = strlen(code);
		export_len(ex, len);
		export_str(ex, code, len);
	}
}

/* where = where was this imported. don't free fname while imported stuff is in use. */
static bool import_pkg(Allocator *allocr, Package *p, FILE *f, const char *fname, ErrCtx *parent_ctx, Location where) {
	Importer i = {0};
	i.allocr = allocr;
	i.importing_from = imptr_calloc(&i, 1, sizeof *i.importing_from);
	i.importing_from->filename = fname;
	i.importing_from->ctx = parent_ctx;
	idents_create(&p->idents);
	i.pkg = p;
	i.in = f;
	i.import_location = where;
	/* read header */
	U8 toc[3];
	toc[0] = import_u8(&i);
	toc[1] = import_u8(&i);
	toc[2] = import_u8(&i);
	if (toc[0] != toc_package_indicator[0] ||
		toc[1] != toc_package_indicator[1] ||
		toc[2] != toc_package_indicator[2]) {
		err_print(where, "%s is not a toc package file.", fname);
		return false;
	}
	U32 version_written = import_u32(&i);
	if (version_written != TOP_FMT_VERSION) {
		warn_print(where, "Warning: toc version mismatch. Package was created with version " U32_FMT " but version " STRINGIFY(TOP_FMT_VERSION) " is being used.\n"
				   "The package may be read incorrectly.",
				   version_written);
	}
	U64 footer_offset = import_u64(&i);
	size_t pkg_name_len = import_len(&i);
	char *pkg_name = import_str(&i, pkg_name_len);
	p->name = allocr_malloc(allocr, pkg_name_len + 1);
	p->c.prefix = p->name;
	memcpy(p->name, pkg_name, pkg_name_len);
	p->name[pkg_name_len] = 0;
	bool has_code = import_bool(&i);
	if (has_code) {
		size_t code_len = import_len(&i);
		char *code = import_str(&i, code_len);
		i.importing_from->contents = code;
	}
	long decls_offset = ftell(f);
	fseek(f, (long)footer_offset, SEEK_SET);
	/* read footer */
	if (!import_footer(&i))
		return false;
	fseek(f, decls_offset, SEEK_SET);
	/* read declarations */
	p->stmts = NULL;
	while (import_u8(&i)) {
		Statement *s = arr_add(&p->stmts);
		s->kind = STMT_DECL;
		import_decl(&i, &s->decl);
	}

	free(i.ident_map);
	if (!block_enter(NULL, p->stmts, 0))
		return false;
	
	return true;
}

/* needs to handle unresolved AND resolved types! (for fns with const params) */
static bool export_type(Exporter *ex, Type *type, Location where) {
	if (type->kind == TYPE_BUILTIN) {
		export_u8(ex, (U8)((int)type->builtin + TYPE_COUNT));
	} else {	
		export_u8(ex, (U8)type->kind);
	}
	assert(sizeof type->flags == 1);
	export_u8(ex, type->flags);
	switch (type->kind) {
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_BUILTIN:
		break;
	case TYPE_PTR: export_type(ex, type->ptr, where); break;
	case TYPE_SLICE: export_type(ex, type->slice, where); break;
	case TYPE_TUPLE:
		export_len(ex, arr_len(type->tuple));
		arr_foreach(type->tuple, Type, sub)
			if (!export_type(ex, sub, where))
				return false;
		break;
	case TYPE_ARR:
		if (type->flags & TYPE_IS_RESOLVED)
			export_vlq(ex, type->arr.n);
		else
			if (!export_expr(ex, type->arr.n_expr))
				return false;
		
		if (!export_type(ex, type->arr.of, where))
			return false;
		break;
	case TYPE_FN: {
		size_t ntypes = arr_len(type->fn.types);
		export_len(ex, ntypes);
		arr_foreach(type->fn.types, Type, sub)
			if (!export_type(ex, sub, where))
				return false;
		export_bool(ex, type->fn.constness != NULL);

		if (type->fn.constness) {
			possibly_static_assert(sizeof(Constness) == 1); /* future-proofing */
			size_t nparams = ntypes - 1;
			for (size_t i = 0; i < nparams; ++i)
				export_u8(ex, type->fn.constness[i]);
		}
	} break;
	case TYPE_STRUCT: {
		StructDef *struc = type->struc;
		if (struc->export.id == 0) {
			StructDef **ptr = arr_add(&ex->exported_structs);
			*ptr = struc;
			size_t nexported_structs = arr_len(ex->exported_structs);
			if (nexported_structs > U32_MAX) {
				err_print(struc->where, "Too many exported structure definitions (the maximum is " STRINGIFY(U32_MAX) ").");
				return false;
			}
				
			struc->export.id = (U32)nexported_structs;
		}
		export_vlq(ex, (U64)struc->export.id);
	} break;
	case TYPE_EXPR:
		if (!export_expr(ex, type->expr))
			return false;
		break;
	}
	return true;
}

static inline Type *imptr_new_type(Importer *im) {
	return imptr_calloc(im, 1, sizeof(Type));
}
static inline Expression *imptr_new_expr(Importer *im) {
	return imptr_calloc(im, 1, sizeof(Expression));
}

static void import_type(Importer *im, Type *type) {
	U8 kind = import_u8(im);
	if (kind > TYPE_COUNT) {
		type->kind = TYPE_BUILTIN;
		type->builtin = (BuiltinType)(kind - TYPE_COUNT);
	} else {
		type->kind = (TypeKind)kind;
	}
	type->flags = import_u8(im);
	unsigned is_resolved = type->flags & TYPE_IS_RESOLVED;
	switch (type->kind) {
	case TYPE_VOID:
	case TYPE_BUILTIN:
	case TYPE_UNKNOWN:
		break;
	case TYPE_PTR:
		import_type(im, type->ptr = imptr_new_type(im));
		break;
	case TYPE_SLICE:
		import_type(im, type->slice = imptr_new_type(im));
		break;
	case TYPE_TUPLE: {
		size_t ntypes = import_arr(im, &type->tuple);
		for (size_t i = 0; i < ntypes; ++i) {
			import_type(im, &type->tuple[i]);
		}
	} break;
	case TYPE_ARR:
		if (is_resolved)
			type->arr.n = import_vlq(im);
		else
			type->arr.n_expr = import_expr_(im);
		import_type(im, type->arr.of = imptr_new_type(im));
		break;
	case TYPE_FN: {
		size_t i, ntypes = import_arr(im, &type->fn.types);
		for (i = 0; i < ntypes; ++i)
			import_type(im, &type->fn.types[i]);
		bool has_constness = import_bool(im);
		if (has_constness) {
			size_t nparams = ntypes - 1;
			type->fn.constness = imptr_malloc(im, nparams * sizeof *type->fn.constness);
			for (i = 0; i < nparams; ++i)
				type->fn.constness[i] = import_u8(im);
		} else type->fn.constness = NULL;
	} break;
	case TYPE_STRUCT: {
		U64 struct_id = import_vlq(im);
		assert(struct_id);
		type->struc = &im->structs[struct_id - 1];
	} break;
	case TYPE_EXPR:
		type->expr = import_expr_(im);
		break;
	}
}

static bool export_fn_ptr(Exporter *ex, FnExpr *f) {
	if (f->export.id == 0) {
		FnExpr **fptr = arr_add(&ex->exported_fns);
		*fptr = f;
		size_t nexported_fns = arr_len(ex->exported_fns);
		if (nexported_fns > U32_MAX) {
			err_print(f->where, "Too many exported functions (the maximum is " STRINGIFY(U32_MAX) ").");
			return false;
		}
		f->export.id = (U32)nexported_fns;
	}
	export_vlq(ex, f->export.id);
	return true;
}

static FnExpr *import_fn_ptr(Importer *im) {
	return &im->fns[import_vlq(im) - 1];
}

static bool export_val(Exporter *ex, Value val, Type *type, Location where);
static bool export_val_ptr(Exporter *ex, void *v, Type *type, Location where) {
	switch (type->kind) {
	case TYPE_VOID: break;
	case TYPE_BUILTIN:
		switch (type->builtin) {
		case BUILTIN_I8: export_i8(ex, *(I8 *)v); break;
		case BUILTIN_U8: export_u8(ex, *(U8 *)v); break;
		case BUILTIN_I16: export_i16(ex, *(I16 *)v); break;
		case BUILTIN_U16: export_u16(ex, *(U16 *)v); break;
		case BUILTIN_I32: export_i32(ex, *(I32 *)v); break;
		case BUILTIN_U32: export_u32(ex, *(U32 *)v); break;
		case BUILTIN_I64: export_i64(ex, *(I64 *)v); break;
		case BUILTIN_U64: export_u64(ex, *(U64 *)v); break;
		case BUILTIN_F32: export_f32(ex, *(F32 *)v); break;
		case BUILTIN_F64: export_f64(ex, *(F64 *)v); break;
		case BUILTIN_BOOL: export_bool(ex, *(bool *)v); break;
		case BUILTIN_CHAR: export_char(ex, *(char *)v); break;
		case BUILTIN_TYPE:
			if (!export_type(ex, *(Type **)v, where))
				return false;
			break;
		case BUILTIN_PKG:
			/* TODO */
			break;
		}
		break;
	case TYPE_TUPLE: {
		size_t n = arr_len(type->tuple);
		Value *vals = *(Value **)v;
		for (size_t i = 0; i < n; ++i) {
			if (!export_val(ex, vals[i], &type->tuple[i], where))
				return false;
		}
	} break;
	case TYPE_PTR:
		err_print(where, "Cannot export pointer.");
		return false;
	case TYPE_ARR: {
		size_t item_size = compiler_sizeof(type->arr.of);
		char *ptr = v;
		for (U64 i = 0; i < type->arr.n; ++i) {
			if (!export_val_ptr(ex, ptr, type->arr.of, where))
				return false;
			ptr += item_size;
		}
	} break;
	case TYPE_STRUCT:
		eval_struct_find_offsets(type->struc);
		arr_foreach(type->struc->fields, Field, f) {
			if (!export_val_ptr(ex, (char *)v + f->offset, &f->type, where))
				return false;
		}
		break;
	case TYPE_SLICE: {
		Slice slice = *(Slice *)v;
		I64 n = slice.n;
		size_t item_size = compiler_sizeof(type->slice);
		export_i64(ex, n);
		char *ptr = slice.data;
		for (I64 i = 0; i < n; ++i) {
			if (!export_val_ptr(ex, ptr, type->slice, where))
				return false;
			ptr += item_size;
		}
	} break;
	case TYPE_FN:
		if (!export_fn_ptr(ex, *(FnExpr **)v))
			return false;
		break;
	case TYPE_UNKNOWN:
	case TYPE_EXPR:
		assert(0);
		return false;
	}
	return true;
}

static inline Value import_val(Importer *im, Type *type);
static void import_val_ptr(Importer *im, void *v, Type *type) {
	switch (type->kind) {
	case TYPE_VOID: break;
	case TYPE_BUILTIN:
		switch (type->builtin) {
		case BUILTIN_I8: *(I8 *)v = import_i8(im); break;
		case BUILTIN_U8: *(U8 *)v = import_u8(im); break;
		case BUILTIN_I16: *(I16 *)v = import_i16(im); break;
		case BUILTIN_U16: *(U16 *)v = import_u16(im); break;
		case BUILTIN_I32: *(I32 *)v = import_i32(im); break;
		case BUILTIN_U32: *(U32 *)v = import_u32(im); break;
		case BUILTIN_I64: *(I64 *)v = import_i64(im); break;
		case BUILTIN_U64: *(U64 *)v = import_u64(im); break;
		case BUILTIN_F32: *(F32 *)v = import_f32(im); break;
		case BUILTIN_F64: *(F64 *)v = import_f64(im); break;
		case BUILTIN_BOOL: *(bool *)v = import_bool(im); break;
		case BUILTIN_CHAR: *(char *)v = import_char(im); break;
		case BUILTIN_TYPE:
			import_type(im, *(Type **)v = imptr_new_type(im));
			break;
		case BUILTIN_PKG:
			/* TODO */
			assert(0);
			break;
		}
		break;
	case TYPE_TUPLE: {
		Value **vals = (Value **)v;
		size_t n = arr_len(type->tuple);
		*vals = imptr_malloc(im, n * sizeof **vals);
		for (size_t i = 0; i < n; ++i) {
			(*vals)[i] = import_val(im, &type->tuple[i]);
		}
	} break;
	case TYPE_ARR: {
		size_t item_size = compiler_sizeof(type->arr.of);
		U64 n = type->arr.n;
		char *ptr = v;
		for (U64 i = 0; i < n; ++i) {
			import_val_ptr(im, ptr, type->arr.of);
			ptr += item_size;
		}
	} break;
	case TYPE_STRUCT: {
		eval_struct_find_offsets(type->struc);
		arr_foreach(type->struc->fields, Field, f) {
			import_val_ptr(im, (char *)v + f->offset, &f->type);
		}
	} break;
	case TYPE_FN:
		*(FnExpr **)v = import_fn_ptr(im);
		break;
	case TYPE_SLICE: {
		Slice *slice = v;
		I64 n = slice->n = import_i64(im);
		size_t item_size = compiler_sizeof(type->slice);
		if (n <= 0) {
			slice->data = NULL;
		} else {
			char *ptr = slice->data = imptr_malloc(im, (U64)n * item_size);
			for (I64 i = 0; i < n; ++i) {
				import_val_ptr(im, ptr, type->slice);
				ptr += item_size;
			}
		}
	} break;
	case TYPE_PTR:
	case TYPE_UNKNOWN:
	case TYPE_EXPR:
		assert(0);
		break;
	}
}


static inline bool export_val(Exporter *ex, Value val, Type *type, Location where) {
	return export_val_ptr(ex, val_get_ptr(&val, type), type, where);
}

static inline Value import_val(Importer *im, Type *type) {
	Value val;
	val = val_alloc(im->allocr, type);
	import_val_ptr(im, val_get_ptr(&val, type), type);
	return val;
}

static inline bool export_optional_val(Exporter *ex, Value *val, Type *type, Location where) {
	bool has_val = val != NULL;
	export_bool(ex, has_val);
	if (has_val) {
		return export_val(ex, *val, type, where);
	} else {
		return true;
	}
}

static inline Value *import_optional_val(Importer *im, Type *type) {
	if (import_bool(im)) {
		Value *val = imptr_malloc(im, sizeof *val);
		*val = import_val(im, type);
		return val;
	}
	return NULL;
}

/* e can be NULL! */
static inline bool export_optional_expr(Exporter *ex, Expression *e) {
	bool has_e = e != NULL;
	export_bool(ex, has_e);
	if (has_e)
		return export_expr(ex, e);
	else
		return true;
}

static inline Expression *import_optional_expr(Importer *im) {
	if (import_bool(im)) {
		return import_expr_(im);
	}
	return NULL;
}

static bool export_expr(Exporter *ex, Expression *e) {
	possibly_static_assert(sizeof e->flags == 1);
	export_u8(ex, (U8)e->flags);
	assert(e->kind < 256);
	export_u8(ex, (U8)e->kind);
	export_location(ex, e->where);
	unsigned found_type = e->flags & EXPR_FOUND_TYPE;
    if (found_type) {
		if (!export_type(ex, &e->type, e->where))
			return false;
	}
	switch (e->kind) {
	case EXPR_LITERAL_INT:
		/* smaller int literals are more common */
		export_vlq(ex, e->intl);
		break;
	case EXPR_LITERAL_FLOAT:
		if (!found_type || (e->type.flags & TYPE_IS_FLEXIBLE) || e->type.builtin == BUILTIN_F64)
			export_f64(ex, (F64)e->floatl);
		else
			export_f32(ex, (F32)e->floatl);
		break;
	case EXPR_LITERAL_BOOL:
		export_bool(ex, e->booll);
		break;
	case EXPR_LITERAL_CHAR:
		export_char(ex, e->charl);
		break;
	case EXPR_LITERAL_STR:
		export_len(ex, e->strl.len);
		fwrite(e->strl.str, 1, e->strl.len, ex->out);
		break;
	case EXPR_C:
		if (!export_expr(ex, e->c.code))
			return false;
		break;
	case EXPR_BUILTIN:
		if (found_type) {
			possibly_static_assert(BUILTIN_VAL_COUNT <= 256);
			export_u8(ex, (U8)e->builtin.which.val);
		} else {
			if (!export_expr(ex, e->builtin.which.expr))
				return false;
		}
		break;
	case EXPR_IDENT:
		export_ident(ex, e->ident);
		break;
	case EXPR_UNARY_OP:
		export_u8(ex, (U8)e->unary.op);
		if (!export_expr(ex, e->unary.of))
			return false;
		break;
	case EXPR_BINARY_OP:
		export_u8(ex, (U8)e->binary.op);
		if (!export_expr(ex, e->binary.lhs))
			return false;
		if (!export_expr(ex, e->binary.rhs))
			return false;
		break;
	case EXPR_VAL:
		if (!export_val(ex, e->val, &e->type, e->where))
			return false;
		break;
	case EXPR_TUPLE:
		export_len(ex, arr_len(e->tuple));
		arr_foreach(e->tuple, Expression, item)
			if (!export_expr(ex, item))
				return false;
		break;
	case EXPR_TYPE:
		if (!export_type(ex, &e->typeval, e->where))
			return false;
		break;
	case EXPR_FN:
		if (!export_fn_ptr(ex, e->fn))
			return false;
		break;
	case EXPR_BLOCK:
		if (!export_block(ex, &e->block))
			return false;
		break;
	case EXPR_NEW:
		if (!export_type(ex, &e->new.type, e->where))
			return false;
		if (!export_optional_expr(ex, e->new.n))
			return false;
		break;
	case EXPR_CAST:
		if (!export_expr(ex, e->cast.expr)
			|| !export_type(ex, &e->cast.type, e->where))
			return false;
		break;
	case EXPR_CALL: {
		CallExpr *c = &e->call;
		if (!export_expr(ex, c->fn))
			return false;
		if (found_type) {
			export_len(ex, arr_len(c->arg_exprs));
			arr_foreach(c->arg_exprs, Expression, arg)
				if (!export_expr(ex, arg))
					return false;
		} else {
			export_len(ex, arr_len(c->args));
			arr_foreach(c->args, Argument, arg) {
				export_location(ex, arg->where);
				export_ident(ex, arg->name);
				if (!export_expr(ex, &arg->val))
					return false;
			}
		}
	} break;
	case EXPR_IF: {
		IfExpr *i = &e->if_;
		if (!export_optional_expr(ex, i->cond))
			return false;
		if (!export_block(ex, &i->body)) return false;
		if (!export_optional_expr(ex, i->next_elif))
			return false;
	} break;
	case EXPR_WHILE: {
		WhileExpr *w = &e->while_;
		if (!export_optional_expr(ex, w->cond))
			return false;
		if (!export_block(ex, &w->body))
			return false;
	} break;
	case EXPR_PKG:
		assert(!found_type);
		if (!export_expr(ex, e->pkg.name_expr))
			return false;
		break;
	case EXPR_SLICE: {
		SliceExpr *s = &e->slice;
		if (!export_expr(ex, s->of)) return false;
		if (!export_optional_expr(ex, s->from))
			return false;
		if (!export_optional_expr(ex, s->to))
			return false;
	} break;
	case EXPR_FOR: {
		ForExpr *fo = e->for_;
		possibly_static_assert(sizeof fo->flags == 1);
		export_u8(ex, fo->flags);
		if ((fo->flags & FOR_ANNOTATED_TYPE) || found_type)
			if (!export_type(ex, &fo->type, e->where))
				return false;
		export_ident(ex, fo->index);
		export_ident(ex, fo->value);
		if (fo->flags & FOR_IS_RANGE) {
			if (!export_expr(ex, fo->range.from))
				return false;
			if (!export_optional_expr(ex, fo->range.to))
				return false;
			if (found_type) {
				if (!export_optional_val(ex, fo->range.stepval, &fo->type, e->where))
					return false;
			} else {
				if (!export_optional_expr(ex, fo->range.step))
					return false;
			}
		} else {
			if (!export_expr(ex, fo->of))
				return false;
		}
		if (!export_block(ex, &fo->body))
			return false;
	} break;
	}
	return true;
}

/* returns a pointer, unlinke import_expr */
static inline Expression *import_expr_(Importer *im) {
	Expression *e = imptr_new_expr(im);
	import_expr(im, e);
	return e;
}

static void import_expr(Importer *im, Expression *e) {
	e->flags = import_u8(im);
	e->kind = import_u8(im);
	e->where = import_location(im);
	unsigned found_type = e->flags & EXPR_FOUND_TYPE;
	if (found_type) {
		import_type(im, &e->type);
	}
	switch (e->kind) {
	case EXPR_LITERAL_INT:
		e->intl = import_vlq(im);
		break;
	case EXPR_LITERAL_FLOAT:
		if (!found_type || (e->type.flags & TYPE_IS_FLEXIBLE) || e->type.builtin == BUILTIN_F64)
			e->floatl = (Floating)import_f64(im);
		else
			e->floatl = (Floating)import_f32(im);
		break;
	case EXPR_LITERAL_BOOL:
		e->booll = import_bool(im);
		break;
	case EXPR_LITERAL_CHAR:
		e->charl = import_char(im);
		break;
	case EXPR_LITERAL_STR: {
		size_t len = import_len(im);
		fread(e->strl.str = malloc(len), 1, len, im->in);
	} break;
	case EXPR_C:
		e->c.code = import_expr_(im);
		break;
	case EXPR_BUILTIN:
		if (found_type) {
			possibly_static_assert(BUILTIN_VAL_COUNT <= 256);
			e->builtin.which.val = import_u8(im);
		} else {
			e->builtin.which.expr = import_expr_(im);
		}
		break;
	case EXPR_IDENT:
		e->ident = import_ident(im);
		break;
	case EXPR_UNARY_OP:
		e->unary.op = import_u8(im);
		e->unary.of = import_expr_(im);
		break;
	case EXPR_BINARY_OP:
		e->binary.op = import_u8(im);
		e->binary.lhs = import_expr_(im);
		e->binary.rhs = import_expr_(im);
		break;
	case EXPR_VAL:
		e->val = import_val(im, &e->type);
		break;
	case EXPR_TUPLE:
		import_arr(im, &e->tuple);
		arr_foreach(e->tuple, Expression, sub) {
			import_expr(im, sub);
		}
		break;
	case EXPR_TYPE:
		import_type(im, &e->typeval);
		break;
	case EXPR_FN:
		e->fn = import_fn_ptr(im);
		break;
	case EXPR_BLOCK:
		import_block(im, &e->block);
		break;
	case EXPR_NEW:
		import_type(im, &e->new.type);
		e->new.n = import_optional_expr(im);
		break;
	case EXPR_CAST:
		e->cast.expr = import_expr_(im);
		import_type(im, &e->cast.type);
		break;
	case EXPR_CALL: {
		CallExpr *c = &e->call;
		memset(c, 0, sizeof *c);
		c->fn = import_expr_(im);
		if (found_type) {
			import_arr(im, &c->arg_exprs);
			arr_foreach(c->arg_exprs, Expression, arg)
				import_expr(im, arg);
		} else {
			import_arr(im, &c->args);
			arr_foreach(c->args, Argument, arg) {
				arg->where = import_location(im);
				arg->name = import_ident(im);
				import_expr(im, &arg->val);
			}
		}
	} break;
	case EXPR_IF: {
		IfExpr *i = &e->if_;
		i->cond = import_optional_expr(im);
		import_block(im, &i->body);
		i->next_elif = import_optional_expr(im);
	} break;
	case EXPR_WHILE: {
		WhileExpr *w = &e->while_;
		w->cond = import_optional_expr(im);
		import_block(im, &w->body);
	} break;
	case EXPR_PKG:
		assert(!found_type);
		e->pkg.name_expr = import_expr_(im);
		break;
	case EXPR_SLICE: {
		SliceExpr *s = &e->slice;
		s->of = import_expr_(im);
		s->from = import_optional_expr(im);
		s->to = import_optional_expr(im);
	} break;
	case EXPR_FOR: {
		ForExpr *fo = e->for_ = imptr_calloc(im, 1, sizeof *fo);
		fo->flags = import_u8(im);
		if ((fo->flags & FOR_ANNOTATED_TYPE) || found_type)
			import_type(im, &fo->type);
		fo->index = import_ident(im);
		fo->value = import_ident(im);
		if (fo->flags & FOR_IS_RANGE) {
			fo->range.from = import_expr_(im);
			fo->range.to = import_optional_expr(im);
			if (found_type) {
				fo->range.stepval = import_optional_val(im, &fo->type);
			} else {
				fo->range.step = import_optional_expr(im);
			}
		} else {
			fo->of = import_expr_(im);
		}
		import_block(im, &fo->body);
	} break;
	}
}


static void export_ident_name(Exporter *ex, Identifier ident) {
	if (ident->export_name) return;
	*(Identifier *)arr_add(&ex->exported_idents) = ident;
	ident->export_name = true;
}

static bool export_decl(Exporter *ex, Declaration *d) {
	assert(ex->started);
	/* printf("EXPORT %ld\n",ftell(ex->out)); */
	possibly_static_assert(sizeof d->flags == 2);
	export_u16(ex, d->flags);

	if ((d->flags & DECL_FOUND_TYPE) && d->type.kind == TYPE_UNKNOWN) {
		err_print(d->where, "Can't export declaration of unknown type.");
		return false;
	}
	if (d->flags & DECL_EXPORT) {
		arr_foreach(d->idents, Identifier, ident) {
			export_ident_name(ex, *ident);
		}
	}
	
	export_location(ex, d->where);
	export_len(ex, arr_len(d->idents));
	arr_foreach(d->idents, Identifier, ident) {
		export_ident(ex, *ident);
	}

	if (d->flags & (DECL_FOUND_TYPE | DECL_ANNOTATES_TYPE)) {
		if (!export_type(ex, &d->type, d->where))
			return false;
	}
	if (d->flags & DECL_FOUND_VAL) {
		if (!export_val(ex, d->val, &d->type, d->where))
			return false;
	} else if (d->flags & DECL_HAS_EXPR) {
		if (!export_expr(ex, &d->expr))
			return false;
	} else if (d->flags & DECL_FOREIGN) {
		if (!(d->flags & DECL_FOUND_TYPE)) {
			if (!export_expr(ex, d->foreign.name))
				return false;
			if (!export_expr(ex, d->foreign.lib))
				return false;
		}
	}
	return true;
}

static void import_decl(Importer *im, Declaration *d) {
	possibly_static_assert(sizeof d->flags == 2);
	/* printf("IMPORT %ld\n",ftell(im->in)); */
	d->flags = import_u16(im);
	d->flags &= (DeclFlags)~(DeclFlags)DECL_EXPORT;
	d->where = import_location(im);
	d->idents = NULL;
	size_t n_idents = import_arr(im, &d->idents);
	for (size_t i = 0; i < n_idents; ++i) {
		d->idents[i] = import_ident(im);
	}
	if (d->flags & (DECL_FOUND_TYPE | DECL_ANNOTATES_TYPE)) {
		import_type(im, &d->type);
	}
	if (d->flags & DECL_FOUND_VAL) {
		d->val = import_val(im, &d->type);
		if (d->flags & DECL_HAS_EXPR) {
			d->expr.kind = EXPR_VAL;
			d->expr.val = d->val;
		}
		d->flags &= (DeclFlags)~(DeclFlags)DECL_HAS_EXPR;
	} else if (d->flags & DECL_HAS_EXPR) {
		import_expr(im, &d->expr);
	} else if (d->flags & DECL_FOREIGN) {
		if (!(d->flags & DECL_FOUND_TYPE)) {
			d->foreign.name = import_expr_(im);
			d->foreign.lib = import_expr_(im);
		}
	}
}

/* exports a declaration. to be used by other files instead of export_decl. */
static bool export_decl_external(Exporter *ex, Declaration *d) {
	export_u8(ex, 1); /* indicate that there are more declarations */
	return export_decl(ex, d);
}

static bool export_stmt(Exporter *ex, Statement *s) {
	possibly_static_assert(sizeof s->flags == 1);
	export_u8(ex, s->flags);
	export_u8(ex, (U8)s->kind);
	export_location(ex, s->where);
	switch (s->kind) {
	case STMT_EXPR:
		if (!export_expr(ex, &s->expr))
			return false;
		break;
	case STMT_DECL:
		if (!export_decl(ex, &s->decl))
			return false;
		break;
	case STMT_RET: {
		possibly_static_assert(sizeof s->ret.flags == 1);
		export_u8(ex, (U8)s->ret.flags);
		if (s->ret.flags & RET_HAS_EXPR)
			if (!export_expr(ex, &s->ret.expr))
				return false;
	} break;
	}
	return true;
}

static void import_stmt(Importer *im, Statement *s) {
	s->flags = import_u8(im);
	s->kind = import_u8(im);
	s->where = import_location(im);
	switch (s->kind) {
	case STMT_EXPR:
		import_expr(im, &s->expr);
		break;
	case STMT_DECL:
		import_decl(im, &s->decl);
		break;
	case STMT_RET:
		s->ret.flags = import_u8(im);
		if (s->ret.flags & RET_HAS_EXPR)
			import_expr(im, &s->expr);
		break;
	}
	fprint_stmt(stdout, s); printf("\n");
}

static bool export_block(Exporter *ex, Block *b) {
	possibly_static_assert(sizeof b->flags == 1);
	export_u8(ex, b->flags);
	export_location(ex, b->where);
	export_len(ex, arr_len(b->stmts));
	arr_foreach(b->stmts, Statement, s) {
		if (!export_stmt(ex, s))
			return false;
	}
	if (!export_optional_expr(ex, b->ret_expr))
		return false;
	return true;
}

static void import_block(Importer *im, Block *b) {
	b->flags = import_u8(im);
	b->where = import_location(im);
	import_arr(im, &b->stmts);
	arr_foreach(b->stmts, Statement, s) {
		import_stmt(im, s);
	}
	b->ret_expr = import_optional_expr(im);
}

static bool export_fn(Exporter *ex, FnExpr *f) {
	possibly_static_assert(sizeof f->flags == 1);
	export_u8(ex, f->flags);
	if (f->flags & FN_EXPR_FOREIGN) {
		export_cstr(ex, f->foreign.name);
		export_cstr(ex, f->foreign.lib);
	} else {
	
		export_location(ex, f->where);
		export_len(ex, arr_len(f->params));
		arr_foreach(f->params, Declaration, param) {
			if (!export_decl(ex, param))
				return false;
			arr_foreach(param->idents, Identifier, ident) {
				export_ident_name(ex, *ident);
			}
		}
		if (!export_type(ex, &f->ret_type, f->where))
			return false;
		export_len(ex, arr_len(f->ret_decls));
		arr_foreach(f->ret_decls, Declaration, ret_decl)
			if (!export_decl(ex, ret_decl))
				return false;
		if (!export_block(ex, &f->body))
			return false;
	}
	return true;
}

static void import_fn(Importer *im, FnExpr *f) {
	memset(f, 0, sizeof *f);
	f->flags = import_u8(im);
	if (f->flags & FN_EXPR_FOREIGN) {
		f->foreign.name = import_cstr(im);
		f->foreign.lib = import_cstr(im);
	} else {
		f->where = import_location(im);
		import_arr(im, &f->params);
		arr_foreach(f->params, Declaration, param) {
			import_decl(im, param);
		}
		import_type(im, &f->ret_type);
		import_arr(im, &f->ret_decls);
		arr_foreach(f->ret_decls, Declaration, ret_decl)
			import_decl(im, ret_decl);
		import_block(im, &f->body);
	}
}

static bool export_struct(Exporter *ex, StructDef *s) {
	export_ident(ex, s->name);
	if (s->name)
		export_ident_name(ex, s->name);
	export_len(ex, arr_len(s->fields));
	arr_foreach(s->fields, Field, f) {
		export_ident(ex, f->name);
		export_ident_name(ex, f->name);
		if (!export_type(ex, &f->type, s->where))
			return false;
	}
	return true;
}

static void import_struct(Importer *im, StructDef *s) {
	s->name = import_ident(im);
	size_t nfields = import_arr(im, &s->fields);
	for (size_t i = 0; i < nfields; ++i) {
		s->fields[i].name = import_ident(im);
		import_type(im, &s->fields[i].type);
	}
}

/* does NOT close the file */
static bool exptr_finish(Exporter *ex) {
	export_u8(ex, 0); /* no more declarations */
	
	long footer_offset = ftell(ex->out);

	fseek(ex->out, 7L, SEEK_SET);
	export_u64(ex, (U64)footer_offset);
	fseek(ex->out, 0L, SEEK_END);

	/* position in file of where the position in file of identifier info is */
	long ident_info_offset_offset = ftell(ex->out);
	export_u64(ex, 0); /* identifier info offset */
	long struct_info_offset_offset = ftell(ex->out);
	export_u64(ex, 0); /* struct info offset */
	
	
	export_len(ex, arr_len(ex->exported_fns));
	typedef FnExpr *FnExprPtr;
	arr_foreach(ex->exported_fns, FnExprPtr, f) {
		if (!export_fn(ex, *f))
			return false;
	}
	arr_clear(&ex->exported_fns);

	long struct_info_offset = ftell(ex->out);
	export_len(ex, arr_len(ex->exported_structs));
	typedef StructDef *StructDefPtr;
	arr_foreach(ex->exported_structs, StructDefPtr, s) {
		if (!export_struct(ex, *s))
			return false;
	}
	arr_clear(&ex->exported_structs);

	long ident_info_offset = ftell(ex->out);
	/* export number of identifiers *whose names matter* */
	fseek(ex->out, ident_info_offset, SEEK_SET);
	export_u64(ex, ex->ident_id);
	export_len(ex, arr_len(ex->exported_idents));
	arr_foreach(ex->exported_idents, Identifier, ident) {
		Identifier i = *ident;
		assert(i->export_name);
		export_vlq(ex, i->export_id);
		export_len(ex, i->len);
		fprint_ident(ex->out, i);
	}

	fseek(ex->out, ident_info_offset_offset, SEEK_SET);
	export_u64(ex, (U64)ident_info_offset);
	
	arr_clear(&ex->exported_idents);

	fseek(ex->out, struct_info_offset_offset, SEEK_SET);
	export_u64(ex, (U64)struct_info_offset);
	
	if (ferror(ex->out)) {
		Location none = {0};
		none.file = &ex->exporting_to;
		warn_print(none, "An error occured while writing the package output. It may be incorrect.");
	}
	
	return true;
}

static bool import_footer(Importer *im) {
	size_t i;
	long footer_offset = ftell(im->in);
	U64 ident_info_offset = import_u64(im);
	fseek(im->in, (long)ident_info_offset, SEEK_SET);
	
	im->max_ident_id = import_u64(im);

	
	im->ident_map = err_calloc(im->max_ident_id + 1, sizeof *im->ident_map);
	size_t n_named_idents = import_len(im);
	for (i = 0; i < n_named_idents; ++i) {
		U64 id = import_vlq(im);
		size_t name_len = import_vlq(im);
		char *name = imptr_malloc(im, name_len+1);
		name[name_len] = 0;
		fread(name, 1, name_len, im->in);
		im->ident_map[id] = ident_insert(&im->pkg->idents, &name);
		im->ident_map[id]->imported = true;
		im->ident_map[id]->from_pkg = im->pkg;
	}
	for (i = 1; i <= im->max_ident_id; ++i) {
		if (!im->ident_map[i]) {
			im->ident_map[i] = ident_new_anonymous(&im->pkg->idents);
			im->ident_map[i]->imported = true;
			im->ident_map[i]->from_pkg = im->pkg;
		}
	}

	fseek(im->in, footer_offset + 8, SEEK_SET);
	U64 struct_offset = import_u64(im);
	fseek(im->in, (long)struct_offset, SEEK_SET);
	
	
	import_arr(im, &im->structs);
#ifdef TOC_DEBUG
	/* for debugging: so that struct names show up as "anonymous struct" if they haven't been imported yet */
	arr_zero(im->structs);
#endif
	arr_foreach(im->structs, StructDef, s)
		import_struct(im, s);

	fseek(im->in, footer_offset + 16, SEEK_SET);
	
	import_arr(im, &im->fns);
	arr_zero(im->fns);
	arr_foreach(im->fns, FnExpr, f) {
		import_fn(im, f);
	}
	
	if (ferror(im->in)) {
		warn_print(im->import_location, "An error occured while reading the package. It may be incorrect.");
	}
	
	return true;
}

static void package_free(Package *pkg) {
	idents_free(&pkg->idents);
	arr_clear(&pkg->stmts);
}
