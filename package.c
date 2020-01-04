/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static bool export_decl(Exporter *ex, Declaration *d);
static bool export_block(Exporter *ex, Block *b);


static void exptr_create(Exporter *ex, FILE *out) {
	ex->out = out;
	ex->export_locations = true;
	ex->exported_fns = NULL;
	ex->exported_structs = NULL;
}


static void export_u8(Exporter *ex, U8 u8) {
	write_u8(ex->out, u8);
}
static void export_i8(Exporter *ex, I8 i8) {
	write_i8(ex->out, i8);
}
static void export_u16(Exporter *ex, U16 u16) {
	write_u16(ex->out, u16);
}
static void export_i16(Exporter *ex, I16 i16) {
	write_i16(ex->out, i16);
}
static void export_u32(Exporter *ex, U32 u32) {
	write_u32(ex->out, u32);
}
static void export_i32(Exporter *ex, I32 i32) {
	write_i32(ex->out, i32);
}
static void export_u64(Exporter *ex, U64 u64) {
	write_u64(ex->out, u64);
}
static void export_i64(Exporter *ex, I64 i64) {
	write_i64(ex->out, i64);
}
static void export_f32(Exporter *ex, F32 f32) {
	write_f32(ex->out, f32);
}
static void export_f64(Exporter *ex, F64 f64) {
	write_f64(ex->out, f64);
}
static void export_bool(Exporter *ex, bool b) {
	write_bool(ex->out, b);
}
static void export_char(Exporter *ex, char c) {
	write_char(ex->out, c);
}


static void export_location(Exporter *ex, Location where) {
	if (ex->export_locations) {
		export_u32(ex, where.line);
		export_u32(ex, where.pos);
	}
}

static void export_ident(Exporter *ex, Identifier i) {
	assert(i->id);
	if (sizeof i->id == 8) {
		export_u64(ex, (U64)i->id);
	} else {
		assert(sizeof i->id == 4);
		export_u32(ex, (U32)i->id);
	}
}

/* TODO: replace with vlq */
static bool export_len(Exporter *ex, size_t len) {
	export_u64(ex, (U64)len);
	return true;
}

static bool export_type(Exporter *ex, Type *type, Location where) {
	assert(type->flags & TYPE_IS_RESOLVED);
	export_u8(ex, (U8)type->kind);
	switch (type->kind) {
	case TYPE_VOID:
	case TYPE_TYPE:
	case TYPE_UNKNOWN:
		break;
	case TYPE_PTR: export_type(ex, type->ptr, where); break;
	case TYPE_SLICE: export_type(ex, type->slice, where); break;
	case TYPE_BUILTIN:
		export_u8(ex, (U8)type->builtin);
		break;
	case TYPE_TUPLE:
		export_len(ex, arr_len(type->tuple));
		arr_foreach(type->tuple, Type, sub)
			if (!export_type(ex, sub, where))
				return false;
		break;
	case TYPE_ARR:
		export_u64(ex, type->arr.n);
		if (!export_type(ex, type->arr.of, where))
			return false;
		break;
	case TYPE_FN:
		export_len(ex, arr_len(type->fn.types));
		arr_foreach(type->fn.types, Type, sub)
			if (!export_type(ex, sub, where))
				return false;
		export_u8(ex, type->fn.constness != NULL);
		/* [implied] if (type->fn.constness) */
		assert(sizeof(Constness) == 1); /* future-proofing */
		arr_foreach(type->fn.constness, Constness, c)
			export_u8(ex, *c);
		break;
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
		export_len(ex, (size_t)struc->export.id);
	} break;
	case TYPE_EXPR:
		assert(0);
		return false;
	}
	return true;
}

static bool export_fn_ptr(Exporter *ex, FnExpr *f, Location where) {
	if (f->export.id == 0) {
		FnExpr **fptr = arr_add(&ex->exported_fns);
		*fptr = f;
		size_t nexported_fns = arr_len(ex->exported_fns);
		if (nexported_fns > U32_MAX) {
			err_print(where, "Too many exported functions (the maximum is " STRINGIFY(U32_MAX) ").");
			return false;
		}
		f->export.id = (U32)nexported_fns;
	}
	export_len(ex, (size_t)f->export.id);
	return true;
}

static bool export_val(Exporter *ex, Value val, Type *type, Location where);
static bool export_val_ptr(Exporter *ex, void *val, Type *type, Location where) {
	switch (type->kind) {
    case TYPE_VOID: break;
	case TYPE_BUILTIN:
		switch (type->builtin) {
		case BUILTIN_I8: export_i8(ex, *(I8 *)val); break;
		case BUILTIN_U8: export_u8(ex, *(U8 *)val); break;
		case BUILTIN_I16: export_i16(ex, *(I16 *)val); break;
		case BUILTIN_U16: export_u16(ex, *(U16 *)val); break;
		case BUILTIN_I32: export_i32(ex, *(I32 *)val); break;
		case BUILTIN_U32: export_u32(ex, *(U32 *)val); break;
		case BUILTIN_I64: export_i64(ex, *(I64 *)val); break;
		case BUILTIN_U64: export_u64(ex, *(U64 *)val); break;
		case BUILTIN_F32: export_f32(ex, *(F32 *)val); break;
		case BUILTIN_F64: export_f64(ex, *(F64 *)val); break;
		case BUILTIN_BOOL: export_bool(ex, *(bool *)val); break;
		case BUILTIN_CHAR: export_char(ex, *(char *)val); break;
		}
		break;
	case TYPE_TUPLE: {
		size_t n = arr_len(type->tuple);
		Value *vals = *(Value **)val;
		for (size_t i = 0; i < n; ++i) {
			if (!export_val(ex, vals[i], &type->tuple[i], where))
				return false;
		}
	} break;
	case TYPE_TYPE:
		if (!export_type(ex, *(Type **)val, where))
			return false;
		break;
	case TYPE_PTR:
		err_print(where, "Cannot export pointer.");
		return false;
	case TYPE_ARR: {
		size_t item_size = compiler_sizeof(type->arr.of);
		char *ptr = val;
		for (U64 i = 0; i < type->arr.n; ++i) {
			if (!export_val_ptr(ex, ptr, type->arr.of, where))
				return false;
			ptr += item_size;
		}
	} break;
	case TYPE_STRUCT:
		eval_struct_find_offsets(type);
		arr_foreach(type->struc->fields, Field, f) {
			if (!export_val_ptr(ex, (char *)val + f->offset, f->type, where))
				return false;
		}
		break;
	case TYPE_SLICE: {
		Slice slice = *(Slice *)val;
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
		if (!export_fn_ptr(ex, *(FnExpr **)val, where))
			return false;
		break;
	case TYPE_UNKNOWN:
	case TYPE_EXPR:
		assert(0);
	    return false;
	}
	return true;
}

static bool export_val(Exporter *ex, Value val, Type *type, Location where) {
	return export_val_ptr(ex, val_get_ptr(&val, type), type, where);
}

static bool export_expr(Exporter *ex, Expression *e) {
	assert(e->flags & EXPR_FOUND_TYPE);
	if (!export_type(ex, &e->type, e->where))
		return false;
	switch (e->kind) {
	case EXPR_LITERAL_INT:
		export_u64(ex, e->intl);
		break;
	case EXPR_LITERAL_FLOAT:
		if (e->type.builtin == BUILTIN_F32)
			export_f32(ex, (F32)e->floatl);
		else
			export_f64(ex, (F64)e->floatl);
		break;
	case EXPR_LITERAL_BOOL:
		export_bool(ex, e->booll);
		break;
	case EXPR_LITERAL_CHAR:
		export_char(ex, e->charl);
		break;
	case EXPR_LITERAL_STR:
		fwrite(e->strl.str, 1, e->strl.len, ex->out);
		break;
	case EXPR_C:
		assert(e->c.code->kind == EXPR_VAL);
		if (!export_val(ex, e->c.code->val, &e->c.code->type, e->where))
			return false;
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
		if (e->binary.op == BINARY_DOT) {
			/* rhs may not typed (if it's a string it will be)! */
			Expression *rhs = e->binary.rhs;
			if (!(rhs->flags & EXPR_FOUND_TYPE)) {
				export_u8(ex, 0);
				assert(rhs->kind == EXPR_IDENT);
				export_ident(ex, rhs->ident);
				break;
			} else 
				export_u8(ex, 1);
		}
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
		if (!export_fn_ptr(ex, e->fn, e->where))
			return false;
		break;
	case EXPR_BLOCK:
		if (!export_block(ex, &e->block))
			return false;
		break;
	case EXPR_NEW:
		if (!export_type(ex, &e->new.type, e->where))
			return false;
		export_u8(ex, e->new.n != NULL);
		if (e->new.n)
			if (!export_expr(ex, e->new.n))
				return false;
		break;
	case EXPR_DSIZEOF:
	case EXPR_DALIGNOF:
		assert(0);
		break;
	}
	return true;
}

enum {
	  DECL_EXPORT_NONE,
	  DECL_EXPORT_EXPR,
	  DECL_EXPORT_VAL
};

static bool export_decl(Exporter *ex, Declaration *d) {
	if (d->type.kind == TYPE_UNKNOWN) {
		err_print(d->where, "Can't export declaration of unknown type.");
		return false;
	}
	export_location(ex, d->where);
	export_len(ex, arr_len(d->idents));
	arr_foreach(d->idents, Identifier, ident) {
		export_ident(ex, *ident);
	}

	if (!export_type(ex, &d->type, d->where))
		return false;
	
	U8 constness = 0;
	if (d->flags & DECL_IS_CONST) constness = 1;
	else if (d->flags & DECL_SEMI_CONST) constness = 2;
	export_u8(ex, constness);

	U8 expr_kind = DECL_EXPORT_NONE;
	if (d->flags & DECL_HAS_EXPR)
		expr_kind = DECL_EXPORT_EXPR;
	if (d->flags & DECL_FOUND_VAL)
		expr_kind = DECL_EXPORT_VAL;
	
	export_u8(ex, expr_kind);
	if (expr_kind == DECL_EXPORT_EXPR) {
		if (!export_expr(ex, &d->expr))
			return false;
	} else if (expr_kind == DECL_EXPORT_VAL) {
		if (!export_val(ex, d->val, &d->type, d->where))
			return false;
	}
	return true;
}

static bool export_stmt(Exporter *ex, Statement *s) {
	export_u8(ex, (U8)s->kind);
	switch (s->kind) {
	case STMT_EXPR:
		if (!export_expr(ex, &s->expr))
			return false;
		break;
	case STMT_DECL:
		if (!export_decl(ex, &s->decl))
			return false;
		break;
	case STMT_RET:
		assert(sizeof s->ret.flags == 1);
		export_u8(ex, (U8)s->ret.flags);
		if (s->ret.flags & RET_HAS_EXPR)
			if (!export_expr(ex, &s->ret.expr))
				return false;
		break;
	}
	return true;
}

static bool export_block(Exporter *ex, Block *b) {
	export_location(ex, b->start);
	export_location(ex, b->end);
	export_len(ex, arr_len(b->stmts));
	arr_foreach(b->stmts, Statement, s) {
		if (!export_stmt(ex, s))
			return false;
	}
	export_u8(ex, b->ret_expr != NULL);
	if (b->ret_expr)
		if (!export_expr(ex, b->ret_expr))
			return false;
	return true;
}

static bool export_fn(Exporter *ex, FnExpr *f) {
	export_len(ex, arr_len(f->params));
	arr_foreach(f->params, Declaration, param)
		if (!export_decl(ex, param))
			return false;
	export_len(ex, arr_len(f->ret_decls));
	arr_foreach(f->ret_decls, Declaration, ret_decl)
		if (!export_decl(ex, ret_decl))
			return false;
	/* no need to export the return type */
	if (!export_block(ex, &f->body))
		return false;
	return true;
}

static bool export_struct(Exporter *ex, StructDef *s) {
	export_len(ex, arr_len(s->fields));
	arr_foreach(s->fields, Field, f) {
		export_ident(ex, f->name);
		if (!export_type(ex, f->type, s->where))
			return false;
	}
	return true;
}

/* does NOT close the file */
static bool exptr_finish(Exporter *ex) {
	export_len(ex, arr_len(ex->exported_fns));
	typedef FnExpr *FnExprPtr;
	arr_foreach(ex->exported_fns, FnExprPtr, f) {
		if (!export_fn(ex, *f))
			return false;
	}
	arr_clear(&ex->exported_fns);
	export_len(ex, arr_len(ex->exported_structs));
	typedef StructDef *StructDefPtr;
	arr_foreach(ex->exported_structs, StructDefPtr, s) {
		if (!export_struct(ex, *s))
			return false;
	}
	arr_clear(&ex->exported_structs);

	if (ferror(ex->out)) {
		warn_print(LOCATION_NONE, "An error occured while writing the package output. It may be incorrect.");
	}
	return true;
}
