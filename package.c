/*
  Copyright (C) 2019 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static void exptr_create(Exporter *exptr, FILE *out) {
	exptr->out = out;
	exptr->export_locations = true;
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
	export_u64(ex, i->id);
}

static void export_type(Exporter *ex, Type *type) {
}

static bool export_len(Exporter *ex, size_t len, const char *for_, Location where) {
	if (len > 65535) {
		err_print(where, "Too many %s (the maximum is 65535).", for_);
		return false;
	}
	export_len(ex, (U16)len);
	return true;
}
   

static bool export_val(Exporter *ex, Value val, Type *type, Location where) {
	export_type(ex, type);
	switch (type->kind) {
    case TYPE_VOID: break;
	case TYPE_BUILTIN:
		switch (type->builtin) {
		case BUILTIN_I8: export_i8(ex, val.i8); break;
		case BUILTIN_U8: export_u8(ex, val.u8); break;
		case BUILTIN_I16: export_i16(ex, val.i16); break;
		case BUILTIN_U16: export_u16(ex, val.u16); break;
		case BUILTIN_I32: export_i32(ex, val.i32); break;
		case BUILTIN_U32: export_u32(ex, val.u32); break;
		case BUILTIN_I64: export_i64(ex, val.i64); break;
		case BUILTIN_U64: export_u64(ex, val.u64); break;
		case BUILTIN_F32: export_f32(ex, val.f32); break;
		case BUILTIN_F64: export_f64(ex, val.f64); break;
		case BUILTIN_BOOL: export_bool(ex, val.boolv); break;
		case BUILTIN_CHAR: export_char(ex, val.charv); break;
		}
		break;
	case TYPE_TUPLE:
		if (arr_len(type->tuple) > 65535) {
			err_print(where, "Too many types in one tuple.");
			return false;
		}
		export_u16((U16)arr_len(type->tuple));
		
		break;
	case TYPE_TYPE:
		export_type(ex, val.type);
		break;
	case TYPE_PTR:
		err_print(where, "Cannot export pointer.");
		return false;
	case TYPE_UNKNOWN:
	case TYPE_EXPR:
		assert(0);
	    return false;
	}
	return true;
}

static bool export_expr(Exporter *ex, Expression *e) {
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
	size_t n_idents = arr_len(d->idents);
	if (n_idents > 65535) {
		err_print(d->where, "Too many identifiers in one declaration (the maximum is 65535).");
		return false;
	}
	export_u16(ex, (U16)arr_len(d->idents));
	arr_foreach(d->idents, Identifier, ident) {
		export_ident(ex, *ident);
	}

	U8 constness = 0;
	if (d->flags & DECL_IS_CONST) constness = 1;
	else if (d->flags & DECL_SEMI_CONST) constness = 2;
	export_u8(ex, constness);

	U8 expr_kind = 0;
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
