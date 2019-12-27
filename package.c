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

static void export_u16(Exporter *ex, U16 u16) {
	write_u16(ex->out, u16);
}

static void export_u32(Exporter *ex, U32 u32) {
	write_u32(ex->out, u32);
}

static void export_u64(Exporter *ex, U64 u64) {
	write_u64(ex->out, u64);
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

static bool export_val(Exporter *ex, Value val, Type *type, Location where) {
	export_type(ex, type);
	switch (type->kind) {
    case TYPE_VOID: break;
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
