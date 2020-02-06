/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static bool cgen_sdecls_stmt(CGenerator *g, Statement *s);
static bool cgen_sdecls_decl(CGenerator *g, Declaration *d);
static bool cgen_sdecls_expr(CGenerator *g, Expression *e);

/* i is the name for this type, NULL if not available */
/* ALWAYS RETURNS TRUE. it just returns a bool for cgen_recurse_into_type to work */
static bool cgen_sdecls_type(CGenerator *g, Type *type) {
	if (!(type->flags & TYPE_IS_RESOLVED)) /* non-instance constant fn parameter type */
		return true;
	if (type->kind == TYPE_STRUCT) {
		StructDef *sdef = type->struc;
		/* we'll actually define the struct later; here we can just declare it */
	
		if (sdef->flags & STRUCT_DEF_CGEN_DECLARED) {
			/* we've already done this */
		} else {
			cgen_write(g, "struct ");
			if (sdef->name) {
				cgen_ident(g, sdef->name);
			} else {
				IdentID id = ++g->ident_counter;
				cgen_ident_id(g, id);
				sdef->c.id = id;
			}
			cgen_write(g, ";");
			cgen_nl(g);
			sdef->flags |= STRUCT_DEF_CGEN_DECLARED;
		}
	}
	cgen_recurse_subtypes(cgen_sdecls_type, g, type);
	return true;
}

static bool cgen_sdecls_block(CGenerator *g, Block *b) {
	arr_foreach(b->stmts, Statement, s)
		if (!cgen_sdecls_stmt(g, s))
			return false;
	if (b->ret_expr && !cgen_sdecls_expr(g, b->ret_expr))
		return false;
	return true;
}

static bool cgen_sdecls_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_CAST:
		cgen_sdecls_type(g, &e->cast.type);
		break;
	case EXPR_FN:
		/* needs to go before decls_cgen.c... */
		e->fn->c.id = ++g->ident_counter;
		break;
	case EXPR_TYPE:
		if (!cgen_sdecls_type(g, &e->typeval))
			return false;
		break;
	case EXPR_NMS:
		e->nms.c.id = 0;
		break;
	default: break;
	}
	cgen_recurse_subexprs(g, e, cgen_sdecls_expr, cgen_sdecls_block, cgen_sdecls_decl);
	return true;

}

static bool cgen_sdecls_decl(CGenerator *g, Declaration *d) {
	if (d->flags & DECL_FOREIGN) {
		/* handled by cgen_decls */
		return true;
	}
	cgen_sdecls_type(g, &d->type);
	if (cgen_fn_is_direct(g, d)) {
		d->expr.fn->c.name = d->idents[0];
	}
	for (int idx = 0; idx < (int)arr_len(d->idents); ++idx) {
		Type *type = decl_type_at_index(d, idx);
		Value *val = decl_val_at_index(d, idx);
		if (type_is_builtin(type, BUILTIN_TYPE)) {
			if (!cgen_sdecls_type(g, val->type))
				return false;
		}
	}
	if (d->flags & DECL_HAS_EXPR) {
		if (!cgen_sdecls_expr(g, &d->expr))
			return false;
	}
	return true;
}

static bool cgen_sdecls_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_DECL:
		if (!cgen_sdecls_decl(g, &s->decl))
			return false;
		break;
	case STMT_EXPR:
		if (!cgen_sdecls_expr(g, &s->expr))
			return false;
		break;
	case STMT_RET:
		if (s->ret.flags & RET_HAS_EXPR)
			if (!cgen_sdecls_expr(g, &s->ret.expr))
				return false;
		break;
	case STMT_INCLUDE:
		arr_foreach(s->inc.stmts, Statement, sub)
			if (!cgen_sdecls_stmt(g, sub))
				return false;
	    break;
	}
	return true;
}

static bool cgen_sdecls_file(CGenerator *g, ParsedFile *f) {
	arr_foreach(f->stmts, Statement, s) {
		if (!cgen_sdecls_stmt(g, s))
			return false;
	}
	return true;
}
