/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static bool cgen_decls_stmt(CGenerator *g, Statement *s);
static bool cgen_decls_block(CGenerator *g, Block *b);
static bool cgen_decls_decl(CGenerator *g, Declaration *d);

static bool cgen_decls_type(CGenerator *g, Type *type) {
	if (!(type->flags & TYPE_IS_RESOLVED)) /* non-instance constant fn parameter type */
		return true;
	if (type->kind == TYPE_STRUCT) {
		StructDef *sdef = type->struc;
		if (!(sdef->flags & STRUCT_DEF_CGEN_DEFINED)) {
			/* generate struct definition */
			cgen_write(g, "struct ");
			if (sdef->name) {
				cgen_ident(g, sdef->name);
			} else {
				assert(sdef->c.id);
				cgen_ident_id(g, sdef->c.id);
			}
			cgen_write(g, "{");
			cgen_nl(g);
			++g->indent_lvl;
			arr_foreach(sdef->fields, Field, f) {
				if (!cgen_type_pre(g, &f->type, sdef->where)) return false;
				cgen_write(g, " ");
				cgen_ident_simple(g, f->name);
				if (!cgen_type_post(g, &f->type, sdef->where)) return false;
				cgen_write(g, ";");
				cgen_nl(g);
			}
			--g->indent_lvl;
			cgen_write(g, "};");
			cgen_nl(g);
			sdef->flags |= STRUCT_DEF_CGEN_DEFINED;
		}
	}
	cgen_recurse_subtypes(cgen_decls_type, g, type);
	return true;
}

static bool cgen_single_fn_decl(CGenerator *g, FnExpr *f, U64 instance, U64 which_are_const) {
	if (cgen_should_gen_fn(f)) {
		if (!cgen_fn_header(g, f, instance, which_are_const))
			return false;
		cgen_write(g, ";");
		cgen_nl(g);
	}
	return true;
}


static bool cgen_decls_fn_instances(CGenerator *g, FnExpr *f) {
	Instance **data = f->instances.data;
	for (U64 i = 0; i < f->instances.cap; ++i) {
		if (f->instances.occupied[i]) {
			if (cgen_should_gen_fn((*data)->fn)) {
				(*data)->fn->c.name = f->c.name;
				(*data)->fn->c.id = f->c.id;
				if (!cgen_single_fn_decl(g, (*data)->fn, (*data)->c.id, (*data)->val.tuple[0].u64))
					return false;
				cgen_write(g, ";");
				cgen_nl(g);
			}
		}
		++data;
	}
	return true;
}

static bool cgen_fn_decl(CGenerator *g, FnExpr *f, Type *t) {
	FnType *fn_type = &t->fn;
	if (fn_type->constness) {
		if (!cgen_decls_fn_instances(g, f))
			return false;
	} else {
		if (!cgen_single_fn_decl(g, f, 0, 0))
			return false;
	}
	return true;
}

static bool cgen_decls_expr(CGenerator *g, Expression *e) {
	assert(e->flags & EXPR_FOUND_TYPE);
	cgen_recurse_subexprs(g, e, cgen_decls_expr, cgen_decls_block, cgen_decls_decl);
	switch (e->kind) {
	case EXPR_FN: {
		FnExpr *f = e->fn;
		f->c.name = NULL;
		if (!f->c.id)
			f->c.id = ++g->ident_counter;
		if (!cgen_fn_decl(g, e->fn, &e->type))
			return false;
	} break;
	case EXPR_TYPE: {
		Type *type = &e->typeval;
		if (!cgen_decls_type(g, type))
			return false;
	} break;
	case EXPR_CAST:
		if (!cgen_decls_type(g, &e->cast.type))
			return false;
		break;
	case EXPR_BINARY_OP: {
		Type *lhs_type = &e->binary.lhs->type;
		if (lhs_type->kind == TYPE_PTR)
			lhs_type = lhs_type->ptr;
	} break;
	default:
		break;
	}
	
	return true;
}

static bool cgen_decls_block(CGenerator *g, Block *b) {
	Block *prev_block = g->block;
	g->block = b;
	arr_foreach(b->stmts, Statement, s)
		if (!cgen_decls_stmt(g, s))
			return false;
	if (b->ret_expr && !cgen_decls_expr(g, b->ret_expr))
		return false;
	g->block = prev_block;
	return true;
}

static bool cgen_decls_decl(CGenerator *g, Declaration *d) {
	if (d->flags & DECL_FOREIGN) {
		cgen_write(g, "extern ");
	    if ((d->flags & DECL_IS_CONST) && (d->type.kind == TYPE_FN) && arr_len(d->idents) == 1) {
			/* foreign function declaration */
			Type *fn_types = d->type.fn.types;
			const char *foreign_name = (d->flags & DECL_FOUND_VAL)
				? d->val.fn->foreign.name
				: d->foreign.name_str;
			if (!cgen_type_pre(g, &fn_types[0], d->where))
				return false;
			cgen_write(g, " %s", foreign_name);
			cgen_write(g, "(");
			arr_foreach(fn_types, Type, t) {
				if (t == fn_types) continue;
				if (t != fn_types+1)
					cgen_write(g, ", ");
				if (!cgen_type_pre(g, t, d->where))
					return false;
				if (!cgen_type_post(g, t, d->where))
					return false;
			}
			cgen_write(g, ")");
			if (!cgen_type_post(g, &fn_types[0], d->where))
				return false;
			cgen_write(g, ";");
			if (!ident_eq_str(d->idents[0], foreign_name)) {
				cgen_write(g, "static ");
				if (!cgen_type_pre(g, &d->type, d->where))
					return false;
				cgen_write(g, " ");
				cgen_ident(g, d->idents[0]);
				if (!cgen_type_post(g, &d->type, d->where))
					return false;
				cgen_write(g, " = %s;", foreign_name);
			}
			cgen_nl(g);
			if (d->flags & DECL_FOUND_VAL)
				d->val.fn->c.name = d->idents[0];
			return true;
		} else {
			/* foreign non-function */
			const char *foreign_name = d->foreign.name_str;
			if (!cgen_type_pre(g, &d->type, d->where))
				return false;
			cgen_write(g, " %s", foreign_name);
			if (!cgen_type_post(g, &d->type, d->where))
				return false;
			cgen_write(g, ";");
			cgen_nl(g);
		}
		return true;
	}
	if (!cgen_decls_type(g, &d->type))
		return false;
	if (cgen_fn_is_direct(g, d)) {
		if (!cgen_fn_decl(g, d->expr.fn, &d->expr.type))
			return false;
		cgen_recurse_subexprs(g, (&d->expr), cgen_decls_expr, cgen_decls_block, cgen_decls_decl);
	} else {
		if (d->flags & DECL_HAS_EXPR) {
			if (!cgen_decls_expr(g, &d->expr))
				return false;
		}
		if ((g->block == NULL || (g->block->flags & BLOCK_IS_NMS)) && g->fn == NULL) {
			for (int i = 0, n_idents = (int)arr_len(d->idents); i < n_idents; ++i) {
				Identifier ident = d->idents[i];
				Type *type = decl_type_at_index(d, i);
				if (!type_is_compileonly(type)) {
					if (!(d->flags & DECL_EXPORT))
						cgen_write(g, "static ");
					if (!cgen_type_pre(g, type, d->where))
						return false;
					cgen_write(g, " ");
					cgen_ident(g, ident);
					if (!cgen_type_post(g, type, d->where))
						return false;
					if (d->flags & DECL_HAS_EXPR) {
						Value *val = decl_val_at_index(d, i);
						cgen_write(g, " = ");
						if (!cgen_val(g, *val, type, d->where))
							return false;
					} else {
						cgen_write(g, " = ");
						cgen_zero_value(g, type);
					}
					cgen_write(g, ";");
					cgen_nl(g);
				}
			}
		}
	}
	return true;
}

static bool cgen_decls_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_DECL:
		if (!cgen_decls_decl(g, s->decl))
			return false;
		break;
	case STMT_EXPR:
		if (!cgen_decls_expr(g, &s->expr))
			return false;
		break;
	case STMT_RET:
		if (s->ret.flags & RET_HAS_EXPR)
			if (!cgen_decls_expr(g, &s->ret.expr))
				return false;
		break;
	case STMT_INCLUDE:
		arr_foreach(s->inc.stmts, Statement, sub)
			if (!cgen_decls_stmt(g, sub))
				return false;
		break;
	}
	return true;
}

static bool cgen_decls_file(CGenerator *g, ParsedFile *f) {
	cgen_write(g, "/* declarations */\n");
	arr_foreach(f->stmts, Statement, s) {
		if (!cgen_decls_stmt(g, s))
			return false;
	}
	return true;
}
