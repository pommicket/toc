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
				cgen_ident(g, f->name);
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

static bool cgen_fn_decl(CGenerator *g, FnExpr *f, Location where, U64 instance, U64 which_are_const) {
	if (cgen_should_gen_fn(f)) {
		if (!fn_enter(f, 0))
			return false;
		if (!cgen_fn_header(g, f, where, instance, which_are_const))
			return false;
		cgen_write(g, ";");
		cgen_nl(g);
		fn_exit(f);
	}
	return true;
}

static bool cgen_decls_fn_instances(CGenerator *g, Expression *e) {
	assert(e->kind == EXPR_FN);
	FnExpr *f = e->fn;
	assert(e->type.fn.constness);
	Instance **data = f->instances.data;
	for (U64 i = 0; i < f->instances.cap; ++i) {
		if (f->instances.occupied[i]) {
			if (cgen_should_gen_fn(&(*data)->fn)) {
				(*data)->fn.c.name = f->c.name;
				(*data)->fn.c.id = f->c.id;
				if (!cgen_fn_decl(g, &(*data)->fn, e->where, (*data)->c.id, (*data)->val.tuple[0].u64))
					return false;
				cgen_write(g, ";");
				cgen_nl(g);
			}
		}
		++data;
	}
	return true;
}

static bool cgen_decls_expr(CGenerator *g, Expression *e) {
	cgen_recurse_subexprs(g, e, cgen_decls_expr, cgen_decls_block, cgen_decls_decl);
	switch (e->kind) {
	case EXPR_FN: {
		FnExpr *f = e->fn;
		f->c.name = NULL;
		if (!f->c.id)
			f->c.id = ++g->ident_counter;
		FnType *fn_type = &e->type.fn;
		if (fn_type->constness) {
			if (!cgen_decls_fn_instances(g, e))
				return false;
		} else {
			if (!cgen_fn_decl(g, e->fn, e->where, 0, 0))
				return false;
		}
	} break;
	case EXPR_TYPE: {
		Type *type = &e->typeval;
		if (!cgen_decls_type(g, type))
			return false;
	} break;
	case EXPR_CAST:
		if (!cgen_decls_type(g, &e->cast.type))
			return false;
	default:
		break;
	}
	
	return true;
}

static bool cgen_decls_block(CGenerator *g, Block *b) {
	Block *prev = g->block;
	if (!cgen_block_enter(g, b))
		return false;
	arr_foreach(b->stmts, Statement, s)
		if (!cgen_decls_stmt(g, s))
			return false;
	if (b->ret_expr && !cgen_decls_expr(g, b->ret_expr))
		return false;
	cgen_block_exit(g, prev);
	return true;
}

static bool cgen_decls_decl(CGenerator *g, Declaration *d) {
	if (!cgen_decls_type(g, &d->type))
		return false;
	if (cgen_fn_is_direct(g, d)) {
		d->expr.fn->c.name = d->idents[0];
		if (d->expr.type.fn.constness) {
			if (!cgen_decls_fn_instances(g, &d->expr))
				return false;
		} else {
			if (!cgen_fn_decl(g, d->expr.fn, d->expr.where, 0, 0))
				return false;
		}
		cgen_recurse_subexprs(g, (&d->expr), cgen_decls_expr, cgen_decls_block, cgen_decls_decl);
	} else {
		if (d->flags & DECL_HAS_EXPR) {
			if (!cgen_decls_expr(g, &d->expr))
				return false;
		}
		if (g->block == NULL && g->fn == NULL) {
			for (int i = 0, n_idents = (int)arr_len(d->idents); i < n_idents; ++i) {
				Identifier ident = d->idents[i];
				Type *type = decl_type_at_index(d, i);
				if (!type_is_compileonly(type)) {
					if (ident->export_name) {
						cgen_write(g, "extern ");
					} else
						cgen_write(g, "static ");
					if (!cgen_type_pre(g, type, d->where))
						return false;
					cgen_write(g, " ");
					cgen_ident(g, ident);
					if (!cgen_type_post(g, type, d->where))
						return false;
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
		if (!cgen_decls_decl(g, &s->decl))
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
