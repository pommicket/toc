/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static void cgen_decls_stmt(CGenerator *g, Statement *s);
static void cgen_decls_block(CGenerator *g, Block *b);
static void cgen_decls_decl(CGenerator *g, Declaration *d);

static void cgen_decls_type(CGenerator *g, Type *type) {
	if (!(type->flags & TYPE_IS_RESOLVED)) /* non-instance constant fn parameter type */
		return;
	if (type->kind == TYPE_STRUCT) {
		StructDef *sdef = type->struc;
		if (!(sdef->flags & STRUCT_DEF_CGEN_DEFINED)) {
			/* generate struct definition */
			cgen_write(g, "struct ");
			cgen_struct_name(g, sdef);
			cgen_write(g, "{");
			cgen_nl(g);
			++g->indent_lvl;
			arr_foreach(sdef->fields, Field, f) {
				cgen_type_pre(g, &f->type, sdef->where);
				cgen_write(g, " ");
				cgen_ident_simple(g, f->name);
				cgen_type_post(g, &f->type, sdef->where);
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
}

static void cgen_single_fn_decl(CGenerator *g, FnExpr *f, U64 instance, U64 which_are_const) {
	if (cgen_should_gen_fn(f)) {
		cgen_fn_header(g, f, instance, which_are_const);
		cgen_write(g, ";");
		cgen_nl(g);
	}
}


static void cgen_decls_fn_instances(CGenerator *g, FnExpr *f) {
	Instance **data = f->instances.data;
	for (U64 i = 0; i < f->instances.cap; ++i) {
		if (f->instances.occupied[i]) {
			if (cgen_should_gen_fn((*data)->fn)) {
				(*data)->fn->c.name = f->c.name;
				(*data)->fn->c.id = f->c.id;
				cgen_single_fn_decl(g, (*data)->fn, (*data)->c.id, (*data)->val.tuple[0].u64);
				cgen_write(g, ";");
				cgen_nl(g);
			}
		}
		++data;
	}
}

static void cgen_fn_decl(CGenerator *g, FnExpr *f, Type *t) {
	FnType *fn_type = &t->fn;
	if (fn_type->constness) {
		cgen_decls_fn_instances(g, f);
	} else {
		cgen_single_fn_decl(g, f, 0, 0);
	}
}

static void cgen_decls_expr(CGenerator *g, Expression *e) {
	assert(e->flags & EXPR_FOUND_TYPE);
	cgen_recurse_subexprs(g, e, cgen_decls_expr, cgen_decls_block, cgen_decls_decl);
	switch (e->kind) {
	case EXPR_FN: {
		FnExpr *f = e->fn;
		f->c.name = NULL;
		if (!f->c.id)
			f->c.id = ++g->ident_counter;
		cgen_fn_decl(g, e->fn, &e->type);
	} break;
	case EXPR_TYPE: {
		Type *type = &e->typeval;
		cgen_decls_type(g, type);
	} break;
	case EXPR_CAST:
		cgen_decls_type(g, &e->cast.type);
		break;
	case EXPR_BINARY_OP: {
		Type *lhs_type = &e->binary.lhs->type;
		if (lhs_type->kind == TYPE_PTR)
			lhs_type = lhs_type->ptr;
	} break;
	default:
		break;
	}
}

static void cgen_decls_block(CGenerator *g, Block *b) {
	Block *prev_block = g->block;
	g->block = b;
	arr_foreach(b->stmts, Statement, s)
		cgen_decls_stmt(g, s);
	if (b->ret_expr) cgen_decls_expr(g, b->ret_expr);
	g->block = prev_block;
}

static void cgen_decls_decl(CGenerator *g, Declaration *d) {
	if (d->flags & DECL_FOREIGN) {
		cgen_write(g, "extern ");
	    if ((d->flags & DECL_IS_CONST) && (d->type.kind == TYPE_FN) && arr_len(d->idents) == 1) {
			/* foreign function declaration */
			Type *fn_types = d->type.fn.types;
			const char *foreign_name = (d->flags & DECL_FOUND_VAL)
				? d->val.fn->foreign.name
				: d->foreign.name_str;
			cgen_type_pre(g, &fn_types[0], d->where);
			cgen_write(g, " %s", foreign_name);
			cgen_write(g, "(");
			arr_foreach(fn_types, Type, t) {
				if (t == fn_types) continue;
				if (t != fn_types+1)
					cgen_write(g, ", ");
				cgen_type_pre(g, t, d->where);
				cgen_type_post(g, t, d->where);
			}
			cgen_write(g, ")");
			cgen_type_post(g, &fn_types[0], d->where);
			cgen_write(g, ";");
			if (!ident_eq_str(d->idents[0], foreign_name)) {
				cgen_write(g, "static ");
				cgen_type_pre(g, &d->type, d->where);
				cgen_write(g, " const ");
				cgen_ident(g, d->idents[0]);
				cgen_type_post(g, &d->type, d->where);
				cgen_write(g, " = %s;", foreign_name);
			}
			cgen_nl(g);
			if (d->flags & DECL_FOUND_VAL)
				d->val.fn->c.name = d->idents[0];
			return;
		} else {
			/* foreign non-function */
			const char *foreign_name = d->foreign.name_str;
			cgen_type_pre(g, &d->type, d->where);
			cgen_write(g, " %s", foreign_name);
			cgen_type_post(g, &d->type, d->where);
			cgen_write(g, ";");
			cgen_nl(g);
		}
		return;
	}
	cgen_decls_type(g, &d->type);
	if (cgen_fn_is_direct(g, d)) {
		cgen_fn_decl(g, d->expr.fn, &d->expr.type);
		cgen_recurse_subexprs(g, (&d->expr), cgen_decls_expr, cgen_decls_block, cgen_decls_decl);
	} else {
		if (d->flags & DECL_HAS_EXPR) {
			cgen_decls_expr(g, &d->expr);
		}
		if ((g->block == NULL || (g->block->flags & BLOCK_IS_NMS)) && g->fn == NULL) {
			for (int i = 0, n_idents = (int)arr_len(d->idents); i < n_idents; ++i) {
				Identifier ident = d->idents[i];
				Type *type = decl_type_at_index(d, i);
				if (!type_is_compileonly(type)) {
					if (!(d->flags & DECL_EXPORT))
						cgen_write(g, "static ");
					cgen_type_pre(g, type, d->where);
					cgen_write(g, " ");
					cgen_ident(g, ident);
					cgen_type_post(g, type, d->where);
					if (d->flags & DECL_HAS_EXPR) {
						Value *val = decl_val_at_index(d, i);
						cgen_write(g, " = ");
						cgen_val(g, *val, type, d->where);
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
}

static void cgen_decls_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_DECL:
		cgen_decls_decl(g, s->decl);
		break;
	case STMT_EXPR:
		cgen_decls_expr(g, &s->expr);
		break;
	case STMT_RET:
		if (s->ret.flags & RET_HAS_EXPR)
			cgen_decls_expr(g, &s->ret.expr);
		break;
	case STMT_INCLUDE:
		arr_foreach(s->inc.stmts, Statement, sub)
			cgen_decls_stmt(g, sub);
		break;
	}
}

static void cgen_decls_file(CGenerator *g, ParsedFile *f) {
	cgen_write(g, "/* declarations */\n");
	arr_foreach(f->stmts, Statement, s) {
		cgen_decls_stmt(g, s);
	}
}
