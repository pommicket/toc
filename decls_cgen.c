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
		if (!fn_enter(f, 0))
			return false;
		if (!cgen_fn_header(g, f, instance, which_are_const))
			return false;
		cgen_write(g, ";");
		cgen_nl(g);
		fn_exit(f);
	}
	return true;
}


static bool cgen_decls_fn_instances(CGenerator *g, FnExpr *f) {
	Instance **data = f->instances.data;
	for (U64 i = 0; i < f->instances.cap; ++i) {
		if (f->instances.occupied[i]) {
			if (cgen_should_gen_fn(&(*data)->fn)) {
				(*data)->fn.c.name = f->c.name;
				(*data)->fn.c.id = f->c.id;
				if (!cgen_single_fn_decl(g, &(*data)->fn, (*data)->c.id, (*data)->val.tuple[0].u64))
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
	if (f->c.declared) return true;
	f->c.declared = true;
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
		if (e->binary.op == BINARY_DOT && type_is_builtin(lhs_type, BUILTIN_PKG)) {
			Identifier ident = e->binary.dot.pkg_ident;
			IdentDecl *idecl = ident_decl(ident);
			assert(idecl);
			assert(idecl->kind == IDECL_DECL);
			Declaration *d = idecl->decl;
			if (e->type.kind == TYPE_FN) {
				FnExpr *f = NULL;
				if (d->flags & DECL_FOUND_VAL)
					f = d->val.fn;
				else if (d->expr.kind == EXPR_FN)
					f = d->expr.fn;
				if (f) {
					if (fn_has_any_const_params(f)) {
						/* declare the instances */
						f->c.name = ident;
						if (!cgen_fn_decl(g, f, &e->type))
							return false;
					} else {
						bool out_param = cgen_uses_ptr(&f->ret_type);
						/* extern function declaration */
						cgen_write(g, "extern ");
						if (out_param) {
							cgen_write(g, "void");
						} else {
							if (!cgen_type_pre(g, &f->ret_type, e->where))
								return false;
						}
						cgen_write(g, " ");
						cgen_ident(g, ident);
						if (!out_param) {
							if (!cgen_type_post(g, &f->ret_type, e->where))
								return false;
						}
						if (!cgen_fn_args(g, f, 0, 0))
							return false;
						cgen_write(g, ";");
						cgen_nl(g);
					}
					break;
				}
			}
			/* extern variable declaration */
			cgen_write(g, "extern ");
			if (!cgen_type_pre(g, &e->type, e->where))
				return false;
			cgen_write(g, " ");
			cgen_ident(g, ident);
			if (!cgen_type_post(g, &e->type, e->where))
				return false;
			cgen_write(g, ";");
			cgen_nl(g);
		}
	} break;
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
		if (!cgen_fn_decl(g, d->expr.fn, &d->expr.type))
			return false;
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
