/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static bool cgen_decls_stmt(CGenerator *g, Statement *s);
static bool cgen_decls_block(CGenerator *g, Block *b);
static bool cgen_decls_decl(CGenerator *g, Declaration *d);

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
			
				if (!cgen_fn_header(g, &(*data)->fn, e->where, (*data)->c.id, (*data)->val.tuple[0].u64))
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
			f->c.id = g->ident_counter++;
		FnType *fn_type = &e->type.fn;
		if (fn_type->constness) {
			if (!cgen_decls_fn_instances(g, e))
				return false;
		} else {
			if (cgen_should_gen_fn(e->fn)) {
				fn_enter(e->fn, 0);
				if (!cgen_fn_header(g, e->fn, e->where, 0, 0))
					return false;
				cgen_write(g, ";");
				cgen_nl(g);
				fn_exit(e->fn);
			}
		}
	} break;
	case EXPR_TYPE: {
		Type *type = &e->typeval;
		if (type->kind == TYPE_STRUCT) {
			StructDef *sdef = type->struc;
			if (!(sdef->flags & STRUCT_DEF_CGENERATED)) {
				/* generate struct definition */
				cgen_write(g, "struct ");
				if (sdef->c.name)
					cgen_ident(g, sdef->c.name);
				else
					cgen_ident_id(g, sdef->c.id);
				cgen_write(g, "{");
				cgen_nl(g);
				++g->indent_lvl;
				arr_foreach(sdef->fields, Field, f) {
					if (!cgen_type_pre(g, f->type, e->where)) return false;
					cgen_write(g, " ");
					cgen_ident(g, f->name);
					if (!cgen_type_post(g, f->type, e->where)) return false;
					cgen_write(g, ";");
					cgen_nl(g);
				}
				--g->indent_lvl;
				cgen_write(g, "};");
				cgen_nl(g);
				sdef->flags |= STRUCT_DEF_CGENERATED;
			}
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
	if (cgen_fn_is_direct(g, d)) {
		d->expr.fn->c.name = d->idents[0];
		if (d->expr.type.fn.constness) {
			if (!cgen_decls_fn_instances(g, &d->expr))
				return false;
		} else {
			if (cgen_should_gen_fn(d->expr.fn)) {
				fn_enter(d->expr.fn, 0);
				if (!cgen_fn_header(g, d->expr.fn, d->where, 0, 0))
					return false;
				cgen_write(g, ";");
				cgen_nl(g);
				fn_exit(d->expr.fn);
			}
		}
		cgen_recurse_subexprs(g, (&d->expr), cgen_decls_expr, cgen_decls_block, cgen_decls_decl);
	} else if (d->flags & DECL_HAS_EXPR) {
		if (!cgen_decls_expr(g, &d->expr))
			return false;
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
