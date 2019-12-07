/*
  Copyright (C) 2019 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static bool typedefs_stmt(CGenerator *g, Statement *s);
static bool typedefs_decl(CGenerator *g, Declaration *d);
static bool typedefs_expr(CGenerator *g, Expression *e);

static bool typedefs_block(CGenerator *g, Block *b) {
	Block *prev = g->block;
	if (!cgen_block_enter(g, b))
		return false;
	arr_foreach(b->stmts, Statement, s)
		if (!typedefs_stmt(g, s))
			return false;
	if (b->ret_expr && !typedefs_expr(g, b->ret_expr))
		return false;
	cgen_block_exit(g, prev);
	return true;
}

static bool typedefs_expr(CGenerator *g, Expression *e) {
	cgen_recurse_subexprs(g, e, typedefs_expr, typedefs_block, typedefs_decl);
	if (e->kind == EXPR_FN) {
		/* needs to go before decls_cgen.c... */
		e->fn.c.id = g->ident_counter++;
	}
	if (e->kind == EXPR_TYPE && e->typeval.kind == TYPE_STRUCT) {
		StructDef *sdef = e->typeval.struc;
		if (sdef->c.id || sdef->c.name) {
			/* we've already done this */
		} else {
			cgen_write(g, "struct ");
			IdentID id = g->ident_counter++;
			cgen_ident_id(g, id);
			sdef->c.id = id;
			cgen_write(g, ";");
		}
	}
	return true;

}

static bool typedefs_decl(CGenerator *g, Declaration *d) {
	if (cgen_fn_is_direct(g, d)) {
		d->expr.fn.c.name = d->idents[0];
	}
	for (int idx = 0; idx < (int)arr_len(d->idents); idx++) {
		Identifier i = d->idents[idx];
		Type *type = decl_type_at_index(d, idx);
		Value *val = decl_val_at_index(d, idx);
		if (type->kind == TYPE_TYPE) {
			/* generate typedef */
			IdentID id = 0;
			if (g->block != NULL || g->fn != NULL)
				id = g->ident_counter++;
			if (val->type->kind == TYPE_STRUCT) {
				/* we'll actually define the struct later; here we can just declare it */
				StructDef *sdef = val->type->struc;
				if (sdef->c.id || sdef->c.name) {
					/* we've already done this */
				} else {
					cgen_write(g, "struct ");
					if (id) {
						cgen_ident_id(g, id);
						sdef->c.id = id;
					} else {
						cgen_ident(g, i);
						sdef->c.name = i;
					}
					cgen_write(g, ";");
				}
			} else {
				cgen_write(g, "typedef ");
				if (!cgen_type_pre(g, val->type, d->where)) return false;
				cgen_write(g, " ");
				if (id) {
					cgen_ident_id(g, id);
				} else {
					cgen_ident(g, i);
				}
				if (val->type->kind != TYPE_STRUCT) {
					if (!cgen_type_post(g, val->type, d->where)) return false;
				}
				cgen_write(g, ";");
			}
			cgen_nl(g);
		}
	}
	if (d->flags & DECL_HAS_EXPR) {
		if (!typedefs_expr(g, &d->expr))
			return false;
	}
	return true;
}

static bool typedefs_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_DECL:
		if (!typedefs_decl(g, &s->decl))
			return false;
		break;
	case STMT_EXPR:
		if (!typedefs_expr(g, &s->expr))
			return false;
		break;
	case STMT_RET:
		if (s->ret.flags & RET_HAS_EXPR)
			if (!typedefs_expr(g, &s->ret.expr))
				return false;
		break;
	}
	return true;
}

static bool typedefs_file(CGenerator *g, ParsedFile *f) {
	arr_foreach(f->stmts, Statement, s) {
		if (!typedefs_stmt(g, s))
			return false;
	}
	return true;
}
