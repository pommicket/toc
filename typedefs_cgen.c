static bool typedefs_stmt(CGenerator *g, Statement *s);

static bool typedefs_block(CGenerator *g, Block *b) {
	Block *prev = g->block;
	if (!cgen_block_enter(g, b))
		return false;
	arr_foreach(b->stmts, Statement, s)
		typedefs_stmt(g, s);
	cgen_block_exit(g, prev);
	return true;
}

static bool typedefs_expr(CGenerator *g, Expression *e) {
	cgen_recurse_subexprs(g, e, typedefs_expr, typedefs_block);
	if (e->kind == EXPR_FN) {
		/* needs to go before decls_cgen.c... */
		e->fn.c.id = g->ident_counter++;
	}
	return true;

}

static bool typedefs_decl(CGenerator *g, Declaration *d) {
	d->c.ids = NULL;
	if (cgen_fn_is_direct(g, d)) {
		d->expr.fn.c.name = d->idents[0];
	}
	for (int idx = 0; idx < (int)arr_len(d->idents); idx++) {
		Identifier i = d->idents[idx];
		Type *type = decl_type_at_index(d, idx);
		Value *val = decl_val_at_index(d, idx);
		if (type->kind == TYPE_TYPE) {
			if (d->c.ids == NULL)
				d->c.ids = calloc(arr_len(d->idents), sizeof *d->c.ids);
			/* generate typedef */
			IdentID id = 0;
			if (g->block != NULL) id = d->c.ids[idx] = g->ident_counter++;
			if (val->type->kind == TYPE_STRUCT) {
				/* we'll actually define the struct later; here we can just declare it */
				cgen_write(g, "struct ");
				if (g->block == NULL) {
					/* we can refer to this by its name */
					cgen_ident(g, i);
				} else {
					/* we need to use an ID ): */
					cgen_ident_id(g, id);
				}
				cgen_write(g, ";");
				cgen_nl(g);
				continue;
			}
			cgen_write(g, "typedef ");
			if (!cgen_type_pre(g, val->type, d->where)) return false;
			cgen_write(g, " ");
			if (g->block == NULL) {
				/* we can refer to this by its name */
				cgen_ident(g, i);
			} else {
				/* we need to use an ID ): */
				cgen_ident_id(g, id);
			}
			if (val->type->kind != TYPE_STRUCT) {
				if (!cgen_type_post(g, val->type, d->where)) return false;
			}
			cgen_write(g, ";");
			cgen_nl(g);
		}
	}
	if (d->flags & DECL_HAS_EXPR) {
		if (!(d->flags & DECL_IS_CONST) || d->expr.kind == EXPR_FN) {
			if (!typedefs_expr(g, &d->expr))
				return false;
		}
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
