static bool cgen_decls_stmt(CGenerator *g, Statement *s);
static bool cgen_decls_block(CGenerator *g, Block *b);

static bool cgen_decls_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_CALL:
		if (e->call.fn->kind == EXPR_IDENT) {
			IdentDecl *idecl = ident_decl(e->call.fn->ident);
			if (idecl->kind == IDECL_DECL &&
				idecl->decl->expr.kind == EXPR_FN) {
				/* directly calling a function; might need to generate a copy of this function */
				/* TODO ASDF */
			}
		}
		break;
	case EXPR_FN:
		e->fn.c.name = NULL;
	    e->fn.c.id = g->ident_counter++;
		fn_enter(&e->fn, 0);
		if (!cgen_fn_header(g, &e->fn, e->where))
			return false;
		cgen_write(g, ";");
		cgen_nl(g);
		fn_exit(&e->fn);
		break;
	default:
		break;
	}
	cgen_recurse_subexprs(g, e, cgen_decls_expr, cgen_decls_block);
	
	return true;
}

static bool cgen_decls_block(CGenerator *g, Block *b) {
	Block *prev = g->block;
	if (!cgen_block_enter(g, b))
		return false;
	arr_foreach(b->stmts, Statement, s)
		cgen_decls_stmt(g, s);
	cgen_block_exit(g, prev);
	return true;
}

static bool cgen_decls_decl(CGenerator *g, Declaration *d) {
	if (cgen_fn_is_direct(g, d)) {
		if (!fn_has_any_const_params(&d->expr.fn)) {
			d->expr.fn.c.name = d->idents[0];
			fn_enter(&d->expr.fn, 0);
			if (!cgen_fn_header(g, &d->expr.fn, d->where))
				return false;
			cgen_write(g, ";");
			cgen_nl(g);
		}
		if (!cgen_decls_block(g, &d->expr.fn.body))
			return false;
		fn_exit(&d->expr.fn);
	} else if ((d->flags & DECL_HAS_EXPR) && !(d->flags & DECL_IS_CONST)) {
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
