static bool cgen_decls_stmt(CGenerator *g, Statement *s);
static bool cgen_decls_block(CGenerator *g, Block *b);

static bool cgen_decls_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_UNARY_OP:
		if (!cgen_decls_expr(g, e->unary.of))
			return false;
		break;
	case EXPR_BINARY_OP:
		if (!cgen_decls_expr(g, e->binary.lhs)
			|| !cgen_decls_expr(g, e->binary.rhs))
			return false;
		break;
	case EXPR_CAST:
		if (!cgen_decls_expr(g, e->cast.expr))
			return false;
		break;
	case EXPR_CALL:
		if (!cgen_decls_expr(g, e->call.fn))
			return false;
		arr_foreach(e->call.arg_exprs, Expression, a)
			if (!cgen_decls_expr(g, a))
				return false;
		break;
	case EXPR_BLOCK:
		if (!cgen_decls_block(g, &e->block))
			return false;
		break;
	case EXPR_IF:
		if (e->if_.cond)
			if (!cgen_decls_expr(g, e->if_.cond))
				return false;
		if (!cgen_decls_block(g, &e->if_.body))
			return false;
		if (e->if_.next_elif)
			if (!cgen_decls_expr(g, e->if_.next_elif))
				return false;
		break;
	case EXPR_WHILE:
		if (e->while_.cond)
			if (!cgen_decls_expr(g, e->while_.cond))
				return false;
		if (!cgen_decls_block(g, &e->while_.body))
			return false;
		break;
	case EXPR_TUPLE:
		arr_foreach(e->tuple, Expression, x)
			if (!cgen_decls_expr(g, x))
				return false;
		break;
	case EXPR_FN:
		e->fn.c.name = NULL;
	    e->fn.c.id = g->ident_counter++;
		if (!cgen_fn_header(g, &e->fn, e->where))
			return false;
		cgen_write(g, ";\n");
		if (!cgen_decls_block(g, &e->fn.body))
			return false;
		break;
	case EXPR_DIRECT:
	case EXPR_NEW:
	case EXPR_IDENT:
	case EXPR_LITERAL_BOOL:
	case EXPR_LITERAL_INT:
	case EXPR_LITERAL_STR:
	case EXPR_LITERAL_CHAR:
	case EXPR_LITERAL_FLOAT:
		break;
	}
	return true;
}

static bool cgen_decls_block(CGenerator *g, Block *b) {
	Block *prev = g->block;
	cgen_block_enter(g, b);
	arr_foreach(b->stmts, Statement, s)
		cgen_decls_stmt(g, s);
	cgen_block_exit(g, prev);
	return true;
}

static bool cgen_decls_decl(CGenerator *g, Declaration *d) {
	if (cgen_fn_is_direct(g, d)) {
		d->expr.fn.c.name = d->idents[0];
		if (!cgen_fn_header(g, &d->expr.fn, d->where))
			return false;
		cgen_write(g, ";\n");
		if (!cgen_decls_block(g, &d->expr.fn.body))
			return false;
	} else if (d->flags & DECL_FLAG_HAS_EXPR) {
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
		break;
	case STMT_RET:
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
