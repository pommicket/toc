static bool cgen_decls_expr(CGenerator *g, Expression *e) {
	return true;
}

static bool cgen_decls_block(CGenerator *g, Block *b) {
	return true;
}

static bool cgen_decls_decl(CGenerator *g, Declaration *d) {
	if ((d->flags & DECL_FLAG_HAS_EXPR) && d->expr.kind == EXPR_FN && arr_len(d->idents) == 1) {
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
