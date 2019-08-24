static bool cgen_types_stmt(CGenerator *g, Statement *s);
static bool cgen_types_fn(CGenerator *g, FnExpr *f) {
	bool ret = true;
	/* assign an ID to the function */
	if (f->name) {
		f->id = f->name->c_fn_reps++;
	} else {
		f->id = g->anon_fn_count++;
	}
	
	if (!cgen_fn_header(g, f)) return false;
	cgen_writeln(g, ";");
	Block *prev_block = g->block;
	cgen_block_enter(g, &f->body);
	arr_foreach(&f->body.stmts, Statement, s) {
		if (!cgen_types_stmt(g, s))
			ret = false;
	}
	cgen_block_exit(g, prev_block);
	return ret;
}


static bool cgen_types_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_FN: {
		if (e->fn.name && g->block == NULL) { /* write named function prototypes in global scope to header file */
			g->writing_to = CGEN_WRITING_TO_H;
		}
		if (!cgen_types_fn(g, &e->fn))
			return false;
		g->writing_to = CGEN_WRITING_TO_C;
	} break;
	case EXPR_CALL:
		if (!cgen_types_expr(g, e->call.fn))
			return false;
		arr_foreach(&e->call.args, Expression, arg) {
			if (!cgen_types_expr(g, arg))
				return false;
		}
		break;
	default: /* TODO */ break;
	}
	return true;
}


static bool cgen_types_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_EXPR:
		if (!cgen_types_expr(g, &s->expr))
			return false;
		break;
	case STMT_DECL: {
		Declaration *d = &s->decl;
		if ((d->flags & DECL_FLAG_HAS_EXPR) && (d->flags & DECL_FLAG_CONST)) {
			/* e.g. foo @= fn() {}; (we want to set the function's name to "foo") */
			if (d->expr.kind == EXPR_FN) {
				d->expr.fn.name = *(Identifier*)d->idents.data;
			}
		}
		cgen_types_expr(g, &d->expr);
	} break;
			
	}
	return true;
}

static bool cgen_types(CGenerator *g, ParsedFile *f) {
	arr_foreach(&f->stmts, Statement, s) {
		cgen_types_stmt(g, s);
	}
	return true;
}
