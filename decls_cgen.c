/* C declarations of functions and global variables */
static bool cgen_decl_fn(CGenerator *g, FnExpr *f) {
	/* assign an ID to the function */
	if (f->name && g->block == NULL) {
		f->id = f->name->c_fn_reps++;
	} else {
		f->id = g->anon_fn_count++;
	}
	
	if (!cgen_fn_header(g, f)) return false;
	cgen_writeln(g, ";");
	return true;
}

static bool cgen_decls_stmt(CGenerator *g, Statement *s);

static bool cgen_decls_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_FN: {
		FnExpr *f = &e->fn;
		if (f->name && g->block == NULL) { /* write named function prototypes in global scope to header file */
			g->writing_to = CGEN_WRITING_TO_H;
		} else {
			g->writing_to = CGEN_WRITING_TO_C;
		}
		if (!cgen_decl_fn(g, f))
			return false;
		g->writing_to = CGEN_WRITING_TO_C;

		
		bool ret = true;
		Block *prev_block = g->block;
		cgen_block_enter(g, &f->body);
		arr_foreach(&f->body.stmts, Statement, s) {
			if (!cgen_decls_stmt(g, s))
				ret = false;
		}
		cgen_block_exit(g, prev_block);
		return ret;
	}
	case EXPR_CALL:
		if (!cgen_decls_expr(g, e->call.fn))
			return false;
		arr_foreach(&e->call.args, Expression, arg) {
			if (!cgen_decls_expr(g, arg))
				return false;
		}
		break;
	default: break;
	}
	return true;
}


static bool cgen_expr(CGenerator *g, Expression *e);
static bool cgen_decls_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_EXPR:
		return cgen_decls_expr(g, &s->expr);
	case STMT_DECL: {
		Declaration *d = &s->decl;
		bool is_const_fn = (d->flags & DECL_FLAG_HAS_EXPR) && (d->flags & DECL_FLAG_CONST)
			&& d->expr.kind == EXPR_FN;
		
		if (is_const_fn) {
			/* e.g. foo @= fn() {}; (we want to set the function's name to "foo") */
			d->expr.fn.name = *(Identifier*)d->idents.data;
		}

		if (d->flags & DECL_FLAG_HAS_EXPR) {
			cgen_decls_expr(g, &d->expr);
		}
		
		if (!is_const_fn) {
			if (g->block == NULL) {
				/* declare this/these global variable(s) */
				arr_foreach(&d->idents, Identifier, i) {
					if (!cgen_type_pre(g, &d->type)) return false;
					cgen_ident(g, *i, NULL);
					if (!cgen_type_post(g, &d->type)) return false;
					if (d->flags & DECL_FLAG_HAS_EXPR) { /* TODO: check if expr is const */
						cgen_write_space(g);
						cgen_write(g, "=");
						cgen_write_space(g);
						if (!cgen_expr(g, &d->expr))
							return false;
					}
					cgen_write(g, ";");
					cgen_write_space(g);
				}
				cgen_writeln(g, "");
			}
		}
		
	} break;
	}
	return true;
}

static bool cgen_decls_file(CGenerator *g, ParsedFile *f) {
	arr_foreach(&f->stmts, Statement, s) {
		if (!cgen_decls_stmt(g, s))
			return false;
	}
	return true;
}
