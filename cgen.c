static bool cgen_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_INT_LITERAL:
		cgen_write(g, "%lld", e->intl);
		break;
	case EXPR_FLOAT_LITERAL:
		/* TODO: more precision */
		cgen_write(g, "%f", e->floatl);
		break;
	case EXPR_STR_LITERAL:
		cgen_write(g, "\"");
		/* OPTIM: Maybe don't use i? this will probably be optimized by the compiler though... */
		for (size_t i = 0; i < e->strl.len; i++) {
			/* TODO: Print ordinary characters nicely */
			cgen_write(g, "\\x%02x", e->strl.str[i]);
		}
		cgen_write(g, "\"");
		break;
	case EXPR_IDENT:
		if (!cgen_ident(g, e->ident, &e->where)) return false;
		break;
	case EXPR_BINARY_OP:
		cgen_write(g, "(");
		if (!cgen_expr(g, e->binary.lhs)) return false;
		switch (e->binary.op) {
		case BINARY_PLUS:
			cgen_write(g, "+");
			break;
		case BINARY_MINUS:
			cgen_write(g, "-");
			break;
		}
		if (!cgen_expr(g, e->binary.rhs)) return false;
		cgen_write(g, ")");
		break;
	case EXPR_UNARY_OP:
		cgen_write(g, "(");
		switch (e->unary.op) {
		case UNARY_MINUS:
			cgen_write(g, "-");
			break;
		}
		if (!cgen_expr(g, e->unary.of)) return false;
		cgen_write(g, ")");
		break;
	case EXPR_FN:
		if (!cgen_fn_name(g, &e->fn, &e->where)) return false;
		break;
	case EXPR_CALL:
		if (!cgen_expr(g, e->call.fn)) return false;
		cgen_write(g, "(");
		arr_foreach(&e->call.args, Expression, arg) {
			if (arg != e->call.args.data) {
				cgen_write(g, ",");
				cgen_write_space(g);
			}
			if (!cgen_expr(g, arg)) return false;
		}
		cgen_write(g, ")");
		break;
	}
	return true;
}

static bool cgen_stmt(CGenerator *g, Statement *s);


static bool cgen_decl(CGenerator *g, Declaration *d) {
	arr_foreach(&d->idents, Identifier, ident) {
		cgen_type_pre(g, &d->type);
		cgen_ident(g, *ident, NULL);
		cgen_type_post(g, &d->type);
		cgen_write_space(g);
		cgen_write(g, "=");
		cgen_write_space(g);
		if (!cgen_expr(g, &d->expr)) {
			return false;
		}
		cgen_write(g, "; ");
	}
	cgen_writeln(g, "");
	return true;
}

static bool cgen_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_EXPR:
		if (!cgen_expr(g, &s->expr))
			return false;
		cgen_writeln(g, ";");
		break;
	case STMT_DECL: {
		Declaration *d = &s->decl;
		if ((d->flags & DECL_FLAG_HAS_EXPR) && (d->flags & DECL_FLAG_CONST))
			if (d->expr.kind == EXPR_FN)
				return true; /* already dealt with below */
			
		return cgen_decl(g, &s->decl);
	}
	}
	return true;
}

static bool cgen_fns_in_stmt(CGenerator *g, Statement *s);

/* Generates function definition, and the definitions of all functions inside this */
static bool cgen_fn(CGenerator *g, FnExpr *f) {
	if (!cgen_fn_header(g, f)) return false;
	Block *prev_block = g->block;
	cgen_block_enter(g, &f->body);
	bool ret = true;
	cgen_write_space(g);
	cgen_writeln(g, "{");
	g->indent_level++;
	arr_foreach(&f->body.stmts, Statement, s) {
		if (!cgen_stmt(g, s))
			ret = false;
	}
	g->indent_level--;
	cgen_writeln(g, "}");
	if (ret) {
		arr_foreach(&f->body.stmts, Statement, stmt) {
			if (!cgen_fns_in_stmt(g, stmt)) ret = false;
		}
	}
	cgen_block_exit(g, prev_block);
	return ret;
}

static bool cgen_fns_in_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_FN: {
		return cgen_fn(g, &e->fn);
	}
	case EXPR_CALL:
		return cgen_fns_in_expr(g, e->call.fn); 
	default: return true;
	}
}

static bool cgen_fns_in_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_EXPR:
		if (s->expr.kind == EXPR_FN) {
			warn_print(s->where, "Statement of function has no effect (try assigning the function to a variable).");
		} else {
			return cgen_fns_in_expr(g, &s->expr);
		}
		break;
	case STMT_DECL: {
		Declaration *d = &s->decl;
		if (d->flags & DECL_FLAG_HAS_EXPR)
			return cgen_fns_in_expr(g, &d->expr);
	} break;
	}
	return true;
}

static bool cgen_file(CGenerator *g, ParsedFile *f) {
	cgen_write_line_comment(g, "toc");
	bool ret = true;
	if (!cgen_types(g, f)) return false;
	arr_foreach(&f->stmts, Statement, s) {
		if (!cgen_fns_in_stmt(g, s)) return false;
	}
	g->writing_to = CGEN_WRITING_TO_C;
	/* write actual main function */
	cgen_write(g, "\nint main(void) {\n"
			   "\tmain__();\n"
			   "\treturn 0;\n"
			   "}\n");
	return ret;
}
