static void cgen_create(CGenerator *g, Identifiers *ids, FILE *c_out, FILE *h_out, const char *h_filename) {
	g->c_out = c_out;
	g->h_out = h_out;
	g->anon_fn_count = 0;
	g->anon_var_count = 0;
	g->indent_level = 0;
	g->block = NULL;
    g->indent_next = true;
	g->main_ident = ident_get(ids, "main");
	
	g->writing_to = CGEN_WRITING_TO_H;
	cgen_write(g, "#include <stddef.h>\n"
			   "#include <stdint.h>\n"
			   "#include <string.h>\n");
	
	g->writing_to = CGEN_WRITING_TO_C;
	cgen_write(g, "#include \"%s\"\n", h_filename);
	cgen_writeln(g, ""); /* extra newline between includes and code */
}

static bool cgen_direct(CGenerator *g, DirectExpr *direct, Location where) {
	switch (direct->which) {
	case DIRECT_C: {
		Expression *args = direct->args.data;
		size_t nargs = direct->args.len;
		if (nargs != 1) {
			err_print(where, "Expected 1 argument to #C directive, but got %lu.", nargs);
		}
		/* TODO: compile-time constants */
		if (args[0].kind != EXPR_STR_LITERAL) {
			err_print(args[0].where, "Argument to #C directive must be a string literal.");
		}
		cgen_write(g, "%s", args[0].strl.str);
	} break;
	case DIRECT_COUNT:
		assert(0);
		return false;
	}
	return true;
}


/* generates C statements which must go before an expression. */
static bool cgen_expr_pre(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_BINARY_OP:
		if (!cgen_expr_pre(g, e->binary.lhs)) return false;
		if (!cgen_expr_pre(g, e->binary.rhs)) return false;
		/* TODO(eventually): Short-circuiting operators will need some work */
		break;
	case EXPR_UNARY_OP:
		if (!cgen_expr_pre(g, e->unary.of)) return false;
		break;
	case EXPR_CALL: {
		if (!cgen_expr_pre(g, e->call.fn)) return false;
		arr_foreach(&e->call.args, Expression, arg)
			if (!cgen_expr_pre(g, arg))
				return false;
		Type *fn_types = e->call.fn->type.fn.types.data;
		Type *ret_type = &fn_types[0];
		if (cgen_fn_uses_out_param(ret_type)) {
			/* generate out call */
			e->call.out_var = g->anon_var_count++;

			/* declaration of out variable */
			cgen_type_pre(g, ret_type);
			cgen_anon_var(g, e->call.out_var);
			cgen_type_post(g, ret_type);
			cgen_writeln(g, ";");

			cgen_expr(g, e->call.fn);
			
			cgen_write(g, "(");
			arr_foreach(&e->call.args, Expression, arg) {
				if (arg != e->call.args.data) {
					cgen_write(g, ",");
					cgen_space(g);
				}
				if (!cgen_expr(g, arg)) return false;
			}
			if (e->call.args.len) {
				cgen_write(g, ",");
				cgen_space(g);
			}
			cgen_write(g, "&");
			cgen_anon_var(g, e->call.out_var);
			cgen_writeln(g, ");");
		}
	} break;
	default: break;
	}
	return true;
}

static bool cgen_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_INT_LITERAL:
		cgen_write(g, "%lld", e->intl);
		break;
	case EXPR_FLOAT_LITERAL:
		/* TODO: more precision */
		cgen_write(g, "%f", (double)e->floatl);
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
		case BINARY_SET:
			cgen_write(g, "=");
			break;
		case BINARY_AT_INDEX:
			cgen_write(g, "[");
			break;
		case BINARY_COMMA:
			assert(0);
			return false;
		}
		if (!cgen_expr(g, e->binary.rhs)) return false;
		if (e->binary.op == BINARY_AT_INDEX) {
			cgen_write(g, "]");
		}
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
	case EXPR_CALL: {
		Type *fn_types = e->call.fn->type.fn.types.data;
		Type *ret_type = &fn_types[0];
		if (cgen_fn_uses_out_param(ret_type)) { /* if there's an out parameter, */
			cgen_anon_var(g, e->call.out_var); /* just use the variable we made earlier */
		} else {
			if (!cgen_expr(g, e->call.fn)) return false;
			cgen_write(g, "(");
			arr_foreach(&e->call.args, Expression, arg) {
				if (arg != e->call.args.data) {
					cgen_write(g, ",");
					cgen_space(g);
				}
				if (!cgen_expr(g, arg)) return false;
			}
			cgen_write(g, ")");
		}
	} break;
	case EXPR_DIRECT:
		if (!cgen_direct(g, &e->direct, e->where)) return false;
		break;
	}
	return true;
}

static bool cgen_stmt(CGenerator *g, Statement *s);

static void cgen_zero_value(CGenerator *g, Type *t) {
	switch (t->kind) {
	case TYPE_VOID:	/* we should never need this */
		assert(0);
		break;
	case TYPE_FN:
		cgen_write(g, "NULL");
		break;
	case TYPE_ARR:
		cgen_write(g, "{");
		cgen_zero_value(g, t->arr.of);
		cgen_write(g, "}");
		break;
	case TYPE_TUPLE:
		assert(0);
		break;
	case TYPE_BUILTIN:
		if (type_builtin_is_numerical(t->builtin)) {
			cgen_write(g, "0");
		} else {
			assert(0);
		}
		break;
	case TYPE_UNKNOWN:
		assert(0);
		break;
	}
}

static bool cgen_decl(CGenerator *g, Declaration *d) {
	size_t i = d->idents.len;
	if (d->flags & DECL_FLAG_HAS_EXPR) {
		if (!cgen_expr_pre(g, &d->expr))
			return false;
	}
	/* because , is left-associative, we want to go backwards */
	arr_foreach_reverse(&d->idents, Identifier, ident) {
		Type *type;
		if (d->idents.len > 1) {
			/* it's a tuple! */
			type = &(((Type*)d->type.tuple.data)[--i]);
		} else {
			type = &d->type;
			if (type->kind == TYPE_TUPLE) {
				/* TODO */
				err_print(d->where, "Direct declaration of tuples is not supported yet.");
				return false;
			}
		}
		if (type->kind == TYPE_ARR) {
			/* if you do a : [3]int; translates into int64_t av___23[3] = {0}; int64_t *a = av___23; */
			int has_expr = d->flags & DECL_FLAG_HAS_EXPR;
			unsigned long var;
			if (!has_expr) {
				/* int64_t av___23[3] = {0}; */
				var = g->anon_var_count++;
				cgen_type_pre(g, type);
				cgen_anon_var(g, var);
				cgen_type_post(g, type);
				cgen_space(g);
				cgen_write(g, "=");
				cgen_space(g);
				cgen_zero_value(g, type);
				cgen_writeln(g, ";");
			}
			/* int64_t *a = av___23; */
			cgen_type_pre(g, type->arr.of);
			cgen_write(g, "(*");
			cgen_ident(g, *ident, NULL);
			cgen_write(g, ")");
			cgen_type_post(g, type->arr.of);
			cgen_space(g);
			cgen_write(g, "=");
			cgen_space(g);
			if (has_expr) {
				if (!cgen_expr(g, &d->expr)) return false;
			} else {
				cgen_anon_var(g, var);
			}
			cgen_writeln(g, ";");
			return true;
		}
		
		cgen_type_pre(g, type);
		if (d->flags & DECL_FLAG_CONST) { /* TODO: remove this (never actually produce constants) */
			cgen_space(g);
			cgen_write(g, "const");
			cgen_space(g);
		}
		cgen_ident(g, *ident, NULL);
		cgen_type_post(g, type);
		cgen_space(g);
		cgen_write(g, "=");
		cgen_space(g);
		if (d->flags & DECL_FLAG_HAS_EXPR) {
			Expression *expr = &d->expr;
			if (d->idents.len > 1) {
				if (expr->kind == EXPR_BINARY_OP && expr->binary.op == BINARY_COMMA) {
					if (!cgen_expr(g, expr->binary.rhs)) return false;
					expr = expr->binary.lhs; /* ((3,4),5),6 => (3,4),5 */
				} else {
					/* last iteration */
					if (!cgen_expr(g, expr)) return false;
				}

			} else {
				if (!cgen_expr(g, expr)) return false;
			}
		} else {
			cgen_zero_value(g, type);
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

typedef struct {
	bool is_return; /* true => this is a function return */
	unsigned long var_no; /* if is_return = false, set the anonymous variable with this number to the return value. */
	const char *exit_with; /* how to exit this block in C, e.g. "break" (not needed if is_return = true). */
} BlockExitKind;

/* generates a block but not the functions, etc. inside it */
static bool cgen_block(CGenerator *g, Block *b, BlockExitKind *exit_kind) {
	bool success = true;
	cgen_writeln(g, "{");
	g->indent_level++;
	arr_foreach(&b->stmts, Statement, s) {
		if (!cgen_stmt(g, s))
			success = false;
	}
	if (exit_kind->is_return) {
		/* generate return from function */
		Expression *ret = b->ret_expr;
		if (ret && cgen_fn_uses_out_param(&ret->type)) {
			if (ret->type.kind == TYPE_ARR) {
				/* returning possibly multi-dimensional arrays */
				size_t total_size = 1; /* product of all dimensions */
				Type *type;
				for (type = &ret->type; type->kind == TYPE_ARR; type = type->arr.of)
					total_size *= type->arr.n;
				/* type is now the base type of the array, e.g. [3][3][3]fn() => fn() */
				cgen_write(g, "memcpy(*out__, ");
				if (!cgen_expr(g, b->ret_expr)) return false;
				cgen_write(g, ", %lu * sizeof(", total_size);
				cgen_type_pre(g, type);
				cgen_type_post(g, type);
				cgen_writeln(g, ")); return;");
			} else {
				cgen_write(g, "*out__ = ");
				if (!cgen_expr(g, b->ret_expr)) return false;
				cgen_writeln(g, ";");
				cgen_writeln(g, "return;");
			}
		} else {
			cgen_write(g, "return");
			if (b->ret_expr) {
				cgen_write(g, " ");
				if (!cgen_expr(g, b->ret_expr)) return false;
			}
			cgen_writeln(g, ";");
		}
	} else {
		err_print(b->ret_expr->where, "TODO");
		return false;
	}
	if (success) {
		g->indent_level--;
		cgen_writeln(g, "}");
	}
	return success;
}

/* Generates function definition, and the definitions of all functions inside this */
static bool cgen_fn(CGenerator *g, FnExpr *f) {
	if (!cgen_fn_header(g, f)) return false;
	Block *prev_block = g->block;
	cgen_block_enter(g, &f->body);
	cgen_space(g);
	BlockExitKind e_kind;
	e_kind.is_return = 1;
	if (!cgen_block(g, &f->body, &e_kind)) return false;

	bool ret = true;
	arr_foreach(&f->body.stmts, Statement, stmt) {
		if (!cgen_fns_in_stmt(g, stmt)) ret = false;
	}
	cgen_block_exit(g, prev_block);
	return ret;
}

static bool cgen_fns_in_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_FN:
		return cgen_fn(g, &e->fn);
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

/* generate a statement at top level, including any functions in it. */
static bool cgen_stmt_top(CGenerator *g, Statement *s) {
	if (!cgen_fns_in_stmt(g, s)) return false;
	switch (s->kind) {
	case STMT_EXPR: {
		Expression *e = &s->expr;
		bool ignored = true;
		switch (e->kind) {
		case EXPR_DIRECT:
			switch (e->direct.which) {
			case DIRECT_C:
				ignored = false;
				cgen_direct(g, &e->direct, e->where);
				break;
			case DIRECT_COUNT: assert(0); break;
			}
		default: break;
		}
		if (ignored)
			warn_print(e->where, "Expression at top level currently ignored."); /* TODO */
	} break;
	case STMT_DECL: break;
	}
	return true;
}

static bool cgen_file(CGenerator *g, ParsedFile *f) {
	cgen_write_line_comment(g, "toc");
	bool ret = true;
	if (!cgen_decls_file(g, f)) return false;
	arr_foreach(&f->stmts, Statement, s) {
		if (!cgen_stmt_top(g, s)) return false;
	}
	g->writing_to = CGEN_WRITING_TO_C;
	/* write actual main function */
	cgen_write(g, "\nint main(void) {\n"
			   "\tmain__();\n"
			   "\treturn 0;\n"
			   "}\n");
	return ret;
}
