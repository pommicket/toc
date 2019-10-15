static bool cgen_stmt(CGenerator *g, Statement *s);
static bool cgen_block(CGenerator *g, Block *b);
static bool cgen_expr(CGenerator *g, Expression *e);
static bool cgen_set_tuple(CGenerator *g, Expression *exprs, Identifier *idents, Expression *to);
static bool cgen_type_pre(CGenerator *g, Type *t, Location where);
static bool cgen_type_post(CGenerator *g, Type *t, Location where);
static bool cgen_decl(CGenerator *g, Declaration *d);

static void cgen_create(CGenerator *g, FILE *out, Identifiers *ids, Evaluator *ev) {
	g->outc = out;
	g->ident_counter = 0;
	g->main_ident = ident_get(ids, "main");
	g->evalr = ev;
	g->will_indent = true;
	g->indent_lvl = 0;
	g->anon_fns = NULL;
}

static void cgen_block_enter(CGenerator *g, Block *b) {
	g->block = b;
	Statement *stmts;
	if (b == NULL) {
		stmts = g->file->stmts;
	} else {
		stmts = b->stmts;
	}
	if (b) g->indent_lvl++;
	block_enter(b, stmts);
}

static void cgen_block_exit(CGenerator *g, Block *into) {
	Block *b = g->block;
	Statement *stmts;
	if (b == NULL) {
		stmts = g->file->stmts;
	} else {
		stmts = b->stmts;
	}
	block_exit(b, stmts);
	if (b) g->indent_lvl--;
	g->block = into;
}

static inline FILE *cgen_writing_to(CGenerator *g) {
	return g->outc;	/* for now */
}

/* indent iff needed */
static inline void cgen_indent(CGenerator *g) {
	if (g->will_indent) {
		for (int i = 0; i < g->indent_lvl; i++)
			fprintf(cgen_writing_to(g), "\t");
		g->will_indent = false;
	}
}

static inline void cgen_write(CGenerator *g, const char *fmt, ...) {
	va_list args;
	cgen_indent(g);
	va_start(args, fmt);
	vfprintf(cgen_writing_to(g), fmt, args);
	va_end(args);
}

static inline void cgen_nl(CGenerator *g) {
	fprintf(cgen_writing_to(g), "\n");
	g->will_indent = true;
}

static inline void cgen_writeln(CGenerator *g, const char *fmt, ...) {
	va_list args;
	cgen_indent(g);
	va_start(args, fmt);
	vfprintf(cgen_writing_to(g), fmt, args);
	va_end(args);
	cgen_nl(g);
}

/* should declaration be a direct function declaration C (as opposed to using a function pointer or not being a function) */
static bool cgen_fn_is_direct(CGenerator *g, Declaration *d) {
	return g->block == NULL && (d->flags & DECL_FLAG_HAS_EXPR) && d->expr.kind == EXPR_FN && arr_len(d->idents) == 1;
}

static bool cgen_uses_ptr(Type *t) {
	switch (t->kind) {
	case TYPE_TUPLE:
	case TYPE_ARR:
		return true;
	case TYPE_BUILTIN:
	case TYPE_PTR:
	case TYPE_FN:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
		return false;
	}
	assert(0);
	return false;
}

static void cgen_ident(CGenerator *g, Identifier i) {
	if (i == g->main_ident) {
		/* don't conflict with C's main! */
		cgen_write(g, "main__");
	} else {
		cgen_indent(g);
		fprint_ident(cgen_writing_to(g), i);
	}
}

static char *cgen_ident_to_str(Identifier i) {
	return ident_to_str(i);
}

static void cgen_ident_id(CGenerator *g, IdentID id) {
	cgen_write(g, "a%lu_", (unsigned long)id);
}

static bool cgen_type_post(CGenerator *g, Type *t, Location where);
static bool cgen_type_pre(CGenerator *g, Type *t, Location where) {
	switch (t->kind) {
	case TYPE_BUILTIN:
		switch (t->builtin) {
		case BUILTIN_I8: cgen_write(g, "i8"); break;
		case BUILTIN_I16: cgen_write(g, "i16"); break;
		case BUILTIN_I32: cgen_write(g, "i32"); break;
		case BUILTIN_I64: cgen_write(g, "i64"); break;
		case BUILTIN_U8: cgen_write(g, "u8"); break;
		case BUILTIN_U16: cgen_write(g, "u16"); break;
		case BUILTIN_U32: cgen_write(g, "u32"); break;
		case BUILTIN_U64: cgen_write(g, "u64"); break;
		case BUILTIN_CHAR: cgen_write(g, "char"); break;
		case BUILTIN_BOOL: cgen_write(g, "bool"); break;
		case BUILTIN_F32: cgen_write(g, "f32"); break;
		case BUILTIN_F64: cgen_write(g, "f64"); break;
		} break;
	case TYPE_PTR:
		if (!cgen_type_pre(g, t->ptr.of, where))
			return false;
		cgen_write(g, "(*");
		break;
	case TYPE_ARR:
		if (!cgen_type_pre(g, t->arr.of, where))
			return false;
		cgen_write(g, "(");
		break;
	case TYPE_FN:
		if (cgen_uses_ptr(&t->fn.types[0])) {
			cgen_write(g, "void");
		} else {
			if (!cgen_type_pre(g, &t->fn.types[0], where))
				return false;
		}
		cgen_write(g, " (*");
		break;
	case TYPE_VOID: cgen_write(g, "void"); break;
	case TYPE_UNKNOWN:
		err_print(t->where, "Can't determine type.");
		return false;
	case TYPE_TUPLE:
		/* We should never try to generate a tuple */
		assert(0);
		return false;
	}
	return true;
}

static bool cgen_type_post(CGenerator *g, Type *t, Location where) {
	switch (t->kind) {
	case TYPE_PTR:
		cgen_write(g, ")");
		if (!cgen_type_post(g, t->ptr.of, where))
			return false;
		break;
	case TYPE_ARR:
		cgen_write(g, "[%lu])", (unsigned long)t->arr.n);
		if (!cgen_type_post(g, t->arr.of, where))
			return false;
		break;
	case TYPE_FN: {
		bool out_param = cgen_uses_ptr(&t->fn.types[0]);
		cgen_write(g, ")(");
		for (size_t i = 1; i < arr_len(t->fn.types); i++) {
			if (i != 1)
				cgen_write(g, ", ");
			if (!cgen_type_pre(g, &t->fn.types[i], where))
				return false;
			if (!cgen_type_post(g, &t->fn.types[i], where))
				return false;
		}
		if (out_param) {
			if (arr_len(t->fn.types) > 1)
				cgen_write(g, ", ");
			if (!cgen_type_pre(g, &t->fn.types[0], where))
				return false;
			cgen_write(g, "(*)");
			if (!cgen_type_post(g, &t->fn.types[0], where))
				return false;
		}
		if (arr_len(t->fn.types) == 1 && !out_param)
			cgen_write(g, "void");
		cgen_write(g, ")");
		if (!out_param)
			if (!cgen_type_post(g, &t->fn.types[0], where))
				return false;
	} break;
	case TYPE_BUILTIN:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TUPLE:
		break;
	}
	return true;
}

static inline void cgen_fn_name(CGenerator *g, FnExpr *f) {
	if (f->c.name) {
		cgen_ident(g, f->c.name);
	} else {
		cgen_ident_id(g, f->c.id);
	}
}

static bool cgen_fn_header(CGenerator *g, FnExpr *f, Location where) {
	bool out_param = cgen_uses_ptr(&f->ret_type);
	bool any_params = false;
	if (out_param) {
		cgen_write(g, "void ");
	} else {
		if (!cgen_type_pre(g, &f->ret_type, where)) return false;
		cgen_write(g, " ");
	}
	cgen_fn_name(g, f);
	if (!out_param) {
		if (!cgen_type_post(g, &f->ret_type, where)) return false;
	}
	cgen_write(g, "(");
	arr_foreach(f->params, Declaration, d) {
		arr_foreach(d->idents, Identifier, i) {
			if (d != f->params || i != d->idents)
				cgen_write(g, ", ");
			any_params = true;
			if (!cgen_type_pre(g, &d->type, where))
				return false;
			cgen_write(g, " ");
			cgen_ident(g, *i);
			if (!cgen_type_post(g, &d->type, where))
				return false;
		}
	}
	if (out_param) {
		if (f->ret_type.kind == TYPE_TUPLE) {
			/* multiple return variables */
			for (size_t i = 0; i < arr_len(f->ret_type.tuple); i++) {
				Type *x = &f->ret_type.tuple[i];
				if (any_params || i > 0)
					cgen_write(g, ", ");
				if (!cgen_type_pre(g, x, where)) return false;
				cgen_write(g, "(*ret%lu__)", (unsigned long)i);
				if (!cgen_type_post(g, x, where)) return false;
			}
		} else {
			if (any_params)
				cgen_write(g, ", ");
			if (!cgen_type_pre(g, &f->ret_type, where))
				return false;
			cgen_write(g, " (*ret__)");
			if (!cgen_type_post(g, &f->ret_type, where))
				return false;
		}
	}
	if (!out_param && arr_len(f->params) == 0)
		cgen_write(g, "void");
	cgen_write(g, ")");
	return true;
}


/* 
Either set_expr or set_str should be NULL and either to_expr or to_str should be NULL 
Also, set_str and/or to_str should be NULL
*/
static bool cgen_set(CGenerator *g, Expression *set_expr, const char *set_str, Expression *to_expr,
					 const char *to_str) {
	Type *type;
	Location where;
	if (set_expr) {
		type = &set_expr->type;
		where = set_expr->where;
	} else {
		assert(to_expr);
		type = &to_expr->type;
		where = to_expr->where;
	}
	switch (type->kind) {
	case TYPE_BUILTIN:
	case TYPE_FN:
	case TYPE_PTR:
	case TYPE_UNKNOWN:
		if (set_expr) {
			if (!cgen_expr(g, set_expr)) return false;
		} else {
			cgen_write(g, set_str);
		}
		cgen_write(g, " = ");
		if (to_expr) {
			if (!cgen_expr(g, to_expr)) return false;
		} else {
			cgen_write(g, to_str);
		}
		cgen_write(g, ";");
		break;
	case TYPE_ARR:
		cgen_write(g, "{");
		cgen_nl(g);
		cgen_write(g, "size_t i;");
		if (!cgen_type_pre(g, type->arr.of, where)) return false;
		cgen_write(g, "(*arr__in)");
		if (!cgen_type_post(g, type->arr.of, where)) return false;
		cgen_write(g, " = ");
		if (set_expr) {
			if (!cgen_expr(g, set_expr)) return false;
		} else {
			cgen_write(g, set_str);
		}
		cgen_write(g, "; ");
		if (!cgen_type_pre(g, type->arr.of, where)) return false;
		cgen_write(g, "(*arr__out)");
		if (!cgen_type_post(g, type->arr.of, where)) return false;
		cgen_write(g, " = ");
		if (to_expr) {
			if (!cgen_expr(g, to_expr)) return false;
		} else {
			cgen_write(g, to_str);
		}
		cgen_write(g, ";");
		cgen_nl(g);
		cgen_write(g, "for (i = 0; i < %lu; i++) arr__out[i] = arr__in[i];", (unsigned long)type->arr.n);
		cgen_nl(g);
		cgen_write(g, "}");
		break;
	case TYPE_TUPLE:
		assert(set_expr);
		assert(to_expr);
	    assert(set_expr->kind == EXPR_TUPLE);
		if (!cgen_set_tuple(g, set_expr->tuple, NULL, to_expr))
			return false;
		break;
	case TYPE_VOID:
		assert(0);
		return false;
	}
	return true;
}


/* Either exprs or idents should be NULL */
static bool cgen_set_tuple(CGenerator *g, Expression *exprs, Identifier *idents, Expression *to) {
	switch (to->kind) {
	case EXPR_TUPLE:
		/* e.g. a, b = 3, 5; */
		for (size_t i = 0; i < arr_len(to->tuple); i++) {
			char *s = NULL;
			Expression *e = NULL;
			if (idents)
				s = cgen_ident_to_str(idents[i]);
			else
				e = &exprs[i];
			if (!cgen_set(g, e, s, to, NULL)) return false;
			free(s);
		}
		break;
	case EXPR_CALL: {
		/* e.g. a, b = fn_which_returns_tuple(); */
		if (!cgen_expr(g, to->call.fn)) return false;
		cgen_write(g, "(");
		bool any_args = arr_len(to->call.arg_exprs) != 0;
		arr_foreach(to->call.arg_exprs, Expression, arg) {
			if (arg != to->call.arg_exprs)
				cgen_write(g, ", ");
			if (!cgen_expr(g, arg))
				return false;
		}
		/* out params */
		size_t len = exprs ? arr_len(exprs) : arr_len(idents);

		for (size_t i = 0; i < len; i++) {
			if (any_args || i > 0)
				cgen_write(g, ", ");
			cgen_write(g, "&");
			if (exprs) {
				if (!cgen_expr(g, &exprs[i]))
					return false;
			} else {
				cgen_ident(g, idents[i]);
			}
		}
		cgen_writeln(g, ");");
	} break;
	default:
		assert(0);
		return false;
	}
	return true;
}

static bool cgen_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_LITERAL_FLOAT:
		cgen_write(g, "%f", e->floatl); /* TODO(eventually): better precision? */
		break;
	case EXPR_LITERAL_INT:
		cgen_write(g, UINTEGER_FMT, e->intl);
		break;
	case EXPR_LITERAL_STR: {
		size_t c;
		cgen_write(g, "\"");
		for (c = 0; c < e->strl.len; c++) {
			cgen_write(g, "\\x%x", e->strl.str[c]);
		}
		cgen_write(g, "\"");
	} break;
	case EXPR_LITERAL_BOOL:
		cgen_write(g, e->booll ? "true" : "false");
		break;
	case EXPR_LITERAL_CHAR:
		cgen_write(g, "((char)%d)", e->charl);
		break;
	case EXPR_IDENT:
		cgen_ident(g, e->ident);
		break;
	case EXPR_BINARY_OP: {
		const char *s = "";
		if (e->binary.op == BINARY_SET) {
			if (!cgen_set(g, e->binary.lhs, NULL, e->binary.rhs, NULL)) return false;
			break;
		}
		switch (e->binary.op) {
		case BINARY_SUB:
			s = "-"; break;
		case BINARY_ADD:
			s = "+"; break;
		case BINARY_MUL:
			s = "*"; break;
		case BINARY_DIV:
			s = "/"; break;
		case BINARY_SET: assert(0); break;
		case BINARY_GT:
			s = ">"; break;
		case BINARY_LT:
			s = "<"; break;
		case BINARY_GE:
			s = ">="; break;
		case BINARY_LE:
			s = "<="; break;
		case BINARY_EQ:
			s = "=="; break;
		case BINARY_NE:
			s = "!="; break;
		case BINARY_AT_INDEX:
			s = "["; break;
		}
		cgen_write(g, "(");
		if (!cgen_expr(g, e->binary.lhs))
			return false; 
		cgen_write(g, "%s", s);
		if (!cgen_expr(g, e->binary.rhs))
			return false;
		if (e->binary.op == BINARY_AT_INDEX)
			cgen_write(g, "]");
		cgen_write(g, ")");
	} break;
	case EXPR_UNARY_OP: {
		const char *s = "";
		switch (e->unary.op) {
		case UNARY_MINUS:
			s = "-"; break;
		case UNARY_DEREF:
			s = "*"; break;
		case UNARY_ADDRESS:
			s = "&"; break;
		case UNARY_NOT:
			s = "!"; break;
		case UNARY_DEL:
			s = "free("; break;
		}
		cgen_write(g, "(");
		cgen_write(g, "%s", s);
		if (!cgen_expr(g, e->unary.of))
			return false;
		if (e->unary.op == UNARY_DEL)
			cgen_write(g, ")");
		cgen_write(g, ")");
	} break;
	case EXPR_NEW:
		cgen_write(g, "((");
		if (!cgen_type_pre(g, &e->type, e->where))
			return false;
		if (!cgen_type_post(g, &e->type, e->where))
			return false;
		cgen_write(g, ")calloc(1, sizeof(");
		if (!cgen_type_pre(g, &e->new.type, e->where))
			return false;
		if (!cgen_type_post(g, &e->new.type, e->where))
			return false;
		cgen_write(g, ")))");
		break;
	case EXPR_IF: {
		IfExpr *curr = &e->if_;
		while (1) {
			if (curr->cond) {
				cgen_write(g, "if (");
				if (!cgen_expr(g, curr->cond))
					return false;
				cgen_write(g, ") ");
			}
			if (!cgen_block(g, &curr->body))
				return false;
			if (curr->next_elif) {
				cgen_write(g, " else ");
				curr = &curr->next_elif->if_;
			} else break;
		}
	} break;
	case EXPR_CALL:
		cgen_write(g, "(");
		if (!cgen_expr(g, e->call.fn))
			return false;
		cgen_write(g, "(");
		arr_foreach(e->call.arg_exprs, Expression, arg) {
			if (arg != e->call.arg_exprs)
				cgen_write(g, ", ");
			if (!cgen_expr(g, arg))
				return false;
		}
		cgen_write(g, "))");
		break;
	case EXPR_DIRECT:
		switch (e->direct.which) {
		case DIRECT_C: {
			Value val;
			if (!eval_expr(g->evalr, &e->direct.args[0], &val))
				return false;
			cgen_indent(g);
			fwrite(val.arr, 1, e->direct.args[0].type.arr.n, cgen_writing_to(g));
		} break;
		case DIRECT_COUNT: assert(0); break;
		}
		break;
	case EXPR_FN: {
		if (g->block != NULL) {
			Expression **eptr = arr_add(&g->anon_fns);
			*eptr = e;
		}
		cgen_fn_name(g, &e->fn);
	} break;
	}
	return true;
}

static bool cgen_block(CGenerator *g, Block *b) {
	Block *prev = g->block;
	cgen_block_enter(g, b);
	cgen_write(g, "{");
	cgen_nl(g);
	arr_foreach(b->stmts, Statement, s)
		if (!cgen_stmt(g, s))
			return false;
	
	cgen_block_exit(g, prev);
	cgen_write(g, "}");
	return true;
}

static void cgen_zero_value(CGenerator *g, Type *t) {
	switch (t->kind) {
	case TYPE_BUILTIN:
		cgen_write(g, "0");
		break;
	case TYPE_PTR:
	case TYPE_FN:
		cgen_write(g, "NULL");
		break;
	case TYPE_ARR:
		cgen_write(g, "{");
		cgen_zero_value(g, t->arr.of);
		cgen_write(g, "}");
		break;
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TUPLE:
		assert(0);
		break;
	}
}

static bool cgen_fn(CGenerator *g, FnExpr *f, Location where) {
	if (!cgen_fn_header(g, f, where))
		return false;
	cgen_write(g, " ");
	FnExpr *prev_fn = g->fn;
	g->fn = f;
	cgen_write(g, "{");
	cgen_nl(g);
	arr_foreach(f->ret_decls, Declaration, d) {
		cgen_decl(g, d);
	}
	if (!cgen_block(g, &f->body))
		return false;
	if (f->ret_decls) {
		if (cgen_uses_ptr(&f->ret_type)) {
		} else {
			cgen_write(g, "return ");
			cgen_ident(g, f->ret_decls[0].idents[0]);
			cgen_writeln(g, ";");
		}
	}
	cgen_write(g, "}");
	cgen_nl(g);
	g->fn = prev_fn;
	cgen_nl(g);
	cgen_nl(g);
	return true;
}

static bool cgen_decl(CGenerator *g, Declaration *d) {
	if (cgen_fn_is_direct(g, d)) {
		cgen_fn(g, &d->expr.fn, d->where);
	} else if (d->flags & DECL_FLAG_CONST) {
		/* TODO */
	} else {
		for (size_t idx = 0; idx < arr_len(d->idents); idx++) {
			Identifier *i = &d->idents[idx];
			Type *t = d->type.kind == TYPE_TUPLE ? &d->type.tuple[idx] : &d->type;
			if (!cgen_type_pre(g, t, d->where)) return false;
			cgen_write(g, " ");
			cgen_ident(g, *i);
			if (!cgen_type_post(g, t, d->where)) return false;
			if (g->block == NULL && (d->flags & DECL_FLAG_HAS_EXPR)) {
				cgen_write(g, " = ");
				/* directly initialize iff we are in global scope */
				if (!cgen_expr(g, &d->expr))
					return false;
			} else if (!(d->flags & DECL_FLAG_HAS_EXPR)) {
				cgen_write(g, " = ");
		    	cgen_zero_value(g, t);
			}
				
			cgen_write(g, "; ");
		}
		/* TODO: global tuples */
		if (g->block != NULL && (d->flags & DECL_FLAG_HAS_EXPR)) {
			if (d->expr.type.kind == TYPE_TUPLE) {
				if (!cgen_set_tuple(g, NULL, d->idents, &d->expr)) return false;
			} else {
				cgen_write(g, "{");
		 
				cgen_nl(g);
				if (!cgen_type_pre(g, &d->expr.type, d->expr.where)) return false;
				cgen_write(g, " expr__");
				if (!cgen_type_post(g, &d->expr.type, d->expr.where)) return false;
				cgen_write(g, "; ");
				if (!cgen_set(g, NULL, "expr__", &d->expr, NULL))
					return false;
				arr_foreach(d->idents, Identifier, i) {
					Expression e;
					e.flags = 0;
					e.kind = EXPR_IDENT;
					e.type = d->type;
					e.ident = *i;
					if (!cgen_set(g, &e, NULL, NULL, "expr__"))
						return false;
				}
				cgen_write(g, "}");
			}
		}
		cgen_nl(g);
	}
	return true;
}


static bool cgen_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_DECL:
		if (!cgen_decl(g, &s->decl)) return false;
		break;
	case STMT_EXPR:
		if (!cgen_expr(g, &s->expr)) return false;
		cgen_write(g, ";");
		cgen_nl(g);
		break;
	case STMT_RET:
		if (g->fn->ret_type.kind == TYPE_VOID) {
			cgen_write(g, "return");
		} else if (cgen_uses_ptr(&g->fn->ret_type)) {
			if (!cgen_set(g, NULL, "*ret__", &s->ret.expr, NULL)) return false;
			cgen_write(g, "return");
		} else {
			cgen_write(g, "return ");
			
			if (!cgen_expr(g, &s->ret.expr)) return false;
		}
		cgen_write(g, ";");
		cgen_nl(g);
		break;
	}
	return true;
}

static bool cgen_decls_file(CGenerator *g, ParsedFile *f);

static bool cgen_file(CGenerator *g, ParsedFile *f) {
	g->file = f;
	cgen_write(g, "#include <stdint.h>\n"
			   "#include <stdlib.h>\n"
			   "typedef int8_t i8;\n"
			   "typedef int16_t i16;\n"
			   "typedef int32_t i32;\n"
			   "typedef int64_t i64;\n"
			   "typedef uint8_t u8;\n"
			   "typedef uint16_t u16;\n"
			   "typedef uint32_t u32;\n"
			   "typedef uint64_t u64;\n"
			   "typedef float f32;\n"
			   "typedef double f64;\n"
			   "typedef unsigned char bool;\n"
			   "#define false ((bool)0)\n"
			   "#define true ((bool)1)\n\n\n");
	cgen_block_enter(g, NULL);
	if (!cgen_decls_file(g, f))
		return false;
	cgen_write(g, "/* code */\n");
	cgen_block_exit(g, NULL);
	cgen_write(g, "int main() {\n\tmain__();\n\treturn 0;\n}\n\n");

	arr_foreach(f->stmts, Statement, s)
		if (!cgen_stmt(g, s))
			return false;

	typedef Expression *ExprPtr;
	arr_foreach(g->anon_fns, ExprPtr, eptr) {
		Expression *e = *eptr;
		cgen_fn(g, &e->fn, e->where);
	}
	
	return true;
}
