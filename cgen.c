typedef struct {
	FILE *outc;
	unsigned long ident_counter;
	ParsedFile *file;
	Block *block;
	Evaluator *evalr;
	Identifier main_ident;
} CGenerator;

static bool cgen_stmt(CGenerator *g, Statement *s);
static bool cgen_block(CGenerator *g, Block *b);

static void cgen_create(CGenerator *g, FILE *out, Identifiers *ids, Evaluator *ev) {
	g->outc = out;
	g->ident_counter = 0;
	g->main_ident = ident_get(ids, "main");
	g->evalr = ev;
}

static void cgen_block_enter(CGenerator *g, Block *b) {
	g->block = b;
	Statement *stmts;
	if (b == NULL) {
		stmts = g->file->stmts;
	} else {
		stmts = b->stmts;
	}
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
	g->block = into;
}

static inline FILE *cgen_writing_to(CGenerator *g) {
	return g->outc;	/* for now */
}

static void cgen_write(CGenerator *g, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vfprintf(cgen_writing_to(g), fmt, args);
	va_end(args);
}

static void cgen_nl(CGenerator *g) {
	fprintf(cgen_writing_to(g), "\n");
}

/* should declaration be a direct function declaration C (as opposed to using a function pointer or not being a function) */
static bool cgen_is_fn_direct(CGenerator *g, Declaration *d) {
	return g->block == NULL && (d->flags & DECL_FLAG_HAS_EXPR) && d->expr.kind == EXPR_FN && arr_len(d->idents) == 1;
}

static void cgen_ident(CGenerator *g, Identifier i) {
	if (i == g->main_ident) {
		/* don't conflict with C's main! */
		cgen_write(g, "main__");
	} else {
		fprint_ident(cgen_writing_to(g), i);
	}
}

static void cgen_ident_id(CGenerator *g, unsigned long id) {
	cgen_write(g, "a%lu_", id);
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
		if (!cgen_type_pre(g, &t->fn.types[0], where))
			return false;
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
		cgen_write(g, ")(");
		for (size_t i = 1; i < arr_len(t->fn.types); i++) {
			if (i != 1)
				cgen_write(g, ", ");
			if (!cgen_type_pre(g, &t->fn.types[i], where))
				return false;
			if (!cgen_type_post(g, &t->fn.types[i], where))
				return false;
		}
		cgen_write(g, ")");
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

static bool cgen_fn_header(CGenerator *g, FnExpr *f, Location where) {
	if (!cgen_type_pre(g, &f->ret_type, where)) return false;
	cgen_write(g, " ");
	if (f->c.name) {
		cgen_ident(g, f->c.name);
	} else {
		cgen_ident_id(g, f->c.id);
	}
	if (!cgen_type_post(g, &f->ret_type, where)) return false;
	cgen_write(g, "(");
	arr_foreach(f->params, Declaration, d) {
		arr_foreach(d->idents, Identifier, i) {
			if (d != f->params || i != d->idents)
				cgen_write(g, ", ");
			if (!cgen_type_pre(g, &d->type, where))
				return false;
			cgen_write(g, " ");
			cgen_ident(g, *i);
			if (!cgen_type_post(g, &d->type, where))
				return false;
		}
	}
	cgen_write(g, ")");
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
		switch (e->binary.op) {
		case BINARY_SUB:
			s = "-"; break;
		case BINARY_ADD:
			s = "+"; break;
		case BINARY_MUL:
			s = "*"; break;
		case BINARY_DIV:
			s = "/"; break;
		case BINARY_SET:
			s = "="; break;
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
			fwrite(val.arr, 1, e->direct.args[0].type.arr.n, cgen_writing_to(g));
		} break;
		case DIRECT_COUNT: assert(0); break;
		}
		break;
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

static bool cgen_decl(CGenerator *g, Declaration *d) {
	if (cgen_is_fn_direct(g, d)) {
		if (!cgen_fn_header(g, &d->expr.fn, d->where))
			return false;
		cgen_write(g, " ");
		if (!cgen_block(g, &d->expr.fn.body))
			return false;
		cgen_nl(g);
		cgen_nl(g);
	} else if (d->flags & DECL_FLAG_CONST) {
		/* TODO */
	} else {
		arr_foreach(d->idents, Identifier, i) {
			if (!cgen_type_pre(g, &d->type, d->where)) return false;
			cgen_write(g, " ");
			cgen_ident(g, *i);
			if (!cgen_type_post(g, &d->type, d->where)) return false;
			if (g->block == NULL && (d->flags & DECL_FLAG_HAS_EXPR)) {
				cgen_write(g, " = ");
				/* repeat expression for each ident iff we are in global scope */
				if (!cgen_expr(g, &d->expr))
					return false;
			} else if (!(d->flags & DECL_FLAG_HAS_EXPR)) {
				cgen_write(g, " = ");
		    	cgen_zero_value(g, &d->type);
			}
				
			cgen_write(g, "; ");
		}
		if (g->block != NULL && (d->flags & DECL_FLAG_HAS_EXPR)) {
			arr_foreach(d->idents, Identifier, i) {
				cgen_ident(g, *i);
				cgen_write(g, " = ");
				if (!cgen_expr(g, &d->expr))
					return false;
				cgen_write(g, "; ");
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
		cgen_write(g, "return ");
		if (!cgen_expr(g, &s->ret.expr)) return false;
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

	return true;
}
