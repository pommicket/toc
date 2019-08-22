/* the generation of C code */

typedef struct {
	FILE *out;
} CGenerator;


static void cgen_vwrite(CGenerator *g, const char *fmt, va_list args) {
	vfprintf(g->out, fmt, args);
}

static void cgen_write(CGenerator *g, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	cgen_vwrite(g, fmt, args);
	va_end(args);
}

static void cgen_writeln(CGenerator *g, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	cgen_vwrite(g, fmt, args);
	va_end(args);
	cgen_write(g, "\n");
}
	
static void cgen_write_comment(CGenerator *g, const char *fmt, ...) {
	cgen_write(g, "/* ");
	va_list args;
	va_start(args, fmt);
	cgen_vwrite(g, fmt, args);
	va_end(args);
	cgen_write(g, " */");
}

static void cgen_write_line_comment(CGenerator *g, const char *fmt, ...) {
	/* could switch to // for c99 */
	cgen_write(g, "/* ");
	va_list args;
	va_start(args, fmt);
	cgen_vwrite(g, fmt, args);
	va_end(args);
	cgen_write(g, " */\n");
}

static void cgen_write_ident(CGenerator *g, Identifier i) {
	ident_fprint(g->out, i);
}

static void cgen_create(CGenerator *g, FILE *out) {
	g->out = out;
}

static bool expr_cgen(Expression *e, CGenerator *g) {
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
		/* TODO: check if declared */
		cgen_write_ident(g, e->ident); 
		break;
	case EXPR_BINARY_OP:
		cgen_write(g, "(");
		expr_cgen(e->binary.lhs, g);
		switch (e->binary.op) {
		case BINARY_PLUS:
			cgen_write(g, "+");
			break;
		case BINARY_MINUS:
			cgen_write(g, "-");
			break;
		}
		expr_cgen(e->binary.rhs, g);
		cgen_write(g, ")");
		break;
	case EXPR_UNARY_OP:
		cgen_write(g, "(");
		switch (e->unary.op) {
		case UNARY_MINUS:
			cgen_write(g, "-");
			break;
		}
		expr_cgen(e->unary.of, g);
		cgen_write(g, ")");
		break;
	}
	return true;
}

static bool stmt_cgen(Statement *s, CGenerator *g) {
	switch (s->kind) {
	case STMT_EXPR:
		if (!expr_cgen(&s->expr, g))
			return false;
		cgen_write(g, ";\n");
		break;
	}
	return true;
}

static bool file_cgen(ParsedFile *f, CGenerator *g) {
	cgen_write_line_comment(g, "toc");
	bool ret = true;
	arr_foreach(&f->stmts, Statement, stmt) {
		if (stmt->kind == STMT_EXPR) {
			/* TODO: eventually make this an error / compile-time statement */
			warn_print(stmt->where, "Expression statement at top level.");
		}
		if (!stmt_cgen(stmt, g))
			ret = false;
	}
	return ret;
}
