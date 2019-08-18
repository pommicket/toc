typedef struct {
	Location where;
	Identifier var;
	bool is_const;
	bool has_expr;
} Declaration;

arr_declaration(Declarations, Declaration, decls_)

typedef enum {
			  STMT_DECLS
} StatementKind;
	
typedef struct {
	StatementKind kind;
	Location where;
	union {
		Declarations decls;
	};
} Statement;

arr_declaration(Statements, Statement, stmts_)

typedef struct {
	Statements stmts;
} ParsedFile;

/* TODO: Add newline tokens back in; give tokens pointer to text */
static bool parse_decls(Declarations *ds, Tokenizer *t) {
	decls_create(ds);
	while (1) {
		Declaration decl = {0};
		if (t->token->kind != TOKEN_IDENT) {
			tokr_err(t, "Cannot declare non-identifier.");
			return false;
		}

		decl.where = t->token->where;
		decl.var = t->token->ident;
		t->token++;

		if (!token_is_kw(t->token, KW_COLON)) {
			tokr_err(t, "Expected ':' in declaration.");
			return false;
		}

		/* TODO: type */

		t->token++;
	
		if (token_is_kw(t->token, KW_SEMICOLON)) {
		} else if (token_is_kw(t->token, KW_EQ)) {
			t->token++;
			decl.has_expr = true;
		} else if (token_is_kw(t->token, KW_MINUS)) {
			t->token++;
			decl.has_expr = true;
			decl.is_const = true;
		}
		decls_add(ds, &decl);
		if (token_is_kw(t->token, KW_SEMICOLON)) {
			t->token++;
			break;
		}
		if (!token_is_kw(t->token, KW_COMMA)) {
			tokr_err(t, "Expected ';' or ',' to finish or continue declaration.");
			return false;
		}
		t->token++; /* move past comma */
	}
	return true;
}

static bool parse_stmt(Statement *s, Tokenizer *t) {
	if (token_is_kw(t->token + 1, KW_COLON)) {
		return parse_decls(&s->decls, t);
	} else {
		tokr_err(t, "Unreocgnized statement.");
		return false;
	}
}

static bool parse_file(ParsedFile *f, Tokenizer *t) {
	stmts_create(&f->stmts);
	bool ret = true;
	while (t->token->kind != TOKEN_EOF) {
		Statement stmt = {0};
		if (!parse_stmt(&stmt, t))
			ret = false;
		stmts_add(&f->stmts, &stmt);
	}
	return ret;
}

static void decl_fprint(FILE *out, Declaration *d) {
	fprintf(out, "l%lu:", (unsigned long)d->where.line);
	ident_fprint(out, d->var);
	if (d->is_const) {
		fprintf(out, "[const]");
	}
	if (d->has_expr) {
		fprintf(out, "=");
	}
}

static void stmt_fprint(FILE *out, Statement *s) {
	switch (s->kind) {
	case STMT_DECLS:
		arr_foreach(s->decls, Declaration, decl) {
			if (decl != s->decls.data) {
				fprintf(out, ", ");
			}
			decl_fprint(out, decl);
		}
		fprintf(out, ";\n");
		break;
	}
}

static void parsed_file_fprint(FILE *out, ParsedFile *f) {
	arr_foreach(f->stmts, Statement, stmt) {
		stmt_fprint(out, stmt);
	}
}
