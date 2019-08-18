typedef struct {
	LineNo line;
	LineNo col;
} Location;

typedef struct {
	Location where;
	char *var;
} Declaration;

arr_declaration(Declarations, Declaration, decls_)

typedef struct {
	int type;
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
	if (t->token->kind != TOKEN_IDENT) {
		tokr_err(t, "Cannot declare non-identifier.");
		return false;
	}
	t->token++;
	return true;
}

static bool parse_stmt(Statement *s, Tokenizer *t) {
	if (token_is_kw(t->token + 1, KW_COLON)) {
		return parse_decls(&s->decls, t);
	} else {
		t->token++;	/* TODO: This is temporary */
	}
	return true;
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

static void stmt_fprint(FILE *out, Statement *s) {
	fprintf(out, "statement!\n");
}

static void parsed_file_fprint(FILE *out, ParsedFile *f) {
	arr_foreach(f->stmts, Statement, stmt) {
		stmt_fprint(out, stmt);
	}
}
