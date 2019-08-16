typedef enum {
			  TOKEN_KW,
			  TOKEN_EOF
} TokenKind;

typedef enum {
			  KW_SEMICOLON,
			  KW_EQEQ,
			  KW_LT,
			  KW_LE,
			  KW_EQ,
			  KW_COUNT
} Keyword;

static const char *keywords[KW_COUNT] =
	{";", "==", "<", "<=", "="};


/* NOTE: LineNo is typedef'd in util/err.c */
typedef struct {
	TokenKind kind;
	LineNo line;
    LineNo col;
	union {
		Keyword kw;
	};
} Token;

typedef struct {
	Token *tokens;
	size_t ntokens;
	size_t cap;	/* used internally */
	Token *token; /* token currently being processed */
} Tokenizer;

static void token_fprint(FILE *out, Token *t) {
	fprintf(out, "l%luc%lu-", (unsigned long)t->line, (unsigned long)t->col);
	switch (t->kind) {
	case TOKEN_KW:
		fprintf(out, "keyword: %s", keywords[t->kw]);
		break;
	case TOKEN_EOF:
		fprintf(out, "eof");
		break;
	}
}

static void tokenizer_add(Tokenizer *t, Token *token, LineNo line, LineNo col) {
	if (t->ntokens == t->cap) {
		t->cap *= 2;
		t->tokens = realloc(t->tokens, t->cap);
	}
	token->line = line;
	token->col = col;
	t->tokens[t->ntokens++] = *token;
}

static Tokenizer tokenize_file(FILE *fp) {
	char buf[4096];
	setvbuf(fp, buf, _IOFBF, sizeof buf);
	char errbuf[256] = {0}; /* for errors */
	int has_err = 0;
	Tokenizer t;
	t.cap = 4096;
	t.ntokens = 0;
	t.tokens = malloc(t.cap * sizeof(*t.tokens));

	LineNo line = 1;
	LineNo col = 1;
	
	while (1) {
		int c = fpeekc(fp);
	    if (c == EOF) break;
		if (isspace(c)) {
			if (c == '\n') {
				line++;
				col = 0;
			}
			fnextc(fp);
			col++;
	    	continue;
		}
		Keyword kw;
		for (kw = 0; kw < KW_COUNT; kw++) {
			if (fhasprefix(fp, keywords[kw])) {
				break;
			}
		}
		if (kw != KW_COUNT) {
			Token kw_token;
			kw_token.kind = TOKEN_KW;
			kw_token.kw = kw;
			tokenizer_add(&t, &kw_token, line, col);
			col += (LineNo)strlen(keywords[kw]);
			continue;
		}
		
		fgets(errbuf, sizeof errbuf, fp);
		size_t len = strlen(errbuf);
		int has_newline = len && errbuf[len-1] == '\n';
		if (has_newline) {
			/* remove newline */
			errbuf[len-1] = 0;
		}
		err_print(line, col, "Unrecognized token:\n\there --> %s\n", errbuf);
		has_err = 1;
		if (has_newline) {
			/* increment line counter because of it */
		    line++;
			col = 1;
		} else {
			col += (LineNo)(sizeof errbuf);
		}
	}
	/* TODO: Check ferror/errno */
	if (has_err) {
		fprintf(stderr, "Errors occured while preprocessing.\n");
		abort();
	}
	t.token = t.tokens;
	return t;
}

static void tokenizer_free(Tokenizer *t) {
	free(t->tokens);
}
