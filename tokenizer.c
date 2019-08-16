typedef enum {
			  TOKEN_KW,
			  TOKEN_IDENT,
			  TOKEN_NUM_LITERAL,
			  TOKEN_EOF
			  /* TODO: char literals, str literals */
} TokenKind;

typedef enum {
			  KW_SEMICOLON,
			  KW_EQEQ,
			  KW_LT,
			  KW_LE,
			  KW_EQ,
			  KW_COUNT
} Keyword;

/* OPTIM: Use a trie or just a function if this gets too long */
static const char *keywords[KW_COUNT] =
	{";", "==", "<", "<=", "="}; 

#define TOKENIZER_USE_LLONG 1

#if TOKENIZER_USE_LLONG
typedef long long LiteralInt;
typedef unsigned long long LiteralUInt;
#define LITERAL_INT_FMT "%lld"
#define LITERAL_UINT_FMT "%llu"
#else
typedef long LiteralInt;
typedef unsigned long LiteralUInt;
#define LITERAL_INT_FMT "%ld"
#define LITERAL_UINT_FMT "%lu"
#endif

typedef double LiteralReal;

typedef enum {
			  NUM_LITERAL_INT,
			  NUM_LITERAL_UINT,
			  NUM_LITERAL_REAL
} NumLiteralKind;

typedef struct {
	NumLiteralKind kind;
	union {
		LiteralInt intval;
		LiteralUInt uintval;
		LiteralReal realval;
	};
} NumLiteral;

/* NOTE: LineNo is typedef'd in util/err.c */
typedef struct {
	TokenKind kind;
	LineNo line;
    LineNo col;
	union {
		Keyword kw;
		Identifier ident;
		NumLiteral num;
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
	case TOKEN_IDENT:
		fprintf(out, "identifier: %ld:", t->ident->id);
		ident_fprint(out, t->ident);
		break;
	case TOKEN_NUM_LITERAL:
		fprintf(out, "number: ");
		switch (t->num.kind) {
		case NUM_LITERAL_INT:
			fprintf(out, LITERAL_INT_FMT, t->num.intval);
			break;
		case NUM_LITERAL_UINT:
			fprintf(out, LITERAL_UINT_FMT, t->num.uintval);
			break;
		case NUM_LITERAL_REAL:
			fprintf(out, "%f", t->num.realval);
			break;
		}
		break;
	case TOKEN_EOF:
		fprintf(out, "eof");
		break;
	}
}

static void tokenizer_add(Tokenizer *t, Token *token, LineNo line, LineNo col) {
	if (t->ntokens >= t->cap) {
		t->cap *= 2;
		t->tokens = err_realloc(t->tokens, t->cap);
	}
	token->line = line;
	token->col = col;
	t->tokens[t->ntokens++] = *token;
}

static Tokenizer tokenize_string(char *s) {	/* NOTE: May modify string. Don't even try to pass it a literal.*/
	int has_err = 0;
	Tokenizer t;
	t.cap = 4096; /* TODO: test more tokens than this */
	t.ntokens = 0;
	t.tokens = malloc(t.cap * sizeof(*t.tokens));

	LineNo line = 1;
	LineNo col = 1;
	
	while (1) {
	    if (*s == 0) break;
		if (isspace(*s)) {
			if (*s == '\n') {
				line++;
				col = 0;
			}
			s++; col++;
	    	continue;
		}

		if (*s == '/') {
			/* maybe it's a comment */
			int is_comment = 1;
			s++; col++;
			switch (*s) {
			case '/': /* single line comment */
				for (s++; *s != '\n' && *s; s++);
				line++;
				col = 1;
				break;
			case '*': { /* multi line comment */
				int comment_level = 1; /* allow nested multi-line comments */
			    while (*s) {
					if (*s == '\n') {
						line++;
						col = 1;
						s++;
						continue;
					}
					if (s[0] == '*' && s[1] == '/') {
						s += 2; col += 2;
						comment_level--;
						if (comment_level == 0) {
							break;
						}
					} else if (s[0] == '/' && s[1] == '*') {
						s += 2; col += 2;
						comment_level++;
					} else {
						s++; col++;
					}
				}
				if (*s == 0) {
					err_print(line, col, "End of file reached inside multi-line comment.");
					abort(); /* there won't be any further errors, of course */
				}
			} break;
			default:
				is_comment = 0;
				s--; /* go back */
				break;
			}
			if (is_comment) continue;
		}
		Keyword kw;
		for (kw = 0; kw < KW_COUNT; kw++) {
			if (strncmp(s, keywords[kw], strlen(keywords[kw])) == 0) {
				break;
			}
		}
		if (kw != KW_COUNT) {
			/* it's a keyword */
			Token token;
			token.kind = TOKEN_KW;
			token.kw = kw;
			tokenizer_add(&t, &token, line, col);
			col += (LineNo)strlen(keywords[kw]);
			s += (LineNo)strlen(keywords[kw]);
			continue;
		}

		if (isdigit(*s)) {
			/* it's a numerical constant */
			int base = 10;
			LiteralInt intval = 0;
			LineNo line_start = line, col_start = col;
			if (*s == '0') {
				s++; col++;
				/* octal/hexadecimal/binary (or zero) */
				char format = *s;
				if (isdigit(format)) /* octal */
					base = 8;
				else {
					switch (format) {
					case 'b':
						base = 2;
						s++; col++;
						break;
					case 'x':
						base = 16;
						s++; col++;
						break;
					default:
						/* it's 0/0.something etc.  */
						break;
					}
				}
			}
			while (1) {
				if (*s == '.') {
					/* TODO */
				} else if (*s == 'e') {
					/* TODO */
				}
				int digit = -1;
				if (base == 16) {
					if (*s >= 'a' && *s <= 'f')
						digit = 10 + *s - 'a';
					else if (*s >= 'A' && *s <= 'F')
						digit = *s - 'A';
				}
				if (digit == -1) {
					if (*s >= '0' && *s <= '9')
						digit = *s - '0';
				}
				if (digit < 0 || digit >= base) {
					/* end of numerical literal */
					break;
				}
				/* TODO: check overflow; switch to uint */
				intval *= base;
				intval += digit;
				s++; col++;
			}
			Token token;
			token.kind = TOKEN_NUM_LITERAL;
			token.num.kind = NUM_LITERAL_INT;
			token.num.intval = intval;
			tokenizer_add(&t, &token, line_start, col_start);
			continue;
		}
		
		if (isident(*s)) {
			/* it's an identifier */
			Identifier ident = ident_insert(&s);
			Token token;
			token.kind = TOKEN_IDENT;
			token.ident = ident;
			tokenizer_add(&t, &token, line, col);			
			continue;
		}

		int has_newline;
		char *end_of_line = strchr(s, '\n');
		has_newline = end_of_line != NULL;
		if (has_newline)
			*end_of_line = 0;
		
		err_print(line, col, "Unrecognized token:\n\there --> %s\n", s);
		has_err = 1;
		if (has_newline) {
			/* increment line counter because of it */
		    line++;
			col = 1;
		} else {
			col += (LineNo)strlen(s);
		}
		s += strlen(s);
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
