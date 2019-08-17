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

typedef unsigned long long LiteralInt;

typedef long double LiteralReal; /* OPTIM: Maybe only use double */

typedef enum {
			  NUM_LITERAL_INT,
			  NUM_LITERAL_REAL
} NumLiteralKind;

typedef struct {
	NumLiteralKind kind;
	union {
		LiteralInt intval;
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
			fprintf(out, "%llu", t->num.intval);
			break;
		case NUM_LITERAL_REAL:
			fprintf(out, "%g", (double)t->num.realval);
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
		t->tokens = err_realloc(t->tokens, t->cap * sizeof(*t->tokens));
	}
	token->line = line;
	token->col = col;
	t->tokens[t->ntokens++] = *token;
}

static Tokenizer tokenize_string(char *s) {	/* NOTE: May modify string. Don't even try to pass it a literal.*/
	int has_err = 0;
	Tokenizer t;
	t.cap = 256;
	t.ntokens = 0;
	t.tokens = err_malloc(t.cap * sizeof(*t.tokens));

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
		
		/* check if it's a number */

		if (isdigit(*s)) {
			/* it's a numerical literal */
			int base = 10;
			LiteralReal decimal_pow10;
			NumLiteral l;
			l.kind = NUM_LITERAL_INT;
			l.intval = 0;
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
					if (l.kind == NUM_LITERAL_REAL) {
						err_print(line, col, "Double . in number.");
						goto err;
					}
					if (base != 10) {
						err_print(line, col, "Decimal point in non base 10 number.");
						goto err;
					}
					l.kind = NUM_LITERAL_REAL;
					decimal_pow10 = 0.1;
					l.realval = (LiteralReal)l.intval;
					s++, col++;
					continue;
				} else if (*s == 'e') {
					s++; col++;
					if (l.kind == NUM_LITERAL_INT) {
						l.kind = NUM_LITERAL_REAL;
						l.realval = (LiteralReal)l.intval;
					}
					/* TODO: check if exceeding maximum exponent */
					int exponent = 0;
					if (*s == '+') {
						s++; col++;
					}
					
					int negative_exponent = 0;
					if (*s == '-') {
						s++; col++;
						negative_exponent = 1;
					}
					for (; isdigit(*s); s++, col++) {
						exponent *= 10;
						exponent += *s - '0';
					}
					/* OPTIM: Slow for very large exponents (unlikely to happen) */
					for (int i = 0; i < exponent; i++) {
						if (negative_exponent)
							l.realval /= 10;
						else
							l.realval *= 10;
					}
						
					break;
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
					if (isdigit(*s)) {
						/* something like 0b011012 */
						err_print(line, col, "Digit %d cannot appear in a base %d number.", digit, base);
						goto err;
					}
					/* end of numerical literal */
					break;
				}
				switch (l.kind) {
				case NUM_LITERAL_INT:
					if (l.intval > ULLONG_MAX / (LiteralInt)base) {
						/* too big! */
						err_print(line, col, "Number too big to fit in a numerical literal.");
						goto err;
					}
					l.intval *= (LiteralInt)base;
					l.intval += (LiteralInt)digit;
					break;
				case NUM_LITERAL_REAL:
					l.realval += decimal_pow10 * (LiteralReal)digit;
					decimal_pow10 /= 10;
					break;
				}
				s++; col++;
			}
			Token token;
			token.kind = TOKEN_NUM_LITERAL;
			token.num = l;
			tokenizer_add(&t, &token, line_start, col_start);
			continue;
		}
		
		if (isidentstart(*s)) {
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
		
		err_print(line, col, TEXT_IMPORTANT("Unrecognized token:") "\n\there --> %s\n", s);
		if (has_newline)
			*end_of_line = '\n';
	err:
		has_err = 1;
		s = strchr(s, '\n');
		if (s == NULL) break;
		s++; /* move past newline */
		col = 1;
		line++;
				
	}
	if (has_err) {
		fprintf(stderr, TEXT_IMPORTANT("Errors occured while preprocessing.\n"));
		abort();
	}
	t.token = t.tokens;
	return t;
}

static void tokenizer_free(Tokenizer *t) {
	free(t->tokens);
}
