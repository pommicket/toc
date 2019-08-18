typedef enum {
			  TOKEN_KW,
			  TOKEN_IDENT,
			  TOKEN_NUM_CONST,
			  TOKEN_CHAR_CONST,
			  TOKEN_STR_CONST,
			  TOKEN_EOF
} TokenKind;

typedef enum {
			  KW_SEMICOLON,
			  KW_EQ,
			  KW_COLON,
			  KW_FN,
			  KW_LPAREN,
			  KW_RPAREN,
			  KW_LBRACE,
			  KW_RBRACE,
			  KW_EQEQ,
			  KW_LT,
			  KW_LE,
			  KW_MINUS,
			  KW_COUNT
} Keyword;

/* OPTIM: Use a trie or just a function if this gets too long */
static const char *keywords[KW_COUNT] =
	{";", "=", ":", "fn", "(", ")", "{", "}", "==", "<", "<=", "-"}; 

#define TOKR_USE_LLONG 1

typedef unsigned long long IntConst;

typedef long double RealConst; /* OPTIM: Switch to double */

typedef enum {
			  NUM_CONST_INT,
			  NUM_CONST_REAL
} NumConstKind;

typedef struct {
	NumConstKind kind;
	union {
		IntConst intval;
		RealConst realval;
	};
} NumConst;

typedef struct {
	char *str;
	size_t len;
} StrConst;

/* NOTE: LineNo is typedef'd in util/err.c */
typedef struct {
	TokenKind kind;
	LineNo line;
	char *code;
	union {
		Keyword kw;
		Identifier ident;
		NumConst num;
		char chr;
		StrConst str;
	};
} Token;

arr_declaration(Tokens, Token, tokens_)

typedef struct {
	Tokens tokens;
	char *s; /* string being parsed */
	LineNo line;
	Token *token; /* token currently being processed */
} Tokenizer;

static bool token_is_kw(Token *t, Keyword kw) {
	return t->kind == TOKEN_KW && t->kw == kw;
}

static void token_fprint(FILE *out, Token *t) {
	fprintf(out, "l%lu-", (unsigned long)t->line);
	switch (t->kind) {
	case TOKEN_KW:
		fprintf(out, "keyword: %s", keywords[t->kw]);
		break;
	case TOKEN_IDENT:
		fprintf(out, "identifier: %ld:", t->ident->id);
		ident_fprint(out, t->ident);
		break;
	case TOKEN_NUM_CONST:
		fprintf(out, "number: ");
		switch (t->num.kind) {
		case NUM_CONST_INT:
			fprintf(out, "%llu", t->num.intval);
			break;
		case NUM_CONST_REAL:
			fprintf(out, "%g", (double)t->num.realval);
			break;
		}
		break;
	case TOKEN_CHAR_CONST:
		fprintf(out, "char: '%c' (%d)", t->chr, t->chr);
		break;
	case TOKEN_STR_CONST:
		fprintf(out, "str: \"%s\"", t->str.str);
		break;
	case TOKEN_EOF:
		fprintf(out, "eof");
		break;
	}
}

static void tokr_add(Tokenizer *t, Token *token) {
	if (!token->line)
		token->line = t->line;
	if (!token->code)
		token->code = t->s;
	tokens_add(&t->tokens, token);
}

static void tokr_nextchar(Tokenizer *t) {
	if (*(t->s) == '\n') {
		t->line++;
	}
	t->s++;
}

static char tokr_esc_seq(Tokenizer *t) {
	/* TODO: add more of these incl. \x41, \100 */
	switch (*t->s) {
	case '\'':
		tokr_nextchar(t);
		return '\'';
	case '"':
		tokr_nextchar(t);
		return '"';
	case '\\':
		tokr_nextchar(t);
		return '\\';
	case 'n':
		tokr_nextchar(t);
		return '\n';
	default:
		return 0;
	}

}

/* to be used during tokenization */
static void tokenization_err(Tokenizer *t, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	err_vprint(t->line, t->s, fmt, args);
	va_end(args);
	
	char *end_of_line = strchr(t->s, '\n');
	if (end_of_line) {
		t->s = end_of_line;
		t->s++; /* move past newline */
	} else {
		t->s = strchr(t->s, '\0');
	}
	t->line++;
}

/* to be used after tokenization */
static void tokr_err(Tokenizer *t, const char *fmt, ...) {
	LineNo line = t->token->line;
	va_list args;
	va_start(args, fmt);
	err_vprint(line, t->token->code, fmt, args);
	va_end(args);
	while (1) {
		if (t->token->line != line) break;
		if (t->token->kind == TOKEN_EOF) break;
		t->token++;
	}
}

static bool tokenize_string(Tokenizer *tokr, char *str) {
	int has_err = 0;
	Tokenizer t;
	tokens_create(&t.tokens);
	tokens_reserve(&t.tokens, 256);
	t.s = str;
	t.line = 1;
	
	while (1) {
	    if (*t.s == 0) break;
		if (isspace(*t.s)) {
			tokr_nextchar(&t);
	    	continue;
		}

		if (*t.s == '/') {
			/* maybe it's a comment */
			int is_comment = 1;
			switch (t.s[1]) {
			case '/': /* single line comment */
				tokr_nextchar(&t);
				for (t.s++; *t.s != '\n' && *t.s; t.s++);
				t.line++;
				break;
			case '*': { /* multi line comment */
				tokr_nextchar(&t);
				int comment_level = 1; /* allow nested multi-line comments */
			    while (*t.s) {
					if (t.s[0] == '*' && t.s[1] == '/') {
						t.s += 2;
						comment_level--;
						if (comment_level == 0) {
							break;
						}
					} else if (t.s[0] == '/' && t.s[1] == '*') {
						t.s += 2;
						comment_level++;
					} else {
						tokr_nextchar(&t);
					}
				}
				if (*t.s == 0) {
					tokenization_err(&t, "End of file reached inside multi-line comment.");
					abort(); /* there won't be any further errors, of course */
				}
			} break;
			default:
				is_comment = 0;
				break;
			}
			if (is_comment) continue;
		}
		Keyword kw;
		for (kw = 0; kw < KW_COUNT; kw++) {
			if (strncmp(t.s, keywords[kw], strlen(keywords[kw])) == 0) {
				break;
			}
		}
		if (kw != KW_COUNT) {
			/* it's a keyword */
			Token token = {0};
			token.kind = TOKEN_KW;
			token.kw = kw;
			tokr_add(&t, &token);
			t.s += (LineNo)strlen(keywords[kw]);
			continue;
		}
		
		/* check if it's a number */

		if (isdigit(*t.s)) {
			/* it's a numeric constant */
			int base = 10;
			RealConst decimal_pow10;
			NumConst n;
			n.kind = NUM_CONST_INT;
			n.intval = 0;
			Token token = {0};
			token.line = t.line;
			token.code = t.s;
			if (*t.s == '0') {
				tokr_nextchar(&t);
				/* octal/hexadecimal/binary (or zero) */
				char format = *t.s;
				if (isdigit(format)) /* octal */
					base = 8;
				else {
					switch (format) {
					case 'b':
						base = 2;
						tokr_nextchar(&t);
						break;
					case 'x':
						base = 16;
						tokr_nextchar(&t);
						break;
					default:
						/* it's 0/0.something etc.  */
						break;
					}
				}
			}

			while (1) {
				if (*t.s == '.') {
					if (n.kind == NUM_CONST_REAL) {
						tokenization_err(&t, "Double . in number.");
						goto err;
					}
					if (base != 10) {
						tokenization_err(&t, "Decimal point in non base 10 number.");
						goto err;
					}
				    n.kind = NUM_CONST_REAL;
					decimal_pow10 = 0.1;
					n.realval = (RealConst)n.intval;
					tokr_nextchar(&t);
					continue;
				} else if (*t.s == 'e') {
					tokr_nextchar(&t);
					if (n.kind == NUM_CONST_INT) {
						n.kind = NUM_CONST_REAL;
						n.realval = (RealConst)n.intval;
					}
					/* TODO: check if exceeding maximum exponent */
					int exponent = 0;
					if (*t.s == '+')
						tokr_nextchar(&t); /* ignore + after e */
					
					int negative_exponent = 0;
					if (*t.s == '-') {
						tokr_nextchar(&t);
						negative_exponent = 1;
					}
					for (; isdigit(*t.s); tokr_nextchar(&t)) {
						exponent *= 10;
						exponent += *t.s - '0';
					}
					/* OPTIM: Slow for very large exponents (unlikely to happen) */
					for (int i = 0; i < exponent; i++) {
						if (negative_exponent)
							n.realval /= 10;
						else
							n.realval *= 10;
					}
						
					break;
				}
				int digit = -1;
				if (base == 16) {
					if (*t.s >= 'a' && *t.s <= 'f')
						digit = 10 + *t.s - 'a';
					else if (*t.s >= 'A' && *t.s <= 'F')
						digit = *t.s - 'A';
				}
				if (digit == -1) {
					if (*t.s >= '0' && *t.s <= '9')
						digit = *t.s - '0';
				}
				if (digit < 0 || digit >= base) {
					if (isdigit(*t.s)) {
						/* something like 0b011012 */
						tokenization_err(&t, "Digit %d cannot appear in a base %d number.", digit, base);
						goto err;
					}
					/* end of numeric constant */
					break;
				}
				switch (n.kind) {
				case NUM_CONST_INT:
					if (n.intval > ULLONG_MAX / (IntConst)base ||
						n.intval * (IntConst)base > ULLONG_MAX - (IntConst)digit) {
						/* too big! */
						tokenization_err(&t, "Number too big to fit in a numeric constant.");
						goto err;
					}
					n.intval *= (IntConst)base;
					n.intval += (IntConst)digit;
					break;
				case NUM_CONST_REAL:
					n.realval += decimal_pow10 * (RealConst)digit;
					decimal_pow10 /= 10;
					break;
				}
				tokr_nextchar(&t);
			}
			token.kind = TOKEN_NUM_CONST;
			token.num = n;
			tokr_add(&t, &token);
			continue;
		}

		if (*t.s == '\'') {
			/* it's a character constant! */
			tokr_nextchar(&t);
			Token token = {0};
			token.line = t.line;
			token.code = t.s;
			char c;
			if (*t.s == '\\') {
				/* escape sequence */
				tokr_nextchar(&t);
				c = tokr_esc_seq(&t);
				if (c == 0) {
					tokenization_err(&t, "Unrecognized escape character: '\\%c'.", *t.s);
					goto err;
				}
			} else {
				c = *t.s;
				tokr_nextchar(&t);
			}
			if (*t.s != '\'') {
				tokenization_err(&t, "End of character constant expected.");
				goto err;
			}
			tokr_nextchar(&t);
			token.kind = TOKEN_CHAR_CONST;
			token.chr = c;
			tokr_add(&t, &token);
			continue;
		}

		if (*t.s == '"') {
			/* it's a string constant! */
			Token token;
			token.line = t.line;
			token.code = t.s;
			tokr_nextchar(&t);
			size_t len = 0;
			size_t backslashes = 0;
			while (*t.s != '"' || backslashes % 2 == 1) {
				if (*t.s == '\\') {
					backslashes++;
				} else if (*t.s == 0) {
					/* return t to opening " so that we go to the next line */
					t.line = token.line;
					t.s = token.code;
					tokenization_err(&t, "No matching \" found.");
					goto err;
				} else {
					backslashes = 0;
				}
				len++;
				tokr_nextchar(&t);
			}
			char *str = malloc(len + 1);
		    char *strptr = str;
			t.s = token.code;
			t.line = token.line;
			tokr_nextchar(&t); /* past opening " */
			while (*t.s != '"') {
				assert(*t.s);
				if (*t.s == '\\') {
					tokr_nextchar(&t);
					char c = tokr_esc_seq(&t);
					if (c == 0) {
						tokenization_err(&t, "Unrecognized escape character: '\\%c'.", *t.s);
						goto err;
					}
					*strptr++ = c;
				} else {
					*strptr++ = *t.s;
					tokr_nextchar(&t);
				}
			}
			*strptr = 0;
			token.kind = TOKEN_STR_CONST;
			token.str.len = len;
			token.str.str = str;
			tokr_add(&t, &token);
			tokr_nextchar(&t); /* move past closing " */
			continue;
		}
		
		if (isidentstart(*t.s)) {
			/* it's an identifier */
			Token token = {0};
			token.line = t.line;
			token.code = t.s;
			Identifier ident = ident_insert(&t.s);
			token.kind = TOKEN_IDENT;
			token.ident = ident;
			tokr_add(&t, &token);			
			continue;
		}		
		tokenization_err(&t, "Token not recognized");
	err:
		has_err = 1;
	}
	Token token = {0};
	token.kind = TOKEN_EOF;
	tokr_add(&t, &token);
	
	t.token = t.tokens.data;
	*tokr = t;
	return !has_err;
}

static void tokr_free(Tokenizer *t) {
	arr_foreach(t->tokens, Token, token) {
		switch (token->kind) {
		case TOKEN_STR_CONST:
			free(token->str.str);
			break;
		default: break;
		}
	}
	tokens_clear(&t->tokens);
}
