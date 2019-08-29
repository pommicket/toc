typedef enum {
			  TOKEN_KW,
			  TOKEN_IDENT,
			  TOKEN_NUM_LITERAL,
			  TOKEN_CHAR_LITERAL,
			  TOKEN_STR_LITERAL,
			  TOKEN_EOF
} TokenKind;

typedef enum {
			  KW_SEMICOLON,
			  KW_EQ,
			  KW_COLON,
			  KW_AT,
			  KW_COMMA,
			  KW_LPAREN,
			  KW_RPAREN,
			  KW_LBRACE,
			  KW_RBRACE,
			  KW_LSQUARE,
			  KW_RSQUARE,
			  KW_EQEQ,
			  KW_LT,
			  KW_LE,			  
			  KW_MINUS,
			  KW_PLUS,
			  KW_LAST_SYMBOL = KW_PLUS, /* last one entirely consisting of symbols */
			  KW_FN,
			  KW_INT,
			  KW_I8,
			  KW_I16,
			  KW_I32,
			  KW_I64,
			  KW_U8,
			  KW_U16,
			  KW_U32,
			  KW_U64,
			  KW_FLOAT,
			  KW_DOUBLE,
			  KW_COUNT
} Keyword;

static const char *keywords[KW_COUNT] =
	{";", "=", ":", "@", ",", "(", ")", "{", "}", "[", "]", "==", "<", "<=", "-", "+", "fn",
	 "int", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "float", "double"};

/* Returns KW_COUNT if it's not a keyword */
/* OPTIM: don't use strncmp so much */
static Keyword tokenize_kw(char **s) {
	for (Keyword k = 0; k < KW_COUNT; k++) {
		size_t len = strlen(keywords[k]);
		if (strncmp(*s, keywords[k], len) == 0) {
			if (k > KW_LAST_SYMBOL) {
				/* 
				   it's not a symbol, so we need to check if it's something like "intfoo"
				 */
				if (isident((*s)[len])) {
					return KW_COUNT;
				}
			}
			*s += len;
			return k;
		}
	}
	return KW_COUNT;
}

typedef unsigned long long IntLiteral;
typedef long double FloatLiteral; /* OPTIM: Switch to double */
#define INT_LITERAL_FMT "%llu"

typedef enum {
			  NUM_LITERAL_INT,
			  NUM_LITERAL_FLOAT
} NumLiteralKind;

typedef struct {
	NumLiteralKind kind;
	union {
		IntLiteral intval;
		FloatLiteral floatval;
	};
} NumLiteral;

typedef struct {
	char *str;
	size_t len;
} StrLiteral;

/* NOTE: Location is typedef'd in util/err.c */
typedef struct {
	TokenKind kind;
	Location where;
	union {
		Keyword kw;
		Identifier ident;
		NumLiteral num;
		char chr;
		StrLiteral str;
	};
} Token;

typedef struct {
	Array tokens;
	char *s; /* string being parsed */
	LineNo line;
	Token *token; /* token currently being processed */
} Tokenizer;



static bool token_is_kw(Token *t, Keyword kw) {
	return t->kind == TOKEN_KW && t->kw == kw;
}

static void token_fprint(FILE *out, Token *t) {
	fprintf(out, "l%lu-", (unsigned long)t->where.line);
	switch (t->kind) {
	case TOKEN_KW:
		fprintf(out, "keyword: %s", keywords[t->kw]);
		break;
	case TOKEN_IDENT:
		fprintf(out, "identifier: %ld:", t->ident->id);
		fprint_ident(out, t->ident);
		break;
	case TOKEN_NUM_LITERAL:
		fprintf(out, "number: ");
		switch (t->num.kind) {
		case NUM_LITERAL_INT:
			fprintf(out, INT_LITERAL_FMT, t->num.intval);
			break;
		case NUM_LITERAL_FLOAT:
			fprintf(out, "%g", (double)t->num.floatval);
			break;
		}
		break;
	case TOKEN_CHAR_LITERAL:
		fprintf(out, "char: '%c' (%d)", t->chr, t->chr);
		break;
	case TOKEN_STR_LITERAL:
		fprintf(out, "str: \"%s\"", t->str.str);
		break;
	case TOKEN_EOF:
		fprintf(out, "eof");
		break;
	}
}

static Token *tokr_add(Tokenizer *t) {
	Token *token = arr_add(&t->tokens);
	token->where.line = t->line;
	token->where.code = t->s;
	return token;
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
	Location where = {t->line, t->s};
	va_start(args, fmt);
	err_vprint(where, fmt, args);
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
static void tokr_err_(const char *src_file, int src_line, Tokenizer *t, const char *fmt, ...) {
	err_fprint("At line %d of %s:\n", src_line, src_file); /* RELEASE: Remove this */
	va_list args;
	va_start(args, fmt);
	err_vprint(t->token->where, fmt, args);
	va_end(args);
}
#define tokr_err(...) tokr_err_(__FILE__, __LINE__, __VA_ARGS__)

static void tokr_put_location(Tokenizer *tokr, Token *t) {
	t->where.line = tokr->line;
	t->where.code = tokr->s;
}

static void tokr_get_location(Tokenizer *tokr, Token *t) {
	tokr->line = t->where.line;
	tokr->s = t->where.code;
}

static bool tokenize_string(Tokenizer *tokr, char *str) {
	int has_err = 0;
	Tokenizer t;
	arr_create(&t.tokens, sizeof(Token));
	arr_reserve(&t.tokens, 256);
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
		{
			char *start_s = t.s;
			Keyword kw = tokenize_kw(&t.s);
			if (kw != KW_COUNT) {
				/* it's a keyword */
				Token *token = tokr_add(&t);
				token->where.line = t.line;
				token->where.code = start_s;
				token->kind = TOKEN_KW;
				token->kw = kw;
				continue;
			}
		}
		
		/* check if it's a number */

		if (isdigit(*t.s)) {
			/* it's a numeric literal */
			int base = 10;
			FloatLiteral decimal_pow10;
			NumLiteral n;
			n.kind = NUM_LITERAL_INT;
			n.intval = 0;
			Token *token = tokr_add(&t);
			tokr_put_location(&t, token);
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
					if (n.kind == NUM_LITERAL_FLOAT) {
						tokenization_err(&t, "Double . in number.");
						goto err;
					}
					if (base != 10) {
						tokenization_err(&t, "Decimal point in non base 10 number.");
						goto err;
					}
				    n.kind = NUM_LITERAL_FLOAT;
					decimal_pow10 = 0.1;
					n.floatval = (FloatLiteral)n.intval;
					tokr_nextchar(&t);
					continue;
				} else if (*t.s == 'e') {
					tokr_nextchar(&t);
					if (n.kind == NUM_LITERAL_INT) {
						n.kind = NUM_LITERAL_FLOAT;
						n.floatval = (FloatLiteral)n.intval;
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
							n.floatval /= 10;
						else
							n.floatval *= 10;
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
					/* end of numeric literal */
					break;
				}
				switch (n.kind) {
				case NUM_LITERAL_INT:
					if (n.intval > ULLONG_MAX / (IntLiteral)base ||
						n.intval * (IntLiteral)base > ULLONG_MAX - (IntLiteral)digit) {
						/* too big! */
						tokenization_err(&t, "Number too big to fit in a numeric literal.");
						goto err;
					}
					n.intval *= (IntLiteral)base;
					n.intval += (IntLiteral)digit;
					break;
				case NUM_LITERAL_FLOAT:
					n.floatval += decimal_pow10 * (FloatLiteral)digit;
					decimal_pow10 /= 10;
					break;
				}
				tokr_nextchar(&t);
			}
			token->kind = TOKEN_NUM_LITERAL;
			token->num = n;
			continue;
		}

		if (*t.s == '\'') {
			/* it's a character literal! */
			tokr_nextchar(&t);
			Token *token = tokr_add(&t);
			tokr_put_location(&t, token);
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
				tokenization_err(&t, "End of character literal expected.");
				goto err;
			}
			tokr_nextchar(&t);
			token->kind = TOKEN_CHAR_LITERAL;
			token->chr = c;
			continue;
		}

		if (*t.s == '"') {
			/* it's a string literal! */
			Token *token = tokr_add(&t);
			tokr_put_location(&t, token);
			tokr_nextchar(&t);
			size_t len = 0;
			size_t backslashes = 0;
			while (*t.s != '"' || backslashes % 2 == 1) {
				if (*t.s == '\\') {
					backslashes++;
				} else if (*t.s == 0) {
					/* return t to opening " so that we go to the next line */
					tokr_get_location(&t, token);
					tokenization_err(&t, "No matching \" found.");
					goto err;
				} else {
					backslashes = 0;
				}
				len++;
				tokr_nextchar(&t);
			}
			char *strlit = malloc(len + 1);
		    char *strptr = strlit;
			tokr_get_location(&t, token);
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
			token->kind = TOKEN_STR_LITERAL;
			token->str.len = len;
			token->str.str = strlit;
			tokr_nextchar(&t); /* move past closing " */
			continue;
		}
		
		if (isident(*t.s)) {
			/* it's an identifier */
			Token *token = tokr_add(&t);
			tokr_put_location(&t, token);
			Identifier ident = ident_insert(&t.s);
			token->kind = TOKEN_IDENT;
			token->ident = ident;
			continue;
		}		
		tokenization_err(&t, "Token not recognized");
	err:
		has_err = 1;
	}
	Token *token = tokr_add(&t);
	token->kind = TOKEN_EOF;
	
	t.token = t.tokens.data;
	*tokr = t;
	return !has_err;
}

/* Does NOT free string literals!!! */
static void tokr_free(Tokenizer *t) {
	arr_clear(&t->tokens);
}
