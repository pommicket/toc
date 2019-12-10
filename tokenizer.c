/*
  Copyright (C) 2019 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static const char *keywords[KW_COUNT] =
	{";", "::", ":", ",", "(", ")", "{", "}", "[", "]", "==",
	 "+=", "-=", "*=", "/=",
	 "!=", "<=", "<", ">=", ">",
	 "+", "-", "*", "!", "&", "/", "..", ".",
	 "=",
	 "if", "elif", "else", "while", "each", "return", "fn", "as",
	 "new", "del", "struct",
	 "int", "i8", "i16", "i32", "i64",
	 "u8", "u16", "u32", "u64", "float", "f32", "f64", "Type",
	 "char", "bool", "true", "false"};

static inline const char *kw_to_str(Keyword k) { return keywords[k]; }

static const char *directives[DIRECT_COUNT] =
	{"C", "sizeof", "alignof"};

/* Returns KW_COUNT if it's not a keyword */
/* OPTIM: don't use strncmp so much */
static Keyword tokenize_kw(char **s) {
	for (Keyword k = 0; k < KW_COUNT; k = k + 1) {
		size_t len = strlen(keywords[k]);
		if (strncmp(*s, keywords[k], len) == 0) {
			if (k > KW_LAST_SYMBOL) {
				/* 
				   it's not a symbol, so we need to check if it's something like "intfoo"
				 */
				if (isident((*s)[len])) {
					continue;
				}
			}
			*s += len;
			return k;
		}
	}
	return KW_COUNT;
}



/* Returns DIRECT_COUNT if it's not a directive */
static Directive tokenize_direct(char **s) {
	for (Directive d = 0; d < DIRECT_COUNT; d = d + 1) {
		size_t len = strlen(directives[d]);
		if (strncmp(*s, directives[d], len) == 0) {
			if (isident((*s)[len])) {
				continue;
			}
			*s += len;
			return d;
		}
	}
	return DIRECT_COUNT;
}

static inline bool token_is_kw(Token *t, Keyword kw) {
	return t->kind == TOKEN_KW && t->kw == kw;
}

static const char *token_kind_to_str(TokenKind t) {
	switch (t) {
	case TOKEN_KW: return "keyword";
	case TOKEN_IDENT: return "identifier";
	case TOKEN_DIRECT: return "directive";
	case TOKEN_LITERAL_NUM: return "numerical literal";
	case TOKEN_LITERAL_CHAR: return "character literal";
	case TOKEN_LITERAL_STR: return "string literal";
	case TOKEN_EOF: return "end of file";
	}
	assert(0);
	return "";
}

static void fprint_token(FILE *out, Token *t) {
	fprintf(out, "l%lu-", (unsigned long)t->where.line);
	switch (t->kind) {
	case TOKEN_KW:
		fprintf(out, "keyword: %s", kw_to_str(t->kw));
		break;
	case TOKEN_IDENT:
		fprintf(out, "identifier: %p: ", (void*)t->ident);
		fprint_ident(out, t->ident);
		break;
	case TOKEN_LITERAL_NUM:
		fprintf(out, "number: ");
		switch (t->num.kind) {
		case NUM_LITERAL_INT:
			fprintf(out, INTEGER_FMT, t->num.intval);
			break;
		case NUM_LITERAL_FLOAT:
			fprintf(out, "%g", (double)t->num.floatval);
			break;
		}
		break;
	case TOKEN_LITERAL_CHAR:
		fprintf(out, "char: '%c' (%d)", t->chr, t->chr);
		break;
	case TOKEN_LITERAL_STR:
		fprintf(out, "str: \"%s\"", t->str.str);
		break;
	case TOKEN_DIRECT:
		fprintf(out, "directive: #%s", directives[t->direct]);
		break;
	case TOKEN_EOF:
		fprintf(out, "eof");
		break;
	}
}

static inline void tokr_nextchar(Tokenizer *t) {
	if (*(t->s) == '\n') {
		++t->line;
	}
	++t->s;
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
	Location where;
	where.line = t->line;
	where.ctx = t->err_ctx;
	where.pos = (CodePos)(t->s - where.ctx->str);
	va_start(args, fmt);
	err_vprint(where, fmt, args);
	va_end(args);
	char *end_of_line = strchr(t->s, '\n');
	if (end_of_line) {
		t->s = end_of_line;
		++t->s; /* move past newline */
	} else {
		t->s = strchr(t->s, '\0');
	}
	++t->line;
}

/* to be used after tokenization */
static void tokr_err_(
#if ERR_SHOW_SOURCE_LOCATION
					  const char *src_file, int src_line,
#endif
					  Tokenizer *t, const char *fmt, ...) {
	if (!t->token->where.ctx->enabled) return;
#if ERR_SHOW_SOURCE_LOCATION
	err_fprint("At line %d of %s:\n", src_line, src_file);
#endif
	va_list args;
	va_start(args, fmt);
	err_vprint(t->token->where, fmt, args);
	va_end(args);
}

#if ERR_SHOW_SOURCE_LOCATION
#define tokr_err(...) tokr_err_(__FILE__, __LINE__, __VA_ARGS__)
#else
#define tokr_err tokr_err_
#endif

static void tokr_put_location(Tokenizer *tokr, Token *t) {
	t->where.line = tokr->line;
	t->where.ctx = tokr->err_ctx;
	t->where.pos = (CodePos)(tokr->s - t->where.ctx->str);
}

static void tokr_get_location(Tokenizer *tokr, Token *t) {
	tokr->line = t->where.line;
	tokr->s = t->where.pos + t->where.ctx->str;
}

/* 
the allocator you pass it will be used for string literals, so it shouldn't be freed
until everything is done
*/
static void tokr_create(Tokenizer *t, Identifiers *idents, ErrCtx *err_ctx, Allocator *allocr) {
	t->tokens = NULL;
	arr_resv(&t->tokens, 256);
	t->allocr = allocr;
	t->idents = idents;
	t->err_ctx = err_ctx;
}

static inline void *tokr_malloc(Tokenizer *t, size_t bytes) {
	return allocr_malloc(t->allocr, bytes);
}

static Token *tokr_add(Tokenizer *t) {
	Token *token = arr_add(&t->tokens);
	tokr_put_location(t, token);
	return token;
}

static bool tokenize_string(Tokenizer *t, char *str) {
	int has_err = 0;
	t->s = str;
	t->err_ctx->str = str;
	t->line = 1;
	while (1) {
		if (*t->s == 0) break;
		if (isspace(*t->s)) {
			tokr_nextchar(t);
	    	continue;
		}
		
		if (*t->s == '/') {
			/* maybe it's a comment */
			int is_comment = 1;
			switch (t->s[1]) {
			case '/': /* single line comment */
				tokr_nextchar(t);
				while (*t->s && *t->s != '\n') ++t->s;
				if (*t->s) tokr_nextchar(t); /* skip newline */
				break;
			case '*': { /* multi line comment */
				tokr_nextchar(t);
				int comment_level = 1; /* allow nested multi-line comments */
			    while (*t->s) {
					if (t->s[0] == '*' && t->s[1] == '/') {
						t->s += 2;
						--comment_level;
						if (comment_level == 0) {
							break;
						}
					} else if (t->s[0] == '/' && t->s[1] == '*') {
						t->s += 2;
						++comment_level;
					} else {
						tokr_nextchar(t);
					}
				}
				if (*t->s == 0) {
					tokenization_err(t, "End of file reached inside multi-line comment.");
					abort(); /* there won't be any further errors, of course */
				}
			} break;
			default:
				is_comment = 0;
				break;
			}
			if (is_comment) continue;
		}
		
		if (*t->s == '#') {
			/* it's a directive */
			char *start_s = t->s;
			++t->s; /* move past # */
		    Directive direct = tokenize_direct(&t->s);
			if (direct != DIRECT_COUNT) {
				/* it's a directive */
				Token *token = tokr_add(t);
				tokr_put_location(t, token);
				token->where.pos = (CodePos)(start_s - token->where.ctx->str);
				token->kind = TOKEN_DIRECT;
				token->direct = direct;
				continue;
			}
			--t->s; /* go back to # */
			tokenization_err(t, "Unrecognized directive.");
			goto err;
		}
		
		{
			char *start_s = t->s;
			Keyword kw = tokenize_kw(&t->s);
			if (kw != KW_COUNT) {
				/* it's a keyword */
				Token *token = tokr_add(t);
				tokr_put_location(t, token);
				token->where.pos = (CodePos)(start_s - token->where.ctx->str);
				token->kind = TOKEN_KW;
				token->kw = kw;
				continue;
			}
		}
		
		/* check if it's a number */
		if (isdigit(*t->s)) {
			/* it's a numeric literal */
			int base = 10;
			Floating decimal_pow10 = 0;
			Token *token = tokr_add(t);
			tokr_put_location(t, token);
			NumLiteral *n = &token->num;
			n->kind = NUM_LITERAL_INT;
			n->intval = 0;
			
			if (*t->s == '0') {
				tokr_nextchar(t);
				/* octal/hexadecimal/binary (or zero) */
				char format = *t->s;
				if (isdigit(format)) /* octal */
					base = 8;
				else {
					switch (format) {
					case 'b':
						base = 2;
						tokr_nextchar(t);
						break;
					case 'x':
						base = 16;
						tokr_nextchar(t);
						break;
					default:
						/* it's 0/0.something etc.  */
						break;
					}
				}
			}
			while (1) {
				if (*t->s == '.') {
					if (t->s[1] == '.') {
						/* .. (not a decimal point; end the number here) */
						break;
					}
					if (n->kind == NUM_LITERAL_FLOAT) {
						tokenization_err(t, "Double . in number.");
						goto err;
					}
					if (base != 10) {
						tokenization_err(t, "Decimal point in non base 10 number.");
						goto err;
					}
				    n->kind = NUM_LITERAL_FLOAT;
					decimal_pow10 = 0.1;
					n->floatval = (Floating)n->intval;
					tokr_nextchar(t);
					continue;
				} else if (*t->s == 'e') {
					tokr_nextchar(t);
					if (n->kind == NUM_LITERAL_INT) {
						n->kind = NUM_LITERAL_FLOAT;
						n->floatval = (Floating)n->intval;
					}
					/* TODO: check if exceeding maximum exponent */
					int exponent = 0;
					if (*t->s == '+')
						tokr_nextchar(t); /* ignore + after e */
					
					int negative_exponent = 0;
					if (*t->s == '-') {
						tokr_nextchar(t);
						negative_exponent = 1;
					}
					for (; isdigit(*t->s); tokr_nextchar(t)) {
						exponent *= 10;
						exponent += *t->s - '0';
					}
					/* OPTIM: Slow for very large exponents (unlikely to happen) */
					for (int i = 0; i < exponent; ++i) {
						if (negative_exponent)
							n->floatval /= 10;
						else
							n->floatval *= 10;
					}
						
					break;
				}
				int digit = -1;
				if (base == 16) {
					if (*t->s >= 'a' && *t->s <= 'f')
						digit = 10 + *t->s - 'a';
					else if (*t->s >= 'A' && *t->s <= 'F')
						digit = *t->s - 'A';
				}
				if (digit == -1) {
					if (*t->s >= '0' && *t->s <= '9')
						digit = *t->s - '0';
				}
				if (digit < 0 || digit >= base) {
					if (isdigit(*t->s)) {
						/* something like 0b011012 */
						tokenization_err(t, "Digit %d cannot appear in a base %d number.", digit, base);
						goto err;
					}
					/* end of numeric literal */
					break;
				}
				switch (n->kind) {
				case NUM_LITERAL_INT:
					if ((UInteger)n->intval > (UInteger)UINTEGER_MAX / (UInteger)base ||
						(UInteger)n->intval * (UInteger)base > (UInteger)UINTEGER_MAX - (UInteger)digit) {
						/* too big! */
						tokenization_err(t, "Number too big to fit in a numeric literal.");
						goto err;
					}
					n->intval *= (UInteger)base;
					n->intval += (UInteger)digit;
					break;
				case NUM_LITERAL_FLOAT:
					n->floatval += decimal_pow10 * (Floating)digit;
					decimal_pow10 /= 10;
					break;
				}
				tokr_nextchar(t);
			}
		    
			token->kind = TOKEN_LITERAL_NUM;
			continue;
		}

		if (*t->s == '\'') {
			/* it's a character literal! */
			Token *token = tokr_add(t);
			tokr_put_location(t, token);
			tokr_nextchar(t);
			char c;
			if (*t->s == '\\') {
				/* escape sequence */
				tokr_nextchar(t);
				c = tokr_esc_seq(t);
				if (c == 0) {
					tokenization_err(t, "Unrecognized escape character: '\\%c'.", *t->s);
					goto err;
				}
			} else {
				c = *t->s;
				tokr_nextchar(t);
			}
			if (*t->s != '\'') {
				tokenization_err(t, "End of character literal expected.");
				goto err;
			}
			tokr_nextchar(t);
			token->kind = TOKEN_LITERAL_CHAR;
			token->chr = c;
			continue;
		}

		if (*t->s == '"') {
			/* it's a string literal! */
			Token *token = tokr_add(t);
			tokr_put_location(t, token);
			tokr_nextchar(t);
			size_t len = 0; /* counts \n as 2 chars */
			size_t backslashes = 0;
			while (*t->s != '"' || backslashes % 2 == 1) {
				if (*t->s == '\\') {
					++backslashes;
				} else if (*t->s == 0) {
					/* return t to opening " so that we go to the next line */
					tokr_get_location(t, token);
					tokenization_err(t, "No matching \" found.");
					goto err;
				} else {
					backslashes = 0;
				}
				++len;
				tokr_nextchar(t);
			}
			char *strlit = tokr_malloc(t, len + 1);
		    char *strptr = strlit;
			tokr_get_location(t, token);
			tokr_nextchar(t); /* past opening " */
			while (*t->s != '"') {
				assert(*t->s);
				if (*t->s == '\\') {
					tokr_nextchar(t);
					char c = tokr_esc_seq(t);
					if (c == 0) {
						tokenization_err(t, "Unrecognized escape character: '\\%c'.", *t->s);
						goto err;
					}
					*strptr++ = c;
				} else {
					*strptr++ = *t->s;
					tokr_nextchar(t);
				}
			}
			*strptr = 0;
			token->kind = TOKEN_LITERAL_STR;
			token->str.len = (size_t)(strptr - strlit);
			token->str.str = strlit;
			tokr_nextchar(t); /* move past closing " */
			continue;
		}

		if (isident(*t->s)) {
			/* it's an identifier */
			Token *token = tokr_add(t);
			tokr_put_location(t, token);
			Identifier ident = ident_insert(t->idents, &t->s);
			token->kind = TOKEN_IDENT;
			token->ident = ident;
			continue;
		}		
		tokenization_err(t, "Token not recognized");
	err:
		has_err = 1;
	}
	Token *token = tokr_add(t);
	token->kind = TOKEN_EOF;
	
	t->token = t->tokens;
	return !has_err;
}

/* 
   skip to one token past the next semicolon not in braces (or the end of the file).
*/
static void tokr_skip_semicolon(Tokenizer *t) {
	int brace_level = 0;
	while (t->token->kind != TOKEN_EOF) {
		if (t->token->kind == TOKEN_KW) switch (t->token->kw) {
			case KW_LBRACE: ++brace_level; break;
			case KW_RBRACE: --brace_level; break;
			case KW_SEMICOLON:
				if (brace_level == 0) {
					++t->token;
					return;
				}
				break;
			default: break;
			}
		++t->token;
	}
}

/* only frees tokens, not string literals (because those are on the allocator). */
static void tokr_free(Tokenizer *t) {
    arr_clear(&t->tokens);
}
