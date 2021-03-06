/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

static inline const char *kw_to_str(Keyword k) { return keywords[k]; }

// Returns KW_COUNT if it's not a keyword
// @OPTIM: don't use strncmp so much
static Keyword tokenize_kw(char **s) {
	for (Keyword k = 0; k < KW_COUNT; k = k + 1) {
		size_t len = strlen(keywords[k]);
		if (strncmp(*s, keywords[k], len) == 0) {
			if (k > KW_LAST_SYMBOL) {
				/*
				   it's not a symbol, so we need to check if it's something like "intfoo"
				 */
				if (is_ident((*s)[len])) {
					continue;
				}
			}
			*s += len;
			return k;
		}
	}
	return KW_COUNT;
}



// Returns DIRECT_COUNT if it's not a directive
static Directive tokenize_direct(char **s) {
	for (Directive d = 0; d < DIRECT_COUNT; d = d + 1) {
		size_t len = strlen(directives[d]);
		if (strncmp(*s, directives[d], len) == 0) {
			if (is_ident((*s)[len])) {
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

static inline bool token_is_direct(Token *t, Directive d) {
	return t->kind == TOKEN_DIRECT && t->direct == d;
}

static const char *token_kind_to_str(TokenKind t) {
	switch (t) {
	case TOKEN_KW: return "keyword";
	case TOKEN_IDENT: return "identifier";
	case TOKEN_DIRECT: return "directive";
	case TOKEN_LITERAL_INT: return "integer literal";
	case TOKEN_LITERAL_FLOAT: return "floating-point literal";
	case TOKEN_LITERAL_CHAR: return "character literal";
	case TOKEN_LITERAL_STR: return "string literal";
	case TOKEN_EOF: return "end of file";
	}
	assert(0);
	return "";
}

static void fprint_token(FILE *out, Token *t) {
	fprintf(out, "l%lu-", (unsigned long)t->pos.line);
	switch (t->kind) {
	case TOKEN_KW:
		fprintf(out, "keyword: %s", kw_to_str(t->kw));
		break;
	case TOKEN_IDENT: {
		fprintf(out, "identifier: ");
		fprint_ident_str(out, t->ident);
	} break;
	case TOKEN_LITERAL_INT:
		fprintf(out, U64_FMT, t->intl);
		break;
	case TOKEN_LITERAL_FLOAT:
		fprintf(out, "%g", (double)t->floatl);
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

static void print_token(Token *t) {
	fprint_token(stdout, t);
	printf("\n");
}

static inline void tokr_nextchar(Tokenizer *t) {
	if (*(t->s) == '\n') {
		++t->line;
	}
	++t->s;
}

// returns -1 if not a hex digit, otherwise 0-15
static inline int char_as_hex_digit(char c) {
	if (c >= '0' && c <= '9')
		return c - '0';
	if (c >= 'a' && c <= 'f')
		return 10 + c - 'a';
	if (c >= 'A' && c <= 'F')
		return 10 + c - 'A';
	return -1;
}

// returns -1 if escape sequence is invalid
static int tokr_esc_seq(Tokenizer *t) {
	// @TODO: octal (\032)?
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
	case '0':
		tokr_nextchar(t);
		return '\0';
	case 'x': {
		int c1 = char_as_hex_digit(t->s[1]);
		if (c1 == -1) return 0;
		int c2 = char_as_hex_digit(t->s[2]);
		if (c2 == -1) return 0;
		tokr_nextchar(t);
		tokr_nextchar(t);
		tokr_nextchar(t);
		return (char)(c1 * 16 + c2);
	}
	default:
		return -1;
	}

}

static Location token_location(File *file, Token *t) {
	Location loc;
	loc.start = (U32)(t - file->tokens);
	loc.end = loc.start + 1;
	loc.file = file;
	return loc;
}

static void print_token_location(File *file, Token *t) {
	print_location(token_location(file, t));
}

// for use during tokenization
static void tokenization_err_(
#if ERR_SHOW_SOURCE_LOCATION
							  const char *src_file, int src_line,
#endif
							  Tokenizer *t, const char *fmt, ...) {
	va_list args;
	ErrCtx *ctx = t->err_ctx;
	if (!ctx->enabled) return;
#if ERR_SHOW_SOURCE_LOCATION
	err_fprint(ctx, "Generated by line %d of %s:\n", src_line, src_file);
#endif
	va_start(args, fmt);
	err_text_err(ctx, "error");
	err_fprint(ctx, " at line %lu of %s:\n", (unsigned long)t->line, t->file->filename);
	err_vfprint(ctx, fmt, args);
	va_end(args);
	err_fprint(ctx, "\n\t");
	U32 pos = (U32)(t->s - t->file->contents);
	print_pos_highlight(err_ctx_file(ctx), ctx, t->file, pos, pos + 1);
	while (*t->s) {
		if (*t->s == '\n') {
			tokr_nextchar(t);
			break;
		}
		++t->s;
	}
}

#if ERR_SHOW_SOURCE_LOCATION
#define tokenization_err(...) tokenization_err_(__FILE__, __LINE__, __VA_ARGS__)
#else
#define tokenization_err tokenization_err_
#endif

// for use after tokenization
static void tokr_err_(
#if ERR_SHOW_SOURCE_LOCATION
					  const char *src_file, int src_line,
#endif
					  Tokenizer *t, const char *fmt, ...) {
	ErrCtx *ctx = t->err_ctx;
	if (!ctx->enabled) return;
#if ERR_SHOW_SOURCE_LOCATION
	err_fprint(ctx, "Generated by line %d of %s:\n", src_line, src_file);
#endif
	va_list args;
	va_start(args, fmt);
	err_vprint(token_location(t->file, t->token), fmt, args);
	va_end(args);
}

#if ERR_SHOW_SOURCE_LOCATION
#define tokr_err(...) tokr_err_(__FILE__, __LINE__, __VA_ARGS__)
#else
#define tokr_err tokr_err_
#endif


static void tokr_put_start_pos(Tokenizer *tokr, Token *t) {
	t->pos.line = tokr->line;
	t->pos.start = (U32)(tokr->s - tokr->file->contents);
}

static void tokr_put_end_pos(Tokenizer *tokr, Token *t) {
	t->pos.end = (U32)(tokr->s - tokr->file->contents);
}

static void tokr_get_start_pos(Tokenizer *tokr, Token *t) {
	tokr->line = t->pos.line;
	tokr->s = tokr->file->contents + t->pos.start;
}

/*
the allocator you pass it will be used for string literals, so it shouldn't be freed
until everything is done
*/
static void tokr_create(Tokenizer *t, ErrCtx *err_ctx, Allocator *allocr) {
	t->tokens = NULL;
	arr_resva(t->tokens, 256, allocr);
	t->allocr = allocr;
	t->err_ctx = err_ctx;
}

static inline void *tokr_malloc(Tokenizer *t, size_t bytes) {
	return allocr_malloc(t->allocr, bytes);
}

static Token *tokr_add(Tokenizer *t) {
	Token *token = arr_adda_ptr(t->tokens, t->allocr);
	tokr_put_start_pos(t, token);
	return token;
}

static Status tokenize_file(Tokenizer *t, File *file) {
	int has_err = 0;
	t->s = file->contents;
	t->file = file;
	t->line = 1;
	while (1) {
		if (*t->s == 0) break;
		if (isspace(*t->s)) {
			tokr_nextchar(t);
			continue;
		}
		
		if (*t->s == '/') {
			// maybe it's a comment
			int is_comment = 1;
			switch (t->s[1]) {
			case '/': // single line comment
				tokr_nextchar(t);
				while (*t->s && *t->s != '\n') ++t->s;
				if (*t->s) tokr_nextchar(t); // skip newline
				break;
			case '*': { // multi line comment
				tokr_nextchar(t);
				int comment_level = 1; // allow nested multi-line comments
				while (1) {
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
						if (*t->s == 0) {
							tokenization_err(t, "End of file reached inside multi-line comment.");
							return false;
						}

						tokr_nextchar(t);
					}
				}
			} break;
			default:
				is_comment = 0;
				break;
			}
			if (is_comment) continue;
		}
		
		if (*t->s == '#') {
			// it's a directive
			Token token;
			tokr_put_start_pos(t, &token);
			++t->s; // move past #
			Directive direct = tokenize_direct(&t->s);
			if (direct != DIRECT_COUNT) {
				// it's a directive
				if (direct == DIRECT_NO_WARN) {
					arr_adda(file->no_warn_lines, t->line, t->allocr);
				} else {
					tokr_put_end_pos(t, &token);
					token.kind = TOKEN_DIRECT;
					token.direct = direct;
					arr_adda(t->tokens, token, t->allocr);
				}
				continue;
			}
			--t->s; // go back to #
			tokenization_err(t, "Unrecognized directive.");
			goto err;
		}
		
		{
			Token token;
			tokr_put_start_pos(t, &token);
			Keyword kw = tokenize_kw(&t->s);
			if (kw != KW_COUNT) {
				// it's a keyword
				tokr_put_end_pos(t, &token);
				token.kind = TOKEN_KW;
				token.kw = kw;
				arr_adda(t->tokens, token, t->allocr);
				continue;
			}
		}
		
		// check if it's a number
		if (isdigit(*t->s)) {
			// it's a numeric literal
			int base = 10;
			Floating decimal_pow10 = 0;
			Token *token = tokr_add(t);
			token->kind = TOKEN_LITERAL_INT;
			token->intl = 0;
			
			if (*t->s == '0') {
				tokr_nextchar(t);
				// octal/hexadecimal/binary (or zero)
				char format = *t->s;
				if (isdigit(format)) {
					--t->s;
					tokenization_err(t, "Number starts with a 0. If you're trying to do an octal number, use 0o (e.g. 0o54123).");
					goto err;
				}
				switch (format) {
				case 'b':
					base = 2;
					tokr_nextchar(t);
					break;
				case 'x':
					base = 16;
					tokr_nextchar(t);
					break;
				case 'o': // also octal
					base = 8;
					tokr_nextchar(t);
					break;
				default:
					// it's 0/0.something etc.
					break;
				}
			}
			while (1) {
				if (*t->s == '.') {
					if (!isdigit(t->s[1])) {
						// not a decimal point; end the number here (could be .. or .,)
						break;
					}
					if (token->kind == TOKEN_LITERAL_FLOAT) {
						tokenization_err(t, "Double . in number.");
						goto err;
					}
					if (base != 10) {
						tokenization_err(t, "Decimal point in non base 10 number.");
						goto err;
					}
					token->kind = TOKEN_LITERAL_FLOAT;
					decimal_pow10 = 0.1;
					U64 i = token->intl;
					token->floatl = (Floating)i;
					tokr_nextchar(t);
					continue;
				} else if (*t->s == 'e' && base != 16) {
					tokr_nextchar(t);
					if (token->kind == TOKEN_LITERAL_INT) {
						token->kind = TOKEN_LITERAL_FLOAT;
						U64 i = token->intl;
						token->floatl = (Floating)i;
					}
					// @TODO: check if exceeding maximum exponent
					int exponent = 0;
					if (*t->s == '+')
						tokr_nextchar(t); // ignore + after e
					
					int negative_exponent = 0;
					if (*t->s == '-') {
						tokr_nextchar(t);
						negative_exponent = 1;
					}
					for (; isdigit(*t->s); tokr_nextchar(t)) {
						exponent *= 10;
						exponent += *t->s - '0';
					}
					// @OPTIM: Slow for very large exponents (unlikely to happen)
					for (int i = 0; i < exponent; ++i) {
						if (negative_exponent)
							token->floatl /= 10;
						else
							token->floatl *= 10;
					}
						
					break;
				}
				int digit = -1;
				if (base == 16) {
					if (*t->s >= 'a' && *t->s <= 'f')
						digit = 10 + *t->s - 'a';
					else if (*t->s >= 'A' && *t->s <= 'F')
						digit = 10 + *t->s - 'A';
				}
				if (digit == -1) {
					if (*t->s >= '0' && *t->s <= '9')
						digit = *t->s - '0';
				}
				if (digit < 0 || digit >= base) {
					if (isdigit(*t->s)) {
						// something like 0b011012
						tokenization_err(t, "Digit %d cannot appear in a base %d number.", digit, base);
						goto err;
					}
					// end of numeric literal
					break;
				}
				switch (token->kind) {
				case TOKEN_LITERAL_INT:
					if (token->intl > U64_MAX / (U64)base ||
						token->intl * (U64)base > U64_MAX - (U64)digit) {
						// too big!
						tokenization_err(t, "Number too big to fit in a numeric literal.");
						goto err;
					}
					token->intl *= (U64)base;
					token->intl += (U64)digit;
					break;
				case TOKEN_LITERAL_FLOAT:
					token->floatl += decimal_pow10 * (Floating)digit;
					decimal_pow10 /= 10;
					break;
				default: break;
				}
				tokr_nextchar(t);
			}
			tokr_put_end_pos(t, token);
			continue;
		}

		if (*t->s == '\'') {
			// it's a character literal!
			Token *token = tokr_add(t);
			tokr_nextchar(t);
			char c;
			if (*t->s == '\\') {
				// escape sequence
				tokr_nextchar(t);
				int e = tokr_esc_seq(t);
				if (e == -1) {
					tokenization_err(t, "Unrecognized escape character: '\\%c'.", *t->s);
					goto err;
				}
				c = (char)e;
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
			tokr_put_end_pos(t, token);
			continue;
		}

		if (*t->s == '"') {
			// it's a string literal!
			Token *token = tokr_add(t);
			tokr_nextchar(t);
			size_t len = 0; // counts \n as 2 chars
			size_t backslashes = 0;
			while (*t->s != '"' || backslashes % 2 == 1) {
				if (*t->s == '\\') {
					++backslashes;
				} else if (*t->s == 0) {
					// return t to opening "
					tokr_get_start_pos(t, token);
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
			tokr_get_start_pos(t, token);
			tokr_nextchar(t); // past opening "
			while (*t->s != '"') {
				assert(*t->s);
				if (*t->s == '\\') {
					tokr_nextchar(t);
					int c = tokr_esc_seq(t);
					if (c == -1) {
						tokenization_err(t, "Unrecognized escape character: '\\%c'.", *t->s);
						goto err;
					}
					*strptr++ = (char)c;
				} else {
					*strptr++ = *t->s;
					tokr_nextchar(t);
				}
			}
			*strptr = 0;
			token->kind = TOKEN_LITERAL_STR;
			token->str.len = (size_t)(strptr - strlit);
			token->str.str = strlit;
			tokr_nextchar(t); // move past closing "
			tokr_put_end_pos(t, token);
			continue;
		}

		if (is_ident(*t->s)) {
			// it's an identifier
			Token *token = tokr_add(t);
			token->kind = TOKEN_IDENT;
			token->ident = t->s;
			while (is_ident(*t->s)) ++t->s;
			tokr_put_end_pos(t, token);
			continue;
		}
		tokenization_err(t, "Token not recognized");
	err:
		has_err = 1;
	}
	Token *token = tokr_add(t);
	token->kind = TOKEN_EOF;
	
	t->token = t->tokens;
	file->tokens = t->tokens;
	return !has_err;
}

// skip to one token past the next semicolon not in braces (or the end of the file).
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

static inline void tokr_skip_to_eof(Tokenizer *t) {
	while (t->token->kind != TOKEN_EOF) ++t->token; // move to end of file
}
