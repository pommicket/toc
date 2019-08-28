/* TODO: stmt_parse -> parse_stmt, etc. */
typedef enum {
			  TYPE_VOID,
			  TYPE_BUILTIN,
			  TYPE_FN
} TypeKind;

typedef enum {
			  BUILTIN_INT,
			  BUILTIN_I8,
			  BUILTIN_I16,
			  BUILTIN_I32,
			  BUILTIN_I64,
			  BUILTIN_U8,
			  BUILTIN_U16,
			  BUILTIN_U32,
			  BUILTIN_U64,
			  BUILTIN_FLOAT,
			  BUILTIN_DOUBLE,
			  BUILTIN_TYPE_COUNT
} BuiltinType;


typedef struct Type {
	Location where;
	TypeKind kind;
	union {
	    BuiltinType builtin;
		struct {
			Array types; /* [0] = ret_type, [1..] = param_types */
		} fn;
	};
} Type;

typedef struct {
	Identifier name;
	Type type;
} Param;

typedef struct Block {
	Array stmts;
} Block;

typedef struct {
	Array params;
	Type ret_type;
	Block body;
	Identifier name; /* NULL if the function is anonymous (set to NULL by parse.c, set to actual value by types_cgen.c) */
	unsigned long id; /* this is used to keep track of local vs global/other local functions (there might be multiple functions called "foo") */
} FnExpr; /* an expression such as fn(x: int) int {return 2 * x;} */

typedef enum {
			  EXPR_INT_LITERAL,
			  EXPR_FLOAT_LITERAL,
			  EXPR_STR_LITERAL,
			  EXPR_IDENT, /* variable or constant */
			  EXPR_BINARY_OP,
			  EXPR_UNARY_OP,
			  EXPR_FN,
			  EXPR_CALL
} ExprKind;

typedef enum {
			  UNARY_MINUS
} UnaryOp;

typedef enum {
			  BINARY_PLUS,
			  BINARY_MINUS
} BinaryOp;

#define EXPR_FLAG_FLEXIBLE 0x01	/* e.g. 4 => float/i32/etc. */

typedef struct Expression {
	Location where;
	ExprKind kind;
	Type type;
	unsigned short flags;
	union {
		FloatLiteral floatl;
		IntLiteral intl;
		StrLiteral strl;
		struct {
			UnaryOp op;
			struct Expression *of;
		} unary;
		struct {
			BinaryOp op;
			struct Expression *lhs;
			struct Expression *rhs;
		} binary;
		struct {
			struct Expression *fn;
			Array args;	/* of expression */
		} call;
		Identifier ident;
		FnExpr fn;
	};
} Expression;

#define DECL_FLAG_INFER_TYPE 0x01
#define DECL_FLAG_CONST 0x02
#define DECL_FLAG_HAS_EXPR 0x04
#define DECL_FLAG_FOUND_TYPE 0x08

/* OPTIM: Instead of using dynamic arrays, do two passes. */
typedef struct Declaration {
	Location where;
	Array idents;
	Type type;
	Expression expr;
	unsigned short flags;
} Declaration;

typedef enum {
			  STMT_DECL,
			  STMT_EXPR
} StatementKind;
	
typedef struct {
	Location where;
	StatementKind kind;
	union {
		Declaration decl;
		Expression expr;
	};
} Statement;

typedef struct {
	Array stmts;
} ParsedFile;

typedef struct {
	Tokenizer *tokr;
	BlockArr exprs; /* a dynamic array of expressions, so that we don't need to call malloc every time we make an expression */
	Block *block; /* which block are we in? NULL = file scope */
} Parser;

/* 
   allocate a new expression.
   IMPORTANT: This invalidates all other parser-allocated Expression pointers.
*/
static Expression *parser_new_expr(Parser *p) {
	return block_arr_add(&p->exprs);
}

/* returns BUILTIN_TYPE_COUNT on failure */
static BuiltinType kw_to_builtin_type(Keyword kw) {
	switch (kw) {
	case KW_INT: return BUILTIN_INT;
	case KW_I8: return BUILTIN_I8;
	case KW_I16: return BUILTIN_I16;
	case KW_I32: return BUILTIN_I32;
	case KW_I64: return BUILTIN_I64;
	case KW_U8: return BUILTIN_U8;
	case KW_U16: return BUILTIN_U16;
	case KW_U32: return BUILTIN_U32;
	case KW_U64: return BUILTIN_U64;
	case KW_FLOAT: return BUILTIN_FLOAT;
	case KW_DOUBLE: return BUILTIN_DOUBLE;
	default: return BUILTIN_TYPE_COUNT;
	}
}

static Keyword builtin_type_to_kw(BuiltinType t) {
	switch (t) {
	case BUILTIN_INT: return KW_INT;
	case BUILTIN_I8: return KW_I8;
	case BUILTIN_I16: return KW_I16;
	case BUILTIN_I32: return KW_I32;
	case BUILTIN_I64: return KW_I64;
	case BUILTIN_U8: return KW_U8;
	case BUILTIN_U16: return KW_U16;
	case BUILTIN_U32: return KW_U32;
	case BUILTIN_U64: return KW_U64;
	case BUILTIN_FLOAT: return KW_FLOAT;
	case BUILTIN_DOUBLE: return KW_DOUBLE;
	case BUILTIN_TYPE_COUNT: break;
	}
	assert(0);
	return KW_COUNT;
}

static bool parse_type(Parser *p, Type *type) {
	Tokenizer *t = p->tokr;
	type->where = t->token->where;
	switch (t->token->kind) {
	case TOKEN_KW:
		type->kind = TYPE_BUILTIN;
		type->builtin = kw_to_builtin_type(t->token->kw);
		if (type->builtin == BUILTIN_TYPE_COUNT) {
			/* Not a builtin */
			if (t->token->kw == KW_FN) {
				/* function type */
				type->kind = TYPE_FN;
				arr_create(&type->fn.types, sizeof(Type));
				t->token++;
				if (!token_is_kw(t->token, KW_LPAREN)) {
					tokr_err(t, "Expected ( for function type.");
					return false;
				}
				arr_add(&type->fn.types); /* add return type */
				t->token++;
				if (!token_is_kw(t->token, KW_RPAREN)) {
					while (1) {
						Type *param_type = arr_add(&type->fn.types);
						if (!parse_type(p, param_type)) return false;
						if (token_is_kw(t->token, KW_RPAREN))
							break;
						if (!token_is_kw(t->token, KW_COMMA)) {
							tokr_err(t, "Expected , to continue function type parameter list.");
							return false;
						}
						t->token++; /* move past , */
					}
				}
				t->token++;	/* move past ) */
				Type *ret_type = type->fn.types.data;
				/* if there's a symbol, that can't be the start of a type */
				if (t->token->kind == TOKEN_KW
					&& t->token->kw <= KW_LAST_SYMBOL) {
					ret_type->kind = TYPE_VOID;
				} else {
					if (!parse_type(p, ret_type))
						return false;
				}
				return true;
			}
			
			break;
		} else {
			t->token++;
			return true;
		}
		break;
	default: break;
	}
	tokr_err(t, "Unrecognized type.");
	return false;
}

static bool parse_param(Parser *parser, Param *p) {
	Tokenizer *t = parser->tokr;
	if (t->token->kind != TOKEN_IDENT) {
		tokr_err(t, "Expected parameter name.");
		return false;
	}
	p->name = t->token->ident;
	t->token++;
	if (!token_is_kw(t->token, KW_COLON)) {
		tokr_err(t, "Expected ':' between parameter name and type.");
		return false;
	}
	t->token++;
	if (!parse_type(parser, &p->type))
		return false;
	return true;
}

static bool parse_stmt(Parser *p, Statement *s);

static bool parse_block(Parser *p, Block *b) {
	Tokenizer *t = p->tokr;
	Block *prev_block = p->block;
	p->block = b;
	if (!token_is_kw(t->token, KW_LBRACE)) {
		tokr_err(t, "Expected '{' to open block.");
		return false;
	}
	t->token++;	/* move past { */
	arr_create(&b->stmts, sizeof(Statement));
	bool ret = true;
	if (!token_is_kw(t->token, KW_RBRACE)) {
		/* non-empty function body */
		while (1) {
			Statement *stmt = arr_add(&b->stmts);
			if (!parse_stmt(p, stmt)) {
				ret = false;
			}
			if (token_is_kw(t->token, KW_RBRACE)) break;
			if (t->token->kind == TOKEN_EOF) {
				tokr_err(t, "Expected '}' to close function body.");
				return false;
			}
		}
	}
	
	t->token++;	/* move past } */
	p->block = prev_block;
	return ret;
}

static bool parse_fn_expr(Parser *p, FnExpr *f) {
	Tokenizer *t = p->tokr;
	/* only called when token is fn */
	assert(token_is_kw(t->token, KW_FN));
	f->name = NULL;
	t->token++;
	if (!token_is_kw(t->token, KW_LPAREN)) {
		tokr_err(t, "Expected '(' after 'fn'.");
		return false;
	}
	arr_create(&f->params, sizeof(Param));
	
	t->token++;
	
	if (!token_is_kw(t->token, KW_RPAREN)) {
		/* non-empty parameter list */
		while (1) {
			Param *param = arr_add(&f->params);
			if (!parse_param(p, param))
				return false;
			if (token_is_kw(t->token, KW_RPAREN)) break;
			if (token_is_kw(t->token, KW_COMMA)) {
				t->token++;
				continue;
			}
			tokr_err(t, "Expected ',' or ')' to continue or end parameter list.");
			return false;
		}
	}
	
	t->token++;	/* move past ) */
	if (token_is_kw(t->token, KW_LBRACE)) {
		/* void function */
		f->ret_type.kind = TYPE_VOID;
	} else {
		if (!parse_type(p, &f->ret_type)) {
			return false;
		}
	}
	return parse_block(p, &f->body);
}

#define NOT_AN_OP -1
static int op_precedence(Keyword op) {
	switch (op) {
	case KW_PLUS:
		return 10;
	case KW_MINUS:
		return 20;
	default:
		return NOT_AN_OP;
	}
}


/*
  ends_with = which keyword does this expression end with?
  if it's KW_RPAREN, this will match parentheses properly.
*/
typedef enum {
			  EXPR_END_RPAREN_OR_COMMA,
			  EXPR_END_SEMICOLON
} ExprEndKind;
static Token *expr_find_end(Parser *p, ExprEndKind ends_with)  {
	Tokenizer *t = p->tokr;
	int bracket_level = 0;
	int brace_level = 0;
	Token *token = t->token;
	while (1) {
		switch (ends_with) {
		case EXPR_END_RPAREN_OR_COMMA:
			if (token->kind == TOKEN_KW) {
				switch (token->kw) {
				case KW_COMMA:
					if (bracket_level == 0)
						return token;
					break;
				case KW_LPAREN:
					bracket_level++;
					break;
				case KW_RPAREN:
					bracket_level--;
					if (bracket_level < 0)
						return token;
					break;
				default: break;
				}
			}
			break;
		case EXPR_END_SEMICOLON:
			if (token->kind == TOKEN_KW) {
				switch (token->kw) {
				case KW_SEMICOLON:
					/* ignore semicolons inside braces {} */
					if (brace_level == 0)
						return token;
					break;
				case KW_LBRACE:
					brace_level++;
					break;
				case KW_RBRACE:
					brace_level--;
					if (brace_level < 0) {
						t->token = token;
						tokr_err(t, "Closing '}' without matching opening '{'.");
						return NULL;
					}
					break;
				default: break;
				}
			}
			break;
		}
		if (token->kind == TOKEN_EOF) {
			switch (ends_with) {
			case EXPR_END_SEMICOLON:
				if (brace_level > 0) {
					tokr_err(t, "Opening brace was never closed."); /* FEATURE: Find out where this is */
					return NULL;
				} else {
					tokr_err(t, "Could not find ';' at end of expression.");
					return NULL;
				}
			case EXPR_END_RPAREN_OR_COMMA:
				if (bracket_level > 0) {
					tokr_err(t, "Opening parenthesis was never closed."); /* FEATURE: Find out where this is */
					return NULL;
				} else {
					tokr_err(t, "Could not find ')' or ',' at end of expression.");
					return NULL;
				}
			}
		}
		token++;
	}
}

static bool parse_expr(Parser *p, Expression *e, Token *end) {
	Tokenizer *t = p->tokr;
	if (end == NULL) return false;
	e->flags = 0;
	e->where = t->token->where;
	if (end <= t->token) {
		tokr_err(t, "Empty expression.");
		return false;
	}
	if (end - t->token == 1) {
		/* 1-token expression */
		switch (t->token->kind) {
		case TOKEN_NUM_LITERAL: {
			NumLiteral *num = &t->token->num;
			switch (num->kind) {
			case NUM_LITERAL_FLOAT:
				e->kind = EXPR_FLOAT_LITERAL;
				e->type.kind = TYPE_BUILTIN;
				e->type.builtin = BUILTIN_FLOAT;
				e->floatl = num->floatval;
				break;
			case NUM_LITERAL_INT:
				e->kind = EXPR_INT_LITERAL;
				e->flags |= EXPR_FLAG_FLEXIBLE;
				e->type.kind = TYPE_BUILTIN;
				e->type.builtin = BUILTIN_INT; /* TODO: if it's too big, use a u64 instead. */
				e->intl = num->intval;
				break;
			}
		} break;
		case TOKEN_IDENT:
			e->kind = EXPR_IDENT;
			e->ident = t->token->ident;
			break;
		case TOKEN_STR_LITERAL:
			e->kind = EXPR_STR_LITERAL;
			e->strl = t->token->str;
	    	break;
		default:
			tokr_err(t, "Unrecognized expression.");
			return false;
		}
		t->token = end;
		return true;
	}
			
	if (token_is_kw(t->token, KW_FN)) {
		/* this is a function */
		e->kind = EXPR_FN;
		if (!parse_fn_expr(p, &e->fn))
			return false;
			
		if (t->token != end) {
			tokr_err(t, "Direct function calling in an expression is not supported yet.\nYou can wrap the function in parentheses.");
			/* TODO */
			return false;
		}
		return true;
	}
	
	/* Find the lowest-precedence operator not in parentheses/braces */
	int paren_level = 0;
	int brace_level = 0;
	int lowest_precedence = NOT_AN_OP;
	/* e.g. (5+3) */
	bool entirely_within_parentheses = token_is_kw(t->token, KW_LPAREN);
	Token *lowest_precedence_op;
	for (Token *token = t->token; token < end; token++) {
		if (token->kind == TOKEN_KW) {
			switch (token->kw) {
			case KW_LPAREN:
				paren_level++;
				break;
			case KW_RPAREN:
				paren_level--;
				if (paren_level == 0 && token != end - 1)
					entirely_within_parentheses = false;
				if (paren_level < 0) {
					t->token = token;
					tokr_err(t, "Excessive closing parenthesis.");
					t->token = end + 1;
					return false;
				}
				break;
			case KW_LBRACE:
				brace_level++;
				break;
			case KW_RBRACE:
				brace_level--;
				if (brace_level < 0) {
					t->token = token;
					tokr_err(t, "Excessive closing brace.");
					return false;
				}
				break;
			default: { /* OPTIM: use individual cases for each op */
				if (paren_level == 0 && brace_level == 0) {
					int precedence = op_precedence(token->kw);
					if (precedence == NOT_AN_OP) break; /* nvm it's not an operator */
					if (lowest_precedence == NOT_AN_OP || precedence <= lowest_precedence) {
						lowest_precedence = precedence;
						lowest_precedence_op = token;
					}
				}
			} break;
			}
		}
	}

	/* TODO: These errors are bad for functions, since they can be very long,
	   and this will only point to the end. 
	*/
	if (paren_level > 0) {
		tokr_err(t, "Too many opening parentheses.");
		return false;
	}
	if (brace_level > 0) {
		tokr_err(t, "Too many opening braces.");
		return false;
	}
	
	if (entirely_within_parentheses) {
		t->token++;	/* move past opening ( */
		Token *new_end = end - 1; /* parse to ending ) */
		if (!parse_expr(p, e, new_end))
			return false;
		t->token++;	/* move past closing ) */
		return true;
	}
	
	if (lowest_precedence == NOT_AN_OP) {
		/* function calls, array accesses, etc. */
		
		/* try a function call */
		Token *token = t->token;
		/* 
		   can't call at start, e.g. in (fn() {})(), it is not the empty function ""
		   being called with fn() {} as an argument
		 */
		if (token_is_kw(t->token, KW_LPAREN)) {
			paren_level++;
			token++;
		}
		for (; token < end; token++) {
			if (token->kind == TOKEN_KW) {
				if (token->kw == KW_LPAREN) {
					if (paren_level == 0)
						break; /* this left parenthesis opens the function call */
					paren_level++;
				}
				if (token->kw == KW_RPAREN) {
					paren_level--;
				}
			}
		}
		if (token != t->token && token != end) {
			/* it's a function call! */
			e->kind = EXPR_CALL;
			e->call.fn = parser_new_expr(p);
			if (!parse_expr(p, e->call.fn, token)) { /* parse up to ( as function */
				return false;
			}
			arr_create(&e->call.args, sizeof(Expression));
			t->token = token + 1; /* move past ( */
			if (!token_is_kw(t->token, KW_RPAREN)) {
				/* non-empty arg list */
				while (1) {
					if (t->token->kind == TOKEN_EOF) {
						tokr_err(t, "Expected argument list to continue.");
						return false;
					}
					Expression *arg = arr_add(&e->call.args);
					if (!parse_expr(p, arg, expr_find_end(p, EXPR_END_RPAREN_OR_COMMA))) {
						return false;
					}
					if (token_is_kw(t->token, KW_RPAREN))
						break;
					t->token++;	/* move past , */
				}
			}
			t->token++;	/* move past ) */
			return true;
		}
		/* array accesses, etc. */
		tokr_err(t, "Not implemented yet.");
		return false;
	}
	
	/* This is a unary op not a binary one. */
	while (lowest_precedence_op != t->token
		   && lowest_precedence_op[-1].kind == TOKEN_KW
		   && op_precedence(lowest_precedence_op[-1].kw) != NOT_AN_OP) {
		lowest_precedence_op--;
	}

	/* Unary */
	if (lowest_precedence_op == t->token) {
		UnaryOp op;
		bool is_unary;
		switch (lowest_precedence_op->kw) {
		case KW_PLUS:
			/* unary + is ignored entirely */
			t->token++;
			/* re-parse this expression without + */
			return parse_expr(p, e, end);
		case KW_MINUS:
			is_unary = true;
			op = UNARY_MINUS;
			break;
		default:
			is_unary = false;
			break;
		}
		if (!is_unary) {
			tokr_err(t, "%s is not a unary operator.", keywords[lowest_precedence_op->kw]);
			return false;
		}
		e->unary.op = op;
		e->kind = EXPR_UNARY_OP;
		t->token++;
		Expression *of = parser_new_expr(p);
		e->unary.of = of;
		return parse_expr(p, of, end);
	}
	
	
	BinaryOp op; 
	switch (lowest_precedence_op->kw) {
	case KW_PLUS:
		op = BINARY_PLUS;
		break;
	case KW_MINUS:
		op = BINARY_MINUS;
		break;
	default: assert(0); break;
	}
	e->binary.op = op;
	e->kind = EXPR_BINARY_OP;

	Expression *lhs = parser_new_expr(p);
	e->binary.lhs = lhs;
	if (!parse_expr(p, lhs, lowest_precedence_op)) {
		return false;
	}
	
	Expression *rhs = parser_new_expr(p);
	t->token = lowest_precedence_op + 1;
	e->binary.rhs = rhs;
	if (!parse_expr(p, rhs, end)) {
		return false;
	}
	
	return true;
}

static bool decl_parse(Declaration *d, Parser *p) {
	Tokenizer *t = p->tokr;
	/* OPTIM: Maybe don't use a dynamic array or use parser allocator. */
	d->where = t->token->where;
	arr_create(&d->idents, sizeof(Identifier));
	
	d->flags = 0;
	
	while (1) {
		Identifier *ident = arr_add(&d->idents);
		if (t->token->kind != TOKEN_IDENT) {
			tokr_err(t, "Cannot declare non-identifier.");
			return false;
		}
		*ident = t->token->ident;
		/* 
		   only keep track of file scoped declarations---
		   block enter/exit code will handle the rest
		*/
		if (p->block == NULL) {
			if ((*ident)->decls.len) {
				/* this was already declared! */
				IdentDecl *prev = (*ident)->decls.data;
				tokr_err(t, "Re-declaration of identifier in global scope.");
				info_print(prev->decl->where, "Previous declaration was here.");
				return false;
			}
			assert(!(*ident)->decls.item_sz);
			arr_create(&(*ident)->decls, sizeof(IdentDecl));
			IdentDecl *ident_decl = arr_add(&(*ident)->decls);
			ident_decl->decl = d;
			ident_decl->scope = NULL;
		}
		t->token++;
		if (token_is_kw(t->token, KW_COMMA)) {
			t->token++;
			continue;
		}
		if (token_is_kw(t->token, KW_COLON)) {
			t->token++;
			break;
		}
		if (token_is_kw(t->token, KW_AT)) {
			d->flags |= DECL_FLAG_CONST;
			t->token++;
			break;
		}
		tokr_err(t, "Expected ',' to continue listing variables or ':' / '@' to indicate type.");
		return false;
	}
	
	
	if (token_is_kw(t->token, KW_SEMICOLON)) {
		/* e.g. foo :; */
		tokr_err(t, "Cannot infer type without expression.");
		return false;
	}
	
	if (token_is_kw(t->token, KW_EQ)) {
		/* := / @= */
		d->flags |= DECL_FLAG_INFER_TYPE;
	} else {
		if (!parse_type(p, &d->type)) {
			return false;
		}
	}

	/* OPTIM: switch */
    if (token_is_kw(t->token, KW_EQ)) {
		t->token++;
		if (!parse_expr(p, &d->expr, expr_find_end(p, EXPR_END_SEMICOLON)))
			return false;
		d->flags |= DECL_FLAG_HAS_EXPR;
		if (token_is_kw(t->token, KW_SEMICOLON)) {
			t->token++;
			return true;
		}
		tokr_err(t, "Expected ';' at end of expression"); /* should never happen in theory right now */
		return false;
	} else if (token_is_kw(t->token, KW_SEMICOLON)) {
		t->token++;
		return true;
	} else {
		tokr_err(t, "Expected ';' or '=' at end of delaration.");
		return false;
	}
}

static bool parse_stmt(Parser *p, Statement *s) {
	Tokenizer *t = p->tokr;
	if (t->token->kind == TOKEN_EOF)
		tokr_err(t, "Expected statement.");
	s->where = t->token->where;
	/* 
	   NOTE: This may cause problems in the future! Other statements might have comma
	   as the second token.
	*/
	if (token_is_kw(t->token + 1, KW_COLON) || token_is_kw(t->token + 1, KW_COMMA)
		|| token_is_kw(t->token + 1, KW_AT)) {
		s->kind = STMT_DECL;
		if (!decl_parse(&s->decl, p)) {
			/* move to next statement */
			/* TODO: This might cause unhelpful errors if the first semicolon is inside a block, etc. */
			while (!token_is_kw(t->token, KW_SEMICOLON)) {
				if (t->token->kind == TOKEN_EOF) {
					/* don't bother continuing */
					tokr_err(t, "No semicolon found at end of declaration.");
					return false;
				}
				t->token++;
			}
			t->token++;	/* move past ; */
			return false;
		}
		return true;
	} else {
		s->kind = STMT_EXPR;
		Token *end = expr_find_end(p, EXPR_END_SEMICOLON);
		if (!end) {
			tokr_err(t, "No semicolon found at end of statement.");
			while (t->token->kind != TOKEN_EOF) t->token++; /* move to end of file */
			return false;
		}
		if (!parse_expr(p, &s->expr, end)) {
			t->token = end + 1;
			return false;
		}
		if (!token_is_kw(t->token, KW_SEMICOLON)) {
			tokr_err(t, "Expected ';' at end of statement.");
			t->token = end + 1;
			return false;
		}
		t->token++;	/* move past ; */
		return true;
	}
}

static void parser_from_tokenizer(Parser *p, Tokenizer *t) {
	p->tokr = t;
	p->block = NULL;
	block_arr_create(&p->exprs, 10, sizeof(Expression)); /* block size = 1024 */
}

static bool parse_file(Parser *p, ParsedFile *f) {
	Tokenizer *t = p->tokr;
	arr_create(&f->stmts, sizeof(Statement));
	bool ret = true;
	while (t->token->kind != TOKEN_EOF) {
		Statement *stmt = arr_add(&f->stmts);
		if (!parse_stmt(p, stmt))
			ret = false;
	}
	return ret;
}

#define PARSE_PRINT_LOCATION(l) //fprintf(out, "[l%lu]", (unsigned long)(l).line);


static void fprint_type(FILE *out, Type *t) {
	PARSE_PRINT_LOCATION(t->where);
	switch (t->kind) {
	case TYPE_BUILTIN:
		fprintf(out, "%s", keywords[builtin_type_to_kw(t->builtin)]);
		break;
	case TYPE_VOID:
		fprintf(out, "void");
		break;
	case TYPE_FN: {
		Type *types = t->fn.types.data;
		fprintf(out, "fn (");
		for (size_t i = 1; i < t->fn.types.len; i++){ 
			fprint_type(out, &types[i]);
			fprintf(out, ",");
		}
		fprintf(out, ") ");
		fprint_type(out, &types[0]);
	} break;
	}
}

static void fprint_param(FILE *out, Param *p) {
	fprint_ident(out, p->name);
	fprintf(out, ": ");
	fprint_type(out, &p->type);
}

static void fprint_stmt(FILE *out, Statement *s);

static void fprint_block(FILE *out, Block *b) {
	fprintf(out, "{\n");
	arr_foreach(&b->stmts, Statement, stmt) {
		fprint_stmt(out, stmt);
	}
	fprintf(out, "}");

}

static void fprint_fn_expr(FILE *out, FnExpr *f) {
	fprintf(out, "fn (");
	arr_foreach(&f->params, Param, param) {
		if (param != f->params.data)
			fprintf(out, ", ");
		fprint_param(out, param);
	}
	fprintf(out, ") ");
	fprint_type(out, &f->ret_type);
	fprintf(out, " ");
	fprint_block(out, &f->body);
}

static void fprint_expr(FILE *out, Expression *e) {
	PARSE_PRINT_LOCATION(e->where);
	switch (e->kind) {
	case EXPR_INT_LITERAL:
		fprintf(out, "%lld", (long long)e->intl);
		break;
	case EXPR_FLOAT_LITERAL:
		fprintf(out, "%f", (double)e->floatl);
		break;
	case EXPR_STR_LITERAL:
		fprintf(out, "\"%s\"", e->strl.str);
		break;
	case EXPR_IDENT:
		fprint_ident(out, e->ident);
		break;
	case EXPR_BINARY_OP:
		switch (e->binary.op) {
		case BINARY_PLUS:
			fprintf(out, "add");
			break;
		case BINARY_MINUS:
			fprintf(out, "subtract");
			break;
		}
		fprintf(out, "(");
		fprint_expr(out, e->binary.lhs);
		fprintf(out, ",");
		fprint_expr(out, e->binary.rhs);
		fprintf(out, ")");
		break;
	case EXPR_UNARY_OP:
		switch (e->unary.op) {
		case UNARY_MINUS:
			fprintf(out, "negate");
			break;
		}
		fprintf(out, "(");
		fprint_expr(out, e->unary.of);
		fprintf(out, ")");
		break;
	case EXPR_FN:
		fprint_fn_expr(out, &e->fn);
		break;
	case EXPR_CALL:
		fprint_expr(out, e->call.fn);
		fprintf(out, "(");
		arr_foreach(&e->call.args, Expression, arg) {
			if (arg != e->call.args.data) fprintf(out, ", ");
			fprint_expr(out, arg);
		}
		fprintf(out, ")");
		break;
	}
}


static void fprint_decl(FILE *out, Declaration *d) {
	PARSE_PRINT_LOCATION(d->where);
	arr_foreach(&d->idents, Identifier, ident) {
		if (ident != d->idents.data) fprintf(out, ", ");
		fprint_ident(out, *ident);
	}
	if (d->flags & DECL_FLAG_CONST) {
		fprintf(out, "[const]");
	}
	fprintf(out, ":");
	if (!(d->flags & DECL_FLAG_INFER_TYPE)) {
		fprint_type(out, &d->type);
	}
	if (d->flags & DECL_FLAG_HAS_EXPR) {
		fprintf(out, "=");
		fprint_expr(out, &d->expr);
	}
}

static void fprint_stmt(FILE *out, Statement *s) {
	PARSE_PRINT_LOCATION(s->where);
	switch (s->kind) {
	case STMT_DECL:
		fprint_decl(out, &s->decl);
		fprintf(out, ";\n");
		break;
	case STMT_EXPR:
		fprint_expr(out, &s->expr);
		fprintf(out, ";\n");
		break;
	}
}

static void fprint_parsed_file(FILE *out, ParsedFile *f) {
	arr_foreach(&f->stmts, Statement, stmt) {
		fprint_stmt(out, stmt);
	}
}

/* TODO: Freeing parser (remember to free args) */
