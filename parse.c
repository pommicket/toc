/* 
   TODO: 
   all of these functions should leave the tokenizer at a "reasonable" place 
   for parsing to continue.
*/

typedef enum {
			  TYPE_VOID,
			  TYPE_BUILTIN
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
			  BUILTIN_F32,
			  BUILTIN_F64,
			  BUILTIN_TYPE_COUNT
} BuiltinType;


typedef struct {
	Location where;
	TypeKind kind;
	union {
	    BuiltinType builtin;
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

#define EXPR_FLAG_FLEXIBLE 0x01

typedef struct Expression {
	Location where;
	ExprKind kind;
	Type type;
	uint16_t flags;
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

/* OPTIM: Instead of using dynamic arrays, do two passes. */
typedef struct Declaration {
	Location where;
	Array idents;
	Type type;
	Expression expr;
	uint16_t flags;
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
	case KW_F32: return BUILTIN_F32;
	case KW_F64: return BUILTIN_F64;
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
	case BUILTIN_F32: return KW_F32;
	case BUILTIN_F64: return KW_F64;
	case BUILTIN_TYPE_COUNT: break;
	}
	assert(0);
	return KW_COUNT;
}

static bool type_parse(Type *type, Parser *p) {
	Tokenizer *t = p->tokr;
	type->where = t->token->where;
	switch (t->token->kind) {
	case TOKEN_KW:
		type->kind = TYPE_BUILTIN;
		type->builtin = kw_to_builtin_type(t->token->kw);
		if (type->builtin == BUILTIN_TYPE_COUNT) {
			tokr_err(t, "Expected type.");
			return false;
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

static bool param_parse(Param *p, Parser *parser) {
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
	if (!type_parse(&p->type, parser))
		return false;
	return true;
}

static bool stmt_parse(Statement *s, Parser *p);

static bool block_parse(Block *b, Parser *p) {
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
			if (!stmt_parse(stmt, p)) {
				ret = false;
				continue;
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

static bool fn_expr_parse(FnExpr *f, Parser *p) {
	Tokenizer *t = p->tokr;
	/* only called when token is fn */
	assert(token_is_kw(t->token, KW_FN));
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
			if (!param_parse(param, p))
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
		if (!type_parse(&f->ret_type, p)) {
			return false;
		}
	}
	return block_parse(&f->body, p);
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

static bool expr_parse(Expression *e, Parser *p, Token *end) {
	Tokenizer *t = p->tokr;
	if (end == NULL) return false;
	e->flags = 0;
	e->where = t->token->where;
	if (end <= t->token) {
		tokr_err(t, "Empty expression.");
		t->token = end + 1;
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
			t->token = end + 1;
			return false;
		}
		t->token = end;
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
					t->token = end + 1;
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
		t->token = end + 1;
		return false;
	}
	if (brace_level > 0) {
		tokr_err(t, "Too many opening braces.");
		t->token = end + 1;
		return false;
	}
	
	if (entirely_within_parentheses) {
		t->token++;	/* move past opening ( */
		Token *new_end = end - 1; /* parse to ending ) */
		if (!expr_parse(e, p, new_end)) {
			t->token = end + 1;
			return false;
		}
		t->token++;	/* move past closing ) */
		return true;
	}
	
	if (lowest_precedence == NOT_AN_OP) {
		/* functions, function calls, array accesses, etc. */
		if (token_is_kw(t->token, KW_FN)) {
			/* this is a function */
			e->kind = EXPR_FN;
			if (!fn_expr_parse(&e->fn, p)) {
				t->token = end + 1; /* move token past end for further parsing */
				return false;
			}
			if (t->token != end) {
				tokr_err(t, "Direct function calling in an expression is not supported yet.\nYou can wrap the function in parentheses.");
				/* TODO */
				t->token = end + 1;
				return false;
			}
			return true;
		}
		
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
			if (!expr_parse(e->call.fn, p, token)) { /* parse up to ( as function */
				t->token = end + 1;
				return false;
			}
			arr_create(&e->call.args, sizeof(Expression));
			t->token = token + 1; /* move past ( */
			if (!token_is_kw(t->token, KW_RPAREN)) {
				/* non-empty arg list */
				while (1) {
					if (t->token->kind == TOKEN_EOF) {
						tokr_err(t, "Expected argument list to continue.");
						t->token = end + 1;
						return false;
					}
					Expression *arg = arr_add(&e->call.args);
					if (!expr_parse(arg, p, expr_find_end(p, EXPR_END_RPAREN_OR_COMMA))) {
						t->token = end + 1;
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
		t->token = end + 1;
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
			return expr_parse(e, p, end);
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
			t->token = end + 1;
			return false;
		}
		e->unary.op = op;
		e->kind = EXPR_UNARY_OP;
		t->token++;
		Expression *of = parser_new_expr(p);
		e->unary.of = of;
		return expr_parse(of, p, end);
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
	if (!expr_parse(lhs, p, lowest_precedence_op)) {
		t->token = end + 1;
		return false;
	}
	
	Expression *rhs = parser_new_expr(p);
	t->token = lowest_precedence_op + 1;
	e->binary.rhs = rhs;
	if (!expr_parse(rhs, p, end)) {
		t->token = end + 1;
		return false;
	}
	
	return true;
}

static bool decl_parse(Declaration *d, Parser *p) {
	Tokenizer *t = p->tokr;
	/* OPTIM: Maybe don't use a dynamic array or use parser allocator. */
	d->where = t->token->where;
	arr_create(&d->idents, sizeof(Identifier));
	
	while (1) {
		Identifier *ident = arr_add(&d->idents);
		if (t->token->kind != TOKEN_IDENT) {
			tokr_err(t, "Cannot declare non-identifier.");
			return false;
		}
		*ident = t->token->ident;
		/* 
		   only keep track of file scoped declarations---
		   blocks.c will handle the rest
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
		tokr_err(t, "Expected ',' to continue listing variables or ':' to indicate type.");
		return false;
	}
	
	d->flags = 0;
	
	

	if (!token_is_kw(t->token, KW_MINUS)
		&& !token_is_kw(t->token, KW_EQ)
		&& !token_is_kw(t->token, KW_SEMICOLON)) {
		if (!type_parse(&d->type, p))
			return false;
	} else {
		d->flags |= DECL_FLAG_INFER_TYPE;
	}
		
	if (token_is_kw(t->token, KW_SEMICOLON)) {
		if (d->flags & DECL_FLAG_INFER_TYPE) {
			tokr_err(t, "Cannot infer type without expression.");
			return false;
		}
	} else if (token_is_kw(t->token, KW_EQ)) {
		t->token++;
		if (!expr_parse(&d->expr, p, expr_find_end(p, EXPR_END_SEMICOLON)))
			return false;
		d->flags |= DECL_FLAG_HAS_EXPR;
	} else if (token_is_kw(t->token, KW_MINUS)) {
		t->token++;
		if (!expr_parse(&d->expr, p, expr_find_end(p, EXPR_END_SEMICOLON)))
			return false;
		d->flags |= DECL_FLAG_HAS_EXPR | DECL_FLAG_CONST;
	} else {
		tokr_err(t, "Expected ';', '=', or '-' in delaration.");
		return false;
	}
	if (token_is_kw(t->token, KW_SEMICOLON)) {
		t->token++;
		return true;
	}
	tokr_err(t, "Expected ';' at end of expression"); /* should never happen in theory right now */
	return false;
}

static bool stmt_parse(Statement *s, Parser *p) {
	Tokenizer *t = p->tokr;
	if (t->token->kind == TOKEN_EOF)
		tokr_err(t, "Expected statement.");
	s->where = t->token->where;
	/* 
	   NOTE: This may cause problems in the future! Other statements might have comma
	   as the second token.
	*/
	if (token_is_kw(t->token + 1, KW_COLON) || token_is_kw(t->token + 1, KW_COMMA)) {
		s->kind = STMT_DECL;
		if (!decl_parse(&s->decl, p)) {
			/* move to next statement */
			/* TODO: This might cause unhelpful errors if the first semicolon is inside a block, etc. */
			while (!token_is_kw(t->token, KW_SEMICOLON)) {
				if (t->token->kind == TOKEN_EOF) {
					/* don't bother continuing */
					return false;
				}
				t->token++;
			}
			return false;
		}
		return true;
	} else {
		s->kind = STMT_EXPR;
		Token *end = expr_find_end(p, EXPR_END_SEMICOLON);
		if (!expr_parse(&s->expr, p, end)) {
			t->token = end;
			return false;
		}
		if (!token_is_kw(t->token, KW_SEMICOLON)) {
			tokr_err(t, "Expected ';' at end of statement.");
			t->token = end;
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

static bool file_parse(ParsedFile *f, Parser *p) {
	Tokenizer *t = p->tokr;
	arr_create(&f->stmts, sizeof(Statement));
	bool ret = true;
	while (t->token->kind != TOKEN_EOF) {
		Statement *stmt = arr_add(&f->stmts);
		if (!stmt_parse(stmt, p))
			ret = false;
	}
	return ret;
}

#define PARSE_PRINT_LOCATION(l) //fprintf(out, "[l%lu]", (unsigned long)(l).line);


static void type_fprint(FILE *out, Type *t) {
	PARSE_PRINT_LOCATION(t->where);
	switch (t->kind) {
	case TYPE_BUILTIN:
		fprintf(out, "%s", keywords[builtin_type_to_kw(t->builtin)]);
		break;
	case TYPE_VOID:
		fprintf(out, "void");
		break;
	}
}

static void param_fprint(FILE *out, Param *p) {
	ident_fprint(out, p->name);
	fprintf(out, ": ");
	type_fprint(out, &p->type);
}

static void stmt_fprint(FILE *out, Statement *s);

static void block_fprint(FILE *out, Block *b) {
	fprintf(out, "{\n");
	arr_foreach(&b->stmts, Statement, stmt) {
		stmt_fprint(out, stmt);
	}
	fprintf(out, "}");

}

static void fn_expr_fprint(FILE *out, FnExpr *f) {
	fprintf(out, "fn (");
	arr_foreach(&f->params, Param, param) {
		if (param != f->params.data)
			fprintf(out, ", ");
		param_fprint(out, param);
	}
	fprintf(out, ") ");
	type_fprint(out, &f->ret_type);
	fprintf(out, " ");
	block_fprint(out, &f->body);
}

static void expr_fprint(FILE *out, Expression *e) {
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
		ident_fprint(out, e->ident);
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
		expr_fprint(out, e->binary.lhs);
		fprintf(out, ",");
		expr_fprint(out, e->binary.rhs);
		fprintf(out, ")");
		break;
	case EXPR_UNARY_OP:
		switch (e->unary.op) {
		case UNARY_MINUS:
			fprintf(out, "negate");
			break;
		}
		fprintf(out, "(");
		expr_fprint(out, e->unary.of);
		fprintf(out, ")");
		break;
	case EXPR_FN:
		fn_expr_fprint(out, &e->fn);
		break;
	case EXPR_CALL:
		expr_fprint(out, e->call.fn);
		fprintf(out, "(");
		arr_foreach(&e->call.args, Expression, arg) {
			if (arg != e->call.args.data) fprintf(out, ", ");
			expr_fprint(out, arg);
		}
		fprintf(out, ")");
		break;
	}
}


static void decl_fprint(FILE *out, Declaration *d) {
	PARSE_PRINT_LOCATION(d->where);
	arr_foreach(&d->idents, Identifier, ident) {
		if (ident != d->idents.data) fprintf(out, ", ");
		ident_fprint(out, *ident);
	}
	if (d->flags & DECL_FLAG_CONST) {
		fprintf(out, "[const]");
	}
	fprintf(out, ":");
	if (!(d->flags & DECL_FLAG_INFER_TYPE)) {
		type_fprint(out, &d->type);
	}
	if (d->flags & DECL_FLAG_HAS_EXPR) {
		fprintf(out, "=");
		expr_fprint(out, &d->expr);
	}
}

static void stmt_fprint(FILE *out, Statement *s) {
	PARSE_PRINT_LOCATION(s->where);
	switch (s->kind) {
	case STMT_DECL:
		decl_fprint(out, &s->decl);
		fprintf(out, ";\n");
		break;
	case STMT_EXPR:
		expr_fprint(out, &s->expr);
		fprintf(out, ";\n");
		break;
	}
}

static void parsed_file_fprint(FILE *out, ParsedFile *f) {
	arr_foreach(&f->stmts, Statement, stmt) {
		stmt_fprint(out, stmt);
	}
}

/* TODO: Freeing parser (remember to free args) */
