/* TODO: stmt_parse -> parse_stmt, etc. */
typedef enum {
			  TYPE_VOID,
			  TYPE_UNKNOWN,
			  TYPE_BUILTIN,
			  TYPE_FN,
			  TYPE_TUPLE,
			  TYPE_ARR /* e.g. [5]int */
} TypeKind;

typedef enum {
			  BUILTIN_I8,
			  BUILTIN_I16,
			  BUILTIN_I32,
			  BUILTIN_I64,
			  BUILTIN_U8,
			  BUILTIN_U16,
			  BUILTIN_U32,
			  BUILTIN_U64,
			  BUILTIN_F32,
			  BUILTIN_F64,
			  BUILTIN_TYPE_COUNT
} BuiltinType;

#define TYPE_FLAG_FLEXIBLE 0x01
#define TYPE_FLAG_RESOLVED 0x02

typedef struct Type {
	Location where;
	TypeKind kind;
	unsigned short flags;
	union {
	    BuiltinType builtin;
		struct {
			Array types; /* [0] = ret_type, [1..] = param_types */
		} fn;
		Array tuple;
		struct {
			struct Type *of;
			union {
				UInteger n; /* this is NOT set by parse_type; it will be handled by types.c */
				struct Expression *n_expr;
			};
		} arr;
	};
} Type;

typedef struct Block {
	Array stmts;
    struct Expression *ret_expr; /* the return expression of this block, e.g. {foo(); 3} => 3  NULL for no expression. */
} Block;

typedef enum {
			  EXPR_LITERAL_FLOAT,
			  EXPR_LITERAL_INT,
			  EXPR_LITERAL_STR,
			  EXPR_IDENT, /* variable or constant */
			  EXPR_BINARY_OP,
			  EXPR_UNARY_OP,
			  EXPR_FN,
			  EXPR_CALL,
			  EXPR_DIRECT
} ExprKind;

typedef enum {
			  UNARY_MINUS
} UnaryOp;

typedef enum {
			  BINARY_SET, /* e.g. x = y */
			  BINARY_PLUS,
			  BINARY_MINUS,
			  BINARY_COMMA,
			  BINARY_AT_INDEX /* e.g. x[i] */
} BinaryOp;

typedef struct {
	Directive which;
	Array args;	/* of Expression */
} DirectExpr;

typedef struct {
	struct Expression *fn;
	Array args;	/* of Expression */
	unsigned long out_var; /* which out variable is used for this call (used by cgen) */
} CallExpr;

#define EXPR_FLAG_FOUND_TYPE 0x01

typedef struct Expression {
	Location where;
	ExprKind kind;
	uint16_t flags;
	Type type;
	union {
		Floating floatl;
		UInteger intl;
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
		CallExpr call;
	    DirectExpr direct;
		Identifier ident;
		struct FnExpr *fn;
	};
} Expression;

#define DECL_FLAG_ANNOTATES_TYPE 0x01
#define DECL_FLAG_CONST 0x02
#define DECL_FLAG_HAS_EXPR 0x04
#define DECL_FLAG_FOUND_TYPE 0x08

/* OPTIM: Instead of using dynamic arrays, do two passes. */
typedef struct Declaration {
	Location where;
	Array idents;
	Type type;
	unsigned short flags;
	Expression expr;
} Declaration;

typedef struct FnExpr {
	Declaration params;	/* declaration of the parameters to this function */
	Type ret_type;
	Block body;
	Identifier name; /* NULL if the function is anonymous (set to NULL by parse.c, set to actual value by types_cgen.c) */
	unsigned long id; /* this is used to keep track of local vs global/other local functions (there might be multiple functions called "foo") */
} FnExpr; /* an expression such as fn(x: int) int {return 2 * x;} */

typedef enum {
			  STMT_DECL,
			  STMT_EXPR
} StatementKind;

#define STMT_FLAG_VOIDED_EXPR 0x01 /* the "4;" in fn () { 4; } is a voided expression, but the "4" in fn () int { 4 } is not */
typedef struct {
	Location where;
	StatementKind kind;
	unsigned short flags;
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

typedef enum {
			  DECL_END_SEMICOLON,
			  DECL_END_RPAREN
} DeclEndType;

static bool parse_expr(Parser *p, Expression *e, Token *end);
static bool parse_decl(Parser *p, Declaration *d, DeclEndType ends_with);

static const char *binary_op_to_str(BinaryOp b) {
	switch (b) {
	case BINARY_PLUS: return "+";
	case BINARY_MINUS: return "-";
	case BINARY_SET: return "=";
	case BINARY_COMMA: return ",";
	case BINARY_AT_INDEX: return "[]";
	}
	assert(0);
	return "";
}

static bool type_builtin_is_integer(BuiltinType b) {
	switch (b) {
	case BUILTIN_I8:
	case BUILTIN_I16:
	case BUILTIN_I32:
	case BUILTIN_I64:
	case BUILTIN_U8:
	case BUILTIN_U16:
	case BUILTIN_U32:
	case BUILTIN_U64:
		return true;
	default: return false;
	}
}

static bool type_builtin_is_floating(BuiltinType b) {
	switch (b) {
	case BUILTIN_F32:
	case BUILTIN_F64:
		return true;
	default: return false;
	}
}

static bool type_builtin_is_numerical(BuiltinType b) {
	return type_builtin_is_integer(b) || type_builtin_is_floating(b);
}


/* returns BUILTIN_TYPE_COUNT on failure */
static BuiltinType kw_to_builtin_type(Keyword kw) {
	switch (kw) {
	case KW_I8: return BUILTIN_I8;
	case KW_I16: return BUILTIN_I16;
	case KW_I32: return BUILTIN_I32;
	case KW_I64: return BUILTIN_I64;
	case KW_INT: return BUILTIN_I64;
	case KW_U8: return BUILTIN_U8;
	case KW_U16: return BUILTIN_U16;
	case KW_U32: return BUILTIN_U32;
	case KW_U64: return BUILTIN_U64;
	case KW_F32: return BUILTIN_F32;
	case KW_F64: return BUILTIN_F64;
	default: return BUILTIN_TYPE_COUNT;
	}
}

static Keyword builtin_type_to_kw(BuiltinType t) {
	switch (t) {
	case BUILTIN_I8: return KW_I8;
	case BUILTIN_I16: return KW_I16;
	case BUILTIN_I32: return KW_I32;
	case BUILTIN_I64: return KW_I64;
	case BUILTIN_U8: return KW_U8;
	case BUILTIN_U16: return KW_U16;
	case BUILTIN_U32: return KW_U32;
	case BUILTIN_U64: return KW_U64;
	case BUILTIN_F32: return KW_F32;
	case BUILTIN_F64: return KW_F64;
	case BUILTIN_TYPE_COUNT: break;
	}
	assert(0);
	return KW_COUNT;
}

/* returns the number of characters written, not including the null character */
static size_t type_to_str_(Type *t, char *buffer, size_t bufsize) {
	switch (t->kind) {
	case TYPE_VOID:
		return str_copy(buffer, bufsize, "void");
	case TYPE_UNKNOWN:
		return str_copy(buffer, bufsize, "???");
	case TYPE_BUILTIN: {
		const char *s = keywords[builtin_type_to_kw(t->builtin)];
		return str_copy(buffer, bufsize, s);
	}
	case TYPE_FN: {
		/* number of chars written */
		size_t written = str_copy(buffer, bufsize, "fn (");
		Type *ret_type = t->fn.types.data;
		Type *param_types = ret_type + 1;
		size_t nparams = t->fn.types.len - 1;
		for (size_t i = 0; i < nparams; i++) {
			if (i > 0)
				written += str_copy(buffer + written, bufsize - written, ", ");
			written += type_to_str_(&param_types[i], buffer + written, bufsize - written);
		}
		written += str_copy(buffer + written, bufsize - written, ")");
		if (ret_type->kind != TYPE_VOID) {
			written += str_copy(buffer + written, bufsize - written, " ");
			written += type_to_str_(ret_type, buffer + written, bufsize - written);
		}
		return written;
	} break;
	case TYPE_ARR: {
		size_t written = str_copy(buffer, bufsize, "[");
		if (t->flags & TYPE_FLAG_RESOLVED) {
			snprintf(buffer + written, bufsize - written, UINTEGER_FMT, t->arr.n);
			written += strlen(buffer + written);
		} else {
			written += str_copy(buffer + written, bufsize - written, "N");
		}
		written += str_copy(buffer + written, bufsize - written, "]");
		written += type_to_str_(t->arr.of, buffer + written, bufsize - written);
		return written;
	} break;
	case TYPE_TUPLE: {
		size_t written = str_copy(buffer, bufsize, "(");
		arr_foreach(&t->tuple, Type, child) {
			if (child != t->tuple.data)
				written += str_copy(buffer + written, bufsize - written, ", ");
			written += type_to_str_(child, buffer + written, bufsize - written);
		}
		written += str_copy(buffer + written, bufsize - written, ")");
		return written;
	}
	}

	assert(0);
	return 0;
}

/* return value should be freed by caller */
static char *type_to_str(Type *t) {
	/* TODO allow types >255 chars */
	char *ret = err_malloc(256);
	type_to_str_(t, ret, 256);
	return ret;
}

/* 
   allocate a new expression.
*/
static Expression *parser_new_expr(Parser *p) {
	return block_arr_add(&p->exprs);
}

#define NOT_AN_OP -1
static int op_precedence(Keyword op) {
	switch (op) {
	case KW_EQ:
		return 0;
	case KW_COMMA:
		return 5;
	case KW_PLUS:
		return 10;
	case KW_MINUS:
		return 20;
	default:
		return NOT_AN_OP;
	}
}

/* TODO: check that we check which thing ends it everywhere */

#define EXPR_CAN_END_WITH_COMMA 0x01 /* a comma could end the expression */

static Token *expr_find_end(Parser *p, unsigned flags)  {
	Tokenizer *t = p->tokr;
	int paren_level = 0;
	int brace_level = 0;
	int square_level = 0;
	Token *token = t->token;
	while (1) {
		if (token->kind == TOKEN_KW) {
			switch (token->kw) {
			case KW_COMMA:
				if ((flags & EXPR_CAN_END_WITH_COMMA) &&
					paren_level == 0 && brace_level == 0 && square_level == 0)
					return token;
				break;
			case KW_LPAREN:
				paren_level++;
				break;
			case KW_RPAREN:
				paren_level--;
				if (paren_level < 0)
					return token;
				break;
			case KW_LSQUARE:
				square_level++;
				break;
			case KW_RSQUARE:
			    square_level--;
				if (square_level < 0)
					return token;
				break;
			case KW_LBRACE:
				brace_level++;
				break;
			case KW_RBRACE:
				brace_level--;
				if (brace_level < 0)
					return token;
				break;
			case KW_SEMICOLON:
				if (brace_level == 0)
					return token;
				break;
			default: break;
			}
		}
		if (token->kind == TOKEN_EOF) {
			if (brace_level > 0) {
				tokr_err(t, "Opening brace { was never closed."); /* FEATURE: Find out where this is */
			} else if (paren_level > 0) {
				tokr_err(t, "Opening parenthesis ( was never closed.");
			} else if (square_level > 0) {
				tokr_err(t, "Opening square bracket [ was never closed.");
			} else {
				tokr_err(t, "Could not find end of expression.");
			}
			return NULL;
		}
		token++;
	}
}

static bool parse_type(Parser *p, Type *type) {
	Tokenizer *t = p->tokr;
	type->where = t->token->where;
	type->flags = 0;
	switch (t->token->kind) {
	case TOKEN_KW:
		type->kind = TYPE_BUILTIN;
		type->builtin = kw_to_builtin_type(t->token->kw);
		if (type->builtin != BUILTIN_TYPE_COUNT) {
			t->token++;
			break;
		}
		/* Not a builtin */
		switch (t->token->kw) {
		case KW_FN: {
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
			/* if there's a symbol that isn't [ or (, that can't be the start of a type */
			if (t->token->kind == TOKEN_KW
				&& t->token->kw <= KW_LAST_SYMBOL
				&& t->token->kw != KW_LSQUARE
				&& t->token->kw != KW_LPAREN) {
				ret_type->kind = TYPE_VOID;
				ret_type->flags = 0;
			} else {
				if (!parse_type(p, ret_type))
					return false;
			}
			break;
		}
		case KW_LSQUARE: {
			/* array type */
			Token *start = t->token;
			type->kind = TYPE_ARR;
			t->token++;	/* move past [ */
			Token *end = expr_find_end(p, 0);
			type->arr.n_expr = parser_new_expr(p);
			if (!parse_expr(p, type->arr.n_expr, end)) return false;
			t->token = end + 1;	/* go past ] */
			type->arr.of = err_malloc(sizeof *type->arr.of); /* OPTIM */
			if (!parse_type(p, type->arr.of)) return false;
			type->flags = 0;
			type->where = start->where;
			break;
		}
		case KW_LPAREN:
			/* tuple! */
			type->kind = TYPE_TUPLE;
			arr_create(&type->tuple, sizeof(Type));
			t->token++;	/* move past ( */
			while (1) {
				Type *child = arr_add(&type->tuple);
				parse_type(p, child);
				if (token_is_kw(t->token, KW_RPAREN)) { /* we're done with the tuple */
					t->token++;	/* move past ) */
					break;
				}
				if (token_is_kw(t->token, KW_COMMA)) {
					t->token++;	/* move past , */
					continue;
				} else {
					tokr_err(t, "Expected , to list next tuple type or ) to end tuple type.");
					return false;
				}
			}
			break;
		default:
			tokr_err(t, "Unrecognized type.");
			return false;
		}
		break;
	default:
		tokr_err(t, "Unrecognized type.");
		return false;
	}
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
	b->ret_expr = NULL; /* default to no return unless overwritten later */
	if (!token_is_kw(t->token, KW_RBRACE)) {
		/* non-empty function body */
		while (1) {
			Statement *stmt = arr_add(&b->stmts);
			bool success = parse_stmt(p, stmt);
			if (!success) {
				ret = false;
			}
			
			if (token_is_kw(t->token, KW_RBRACE)) {
				if (success && stmt->kind == STMT_EXPR) {
					if (!(stmt->flags & STMT_FLAG_VOIDED_EXPR)) {
						b->ret_expr = parser_new_expr(p);
						*b->ret_expr = stmt->expr;
						arr_remove_last(&b->stmts); /* only keep this expression in the return value */
					}
				}
				break;
			}
			
			if (success) {
				if (stmt->kind == STMT_EXPR && !(stmt->flags & STMT_FLAG_VOIDED_EXPR)) {
					/* in theory, this should never happen right now */
					err_print(stmt->where, "Non-voided expression is not the last statement in a block (you might want to add a ';' to the end of this statement).");
					return false;
				}
			}
			
			if (t->token->kind == TOKEN_EOF) {
				tokr_err(t, "Expected '}' to close function body.");
				return false;
			}
			
		}
	} else {
		b->ret_expr = NULL;
	}
	t->token++;	/* move past } */
	p->block = prev_block;
	return ret;
}

static bool parse_fn_expr(Parser *p, FnExpr *f) {
	Tokenizer *t = p->tokr;
	/* only called when token is fn */
	assert(token_is_kw(t->token, KW_FN));
	f->name = 0;
	t->token++;
	if (!token_is_kw(t->token, KW_LPAREN)) {
		tokr_err(t, "Expected '(' after 'fn'.");
		return false;
	}
	t->token++;
	parse_decl(p, &f->params, DECL_END_RPAREN);
	
	if (token_is_kw(t->token, KW_LBRACE)) {
		/* void function */
		f->ret_type.kind = TYPE_VOID;
		f->ret_type.flags = 0;
	} else {
		if (!parse_type(p, &f->ret_type)) {
			return false;
		}
	}
	return parse_block(p, &f->body);
}

/* parses, e.g. "(3, 5, foo)" */
static bool parse_args(Parser *p, Array *args) {
	Tokenizer *t = p->tokr;
	Token *start = t->token;
	assert(token_is_kw(start, KW_LPAREN));
	arr_create(args, sizeof(Expression));
	t->token++; /* move past ( */
	if (!token_is_kw(t->token, KW_RPAREN)) {
		/* non-empty arg list */
		while (1) {
			if (t->token->kind == TOKEN_EOF) {
				tokr_err(t, "Expected argument list to continue.");
				info_print(start->where, "This is where the argument list starts.");
				return false;
			}
			Expression *arg = arr_add(args);
			if (!parse_expr(p, arg, expr_find_end(p, EXPR_CAN_END_WITH_COMMA))) {
				return false;
			}
			if (token_is_kw(t->token, KW_RPAREN))
				break;
			assert(token_is_kw(t->token, KW_COMMA));
			t->token++;	/* move past , */
		}
	}
	t->token++;	/* move past ) */
	return true;
}

static bool parse_expr(Parser *p, Expression *e, Token *end) {
	Tokenizer *t = p->tokr;
	e->flags = 0;
	if (end == NULL) return false;
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
				e->kind = EXPR_LITERAL_FLOAT;
				e->floatl = num->floatval;
				break;
			case NUM_LITERAL_INT:
				e->kind = EXPR_LITERAL_INT;
				e->intl = num->intval;
				break;
			}
		} break;
		case TOKEN_IDENT:
			e->kind = EXPR_IDENT;
			e->ident = t->token->ident;
			break;
		case TOKEN_STR_LITERAL:
			e->kind = EXPR_LITERAL_STR;
			e->strl = t->token->str;
	    	break;
		default:
			tokr_err(t, "Unrecognized expression.");
			return false;
		}
		t->token = end;
		return true;
	}

	Token *start = t->token;
			
	if (token_is_kw(t->token, KW_FN)) {
		/* this is a function */
		e->kind = EXPR_FN;
		e->fn = err_malloc(sizeof *e->fn);
		if (!parse_fn_expr(p, e->fn))
			return false;
			
		if (t->token != end) {
			tokr_err(t, "Direct function calling in an expression is not supported yet.\nYou can wrap the function in parentheses.");
			/* TODO */
			return false;
		}
		return true;
	}
	
	/* Find the lowest-precedence operator not in parentheses/braces/square brackets */
	int paren_level = 0;
	int brace_level = 0;
	int square_level = 0;
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
					tokr_err(t, "Excessive closing ).");
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
					tokr_err(t, "Excessive closing }.");
					return false;
				}
				break;
			case KW_LSQUARE:
				square_level++;
				break;
			case KW_RSQUARE:
				square_level--;
				if (square_level < 0) {
					tokr_err(t, "Excessive closing ].");
					return false;
				}
				break;
			default: { /* OPTIM: use individual cases for each op */
				if (paren_level == 0 && brace_level == 0 && square_level == 0) {
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

	if (paren_level > 0) {
		t->token = start;
		tokr_err(t, "Too many opening parentheses (.");
		return false;
	}
	if (brace_level > 0) {
		t->token = start;
		tokr_err(t, "Too many opening braces {.");
		return false;
	}
	if (square_level > 0) {
		t->token = start;
		tokr_err(t, "Too many opening square brackets [.");
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
		
		/* try a function call or array access */
		Token *token = t->token;
		
		/* currently unnecessary: paren_level = square_level = 0; */
		/* 
		   can't call at start, e.g. in (fn() {})(), it is not the empty function ""
		   being called with fn() {} as an argument
		 */
		if (token_is_kw(t->token, KW_LPAREN)) {
			paren_level++;
			token++;
		}
		/* which opening bracket starts the call/array access */
		Token *opening_bracket = NULL;
		for (; token < end; token++) {
			if (token->kind == TOKEN_KW) {
				switch (token->kw) {
				case KW_LPAREN:
					if (square_level == 0 && paren_level == 0 && token != t->tokens.data
						&& token[-1].kind != TOKEN_DIRECT /* don't include directives */)
						opening_bracket = token; /* maybe this left parenthesis opens the function call */
					paren_level++;
					break;
				case KW_LSQUARE:
					if (square_level == 0 && paren_level == 0)
						opening_bracket = token; /* ^^ (array access) */
					square_level++;
					break;
				case KW_RPAREN:
					paren_level--;
					break;
				case KW_RSQUARE:
					square_level--;
					break;
				default: break;
				}
				
			} else if (token->kind == TOKEN_EOF) {
				if (paren_level > 0) {
					tokr_err(t, "Unmatched ( parenthesis.");
					return false;
				}
				if (square_level > 0) {
					tokr_err(t, "Unmatched [ square bracket.");
					return false;
				}
				break;
			}
		}
		if (opening_bracket) {
			switch (opening_bracket->kw) {
			case KW_LPAREN: {
				/* it's a function call! */
				e->kind = EXPR_CALL;
				e->call.fn = parser_new_expr(p);
				if (!parse_expr(p, e->call.fn, opening_bracket)) { /* parse up to ( as function */
					return false;
				}
				t->token = opening_bracket;
				return parse_args(p, &e->call.args);
			}
			case KW_LSQUARE: {
				/* it's an array access */
				e->kind = EXPR_BINARY_OP;
				e->binary.op = BINARY_AT_INDEX;
				e->binary.lhs = parser_new_expr(p);
				e->binary.rhs = parser_new_expr(p);
				/* parse array */
				if (!parse_expr(p, e->binary.lhs, opening_bracket)) return false;
				/* parse index */
				t->token = opening_bracket + 1;
				Token *index_end = expr_find_end(p, 0);
				if (!parse_expr(p, e->binary.rhs, index_end))
					return false;
				t->token++;	/* move past ] */
			    return true;
			}
			default:
				assert(0);
				return false;
			}
		}

		if (t->token->kind == TOKEN_DIRECT) {
			/* it's a directive */
			e->kind = EXPR_DIRECT;
			e->direct.which = t->token->direct;
			if (token_is_kw(&t->token[1], KW_LPAREN)) {
				/* has args (but maybe it's just "#foo()") */
				t->token++;	/* move to ( */
				return parse_args(p, &e->direct.args);
			} else {
				/* no args */
				arr_create(&e->direct.args, sizeof(Expression));
				t->token++;
				return true;
			}
		}
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
	case KW_EQ:
		op = BINARY_SET;
		break;
	case KW_COMMA:
		op = BINARY_COMMA;
		break;
	default: assert(0); return false;
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

/* 
parses
x : int, y : float; 
^^this^^
then recursively calls itself to parse the rest
NOTE: this function actually parses all types in the declaration, but it just
calls itself to do that.

*/
static inline bool ends_decl(Token *t, DeclEndType ends_with) {
	return (token_is_kw(t, KW_SEMICOLON) && ends_with == DECL_END_SEMICOLON)
		|| (token_is_kw(t, KW_RPAREN) && ends_with == DECL_END_RPAREN);
}

static bool parse_single_type_in_decl(Parser *p, Declaration *d, DeclEndType ends_with) {
	Tokenizer *t = p->tokr;
	/* OPTIM: Maybe don't use a dynamic array or use parser allocator. */
	size_t n_idents_with_this_type = 1;
	while (1) {
		Identifier *ident = arr_add(&d->idents);
		if (t->token->kind != TOKEN_IDENT) {
			tokr_err(t, "Cannot declare non-identifier.");
			return false;
		}
		*ident = t->token->ident;
		t->token++;
		if (token_is_kw(t->token, KW_COMMA)) {
			t->token++;
			n_idents_with_this_type++;
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
	
	
	if (token_is_kw(t->token, KW_SEMICOLON) || token_is_kw(t->token, KW_RPAREN)) {
		/* e.g. foo :; */
		tokr_err(t, "Cannot infer type without expression.");
		return false;
	}

	bool annotates_type = !token_is_kw(t->token, KW_EQ) && !token_is_kw(t->token, KW_COMMA);
	if (d->type.kind != TYPE_VOID /* multiple types in one declaration */
		&& (!!(d->flags & DECL_FLAG_ANNOTATES_TYPE)) != annotates_type) { /* annotation on one decl but not the other */
		/* e.g. x: int, y := 3, 5;*/
		tokr_err(t, "You must specify either all types or no types in a single declaration.");
		return false;
	}
	if (annotates_type) {
		d->flags |= DECL_FLAG_ANNOTATES_TYPE;
		Type type;
		if (!parse_type(p, &type)) {
			return false;
		}
		if (n_idents_with_this_type == 1 && d->type.kind == TYPE_VOID) {
			d->type = type;
		} else if (d->type.kind == TYPE_TUPLE) {
			/* add to tuple */
			for (size_t i = 0; i < n_idents_with_this_type; i++) {
				*(Type*)arr_add(&d->type.tuple) = type;
			}
		} else {
			/* construct tuple */
			Array tup_arr;
			arr_create(&tup_arr, sizeof(Type));
			if (d->type.kind != TYPE_VOID) {
				*(Type*)arr_add(&tup_arr) = d->type; /* add current type */
			}
			d->type.flags = 0;
			d->type.kind = TYPE_TUPLE;
			d->type.tuple = tup_arr;
			for (size_t i = 0; i < n_idents_with_this_type; i++) {
				*(Type*)arr_add(&d->type.tuple) = type;
			}
		}
	}

	if (token_is_kw(t->token, KW_COMMA)) {
		/* next type in declaration */
		t->token++;	/* move past , */
		return parse_single_type_in_decl(p, d, ends_with);
	}
	
	/* OPTIM: switch t->token->kw ? */
    if (token_is_kw(t->token, KW_EQ)) {
		t->token++;
		if (!parse_expr(p, &d->expr, expr_find_end(p, 0)))
			return false;
		d->flags |= DECL_FLAG_HAS_EXPR;
		if (ends_decl(t->token, ends_with)) {
			t->token++;
			return true;
		}
		tokr_err(t, "Expected '%c' at end of expression.",
				 ends_with == DECL_END_SEMICOLON ? ';' : ')');
		return false;
		
	} else if (ends_decl(t->token, ends_with)) {
		t->token++;
		return true;
	} else {
		tokr_err(t, "Expected '%c' or '=' at end of delaration.",
				 ends_with == DECL_END_SEMICOLON ? ';' : ')');
		return false;
	}
}

static bool parse_decl(Parser *p, Declaration *d, DeclEndType ends_with) {
	d->type.kind = TYPE_VOID;
	d->where = p->tokr->token->where;
	arr_create(&d->idents, sizeof(Identifier));
	Tokenizer *t = p->tokr;
	d->flags = 0;
	if (ends_decl(t->token, ends_with)) {
		t->token++;
		return true;
	}
	return parse_single_type_in_decl(p, d, ends_with); /* recursively calls itself to parse all types */
}

static bool parse_stmt(Parser *p, Statement *s) {
	Tokenizer *t = p->tokr;
	s->flags = 0;
	if (t->token->kind == TOKEN_EOF)
		tokr_err(t, "Expected statement.");
	s->where = t->token->where;
	/* 
	   TODO: statements such as 3, 5; will not work.
	*/
	if (token_is_kw(t->token + 1, KW_COLON) || token_is_kw(t->token + 1, KW_COMMA)
		|| token_is_kw(t->token + 1, KW_AT)) {
		s->kind = STMT_DECL;
		if (!parse_decl(p, &s->decl, DECL_END_SEMICOLON)) {
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
		Token *end = expr_find_end(p, 0);
		if (token_is_kw(end, KW_SEMICOLON)) {
			s->flags |= STMT_FLAG_VOIDED_EXPR;
		}
		if (!end) {
			tokr_err(t, "No semicolon found at end of statement.");
			while (t->token->kind != TOKEN_EOF) t->token++; /* move to end of file */
			return false;
		}
	    bool success = parse_expr(p, &s->expr, end);
		
		/* go past end regardless of whether successful or not */
		if (end->kind == TOKEN_KW) {
			switch (end->kw) {
			case KW_SEMICOLON:
				t->token = end + 1;
				break;
			case KW_RBRACE:
				t->token = end;	/* the } is past the end of the expr */
				break;
			default: assert(0); break;
			}
		} else {
			t->token = end + 1;
		}
		
		return success;
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

static void fprint_expr(FILE *out, Expression *e);
static void fprint_stmt(FILE *out, Statement *s);
static void fprint_decl(FILE *out, Declaration *d);

static void fprint_type(FILE *out, Type *t) {
	PARSE_PRINT_LOCATION(t->where);
	switch (t->kind) {
	case TYPE_BUILTIN:
		fprintf(out, "%s", keywords[builtin_type_to_kw(t->builtin)]);
		break;
	case TYPE_VOID:
		fprintf(out, "void");
		break;
	case TYPE_UNKNOWN:
		fprintf(out, "???");
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
	case TYPE_TUPLE: {
		fprintf(out, "(");
		arr_foreach(&t->tuple, Type, child) {
			if (child != t->tuple.data) {
				fprintf(out, ", ");
			}
			fprint_type(out, child);
		}
		fprintf(out, ")");
	} break;
	case TYPE_ARR:
		fprintf(out, "[");
		if (t->flags & TYPE_FLAG_RESOLVED) {
			fprintf(out, INTEGER_FMT, t->arr.n);
		} else {
			fprint_expr(out, t->arr.n_expr);
		}
		fprintf(out, "]");
		fprint_type(out, t->arr.of);
		break;
	}
}


static void fprint_block(FILE *out,  Block *b) {
	fprintf(out, "{\n");
	arr_foreach(&b->stmts, Statement, stmt) {
		fprint_stmt(out, stmt);
	}
	fprintf(out, "}");
	if (b->ret_expr) {
		fprintf(out, " returns ");
		fprint_expr(out, b->ret_expr);
	}
	
}

static void fprint_fn_expr(FILE *out, FnExpr *f) {
	fprintf(out, "fn (");
	fprint_decl(out, &f->params);
	fprintf(out, ") ");
	fprint_type(out, &f->ret_type);
	fprintf(out, " ");
	fprint_block(out, &f->body);
}

static void fprint_args(FILE *out, Array *args) {
	fprintf(out, "(");
	arr_foreach(args, Expression, arg) {
		if (arg != args->data) fprintf(out, ", ");
		fprint_expr(out, arg);
	}
	fprintf(out, ")");
}

static void fprint_expr(FILE *out, Expression *e) {
	PARSE_PRINT_LOCATION(e->where);
	switch (e->kind) {
	case EXPR_LITERAL_INT:
		fprintf(out, "%lld", (long long)e->intl);
		break;
	case EXPR_LITERAL_FLOAT:
		fprintf(out, "%f", (double)e->floatl);
		break;
	case EXPR_LITERAL_STR:
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
		case BINARY_SET:
			fprintf(out, "set");
			break;
		case BINARY_AT_INDEX:
			fprintf(out, "at");
			break;
		case BINARY_COMMA:
			fprintf(out, "tuple");
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
		fprint_fn_expr(out, e->fn);
		break;
	case EXPR_CALL:
		fprint_expr(out, e->call.fn);
		fprint_args(out, &e->call.args);
		break;
	case EXPR_DIRECT:
		fprintf(out, "#");
		fprintf(out, "%s", directives[e->direct.which]);
		fprint_args(out, &e->direct.args);
		break;
	}
	fprintf(out, ":");
	fprint_type(out, &e->type);
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
	if ((d->flags & DECL_FLAG_FOUND_TYPE) || (d->flags & DECL_FLAG_ANNOTATES_TYPE)) {
		fprint_type(out, &d->type);
	}
	if (d->flags & DECL_FLAG_HAS_EXPR) {
		fprintf(out, "=");
		fprint_expr(out, &d->expr);
	}
}

static void fprint_stmt(FILE *out, Statement *s) {
	PARSE_PRINT_LOCATION(s->where);
	if (s->flags & STMT_FLAG_VOIDED_EXPR)
		fprintf(out, "(void)");
	
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
