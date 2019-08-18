typedef enum {
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
	TypeKind kind;
	union {
	    BuiltinType builtin;
	};
} Type;

typedef enum {
			  EXPR_INT_CONST,
			  EXPR_FLOAT_CONST
} ExprKind;

typedef struct {
	ExprKind kind;
	Type type;
	bool is_flexible_num:1; /* expressions like 5 or 7*8+3 can be any numerical type */
	union {
		FloatConst floatc;
		IntConst intc;
	};
} Expression;

typedef struct {
	Location where;
	Identifier var;
	Type type;
	Expression expr;
	bool infer_type:1;
	bool is_const:1;
	bool has_expr:1;
} Declaration;

/* OPTIM: Instead of using dynamic arrays, do two passes. */

arr_declaration(Declarations, Declaration, decls_)

typedef enum {
			  STMT_DECLS
} StatementKind;
	
typedef struct {
	StatementKind kind;
	Location where;
	union {
		Declarations decls;
	};
} Statement;

arr_declaration(Statements, Statement, stmts_)

typedef struct {
	Statements stmts;
} ParsedFile;


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

static bool parse_type(Type *type, Tokenizer *t) {
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

static bool parse_expr(Expression *e, Tokenizer *t, Token *end) {
	if (end == NULL) return false;
	memset(e, 0, sizeof *e);
	if (end - t->token == 1) {
		/* 1-token expression */
		switch (t->token->kind) {
		case TOKEN_NUM_CONST: {
			NumConst *num = &t->token->num;
			switch (num->kind) {
			case NUM_CONST_FLOAT:
				e->kind = EXPR_FLOAT_CONST;
				e->type.kind = TYPE_BUILTIN;
				e->type.builtin = BUILTIN_FLOAT;
				e->floatc = num->floatval;
				break;
			case NUM_CONST_INT:
				e->kind = EXPR_INT_CONST;
				e->is_flexible_num = true;
				e->type.kind = TYPE_BUILTIN;
				e->type.builtin = BUILTIN_INT; /* TODO: if it's too big, use a u64 instead. */
				e->floatc = num->intval;
				break;
			}
		} break;
		default:
			tokr_err(t, "Unrecognized expression.");
			return false;
		}
		t->token = end;
		return true;
	}
	/* TODO */
	tokr_err(t, "multi-token exprs not supported yet.");
	return false;
}

/*
ends_with = which keyword does this expression end with?
if it's KW_RPAREN, this will match parentheses properly.
*/
typedef enum {
	  EXPR_END_RPAREN_OR_COMMA,
	  EXPR_END_SEMICOLON
} ExprEndKind;
static Token *expr_find_end(Tokenizer *t, ExprEndKind ends_with) {
	long bracket_level = 0;
	Token *token = t->token;
	while (1) {
		switch (ends_with) {
		case EXPR_END_RPAREN_OR_COMMA:
			if (token->kind == TOKEN_KW) {
				if (token->kw == KW_COMMA && bracket_level == 0)
					return token;
				if (token->kw == KW_LPAREN)
					bracket_level++;
				if (token->kw == KW_RPAREN) {
					bracket_level--;
					if (bracket_level == 0) {
						return token;
					}
				}
			}
			break;
		case EXPR_END_SEMICOLON:
			if (token_is_kw(token, KW_SEMICOLON))
				return token;
			break;
		}
		if (token->kind == TOKEN_EOF) {
			switch (ends_with) {
			case EXPR_END_SEMICOLON:
				tokr_err(t, "Could not find ';' at end of expression.");
				return NULL;
			case EXPR_END_RPAREN_OR_COMMA:
				if (bracket_level > 0) {
					tokr_err(t, "Mismatched parentheses."); /* FEATURE: Find out where this is */
					return NULL;
				} else {
					tokr_err(t, "Could not find ')' or ',' at end of expression.");
					return NULL;
				}
				return NULL;
			}
		}
		token++;
	}
}

static bool parse_decls(Declarations *ds, Tokenizer *t) {
	decls_create(ds);
	while (1) {
		Declaration decl = {0};
		if (t->token->kind != TOKEN_IDENT) {
			tokr_err(t, "Cannot declare non-identifier.");
			return false;
		}

		decl.where = t->token->where;
		decl.var = t->token->ident;
		t->token++;

		if (!token_is_kw(t->token, KW_COLON)) {
			tokr_err(t, "Expected ':' in declaration.");
			return false;
		}
		t->token++;

		if (!token_is_kw(t->token, KW_MINUS)
			&& !token_is_kw(t->token, KW_EQ)
			&& !token_is_kw(t->token, KW_SEMICOLON)) {
			if (!parse_type(&decl.type, t))
				return false;
		} else {
			decl.infer_type = true;
		}
		
		if (token_is_kw(t->token, KW_SEMICOLON)) {
			if (decl.infer_type) {
				tokr_err(t, "Cannot infer type without expression.");
				return false;
			}
		} else if (token_is_kw(t->token, KW_EQ)) {
			t->token++;
			if (!parse_expr(&decl.expr, t, expr_find_end(t, EXPR_END_SEMICOLON)))
				return false;
			decl.has_expr = true;
		} else if (token_is_kw(t->token, KW_MINUS)) {
			t->token++;
			if (!parse_expr(&decl.expr, t, expr_find_end(t, EXPR_END_SEMICOLON)))
				return false;
			decl.has_expr = true;
			decl.is_const = true;
		} else {
			tokr_err(t, "Expected ';', '=', or '-' in delaration.");
			return false;
		}
		decls_add(ds, &decl);
		if (token_is_kw(t->token, KW_SEMICOLON)) {
			t->token++;
			break;
		}
		if (!token_is_kw(t->token, KW_COMMA)) {
			tokr_err(t, "Expected ';' or ',' to finish or continue declaration.");
			return false;
		}
		t->token++; /* move past comma */
	}
	return true;
}

static bool parse_stmt(Statement *s, Tokenizer *t) {
	if (token_is_kw(t->token + 1, KW_COLON)) {
		return parse_decls(&s->decls, t);
	} else {
		tokr_err(t, "Unreocgnized statement.");
		return false;
	}
}

static bool parse_file(ParsedFile *f, Tokenizer *t) {
	stmts_create(&f->stmts);
	bool ret = true;
	while (t->token->kind != TOKEN_EOF) {
		Statement stmt = {0};
		if (!parse_stmt(&stmt, t))
			ret = false;
		stmts_add(&f->stmts, &stmt);
	}
	return ret;
}

static void expr_fprint(FILE *out, Expression *e) {
	/* TODO */
/* 	switch (e->kind) { */
/* 	case : */
/* 	} */
}

static void decl_fprint(FILE *out, Declaration *d) {
	fprintf(out, "l%lu:", (unsigned long)d->where.line);
	ident_fprint(out, d->var);
	if (d->is_const) {
		fprintf(out, "[const]");
	}
	if (d->has_expr) {
		fprintf(out, "=");
	}
}

static void stmt_fprint(FILE *out, Statement *s) {
	switch (s->kind) {
	case STMT_DECLS:
		arr_foreach(s->decls, Declaration, decl) {
			if (decl != s->decls.data) {
				fprintf(out, ", ");
			}
			decl_fprint(out, decl);
		}
		fprintf(out, ";\n");
		break;
	}
}

static void parsed_file_fprint(FILE *out, ParsedFile *f) {
	arr_foreach(f->stmts, Statement, stmt) {
		stmt_fprint(out, stmt);
	}
}
