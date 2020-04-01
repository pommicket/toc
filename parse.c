/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static Status parse_expr(Parser *p, Expression *e, Token *end);
static Status parse_stmt(Parser *p, Statement *s, bool *was_a_statement);
enum {
	  PARSE_DECL_ALLOW_CONST_WITH_NO_EXPR = 0x01,
	  PARSE_DECL_ALLOW_SEMI_CONST = 0x02,
	  PARSE_DECL_ALLOW_INFER = 0x04,
	  PARSE_DECL_ALLOW_EXPORT = 0x08,
	  PARSE_DECL_DONT_SET_IDECLS = 0x10
};
static Status parse_decl(Parser *p, Declaration *d, DeclEndKind ends_with, U16 flags);
static Status parse_decl_list(Parser *p, Declaration **decls, DeclEndKind decl_end);
enum {
	  PARSE_BLOCK_DONT_CREATE_IDENTS = 0x01
};
static bool parse_block(Parser *p, Block *b, U8 flags);
static bool is_decl(Tokenizer *t);
static inline bool ends_decl(Token *t, DeclEndKind ends_with);

static bool fn_has_any_const_params(FnExpr *f) {
	arr_foreach(f->params, Declaration, param)
		if (param->flags & (DECL_IS_CONST | DECL_SEMI_CONST))
			return true;
	return false;
}

static const char *expr_kind_to_str(ExprKind k) {
	switch (k) {
	case EXPR_LITERAL_FLOAT: return "float literal";
	case EXPR_LITERAL_INT: return "integer literal";
	case EXPR_LITERAL_STR: return "string literal";
	case EXPR_LITERAL_BOOL: return "boolean literal";
	case EXPR_LITERAL_CHAR: return "character literal";
	case EXPR_IF: return "if expression";
	case EXPR_WHILE: return "while expression";
	case EXPR_FOR: return "for expression";
	case EXPR_CALL: return "function call";
	case EXPR_C: return "C code";
	case EXPR_BUILTIN: return "#builtin value";
	case EXPR_CAST: return "cast expression";
	case EXPR_UNARY_OP: return "unary operator";
	case EXPR_BINARY_OP: return "binary operator";
	case EXPR_FN: return "function expression";
	case EXPR_TUPLE: return "tuple";
	case EXPR_BLOCK: return "block";
	case EXPR_IDENT: return "identifier";
	case EXPR_SLICE: return "slice";
	case EXPR_TYPE: return "type";
	case EXPR_VAL: return "value";
	case EXPR_NMS: return "namespace";
	}
	assert(0);
	return "";
}

static const char *unary_op_to_str(UnaryOp u) {
	switch (u) {
	case UNARY_MINUS: return "-";
	case UNARY_ADDRESS: return "&";
	case UNARY_DEREF: return "*";
	case UNARY_NOT: return "!";
	case UNARY_LEN: return "len";
	case UNARY_DSIZEOF: return "#sizeof";
	case UNARY_DALIGNOF: return "#alignof";
	case UNARY_SIZEOF: return "sizeof";
	case UNARY_ALIGNOF: return "alignof";
	case UNARY_TYPEOF: return "typeof";
	}
	assert(0);
	return "";
}

static const char *binary_op_to_str(BinaryOp b) {
	switch (b) {
	case BINARY_ADD: return "+";
	case BINARY_SUB: return "-";
	case BINARY_MUL: return "*";
	case BINARY_DIV: return "/";
	case BINARY_SET: return "=";
	case BINARY_SET_ADD: return "+=";
	case BINARY_SET_SUB: return "-=";
	case BINARY_SET_MUL: return "*=";
	case BINARY_SET_DIV: return "/=";
	case BINARY_SET_MOD: return "%=";
	case BINARY_AT_INDEX: return "[]";
	case BINARY_LT: return "<";
	case BINARY_LE: return "<=";
	case BINARY_GT: return ">";
	case BINARY_GE: return ">=";
	case BINARY_EQ: return "==";
	case BINARY_NE: return "!=";
	case BINARY_DOT: return ".";
	case BINARY_MOD: return "%";
	}
	assert(0);
	return "";
}

static bool type_builtin_is_signed(BuiltinType b) {
	switch (b) {
	case BUILTIN_I8:
	case BUILTIN_I16:
	case BUILTIN_I32:
	case BUILTIN_I64:
	case BUILTIN_F32:
	case BUILTIN_F64:
		return true;
	default: return false;
	}
}

static bool type_builtin_is_int(BuiltinType b) {
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

static bool type_builtin_is_float(BuiltinType b) {
	switch (b) {
	case BUILTIN_F32:
	case BUILTIN_F64:
		return true;
	default: return false;
	}
}

static bool type_builtin_is_numerical(BuiltinType b) {
	return type_builtin_is_int(b) || type_builtin_is_float(b);
}


/* returns -1 on failure */
static int kw_to_builtin_type(Keyword kw) {
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
	case KW_FLOAT: return BUILTIN_F32;
	case KW_F32: return BUILTIN_F32;
	case KW_F64: return BUILTIN_F64;
	case KW_BOOL: return BUILTIN_BOOL;
	case KW_CHAR: return BUILTIN_CHAR;
	case KW_TYPE: return BUILTIN_TYPE;
	case KW_NAMESPACE: return BUILTIN_NMS;
	/* don't allow .. => varargs because it's not a normal type */
	default: return -1;
	}
	return -1;
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
	case BUILTIN_BOOL: return KW_BOOL;
	case BUILTIN_CHAR: return KW_CHAR;
	case BUILTIN_TYPE: return KW_TYPE;
	case BUILTIN_NMS: return KW_NAMESPACE;
	case BUILTIN_VARARGS: return KW_DOTDOT;
	}
	assert(0);
	return KW_COUNT;
}

/* returns the number of characters written, not including the null character */
static size_t type_to_str_(Type *t, char *buffer, size_t bufsize) {
	bool resolved = (t->flags & TYPE_IS_RESOLVED) != 0;
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
		size_t written = str_copy(buffer, bufsize, "fn (");
		Type *ret_type = t->fn.types;
		Type *param_types = ret_type + 1;
		size_t nparams = arr_len(t->fn.types) - 1;
		for (size_t i = 0; i < nparams; ++i) {
			if (i > 0)
				written += str_copy(buffer + written, bufsize - written, ", ");
			if (t->fn.constness) {
				switch (t->fn.constness[i]) {
				case CONSTNESS_NO: break;
				case CONSTNESS_SEMI:
					written += str_copy(buffer + written, bufsize - written, ":::");
					break;
				case CONSTNESS_YES:
					written += str_copy(buffer + written, bufsize - written, "::");
					break;
				}
			}

			written += type_to_str_(&param_types[i], buffer + written, bufsize - written);
		}
		written += str_copy(buffer + written, bufsize - written, ")");
		if (ret_type->kind != TYPE_VOID) {
			written += str_copy(buffer + written, bufsize - written, " ");
			written += type_to_str_(ret_type, buffer + written, bufsize - written);
		}
		return written;
	}
	case TYPE_STRUCT: {
		size_t written = 0;
		StructDef *def = t->struc;
		Identifier name = def->name;
		if (name) {
			char *namestr = ident_to_str(name);
			written = str_copy(buffer, bufsize, namestr);
			free(namestr);
		} else {
			 written = str_copy(buffer, bufsize, "anonymous struct");
		}
		if (resolved) {
			if (def->params) {
				written += str_copy(buffer + written, bufsize - written, "(");
				arr_foreach(def->params, Declaration, param) {
					/* TODO: val to str */
					if (param != def->params)
						written += str_copy(buffer + written, bufsize - written, ", ");
					written += str_copy(buffer + written, bufsize - written, "<argument>");
				}
				written += str_copy(buffer + written, bufsize - written, ")");
			}
		}
		return written;
	}
	case TYPE_ARR: {
		size_t written = str_copy(buffer, bufsize, "[");
		if (resolved) {
			snprintf(buffer + written, bufsize - written, U64_FMT, t->arr.n);
			written += strlen(buffer + written);
		} else {
			written += str_copy(buffer + written, bufsize - written, "N");
		}
		written += str_copy(buffer + written, bufsize - written, "]");
		written += type_to_str_(t->arr.of, buffer + written, bufsize - written);
		return written;
	}
	case TYPE_SLICE: {
		size_t written = str_copy(buffer, bufsize, "[");
		written += str_copy(buffer + written, bufsize - written, "]");
		written += type_to_str_(t->slice, buffer + written, bufsize - written);
		return written;
	}
	case TYPE_TUPLE: {
		size_t written = str_copy(buffer, bufsize, "(");
		arr_foreach(t->tuple, Type, child) {
			if (child != t->tuple)
				written += str_copy(buffer + written, bufsize - written, ", ");
			written += type_to_str_(child, buffer + written, bufsize - written);
		}
		written += str_copy(buffer + written, bufsize - written, ")");
		return written;
	}
	case TYPE_PTR: {
		size_t written = str_copy(buffer, bufsize, "&");
		written += type_to_str_(t->ptr, buffer + written, bufsize - written);
		return written;
	}
	case TYPE_EXPR:
		/* TODO: improve this... we're gonna need expr_to_str ): */
		return str_copy(buffer, bufsize, "<type expression>");
	}

	assert(0);
	return 0;
}

/* return value should be freed by caller */
static char *type_to_str(Type *t) {
	char *ret = err_malloc(256);
	type_to_str_(t, ret, 256);
	return ret;
}

/* defaults start to wherever we are now */
static inline Location parser_mk_loc(Parser *p) {
	Location loc = {0};
	loc.file = p->file;
	assert(p->file->tokens == p->tokr->tokens);
	loc.start = (U32)(p->tokr->token - p->tokr->tokens);
	return loc;
}

static inline void parser_set_end_to_token(Parser *p, Location *l, Token *t) {
	l->end = (U32)(t - p->tokr->tokens);
	assert(p->file == l->file);
}

static inline void parser_put_end(Parser *p, Location *l) {
	parser_set_end_to_token(p, l, p->tokr->token);
}

static inline void *parser_arr_add_(Parser *p, void **a, size_t sz) {
	return arr_adda_(a, sz, p->allocr);
}

#define parser_arr_add(p, a) parser_arr_add_(p, (void **)(a), sizeof **(a))

static inline void *parser_malloc(Parser *p, size_t bytes) {
	return allocr_malloc(p->allocr, bytes);
}

static inline void *parser_calloc(Parser *p, size_t n, size_t bytes) {
	return allocr_calloc(p->allocr, n, bytes);
}

/* allocate a new expression. */
static inline Expression *parser_new_expr(Parser *p) {
	return parser_malloc(p, sizeof(Expression));
}

typedef enum {
			  EXPR_CAN_END_WITH_COMMA = 0x01, /* a comma could end the expression */
			  EXPR_CAN_END_WITH_LBRACE = 0x02,
			  EXPR_CAN_END_WITH_COLON = 0x04,
			  EXPR_CAN_END_WITH_DOTDOT = 0x08,
			  EXPR_CAN_END_WITH_EQ = 0x10,
			  EXPR_CAN_END_WITH_WHERE = 0x20
			  /* note that parse_type uses -1 for this */
} ExprEndFlags;

static Token *expr_find_end(Parser *p, ExprEndFlags flags)  {
	Tokenizer *t = p->tokr;
	int paren_level = 0;
	int brace_level = 0;
	int square_level = 0;
	Token *token = t->token;
	while (1) {
		bool all_levels_0 = paren_level == 0 && brace_level == 0 && square_level == 0;
		if (token->kind == TOKEN_KW) {
			switch (token->kw) {
			case KW_COMMA:
				if ((flags & EXPR_CAN_END_WITH_COMMA) && all_levels_0)
					return token;
				break;
			case KW_LPAREN:
				++paren_level;
				break;
			case KW_RPAREN:
				--paren_level;
				if (paren_level < 0)
					return token;
				break;
			case KW_LSQUARE:
				++square_level;
				break;
			case KW_RSQUARE:
				--square_level;
				if (square_level < 0)
					return token;
				break;
			case KW_LBRACE:
				if ((flags & EXPR_CAN_END_WITH_LBRACE) && square_level == 0 && paren_level == 0)
					return token;
				++brace_level;
				break;
			case KW_RBRACE:
				--brace_level;
				if (paren_level == 0 && brace_level == 0 && square_level == 0) {
					/* if there's an else/elif, the expr must continue */
					if (!(token_is_kw(token + 1, KW_ELSE) || token_is_kw(token + 1, KW_ELIF)))
						return token + 1; /* token afer } is end */
				}
				if (brace_level < 0)
					return token;
				break;
			case KW_SEMICOLON:
				if (brace_level == 0)
					return token;
				break;
			case KW_DOTDOT:
				if (all_levels_0 && (flags & EXPR_CAN_END_WITH_DOTDOT))
					return token;
				break;
			case KW_EQ:
				if (all_levels_0 && (flags & EXPR_CAN_END_WITH_EQ))
					return token;
				break;
			case KW_COLON:
				if ((flags & EXPR_CAN_END_WITH_COLON) && all_levels_0)
					return token;
			default: break;
			}
		}
		if (token->kind == TOKEN_EOF) {
			if (brace_level > 0) {
				tokr_err(t, "Opening brace { was never closed."); /* TODO: Find out where this is */
			} else if (paren_level > 0) {
				tokr_err(t, "Opening parenthesis ( was never closed.");
			} else if (square_level > 0) {
				tokr_err(t, "Opening square bracket [ was never closed.");
			} else {
				tokr_err(t, "Could not find end of expression (did you forget a semicolon?).");
				/* TODO: ? improve err message */
			}
			t->token = token; /* don't try to continue */
			return NULL;
		}
		++token;
	}
}

/* parses, e.g. "(3, 5, foo)" */
static Status parse_args(Parser *p, Argument **args) {
	Tokenizer *t = p->tokr;
	Token *start = t->token;
	assert(token_is_kw(start, KW_LPAREN));
	*args = NULL;
	++t->token; /* move past ( */
	if (!token_is_kw(t->token, KW_RPAREN)) {
		/* non-empty arg list */
		while (1) {
			if (t->token->kind == TOKEN_EOF) {
				tokr_err(t, "Expected argument list to continue.");
				info_print(token_location(p->file, start), "This is where the argument list starts.");
				return false;
			}
			Argument *arg = parser_arr_add(p, args);
			arg->where = parser_mk_loc(p);
			/* named arguments */
			if (t->token->kind == TOKEN_IDENT && token_is_kw(t->token + 1, KW_EQ)) {
				arg->name = t->token->ident;
				t->token += 2;
			} else {
				arg->name = NULL;
			}
			if (!parse_expr(p, &arg->val, expr_find_end(p, EXPR_CAN_END_WITH_COMMA))) {
				return false;
			}
			parser_put_end(p, &arg->where);
			
			if (token_is_kw(t->token, KW_RPAREN))
				break;
			assert(token_is_kw(t->token, KW_COMMA));
			++t->token;	/* move past , */
		}
	}
	++t->token;	/* move past ) */
	return true;
}
static void correct_ret_type(Parser *p, Type *ret_type) {
	if (ret_type->kind == TYPE_EXPR && ret_type->expr->kind == EXPR_TUPLE) {
		/* it's returning a tuple! */
		Expression *tuple_members = ret_type->expr->tuple;
		size_t ntuple_members = arr_len(tuple_members);
		ret_type->kind = TYPE_TUPLE;
		ret_type->tuple = NULL;
		arr_set_lena(&ret_type->tuple, ntuple_members, p->allocr);
		for (size_t i = 0; i < ntuple_members; ++i) {
			Type *out_type = &ret_type->tuple[i];
			out_type->flags = 0;
			out_type->kind = TYPE_EXPR;
			out_type->expr = &tuple_members[i];
		}
	}
}

/* where will be filled out with the location, if not NULL */
static Status parse_type(Parser *p, Type *type, Location *where) {
	Tokenizer *t = p->tokr;
	if (where) {
		*where = parser_mk_loc(p);
	}
	type->flags = 0;
	switch (t->token->kind) {
	case TOKEN_KW:
		type->kind = TYPE_BUILTIN;
		{
			int b = kw_to_builtin_type(t->token->kw);
			if (b != -1) {
				type->builtin = (BuiltinType)b;
				++t->token;
				break;
			}
		}
		/* Not a builtin */
		switch (t->token->kw) {
		case KW_FN: {
			/* function type */
			type->kind = TYPE_FN;
			type->fn.types = NULL;
			type->fn.constness = NULL;
			++t->token;
			if (!token_is_kw(t->token, KW_LPAREN)) {
				tokr_err(t, "Expected ( to follow fn.");
				return false;
			}
			parser_arr_add(p, &type->fn.types); /* add return type */
			++t->token;
			if (!token_is_kw(t->token, KW_RPAREN)) {
				while (1) {
					Type *param_type = parser_arr_add(p, &type->fn.types);
					Location type_where;
					if (!parse_type(p, param_type, &type_where)) return false;
					if (token_is_kw(t->token, KW_RPAREN))
						break;
					if (!token_is_kw(t->token, KW_COMMA)) {
						tokr_err(t, "Expected , to continue function type parameter list.");
						return false;
					}
					++t->token; /* move past , */
				}
			}
			++t->token;	/* move past ) */
			Type *ret_type = type->fn.types;
			/* if there's a symbol that isn't [, (, or &, that can't be the start of a type */
			if ((t->token->kind == TOKEN_KW
				 && t->token->kw <= KW_LAST_SYMBOL
				 && t->token->kw != KW_LSQUARE
				 && t->token->kw != KW_LPAREN
				 && t->token->kw != KW_AMPERSAND)
				|| t->token->kw == KW_AS) {
				ret_type->kind = TYPE_VOID;
				ret_type->flags = 0;
			} else {
				if (!parse_type(p, ret_type, NULL))
					return false;
				correct_ret_type(p, ret_type);
			}
			break;
		}
		case KW_LSQUARE: {
			/* array/slice */
			type->kind = TYPE_ARR;
			++t->token;	/* move past [ */
			if (token_is_kw(t->token, KW_RSQUARE)) {
				/* slice */
				type->kind = TYPE_SLICE;
				type->slice = parser_malloc(p, sizeof *type->slice);
				++t->token; /* move past ] */
				Location slice_where;
				if (!parse_type(p, type->slice, &slice_where)) return false;
				break;
			}
			Token *end = expr_find_end(p, 0);
			type->arr.n_expr = parser_new_expr(p);
			if (!parse_expr(p, type->arr.n_expr, end)) return false;
			t->token = end + 1;	/* go past ] */
			type->arr.of = parser_malloc(p, sizeof *type->arr.of);
			Location of_where;
			if (!parse_type(p, type->arr.of, &of_where)) return false;
		} break;
		case KW_AMPERSAND: {
			/* pointer */
			type->kind = TYPE_PTR;
			type->ptr = parser_malloc(p, sizeof *type->ptr);
			++t->token;	/* move past & */
			Location ptr_where;
			if (!parse_type(p, type->ptr, &ptr_where)) return false;
		} break;
		case KW_STRUCT: {
			/* struct */
			type->kind = TYPE_STRUCT;
			StructDef *struc = type->struc = parser_malloc(p, sizeof *type->struc);
			struc->flags = 0;
			struc->name = NULL;
			/* help cgen out */
			struc->c.id = 0;
			struc->params = NULL;
			struc->where = parser_mk_loc(p);
			memset(&struc->body, 0, sizeof struc->body);
			idents_create(&struc->body.idents, p->allocr, &struc->body);
			memset(&struc->instances, 0, sizeof struc->instances);
			struc->body.parent = p->block;
			
			Block *prev_block = p->block;
			p->block = &struc->body;
			
			++t->token;
			if (token_is_kw(t->token, KW_LPAREN)) {
				++t->token;
				if (token_is_kw(t->token, KW_RPAREN)) {
					tokr_err(t, "Empty struct parameter lists are not allowed.");
					goto struct_fail;
				}
				if (!parse_decl_list(p, &struc->params, DECL_END_RPAREN_COMMA))
					goto struct_fail;

				arr_foreach(struc->params, Declaration, param) {
					if (!(param->flags & DECL_IS_CONST)) {
						err_print(param->where, "Struct parameters must be constant.");
						goto struct_fail;
					}
					if ((param->flags & DECL_ANNOTATES_TYPE) && type_is_builtin(&param->type, BUILTIN_VARARGS)) {
						/* TODO(eventually) */
						err_print(param->where, "structs cannot have varargs parameters (yet).");
						goto struct_fail;
					}
					if (param->flags & DECL_INFER) {
						/* TODO(eventually) */
						err_print(param->where, "Struct parameters cannot be inferred (yet).");
						goto struct_fail;
					}
					param->flags |= DECL_IS_PARAM;
				}
			}
			
			p->block = prev_block;
			if (!parse_block(p, &struc->body, PARSE_BLOCK_DONT_CREATE_IDENTS))
				return false;
			struc->where = struc->body.where;
			break;
			
		struct_fail:
			p->block = prev_block;
			return false;
		}
			
		default:
			goto type_expr;
		}
		break;
	default:
	type_expr: {
			/* TYPE_EXPR */
			Token *end = expr_find_end(p, (ExprEndFlags)-1 /* end as soon as possible */);
			if (parse_expr(p, type->expr = parser_new_expr(p), end)) {
				type->kind = TYPE_EXPR;
			} else {
				return false;
			}
		} break;
	}
	if (where)
		parser_put_end(p, where);
	return true;
	
}

/* 
   is the thing we're looking at definitely a type, as opposed to an expression? 
   if end is not NULL, it is set to the token one past the last one in the type,
   assuming it's successful
*/
static bool parser_is_definitely_type(Parser *p, Token **end) {
	Tokenizer *t = p->tokr;
	Token *start = t->token;
	bool ret = false;
	do {
	continu:
		switch (t->token->kind) {
		case TOKEN_KW:
			switch (t->token->kw) {
			case KW_STRUCT:
				ret = true;
				if (end) {
					int level = 1;
					t->token += 2; /* skip struct { */
					while (t->token->kind != TOKEN_EOF) {
						if (t->token->kind == TOKEN_KW) switch (t->token->kw) {
							case KW_LBRACE:
								++level;
								break;
							case KW_RBRACE:
								--level;
								if (level == 0) goto end;
								break;
							default: break;
							}
						++t->token;
					}
				}
				break;
			case KW_LSQUARE:
				ret = true;
				if (end) {
					int level = 1;
					++t->token;
					while (t->token->kind != TOKEN_EOF) {
						if (t->token->kind == TOKEN_KW) switch (t->token->kw) {
							case KW_LSQUARE:
								++level;
								break;
							case KW_RSQUARE:
								--level;
								if (level == 0) {
									if (end) {
										++t->token;
										parser_is_definitely_type(p, &t->token); /* move to end of type */
									}
									goto end;
								}
								break;
							default: break;
							}
						++t->token;
					}
				}
				break;
			case KW_FN: {
				ret = false;
				++t->token;
				if (!token_is_kw(t->token, KW_LPAREN)) {
					break;
				}
				++t->token;
				int paren_level = 1;
				while (t->token->kind != TOKEN_EOF) {
					if (t->token->kind == TOKEN_KW) switch (t->token->kw) {
						case KW_LPAREN:
							++paren_level;
							break;
						case KW_RPAREN:
							--paren_level;
							if (paren_level == 0) {
								++t->token;
								if (token_is_kw(t->token, KW_LBRACE)) goto end; /* void fn expr */
								if (is_decl(t)) /* has return declaration */
									goto end;
								
								if (t->token->kind == TOKEN_KW &&
									(t->token->kw == KW_SEMICOLON || t->token->kw == KW_COMMA || t->token->kw == KW_RPAREN || t->token->kw == KW_LBRACE)) {
									/* void fn type */
									ret = true;
									goto end;
								}
								Type return_type;
								if (!parse_type(p, &return_type, NULL)) {
									return false;
								}
								if (token_is_kw(t->token, KW_LBRACE)) {
									/* non-void fn expr */
									goto end;
								}
								/* it's a non-void function type */
								ret = true;
								goto end;
							}
							break;
						default: break;
						}
					++t->token;
				}
			} break;
			case KW_DOTDOT:
				return true;
			case KW_AMPERSAND:
				++t->token; /* continue; see if next thing is definitely a type */
				goto continu;
			default: {
				int x = kw_to_builtin_type(t->token->kw);
				if ((ret = x != -1)) {
					++t->token;
				}
				break;
			}
			} break;
		case TOKEN_DIRECT:
		case TOKEN_LITERAL_NUM:
		case TOKEN_LITERAL_CHAR:
		case TOKEN_LITERAL_STR:
		case TOKEN_EOF:
		case TOKEN_IDENT:
			ret = false;
			break;
		}
	} while (0);
 end:
	if (ret && end) *end = t->token;
	t->token = start;
	return ret;
}

static Status parse_block(Parser *p, Block *b, U8 flags) {
	Tokenizer *t = p->tokr;
	Block *prev_block = p->block;
	b->flags = 0;
	b->kind = BLOCK_OTHER;
	b->ret_expr = NULL;
	assert(p->block != b);
	b->parent = p->block;
	p->block = b;
	if (!(flags & PARSE_BLOCK_DONT_CREATE_IDENTS))
		idents_create(&b->idents, p->allocr, p->block);
	if (!token_is_kw(t->token, KW_LBRACE)) {
		tokr_err(t, "Expected '{' to open block.");
		return false;
	}
	b->where = parser_mk_loc(p);
#ifdef TOC_DEBUG
	/* temporary, so we can still print the location of the block while we're parsing it. */
	parser_set_end_to_token(p, &b->where, t->token + 1);
#endif
	++t->token;	/* move past { */
	b->stmts = NULL;
	bool ret = true;
	if (!token_is_kw(t->token, KW_RBRACE)) {
		/* non-empty block */
		while (1) {
			Statement *stmt = parser_arr_add(p, &b->stmts);
			bool was_a_statement;
			bool success = parse_stmt(p, stmt, &was_a_statement);
			if (!success) {
				ret = false;
			}
			if (!was_a_statement) {
				arr_remove_lasta(&b->stmts, p->allocr);
			}
			if (token_is_kw(t->token, KW_RBRACE)) {
				break;
			}
			if (t->token->kind == TOKEN_EOF) {
				tokr_err(t, "Expected '}' to close function body.");
				ret = false;
				goto end;
			}
			
		}
	}
	++t->token;	/* move past } */
	parser_put_end(p, &b->where);
 end:
	p->block = prev_block;
	return ret;
}

/* does NOT handle empty declaration lists */
static Status parse_decl_list(Parser *p, Declaration **decls, DeclEndKind decl_end) {
	Tokenizer *t = p->tokr;
	bool ret = true;
	bool first = true;
	*decls = NULL;
	while (t->token->kind != TOKEN_EOF &&
		   (first || (
					  !token_is_kw(t->token - 1, KW_RPAREN) &&
					  !token_is_kw(t->token - 1, KW_LBRACE)))) {
		first = false;
		Declaration *decl = parser_arr_add(p, decls);
		if (!parse_decl(p, decl, decl_end, PARSE_DECL_ALLOW_CONST_WITH_NO_EXPR | PARSE_DECL_ALLOW_SEMI_CONST | PARSE_DECL_ALLOW_INFER)) {
			ret = false;
			/* skip to end of list */
			while (t->token->kind != TOKEN_EOF && !ends_decl(t->token, decl_end))
				++t->token;
			break;
		}
		if (decl->flags & DECL_INFER) {
			/* split this declaration */
			size_t nidents = arr_len(decl->idents);
			for (size_t i = 1; i < nidents; ++i) {
				Declaration *new_decl = parser_arr_add(p, decls);
				*new_decl = *decl;
				new_decl->idents = NULL;
				arr_set_lena(&new_decl->idents, 1, p->allocr);
				new_decl->idents[0] = decl->idents[i];
			}
			arr_set_lena(&decl->idents, 1, p->allocr);
		}
	}
	return ret;
}

static Status parse_fn_expr(Parser *p, FnExpr *f) {
	Tokenizer *t = p->tokr;
	f->instance_id = 0;
	f->ret_decls = NULL;
	f->instances = NULL;
	/* only called when token is fn */
	assert(token_is_kw(t->token, KW_FN));
	++t->token;
	if (!token_is_kw(t->token, KW_LPAREN)) {
		tokr_err(t, "Expected '(' after 'fn'.");
		return false;
	}
	++t->token;
	f->params = NULL;
	bool success = true;
	Block *prev_block = p->block;
	/* enter block so that parameters' scope will be the function body */
	f->body.parent = p->block;
	p->block = &f->body;
	idents_create(&f->body.idents, p->allocr, &f->body);
	if (token_is_kw(t->token, KW_RPAREN)) {
		++t->token;
	} else {
		if (!parse_decl_list(p, &f->params, DECL_END_RPAREN_COMMA))
			return false;
		arr_foreach(f->params, Declaration, param) {
			param->flags |= DECL_IS_PARAM;
		}
	}
	
	if (t->token->kind == TOKEN_EOF) {
		tokr_err(t, "End of file encountered while parsing parameter list.");
		success = false; goto ret;
	}
	
	if (token_is_kw(t->token, KW_LBRACE)) {
		/* void function */
		f->ret_type.kind = TYPE_VOID;
		f->ret_type.flags = 0;
	} else if (is_decl(t)) {
		if (!parse_decl_list(p, &f->ret_decls, DECL_END_LBRACE_COMMA))
			return false;
		arr_foreach(f->ret_decls, Declaration, d) {
			if ((d->flags & DECL_IS_CONST) || (d->flags & DECL_SEMI_CONST)) {
				err_print(d->where, "Named return values cannot be constant.");
				success = false; goto ret;
			}
			if (d->flags & DECL_INFER) {
				err_print(d->where, "Can't infer the value of a named return value!");
				success = false; goto ret;
			}
		}
		--t->token;	/* move back to { */
		/* just set return type to void. the actual return type will be set by types.c:type_of_fn */
		f->ret_type.kind = TYPE_VOID;
		f->ret_type.flags = 0;
	} else {
		if (!parse_type(p, &f->ret_type, NULL)) {
			success = false;
			goto ret;
		}
		correct_ret_type(p, &f->ret_type);
	}
	p->block = prev_block; /* be nice to parse_block */
	if (!parse_block(p, &f->body, PARSE_BLOCK_DONT_CREATE_IDENTS))
		success = false;
 ret:
	f->body.kind = BLOCK_FN;
	p->block = prev_block;
	return success;
}

static void fprint_expr(FILE *out, Expression *e);


#define NOT_AN_OP -1
/* cast isn't really an operator since it operates on types, not exprs. */
#define CAST_PRECEDENCE 2
#define DSIZEOF_PRECEDENCE 5
#define DALIGNOF_PRECEDENCE 5
static int op_precedence(Keyword op) {
	switch (op) {
	case KW_EQ:
	case KW_PLUS_EQ:
	case KW_MINUS_EQ:
	case KW_ASTERISK_EQ:
	case KW_SLASH_EQ:
	case KW_PERCENT_EQ:
		return 0;
	case KW_COMMA: return 1;
	case KW_LT: return 3;
	case KW_GT: return 3;
	case KW_LE: return 3;
	case KW_GE: return 3;
	case KW_EQ_EQ: return 3;
	case KW_NE: return 3;
	case KW_SIZEOF:
	case KW_ALIGNOF:
		return 5;
	case KW_TYPEOF:
		return 6;
	case KW_PLUS: return 10;
	case KW_MINUS: return 20;
	case KW_AMPERSAND: return 25;
	case KW_ASTERISK: return 30;
	case KW_SLASH: return 40;
	case KW_PERCENT: return 45;
	case KW_EXCLAMATION: return 50;
	default: return NOT_AN_OP;
	}
}

static Identifier parser_ident_insert(Parser *p, char *str) {
	Identifiers *idents = p->block ? &p->block->idents : p->globals;
	Identifier i = ident_insert(idents, &str);
	assert(i->idents->scope == p->block);
	return i;
}

static Status check_ident_redecl(Parser *p, Identifier i) {
	Tokenizer *t = p->tokr;
	if (i->idents == (p->block ? &p->block->idents : p->globals)) { /* in the same scope */
		if (i->decl_kind != IDECL_NONE) { /* declared */
			char *s = ident_to_str(i);
			tokr_err(t, "Redeclaration of identifier %s.", s);
			info_print(ident_decl_location(p->file, i), "Previous declaration was here.");
			free(s);
			return false;
		}
	}
	return true;
}

static BuiltinType int_with_size(size_t size) {
	switch (size) {
	case 1:	return BUILTIN_I8;
	case 2:	return BUILTIN_I16;
	case 4:	return BUILTIN_I32;
	case 8:	return BUILTIN_I64;
	}
	return BUILTIN_F32;
}

static BuiltinType uint_with_size(size_t size) {
	switch (size) {
	case 1: return BUILTIN_U8;
	case 2: return BUILTIN_U16;
	case 4: return BUILTIN_U32;
	case 8: return BUILTIN_U64;
	}
	return BUILTIN_F32;
}

static Status ctype_to_type(Allocator *a, CType *ctype, Type *type, Location where) {
	memset(type, 0, sizeof *type);
	type->kind = TYPE_BUILTIN;
	size_t size = 0;
	switch (ctype->kind) {
	case CTYPE_NONE:
		type->kind = TYPE_UNKNOWN;
		break;
	case CTYPE_CHAR:
		type->builtin = BUILTIN_CHAR;
		break;
	case CTYPE_SIGNED_CHAR:
	case CTYPE_UNSIGNED_CHAR:
		size = 1;
		break;
	case CTYPE_SHORT:
	case CTYPE_UNSIGNED_SHORT:
		size = sizeof(short);
		break;
	case CTYPE_INT:
	case CTYPE_UNSIGNED_INT:
		size = sizeof(int);
		break;
	case CTYPE_LONG:
	case CTYPE_UNSIGNED_LONG:
		size = sizeof(long);
		break;
	case CTYPE_SIZE_T:
		size = sizeof(size_t);
		break;
	case CTYPE_VARARGS:
		type->builtin = BUILTIN_VARARGS;
		break;
	case CTYPE_LONGLONG:
	case CTYPE_UNSIGNED_LONGLONG:
#if LONGLONG_AVAILABLE
		size = sizeof(longlong);
#else
		err_print(where, "long long is not supported. Did you compile toc with a pre-C99 compiler?");
		return false;
#endif
		break;
	case CTYPE_FLOAT:
		type->builtin = BUILTIN_F32;
		break;
	case CTYPE_DOUBLE:
		type->builtin = BUILTIN_F64;
		break;
	case CTYPE_PTR:
		type->kind = TYPE_PTR;
		type->ptr = allocr_calloc(a, 1, sizeof *type->ptr);
		type->ptr->kind = TYPE_UNKNOWN;
		break;
	case CTYPE_UNSIGNED: assert(0); break;
	}
	if (size != 0) {
		type->builtin = (((ctype->kind & CTYPE_UNSIGNED) || ctype->kind == CTYPE_SIZE_T) ? uint_with_size : int_with_size)(size);
		if (type->builtin == BUILTIN_F32) {
			err_print(where, "This C type is not representable by a toc type, because it is %lu bytes (not 1, 2, 4, or 8).", size);
			return false;
		}
	}
	return true;
}

static Status parse_c_type(Parser *p, CType *ctype, Type *type) {
	Tokenizer *t = p->tokr;
	if (token_is_direct(t->token, DIRECT_C)) {
		++t->token;
		ctype->kind = CTYPE_NONE;
		if (t->token->kind == TOKEN_KW) {
			switch (t->token->kw) {
			case KW_INT:
				ctype->kind = CTYPE_INT;
				++t->token;
				break;
			case CTYPE_FLOAT:
				ctype->kind = CTYPE_FLOAT;
				++t->token;
				break;
			case KW_CHAR:
				ctype->kind = CTYPE_CHAR;
				++t->token;
				break;
			case KW_AMPERSAND:
				ctype->kind = CTYPE_PTR;
				++t->token;
				if (t->token->kind == TOKEN_LITERAL_STR) {
					size_t n = t->token->str.len;
					ctype->points_to = parser_malloc(p, n+1);
					memcpy(ctype->points_to, t->token->str.str, n);
					ctype->points_to[n] = 0;
				} else {
					tokr_err(t, "Expected string literal to follow #C &");
					return false;
				}
				++t->token;
				break;
			case KW_DOTDOT:
				ctype->kind = CTYPE_VARARGS;
				++t->token;
				break;
			default:
				tokr_err(t, "Unrecognized C type");
				return false;
			}
		} else if (t->token->kind == TOKEN_IDENT) {
			char *id = t->token->ident;
			ctype->kind = 0;
			if (ident_str_len(id) > 9 && strncmp(id, "unsigned_", 9) == 0) {
				ctype->kind |= CTYPE_UNSIGNED;
				id += 9;
			}
			if (ident_str_eq_str(id, "signed_char"))
				ctype->kind = CTYPE_SIGNED_CHAR;
			else if (ident_str_eq_str(id, "short"))
				ctype->kind |= CTYPE_SHORT;
			else if (ident_str_eq_str(id, "long"))
				ctype->kind |= CTYPE_LONG;
			else if (ident_str_eq_str(id, "long_long"))
				ctype->kind |= CTYPE_LONGLONG;
			else if (ident_str_eq_str(id, "double"))
				ctype->kind = CTYPE_DOUBLE;
			else if (ident_str_eq_str(id, "size_t"))
				ctype->kind = CTYPE_SIZE_T;
			else if (ident_str_eq_str(id, "long_double")) {
				tokr_err(t, "long double is not supported for #foreign functions.");
				return false;
			} else {
				tokr_err(t, "Unrecognized C type.");
				return false;
			}
			++t->token;
		} else {
			tokr_err(t, "Unrecognized C type.");
			return false;
		}
		assert(ctype->kind != CTYPE_NONE && ctype->kind != CTYPE_UNSIGNED);
		if (!ctype_to_type(p->allocr, ctype, type, token_location(p->file, t->token)))
			return false;
	} else {
		ctype->kind = CTYPE_NONE;
		if (!parse_type(p, type, NULL))
			return false;
	}
	return true;
}

static Status parse_expr(Parser *p, Expression *e, Token *end) {
	Tokenizer *t = p->tokr;

#if 0
	{
		Location where;
		where.file = p->file;
		where.start = t->token;
		where.end = end;
		printf("PARSING ");
		fprint_location(stdout, where);
	}
#endif
	
	e->flags = 0;
	e->type.flags = 0;
	if (end == NULL) return false;
	e->where = parser_mk_loc(p);
	if (end <= t->token) {
		tokr_err(t, "Empty expression.");
		return false;
	}
	{
		Token *before = t->token;
		if (parser_is_definitely_type(p, NULL)) {
			/* it's a type! */
			e->kind = EXPR_TYPE;
			if (!parse_type(p, e->typeval = parser_malloc(p, sizeof *e->typeval), NULL))
				return false;
			if (t->token == end) goto success;
			/* there's more stuff after */
		}
		t->token = before;
		if (end - t->token == 1) {
			/* 1-token expression */
			switch (t->token->kind) {
			case TOKEN_LITERAL_NUM: {
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
				e->ident = parser_ident_insert(p, t->token->ident);
				break;
			case TOKEN_LITERAL_STR:
				e->kind = EXPR_LITERAL_STR;
				e->strl = t->token->str;
				break;
			case TOKEN_LITERAL_CHAR:
				e->kind = EXPR_LITERAL_CHAR;
				e->charl = t->token->chr;
				break;
			case TOKEN_KW:
				switch (t->token->kw) {
				case KW_TRUE:
					e->kind = EXPR_LITERAL_BOOL;
					e->booll = true;
					break;
				case KW_FALSE:
					e->kind = EXPR_LITERAL_BOOL;
					e->booll = false;
					break;
				default: goto unrecognized;
				}
				break;
			default:
			unrecognized:
				tokr_err(t, "Unrecognized expression.");
				t->token = end + 1;
				return false;
			}
			t->token = end;
			goto success;
		}


		Token *start = t->token;
		if (token_is_direct(t->token, DIRECT_IF)) {
			goto if_expr;
		}
		if (t->token->kind == TOKEN_KW) switch (t->token->kw) {
			case KW_FN: {
				/* this is a function */
				e->kind = EXPR_FN;
				if (!parse_fn_expr(p, e->fn = parser_calloc(p, 1, sizeof *e->fn)))
					return false;
				if (t->token != end) {
					if (token_is_kw(t->token, KW_LPAREN))
						tokr_err(t, "Direct function calling in an expression is not supported.\nYou can wrap the function in parentheses.");
					else
						tokr_err(t, "Expected end of function (did you forget a semicolon?).");
					return false;
				}
				goto success;
			}
			case KW_NMS: {
				Namespace *n = e->nms = parser_malloc(p, sizeof *n);
				e->kind = EXPR_NMS;
				++t->token;
				if (!parse_block(p, &n->body, 0))
					return false;
 				goto success;
			}
			case KW_IF:
			if_expr: {
				IfExpr *i = e->if_ = parser_malloc(p, sizeof *i);
				i->flags = 0;
				if (t->token->kind == TOKEN_DIRECT) {
					i->flags |= IF_STATIC;
				}
				e->kind = EXPR_IF;
				++t->token;
				Token *cond_end = expr_find_end(p, EXPR_CAN_END_WITH_LBRACE);
				if (!cond_end) return false;
				if (!token_is_kw(cond_end, KW_LBRACE)) {
					t->token = cond_end;
					tokr_err(t, "Expected { to open if body.");
					return false;
				}
				i->cond = parser_new_expr(p);
				if (!parse_expr(p, i->cond, cond_end)) return false;
				if (!parse_block(p, &i->body, 0)) return false;
				IfExpr *curr = i;
				while (1) {
					bool is_else = token_is_kw(t->token, KW_ELSE);
					bool is_elif = token_is_kw(t->token, KW_ELIF);
					if (!is_else && !is_elif) {
						curr->next_elif = NULL;
						break;
					}
					if (curr->cond == NULL) {
						tokr_err(t, "You can't have more elif/elses after an else.");
						return false;
					}
					Expression *next = parser_new_expr(p);
					next->flags = 0;
					next->kind = EXPR_IF;
					next->where = parser_mk_loc(p);
					curr->next_elif = next;
					IfExpr *nexti = next->if_ = parser_malloc(p, sizeof *nexti);
					nexti->flags = 0;
					if (is_else) {
						++t->token;
						nexti->cond = NULL;
						if (!parse_block(p, &nexti->body, 0)) return false;
					} else {
						/* elif */
						++t->token;
						cond_end = expr_find_end(p, EXPR_CAN_END_WITH_LBRACE);
						if (!cond_end) return false;
						if (!token_is_kw(cond_end, KW_LBRACE)) {
							t->token = cond_end;
							tokr_err(t, "Expected { to open elif body.");
							return false;
						}
						Expression *cond = parser_new_expr(p);
						if (!parse_expr(p, cond, cond_end))
							return false;
						nexti->cond = cond;
						if (!parse_block(p, &nexti->body, 0)) return false;
					}
					parser_put_end(p, &next->where);
					curr = nexti;
				}
				goto success;
			}
			case KW_WHILE: {
				e->kind = EXPR_WHILE;
				WhileExpr *w = e->while_ = parser_malloc(p, sizeof *w);
				++t->token;
				if (token_is_kw(t->token, KW_LBRACE)) {
					/* infinite loop */
					w->cond = NULL;
				} else {
					Token *cond_end = expr_find_end(p, EXPR_CAN_END_WITH_LBRACE);
					if (!cond_end) return false;
					if (!token_is_kw(cond_end, KW_LBRACE)) {
						t->token = cond_end;
						tokr_err(t, "Expected { to open while body.");
						return false;
					}
					Expression *cond = parser_new_expr(p);
					w->cond = cond;
				
					if (!parse_expr(p, cond, cond_end))
						return false;
				}
				if (!parse_block(p, &w->body, 0)) return false;
				w->body.kind = BLOCK_WHILE;
				goto success;
			}
			case KW_FOR: {
				e->kind = EXPR_FOR;
				ForExpr *fo = e->for_ = parser_malloc(p, sizeof *fo);
				fo->val_stack = NULL;
				fo->flags = 0;
				fo->value = NULL;
				fo->index = NULL;
				Block *prev_block = p->block;
				fo->body.parent = p->block;
				p->block = &fo->body;
				idents_create(&p->block->idents, p->allocr, p->block);
				++t->token;
				if (token_is_kw(t->token, KW_COLON)
					|| (t->token->kind == TOKEN_IDENT
						&& (token_is_kw(t->token + 1, KW_COLON)
							|| (token_is_kw(t->token + 1, KW_COMMA)
								&& t->token[2].kind == TOKEN_IDENT
								&& token_is_kw(t->token + 3, KW_COLON))))) {
					if (t->token->kind == TOKEN_IDENT) {
						fo->value = parser_ident_insert(p, t->token->ident);
						if (!check_ident_redecl(p, fo->value))
							goto for_fail;
						if (ident_eq_str(fo->value, "_")) { /* ignore value */
							fo->value = NULL;
						} else {
							fo->value->decl_kind = IDECL_FOR;
							fo->value->decl_for = fo;
						}
						++t->token;
						if (token_is_kw(t->token, KW_COMMA)) {
							++t->token;
							if (t->token->kind == TOKEN_IDENT) {
								fo->index = parser_ident_insert(p, t->token->ident);
								if (!check_ident_redecl(p, fo->index))
									goto for_fail;
								if (ident_eq_str(fo->index, "_")) { /* ignore index */
									fo->index = NULL;
								} else {
									fo->index->decl_kind = IDECL_FOR;
									fo->index->decl_for = fo;
								}
								++t->token;
							} else {
								tokr_err(t, "Expected identifier after , in for loop.");
								goto for_fail;
							}
						}
					}
					if (!token_is_kw(t->token, KW_COLON)) {
						tokr_err(t, "Expected : following identifiers in for loop.");
						goto for_fail;
					}
					++t->token;
					if (token_is_kw(t->token, KW_COLON)) {
						tokr_err(t, "The variable(s) in a for loop cannot be constant.");
						goto for_fail;
					}
					if (!token_is_kw(t->token, KW_EQ)) {
						fo->flags |= FOR_ANNOTATED_TYPE;
						if (!parse_type(p, &fo->type, NULL))
							goto for_fail;
						if (!token_is_kw(t->token, KW_EQ)) {
							tokr_err(t, "Expected = in for statement.");
							goto for_fail;
						}
					}
					++t->token;
				}
				Token *first_end; first_end = expr_find_end(p, EXPR_CAN_END_WITH_COMMA|EXPR_CAN_END_WITH_DOTDOT|EXPR_CAN_END_WITH_LBRACE);
				Expression *first; first = parser_new_expr(p);
				if (!parse_expr(p, first, first_end))
					goto for_fail;
				if (token_is_kw(first_end, KW_LBRACE)) {
					fo->of = first;
				} else if (token_is_kw(first_end, KW_DOTDOT) || token_is_kw(first_end, KW_COMMA)) {
					fo->flags |= FOR_IS_RANGE;
					fo->range.from = first;
					if (token_is_kw(first_end, KW_COMMA)) {
						/* step */
						++t->token;
						fo->range.step = parser_new_expr(p);
						Token *step_end = expr_find_end(p, EXPR_CAN_END_WITH_LBRACE|EXPR_CAN_END_WITH_DOTDOT);
						if (!parse_expr(p, fo->range.step, step_end))
							goto for_fail;
						if (!token_is_kw(step_end, KW_DOTDOT)) {
							err_print(token_location(p->file, step_end), "Expected .. to follow step in for statement.");
							goto for_fail;
						}
					} else {
						fo->range.step = NULL;
					}
					++t->token; /* move past .. */
					if (token_is_kw(t->token, KW_LBRACE)) {
						fo->range.to = NULL; /* infinite loop! */
					} else {
						fo->range.to = parser_new_expr(p);
						Token *to_end = expr_find_end(p, EXPR_CAN_END_WITH_LBRACE);
						if (!parse_expr(p, fo->range.to, to_end))
							goto for_fail;
						if (!token_is_kw(t->token, KW_LBRACE)) {
							tokr_err(t, "Expected { to open body of for statement.");
							goto for_fail;
						}
					}
				} else {
					err_print(token_location(p->file, first_end), "Expected { or .. to follow expression in for statement.");
					goto for_fail;
				}
				p->block = prev_block;
				if (!parse_block(p, &fo->body, PARSE_BLOCK_DONT_CREATE_IDENTS))
					goto for_fail;
				fo->body.kind = BLOCK_FOR;
				goto success;
				for_fail:
				p->block = prev_block;
				return false;
			}
			default: break;
			}
		if (token_is_direct(t->token, DIRECT_FOREIGN)) {
			e->kind = EXPR_FN;
			FnExpr *fn = e->fn = parser_calloc(p, 1, sizeof *e->fn);
			fn->flags |= FN_EXPR_FOREIGN;
			fn->foreign.fn_ptr = 0;
			fn->where = parser_mk_loc(p);
			++t->token;
			if (!token_is_kw(t->token, KW_LPAREN)) {
				tokr_err(t, "Expected ( following #foreign.");
				return false;
			}
			++t->token;
			Type *fn_t = &fn->foreign.type;
			fn_t->kind = TYPE_FN;
			FnType *fn_type = &fn_t->fn;
			fn_type->constness = NULL;
			fn_type->types = NULL;
			Type *ret_type = parser_arr_add(p, &fn_type->types);
			CType *ret_ctype = parser_arr_add(p, &fn->foreign.ctypes);
			Expression *name = fn->foreign.name_expr = parser_new_expr(p);
					
			if (!parse_expr(p, name, expr_find_end(p, EXPR_CAN_END_WITH_COMMA)))
				return false;
			if (token_is_kw(t->token, KW_RPAREN)) {
				fn->foreign.lib_expr = NULL;
			} else {
				if (!token_is_kw(t->token, KW_COMMA)) {
					tokr_err(t, "Expected , to follow #foreign name.");
					return false;
				}
				++t->token;
				Expression *lib = fn->foreign.lib_expr = parser_new_expr(p);
				if (!parse_expr(p, lib, expr_find_end(p, 0)))
					return false;
				if (!token_is_kw(t->token, KW_RPAREN)) {
					tokr_err(t, "Expected ) to follow #foreign lib.");
					return false;
				}	
			}
			++t->token;
					
					
			if (!token_is_kw(t->token, KW_FN)) {
				tokr_err(t, "Expected fn to follow #foreign.");
				return false;
			}
			++t->token;
			if (!token_is_kw(t->token, KW_LPAREN)) {
				tokr_err(t, "Expected ( after #foreign fn");
				return false;
			}
			++t->token;
			while (!token_is_kw(t->token, KW_RPAREN)) {
				Type *type = parser_arr_add(p, &fn_type->types);
				CType *ctype = parser_arr_add(p, &fn->foreign.ctypes);
				if (!parse_c_type(p, ctype, type)) {
					return false;
				}
				if (token_is_kw(t->token, KW_COMMA)) {
					++t->token;
				} else if (token_is_kw(t->token, KW_RPAREN)) {
					++t->token;
					break;
				} else {
					tokr_err(t, "Expected , or ) following #foreign fn type.");
					return false;
				}
			}
			if (t->token == end) {
				/* void */
				ret_ctype->kind = CTYPE_NONE;
				ret_type->kind = TYPE_VOID;
				ret_type->flags = 0;
				ret_type->was_expr = NULL;
			} else {
				if (!parse_c_type(p, ret_ctype, ret_type))
					return false;
			}
			parser_put_end(p, &fn->where);
			return true;
		}

		/* NOTE: the . operator is not handled here, but further down, in order to allow some_struct.fn_member() */
		Token *dot = NULL; /* this keeps track of it for later */
	
		/* Find the lowest-precedence operator not in parentheses/braces/square brackets */
		int paren_level = 0;
		int brace_level = 0;
		int square_level = 0;
		int lowest_precedence = NOT_AN_OP;
		/* e.g. (5+3) */
		bool entirely_within_parentheses = token_is_kw(t->token, KW_LPAREN);
		Token *lowest_precedence_op = NULL;
		for (Token *token = t->token; token < end; ++token) {
			if (token->kind == TOKEN_KW) {
				switch (token->kw) {
				case KW_LPAREN:
					++paren_level;
					break;
				case KW_RPAREN:
					--paren_level;
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
					++brace_level;
					break;
				case KW_RBRACE:
					--brace_level;
					if (brace_level < 0) {
						t->token = token;
						tokr_err(t, "Excessive closing }.");
						return false;
					}
					break;
				case KW_LSQUARE:
					++square_level;
					break;
				case KW_RSQUARE:
					--square_level;
					if (square_level < 0) {
						tokr_err(t, "Excessive closing ].");
						return false;
					}
					break;
				case KW_DOT:
					if (paren_level == 0 && brace_level == 0 && square_level == 0) {
						dot = token;
					}
					break;
				default: break;
				}
			}
			
			if (paren_level == 0 && brace_level == 0 && square_level == 0) {
				int precedence = NOT_AN_OP;
				if (token->kind == TOKEN_KW) {
					switch (token->kw) {
					case KW_AS: precedence = CAST_PRECEDENCE; break;
					default: precedence = op_precedence(token->kw); break;
					}
				} else if (token->kind == TOKEN_DIRECT) {
					switch (token->direct) {
					case DIRECT_SIZEOF: precedence = DSIZEOF_PRECEDENCE; break;
					case DIRECT_ALIGNOF: precedence = DALIGNOF_PRECEDENCE; break;
					default: break;
					}
				}
				if (precedence != NOT_AN_OP) {
					if (lowest_precedence == NOT_AN_OP || precedence <= lowest_precedence) {
						lowest_precedence = precedence;
						lowest_precedence_op = token;
					}
				}
			}
		}

		if (paren_level > 0) {
			t->token = start;
			while (!token_is_kw(t->token, KW_LPAREN))
				++t->token;
			
			tokr_err(t, "Too many opening parentheses (.");
			return false;
		}
		if (brace_level > 0) {
			t->token = start;
			while (!token_is_kw(t->token, KW_LBRACE))
				++t->token;
			
			tokr_err(t, "Too many opening braces {.");
			return false;
		}
		if (square_level > 0) {
			t->token = start;
			while (!token_is_kw(t->token, KW_LSQUARE))
				++t->token;
			tokr_err(t, "Too many opening square brackets [.");
			return false;
		}
	
		if (entirely_within_parentheses) {
			++t->token;	/* move past opening ( */
			if (token_is_kw(t->token, KW_RPAREN)) {
				/* ()foo */
				--t->token;
				tokr_err(t, "Stray () (maybe try wrapping the stuff before this in parentheses)");
				return false;
			}
			Token *new_end = end - 1; /* parse to ending ) */
			U32 start_idx = e->where.start;
			if (!parse_expr(p, e, new_end))
				return false;
			e->where.start = start_idx; /* make sure we keep e->where.start intact */
			++t->token;	/* move past closing ) */
			goto success;
		}
	
		if (lowest_precedence != NOT_AN_OP) {
		
			/* Check if this is a unary op not a binary one (e.g. +-3 => +(-3), not (+)-(3)). */
			while (lowest_precedence_op != t->token
				   && lowest_precedence_op[-1].kind == TOKEN_KW
				   && op_precedence(lowest_precedence_op[-1].kw) != NOT_AN_OP) {
				--lowest_precedence_op;
			}
			if (lowest_precedence_op == t->token) {
				/* Unary */
				UnaryOp op;
				bool is_unary = true;
				if (t->token->kind == TOKEN_KW) {
					switch (t->token->kw) {
					case KW_PLUS:
						/* unary + is ignored entirely */
						++t->token;
						/* re-parse this expression without + */
						return parse_expr(p, e, end);
					case KW_MINUS:
						op = UNARY_MINUS;
						break;
					case KW_AMPERSAND:
						op = UNARY_ADDRESS;
						break;
					case KW_ASTERISK:
						op = UNARY_DEREF;
						break;
					case KW_EXCLAMATION:
						op = UNARY_NOT;
						break;
					case KW_SIZEOF:
						op = UNARY_SIZEOF;
						break;
					case KW_ALIGNOF:
						op = UNARY_ALIGNOF;
						break;
					case KW_TYPEOF:
						op = UNARY_TYPEOF;
						break;
					default:
						is_unary = false;
						break;
					}
				} else if (t->token->kind == TOKEN_DIRECT) {
					switch (t->token->direct) {
					case DIRECT_SIZEOF:
						op = UNARY_DSIZEOF;
						break;
					case DIRECT_ALIGNOF:
						op = UNARY_DALIGNOF;
						break;
					default:
						is_unary = false;
						break;
					}
				} else {
					is_unary = false;
				}
				if (!is_unary) {
					tokr_err(t, "This is not a unary operator, but it's being used as one.");
					return false;
				}
				e->unary.op = op;
				e->kind = EXPR_UNARY_OP;
				++t->token;
				Expression *of = parser_new_expr(p);
				e->unary.of = of;
				if (!parse_expr(p, of, end))
					return false;
				goto success;
			}

			if (lowest_precedence_op->kw == KW_AS) {
				/* cast */
				Expression *casted = parser_new_expr(p);
				e->kind = EXPR_CAST;
				e->cast.expr = casted;
				if (!parse_expr(p, casted, lowest_precedence_op))
					return false;
				t->token = lowest_precedence_op + 1;
				if (token_is_direct(t->token, DIRECT_C)) {
					/* cast to #C type */
					if (!parse_c_type(p, &e->cast.ctype, &e->cast.type))
						return false;
				} else {
					e->cast.ctype.kind = CTYPE_NONE;
					if (!parse_type(p, &e->cast.type, NULL))
						return false;
				}
				if (t->token != end) {
					tokr_err(t, "Cast expression continues after type");
					return false;
				}
				goto success;
			}
	
			if (lowest_precedence_op->kw == KW_COMMA) {
				Expression lhs, rhs;
				if (!parse_expr(p, &lhs, lowest_precedence_op)) return false;
				t->token = lowest_precedence_op + 1;
				if (!parse_expr(p, &rhs, end)) return false;
				/* create tuple expr out of lhs, rhs */
				e->kind = EXPR_TUPLE;
				e->tuple = NULL;
				if (lhs.kind == EXPR_TUPLE) {
					e->tuple = lhs.tuple;
				} else {
					*(Expression *)parser_arr_add(p, &e->tuple) = lhs;
				}
				if (rhs.kind == EXPR_TUPLE) {
					arr_foreach(rhs.tuple, Expression, r) {
						*(Expression *)parser_arr_add(p, &e->tuple) = *r;
					}
				} else {
					*(Expression *)parser_arr_add(p, &e->tuple) = rhs;
				}
				goto success;
			}
			BinaryOp op;
			switch (lowest_precedence_op->kw) {
			case KW_PLUS:
				op = BINARY_ADD;
				break;
			case KW_MINUS:
				op = BINARY_SUB;
				break;
			case KW_ASTERISK:
				op = BINARY_MUL;
				break;
			case KW_SLASH:
				op = BINARY_DIV;
				break;
			case KW_PERCENT:
				op = BINARY_MOD;
				break;
			case KW_EQ_EQ:
				op = BINARY_EQ;
				break;
			case KW_NE:
				op = BINARY_NE;
				break;
			case KW_LT:
				op = BINARY_LT;
				break;
			case KW_LE:
				op = BINARY_LE;
				break;
			case KW_GT:
				op = BINARY_GT;
				break;
			case KW_GE:
				op = BINARY_GE;
				break;
			case KW_EQ:
				op = BINARY_SET;
				break;
			case KW_PLUS_EQ:
				op = BINARY_SET_ADD;
				break;
			case KW_MINUS_EQ:
				op = BINARY_SET_SUB;
				break;
			case KW_ASTERISK_EQ:
				op = BINARY_SET_MUL;
				break;
			case KW_SLASH_EQ:
				op = BINARY_SET_DIV;
				break;
			case KW_PERCENT_EQ:
				op = BINARY_SET_MOD;
				break;
			default:
				err_print(token_location(p->file, lowest_precedence_op), "Unary operator '%s' being used as a binary operator!", kw_to_str(lowest_precedence_op->kw));
				return false;
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
			goto success;
		} else {
			/* function calls, array accesses, etc. */

			if (t->token->kind == TOKEN_DIRECT) {
				/* it's a directive */
				Expression *single_arg = NULL; /* points to an expr if this is a directive with one expression argument */
			
				switch (t->token->direct) {
				case DIRECT_IF:
					assert(0); /* handled above */
					break;
				case DIRECT_C:
					e->kind = EXPR_C;
					single_arg = e->c.code = parser_new_expr(p);
					break;
				case DIRECT_BUILTIN:
					e->kind = EXPR_BUILTIN;
					single_arg = e->builtin.which.expr = parser_new_expr(p);
					break;
				default:
					tokr_err(t, "Unrecognized expression.");
					return false;
				case DIRECT_COUNT: assert(0); break;
				}
				if (single_arg) {
					++t->token;
					if (!token_is_kw(t->token, KW_LPAREN)) {
						tokr_err(t, "Expected ( to follow #%s.", directives[t->token[-1].direct]);
						return false;
					}
					++t->token;
					Token *arg_end = expr_find_end(p, 0);
					if (!token_is_kw(arg_end, KW_RPAREN)) {
						err_print(token_location(p->file, arg_end), "Expected ) at end of #%s directive.", directives[t->token->direct]);
						return false;
					}
					if (!parse_expr(p, single_arg, arg_end))
						return false;
					++t->token;
					goto success;
				}
			}
		
			/* try a function call or array access */
			Token *token = t->token;
		
			/* currently unnecessary: paren_level = square_level = 0; */
			/* 
			   can't call at start, e.g. in (fn() {})(), it is not the empty function ""
			   being called with fn() {} as an argument
			*/
			if (token_is_kw(t->token, KW_LPAREN)) {
				++paren_level;
				++token;
			}
			/* which opening bracket starts the call/array access */
			Token *opening_bracket = NULL;
			Token *closing_bracket = NULL;
			for (; token < end; ++token) {
				if (token->kind == TOKEN_KW) {
					switch (token->kw) {
					case KW_LPAREN:
						if (square_level == 0 && paren_level == 0 && brace_level == 0
							&& token != t->tokens
							&& token[-1].kind != TOKEN_DIRECT /* don't include directives */
							&& !token_is_kw(&token[-1], KW_DOT)) /* or some_struct.("property") */
							opening_bracket = token; /* maybe this left parenthesis opens the function call */
						++paren_level;
						break;
					case KW_LSQUARE:
						if (square_level == 0 && paren_level == 0 && brace_level == 0)
							opening_bracket = token; /* (array access) */
						++square_level;
						break;
					case KW_RPAREN:
						--paren_level;
						if (opening_bracket && token_is_kw(opening_bracket, KW_LPAREN) && square_level == 0 && paren_level == 0 && brace_level == 0)
							closing_bracket = token;
						break;
					case KW_RSQUARE:
						--square_level;
						if (opening_bracket && token_is_kw(opening_bracket, KW_LSQUARE) && square_level == 0 && paren_level == 0 && brace_level == 0)
							closing_bracket = token;
						break;
					case KW_LBRACE:
						++brace_level;
						break;
					case KW_RBRACE:
						--brace_level;
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
			if (opening_bracket && closing_bracket && closing_bracket + 1 == end /* make sure there's nothing after the closing bracket */) {
				switch (opening_bracket->kw) {
				case KW_LPAREN: {
					/* it's a function call! */
					e->kind = EXPR_CALL;
					e->call.fn = parser_new_expr(p);
					if (!parse_expr(p, e->call.fn, opening_bracket)) { /* parse up to ( as function */
						return false;
					}
					t->token = opening_bracket;
					if (!parse_args(p, &e->call.args))
						return false;
					goto success;
				}
				case KW_LSQUARE: {
					Expression *arr = parser_new_expr(p);
					/* it's an array access or slice */
					/* parse array */
					if (!parse_expr(p, arr, opening_bracket)) return false;
				
					t->token = opening_bracket + 1;
					Token *iend = NULL;
					if (token_is_kw(t->token, KW_COLON)) {
						/* slice */
						goto expr_is_slice;
					}
					iend = expr_find_end(p, EXPR_CAN_END_WITH_COLON);
					if (iend->kind != TOKEN_KW) {
						err_print(token_location(p->file, iend), "Expected ] or : after index.");
						return false;
					}
					switch (iend->kw) {
					case KW_RSQUARE:
						/* array access */
						e->kind = EXPR_BINARY_OP;
						e->binary.op = BINARY_AT_INDEX;
						e->binary.lhs = arr;
						e->binary.rhs = parser_new_expr(p);
						if (!parse_expr(p, e->binary.rhs, iend))
							return false;
						break;
					expr_is_slice: 
					case KW_COLON: {
						/* slice */
						SliceExpr *s = &e->slice;
						e->kind = EXPR_SLICE;
						s->of = arr;
						if (iend) {
							s->from = parser_new_expr(p);
							if (!parse_expr(p, s->from, iend))
								return false;
						} else {
							/* e.g. x[:5] */
							s->from = NULL;
						}
						assert(token_is_kw(t->token, KW_COLON));
						++t->token;
						if (token_is_kw(t->token, KW_RSQUARE)) {
							/* e.g. x[5:] */
							s->to = NULL;
						} else {
							s->to = parser_new_expr(p);
							Token *to_end = expr_find_end(p, 0);
							if (!token_is_kw(to_end, KW_RSQUARE)) {
								err_print(token_location(p->file, iend), "Expected ] at end of slice.");
								return false;
							}
							if (!parse_expr(p, s->to, to_end))
								return false;
						}
					} break;
					default:
						err_print(token_location(p->file, iend), "Expected ] or : after index.");
						return false;
					}
					++t->token;	/* move past ] */
					goto success;
				}
				default:
					assert(0);
					return false;
				}
			}

			if (token_is_kw(t->token, KW_LBRACE)) {
				/* it's a block */
				e->kind = EXPR_BLOCK;
				if (!parse_block(p, e->block = parser_malloc(p, sizeof *e->block), 0)) return false;
				if (t->token != end) {
					tokr_err(t, "Expression continues after end of block."); /* TODO: improve this err message */
					return false;
				}
				goto success;
			}

			if (dot) {
				e->kind = EXPR_BINARY_OP;
				e->binary.lhs = parser_new_expr(p);
				e->binary.rhs = parser_new_expr(p);
				e->binary.op = BINARY_DOT;
				if (!parse_expr(p, e->binary.lhs, dot))
					return false;
				t->token = dot + 1;
				if (!parse_expr(p, e->binary.rhs, end))
					return false;
				goto success;
			}
			
			tokr_err(t, "Unrecognized expression.");
			return false;
		}
	}
 success:
	parser_put_end(p, &e->where);
	if (t->token != end) {
		tokr_err(t, "Did not expect this stuff after expression. Did you forget a semicolon?");
		return false;
	}

	if (e->kind == EXPR_FN) {
		e->fn->where = e->where;
	}
	return true;
}

static inline bool ends_decl(Token *t, DeclEndKind ends_with) {
	if (t->kind != TOKEN_KW) return false;
	switch (ends_with) {
	case DECL_END_SEMICOLON:
		return t->kw == KW_SEMICOLON;
	case DECL_END_RPAREN_COMMA:
		return t->kw == KW_RPAREN || t->kw == KW_COMMA;
	case DECL_END_LBRACE_COMMA:
		return t->kw == KW_LBRACE || t->kw == KW_COMMA;
	default: assert(0); return false;
	}
}

static Status parse_decl(Parser *p, Declaration *d, DeclEndKind ends_with, U16 flags) {
	Tokenizer *t = p->tokr;
	d->where = parser_mk_loc(p);
	d->idents = NULL;
	d->flags = 0;
	d->val_stack = NULL;
	
	bool is_varargs = false;
	
	if ((flags & PARSE_DECL_ALLOW_EXPORT) && token_is_direct(t->token, DIRECT_EXPORT)) {
		d->flags |= DECL_EXPORT;
		++t->token;
	}
	
	while (1) {
		Identifier *ident = parser_arr_add(p, &d->idents);
		if (t->token->kind != TOKEN_IDENT) {
			tokr_err(t, "Cannot declare non-identifier (%s).", token_kind_to_str(t->token->kind));
			goto ret_false;
		}
		*ident = parser_ident_insert(p, t->token->ident);
		if (!(flags & PARSE_DECL_DONT_SET_IDECLS) && !ident_eq_str(*ident, "_")) {
			Identifier i = *ident;
			if (!check_ident_redecl(p, i))
				goto ret_false;
			i->decl_kind = IDECL_DECL;
			i->decl = d;
		}
		++t->token;
		if (token_is_kw(t->token, KW_COMMA)) {
			++t->token;
			continue;
		}
		if (token_is_kw(t->token, KW_COLON)) {
			++t->token;
		} else {
			
			tokr_err(t, "Expected ',' to continue listing variables or ':' / '::' to indicate type.");
			goto ret_false;
		}
		if (token_is_kw(t->token, KW_COLON)) {
			++t->token;
			if (token_is_kw(t->token, KW_COLON) && (flags & PARSE_DECL_ALLOW_SEMI_CONST)) {
				++t->token;
				d->flags |= DECL_SEMI_CONST;
			} else {
				d->flags |= DECL_IS_CONST;
			}
			break;
		}
		break;
	}
	
	if (token_is_kw(t->token, KW_SEMICOLON) || token_is_kw(t->token, KW_RPAREN)) {
		/* e.g. foo :; */
		tokr_err(t, "Cannot infer type without expression.");
		goto ret_false;
	}

	{
		bool annotates_type = !token_is_kw(t->token, KW_EQ) && !token_is_kw(t->token, KW_COMMA);
		if (annotates_type) {
			d->flags |= DECL_ANNOTATES_TYPE;
			if (token_is_kw(t->token, KW_DOTDOT)) {
				d->type.kind = TYPE_BUILTIN;
				d->type.flags = 0;
				d->type.was_expr = NULL;
				d->type.builtin = BUILTIN_VARARGS;
				is_varargs = true;
				if (d->flags & DECL_SEMI_CONST) {
					tokr_err(t, "Semi-constant varargs are not allowed. Sorry!");
					goto ret_false;
				}
				++t->token;
			} else {
				Type type;
				Location type_where;
				if (!parse_type(p, &type, &type_where)) {
					goto ret_false;
				}
				d->type = type;
				if (type.kind == TYPE_TUPLE && arr_len(d->type.tuple) != arr_len(d->idents)) {
					err_print(type_where, "Expected to have %lu things declared in declaration, but got %lu.", (unsigned long)arr_len(d->type.tuple), (unsigned long)arr_len(d->idents));
					goto ret_false;
				}
			}
		}
	}
	{
		const char *end_str = NULL;
		switch (ends_with) {
		case DECL_END_SEMICOLON: end_str = "';'"; break;
		case DECL_END_RPAREN_COMMA: end_str = "')' or ','"; break;
		case DECL_END_LBRACE_COMMA: end_str = "'{' or ','"; break;
		}
		assert(end_str);
			
		if (token_is_kw(t->token, KW_EQ)) {
			++t->token;
			if ((flags & PARSE_DECL_ALLOW_INFER) && ends_decl(t->token, ends_with)) {
				/* inferred expression */
				d->flags |= DECL_INFER;
				if (!(d->flags & DECL_IS_CONST)) {
					tokr_err(t, "Inferred parameters must be constant.");
					goto ret_false;
				}
				if (is_varargs) {
					tokr_err(t, "Varargs cannot be inferred.");
					goto ret_false;
				}
				++t->token;
			} else {
				d->flags |= DECL_HAS_EXPR;
				uint16_t expr_flags = 0;
				if (ends_with == DECL_END_RPAREN_COMMA)
					expr_flags |= EXPR_CAN_END_WITH_COMMA;
				if (ends_with == DECL_END_LBRACE_COMMA)
					expr_flags |= EXPR_CAN_END_WITH_LBRACE | EXPR_CAN_END_WITH_COMMA;
				if (is_varargs) {
					tokr_err(t, "Default varargs are not allowed.");
					goto ret_false;
				}
				Token *end = expr_find_end(p, expr_flags);
				if (!end) {
					if (end) t->token = end;
					tokr_err(t, "Expected %s at end of declaration.", end_str);
					goto ret_false;
				}
				if (!parse_expr(p, &d->expr, end)) {
					t->token = end; /* move to ; */
					goto ret_false;
				}
				if (ends_with == DECL_END_SEMICOLON && end > t->tokens && token_is_kw(end - 1, KW_RBRACE)) {
					/* allow semicolon to be ommitted, e.g. f ::= fn() {} */
				} else if (ends_decl(t->token, ends_with)) {
					++t->token;
				} else {
					tokr_err(t, "Expected %s at end of declaration.", end_str);
					goto ret_false;
				}
			}
		} else if (ends_decl(t->token, ends_with)) {
			++t->token;
		} else {
			tokr_err(t, "Expected %s or '=' at end of delaration.", end_str);
			goto ret_false;
		}
	}
	
	if ((d->flags & DECL_IS_CONST) && !(d->flags & DECL_HAS_EXPR) && !(flags & PARSE_DECL_ALLOW_CONST_WITH_NO_EXPR)) {
		--t->token;
		/* disallow constant without an expression, e.g. x :: int; */
		tokr_err(t, "You must have an expression at the end of this constant declaration.");
		goto ret_false;
	}
	
	parser_put_end(p, &d->where);

	return true;
	
 ret_false:
	/* move past end of decl */
	tokr_skip_semicolon(t);
	return false;
}

static bool is_decl(Tokenizer *t) {
	Token *token = t->token;
	
	/* you can only export declarations */
	if (token_is_direct(token, DIRECT_EXPORT))
		return true;	
	
	while (1) {
		if (token->kind != TOKEN_IDENT) return false;
		++token;
		if (token->kind != TOKEN_KW) return false;
		if (token->kw == KW_COLON)
			return true;
		if (token->kw != KW_COMMA) return false;
		++token;
	}
}

/* sets *was_a_statement to false if s was not filled, but the token was advanced */
static Status parse_stmt(Parser *p, Statement *s, bool *was_a_statement) {
	Tokenizer *t = p->tokr;
	if (t->token->kind == TOKEN_EOF) {
		tokr_err(t, "Expected statement.");
		return false;
	}
	s->where = parser_mk_loc(p);
	s->flags = 0;
	*was_a_statement = true;
	if (t->token->kind == TOKEN_KW) {
		switch (t->token->kw) {
		case KW_SEMICOLON:
			*was_a_statement = false;
			++t->token;
			goto success;
		case KW_RETURN: {
			s->kind = STMT_RET;
			++t->token;
			Return *r = s->ret = parser_malloc(p, sizeof *r);
			r->flags = 0;
			if (token_is_kw(t->token, KW_SEMICOLON)) {
				/* return with no expr */
				++t->token;
				goto success;
			}
			r->flags |= RET_HAS_EXPR;
			Token *end = expr_find_end(p, 0);
			if (!end) {
				while (t->token->kind != TOKEN_EOF) ++t->token; /* move to end of file */
				return false;
			}
			if (!token_is_kw(end, KW_SEMICOLON)) {
				err_print(token_location(p->file, end), "Expected ; at end of return statement.");
				t->token = end->kind == TOKEN_EOF ? end : end + 1;
				return false;
			}
			bool parsed = parse_expr(p, &r->expr, end);
			t->token = end + 1;
			if (!parsed) return false;
			goto success;
		}
		case KW_BREAK:
			s->kind = STMT_BREAK;
			++t->token;
			if (!token_is_kw(t->token, KW_SEMICOLON)) {
				tokr_err(t, "Expected ; after break.");
				tokr_skip_semicolon(t);
				return false;
			}
			goto success;
		case KW_CONTINUE:
			s->kind = STMT_CONT;
			++t->token;
			if (!token_is_kw(t->token, KW_SEMICOLON)) {
				tokr_err(t, "Expected ; after continue.");
				tokr_skip_semicolon(t);
				return false;
			}
			goto success;
		case KW_DEFER: {
			if (p->block == NULL) {
				tokr_err(t, "You can't defer something at global scope.");
				return false;
			}
			++t->token;
			s->kind = STMT_DEFER;
			Token *deferred_start = t->token;
			s->defer = parser_malloc(p, sizeof *s->defer);
			if (!parse_stmt(p, s->defer, was_a_statement))
				return false;
			if (!*was_a_statement) {
				err_print(token_location(p->file, deferred_start), "Empty defer");
				return false;
			}
			goto success;
		}
		case KW_USE: {
			++t->token;
			s->kind = STMT_USE;
			s->use = parser_malloc(p, sizeof *s->use);
			if (!parse_expr(p, &s->use->expr, expr_find_end(p, 0)))
				return false;
			goto success;
		}
		default: break;
		}
	} else if (t->token->kind == TOKEN_DIRECT) {
		switch (t->token->direct) {
		case DIRECT_INCLUDE: {
			Include *i = s->inc = parser_malloc(p, sizeof *i);
			++t->token;
			s->kind = STMT_INCLUDE;
			i->flags = 0;
			if (token_is_direct(t->token, DIRECT_FORCE)) {
				i->flags |= INC_FORCED;
				++t->token;
			}
			if (!parse_expr(p, &i->filename, expr_find_end(p, EXPR_CAN_END_WITH_COMMA)))
				return false;
			if (token_is_kw(t->token, KW_COMMA)) {
				Expression filename = i->filename;
				++t->token;
				if (t->token->kind != TOKEN_IDENT) {
					tokr_err(t, "Expected identifier for #include name (after comma).");
					tokr_skip_semicolon(t);
					return false;
				}
				Identifier ident = parser_ident_insert(p, t->token->ident);
				++t->token;
				/* this isn't actually a STMT_INCLUDE, but a STMT_DECL! */
				/* replace #include "io.toc", io => io ::= nms { #include "io.toc"; } */
				s->kind = STMT_DECL;
				Declaration *d = s->decl = parser_calloc(p, 1, sizeof *d);
				d->where = s->where;
				parser_put_end(p, &d->where); /* we haven't set s->where.end, so... */
				d->flags |= DECL_HAS_EXPR|DECL_IS_CONST;
				*(Identifier *)parser_arr_add(p, &d->idents) = ident;
				
				if (!check_ident_redecl(p, ident)) {
					tokr_skip_semicolon(t);
					return false;
				}
				ident->decl_kind = IDECL_DECL;
				ident->decl = d;
			
				Expression *e = &d->expr;
				e->kind = EXPR_NMS;
				e->nms = parser_calloc(p, 1, sizeof *e->nms);
				e->where = s->where;
				e->nms->associated_ident = ident;
				Block *body = &e->nms->body;
				body->kind = BLOCK_NMS;
				body->where = s->where;
				body->parent = p->block;
				idents_create(&body->idents, p->allocr, body);
				Statement *inc_stmt = parser_arr_add(p, &body->stmts);
				inc_stmt->kind = STMT_INCLUDE;
				inc_stmt->flags = STMT_INC_TO_NMS;
				inc_stmt->where = s->where;
				inc_stmt->inc = parser_calloc(p, 1, sizeof *inc_stmt->inc);
				inc_stmt->inc->filename = filename;
			}
			if (!token_is_kw(t->token, KW_SEMICOLON)) {
				tokr_err(t, "Expected ; after #include directive");
				tokr_skip_semicolon(t);
				return false;
			}
			++t->token;
			goto success;
		} break;
		case DIRECT_ERROR:
		case DIRECT_WARN:
		case DIRECT_INFO: {
			MessageKind kind;
			if (t->token->direct == DIRECT_ERROR) {
				kind = MESSAGE_ERROR;
			} else if (t->token->direct == DIRECT_WARN) {
				kind = MESSAGE_WARN;
			} else {
				kind = MESSAGE_INFO;
			}
			++t->token;
			s->kind = STMT_MESSAGE;
			Message *m = s->message = parser_malloc(p, sizeof *m);
			m->kind = kind;
			if (!parse_expr(p, &m->text, expr_find_end(p, 0))) {
				tokr_skip_semicolon(t);
				return false;
			}
			if (!token_is_kw(t->token, KW_SEMICOLON)) {
				tokr_err(t, "Expected ; at end of statement.");
				tokr_skip_semicolon(t);
				return false;
			}
		    goto success;
		}
		default:
			break;
		}
	}
	if (is_decl(t)) {
		s->kind = STMT_DECL;
		if (!parse_decl(p, s->decl = parser_malloc(p, sizeof *s->decl), DECL_END_SEMICOLON, PARSE_DECL_ALLOW_EXPORT)) {
			return false;
		}
	} else {
		s->kind = STMT_EXPR;
		Token *end = expr_find_end(p, 0);
		if (!end) {
			tokr_err(t, "No semicolon found at end of statement.");
			tokr_skip_to_eof(t);
			return false;
		}
		bool valid = parse_expr(p, s->expr = parser_malloc(p, sizeof *s->expr), end);
		
		/* go past end of expr regardless of whether successful or not */
		if (token_is_kw(end, KW_SEMICOLON)) {
			t->token = end + 1;	/* skip ; */
		} else  {
			s->flags |= STMT_EXPR_NO_SEMICOLON;
			t->token = end;
		}
		
		if (!valid) return false;
	}
 success:
	parser_put_end(p, &s->where);
	return true;
}

static void parser_create(Parser *p, Identifiers *globals, Tokenizer *t, Allocator *allocr) {
	p->tokr = t;
	p->block = NULL;
	p->globals = globals;
	p->allocr = allocr;
}

static Status parse_file(Parser *p, ParsedFile *f) {
	Tokenizer *t = p->tokr;
	f->stmts = NULL;
	p->file = t->file;
	p->parsed_file = f;
	bool ret = true;
	while (t->token->kind != TOKEN_EOF) {
		bool was_a_statement;
		Statement *stmt = parser_arr_add(p, &f->stmts);
		if (!parse_stmt(p, stmt, &was_a_statement))
			ret = false;
		if (!was_a_statement)
			arr_remove_lasta(&f->stmts, p->allocr);
		if (token_is_kw(t->token, KW_RBRACE)) {
			tokr_err(t, "} without a matching {.");
			return false;
		}
	}
	return ret;
}

#define PARSE_PRINT_LOCATION(l)  /* fprintf(out, "[%lu:%lu]", (unsigned long)(l).line, (unsigned long)(l).pos); */

static void fprint_expr(FILE *out, Expression *e);
static void fprint_stmt(FILE *out, Statement *s);
static void fprint_decl(FILE *out, Declaration *d);

static void fprint_type(FILE *out, Type *t) {
	PARSE_PRINT_LOCATION(t->where);
	char *s = type_to_str(t);
	fprintf(out, "%s", s);
	free(s);
}

static void print_type(Type *t) {
	fprint_type(stdout, t);
	printf("\n");
}


static void fprint_block(FILE *out,  Block *b) {
	fprintf(out, "{\n");
	arr_foreach(b->stmts, Statement, stmt) {
		fprint_stmt(out, stmt);
	}
	fprintf(out, "}");
	if (b->ret_expr) {
		fprintf(out, " returns ");
		fprint_expr(out, b->ret_expr);
	}
	
}

static void print_block(Block *b) {
	if (b) {
		fprint_block(stdout, b);
		printf("\n");
	} else {
		printf("(null block)\n");
	}
}

static void print_block_location(Block *b) {
	if (b)
		print_location(b->where);
	else
		printf("(global scope)\n");
}

static void fprint_fn_expr(FILE *out, FnExpr *f) {
	fprintf(out, "fn (");
	arr_foreach(f->params, Declaration, decl) {
		if (decl != f->params)
			fprintf(out, ", ");
		fprint_decl(out, decl);
	}
	fprintf(out, ") ");
	fprint_type(out, &f->ret_type);
	fprintf(out, " ");
	fprint_block(out, &f->body);
}

static void fprint_args(FILE *out, Argument *args) {
	fprintf(out, "(");
	arr_foreach(args, Argument, arg) {
		if (arg != args) fprintf(out, ", ");
		if (arg->name) {
			fprint_ident_str(out, arg->name);
			fprintf(out, " = ");
		}
		fprint_expr(out, &arg->val);
	}
	fprintf(out, ")");
}

static void fprint_arg_exprs(FILE *out, Expression *args) {
	fprintf(out, "(");
	arr_foreach(args, Expression, arg) {
		if (arg != args) fprintf(out, ", ");
		fprint_expr(out, arg);
	}
	fprintf(out, ")");
}

static inline void fprint_nms(FILE *out, Namespace *nms) {
	fprintf(out, "namespace ");
	fprint_block(out, &nms->body);
}

static void fprint_val(FILE *f, Value v, Type *t);

static void fprint_expr(FILE *out, Expression *e) {
	PARSE_PRINT_LOCATION(e->where);
	bool found_type = (e->flags & EXPR_FOUND_TYPE) != 0;
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
	case EXPR_LITERAL_BOOL:
		fprintf(out, "%s", e->booll ? "true" : "false");
		break;
	case EXPR_LITERAL_CHAR:
		fprint_char_literal(out, e->charl);
		break;
	case EXPR_IDENT:
		fprint_ident_debug(out, e->ident);
		break;
	case EXPR_BINARY_OP: {
		fprintf(out, "(");
		fprint_expr(out, e->binary.lhs);
		fprintf(out, ")%s(", binary_op_to_str(e->binary.op));
		fprint_expr(out, e->binary.rhs);
		fprintf(out, ")");
	} break;
	case EXPR_UNARY_OP:
		fprintf(out, "%s", unary_op_to_str(e->unary.op));
		fprintf(out, "(");
		fprint_expr(out, e->unary.of);
		fprintf(out, ")");
		break;
	case EXPR_FN:
		fprint_fn_expr(out, e->fn);
		break;
	case EXPR_CAST:
		fprintf(out, "cast(");
		fprint_expr(out, e->cast.expr);
		fprintf(out, ", ");
		fprint_type(out, &e->cast.type);
		fprintf(out, ")");
		break;
	case EXPR_IF: {
		IfExpr *i = e->if_;
		if (i->cond) {
			fprintf(out, "(else)? if ");
			fprint_expr(out, i->cond);
		} else {
			fprintf(out, "else");
		}
		fprint_block(out, &i->body);
		if (i->next_elif)
			fprint_expr(out, i->next_elif);
	} break;
	case EXPR_WHILE: {
		WhileExpr *w = e->while_;
		fprintf(out, "while ");
		if (w->cond) fprint_expr(out, w->cond);
		fprint_block(out, &w->body);
	} break;
	case EXPR_FOR: {
		ForExpr *fo = e->for_;
		fprintf(out, "for ");
		if (fo->index) {
			fprint_ident_debug(out, fo->index);
		} else fprintf(out, "_");
		fprintf(out, ", ");
		if (fo->value) {
			fprint_ident_debug(out, fo->value);
		} else fprintf(out, "_");
		fprintf(out, " :");
		if (fo->flags & FOR_ANNOTATED_TYPE)
			fprint_type(out, &fo->type);
		fprintf(out, "= ");
		if (fo->flags & FOR_IS_RANGE) {
			fprint_expr(out, fo->range.from);
			if (found_type) {
				if (fo->range.stepval) {
					fprintf(out, ",");
					fprint_val(out, *fo->range.stepval, &fo->type);
				}
			} else {
				if (fo->range.step) {
					fprintf(out, ",");
					fprint_expr(out, fo->range.step);
				}
			}
			fprintf(out, "..");
			if (fo->range.to) {
				fprint_expr(out, fo->range.to);
			}
			fprintf(out, " ");
		} else {
			fprint_expr(out, fo->of);
		}
		fprint_block(out, &fo->body);
	} break;
	case EXPR_CALL:
		fprint_expr(out, e->call.fn);
		if (found_type) {
			fprint_arg_exprs(out, e->call.arg_exprs);
		} else {
			fprint_args(out, e->call.args);
		}
		break;
	case EXPR_BLOCK:
		fprint_block(out, e->block);
		break;
	case EXPR_TUPLE:
		fprintf(out, "(");
		arr_foreach(e->tuple, Expression, x) {
			if (x != e->tuple) fprintf(out, ", ");
			fprint_expr(out, x);
		}
		fprintf(out, ")");
		break;
	case EXPR_C:
		fprintf(out, "#C(");
		fprint_expr(out, e->c.code);
		fprintf(out, ")");
		break;
	case EXPR_BUILTIN:
		fprintf(out, "#builtin(");
		if (found_type) {
			fprintf(out, "%s", builtin_val_names[e->builtin.which.val]);
		} else {
			fprint_expr(out, e->builtin.which.expr);
		}
		fprintf(out, ")");
		break;
	case EXPR_SLICE: {
		SliceExpr *s = &e->slice;
		fprint_expr(out, s->of);
		fprintf(out, "[");
		if (s->from) fprint_expr(out, s->from);
		fprintf(out, ":");
		if (s->to) fprint_expr(out, s->to);
		fprintf(out, "]");
	} break;
	case EXPR_TYPE:
		fprint_type(out, e->typeval);
		break;
	case EXPR_VAL:
		fprint_val(out, e->val, &e->type);
		break;
	case EXPR_NMS:
		fprint_nms(out, e->nms);
		break;
	}
	if (found_type) {
		fprintf(out, ":");
		fprint_type(out, &e->type);
	}
}

static void print_expr(Expression *e) {
	fprint_expr(stdout, e);
	printf("\n");
}

static void fprint_decl(FILE *out, Declaration *d) {
	PARSE_PRINT_LOCATION(d->where);
	arr_foreach(d->idents, Identifier, ident) {
		if (ident != d->idents) fprintf(out, ", ");
		fprint_ident_debug(out, *ident);
	}
	if (d->flags & DECL_IS_CONST) {
		fprintf(out, "::");
	} else if (d->flags & DECL_SEMI_CONST) {
		fprintf(out, ":::");
	} else {
		fprintf(out, ":");
	}
	if ((d->flags & DECL_FOUND_TYPE) || (d->flags & DECL_ANNOTATES_TYPE)) {
		fprint_type(out, &d->type);
	}
	if (d->flags & DECL_HAS_EXPR) {
		fprintf(out, "=");
		fprint_expr(out, &d->expr);
	}
	if (d->flags & DECL_FOUND_VAL) {
		fprintf(out, "(");
		fprint_val(out, d->val, &d->type);
		fprintf(out, ")");
	}
}

static void print_decl(Declaration *d) {
	fprint_decl(stdout, d);
	printf("\n");
}

static void fprint_stmt(FILE *out, Statement *s) {
	PARSE_PRINT_LOCATION(s->where);
	switch (s->kind) {
	case STMT_DECL:
		fprint_decl(out, s->decl);
		fprintf(out, ";\n");
		break;
	case STMT_EXPR:
		fprint_expr(out, s->expr);
		fprintf(out, ";\n");
		break;
	case STMT_RET: {
		Return *r = s->ret;
		fprintf(out, "return ");
		if (r->flags & RET_HAS_EXPR)
			fprint_expr(out, &r->expr);
		fprintf(out, ";\n");
	} break;
	case STMT_INCLUDE: {
		Include *i = s->inc;
		if (s->flags & STMT_TYPED) {
			arr_foreach(i->stmts, Statement, sub)
				fprint_stmt(out, sub);
		} else {
			fprintf(out, "#include ");
			fprint_expr(out, &i->filename);
			fprintf(out, ";\n");
		}
	} break;
	case STMT_MESSAGE: {
		Message *m = s->message;
		switch (m->kind) {
		case MESSAGE_ERROR:
			fprintf(out, "#error ");
			break;
		case MESSAGE_WARN:
			fprintf(out, "#warn ");
			break;
		case MESSAGE_INFO:
			fprintf(out, "#info ");
			break;
		}
	} break;
	case STMT_BREAK:
		fprintf(out, "break;");
		break;
	case STMT_CONT:
		fprintf(out, "continue;");
		break;
	case STMT_DEFER:
		fprintf(out, "defer ");
		fprint_stmt(out, s->defer);
		break;
	case STMT_USE:
		fprintf(out, "use ");
		fprint_expr(out, &s->use->expr);
		break;
	}
}

static void print_stmt(Statement *s) {
	fprint_stmt(stdout, s);
	printf("\n");
}

static void fprint_parsed_file(FILE *out, ParsedFile *f) {
	arr_foreach(f->stmts, Statement, stmt) {
		fprint_stmt(out, stmt);
	}
}

static int decl_ident_index(Declaration *d, Identifier i) {
	int idx = 0;
	arr_foreach(d->idents, Identifier, j) {
		if (i == *j)
			return idx;
		++idx;
	}
	return -1;
}

static inline Value *decl_val_at_index(Declaration *d, int i) {
	return d->type.kind == TYPE_TUPLE ? &d->val.tuple[i] : &d->val;
}

static inline Type *decl_type_at_index(Declaration *d, int i) {
	if (d->type.kind == TYPE_TUPLE)
		assert(i < (int)arr_len(d->type.tuple));
	Type *ret = d->type.kind == TYPE_TUPLE ? &d->type.tuple[i] : &d->type;
	assert(ret->kind != TYPE_TUPLE);
	return ret;
}

static bool ident_is_definitely_const(Identifier i) {
	Declaration *decl = i->decl;
	if (i->decl_kind != IDECL_DECL || !(decl->flags & DECL_IS_CONST))
		return false;
	
	return true;
}


static bool expr_is_definitely_const(Expression *e) {
	switch (e->kind) {
	case EXPR_LITERAL_FLOAT:
	case EXPR_LITERAL_INT:
	case EXPR_LITERAL_CHAR:
	case EXPR_LITERAL_STR:
	case EXPR_LITERAL_BOOL:
	case EXPR_TYPE:
	case EXPR_VAL:
	case EXPR_NMS:
		return true;
	case EXPR_IF:
	case EXPR_WHILE:
	case EXPR_C:
	case EXPR_BUILTIN:
	case EXPR_CAST:
	case EXPR_CALL:
	case EXPR_BLOCK:
	case EXPR_TUPLE:
	case EXPR_FOR:
	case EXPR_FN:
		return false;
	case EXPR_UNARY_OP:
		return expr_is_definitely_const(e->unary.of);
	case EXPR_BINARY_OP:
		if (e->binary.op == BINARY_DOT) {
			if (!expr_is_definitely_const(e->binary.lhs))
				return false;
			Type *lhs_type = &e->binary.lhs->type;
			if (lhs_type->kind == TYPE_PTR) lhs_type = lhs_type->ptr;
			return true;
		}
		return expr_is_definitely_const(e->binary.lhs)
			&& expr_is_definitely_const(e->binary.rhs);
	case EXPR_SLICE:
		return expr_is_definitely_const(e->slice.of);
	case EXPR_IDENT:
		return ident_is_definitely_const(e->ident);
	}
	assert(0);
	return false;
}

