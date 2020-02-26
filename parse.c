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
	  PARSE_DECL_ALLOW_EXPORT = 0x08
};
static Status parse_decl(Parser *p, Declaration *d, DeclEndKind ends_with, uint16_t flags);
static Status parse_decl_list(Parser *p, Declaration **decls, DeclEndKind decl_end);

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
	case EXPR_NEW: return "new expression";
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
	case UNARY_DEL: return "del";
	case UNARY_LEN: return "len";
	case UNARY_DSIZEOF: return "#sizeof";
	case UNARY_DALIGNOF: return "#alignof";
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
		size_t written = str_copy(buffer, bufsize, "<");
		arr_foreach(t->tuple, Type, child) {
			if (child != t->tuple)
				written += str_copy(buffer + written, bufsize - written, ", ");
			written += type_to_str_(child, buffer + written, bufsize - written);
		}
		written += str_copy(buffer + written, bufsize - written, ">");
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

static inline Location parser_mk_loc(Parser *p) {
	Location loc = {0};
	loc.file = p->file;
	return loc;
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
			  /* note that parse_type uses -1 for this */
} ExprEndFlags;

static Token *expr_find_end(Parser *p, ExprEndFlags flags)  {
	Tokenizer *t = p->tokr;
	int paren_level = 0;
	int brace_level = 0;
	int square_level = 0;
	Token *token = t->token;
	bool could_be_vbs = false; /* could this be a void block statement (whose semicolons can be omitted)? e.g. {x := 5;} */
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
				could_be_vbs = true;
				break;
			case KW_RBRACE:
				--brace_level;
				if (paren_level == 0 && brace_level == 0 && square_level == 0
					&& could_be_vbs && !token_is_kw(token + 1, KW_RPAREN)) {
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
				could_be_vbs = true;
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
			if (token->kw != KW_RBRACE && token->kw != KW_SEMICOLON && token->kw != KW_LBRACE)
				could_be_vbs = false;
		} else {
			could_be_vbs = false;
		}
		if (token->kind == TOKEN_EOF) {
			if (brace_level > 0) {
				tokr_err(t, "Opening brace { was never closed."); /* FEATURE: Find out where this is */
			} else if (paren_level > 0) {
				tokr_err(t, "Opening parenthesis ( was never closed.");
			} else if (square_level > 0) {
				tokr_err(t, "Opening square bracket [ was never closed.");
			} else {
				tokr_err(t, "Could not find end of expression (did you forget a semicolon?).");
				/* FEATURE: improve err message */
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
			arg->where.start = t->token;
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
			arg->where.end = t->token;
			
			if (token_is_kw(t->token, KW_RPAREN))
				break;
			assert(token_is_kw(t->token, KW_COMMA));
			++t->token;	/* move past , */
		}
	}
	++t->token;	/* move past ) */
	return true;
}

static Status parse_type(Parser *p, Type *type) {
	Tokenizer *t = p->tokr;
	type->where = parser_mk_loc(p);
	type->where.start = t->token;
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
					if (!parse_type(p, param_type)) return false;
					if (param_type->kind == TYPE_TUPLE) {
						err_print(param_type->where, "Functions cannot have tuples as parameters.");
						return false;
					}
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
				if (!parse_type(p, ret_type))
					return false;
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
				if (!parse_type(p, type->slice)) return false;
				if (type->slice->kind == TYPE_TUPLE) {
					err_print(type->where, "You cannot have a slice of tuples.");
					return false;
				}
				break;
			}
			Token *end = expr_find_end(p, 0);
			type->arr.n_expr = parser_new_expr(p);
			if (!parse_expr(p, type->arr.n_expr, end)) return false;
			t->token = end + 1;	/* go past ] */
			type->arr.of = parser_malloc(p, sizeof *type->arr.of);
			if (!parse_type(p, type->arr.of)) return false;
			if (type->arr.of->kind == TYPE_TUPLE) {
				err_print(type->where, "You cannot have an array of tuples.");
				return false;
			}
		} break;
		case KW_LT:
			/* tuple! */
			type->kind = TYPE_TUPLE;
			type->tuple = NULL;
			++t->token;	/* move past < */
			while (1) {
				Type *child = parser_arr_add(p, &type->tuple);
				if (!parse_type(p, child)) return false;
				if (child->kind == TYPE_TUPLE) {
					err_print(child->where, "Tuples cannot contain tuples.");
					return false;
				}
				if (token_is_kw(t->token, KW_GT)) { /* we're done with the tuple */
					++t->token;	/* move past > */
					break;
				}
				if (token_is_kw(t->token, KW_COMMA)) {
					++t->token;	/* move past , */
					continue;
				} else {
					tokr_err(t, "Expected , to list next tuple type or ) to end tuple type.");
					return false;
				}
			}
			break;
		case KW_AMPERSAND:
			/* pointer */
			type->kind = TYPE_PTR;
			type->ptr = parser_malloc(p, sizeof *type->ptr);
			++t->token;	/* move past & */
			if (!parse_type(p, type->ptr)) return false;
			if (type->ptr->kind == TYPE_TUPLE) {
				err_print(type->ptr->where, "You cannot have a pointer to a tuple.");
				return false;
			}
			break;
		case KW_STRUCT: {
			/* struct */
			type->kind = TYPE_STRUCT;
			StructDef *struc = type->struc = parser_malloc(p, sizeof *type->struc);
			struc->flags = 0;
			struc->name = NULL;
			/* help cgen out */
			struc->c.id = 0;
			struc->fields = NULL;
			struc->params = NULL;
			struc->where = parser_mk_loc(p);
			struc->where.start = t->token;
			memset(&struc->scope, 0, sizeof struc->scope);
			idents_create(&struc->scope.idents, p->allocr, &struc->scope);
		    memset(&struc->instances, 0, sizeof struc->instances);
			
			Block *prev_block = p->block;
			p->block = &struc->scope;
			
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
					if (param->flags & DECL_INFER) {
						/* TODO(eventually) */
						err_print(param->where, "Struct parameters cannot be inferred (yet).");
						goto struct_fail;
					}
					param->flags |= DECL_IS_PARAM;
				}
			}
			if (!token_is_kw(t->token, KW_LBRACE)) {
				tokr_err(t, "Expected { to follow struct.");
				goto struct_fail;
			}
			++t->token;
			{
				while (!token_is_kw(t->token, KW_RBRACE)) {
					Declaration field_decl;
					if (!parse_decl(p, &field_decl, DECL_END_SEMICOLON, 0)) {
						goto struct_fail;
					}
					if (field_decl.flags & DECL_IS_CONST) {
						/* TODO */
						err_print(field_decl.where, "Constant struct members are not supported (yet).");
						goto struct_fail;
					}
					if ((field_decl.flags & DECL_FOREIGN) && !(field_decl.flags & DECL_IS_CONST)) {
						err_print(field_decl.where, "Non-constant struct members cannot be foreign.");
						goto struct_fail;
					}
					
					if (field_decl.flags & DECL_HAS_EXPR) {
						err_print(field_decl.where, "struct members cannot have initializers.");
						goto struct_fail;
					}
					long idx = 0;
					arr_foreach(field_decl.idents, Identifier, fident) {
						Type *ftype = field_decl.type.kind == TYPE_TUPLE ? &field_decl.type.tuple[idx] : &field_decl.type;
						Field *f = parser_arr_add(p, &struc->fields);
						f->name = *fident;
						f->where = field_decl.where;
						f->type = *ftype;
						++idx;
					}
				}
				++t->token;
				struc->where.end = t->token;
			}
			p->block = prev_block;
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
	type->where.end = t->token;
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
			case KW_LT: {
				/* no expression can start with < */
				return true;
			} break;
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
								if (!parse_type(p, &return_type)) {
									/* couldn't parse a return type. this shouldn't happen in theory. */
									assert(0);
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

enum {
	  PARSE_BLOCK_DONT_CREATE_IDENTS = 0x01
};
static Status parse_block(Parser *p, Block *b, U8 flags) {
	Tokenizer *t = p->tokr;
	Block *prev_block = p->block;
	b->flags = 0;
	b->ret_expr = NULL;
	p->block = b;
	if (!(flags & PARSE_BLOCK_DONT_CREATE_IDENTS))
		idents_create(&b->idents, p->allocr, p->block);
	if (!token_is_kw(t->token, KW_LBRACE)) {
		tokr_err(t, "Expected '{' to open block.");
		return false;
	}
	b->where = parser_mk_loc(p);
	b->where.start = t->token;
#ifdef TOC_DEBUG
	b->where.end = t->token + 1;
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
	b->where.end = t->token;
	++t->token;	/* move past } */
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
	f->ret_decls = NULL;
	{
		/* help types.c */
		HashTable z = {0};
		f->instances = z;
	}
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
	p->block = &f->body;
	idents_create(&f->body.idents, p->allocr, &f->body);
	if (token_is_kw(t->token, KW_RPAREN)) {
		++t->token;
	} else {
		if (!parse_decl_list(p, &f->params, DECL_END_RPAREN_COMMA))
			return false;
		arr_foreach(f->params, Declaration, param) {
			if (param->flags & DECL_FOREIGN) {
				err_print(param->where, "Parameters cannot be foreign.");
				success = false;
				goto ret;
			}
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
			if (d->flags & DECL_FOREIGN) {
				err_print(d->where, "Named return values can't be foreign.");
			    success = false; goto ret;
			}
		}
		--t->token;	/* move back to { */
		/* just set return type to void. the actual return type will be set by types.c:type_of_fn */
		f->ret_type.kind = TYPE_VOID;
		f->ret_type.flags = 0;
	} else {
		if (!parse_type(p, &f->ret_type)) {
			success = false;
			goto ret;
		}
	}
	if (!parse_block(p, &f->body, PARSE_BLOCK_DONT_CREATE_IDENTS))
		success = false;
 ret:
	f->body.flags |= BLOCK_IS_FN;
	p->block = prev_block;
	return success;
}

static void fprint_expr(FILE *out, Expression *e);


#define NOT_AN_OP -1
/* cast/new aren't really operators since they operate on types, not exprs. */
#define CAST_PRECEDENCE 2
#define NEW_PRECEDENCE 22
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
	case KW_PLUS: return 10;
	case KW_MINUS: return 20;
	case KW_AMPERSAND: return 25;
	case KW_ASTERISK: return 30;
	case KW_SLASH: return 40;
	case KW_PERCENT: return 45;
	case KW_EXCLAMATION: return 50;
	case KW_DEL: return 1000;
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
	if (i->idents == &p->block->idents) {
		if (i->decl_kind != IDECL_NONE) {
			char *s = ident_to_str(i);
			tokr_err(t, "Redeclaration of identifier %s.", s);
			info_print(ident_decl_location(i), "Previous declaration was here.");
			free(s);
			return false;
		}
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
	e->where.start = t->token;
	if (end <= t->token) {
		tokr_err(t, "Empty expression.");
		return false;
	}
	{
		Token *before = t->token;
		if (parser_is_definitely_type(p, NULL)) {
			/* it's a type! */
			e->kind = EXPR_TYPE;
			if (!parse_type(p, &e->typeval))
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
		/* TODO: consider moving this after ops, so that "if true { 5 } else { 3 } as f32" is possible */
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
				Namespace *n = &e->nms;
				e->kind = EXPR_NMS;
				++t->token;
				if (!parse_block(p, &n->body, 0))
					return false;
 				goto success;
			}
			case KW_IF: {
				IfExpr *i = &e->if_;
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
					next->where.start = t->token;
					curr->next_elif = next;
					IfExpr *nexti = &next->if_;
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
					next->where.end = t->token;
					curr = nexti;
				}
				goto success;
			}
			case KW_WHILE: {
				e->kind = EXPR_WHILE;
				WhileExpr *w = &e->while_;
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
						fo->value->decl_kind = IDECL_EXPR;
						fo->value->decl_expr = e;
						if (ident_eq_str(fo->value, "_")) /* ignore value */
							fo->value = NULL;
						++t->token;
						if (token_is_kw(t->token, KW_COMMA)) {
							++t->token;
							if (t->token->kind == TOKEN_IDENT) {
								fo->index = parser_ident_insert(p, t->token->ident);
								if (!check_ident_redecl(p, fo->index))
									goto for_fail;
								fo->index->decl_kind = IDECL_EXPR;
								fo->index->decl_expr = e;
								if (ident_eq_str(fo->index, "_")) /* ignore index */
									fo->index = NULL;
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
						if (!parse_type(p, &fo->type))
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
				e->where.end = t->token; /* temporarily set end so that redeclaration errors aren't messed up */
				if (!parse_block(p, &fo->body, PARSE_BLOCK_DONT_CREATE_IDENTS))
				    goto for_fail;
				p->block = prev_block;
				goto success;
				for_fail:
				p->block = prev_block;
				return false;
			}
			default: break;
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
					if (paren_level == 0 && brace_level == 0 && square_level == 0)
						dot = token;
					break;
				default: { /* OPTIM: use individual cases for each op */
					if (paren_level == 0 && brace_level == 0 && square_level == 0) {
						int precedence;
						switch (token->kw) {
						case KW_AS: precedence = CAST_PRECEDENCE; break;
						case KW_NEW: precedence = NEW_PRECEDENCE; break;
						default: precedence = op_precedence(token->kw); break;
						}
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
			if (!parse_expr(p, e, new_end))
				return false;
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
				case KW_NEW:
					e->kind = EXPR_NEW;
					++t->token;
					if (!token_is_kw(t->token, KW_LPAREN)) {
						tokr_err(t, "Expected ( to follow new.");
						return false;
					}
					++t->token;
					if (!parse_type(p, &e->new.type)) return false;
					if (token_is_kw(t->token, KW_COMMA)) {
						/* new(int, 5) */
						++t->token;
						Token *n_end = expr_find_end(p, 0);
						e->new.n = parser_new_expr(p);
						if (!parse_expr(p, e->new.n, n_end))
							return false;
					} else e->new.n = NULL;
					if (!token_is_kw(t->token, KW_RPAREN)) {
						tokr_err(t, "Expected ).");
						return false;
					}
					++t->token;
					if (e->new.type.kind == TYPE_TUPLE) {
						err_print(e->where, "You cannot new a tuple.");
						return false;
					}
					if (t->token == end)
						goto success;
					/* otherwise, there's more stuff after the new (e.g. new(int, 5).len)*/
					t->token = start;
					goto not_an_op;
				case KW_DEL:
					if (!token_is_kw(t->token + 1, KW_LPAREN)) {
						/* for the future, when del could be a function */
						err_print(e->where, "Expected ( after del.");
						return false;
					}
					op = UNARY_DEL;
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
				if (!parse_type(p, &e->cast.type))
					return false;
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
			case KW_AMPERSAND:
			case KW_EXCLAMATION:
			case KW_DEL:
				err_print(token_location(p->file, lowest_precedence_op), "Unary operator '%s' being used as a binary operator!", kw_to_str(lowest_precedence_op->kw));
				return false;
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
			goto success;
		} else {
		not_an_op:;
			/* function calls, array accesses, etc. */
		
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

			if (t->token->kind == TOKEN_DIRECT) {
				/* it's a directive */
				Expression *single_arg = NULL; /* points to an expr if this is a directive with one expression argument */
			
				switch (t->token->direct) {
				case DIRECT_C:
					e->kind = EXPR_C;
					single_arg = e->c.code = parser_new_expr(p);
					break;
				case DIRECT_BUILTIN:
					e->kind = EXPR_BUILTIN;
					single_arg = e->builtin.which.expr = parser_new_expr(p);
					break;
				case DIRECT_SIZEOF:
					e->kind = EXPR_UNARY_OP;
					e->unary.op = UNARY_DSIZEOF;
					single_arg = e->unary.of = parser_new_expr(p);
					break;
				case DIRECT_ALIGNOF:
					e->kind = EXPR_UNARY_OP;
					e->unary.op = UNARY_DALIGNOF;
					single_arg = e->unary.of = parser_new_expr(p);
					break;
				case DIRECT_FOREIGN:
				case DIRECT_EXPORT:
				case DIRECT_INCLUDE:
					tokr_err(t, "Unrecognized expression.");
					return false;
				case DIRECT_COUNT: assert(0); break;
				}
				if (single_arg) {
					++t->token;
					if (!token_is_kw(t->token, KW_LPAREN)) {
						tokr_err(t, "Expected ( to follow #%s.", directives[t->token->direct]);
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

			if (token_is_kw(t->token, KW_LBRACE)) {
				/* it's a block */
				e->kind = EXPR_BLOCK;
				if (!parse_block(p, &e->block, 0)) return false;
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
	e->where.end = t->token;
	assert(t->token == end);

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
	d->where.start = t->token;
	d->idents = NULL;
	d->flags = 0;
	d->val_stack = NULL;
	
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
		if (!ident_eq_str(*ident, "_")) {
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
			Type type;
			if (!parse_type(p, &type)) {
				goto ret_false;
			}
			d->type = type;
			if (type.kind == TYPE_TUPLE && arr_len(d->type.tuple) != arr_len(d->idents)) {
				err_print(type.where, "Expected to have %lu things declared in declaration, but got %lu.", (unsigned long)arr_len(d->type.tuple), (unsigned long)arr_len(d->idents));
				goto ret_false;
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
			if (token_is_direct(t->token, DIRECT_FOREIGN)) {
				if (!(d->flags & DECL_ANNOTATES_TYPE)) {
					tokr_err(t, "Foreign declaration must have a type.");
					goto ret_false;
				}
				d->flags |= DECL_FOREIGN;
				/* foreign name */
				++t->token;
				d->foreign.name = parser_new_expr(p);
				if (!parse_expr(p, d->foreign.name, expr_find_end(p, EXPR_CAN_END_WITH_COMMA))) {
					goto ret_false;
				}
				d->foreign.lib = NULL;
				if (!ends_decl(t->token, ends_with)) {
					if (!token_is_kw(t->token, KW_COMMA)) {
						tokr_err(t, "Expected comma, followed by foreign library.");
						goto ret_false;
					}
					++t->token;
					/* foreign library */
					d->foreign.lib = parser_new_expr(p);
					if (!parse_expr(p, d->foreign.lib, expr_find_end(p, 0))) {
						goto ret_false;
					}
				}
				
				if (!ends_decl(t->token, ends_with)) {
					tokr_err(t, "Expected declaration to stop after #foreign, but it continues.");
					goto ret_false;
				}
				++t->token;
				
			} else if ((flags & PARSE_DECL_ALLOW_INFER) && ends_decl(t->token, ends_with)) {
				/* inferred expression */
				d->flags |= DECL_INFER;
				if (!(d->flags & DECL_IS_CONST)) {
					tokr_err(t, "Inferred parameters must be constant.");
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
				Token *end = expr_find_end(p, expr_flags);
				if (!end || !ends_decl(end, ends_with)) {
					if (end) t->token = end;
					tokr_err(t, "Expected %s at end of declaration.", end_str);
					goto ret_false;
				}
				if (!parse_expr(p, &d->expr, end)) {
					t->token = end; /* move to ; */
					goto ret_false;
				}
				if (ends_decl(t->token, ends_with)) {
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
	
	if ((d->flags & DECL_IS_CONST) && !(d->flags & (DECL_HAS_EXPR | DECL_FOREIGN)) && !(flags & PARSE_DECL_ALLOW_CONST_WITH_NO_EXPR)) {
		--t->token;
		/* disallowed constant without an expression, e.g. x :: int; */
		tokr_err(t, "You must have an expression at the end of this constant declaration.");
		goto ret_false;
	}
	
	d->where.end = t->token;
	switch (ends_with) {
	case DECL_END_RPAREN_COMMA:
	case DECL_END_LBRACE_COMMA:
		--d->where.end; /* don't include the ) / { as part of the declaration */
	case DECL_END_SEMICOLON: break;
	}
	
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
	s->where.start = t->token;
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
			s->ret.flags = 0;
			if (token_is_kw(t->token, KW_SEMICOLON)) {
				/* return with no expr */
				++t->token;
				goto success;
			}
			s->ret.flags |= RET_HAS_EXPR;
			Token *end = expr_find_end(p, 0);
			if (!end) {
				while (t->token->kind != TOKEN_EOF) ++t->token; /* move to end of file */
				return false;
			}
			if (!token_is_kw(end, KW_SEMICOLON)) {
				err_print(token_location(p->file, end), "Expected ';' at end of return statement.");
				t->token = end->kind == TOKEN_EOF ? end : end + 1;
				return false;
			}
			bool success = parse_expr(p, &s->ret.expr, end);
			t->token = end + 1;
			return success;
		}
		default: break;
		}
	} else if (t->token->kind == TOKEN_DIRECT) {
		switch (t->token->direct) {
		case DIRECT_INCLUDE: {
			++t->token;
			s->kind = STMT_INCLUDE;
			if (!parse_expr(p, &s->inc.filename, expr_find_end(p, 0)))
				return false;
			if (!token_is_kw(t->token, KW_SEMICOLON)) {
				tokr_err(t, "Expected ; after #include directive");
				return false;
			}
			++t->token;
			goto success;
		} break;
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
		bool valid = parse_expr(p, &s->expr, end);
		
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
	s->where.end = t->token;
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
	case EXPR_NEW:
		fprintf(out, "new ");
		fprint_type(out, &e->new.type);
		break;
	case EXPR_IF:
		if (e->if_.cond) {
			fprintf(out, "(else)? if ");
			fprint_expr(out, e->if_.cond);
		} else {
			fprintf(out, "else");
		}
		fprint_block(out, &e->if_.body);
		if (e->if_.next_elif)
			fprint_expr(out, e->if_.next_elif);
		break;
	case EXPR_WHILE:
		fprintf(out, "while ");
		if (e->while_.cond) fprint_expr(out, e->while_.cond);
		fprint_block(out, &e->while_.body);
		break;
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
		fprint_block(out, &e->block);
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
		fprint_type(out, &e->typeval);
		break;
	case EXPR_VAL:
		fprint_val(out, e->val, &e->type);
		break;
	case EXPR_NMS:
		fprint_nms(out, &e->nms);
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

static void fprint_stmt(FILE *out, Statement *s) {
	PARSE_PRINT_LOCATION(s->where);
	switch (s->kind) {
	case STMT_DECL:
		fprint_decl(out, s->decl);
		fprintf(out, ";\n");
		break;
	case STMT_EXPR:
		fprint_expr(out, &s->expr);
		fprintf(out, ";\n");
		break;
	case STMT_RET:
		fprintf(out, "return ");
		if (s->ret.flags & RET_HAS_EXPR)
			fprint_expr(out, &s->ret.expr);
		fprintf(out, ";\n");
		break;
	case STMT_INCLUDE:
		if (s->flags & STMT_TYPED) {
		    arr_foreach(s->inc.stmts, Statement, sub)
				fprint_stmt(out, sub);
		} else {
			fprintf(out, "#include ");
			fprint_expr(out, &s->inc.filename);
			fprintf(out, ";\n");
		}
		break;
	}
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
	if (i->decl->flags & DECL_FOREIGN) {
		if (decl->foreign.lib && COMPILE_TIME_FOREIGN_FN_SUPPORT)
			return true;
		else
			return false;
	} else {
		return true;
	}
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
	case EXPR_NEW:
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

