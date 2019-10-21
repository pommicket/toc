static bool cgen_stmt(CGenerator *g, Statement *s);
#define CGEN_BLOCK_FLAG_NOENTER 0x01 /* should cgen_block actually enter and exit the block? */
#define CGEN_BLOCK_FLAG_NOBRACES 0x02 /* should it use braces? */
static bool cgen_block(CGenerator *g, Block *b, const char *ret_name, uint16_t flags);
static bool cgen_expr_pre(CGenerator *g, Expression *e);
static bool cgen_expr(CGenerator *g, Expression *e);
static bool cgen_set_tuple(CGenerator *g, Expression *exprs, Identifier *idents, const char *prefix, Expression *to);
static bool cgen_type_pre(CGenerator *g, Type *t, Location where);
static bool cgen_type_post(CGenerator *g, Type *t, Location where);
static bool cgen_decl(CGenerator *g, Declaration *d);
static bool cgen_ret(CGenerator *g, Expression *ret);

static void cgen_create(CGenerator *g, FILE *out, Identifiers *ids, Evaluator *ev) {
	g->outc = out;
	g->ident_counter = 0;
	g->main_ident = ident_get(ids, "main");
	g->evalr = ev;
	g->will_indent = true;
	g->indent_lvl = 0;
	g->anon_fns = NULL;
}

static bool cgen_block_enter(CGenerator *g, Block *b) {
	g->block = b;
	Statement *stmts;
	if (b == NULL) {
		stmts = g->file->stmts;
	} else {
		stmts = b->stmts;
	}
	if (b) g->indent_lvl++;
	return block_enter(b, stmts);
}

static void cgen_block_exit(CGenerator *g, Block *into) {
	Block *b = g->block;
	Statement *stmts;
	if (b == NULL) {
		stmts = g->file->stmts;
	} else {
		stmts = b->stmts;
	}
	block_exit(b, stmts);
	if (b) g->indent_lvl--;
	g->block = into;
}

static inline FILE *cgen_writing_to(CGenerator *g) {
	return g->outc;	/* for now */
}

/* indent iff needed */
static inline void cgen_indent(CGenerator *g) {
	if (g->will_indent) {
		for (int i = 0; i < g->indent_lvl; i++)
			fprintf(cgen_writing_to(g), "\t");
		g->will_indent = false;
	}
}

static inline void cgen_write(CGenerator *g, const char *fmt, ...) {
	va_list args;
	cgen_indent(g);
	va_start(args, fmt);
	vfprintf(cgen_writing_to(g), fmt, args);
	va_end(args);
}

static inline void cgen_nl(CGenerator *g) {
	fprintf(cgen_writing_to(g), "\n");
	g->will_indent = true;
}

static inline void cgen_writeln(CGenerator *g, const char *fmt, ...) {
	va_list args;
	cgen_indent(g);
	va_start(args, fmt);
	vfprintf(cgen_writing_to(g), fmt, args);
	va_end(args);
	cgen_nl(g);
}

/* should declaration be a direct function declaration C (as opposed to using a function pointer or not being a function) */
static bool cgen_fn_is_direct(CGenerator *g, Declaration *d) {
	return g->block == NULL && (d->flags & DECL_FLAG_HAS_EXPR) && d->expr.kind == EXPR_FN && arr_len(d->idents) == 1;
}

static bool cgen_uses_ptr(Type *t) {
	switch (t->kind) {
	case TYPE_TUPLE:
	case TYPE_ARR:
		return true;
	case TYPE_BUILTIN:
	case TYPE_PTR:
	case TYPE_FN:
	case TYPE_SLICE:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
		return false;
	}
	assert(0);
	return false;
}

static void cgen_ident(CGenerator *g, Identifier i) {
	if (i == g->main_ident) {
		/* don't conflict with C's main! */
		cgen_write(g, "main__");
	} else {
		cgen_indent(g);
		IdentDecl *idecl = ident_decl(i);
		if (idecl->flags & IDECL_FLAG_CGEN_PTR)
			cgen_write(g, "(*");
		fprint_ident(cgen_writing_to(g), i);
		if (idecl->flags & IDECL_FLAG_CGEN_PTR)
			cgen_write(g, ")");
	}
}

static char *cgen_ident_to_str(Identifier i) {
	return ident_to_str(i);
}

static void cgen_ident_id(CGenerator *g, IdentID id) {
	cgen_write(g, "a%lu_", (unsigned long)id);
}

/* buffer should be at least 32 bytes */
static inline void cgen_ident_id_to_str(char *buffer, IdentID id) {
	snprintf(buffer, 32, "a%lu_", (unsigned long)id);
}

static bool cgen_type_post(CGenerator *g, Type *t, Location where);
static bool cgen_type_pre(CGenerator *g, Type *t, Location where) {
	switch (t->kind) {
	case TYPE_BUILTIN:
		switch (t->builtin) {
		case BUILTIN_I8: cgen_write(g, "i8"); break;
		case BUILTIN_I16: cgen_write(g, "i16"); break;
		case BUILTIN_I32: cgen_write(g, "i32"); break;
		case BUILTIN_I64: cgen_write(g, "i64"); break;
		case BUILTIN_U8: cgen_write(g, "u8"); break;
		case BUILTIN_U16: cgen_write(g, "u16"); break;
		case BUILTIN_U32: cgen_write(g, "u32"); break;
		case BUILTIN_U64: cgen_write(g, "u64"); break;
		case BUILTIN_CHAR: cgen_write(g, "char"); break;
		case BUILTIN_BOOL: cgen_write(g, "bool"); break;
		case BUILTIN_F32: cgen_write(g, "f32"); break;
		case BUILTIN_F64: cgen_write(g, "f64"); break;
		} break;
	case TYPE_PTR:
		if (!cgen_type_pre(g, t->ptr, where))
			return false;
		cgen_write(g, "(*");
		break;
	case TYPE_ARR:
		if (!cgen_type_pre(g, t->arr.of, where))
			return false;
		cgen_write(g, "(");
		break;
	case TYPE_FN:
		if (cgen_uses_ptr(&t->fn.types[0])) {
			cgen_write(g, "void");
		} else {
			if (!cgen_type_pre(g, &t->fn.types[0], where))
				return false;
		}
		cgen_write(g, " (*");
		break;
	case TYPE_SLICE:
		cgen_write(g, "slice_");
		break;
	case TYPE_VOID: cgen_write(g, "void"); break;
	case TYPE_UNKNOWN:
		err_print(t->where, "Can't determine type.");
		return false;
	case TYPE_TUPLE:
		/* We should never try to generate a tuple */
		assert(0);
		return false;
	}
	return true;
}

static bool cgen_type_post(CGenerator *g, Type *t, Location where) {
	switch (t->kind) {
	case TYPE_PTR:
		cgen_write(g, ")");
		if (!cgen_type_post(g, t->ptr, where))
			return false;
		break;
	case TYPE_ARR:
		if (t->flags & TYPE_FLAG_RESOLVED)
			cgen_write(g, "[%lu])", (unsigned long)t->arr.n);
		else {
			cgen_write(g, "[");
			if (!cgen_expr(g, t->arr.n_expr))
				return false;
			cgen_write(g, "])");
		}
		if (!cgen_type_post(g, t->arr.of, where))
			return false;
		break;
	case TYPE_FN: {
		bool out_param = cgen_uses_ptr(&t->fn.types[0]);
		cgen_write(g, ")(");
		for (size_t i = 1; i < arr_len(t->fn.types); i++) {
			if (i != 1)
				cgen_write(g, ", ");
			if (!cgen_type_pre(g, &t->fn.types[i], where))
				return false;
			if (cgen_uses_ptr(&t->fn.types[i]))
				cgen_write(g, "(*)");
			if (!cgen_type_post(g, &t->fn.types[i], where))
				return false;
		}
		if (out_param) {
			Type *ret_type = &t->fn.types[0];
			if (arr_len(t->fn.types) > 1)
				cgen_write(g, ", ");
			if (ret_type->kind == TYPE_TUPLE) {
				arr_foreach(ret_type->tuple, Type, x) {
					if (!cgen_type_pre(g, x, where))
						return false;
					cgen_write(g, "(*)");
					if (!cgen_type_post(g, x, where))
						return false;
					if (x != arr_last(ret_type->tuple)) {
						cgen_write(g, ", ");
					}
				}
			} else {
				if (!cgen_type_pre(g, ret_type, where))
					return false;
				cgen_write(g, "(*)");
				if (!cgen_type_post(g, ret_type, where))
					return false;
			}
		}
		if (arr_len(t->fn.types) == 1 && !out_param)
			cgen_write(g, "void");
		cgen_write(g, ")");
		if (!out_param)
			if (!cgen_type_post(g, &t->fn.types[0], where))
				return false;
	} break;
	case TYPE_BUILTIN:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TUPLE:
	case TYPE_SLICE:
		break;
	}
	return true;
}

static inline void cgen_fn_name(CGenerator *g, FnExpr *f) {
	if (f->c.name) {
		cgen_ident(g, f->c.name);
	} else {
		cgen_ident_id(g, f->c.id);
	}
}

static bool cgen_fn_header(CGenerator *g, FnExpr *f, Location where) {
	bool out_param = cgen_uses_ptr(&f->ret_type);
	bool any_params = false;
	if (out_param) {
		cgen_write(g, "void ");
	} else {
		if (!cgen_type_pre(g, &f->ret_type, where)) return false;
		cgen_write(g, " ");
	}
	cgen_fn_name(g, f);
	if (!out_param) {
		if (!cgen_type_post(g, &f->ret_type, where)) return false;
	}
	cgen_write(g, "(");
	arr_foreach(f->params, Declaration, d) {
		arr_foreach(d->idents, Identifier, i) {
			if (d != f->params || i != d->idents)
				cgen_write(g, ", ");
			any_params = true;
			if (!cgen_type_pre(g, &d->type, where))
				return false;
			cgen_write(g, " ");
			bool ptr = cgen_uses_ptr(&d->type);
			if (ptr) {
				fprint_type(stdout, &d->type);
				puts("");
				IdentDecl *idecl = ident_decl(*i);
				assert(idecl);
				idecl->flags |= IDECL_FLAG_CGEN_PTR;
			}
			cgen_ident(g, *i);
			if (!cgen_type_post(g, &d->type, where))
				return false;
		}
	}
	if (out_param) {
		if (f->ret_type.kind == TYPE_TUPLE) {
			/* multiple return variables */
			for (size_t i = 0; i < arr_len(f->ret_type.tuple); i++) {
				Type *x = &f->ret_type.tuple[i];
				if (any_params || i > 0)
					cgen_write(g, ", ");
				if (!cgen_type_pre(g, x, where)) return false;
				cgen_write(g, "(*ret%lu_)", (unsigned long)i);
				if (!cgen_type_post(g, x, where)) return false;
			}
		} else {
			if (any_params)
				cgen_write(g, ", ");
			if (!cgen_type_pre(g, &f->ret_type, where))
				return false;
			cgen_write(g, " (*ret_)");
			if (!cgen_type_post(g, &f->ret_type, where))
				return false;
		}
	}
	if (!out_param && arr_len(f->params) == 0)
		cgen_write(g, "void");
	cgen_write(g, ")");
	return true;
}


/* 
Either set_expr or set_str should be NULL and either to_expr or to_str should be NULL 
Also, set_str and/or to_str should be NULL
this will call cgen_expr_pre for set_expr and to_expr
*/
static bool cgen_set(CGenerator *g, Expression *set_expr, const char *set_str, Expression *to_expr,
					 const char *to_str) {
	Type *type;
	Location where;
	if (set_expr) {
		type = &set_expr->type;
		where = set_expr->where;
		if (!cgen_expr_pre(g, set_expr)) return false;
	} else {
		assert(to_expr);
		type = &to_expr->type;
		where = to_expr->where;
		if (!cgen_expr_pre(g, to_expr)) return false;
	}
	switch (type->kind) {
	case TYPE_BUILTIN:
	case TYPE_FN:
	case TYPE_PTR:
	case TYPE_SLICE:
	case TYPE_UNKNOWN:
		if (set_expr) {
			if (!cgen_expr(g, set_expr)) return false;
		} else {
			cgen_write(g, set_str);
		}
		cgen_write(g, " = ");
		if (to_expr) {
			if (!cgen_expr(g, to_expr)) return false;
		} else {
			cgen_write(g, to_str);
		}
		cgen_write(g, ";");
		break;
	case TYPE_ARR:
		cgen_write(g, "{");
		cgen_nl(g);
		cgen_write(g, "size_t i;");
		if (!cgen_type_pre(g, type->arr.of, where)) return false;
		cgen_write(g, "(*arr__in)");
		if (!cgen_type_post(g, type->arr.of, where)) return false;
		cgen_write(g, " = ");
		if (to_expr) {
			if (!cgen_expr(g, to_expr)) return false;
		} else {
			cgen_write(g, to_str);
		}
		cgen_write(g, "; ");
		if (!cgen_type_pre(g, type->arr.of, where)) return false;
		cgen_write(g, "(*arr__out)");
		if (!cgen_type_post(g, type->arr.of, where)) return false;
		cgen_write(g, " = ");
		if (set_expr) {
			if (!cgen_expr(g, set_expr)) return false;
		} else {
			cgen_write(g, set_str);
		}
		cgen_write(g, ";");
		cgen_nl(g);
		cgen_write(g, "for (i = 0; i < %lu; i++) arr__out[i] = arr__in[i];", (unsigned long)type->arr.n);
		cgen_nl(g);
		cgen_write(g, "}");
		break;
	case TYPE_TUPLE:
		assert(set_expr);
		assert(to_expr);
	    assert(set_expr->kind == EXPR_TUPLE);
		if (!cgen_set_tuple(g, set_expr->tuple, NULL, NULL, to_expr))
			return false;
		break;
	case TYPE_VOID:
		assert(0);
		return false;
	}
	return true;
}

/* one of exprs, idents, and prefix should be NULL. */
static bool cgen_set_tuple(CGenerator *g, Expression *exprs, Identifier *idents, const char *prefix, Expression *to) {
	IdentID prefix_id; /* ID of prefix for block */
	switch (to->kind) {
	case EXPR_TUPLE:
		/* e.g. a, b = 3, 5; */
		for (size_t i = 0; i < arr_len(to->tuple); i++) {
			char *s = NULL, buf[64];
			Expression *e = NULL;
			if (idents)
				s = cgen_ident_to_str(idents[i]);
			else if (exprs)
				e = &exprs[i];
			else {
				snprintf(buf, sizeof buf, "(%s%lu_)", prefix, i);
				s = buf;
			}
			if (!cgen_set(g, e, s, &to->tuple[i], NULL)) return false;
			if (s != buf) free(s);
		}
		break;
	case EXPR_CALL: {
		/* e.g. a, b = fn_which_returns_tuple(); */
		if (!cgen_expr(g, to->call.fn)) return false;
		cgen_write(g, "(");
		bool any_args = arr_len(to->call.arg_exprs) != 0;
		arr_foreach(to->call.arg_exprs, Expression, arg) {
			if (arg != to->call.arg_exprs)
				cgen_write(g, ", ");
			if (cgen_uses_ptr(&arg->type))
				cgen_write(g, "&");
			if (!cgen_expr(g, arg))
				return false;
		}
		/* out params */
		size_t len = exprs ? arr_len(exprs) : arr_len(idents);

		for (unsigned long i = 0; i < (unsigned long)len; i++) {
			if (any_args || i > 0)
				cgen_write(g, ", ");
			if (exprs) {
				cgen_write(g, "&");
				if (!cgen_expr(g, &exprs[i]))
					return false;
			} else if (idents) {
				cgen_write(g, "&");
				cgen_ident(g, idents[i]);
			} else {
				cgen_write(g, "&(%s%lu_)", prefix, i);
			}
		}
		cgen_writeln(g, ");");
	} break;
	case EXPR_IF:
	    prefix_id = to->if_.c.id;
		goto prefixed;
	case EXPR_BLOCK:
		prefix_id = to->block_ret_id;
		goto prefixed;
	case EXPR_WHILE:
		prefix_id = to->while_.c.id;
		goto prefixed;
	prefixed:
		for (unsigned long i = 0; i < (unsigned long)arr_len(to->type.tuple); i++) {
			cgen_write(g, "(");
			if (exprs) {
				if (!cgen_expr(g, &exprs[i]))
					return false;
			} else if (idents) {
				cgen_ident(g, idents[i]);
			} else {
				cgen_write(g, "%s%lu_", prefix, i);
			}
			cgen_write(g, ") = ");
			cgen_ident_id(g, prefix_id);
			cgen_write(g, "%lu_", i);
			cgen_write(g, "; ");
		}
	    break;
    case EXPR_IDENT:
	case EXPR_LITERAL_INT:
	case EXPR_LITERAL_CHAR:
	case EXPR_LITERAL_BOOL:
	case EXPR_LITERAL_STR:
	case EXPR_LITERAL_FLOAT:
	case EXPR_UNARY_OP:
	case EXPR_BINARY_OP:
	case EXPR_FN:
	case EXPR_CAST:
	case EXPR_NEW:
	case EXPR_DIRECT:
		assert(0);
		return false;
	}
	return true;
}

/* generates the C code for new'ing a slice of array type t (e.g. [5]int) and putting it in the given ident id. */
static bool cgen_new_slice(CGenerator *g, Type *t, IdentID id, Location where) {
	Expression *n_expr = t->arr.n_expr;
	assert(!(t->flags & TYPE_FLAG_RESOLVED)); /* we don't want this to be resolved, because the size might only be known at runtime. */
	if (!cgen_expr_pre(g, n_expr)) return false;
	cgen_write(g, "size_t s");
	cgen_ident_id(g, id);
	cgen_write(g, " = ");
	if (!cgen_expr(g, n_expr)) return false;
	cgen_write(g, "; slice_ ");
	cgen_ident_id(g, id);
	cgen_write(g, ";");
	cgen_ident_id(g, id);
	cgen_write(g, ".data = calloc(s");
	cgen_ident_id(g, id);
	cgen_write(g, ", sizeof(");
	if (t->arr.of->kind == TYPE_ARR) {
		cgen_write(g, "slice_");
	} else {
		if (!cgen_type_pre(g, t->arr.of, where))
			return false;
		if (!cgen_type_post(g, t->arr.of, where))
			return false;
	}
	cgen_write(g, ")); ");
	cgen_ident_id(g, id);
	cgen_write(g, ".n = s");
	cgen_ident_id(g, id);
	cgen_write(g, ";");
	if (t->arr.of->kind == TYPE_ARR) {
		/* slice of slices. initialize the inner slices. */
		IdentID child_id = g->ident_counter++;
		cgen_write(g, "for (u64 i_ = 0; i_ < s");
		cgen_ident_id(g, id);
		cgen_write(g, "; i_++) {");
		cgen_nl(g);
		g->indent_lvl++;
		if (!cgen_new_slice(g, t->arr.of, child_id, where))
			return false;
		cgen_write(g, " ((slice_*)");
		cgen_ident_id(g, id);
		cgen_write(g, ".data)[i_] = ");
		cgen_ident_id(g, child_id);
		cgen_write(g, ";");
		cgen_nl(g);
		g->indent_lvl--;
		cgen_write(g, "}");
	}
	return true;
}

static bool cgen_expr_pre(CGenerator *g, Expression *e) {
	IdentID id;
	char ret_name[64];
	switch (e->kind) {
	case EXPR_IF:
	case EXPR_WHILE:
	case EXPR_BLOCK: {
		id = g->ident_counter++;
		
		cgen_ident_id_to_str(ret_name, id);
		char *p = ret_name + strlen(ret_name);
		if (e->type.kind != TYPE_VOID) {
			if (e->type.kind == TYPE_TUPLE) {
				for (unsigned long i = 0; i < arr_len(e->type.tuple); i++) {
					sprintf(p, "%lu_", i);
					if (!cgen_type_pre(g, &e->type.tuple[i], e->where)) return false;
					cgen_write(g, " %s", ret_name);
					if (!cgen_type_post(g, &e->type.tuple[i], e->where)) return false;
					cgen_write(g, "; ");
				}
			
			} else {
				cgen_type_pre(g, &e->type, e->where);
				cgen_write(g, " %s", ret_name);
				cgen_type_post(g, &e->type, e->where);
				cgen_write(g, ";");
				cgen_nl(g);
			}
		}
		*p = 0; /* clear tuple suffixes */
		
	} break;
	default: break;
	}
	
	switch (e->kind) {
	case EXPR_IF: {
		IfExpr *curr = &e->if_;
		curr->c.id = id;
		while (1) {
			if (curr->cond) {
				cgen_write(g, "if (");
				if (!cgen_expr(g, curr->cond))
					return false;
				cgen_write(g, ") ");
			}
			if (!cgen_block(g, &curr->body, ret_name, 0))
				return false;
			if (curr->next_elif) {
				cgen_write(g, " else ");
				curr = &curr->next_elif->if_;
			} else break;
		}
	} break;
	case EXPR_WHILE: {
		WhileExpr *w = &e->while_;
		w->c.id = id;
		cgen_write(g, "while (");
		if (w->cond) {
			if (!cgen_expr(g, w->cond))
				return false;
		} else {
			cgen_write(g, "true");
		}
		cgen_write(g, ") ");
		if (!cgen_block(g, &w->body, ret_name, 0))
			return false;
	} break;
	case EXPR_BLOCK:
		e->block_ret_id = id;
		if (!cgen_block(g, &e->block, ret_name, 0))
			return false;
		break;
	case EXPR_CALL:
		if (!cgen_expr_pre(g, e->call.fn)) return false;
		arr_foreach(e->call.arg_exprs, Expression, arg)
			if (!cgen_expr_pre(g, arg)) return false;
		break;
	case EXPR_UNARY_OP:
		if (!cgen_expr_pre(g, e->unary.of)) return false;
		break;
	case EXPR_BINARY_OP:
		if (!cgen_expr_pre(g, e->binary.lhs)) return false;
		if (!cgen_expr_pre(g, e->binary.rhs)) return false;
		break;
	case EXPR_CAST:
		if (!cgen_expr_pre(g, e->cast.expr)) return false;
		break;
	case EXPR_NEW:
		if (e->new.n) {
			if (!cgen_expr_pre(g, e->new.n))
				return false;
			IdentID n_id = e->new.c.id = g->ident_counter++;
			cgen_write(g, "slice_ ");
			cgen_ident_id(g, n_id);
			cgen_write(g, "; ");
			cgen_ident_id(g, n_id);
			cgen_write(g, ".data = calloc(");
			if (!cgen_expr(g, e->new.n)) return false;
			cgen_write(g, ", sizeof(");
			if (!cgen_type_pre(g, &e->new.type, e->where)) return false;
			if (!cgen_type_post(g, &e->new.type, e->where)) return false;
			cgen_write(g, ")); ");
			cgen_ident_id(g, n_id);
			cgen_write(g, ".n = ");
			if (!cgen_expr(g, e->new.n)) return false;
			cgen_write(g, ";");
		}
		break;
	case EXPR_LITERAL_INT:
	case EXPR_LITERAL_FLOAT:
	case EXPR_LITERAL_BOOL:
	case EXPR_LITERAL_CHAR:
	case EXPR_LITERAL_STR:
	case EXPR_IDENT:
	case EXPR_FN:
	case EXPR_DIRECT:
		break;
	case EXPR_TUPLE:
		arr_foreach(e->tuple, Expression, x)
			if (!cgen_expr_pre(g, x)) return false;
	}
	return true;
}

static bool cgen_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_LITERAL_FLOAT:
		cgen_write(g, "%f", e->floatl); /* TODO(eventually): better precision? */
		break;
	case EXPR_LITERAL_INT:
		cgen_write(g, UINTEGER_FMT, e->intl);
		break;
	case EXPR_LITERAL_STR: {
		size_t c;
		cgen_write(g, "\"");
		for (c = 0; c < e->strl.len; c++) {
			cgen_write(g, "\\x%x", e->strl.str[c]);
		}
		cgen_write(g, "\"");
	} break;
	case EXPR_LITERAL_BOOL:
		cgen_write(g, e->booll ? "true" : "false");
		break;
	case EXPR_LITERAL_CHAR:
		cgen_write(g, "((char)%d)", e->charl);
		break;
	case EXPR_IDENT:
		cgen_ident(g, e->ident);
		break;
	case EXPR_BINARY_OP: {
		const char *s = "";
		if (e->binary.op == BINARY_SET) {
			if (!cgen_set(g, e->binary.lhs, NULL, e->binary.rhs, NULL)) return false;
			break;
		}
		switch (e->binary.op) {
		case BINARY_SUB:
			s = "-"; break;
		case BINARY_ADD:
			s = "+"; break;
		case BINARY_MUL:
			s = "*"; break;
		case BINARY_DIV:
			s = "/"; break;
		case BINARY_SET: assert(0); break;
		case BINARY_GT:
			s = ">"; break;
		case BINARY_LT:
			s = "<"; break;
		case BINARY_GE:
			s = ">="; break;
		case BINARY_LE:
			s = "<="; break;
		case BINARY_EQ:
			s = "=="; break;
		case BINARY_NE:
			s = "!="; break;
		case BINARY_AT_INDEX:
			s = "["; break;
		}
		cgen_write(g, "(");
		if (!cgen_expr(g, e->binary.lhs))
			return false; 
		cgen_write(g, "%s", s);
		if (!cgen_expr(g, e->binary.rhs))
			return false;
		if (e->binary.op == BINARY_AT_INDEX)
			cgen_write(g, "]");
		cgen_write(g, ")");
	} break;
	case EXPR_UNARY_OP: {
		const char *s = "";
		switch (e->unary.op) {
		case UNARY_MINUS:
			s = "-"; break;
		case UNARY_DEREF:
			s = "*"; break;
		case UNARY_ADDRESS:
			s = "&"; break;
		case UNARY_NOT:
			s = "!"; break;
		case UNARY_DEL:
			s = "free("; break;
		}
		cgen_write(g, "(");
		cgen_write(g, "%s", s);
		if (!cgen_expr(g, e->unary.of))
			return false;
		if (e->unary.op == UNARY_DEL)
			cgen_write(g, ")");
		cgen_write(g, ")");
	} break;
	case EXPR_NEW: {
		if (e->new.n) {
			cgen_ident_id(g, e->new.c.id);
		} else {
			Type *t = &e->new.type;
			cgen_write(g, "((");
			if (!cgen_type_pre(g, &e->type, e->where))
				return false;
			if (!cgen_type_post(g, &e->type, e->where))
				return false;
			cgen_write(g, ")calloc(1, sizeof(");
			if (!cgen_type_pre(g, t, e->where))
				return false;
			if (!cgen_type_post(g, t, e->where))
				return false;
			cgen_write(g, ")))");
		}
	} break;
	case EXPR_IF:
		cgen_ident_id(g, e->if_.c.id);
		break;
	case EXPR_WHILE:
		if (e->type.kind != TYPE_VOID)
			cgen_ident_id(g, e->while_.c.id);
		break;
	case EXPR_BLOCK:
		cgen_ident_id(g, e->block_ret_id);
		break;
	case EXPR_CALL:
		cgen_write(g, "(");
		if (!cgen_expr(g, e->call.fn))
			return false;
		cgen_write(g, "(");
		arr_foreach(e->call.arg_exprs, Expression, arg) {
			if (arg != e->call.arg_exprs)
				cgen_write(g, ", ");
			if (!cgen_expr(g, arg))
				return false;
		}
		cgen_write(g, "))");
		break;
	case EXPR_DIRECT:
		switch (e->direct.which) {
		case DIRECT_C: {
			Value val;
			if (!eval_expr(g->evalr, &e->direct.args[0], &val))
				return false;
			cgen_indent(g);
			fwrite(val.arr, 1, e->direct.args[0].type.arr.n, cgen_writing_to(g));
		} break;
		case DIRECT_COUNT: assert(0); break;
		}
		break;
	case EXPR_CAST:
		cgen_write(g, "((");
		cgen_type_pre(g, &e->cast.type, e->where);
		cgen_type_post(g, &e->cast.type, e->where);
		cgen_write(g, ")(");
		if (!cgen_expr(g, e->cast.expr))
			return false;
		cgen_write(g, ")");
		if (e->cast.expr->type.kind == TYPE_SLICE
			&& e->cast.type.kind != TYPE_SLICE) /* casting from a slice to a non-slice */
			cgen_write(g, ".data");
		cgen_write(g, ")");
		break;
	case EXPR_TUPLE:
		/* the only time this should happen is if you're stating
		   a tuple, e.g. 3, 5;, but we've errored about that before
		   (the comma operator does not exist in toc!) */
		assert(0);
		break;
	case EXPR_FN: {
		if (g->block != NULL) {
			Expression **eptr = arr_add(&g->anon_fns);
			*eptr = e;
		}
		cgen_fn_name(g, &e->fn);
	} break;
	}
	return true;
}

/*
  ret_name = variable to store block return value in; NULL for none. NOTE:
  functions always call with NULL as ret_name, even if they use out params, for now
  at least. 
*/
static bool cgen_block(CGenerator *g, Block *b, const char *ret_name, uint16_t flags) {
	Block *prev = g->block;
	if (!(flags & CGEN_BLOCK_FLAG_NOBRACES))
		cgen_write(g, "{");
	if (!(flags & CGEN_BLOCK_FLAG_NOENTER))
		if (!cgen_block_enter(g, b))
			return false;
	cgen_nl(g);
	arr_foreach(b->stmts, Statement, s)
		if (!cgen_stmt(g, s))
			return false;

	if (b->ret_expr && ret_name) {
		if (b->ret_expr->type.kind == TYPE_TUPLE) {
			if (!cgen_set_tuple(g, NULL, NULL, ret_name, b->ret_expr))
				return false;
		} else {
			if (!cgen_set(g, NULL, ret_name, b->ret_expr, NULL))
				return false;
		}
		cgen_nl(g);
	}
	if (!(flags & CGEN_BLOCK_FLAG_NOENTER))
		cgen_block_exit(g, prev);
	if (!(flags & CGEN_BLOCK_FLAG_NOBRACES))
		cgen_write(g, "}");
	return true;
}

static void cgen_zero_value(CGenerator *g, Type *t) {
	switch (t->kind) {
	case TYPE_BUILTIN:
		cgen_write(g, "0");
		break;
	case TYPE_PTR:
	case TYPE_FN:
		cgen_write(g, "NULL");
		break;
	case TYPE_SLICE:
		cgen_write(g, "{NULL, 0}");
		break;
	case TYPE_ARR:
		cgen_write(g, "{");
		cgen_zero_value(g, t->arr.of);
		cgen_write(g, "}");
		break;
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TUPLE:
		assert(0);
		break;
	}
}

static bool cgen_fn(CGenerator *g, FnExpr *f, Location where) {
	FnExpr *prev_fn = g->fn;
	Block *prev_block = g->block;
	fn_enter(f);
	if (!cgen_fn_header(g, f, where))
		return false;
	cgen_write(g, " ");
	g->fn = f;
	cgen_write(g, "{");
	cgen_nl(g);
	arr_foreach(f->ret_decls, Declaration, d) {
		if (!cgen_decl(g, d))
			return false;
	}
	if (!cgen_block_enter(g, &f->body)) return false;
	if (!cgen_block(g, &f->body, NULL, CGEN_BLOCK_FLAG_NOENTER | CGEN_BLOCK_FLAG_NOBRACES))
		return false;

	if (f->ret_decls) {
		if (cgen_uses_ptr(&f->ret_type)) {
		} else {
			cgen_write(g, "return ");
			cgen_ident(g, f->ret_decls[0].idents[0]);
			cgen_writeln(g, ";");
		}
	} else if (f->body.ret_expr) {
		if (!cgen_ret(g, f->body.ret_expr)) return false;
	}
	cgen_block_exit(g, prev_block);
	
	cgen_write(g, "}");
	fn_exit(f);
	cgen_nl(g);
	g->fn = prev_fn;
	cgen_nl(g);
	cgen_nl(g);
	return true;
}

static bool cgen_decl(CGenerator *g, Declaration *d) {
	if (cgen_fn_is_direct(g, d)) {
		cgen_fn(g, &d->expr.fn, d->where);
	} else if (d->flags & DECL_FLAG_CONST) {
		/* TODO? */
	} else {
		/* TODO: Globals just cgen val */		
		for (size_t idx = 0; idx < arr_len(d->idents); idx++) {
			Identifier *i = &d->idents[idx];
			Type *t = d->type.kind == TYPE_TUPLE ? &d->type.tuple[idx] : &d->type;
			if (!cgen_type_pre(g, t, d->where)) return false;
			cgen_write(g, " ");
			cgen_ident(g, *i);
			if (!cgen_type_post(g, t, d->where)) return false;
			if (g->block == NULL && (d->flags & DECL_FLAG_HAS_EXPR)) {
				cgen_write(g, " = ");
				/* directly initialize iff we are in global scope */
				if (!cgen_expr(g, &d->expr))
					return false;
			} else if (!(d->flags & DECL_FLAG_HAS_EXPR)) {
				cgen_write(g, " = ");
		    	cgen_zero_value(g, t);
			}
				
			cgen_write(g, "; ");
		}
		/* TODO: global tuples */
		if (g->block != NULL && (d->flags & DECL_FLAG_HAS_EXPR)) {
			if (d->expr.type.kind == TYPE_TUPLE) {
				if (!cgen_set_tuple(g, NULL, d->idents, NULL, &d->expr)) return false;
			} else {
				cgen_write(g, "{");
		 
				cgen_nl(g);
				if (!cgen_type_pre(g, &d->expr.type, d->expr.where)) return false;
				cgen_write(g, " expr__");
				if (!cgen_type_post(g, &d->expr.type, d->expr.where)) return false;
				cgen_write(g, "; ");
				if (!cgen_set(g, NULL, "expr__", &d->expr, NULL))
					return false;
				arr_foreach(d->idents, Identifier, i) {
					Expression e;
					e.flags = 0;
					e.kind = EXPR_IDENT;
					e.type = d->type;
					e.ident = *i;
					if (!cgen_set(g, &e, NULL, NULL, "expr__"))
						return false;
				}
				cgen_write(g, "}");
			}
		}
		cgen_nl(g);
	}
	return true;
}

static bool cgen_ret(CGenerator *g, Expression *ret) {
	assert((g->fn->ret_type.kind == TYPE_VOID) == (ret == NULL));
	if (!ret) {
		cgen_write(g, "return");
	} else if (cgen_uses_ptr(&g->fn->ret_type)) {
		if (g->fn->ret_type.kind == TYPE_TUPLE) {
			if (!cgen_set_tuple(g, NULL, NULL, "*ret", ret))
				return false;
		} else {
			if (!cgen_set(g, NULL, "*ret_", ret, NULL)) return false;
		}
		cgen_write(g, "return");
	} else {
		cgen_write(g, "return ");
			
		if (!cgen_expr(g, ret)) return false;
	}
	cgen_write(g, ";");
	cgen_nl(g);
	return true;

}

static bool cgen_stmt(CGenerator *g, Statement *s) {
	/*
	  TODO(eventually): optionally this:
	  cgen_write(g, "/\* %s:%d *\/", s->where.filename, s->where.line);
	*/
	switch (s->kind) {
	case STMT_DECL:
		if (!cgen_decl(g, &s->decl)) return false;
		break;
	case STMT_EXPR:
		if (!cgen_expr_pre(g, &s->expr)) return false;
		if (!cgen_expr(g, &s->expr)) return false;
		cgen_write(g, ";");
		cgen_nl(g);
		break;
	case STMT_RET:
		if (!cgen_ret(g, s->ret.flags & RET_FLAG_EXPR ? &s->ret.expr : NULL))
			return false;
		break;
	}
	return true;
}

static bool cgen_decls_file(CGenerator *g, ParsedFile *f);

static bool cgen_file(CGenerator *g, ParsedFile *f) {
	g->block = NULL;
	g->file = f;
	cgen_write(g, "#include <stdint.h>\n"
			   "#include <stdlib.h>\n"
			   "typedef int8_t i8;\n"
			   "typedef int16_t i16;\n"
			   "typedef int32_t i32;\n"
			   "typedef int64_t i64;\n"
			   "typedef uint8_t u8;\n"
			   "typedef uint16_t u16;\n"
			   "typedef uint32_t u32;\n"
			   "typedef uint64_t u64;\n"
			   "typedef float f32;\n"
			   "typedef double f64;\n"
			   "typedef unsigned char bool;\n"
			   "typedef struct { void *data; u64 n; } slice_;\n"
			   "#define false ((bool)0)\n"
			   "#define true ((bool)1)\n\n\n");
	if (!cgen_decls_file(g, f))
		return false;
	cgen_write(g, "/* code */\n");
	cgen_write(g, "int main() {\n\tmain__();\n\treturn 0;\n}\n\n");

	arr_foreach(f->stmts, Statement, s) {
		if (!cgen_stmt(g, s))
			return false;
	}
	typedef Expression *ExprPtr;
	arr_foreach(g->anon_fns, ExprPtr, eptr) {
		Expression *e = *eptr;
		cgen_fn(g, &e->fn, e->where);
	}
	
	return true;
}
