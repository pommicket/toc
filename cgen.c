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
static bool cgen_val_ptr(CGenerator *g, void *v, Type *t, Location where);

static void cgen_create(CGenerator *g, FILE *out, Identifiers *ids, Evaluator *ev) {
	g->outc = out;
	g->ident_counter = 1; /* some places use 0 to mean no id */
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
	return block_enter(b, stmts, 0);
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

static void cgen_ident_id(CGenerator *g, IdentID id) {
	cgen_write(g, "a%lu_", (unsigned long)id);
}

/* should declaration be a direct function declaration C (as opposed to using a function pointer or not being a function) */
static bool cgen_fn_is_direct(CGenerator *g, Declaration *d) {
	return g->block == NULL && (d->flags & DECL_FLAG_HAS_EXPR) && d->expr.kind == EXPR_FN && arr_len(d->idents) == 1;
}

static bool cgen_uses_ptr(Type *t) {
	switch (t->kind) {
	case TYPE_TUPLE:
	case TYPE_STRUCT:
		return true;
	case TYPE_ARR: /* TODO: test me */
	case TYPE_BUILTIN:
	case TYPE_PTR:
	case TYPE_FN:
	case TYPE_SLICE:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TYPE:
		return false;
	case TYPE_USER:
		return cgen_uses_ptr(type_user_underlying(t));
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
		fprint_ident(cgen_writing_to(g), i);
	}
}

static char *cgen_ident_to_str(Identifier i) {
	return ident_to_str(i);
}


/* buffer should be at least 32 bytes */
static inline void cgen_ident_id_to_str(char *buffer, IdentID id) {
	snprintf(buffer, 32, "a%lu_", (unsigned long)id);
}

static bool cgen_type_pre(CGenerator *g, Type *t, Location where) {
	assert(t->flags & TYPE_FLAG_RESOLVED);
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
		err_print(where, "Can't determine type.");
		return false;
	case TYPE_STRUCT:
		cgen_write(g, "struct {");
		g->indent_lvl++;
		cgen_nl(g);
		arr_foreach(t->struc.fields, Field, f) {
			if (!cgen_type_pre(g, f->type, where)) return false;
			cgen_write(g, " ");
			cgen_ident(g, f->name);
			if (!cgen_type_post(g, f->type, where)) return false;
			cgen_write(g, ";");
			cgen_nl(g);
		}
		g->indent_lvl--;
		cgen_write(g, "}");
		break;
	case TYPE_TUPLE:
	case TYPE_TYPE:
		/* We should never try to generate this type */
		assert(0);
		return false;
	case TYPE_USER: {
		Type *this = t;
		do {
			Type *next = type_user_underlying(this);
			if (next->kind == TYPE_STRUCT) {
				/* use struct tag */
				cgen_write(g, "struct ");
				t = this;
				break;
			}
			this = next;
		} while (this->kind == TYPE_USER);

		Declaration *d = t->user.decl;
		int idx = t->user.index;
		if (d->c.ids[idx])
			cgen_ident_id(g, d->c.ids[idx]);
		else
			cgen_ident(g, d->idents[idx]);
	} break;
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
		assert(t->flags & TYPE_FLAG_RESOLVED);
	    cgen_write(g, "[%lu])", (unsigned long)t->arr.n);
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
	case TYPE_TYPE:
	case TYPE_SLICE:
	case TYPE_USER:
	case TYPE_STRUCT:
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
	if (!f->c.name) /* anonymous fn */
		cgen_write(g, "static ");
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
		long idx = 0;
		arr_foreach(d->idents, Identifier, i) {
			if (d != f->params || i != d->idents)
				cgen_write(g, ", ");
			Type *type = d->type.kind == TYPE_TUPLE ? &d->type.tuple[idx++] : &d->type;
			any_params = true;
			if (!cgen_type_pre(g, type, where))
				return false;
			cgen_write(g, " ");
			cgen_ident(g, *i);
			if (!cgen_type_post(g, type, where))
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
	type = type_inner(type);
	switch (type->kind) {
	case TYPE_BUILTIN:
	case TYPE_FN:
	case TYPE_PTR:
	case TYPE_SLICE:
	case TYPE_STRUCT:
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
	case TYPE_USER:
	case TYPE_VOID:
	case TYPE_TYPE:
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
	case EXPR_SLICE:
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
	case EXPR_C:
	case EXPR_DSIZEOF:
	case EXPR_DALIGNOF:
	case EXPR_TYPE:
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
	cgen_write(g, ".data = e__calloc(s");
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
	IdentID id = 0;
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
	case EXPR_CALL: {
		if (!cgen_expr_pre(g, e->call.fn)) return false;
		arr_foreach(e->call.arg_exprs, Expression, arg)
			if (!cgen_expr_pre(g, arg)) return false;
		if (cgen_uses_ptr(&e->type)) {
			e->call.c.id = g->ident_counter++;
			if (!cgen_type_pre(g, &e->type, e->where)) return false;
			cgen_write(g, " ");
			cgen_ident_id(g, e->call.c.id);
			if (!cgen_type_post(g, &e->type, e->where)) return false;
			cgen_write(g, ";"); cgen_nl(g);
			if (!cgen_expr(g, e->call.fn)) return false;
			cgen_write(g, "(");
			bool any_args = false;
			arr_foreach(e->call.arg_exprs, Expression, arg) {
				any_args = true;
				if (arg != e->call.arg_exprs)
					cgen_write(g, ", ");
				if (!cgen_expr(g, arg))
					return false;
			}
			if (any_args) {
				cgen_write(g, ", ");
			}
			cgen_write(g, "&");
			cgen_ident_id(g, e->call.c.id);
			cgen_write(g, ");");
			cgen_nl(g);
		}
	} break;
	case EXPR_UNARY_OP:
		if (!cgen_expr_pre(g, e->unary.of)) return false;
		break;
	case EXPR_BINARY_OP:
		if (!cgen_expr_pre(g, e->binary.lhs)) return false;
		if (e->binary.op != BINARY_DOT)
			if (!cgen_expr_pre(g, e->binary.rhs)) return false;
		break;
	case EXPR_CAST:
		if (!cgen_expr_pre(g, e->cast.expr)) return false;
		break;
	case EXPR_SLICE: {
		SliceExpr *s = &e->slice;
	    IdentID s_id = e->slice.c.id = g->ident_counter++;
		IdentID from_id = g->ident_counter++;
		if (!cgen_expr_pre(g, s->of))
			return false;
		if (s->from && !cgen_expr_pre(g, s->from))
			return false;
		if (s->to && !cgen_expr_pre(g, s->to))
			return false;
		cgen_write(g, "slice_ ");
		cgen_ident_id(g, s_id);
		cgen_write(g, "; { slice_ of__ = ");
		if (!cgen_expr(g, s->of))
			return false;
		cgen_write(g, "; u64 ");
		cgen_ident_id(g, from_id);
		cgen_write(g, " = ");
		if (s->from) {
			if (!cgen_expr(g, s->from))
				return false;
		} else {
			cgen_write(g, "0");
		}
		cgen_write(g, "; ");
		cgen_ident_id(g, s_id);
		cgen_write(g, ".data = (");
		if (!cgen_type_pre(g, e->type.slice, e->where))
			return false;
		cgen_write(g, "(*)");
		if (!cgen_type_post(g, e->type.slice, e->where))
			return false;
		cgen_write(g, ")(of__");
		if (s->of->type.kind == TYPE_SLICE) {
			cgen_write(g, ".data");
		}
		cgen_write(g, ") + ");
		cgen_ident_id(g, from_id);
		cgen_write(g, "; ");
		cgen_ident_id(g, s_id);
		cgen_write(g, ".n = ");
		if (s->to) {
			if (!cgen_expr(g, s->to))
				return false;
		} else {
			cgen_write(g, "of__.n - 1");
		}
		cgen_write(g, " - ");
		cgen_ident_id(g, from_id);
		cgen_write(g, "; }");
		cgen_nl(g);
	} break;
	case EXPR_NEW:
		if (e->new.n && !cgen_expr_pre(g, e->new.n))
			return false;
		break;
	case EXPR_LITERAL_INT:
	case EXPR_LITERAL_FLOAT:
	case EXPR_LITERAL_BOOL:
	case EXPR_LITERAL_CHAR:
	case EXPR_LITERAL_STR:
	case EXPR_IDENT:
	case EXPR_FN:
	case EXPR_C:
	case EXPR_DSIZEOF:
	case EXPR_DALIGNOF:
	case EXPR_TYPE:
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
		cgen_write(g, "%.16Lf", (long double)e->floatl); /* TODO(eventually): better precision? */
		break;
	case EXPR_LITERAL_INT:
		cgen_write(g, UINTEGER_FMT, e->intl);
		break;
	case EXPR_LITERAL_STR: {
		size_t c;
		cgen_write(g, "mkslice_(\"");
		for (c = 0; c < e->strl.len; c++) {
			cgen_write(g, "\\x%x", e->strl.str[c]);
		}
		cgen_write(g, "\", %lu)", (unsigned long)e->strl.len);
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
		bool handled = false;
		switch (e->binary.op) {
		case BINARY_SUB:
			s = "-"; break;
		case BINARY_ADD:
			s = "+"; break;
		case BINARY_MUL:
			s = "*"; break;
		case BINARY_DIV:
			s = "/"; break;
		case BINARY_SET: 
			if (!cgen_set(g, e->binary.lhs, NULL, e->binary.rhs, NULL)) return false;
			handled = true;
			break;
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
			cgen_write(g, "(");
			switch (e->binary.lhs->type.kind) {
			case TYPE_ARR:
				if (!cgen_expr(g, e->binary.lhs))
					return false;
				cgen_write(g, "[");
				if (!cgen_expr(g, e->binary.rhs))
					return false;
				cgen_write(g, "]");
				break;
			case TYPE_SLICE:
				cgen_write(g, "((");
				if (!cgen_type_pre(g, &e->type, e->where))
					return false;
				cgen_write(g, "(*)");
				if (!cgen_type_post(g, &e->type, e->where))
					return false;
				cgen_write(g, ")(");
				if (!cgen_expr(g, e->binary.lhs))
					return false;
				cgen_write(g, ".data))[");
				if (!cgen_expr(g, e->binary.rhs))
					return false;
				cgen_write(g, "]");
				break;
			default:
				assert(0);
				break;
			}
			cgen_write(g, ")");
			handled = true;
			break;
		case BINARY_DOT: {
			cgen_write(g, "(");
			cgen_expr(g, e->binary.lhs);
			bool is_ptr = type_inner(&e->binary.lhs->type)->kind == TYPE_PTR;
			cgen_write(g, is_ptr ? "->" :".");
			cgen_ident(g, e->binary.field->name);
			cgen_write(g, ")");
			handled = true;
		} break;
		}
		if (handled) break;
		cgen_write(g, "(");
		if (!cgen_expr(g, e->binary.lhs))
			return false; 
		cgen_write(g, "%s", s);
		if (!cgen_expr(g, e->binary.rhs))
			return false;
		cgen_write(g, ")");
	} break;
	case EXPR_UNARY_OP: {
		const char *s = "";
		bool handled = false;
		Type *of_type = &e->unary.of->type;
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
			cgen_write(g, "free(");
			if (!cgen_expr(g, e->unary.of))
				return false;
			if (of_type->kind == TYPE_SLICE)
				cgen_write(g, ".data");
			cgen_write(g, ")");
			handled = true;
			break;
		case UNARY_LEN: {
			bool is_ptr = of_type->kind == TYPE_PTR;
			if (is_ptr) {
				of_type = of_type->ptr;
			}
			switch (of_type->kind) {
			case TYPE_SLICE:
				if (!cgen_expr(g, e->unary.of))
					return false;
				cgen_write(g, "%sn", is_ptr ? "->" : ".");
				break;
			case TYPE_ARR:
				cgen_write(g, "%lu", (unsigned long)of_type->arr.n);
				break;
			default: assert(0); break;
			}
			handled = true;
		} break;
		}
		if (handled) break;
		cgen_write(g, "(");
		cgen_write(g, "%s", s);
		if (!cgen_expr(g, e->unary.of))
			return false;
		cgen_write(g, ")");
	} break;
	case EXPR_NEW: {
		if (e->new.n) {
			cgen_write(g, "mkslice_(e__calloc(");
			if (!cgen_expr(g, e->new.n)) return false;
			cgen_write(g, ", sizeof(");
			if (!cgen_type_pre(g, &e->new.type, e->where)) return false;
			if (!cgen_type_post(g, &e->new.type, e->where)) return false;
			cgen_write(g, ")), ");
			if (!cgen_expr(g, e->new.n)) return false;
			cgen_write(g, ")");
		} else {
			Type *t = &e->new.type;
			cgen_write(g, "((");
			if (!cgen_type_pre(g, &e->type, e->where))
				return false;
			if (!cgen_type_post(g, &e->type, e->where))
				return false;
			cgen_write(g, ")e__calloc(1, sizeof(");
			if (!cgen_type_pre(g, t, e->where))
				return false;
			if (!cgen_type_post(g, t, e->where))
				return false;
			cgen_write(g, ")))");
		}
	} break;
	case EXPR_IF:
		if (e->type.kind != TYPE_VOID)
			cgen_ident_id(g, e->if_.c.id);
		break;
	case EXPR_WHILE:
		if (e->type.kind != TYPE_VOID)
			cgen_ident_id(g, e->while_.c.id);
		break;
	case EXPR_BLOCK:
		if (e->type.kind != TYPE_VOID)
			cgen_ident_id(g, e->block_ret_id);
		break;
	case EXPR_CALL:
		if (cgen_uses_ptr(&e->type)) {
			cgen_ident_id(g, e->call.c.id);
		} else {
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
		}
		break;
	case EXPR_C: {
		Value val;
		if (!eval_expr(g->evalr, e->c.code, &val))
			return false;
		cgen_indent(g);
		fwrite(val.slice.data, 1, (size_t)val.slice.n, cgen_writing_to(g));
	} break;
	case EXPR_DSIZEOF:
	case EXPR_DALIGNOF: {
		Value val;
		if (!eval_expr(g->evalr, e, &val))
			return false;
		cgen_write(g, "%"PRId64, val.i64);
	} break;
	case EXPR_CAST: {
		Type *from = &e->cast.expr->type;
		Type *to = &e->cast.type;
		if (from->kind == TYPE_USER || to->kind == TYPE_USER) {
			/* don't need to cast; they're the same type in C */
			if (!cgen_expr(g, e->cast.expr))
				return false;
		} else {
			cgen_write(g, "((");
			cgen_type_pre(g, to, e->where);
			cgen_type_post(g, to, e->where);
			cgen_write(g, ")(");
			if (!cgen_expr(g, e->cast.expr))
				return false;
			cgen_write(g, ")");
			if (from->kind == TYPE_SLICE /* casting from a slice to a non-slice */
				&& to->kind != TYPE_SLICE) 
				cgen_write(g, ".data");
			cgen_write(g, ")");
		}
	} break;
	case EXPR_TUPLE:
		/* the only time this should happen is if you're stating
		   a tuple, e.g. 3, 5;, but we've errored about that before
		   (the comma operator does not exist in toc!) */
	case EXPR_TYPE:
		assert(0);
		break;
	case EXPR_FN: {
		if (g->block != NULL) {
			Expression **eptr = arr_add(&g->anon_fns);
			*eptr = e;
		}
		cgen_fn_name(g, &e->fn);
	} break;
	case EXPR_SLICE:
		cgen_ident_id(g, e->slice.c.id);
		break;
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
	case TYPE_STRUCT:
		cgen_write(g, "{0}");
		break;
	case TYPE_USER:
		cgen_zero_value(g, type_inner(t));
		break;
	case TYPE_TYPE:
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
	fn_enter(f, 0);
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

static bool cgen_val_ptr_pre(CGenerator *g, void *v, Type *t, Location where) {
	switch (t->kind) {
	case TYPE_SLICE: {
		Slice *s = (Slice *)v;
		for (I64 i = 0; i < s->n; i++) {
			if (!cgen_val_ptr_pre(g, (char *)s->data + (U64)i * compiler_sizeof(t->slice), t->slice, where))
				return false;
		}
		if (!cgen_type_pre(g, t->slice, where)) return false;
		cgen_write(g, "(d%p_[])", v); /* TODO: improve this somehow? */
		if (!cgen_type_post(g, t->slice, where)) return false;
		cgen_write(g, " = {");
		for (I64 i = 0; i < s->n; i++) {
			if (i) cgen_write(g, ", ");
			if (!cgen_val_ptr(g, (char *)s->data + (U64)i * compiler_sizeof(t->slice), t->slice, where))
				return false;
		}
		cgen_write(g, "};");
		cgen_nl(g);
	} break;
	case TYPE_ARR:
		for (size_t i = 0; i < t->arr.n; i++) {
			if (!cgen_val_ptr_pre(g, (char *)*(void **)v + i * compiler_sizeof(t->arr.of), t->arr.of, where))
				return false;
		}
		break;
	case TYPE_USER:
		if (!cgen_val_ptr_pre(g, v, type_inner(t), where))
			return false;
		break;
	case TYPE_FN:
	case TYPE_TYPE:
	case TYPE_UNKNOWN:
	case TYPE_TUPLE:
	case TYPE_VOID:
	case TYPE_BUILTIN:
	case TYPE_PTR:
	case TYPE_STRUCT:
		break;
	}
	return true;
}

/* generate a value from a pointer */
static bool cgen_val_ptr(CGenerator *g, void *v, Type *t, Location where) {
	switch (t->kind) {
	case TYPE_TUPLE:
	case TYPE_VOID:
	case TYPE_TYPE:
		assert(0);
		return false;
	case TYPE_UNKNOWN:
		err_print(where, "Cannot determine type.");
		return false;
	case TYPE_ARR:
		cgen_write(g, "{");
		for (size_t i = 0; i < t->arr.n; i++) {
			if (i) cgen_write(g, ", ");
			if (!cgen_val_ptr(g, *(char **)v + i * compiler_sizeof(t->arr.of), t->arr.of, where))
				return false;
		}
		cgen_write(g, "}");
		break;
	case TYPE_SLICE:
		cgen_write(g, "{d%p_, %lu}", v, ((Slice *)v)->n);
		break;
	case TYPE_STRUCT:
		err_print(where, "TODO");
		/* TODO */
		break;
	case TYPE_FN:
		cgen_fn_name(g, *(FnExpr **)v);
		break;
	case TYPE_PTR:
		err_print(where, "Cannot bring compile time pointer to runtime.");
		return false;
	case TYPE_BUILTIN:
		switch (t->builtin) {
		case BUILTIN_I8: cgen_write(g, "%"PRId8, *(I8 *)v); break;
		case BUILTIN_U8: cgen_write(g, "%"PRIu8, *(U8 *)v); break;
		case BUILTIN_I16: cgen_write(g, "%"PRId16, *(I16 *)v); break;
		case BUILTIN_U16: cgen_write(g, "%"PRIu16, *(U16 *)v); break;
		case BUILTIN_I32: cgen_write(g, "%"PRId32, *(I32 *)v); break;
		case BUILTIN_U32: cgen_write(g, "%"PRIu32, *(U32 *)v); break;
		case BUILTIN_I64: cgen_write(g, "%"PRId64, *(I64 *)v); break;
		case BUILTIN_U64: cgen_write(g, "%"PRIu64, *(U64 *)v); break;
		case BUILTIN_F32: cgen_write(g, F32_FMT, *(F32 *)v); break;
		case BUILTIN_F64: cgen_write(g, F64_FMT, *(F64 *)v); break;
		case BUILTIN_CHAR: cgen_write(g, "\\x%02x", *(char *)v); break;
		case BUILTIN_BOOL: cgen_write(g, "%s", *(bool *)v ? "true" : "false"); break;
		}
		break;
	case TYPE_USER:
		if (!cgen_val_ptr(g, v, type_inner(t), where))
			return false;
		break;
	}
	return true;
}

static bool cgen_val_pre(CGenerator *g, Value *v, Type *t, Location where) {
	return cgen_val_ptr_pre(g, v, t, where);
}

/* generates a value fit for use as an initializer */
static bool cgen_val(CGenerator *g, Value *v, Type *t, Location where) {
	/* 
	   Because Value is a union, a pointer to v works as a pointer to any member.
	   As a result, this function is only needed for type checking.
	 */
	return cgen_val_ptr(g, v, t, where);
}


static bool cgen_decl(CGenerator *g, Declaration *d) {
	int has_expr = d->flags & DECL_FLAG_HAS_EXPR;
	bool is_tuple = d->type.kind == TYPE_TUPLE;
	if (cgen_fn_is_direct(g, d)) {
		if (!cgen_fn(g, &d->expr.fn, d->where))
			return false;
	} else if ((d->flags & DECL_FLAG_CONST) || g->block == NULL) {
		/* declarations where we use a value */
		for (size_t idx = 0; idx < arr_len(d->idents); idx++) {
		    Identifier i = d->idents[idx];
			Type *type = is_tuple ? &d->type.tuple[idx] : &d->type;
			Value *val = is_tuple ? &d->val.tuple[idx] : &d->val;
			if (type->kind == TYPE_TYPE) {
				/* handled in decls_cgen */
				continue;
			}
			if (!cgen_val_pre(g, val, type, d->where))
				return false;
			if (g->block != NULL)
				cgen_write(g, "static ");
			if (!cgen_type_pre(g, type, d->where)) return false;
			cgen_write(g, " ");
			cgen_ident(g, i);
			if (!cgen_type_post(g, type, d->where)) return false;
			if (has_expr) {
				cgen_write(g, " = ");
				if (!cgen_val(g, val, type, d->where))
					return false;
			}
			cgen_write(g, ";");
			cgen_nl(g);
		}
	} else {
		/* declarations where we use an expression */
		for (size_t idx = 0; idx < arr_len(d->idents); idx++) {
			Identifier i = d->idents[idx];
			Type *type = d->type.kind == TYPE_TUPLE ? &d->type.tuple[idx] : &d->type;
			if (!cgen_type_pre(g, type, d->where)) return false;
			cgen_write(g, " ");
			cgen_ident(g, i);
			if (!cgen_type_post(g, type, d->where)) return false;
			if (!has_expr) {
				cgen_write(g, " = ");
		    	cgen_zero_value(g, type);
			}
				
			cgen_write(g, "; ");
		}
		if (has_expr) {
			if (d->expr.type.kind == TYPE_TUPLE) {
				if (!cgen_set_tuple(g, NULL, d->idents, NULL, &d->expr)) return false;
			} else {
				cgen_write(g, "{");
		 
				cgen_nl(g);
				if (!cgen_type_pre(g, &d->type, d->expr.where)) return false;
				cgen_write(g, " expr__");
				if (!cgen_type_post(g, &d->type, d->expr.where)) return false;
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
		if (!cgen_expr_pre(g, ret)) return false;
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
		if (!cgen_ret(g, s->ret.flags & RET_HAS_EXPR ? &s->ret.expr : NULL))
			return false;
		break;
	}
	return true;
}

static bool cgen_file(CGenerator *g, ParsedFile *f) {
	g->block = NULL;
	g->file = f;
	cgen_write(g, "#include <stdint.h>\n"
			   "#include <stdlib.h>\n"
			   "#include <stdio.h>\n"
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
			   "static slice_ mkslice_(void *data, u64 n) { slice_ ret; ret.data = data; ret.n = n; return ret; }\n"
			   "static void *e__calloc(size_t n, size_t sz) { void *ret = calloc(n, sz); if (!ret) { fprintf(stderr, \"Out of memory.\\n\"); abort(); } return ret; }\n"
			   "#define false ((bool)0)\n"
			   "#define true ((bool)1)\n\n\n");
	if (!typedefs_file(g, f))
		return false;
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
