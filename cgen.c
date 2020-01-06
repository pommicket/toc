/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static void cgen_create(CGenerator *g, FILE *out, Identifiers *ids, Evaluator *ev, Allocator *allocr) {
	g->outc = out;
	g->ident_counter = 1; /* some places use 0 to mean no id */
	g->main_ident = ident_get(ids, "main");
	g->evalr = ev;
	g->will_indent = true;
	g->indent_lvl = 0;
	g->idents = ids;
	g->allocr = allocr;
}

static bool cgen_stmt(CGenerator *g, Statement *s);
enum {
	  CGEN_BLOCK_NOENTER = 0x01, /* should cgen_block actually enter and exit the block? */
	  CGEN_BLOCK_NOBRACES = 0x02, /* should it use braces? */
};
static bool cgen_block(CGenerator *g, Block *b, const char *ret_name, uint16_t flags);
static bool cgen_expr_pre(CGenerator *g, Expression *e);
static bool cgen_expr(CGenerator *g, Expression *e);
static bool cgen_set_tuple(CGenerator *g, Expression *exprs, Identifier *idents, const char *prefix, Expression *to);
static bool cgen_type_pre(CGenerator *g, Type *t, Location where);
static bool cgen_type_post(CGenerator *g, Type *t, Location where);
static bool cgen_decl(CGenerator *g, Declaration *d);
static bool cgen_ret(CGenerator *g, Expression *ret);
static bool cgen_val(CGenerator *g, Value v, Type *t, Location where);
static bool cgen_val_pre(CGenerator *g, Value v, Type *t, Location where);
static bool cgen_val_ptr(CGenerator *g, void *v, Type *t, Location where);
static bool cgen_defs_block(CGenerator *g, Block *b);
static bool cgen_defs_decl(CGenerator *g, Declaration *d);

#define cgen_recurse_subexprs_fn_simple(fn, decl_f, block_f)	\
	if (!fn_enter(fn, 0)) return false;							\
	FnExpr *prev_fn = g->f##n;									\
	g->f##n = fn;												\
	arr_foreach(fn->params, Declaration, param)					\
		if (!decl_f(g, param))									\
			return false;										\
	arr_foreach(fn->ret_decls, Declaration, r)					\
		if (!decl_f(g, r))										\
			return false;										\
	if (!block_f(g, &fn->body))									\
		return false;											\
	fn_exit(fn);												\
	g->f##n = prev_fn;										

/* calls f on every sub-expression of e, block_f on every sub-block, and decl_f on every sub-declaration. */
#define cgen_recurse_subexprs(g, e, f, block_f, decl_f)					\
	switch (e->kind) {													\
	case EXPR_TYPE:														\
	case EXPR_VAL:														\
	case EXPR_C:														\
	case EXPR_DSIZEOF:													\
	case EXPR_DALIGNOF:													\
	case EXPR_IDENT:													\
	case EXPR_LITERAL_BOOL:												\
	case EXPR_LITERAL_INT:												\
	case EXPR_LITERAL_STR:												\
	case EXPR_LITERAL_CHAR:												\
	case EXPR_LITERAL_FLOAT:											\
	break;																\
	case EXPR_UNARY_OP:													\
	if (!f(g, e->unary.of)) return false;								\
	break;																\
	case EXPR_BINARY_OP:												\
	if (!f(g, e->binary.lhs)) return false;								\
	if (e->binary.op != BINARY_DOT)										\
		if (!f(g, e->binary.rhs))										\
			return false;												\
	break;																\
	case EXPR_CAST:														\
	if (!f(g, e->cast.expr))											\
		return false;													\
	break;																\
	case EXPR_CALL:														\
	if (!f(g, e->call.fn))												\
		return false;													\
	arr_foreach(e->call.arg_exprs, Expression, arg)						\
		if (!f(g, arg))													\
			return false;												\
	break;																\
	case EXPR_BLOCK:													\
	if (!block_f(g, &e->block))											\
		return false;													\
	break;																\
	case EXPR_IF:														\
	if (e->if_.cond)													\
		if (!f(g, e->if_.cond))											\
			return false;												\
	if (!block_f(g, &e->if_.body))										\
		return false;													\
	if (e->if_.next_elif)												\
		if (!f(g, e->if_.next_elif))									\
			return false;												\
	break;																\
	case EXPR_WHILE:													\
	if (e->while_.cond)													\
		if (!f(g, e->while_.cond))										\
			return false;												\
	if (!block_f(g, &e->while_.body))									\
		return false;													\
	break;																\
	case EXPR_EACH: {													\
		EachExpr *ea = e->each;											\
		if (!each_enter(e)) return false;								\
		if (ea->flags & EACH_IS_RANGE) {								\
			if (!f(g, ea->range.from))									\
				return false;											\
			if (ea->range.to && !f(g, ea->range.to))					\
				return false;											\
			/* step is a value, not an expression */					\
		} else {														\
			if (!f(g, ea->of))											\
				return false;											\
		}																\
		if (!block_f(g, &ea->body)) return false;						\
		each_exit(e);													\
	} break;															\
	case EXPR_TUPLE:													\
	arr_foreach(e->tuple, Expression, x)								\
		if (!f(g, x))													\
			return false;												\
	break;																\
	case EXPR_SLICE:													\
	if (!f(g, e->slice.of)) return false;								\
	if (e->slice.from && !f(g, e->slice.from))							\
		return false;													\
	if (e->slice.to && !f(g, e->slice.to))								\
		return false;													\
	break;																\
	case EXPR_FN: {														\
		FnExpr *fn = e->fn;												\
		if (e->type.fn.constness) {										\
			Instance **data = fn->instances.data;						\
			for (U64 i = 0; i < fn->instances.cap; ++i) {				\
				if (fn->instances.occupied[i]) {						\
					cgen_recurse_subexprs_fn_simple((&(*data)->fn), decl_f, block_f); \
				}														\
				++data;													\
			}															\
	 	} else {														\
			cgen_recurse_subexprs_fn_simple(fn, decl_f, block_f);		\
		}																\
	} break;															\
	case EXPR_NEW:														\
		if (e->new.n && !f(g, e->new.n))								\
			return false;												\
		break;															\
	}

static bool cgen_block_enter(CGenerator *g, Block *b) {
	g->block = b;
	Statement *stmts;
	if (b == NULL) {
		stmts = g->file->stmts;
	} else {
		stmts = b->stmts;
	}
	if (b) ++g->indent_lvl;
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
	if (b) --g->indent_lvl;
	g->block = into;
}

static inline FILE *cgen_writing_to(CGenerator *g) {
	return g->outc;	/* for now */
}

/* indent iff needed */
static inline void cgen_indent(CGenerator *g) {
	if (g->will_indent) {
		for (int i = 0; i < g->indent_lvl; ++i)
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
	return g->block == NULL && (d->flags & DECL_HAS_EXPR) && d->expr.kind == EXPR_FN && arr_len(d->idents) == 1;
}

static bool cgen_uses_ptr(Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_TUPLE:
	case TYPE_STRUCT:
	case TYPE_ARR:
		return true;
	case TYPE_BUILTIN:
	case TYPE_PTR:
	case TYPE_FN:
	case TYPE_SLICE:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TYPE:
		return false;
	case TYPE_EXPR:
		break;
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
		fprint_ident_reduced_charset(cgen_writing_to(g), i);
	}
}


static char *cgen_ident_to_str(Identifier i) {
	return ident_to_str(i);
}


/* buffer should be at least 32 bytes */
static inline void cgen_ident_id_to_str(char *buffer, IdentID id) {
	snprintf(buffer, 32, "a%lu_", (unsigned long)id);
}

static inline Identifier cgen_ident_id_to_ident(CGenerator *g, IdentID id) {
	char s[32];
	cgen_ident_id_to_str(s, id);
	return ident_get(g->idents, s);
}

static bool cgen_type_pre(CGenerator *g, Type *t, Location where) {
	assert(t->flags & TYPE_IS_RESOLVED);
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
		cgen_write(g, "struct ");
		if (t->struc->c.name) {
			cgen_ident(g, t->struc->c.name);
		} else if (t->struc->c.id) {
			cgen_ident_id(g, t->struc->c.id);
		} else {
			assert(0);
		}
		break;
	case TYPE_TUPLE:
	case TYPE_TYPE:
	case TYPE_EXPR:
		/* We should never try to generate this type */
		assert(0);
		return false;
	}
	return true;
}

static bool cgen_type_post(CGenerator *g, Type *t, Location where) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_PTR:
		cgen_write(g, ")");
		if (!cgen_type_post(g, t->ptr, where))
			return false;
		break;
	case TYPE_ARR:
		assert(t->flags & TYPE_IS_RESOLVED);
	    cgen_write(g, "[%lu])", (unsigned long)t->arr.n);
		if (!cgen_type_post(g, t->arr.of, where))
			return false;
		break;
	case TYPE_FN: {
		bool out_param = cgen_uses_ptr(&t->fn.types[0]);
		cgen_write(g, ")(");
		for (size_t i = 1; i < arr_len(t->fn.types); ++i) {
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
	case TYPE_STRUCT:
		break;
	case TYPE_EXPR:
		assert(0);
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

static inline void cgen_fn_instance_number(CGenerator *g, U64 instance) {
	cgen_write(g, U64_FMT"_", instance);
}

/* does this type have a type type in it? (e.g. [5]Type, &&Type) */
static bool type_contains_type(Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_BUILTIN:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
		return false;
	case TYPE_TYPE:
		return true;
	case TYPE_PTR:
		return type_contains_type(t->ptr);
	case TYPE_SLICE:
		return type_contains_type(t->slice);
	case TYPE_ARR:
		return type_contains_type(t->arr.of);
	case TYPE_FN:
		arr_foreach(t->fn.types, Type, sub)
			if (type_contains_type(sub))
				return true;
		return false;
	case TYPE_TUPLE:
		arr_foreach(t->tuple, Type, sub)
			if (type_contains_type(sub))
				return true;
		return false;
	case TYPE_STRUCT:
		arr_foreach(t->struc->fields, Field, f)
			if (type_contains_type(f->type))
				return true;
		return false;
	case TYPE_EXPR: break;
	}
	assert(0);
	return false;
}

/* should we generate this function? (or is it just meant for compile time) */
static bool cgen_should_gen_fn(FnExpr *f) {
	if (f->ret_decls) {
		arr_foreach(f->ret_decls, Declaration, decl)
			if (type_contains_type(&decl->type))
				return false;
		return true;
	} else {
		return !type_contains_type(&f->ret_type);
	}
}

/* unless f has const/semi-const args, instance and which_are_const can be set to 0 */
static bool cgen_fn_header(CGenerator *g, FnExpr *f, Location where, U64 instance, U64 which_are_const) {
	bool out_param = cgen_uses_ptr(&f->ret_type);
	bool any_params = false;
	assert(cgen_should_gen_fn(f));
	if (!f->export.id) /* local to this translation unit */
		cgen_write(g, "static ");
	if (out_param) {
		cgen_write(g, "void ");
	} else {
		if (!cgen_type_pre(g, &f->ret_type, where)) return false;
		cgen_write(g, " ");
	}
	cgen_fn_name(g, f);
	if (instance) {
		cgen_fn_instance_number(g, instance);
	}
	if (!out_param) {
		if (!cgen_type_post(g, &f->ret_type, where)) return false;
	}
	cgen_write(g, "(");
	int semi_const_idx = 0;
	bool any_args = false;
	arr_foreach(f->params, Declaration, d) {
		if (!(d->flags & DECL_IS_CONST) && !((d->flags & DECL_SEMI_CONST)
											 && (which_are_const & (((U64)1) << semi_const_idx++)))) {
			long idx = 0;
			arr_foreach(d->idents, Identifier, i) {
				if (any_args) 
					cgen_write(g, ", ");
				any_args = true;
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
	}
	if (out_param) {
		any_args = true;
		if (f->ret_type.kind == TYPE_TUPLE) {
			/* multiple return variables */
			for (size_t i = 0; i < arr_len(f->ret_type.tuple); ++i) {
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
	if (!any_args)
		cgen_write(g, "void");
	cgen_write(g, ")");
	return true;
}


/* 
   Either set_expr or set_str should be NULL and either to_expr or to_str should be NULL 
   Also, set_str and/or to_str should be NULL
   this DOES NOT call cgen_expr_pre for set_expr or to_expr
*/
static bool cgen_set(CGenerator *g, Expression *set_expr, const char *set_str, Expression *to_expr,
					 const char *to_str) {
	Type *type;
	Location where;
	if (set_expr) {
		type = &set_expr->type;
		where = set_expr->where;
	} else {
		assert(to_expr);
		type = &to_expr->type;
		where = to_expr->where;
	}
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
		cgen_write(g, "for (i = 0; i < %lu; ++i) arr__out[i] = arr__in[i];", (unsigned long)type->arr.n);
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
	case TYPE_TYPE:
	case TYPE_EXPR:
		assert(0);
		return false;
	}
	return true;
}

/* one of exprs, idents, and prefix should be NULL. does NOT call cgen_expr_pre for to/exprs */
static bool cgen_set_tuple(CGenerator *g, Expression *exprs, Identifier *idents, const char *prefix, Expression *to) {
	IdentID prefix_id; /* ID of prefix for block */
	switch (to->kind) {
	case EXPR_VAL:
		assert(0); /* never needed at the moment */
		break;
	case EXPR_TUPLE:
		/* e.g. a, b = 3, 5; */
		for (size_t i = 0; i < arr_len(to->tuple); ++i) {
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
		if (to->call.instance)
			cgen_fn_instance_number(g, to->call.instance->c.id);
		cgen_write(g, "(");
		bool any_args = false;
		Constness *constness = to->call.fn->type.fn.constness;
		int i = 0;
		arr_foreach(to->call.arg_exprs, Expression, arg) {
			if (!constness || !arg_is_const(arg, constness[i])) {
				if (any_args)
					cgen_write(g, ", ");
				any_args = true;
				if (!cgen_expr(g, arg))
					return false;
			}
			++i;
		}
		/* out params */
		size_t len = exprs ? arr_len(exprs) : arr_len(idents);

		for (i = 0; i < (int)len; ++i) {
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
				cgen_write(g, "&(%s%d_)", prefix, i);
			}
		}
		cgen_writeln(g, "); ");
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
	case EXPR_EACH:
		prefix_id = to->each->c.id;
		goto prefixed;
	prefixed:
		for (unsigned long i = 0; i < (unsigned long)arr_len(to->type.tuple); ++i) {
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

static bool cgen_expr_pre(CGenerator *g, Expression *e) {
	IdentID id = 0;
	char ret_name[64];
	switch (e->kind) {
	case EXPR_IF:
	case EXPR_WHILE:
	case EXPR_EACH:
	case EXPR_BLOCK: {
		id = g->ident_counter++;
		
		cgen_ident_id_to_str(ret_name, id);
		char *p = ret_name + strlen(ret_name);
		if (e->type.kind != TYPE_VOID) {
			if (e->type.kind == TYPE_TUPLE) {
				for (unsigned long i = 0; i < arr_len(e->type.tuple); ++i) {
					sprintf(p, "%lu_", i);
					if (!cgen_type_pre(g, &e->type.tuple[i], e->where)) return false;
					cgen_write(g, " %s", ret_name);
					if (!cgen_type_post(g, &e->type.tuple[i], e->where)) return false;
					cgen_write(g, "; ");
				}
			
			} else {
				if (!cgen_type_pre(g, &e->type, e->where)) return false;
				cgen_write(g, " %s", ret_name);
				if (!cgen_type_post(g, &e->type, e->where)) return false;
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
	case EXPR_EACH: {
		EachExpr *ea = e->each;
		int is_range = ea->flags & EACH_IS_RANGE;
		if (is_range) {
			if (!cgen_expr_pre(g, ea->range.from)) return false;
			if (ea->range.to && !cgen_expr_pre(g, ea->range.to)) return false;
		} else {
			if (!cgen_expr_pre(g, ea->of)) return false;
		}
	    
		ea->c.id = id;
		if (!each_enter(e)) return false;
		cgen_write(g, "{");
		if (is_range) {
			if (ea->range.to) {
				/* pre generate to */
				if (!cgen_type_pre(g, &ea->type, e->where)) return false;
				cgen_write(g, " to_");
				if (!cgen_type_post(g, &ea->type, e->where)) return false;
				cgen_write(g, " = ");
				if (!cgen_expr(g, ea->range.to))
					return false;
				cgen_write(g, "; ");
			}
			
			/* set value to from */
			if (ea->value) {
				if (!cgen_type_pre(g, &ea->type, e->where)) return false;
				cgen_write(g, " ");
				cgen_ident(g, ea->value);
				if (!cgen_type_post(g, &ea->type, e->where)) return false;
				cgen_write(g, "; ");
				Expression val_expr;
				val_expr.flags = EXPR_FOUND_TYPE;
				val_expr.kind = EXPR_IDENT;
				val_expr.ident = ea->value;
				val_expr.type = ea->type;
				if (!cgen_set(g, &val_expr, NULL, ea->range.from, NULL))
					return false;
			} else {
				if (!cgen_type_pre(g, &ea->type, e->where)) return false;
				cgen_write(g, " val_");
				if (!cgen_type_post(g, &ea->type, e->where)) return false;
				cgen_write(g, "; ");
				if (!cgen_set(g, NULL, "val_", ea->range.from, NULL))
					return false;
			}
		} else {
			/* pre-generate of */
			if (!cgen_type_pre(g, &ea->of->type, e->where))
				return false;
			cgen_write(g, " of_");
			if (!cgen_type_post(g, &ea->of->type, e->where))
				return false;
			
			cgen_write(g, "; ");
			
			if (!cgen_set(g, NULL, "of_", ea->of, NULL))
				return false;
		}
		cgen_write(g, "for (");
		if (ea->index || !is_range) {
			cgen_write(g, "i64 ");
			if (ea->index)
				cgen_ident(g, ea->index);
			else
				cgen_write(g, "i_");
			cgen_write(g, " = 0");
		}
		cgen_write(g, "; ");
		bool uses_ptr = false;
		Type *of_type = NULL;
		if (!(is_range && !ea->range.to)) { /* if it's finite */
			if (is_range) {
				if (ea->value)
					cgen_ident(g, ea->value);
				else
					cgen_write(g, "val_");
				bool positive_step
					= ea->range.stepval == NULL || val_is_nonnegative(ea->range.stepval, &ea->type);
				cgen_write(g, " %c= to_", positive_step ? '<' : '>');
			} else {
				if (ea->index)
					cgen_ident(g, ea->index);
				else
					cgen_write(g, "i_");
				cgen_write(g, " < ");
				of_type = &ea->of->type;
				uses_ptr = of_type->kind == TYPE_PTR;
				if (uses_ptr) {
					of_type = of_type->ptr;
				}
				switch (of_type->kind) {
				case TYPE_ARR:
					cgen_write(g, "%lu", (unsigned long)of_type->arr.n);
					break;
				case TYPE_SLICE:
					cgen_write(g, "of_%sn", uses_ptr ? "->" : ".");
					break;
				default: assert(0); break;
				}
			}
		}
		cgen_write(g, "; ");
		if (is_range) {
			if (ea->range.stepval) {
				if (!cgen_val_pre(g, *ea->range.stepval, &ea->type, e->where))
					return false;
			}
			if (ea->value)
				cgen_ident(g, ea->value);
			else
				cgen_write(g, "val_");
			cgen_write(g, " += ");
			if (ea->range.stepval) {
				if (!cgen_val(g, *ea->range.stepval, &ea->type, e->where))
					return false;
			} else {
				cgen_write(g, "1");
			}
			if (ea->index) cgen_write(g, ", ");
		}
		if (ea->index || !is_range) {
			if (ea->index)
				cgen_ident(g, ea->index);
			else
				cgen_write(g, "i_");
			cgen_write(g, "++");
		}
		cgen_write(g, ") {");
		cgen_nl(g);
		if (ea->value) {
			if (!is_range) {
				/* necessary for iterating over, e.g., an array of arrays */
				if (!cgen_type_pre(g, &ea->type, e->where))
					return false;
				if (uses_ptr)
					cgen_write(g, " p_");
				else
					cgen_write(g, "(*p_)");
				if (!cgen_type_post(g, &ea->type, e->where))
					return false;
				cgen_write(g, " = ");
				if (of_type->kind == TYPE_SLICE) {
					cgen_write(g, "((");
					if (!cgen_type_pre(g, &ea->type, e->where)) return false;
					if (!uses_ptr) cgen_write(g, "(*)");
					if (!cgen_type_post(g, &ea->type, e->where)) return false;
					cgen_write(g, ")of_%sdata) + ", uses_ptr ? "->" : ".");
					if (ea->index)
						cgen_ident(g, ea->index);
					else
						cgen_write(g, "i_");
				} else {
					cgen_write(g, "&%sof_%s[", uses_ptr ? "(*" : "", uses_ptr ? ")" : "");
					if (ea->index)
						cgen_ident(g, ea->index);
					else
						cgen_write(g, "i_");
					cgen_write(g, "]");
				}
				cgen_write(g, "; ");
				if (!cgen_type_pre(g, &ea->type, e->where)) return false;
				cgen_write(g, " ");
				cgen_ident(g, ea->value);
				if (!cgen_type_post(g, &ea->type, e->where)) return false;
				cgen_write(g, "; ");
				if (uses_ptr) {
					cgen_ident(g, ea->value);
					cgen_write(g, " = p_;");
					cgen_nl(g);
				} else {
					Expression set_expr;
					set_expr.kind = EXPR_IDENT;
					set_expr.ident = ea->value;
					set_expr.type = ea->type;
					set_expr.flags = EXPR_FOUND_TYPE;
				
					if (!cgen_set(g, &set_expr, NULL, NULL, "(*p_)"))
						return false;
				}
			}
		}
		if (!cgen_block(g, &ea->body, ret_name, CGEN_BLOCK_NOBRACES))
			return false;
		cgen_write(g, "}}");
		each_exit(e);
	} break;
	case EXPR_BLOCK:
		e->block_ret_id = id;
		if (!cgen_block(g, &e->block, ret_name, 0))
			return false;
		break;
	case EXPR_CALL: {
		if (!cgen_expr_pre(g, e->call.fn)) return false;
		int i = 0;
	    Constness *constness = e->call.fn->type.fn.constness;
		arr_foreach(e->call.arg_exprs, Expression, arg) {
			if (!constness || !arg_is_const(arg, constness[i])) {
				if (!cgen_expr_pre(g, arg)) return false;
			}
			++i;
		}
		if (cgen_uses_ptr(&e->type)
			&& e->type.kind != TYPE_TUPLE) {
			e->call.c.id = g->ident_counter++;
			if (!cgen_type_pre(g, &e->type, e->where)) return false;
			cgen_write(g, " ");
			cgen_ident_id(g, e->call.c.id);
			if (!cgen_type_post(g, &e->type, e->where)) return false;
			cgen_write(g, ";"); cgen_nl(g);
			if (!cgen_expr(g, e->call.fn)) return false;
			if (e->call.instance) {
				cgen_fn_instance_number(g, e->call.instance->c.id);
			}
			cgen_write(g, "(");
			bool any_args = false;
			i = 0;
			arr_foreach(e->call.arg_exprs, Expression, arg) {
				if (!constness || !arg_is_const(arg, constness[i])) {
					if (any_args) cgen_write(g, ", ");
					any_args = true;
					if (!cgen_expr(g, arg))
						return false;
				}
				++i;
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
		if (s->of->type.kind == TYPE_SLICE) {
			if (!cgen_expr(g, s->of))
				return false;
		} else {
			assert(s->of->type.kind == TYPE_ARR);
			cgen_write(g, "mkslice_(");
			if (!cgen_expr(g, s->of))
				return false;
			cgen_write(g, ", " U64_FMT, s->of->type.arr.n);
			cgen_write(g, ")");
		}
		cgen_write(g, "; i64 ");
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
		cgen_write(g, ".data");
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
	case EXPR_VAL:
		if (!cgen_val_pre(g, e->val, &e->type, e->where))
			return false;
		if (!cgen_type_pre(g, &e->type, e->where)) return false;
		e->val_c_id = g->ident_counter++;
		cgen_write(g, " ");
		cgen_ident_id(g, e->val_c_id);
		if (!cgen_type_post(g, &e->type, e->where)) return false;
		cgen_write(g, " = ");
		if (!cgen_val(g, e->val, &e->type, e->where))
			return false;
		cgen_write(g, ";");
		cgen_nl(g);
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
		cgen_write(g, U64_FMT, e->intl);
		break;
	case EXPR_LITERAL_STR: {
		size_t c;
		cgen_write(g, "mkslice_(\"");
		for (c = 0; c < e->strl.len; ++c) {
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
	case EXPR_IDENT: {
		bool handled = false;
		if (e->type.kind == TYPE_FN) {
			/* generate the right function name, because it might be anonymous */
			IdentDecl *idecl = ident_decl(e->ident);
			if (idecl && idecl->kind == IDECL_DECL) {
				Declaration *d = idecl->decl;
				if (d->flags & DECL_IS_CONST) {
					int index = decl_ident_index(d, e->ident);
					Value fn_val = *decl_val_at_index(d, index);
					FnExpr *fn = fn_val.fn;
					Expression fn_expr;
					
					fn_expr.kind = EXPR_FN;
					fn_expr.fn = allocr_malloc(g->allocr, sizeof *fn_expr.fn);
					*fn_expr.fn = *fn;
					fn_expr.flags = EXPR_FOUND_TYPE;
					fn_expr.type = *decl_type_at_index(d, index);
					
					if (!cgen_expr(g, &fn_expr))
						return false;
					handled = true;
				}
			}
		}
		if (!handled) {
			cgen_ident(g, e->ident);
		}
	} break;
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
		case BINARY_SET_ADD:
			s = "+="; break;
		case BINARY_SET_SUB:
			s = "-="; break;
		case BINARY_SET_MUL:
			s = "*="; break;
		case BINARY_SET_DIV:
			s = "/="; break;
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
			bool is_ptr = e->binary.lhs->type.kind == TYPE_PTR;
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
			cgen_write(g, "free_(");
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
			cgen_write(g, ", (i64)sizeof(");
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
	case EXPR_EACH:
		if (e->type.kind != TYPE_VOID)
			cgen_ident_id(g, e->each->c.id);
		break;
	case EXPR_CALL:
		if (cgen_uses_ptr(&e->type)) {
			cgen_ident_id(g, e->call.c.id);
		} else {
			FnType *fn_type = &e->call.fn->type.fn;
			cgen_write(g, "(");
			if (!cgen_expr(g, e->call.fn))
				return false;
			if (e->call.instance) {
				cgen_fn_instance_number(g, e->call.instance->c.id);
			}
			cgen_write(g, "(");
			bool first_arg = true;
		    int i = 0;
			arr_foreach(e->call.arg_exprs, Expression, arg) {
				if (!fn_type->constness || !arg_is_const(arg, fn_type->constness[i])) {
					if (!first_arg)
						cgen_write(g, ", ");
					first_arg = false;
					if (!cgen_expr(g, arg))
						return false;
				}
				++i;
			}
			cgen_write(g, "))");
		}
		break;
	case EXPR_C: {
		Expression *code = e->c.code;
		assert(code->kind == EXPR_VAL);
		cgen_indent(g);
		fwrite(code->val.slice.data, 1, (size_t)code->val.slice.n, cgen_writing_to(g));
	} break;
	case EXPR_CAST: {
		Type *from = &e->cast.expr->type;
		Type *to = &e->cast.type;
		if (to->kind == TYPE_ARR) {
			/* can't cast to array type */
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
	case EXPR_DSIZEOF:
	case EXPR_DALIGNOF: /* handled by types.c */
	case EXPR_TUPLE:
		/* the only time this should happen is if you're stating
		   a tuple, e.g. 3, 5;, but we've errored about that before
		*/
	case EXPR_TYPE:
		assert(0);
		break;
	case EXPR_FN: {
		FnExpr *f = e->fn;
		cgen_fn_name(g, f);
	} break;
	case EXPR_SLICE:
		cgen_ident_id(g, e->slice.c.id);
		break;
	case EXPR_VAL:
		cgen_ident_id(g, e->val_c_id);
		break;
	}
	return true;
}

/*
  ret_name = variable to store block return value in; NULL for none. NOTE:
  functions always call with NULL as ret_name, even if they use out params, for now
  at least. 
*/
static bool cgen_block(CGenerator *g, Block *b, const char *ret_name, U16 flags) {
	Block *prev = g->block;
	if (!(flags & CGEN_BLOCK_NOBRACES)) {
		cgen_write(g, "{");
		cgen_nl(g);
	}
	if (!(flags & CGEN_BLOCK_NOENTER))
		if (!cgen_block_enter(g, b))
			return false;
	arr_foreach(b->stmts, Statement, s)
		if (!cgen_stmt(g, s))
			return false;
	if (b->ret_expr && ret_name) {
		if (!cgen_expr_pre(g, b->ret_expr))
			return false;
		if (b->ret_expr->type.kind == TYPE_TUPLE) {
			if (!cgen_set_tuple(g, NULL, NULL, ret_name, b->ret_expr))
				return false;
		} else {
			if (!cgen_set(g, NULL, ret_name, b->ret_expr, NULL))
				return false;
		}
		cgen_nl(g);
	}
	if (!(flags & CGEN_BLOCK_NOENTER))
		cgen_block_exit(g, prev);
	if (!(flags & CGEN_BLOCK_NOBRACES))
		cgen_write(g, "}");
	return true;
}

static void cgen_zero_value(CGenerator *g, Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
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
	case TYPE_TYPE:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TUPLE:
	case TYPE_EXPR:
		assert(0);
		break;
	}
}

/* pass 0 for instance and NULL for compile_time_args if there are no compile time arguments. */
static bool cgen_fn(CGenerator *g, FnExpr *f, Location where, U64 instance, Value *compile_time_args) {
	/* see also cgen_defs_expr */
	FnExpr *prev_fn = g->fn;
	Block *prev_block = g->block;
	U64 which_are_const = compile_time_args ? compile_time_args->u64 : 0;
	if (!cgen_should_gen_fn(f))
		return true;
	fn_enter(f, 0);
	if (!cgen_fn_header(g, f, where, instance, which_are_const))
		return false;
	g->fn = f;
	cgen_write(g, " {");
	cgen_nl(g);
	if (compile_time_args) {
		int carg_idx = 0;
		++compile_time_args; /* move past which_are_const */
		int semi_const_idx = 0;
		arr_foreach(f->params, Declaration, param) {
			if ((param->flags & DECL_IS_CONST)
				|| ((param->flags & DECL_SEMI_CONST)
					&& (which_are_const & (((U64)1) << semi_const_idx++)))) {
				int i = 0;
				arr_foreach(param->idents, Identifier, ident) {
					Type *type = param->type.kind == TYPE_TUPLE ? &param->type.tuple[i]
						: &param->type;
					Value arg = compile_time_args[carg_idx];
					if (type->kind == TYPE_TYPE) {
						/* don't need to do anything; we'll just use the type's id */
					} else {
						if (!cgen_val_pre(g, arg, type, where))
							return false;
						if (!cgen_type_pre(g, type, where)) return false;
						cgen_write(g, " const ");
						cgen_ident(g, *ident);
						if (!cgen_type_post(g, type, where)) return false;
						cgen_write(g, " = ");
						if (!cgen_val(g, arg, type, where))
							return false;
						cgen_write(g, ";");
						cgen_nl(g);
					}
					++carg_idx;
				}
			}
		}
					
	}
	/* retdecls need to be after compile time arguments to allow fn(x::int) y := x */
	arr_foreach(f->ret_decls, Declaration, d) {
		if (!cgen_decl(g, d))
			return false;
	}
	if (!cgen_block_enter(g, &f->body)) return false;
	if (!cgen_block(g, &f->body, NULL, CGEN_BLOCK_NOENTER | CGEN_BLOCK_NOBRACES))
		return false;
	if (f->ret_decls) {
		/* OPTIM */

		/* long-winded code to generate a return expression using the ret_decls. */
		Expression ret_expr;
		ret_expr.flags = EXPR_FOUND_TYPE;
	    ret_expr.type = f->ret_type;
		if (arr_len(f->ret_decls) == 1
			&& arr_len(f->ret_decls[0].idents) == 1) {
			ret_expr.kind = EXPR_IDENT;
			ret_expr.ident = f->ret_decls[0].idents[0];
		} else {
			ret_expr.kind = EXPR_TUPLE;
			ret_expr.tuple = NULL;
			size_t i = 0;
			arr_foreach(f->ret_decls, Declaration, d) {
				arr_foreach(d->idents, Identifier, ident) {
					Expression *element = arr_add(&ret_expr.tuple);
					element->flags = EXPR_FOUND_TYPE;
					element->kind = EXPR_IDENT;
					element->type = f->ret_type.tuple[i];
					element->ident = *ident;
					++i;
				}
			}
		}
		
		if (!cgen_ret(g, &ret_expr))
			return false;
		if (ret_expr.kind == EXPR_TUPLE)
			arr_clear(&ret_expr.tuple);
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
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_SLICE: {
		Slice *s = (Slice *)v;
		for (I64 i = 0; i < s->n; ++i) {
			if (!cgen_val_ptr_pre(g, (char *)s->data + (U64)i * compiler_sizeof(t->slice), t->slice, where))
				return false;
		}
		if (!cgen_type_pre(g, t->slice, where)) return false;
		cgen_write(g, "(d%p_[])", v); /* TODO: improve this somehow? */
		if (!cgen_type_post(g, t->slice, where)) return false;
		cgen_write(g, " = {");
		for (I64 i = 0; i < s->n; ++i) {
			if (i) cgen_write(g, ", ");
			if (!cgen_val_ptr(g, (char *)s->data + (U64)i * compiler_sizeof(t->slice), t->slice, where))
				return false;
		}
		cgen_write(g, "};");
		cgen_nl(g);
	} break;
	case TYPE_ARR:
		for (size_t i = 0; i < t->arr.n; ++i) {
			if (!cgen_val_ptr_pre(g, (char *)*(void **)v + i * compiler_sizeof(t->arr.of), t->arr.of, where))
				return false;
		}
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
	case TYPE_EXPR:
		assert(0);
	    return false;
	}
	return true;
}

/* generate a value from a pointer */
static bool cgen_val_ptr(CGenerator *g, void *v, Type *t, Location where) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_TUPLE:
	case TYPE_VOID:
	case TYPE_EXPR:
	case TYPE_TYPE:
		assert(0);
		return false;
	case TYPE_UNKNOWN:
		err_print(where, "Cannot determine type.");
		return false;
	case TYPE_ARR:
		cgen_write(g, "{");
		for (size_t i = 0; i < t->arr.n; ++i) {
			if (i) cgen_write(g, ", ");
			if (!cgen_val_ptr(g, (char *)v + i * compiler_sizeof(t->arr.of), t->arr.of, where))
				return false;
		}
		cgen_write(g, "}");
		break;
	case TYPE_SLICE:
		cgen_write(g, "{d%p_, %lu}", v, ((Slice *)v)->n);
		break;
	case TYPE_STRUCT:
		cgen_write(g, "{");
		arr_foreach(t->struc->fields, Field, f) {
			if (f != t->struc->fields)
				cgen_write(g, ", ");
			cgen_val_ptr(g, (char *)v + f->offset, f->type, where);
		}
		cgen_write(g, "}");
		break;
	case TYPE_FN:
		cgen_fn_name(g, *(FnExpr **)v);
		break;
	case TYPE_PTR:
		err_print(where, "Cannot bring compile time pointer to runtime.");
		return false;
	case TYPE_BUILTIN:
		switch (t->builtin) {
		case BUILTIN_I8: cgen_write(g, I8_FMT, *(I8 *)v); break;
		case BUILTIN_U8: cgen_write(g, U8_FMT, *(U8 *)v); break;
		case BUILTIN_I16: cgen_write(g, I16_FMT, *(I16 *)v); break;
		case BUILTIN_U16: cgen_write(g, U16_FMT, *(U16 *)v); break;
		case BUILTIN_I32: cgen_write(g, I32_FMT, *(I32 *)v); break;
		case BUILTIN_U32: cgen_write(g, U32_FMT, *(U32 *)v); break;
		case BUILTIN_I64: cgen_write(g, I64_FMT, *(I64 *)v); break;
		case BUILTIN_U64: cgen_write(g, U64_FMT, *(U64 *)v); break;
		case BUILTIN_F32: cgen_write(g, F32_FMT"f", *(F32 *)v); break;
		case BUILTIN_F64: cgen_write(g, F64_FMT, *(F64 *)v); break;
		case BUILTIN_CHAR: cgen_write(g, "'\\x%02x'", *(char *)v); break;
		case BUILTIN_BOOL: cgen_write(g, "%s", *(bool *)v ? "true" : "false"); break;
		}
		break;
	}
	return true;
}

static bool cgen_val_pre(CGenerator *g, Value v, Type *t, Location where) {
	return cgen_val_ptr_pre(g, val_get_ptr(&v, t), t, where);
}

/* generates a value fit for use as an initializer */
static bool cgen_val(CGenerator *g, Value v, Type *t, Location where) {
	return cgen_val_ptr(g, val_get_ptr(&v, t), t, where);
}

static bool cgen_decl(CGenerator *g, Declaration *d) {
	int has_expr = d->flags & DECL_HAS_EXPR;
	bool is_tuple = d->type.kind == TYPE_TUPLE;
	if ((d->flags & DECL_IS_CONST) || (g->block == NULL && g->fn == NULL)) {
		/* declarations where we use a value */
		for (size_t idx = 0; idx < arr_len(d->idents); ++idx) {
		    Identifier i = d->idents[idx];
			Type *type = is_tuple ? &d->type.tuple[idx] : &d->type;
			Value *val = is_tuple ? &d->val.tuple[idx] : &d->val;
			if (type->kind == TYPE_TYPE) {
				/* 
				   confusingly,
				   struct declarations are handled by typedefs_cgen,
				   and struct definitions are handled by decls_cgen.
				   we don't need to do anything here.
				*/
				continue;
			} else if (type->kind == TYPE_FN && (d->flags & DECL_IS_CONST)) {
				/* don't generate function pointer declaration for constant fns */
				continue;
			}
			if (has_expr) {
				if (!cgen_val_pre(g, *val, type, d->where))
					return false;
			}
			if (!cgen_type_pre(g, type, d->where)) return false;
			cgen_write(g, " ");
			cgen_ident(g, i);
			if (!cgen_type_post(g, type, d->where)) return false;
			if (has_expr) {
				cgen_write(g, " = ");
				if (!cgen_val(g, *val, type, d->where))
					return false;
			} else {
				cgen_write(g, " = ");
				cgen_zero_value(g, type);
			}
			cgen_write(g, ";");
			cgen_nl(g);
		}
	} else {
		/* declarations where we use an expression */
		size_t nidents = arr_len(d->idents);
		for (size_t idx = 0; idx < nidents; ++idx) {
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
			if (!cgen_expr_pre(g, &d->expr)) return false;
			if (d->expr.type.kind == TYPE_TUPLE) {
				if (!cgen_set_tuple(g, NULL, d->idents, NULL, &d->expr)) return false;
			} else {
				if (nidents > 1) {
					/* set expr__ first to make sure side effects don't happen twice */
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
						e.flags = EXPR_FOUND_TYPE;
						e.kind = EXPR_IDENT;
						e.type = d->type;
						e.ident = *i;
						if (!cgen_set(g, &e, NULL, NULL, "expr__"))
							return false;
					}
					cgen_write(g, "}");
				} else {
					/* set it directly */
					Expression e = {0};
					e.kind = EXPR_IDENT;
					e.type = d->type;
					e.flags = EXPR_FOUND_TYPE;
					e.ident = d->idents[0];
					if (!cgen_set(g, &e, NULL, &d->expr, NULL))
						return false;
				}
			}
		}
		cgen_nl(g);
	}
	return true;
}

static bool cgen_ret(CGenerator *g, Expression *ret) {
	assert((g->fn->ret_type.kind == TYPE_VOID) == (ret == NULL));
	if (ret) {
		assert(type_eq(&g->fn->ret_type, &ret->type));
		if (!cgen_expr_pre(g, ret))
			return false;
	}
	if (!ret) {
		cgen_write(g, "return");
	} else if (cgen_uses_ptr(&g->fn->ret_type)) {
		if (g->fn->ret_type.kind == TYPE_TUPLE) {
			if (!cgen_set_tuple(g, NULL, NULL, "*ret", ret))
				return false;
		} else {
			if (!cgen_set(g, NULL, "*ret_", ret, NULL)) return false;
		}
		cgen_write(g, " return");
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
	  cgen_write(g, "/\* %s:%d *\/", s->where.ctx->filename, s->where.line);
	  (or even #line directives!) 
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
	case STMT_RET: {
		unsigned has_expr = s->ret.flags & RET_HAS_EXPR;
		if (!cgen_ret(g, has_expr ? &s->ret.expr : NULL))
			return false;
	} break;
	}
	return true;
}

static bool cgen_defs_expr(CGenerator *g, Expression *e) {
	if (e->kind == EXPR_FN) {
		FnExpr *f = e->fn;
		FnType *fn_type = &e->type.fn;
		bool any_const = false;
		if (fn_type->constness) {
			for (size_t i = 0; i < arr_len(fn_type->types)-1; ++i) {
				if (fn_type->constness[i] == CONSTNESS_YES)
					any_const = true;
			}
		}
		if (fn_type->constness) {
			HashTable *instances = &f->instances;
			/* generate each instance */
			Instance **is = instances->data;
			for (U64 i = 0; i < instances->cap; ++i) {
				if (instances->occupied[i]) {
					/* generate this instance */
					if (!cgen_fn(g, &is[i]->fn, e->where, is[i]->c.id, is[i]->val.tuple))
						return false;
				}
			}
		}
		if (!any_const) {
			if (!cgen_fn(g, e->fn, e->where, 0, NULL))
				return false;
		}
		
	}
	cgen_recurse_subexprs(g, e, cgen_defs_expr, cgen_defs_block, cgen_defs_decl);
	return true;
}

static bool cgen_defs_decl(CGenerator *g, Declaration *d) {
	if (d->flags & DECL_HAS_EXPR) {
		if (!cgen_defs_expr(g, &d->expr))
			return false;
	}
	return true;
}


static bool cgen_defs_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_DECL:
		if (!cgen_defs_decl(g, &s->decl))
			return false;
		break;
	case STMT_EXPR:
		if (!cgen_defs_expr(g, &s->expr))
			return false;
		break;
	case STMT_RET:
		if (s->ret.flags & RET_HAS_EXPR)
			if (!cgen_defs_expr(g, &s->ret.expr))
				return false;
		break;
	}
	return true;
}

static bool cgen_defs_block(CGenerator *g, Block *b) {
	arr_foreach(b->stmts, Statement, s) {
		if (!cgen_defs_stmt(g, s))
			return false;
	}
	if (b->ret_expr && !cgen_defs_expr(g, b->ret_expr))
		return false;
	return true;
}

static bool cgen_file(CGenerator *g, ParsedFile *f) {
	g->block = NULL;
	g->fn = NULL;
	g->file = f;
	/* 
	   TODO: don't include stdio.h with posix file descriptors
	*/
	cgen_write(g, "#include <stdint.h>\n"
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
			   "typedef u8 bool;\n"
			   "typedef struct { void *data; i64 n; } slice_;\n"
			   "#define false ((bool)0)\n"
			   "#define true ((bool)1)\n"
			   "static slice_ mkslice_(void *data, i64 n) { slice_ ret; ret.data = data; ret.n = n; return ret; }\n"
			   "static void free_(void *data) { extern void free(void *data); free(data); }\n" /* don't introduce free to global namespace */
			   "static void *e__calloc(size_t n, size_t sz) { extern void *calloc(size_t n, size_t size); extern void abort(void); void *ret = calloc(n, sz); if (!ret) { fprintf(stderr, \"Out of memory.\\n\"); abort(); } return ret; }\n\n\n");

	if (!typedefs_file(g, f))
		return false;
	if (!cgen_decls_file(g, f))
		return false;
	cgen_write(g, "/* code */\n");
	if (!f->pkg_name)
		cgen_write(g, "int main() {\n\tmain__();\n\treturn 0;\n}\n\n");
	arr_foreach(f->stmts, Statement, s) {
		if (!cgen_defs_stmt(g, s))
			return false;
	}

	arr_foreach(f->stmts, Statement, s) {
		if (!cgen_stmt(g, s))
			return false;
	}
	
	return true;
}
