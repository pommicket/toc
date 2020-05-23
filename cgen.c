/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static void cgen_create(CGenerator *g, FILE *out, Identifiers *ids, Allocator *allocr) {
	g->outc = out;
	g->ident_counter = 0;
	g->lbl_counter = 0;
	g->main_ident = ident_get(ids, "main");
	g->will_indent = true;
	g->indent_lvl = 0;
	g->globals = ids;
	g->allocr = allocr;
	g->nms_prefixes = NULL;
}

static void cgen_stmt(CGenerator *g, Statement *s);
enum {
	  CGEN_BLOCK_NOBRACES = 0x01 /* should it use braces? */
};
static void cgen_block(CGenerator *g, Block *b, uint16_t flags);
static void cgen_expr_pre(CGenerator *g, Expression *e);
static void cgen_expr(CGenerator *g, Expression *e);
static void cgen_set(CGenerator *g, Expression *set_expr, const char *set_str, Expression *to_expr,
					 const char *to_str);
static void cgen_set_tuple(CGenerator *g, Expression *exprs, Identifier *idents, const char *prefix, Expression *to);
static void cgen_type_pre(CGenerator *g, Type *t);
static void cgen_type_post(CGenerator *g, Type *t);
static void cgen_decl(CGenerator *g, Declaration *d);
static void cgen_ret(CGenerator *g, Block *returning_from, Expression *ret_expr);
/* yes, these do need to take pointers, and furthermore they must be the same pointer (because of slices) */
static void cgen_val(CGenerator *g, Value *v, Type *t);
static void cgen_val_pre(CGenerator *g, Value *v, Type *t);
static void cgen_val_ptr(CGenerator *g, void *v, Type *t);

static inline FILE *cgen_writing_to(CGenerator *g) {
	return g->outc;	/* for now */
}

static inline void *cgen_malloc(CGenerator *g, size_t sz) {
	return allocr_malloc(g->allocr, sz);
}

/* indent iff needed */
static inline void cgen_indent(CGenerator *g) {
	if (g->will_indent) {
		for (unsigned i = 0; i < g->indent_lvl; ++i)
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
#ifdef TOC_DEBUG
	fflush(cgen_writing_to(g));
#endif
}

static inline void cgen_nl(CGenerator *g) {
	fprintf(cgen_writing_to(g), "\n");
	g->will_indent = true;
}

static inline void cgen_string(CGenerator *g, String s) {
	fwrite(s.str, 1, s.len, cgen_writing_to(g));
}

static inline char *cgen_ident_to_str(Identifier i) {
	return ident_to_str_reduced_charset(i);
}

static inline void cgen_ident_id(CGenerator *g, IdentID id) {
	cgen_write(g, "a%lu_", (unsigned long)id);
}

static inline void cgen_lbl(CGenerator *g, IdentID lbl) {
	cgen_write(g, "lbl%lu_", (unsigned long)lbl);
}

/* used for fields */
static inline void cgen_ident_simple(CGenerator *g, Identifier i) {
	cgen_indent(g);
	fprint_ident_reduced_charset(cgen_writing_to(g), i);
}

static void cgen_ident(CGenerator *g, Identifier i) {
	if (i->nms) {
		cgen_write(g, "%s", i->nms->c.prefix);
	}
	if (i == g->main_ident && ident_scope(i) == NULL) {
		/* don't conflict with C's main! */
		cgen_write(g, "main_");
	} else {
		cgen_ident_simple(g, i);
	}
}


#define CGEN_IDENT_ID_STR_SIZE 32
/* buffer should be at least CGEN_IDENT_ID_STR_SIZE bytes */
static inline void cgen_ident_id_to_str(char *buffer, IdentID id) {
	snprintf(buffer, CGEN_IDENT_ID_STR_SIZE, "a%lu_", (unsigned long)id);
}

static inline void cgen_writeln(CGenerator *g, const char *fmt, ...) {
	va_list args;
	cgen_indent(g);
	va_start(args, fmt);
	vfprintf(cgen_writing_to(g), fmt, args);
	va_end(args);
	cgen_nl(g);
}

static inline void cgen_char(CGenerator *g, char c) {
	if (isprint(c) && c != '"')
		cgen_write(g, "%c", c);
	else
		cgen_write(g, "\\x%02x", c);
}
/* should this declaration be a direct function declaration C? (as opposed to using a function pointer or not being a function) */
static bool cgen_fn_is_direct(CGenerator *g, Declaration *d) {
	return (!g->block || g->block->kind == BLOCK_NMS) && (d->flags & DECL_IS_CONST) && (d->flags & DECL_HAS_EXPR) && d->expr.kind == EXPR_FN && arr_len(d->idents) == 1;
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
	case TYPE_UNKNOWN:
		return false;
	case TYPE_EXPR:
		break;
	}
	assert(0);
	return false;
}

static void cgen_struct_name(CGenerator *g, StructDef *sdef) {
	if (sdef->name) {
		cgen_ident(g, sdef->name);
	} else {
		assert(sdef->c.id);
		cgen_ident_id(g, sdef->c.id);
	}
	if (sdef->instance_id) {
		possibly_static_assert(sizeof sdef->instance_id == 8);
		cgen_write(g, U64_FMT "_", sdef->instance_id);
	}
}

static void cgen_type_pre(CGenerator *g, Type *t) {
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
		case BUILTIN_VOID: cgen_write(g, "void"); break;
		case BUILTIN_NMS:
		case BUILTIN_TYPE:
		case BUILTIN_VARARGS:
			assert(0); break;
		} break;
	case TYPE_PTR:
		cgen_type_pre(g, t->ptr);
		cgen_write(g, "(*");
		break;
	case TYPE_ARR:
		cgen_type_pre(g, t->arr.of);
		cgen_write(g, "(");
		break;
	case TYPE_FN:
		if (cgen_uses_ptr(&t->fn.types[0])) {
			cgen_write(g, "void");
		} else {
			cgen_type_pre(g, &t->fn.types[0]);
		}
		cgen_write(g, " (*");
		break;
	case TYPE_SLICE:
		cgen_write(g, "slice_");
		break;
	case TYPE_STRUCT:
		cgen_write(g, "struct ");
		cgen_struct_name(g, t->struc);
		break;
	case TYPE_TUPLE:
	case TYPE_EXPR:
	case TYPE_UNKNOWN:
		/* We should never try to generate this type */
		assert(0);
		break;
	}
}

static void cgen_type_post(CGenerator *g, Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_PTR:
		cgen_write(g, ")");
		cgen_type_post(g, t->ptr);
		break;
	case TYPE_ARR:
		assert(t->flags & TYPE_IS_RESOLVED);
		cgen_write(g, "[%lu])", (unsigned long)t->arr.n);
		cgen_type_post(g, t->arr.of);
		break;
	case TYPE_FN: {
		bool out_param = cgen_uses_ptr(&t->fn.types[0]);
		cgen_write(g, ")(");
		for (size_t i = 1; i < arr_len(t->fn.types); ++i) {
			if (i != 1)
				cgen_write(g, ", ");
			cgen_type_pre(g, &t->fn.types[i]);
			if (cgen_uses_ptr(&t->fn.types[i]))
				cgen_write(g, "(*)");
			cgen_type_post(g, &t->fn.types[i]);
		}
		if (out_param) {
			Type *ret_type = &t->fn.types[0];
			if (arr_len(t->fn.types) > 1)
				cgen_write(g, ", ");
			if (ret_type->kind == TYPE_TUPLE) {
				arr_foreach(ret_type->tuple, Type, x) {
					cgen_type_pre(g, x);
					cgen_write(g, "(*)");
					cgen_type_post(g, x);
					if (x != arr_last_ptr(ret_type->tuple)) {
						cgen_write(g, ", ");
					}
				}
			} else {
				cgen_type_pre(g, ret_type);
				cgen_write(g, "(*)");
				cgen_type_post(g, ret_type);
			}
		}
		if (arr_len(t->fn.types) == 1 && !out_param)
			cgen_write(g, "void");
		cgen_write(g, ")");
		if (!out_param)
			cgen_type_post(g, &t->fn.types[0]);
	} break;
	case TYPE_BUILTIN:
	case TYPE_UNKNOWN:
	case TYPE_TUPLE:
	case TYPE_SLICE:
	case TYPE_STRUCT:
		break;
	case TYPE_EXPR:
		assert(0);
		break;
	}
}

static void cgen_ctype(CGenerator *g, CType *c) {
	if (c->kind & CTYPE_UNSIGNED) {
		c->kind &= (CTypeKind)~(CTypeKind)CTYPE_UNSIGNED;
		cgen_write(g, "unsigned ");
	}
	switch (c->kind) {
	case CTYPE_CHAR:
		cgen_write(g, "char");
		break;
	case CTYPE_SIGNED_CHAR:
		cgen_write(g, "signed char");
		break;
	case CTYPE_SHORT:
		cgen_write(g, "short");
		break;
	case CTYPE_INT:
		cgen_write(g, "int");
		break;
	case CTYPE_LONG:
		cgen_write(g, "long");
		break;
	case CTYPE_LONGLONG:
		cgen_write(g, "long long");
		break;
	case CTYPE_PTR:
		cgen_write(g, "%s *", c->points_to);
		break;
	case CTYPE_FLOAT:
		cgen_write(g, "float");
		break;
	case CTYPE_DOUBLE:
		cgen_write(g, "double");
		break;
	case CTYPE_SIZE_T:
		cgen_write(g, "size_t");
		break;
	case CTYPE_VARARGS:
		cgen_write(g, "...");
		break;
	default:
		assert(0);
		break;
	}
}

static inline void cgen_fn_name(CGenerator *g, FnExpr *f) {
	if (f->c.name) {
		cgen_ident(g, f->c.name);
		if (f->instance_id) {
			cgen_write(g, U64_FMT "_", f->instance_id);
		}
	} else {
		assert(!fn_is_template(f));
		assert(f->c.id);
		cgen_ident_id(g, f->c.id);
	}
}

/* should we generate this function? (or is it just meant for compile time) */
static bool cgen_should_gen_fn(FnExpr *f) {
	if (f->flags & FN_EXPR_FOREIGN)
		return true;
	else if (f->ret_decls) {
		arr_foreach(f->ret_decls, Declaration, decl)
			if (type_is_compileonly(&decl->type))
				return false;
		return true;
	} else {
		return !type_is_compileonly(&f->ret_type);
	}
}

static void cgen_val_ptr_pre(CGenerator *g, void *v, Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_SLICE: {
		Slice *s = (Slice *)v;
		for (I64 i = 0; i < s->len; ++i) {
			cgen_val_ptr_pre(g, (char *)s->data + (U64)i * compiler_sizeof(t->slice), t->slice);
		}
		cgen_write(g, "static ");
		cgen_type_pre(g, t->slice);
		cgen_write(g, "(d%p_[])", v); /* @TODO: improve this somehow? */
		cgen_type_post(g, t->slice);
		cgen_write(g, " = ");
		if (type_is_builtin(t->slice, BUILTIN_CHAR)) {
			char *p = s->data;
			cgen_write(g, "\"");
			for (I64 i = 0; i < s->len; ++i, ++p)
				cgen_char(g, *p);
			cgen_writeln(g, "\";");
		} else {
			cgen_write(g, "{");
			for (I64 i = 0; i < s->len; ++i) {
				if (i) cgen_write(g, ", ");
				cgen_val_ptr(g, (char *)s->data + (U64)i * compiler_sizeof(t->slice), t->slice);
			}
			cgen_writeln(g, "};");
		}
	} break;
	case TYPE_ARR:
		for (size_t i = 0; i < t->arr.n; ++i) {
			cgen_val_ptr_pre(g, (char *)*(void **)v + i * compiler_sizeof(t->arr.of), t->arr.of);
		}
		break;
	case TYPE_FN:
	case TYPE_UNKNOWN:
	case TYPE_TUPLE:
	case TYPE_BUILTIN:
	case TYPE_PTR:
	case TYPE_STRUCT:
		break;
	case TYPE_EXPR:
		assert(0);
		break;
	}
}

/* generate a value from a pointer */
static void cgen_val_ptr(CGenerator *g, void *v, Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_TUPLE:
	case TYPE_EXPR:
	case TYPE_UNKNOWN:
		assert(0);
		return;
	case TYPE_ARR:
		cgen_write(g, "{");
		for (size_t i = 0; i < t->arr.n; ++i) {
			if (i) cgen_write(g, ", ");
			cgen_val_ptr(g, (char *)v + i * compiler_sizeof(t->arr.of), t->arr.of);
		}
		cgen_write(g, "}");
		break;
	case TYPE_SLICE:
		cgen_write(g, "{d%p_, %lu}", v, ((Slice *)v)->len);
		break;
	case TYPE_STRUCT:
		cgen_write(g, "{");
		arr_foreach(t->struc->fields, Field, f) {
			if (f != t->struc->fields)
				cgen_write(g, ", ");
			cgen_val_ptr(g, (char *)v + f->offset, f->type);
		}
		cgen_write(g, "}");
		break;
	case TYPE_FN:
		cgen_fn_name(g, *(FnExpr **)v);
		break;
	case TYPE_PTR:
		/* this can happen; as i'm writing this it's only for null */
		cgen_write(g, "((");
		cgen_type_pre(g, t);
		cgen_type_post(g, t);
		cgen_write(g, ")" ULONGLONG_FMT ")", (ulonglong)*(void **)v);
		break;
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
		case BUILTIN_F32: cgen_write(g, F32_FMT "f", *(F32 *)v); break;
		case BUILTIN_F64: cgen_write(g, F64_FMT, *(F64 *)v); break;
		case BUILTIN_CHAR: 
			cgen_write(g, "'");
			cgen_char(g, *(char *)v);
			cgen_write(g, "'");
			break;
		case BUILTIN_BOOL: cgen_write(g, "%s", *(bool *)v ? "true" : "false"); break;
		case BUILTIN_TYPE:
		case BUILTIN_NMS:
		case BUILTIN_VARARGS:
		case BUILTIN_VOID:
			assert(0);
			break;
		}
		break;
	}
}

static void cgen_val_pre(CGenerator *g, Value *v, Type *t) {
	cgen_val_ptr_pre(g, val_get_ptr(v, t), t);
}

/* generates a value fit for use as an initializer */
static void cgen_val(CGenerator *g, Value *v, Type *t) {
	cgen_val_ptr(g, val_get_ptr(v, t), t);
}

/* can the value generated by cgen_val for this type be used directly (as opposed to being stored in a variable)? */
static inline bool cgen_is_type_simple(Type *t) {
	return t->kind == TYPE_BUILTIN || t->kind == TYPE_FN;
}

static void cgen_fn_params(CGenerator *g, FnExpr *f) {
	bool out_param = cgen_uses_ptr(&f->ret_type);
	cgen_write(g, "(");
	bool any_params = false;
	arr_foreach(f->params, Declaration, d) {
		if (d->flags & DECL_FOUND_VAL) continue;
		if (type_is_builtin(&d->type, BUILTIN_VARARGS)) {
			int idx = 0;
			arr_foreach(d->val.varargs, VarArg, varg) {
				if (any_params) 
					cgen_write(g, ", ");
				any_params = true;
				cgen_type_pre(g, varg->type);
				cgen_write(g, " ");
				cgen_ident_simple(g, d->idents[0]);
				cgen_write(g, "%d_", idx);
				cgen_type_post(g, varg->type);
				++idx;
			}
		} else {
			int idx = 0;
			arr_foreach(d->idents, Identifier, i) {
				if (any_params) 
					cgen_write(g, ", ");
				any_params = true;
				Type *type = d->type.kind == TYPE_TUPLE ? &d->type.tuple[idx++] : &d->type;
				cgen_type_pre(g, type);
				cgen_write(g, " ");
				cgen_ident_simple(g, *i);
				cgen_type_post(g, type);
			}
		}
	}
	if (out_param) {
		if (f->ret_type.kind == TYPE_TUPLE) {
			/* multiple return variables */
			for (size_t i = 0; i < arr_len(f->ret_type.tuple); ++i) {
				Type *x = &f->ret_type.tuple[i];
				if (any_params || i > 0)
					cgen_write(g, ", ");
				cgen_type_pre(g, x);
				cgen_write(g, "(*ret__%lu)", (unsigned long)i);
				cgen_type_post(g, x);
			}
		} else {
			if (any_params)
				cgen_write(g, ", ");
			cgen_type_pre(g, &f->ret_type);
			cgen_write(g, " (*ret__)");
			cgen_type_post(g, &f->ret_type);
		}
		any_params = true;
	}
	if (!any_params)
		cgen_write(g, "void");
	cgen_write(g, ")");
}

static inline void cgen_arg_pre(CGenerator *g, Expression *arg) {
	cgen_expr_pre(g, arg);
	if (arg->type.kind == TYPE_ARR) {
		/* create copy of array */
		IdentID copy = ++g->ident_counter;
		cgen_type_pre(g, &arg->type);
		char s[CGEN_IDENT_ID_STR_SIZE];
		cgen_ident_id_to_str(s, copy);

		cgen_write(g, " %s", s);
		cgen_type_post(g, &arg->type);
		cgen_write(g, "; ");
		cgen_set(g, NULL, s, arg, NULL);
		arg->cgen.id = copy;
	}
}

static inline void cgen_arg(CGenerator *g, Expression *arg) {
	if (arg->type.kind == TYPE_ARR) {
		cgen_ident_id(g, arg->cgen.id);
	} else {
		cgen_expr(g, arg);
	}
}

/* unless f has const/semi-const args, which_are_const can be set to 0 */
static void cgen_fn_header(CGenerator *g, FnExpr *f) {
	assert(!(f->flags & FN_EXPR_FOREIGN));
	
	bool out_param = cgen_uses_ptr(&f->ret_type);
	assert(cgen_should_gen_fn(f));
	if (!(f->flags & FN_EXPR_EXPORT))
		cgen_write(g, "static ");
	if (out_param) {
		cgen_write(g, "void ");
	} else {
		cgen_type_pre(g, &f->ret_type);
		cgen_write(g, " ");
	}
	cgen_fn_name(g, f);	
	cgen_fn_params(g, f);
	if (!out_param) {
		cgen_type_post(g, &f->ret_type);
	}
}

static inline void cgen_deferred_from_block(CGenerator *g, Block *from) {
	arr_foreach(from->deferred, StatementPtr, s) {
		cgen_stmt(g, *s);
	}
}

/* generates deferred statements in g->block, g->block->parent, ..., to) */
static inline void cgen_deferred_up_to(CGenerator *g, Block *to) {
	for (Block *b = g->block; b; b = b == to ? NULL : b->parent) {
		cgen_deferred_from_block(g, b);
	}
}

/* same as cgen_deferred_up_to but doesn't generate to->deferred */
static inline void cgen_deferred_up_to_not_including(CGenerator *g, Block *to) {
	for (Block *b = g->block; b != to; b = b->parent)
		cgen_deferred_from_block(g, b);
}

/* 
   Either set_expr or set_str should be NULL and either to_expr or to_str should be NULL 
   Also, set_str and/or to_str should be NULL
   this DOES NOT call cgen_expr_pre for set_expr or to_expr
*/
static void cgen_set(CGenerator *g, Expression *set_expr, const char *set_str, Expression *to_expr,
					 const char *to_str) {
	Type *type;
	if (set_expr) {
		type = &set_expr->type;
	} else {
		assert(to_expr);
		type = &to_expr->type;
	}
	switch (type->kind) {
	case TYPE_BUILTIN:
	case TYPE_FN:
	case TYPE_PTR:
	case TYPE_SLICE:
	case TYPE_STRUCT:
	case TYPE_UNKNOWN:
		if (set_expr) {
			cgen_expr(g, set_expr);
		} else {
			cgen_write(g, set_str);
		}
		cgen_write(g, " = ");
		if (to_expr) {
			cgen_expr(g, to_expr);
		} else {
			cgen_write(g, to_str);
		}
		cgen_write(g, ";");
		break;
	case TYPE_ARR:
		cgen_write(g, "{");
		cgen_nl(g);
		cgen_write(g, "size_t i;");
		cgen_type_pre(g, type->arr.of);
		cgen_write(g, "(*arr__in)");
		cgen_type_post(g, type->arr.of);
		cgen_write(g, " = ");
		if (to_expr) {
			cgen_expr(g, to_expr);
		} else {
			cgen_write(g, to_str);
		}
		cgen_write(g, "; ");
		cgen_type_pre(g, type->arr.of);
		cgen_write(g, "(*arr__out)");
		cgen_type_post(g, type->arr.of);
		cgen_write(g, " = ");
		if (set_expr) {
			cgen_expr(g, set_expr);
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
		cgen_set_tuple(g, set_expr->tuple, NULL, NULL, to_expr);
		break;
	case TYPE_EXPR:
		assert(0);
		break;
	}
}

/* one of exprs, idents, and prefix should be NULL. does NOT call cgen_expr_pre for to/exprs */
static void cgen_set_tuple(CGenerator *g, Expression *exprs, Identifier *idents, const char *prefix, Expression *to) {
	switch (to->kind) {
	case EXPR_VAL:
		assert(0); /* never needed at the moment */
		break;
	case EXPR_TUPLE:
		/* e.g. a, b = 3, 5; */
		if (exprs) {
			for (size_t i = 0; i < arr_len(to->tuple); ++i) {
				cgen_expr_pre(g, &exprs[i]);
			}
		}
		for (size_t i = 0; i < arr_len(to->tuple); ++i) {
			char *s = NULL, buf[64];
			Expression *e = NULL;
			if (idents)
				s = cgen_ident_to_str(idents[i]);
			else if (exprs)
				e = &exprs[i];
			else {
				snprintf(buf, sizeof buf, "(%s%lu)", prefix, (unsigned long)i);
				s = buf;
			}
			cgen_set(g, e, s, &to->tuple[i], NULL);
			if (s != buf) free(s);
		}
		break;
	case EXPR_CALL: {
		FnType *fn_type = &to->call.fn->type.fn;
		Type *ret_type = &fn_type->types[0];
		Constness *constness = fn_type->constness;
		int i = 0;
		IdentID *underscore_ids = NULL;
		int nout_params = (int)(exprs ? arr_len(exprs) : arr_len(idents));
		
		if (idents) {
			for (i = 0; i < nout_params; ++i) {
				if (ident_eq_str(idents[i], "_")) {
					Type *type = &ret_type->tuple[i];
					IdentID id = ++g->ident_counter;
					cgen_type_pre(g, type);
					cgen_write(g, " ");
					cgen_ident_id(g, id);
					cgen_type_post(g, type);
					cgen_write(g, "; ");
					arr_add(underscore_ids, id);
				}
			}
		}
		
		/* e.g. a, b = fn_which_returns_tuple(); */
		arr_foreach(to->call.arg_exprs, Expression, arg) {
			if (!constness || !arg_is_const(arg, constness[i])) {
				cgen_arg_pre(g, arg);
			}
		}

		cgen_expr_pre(g, to->call.fn);
		cgen_expr(g, to->call.fn);
		
		cgen_write(g, "(");
		bool any_args = false;
		i = 0;
		arr_foreach(to->call.arg_exprs, Expression, arg) {
			if (!constness || !arg_is_const(arg, constness[i])) {
				if (any_args)
					cgen_write(g, ", ");
				any_args = true;
				cgen_arg(g, arg);
			}
			++i;
		}
		/* out params */
		IdentID *u = underscore_ids;
		for (i = 0; i < (int)nout_params; ++i) {
			if (any_args || i > 0)
				cgen_write(g, ", ");
			if (exprs) {
				cgen_write(g, "&");
				cgen_expr(g, &exprs[i]);
			} else if (idents) {
				cgen_write(g, "&");
				if (ident_eq_str(idents[i], "_"))
					cgen_ident_id(g, *u++);
				else
					cgen_ident(g, idents[i]);
			} else {
				cgen_write(g, "&(%s%d_)", prefix, i);
			}
		}
		arr_clear(underscore_ids);
		cgen_writeln(g, "); ");
	} break;
		/* things which can never be tuples */
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
	case EXPR_C:
	case EXPR_BUILTIN:
	case EXPR_TYPE:
	case EXPR_NMS:
		assert(0);
		break;
	}
}

static void cgen_truthiness(CGenerator *g, Expression *e) {
	switch (e->type.kind) {
	case TYPE_SLICE:
		cgen_expr(g, e);
		cgen_write(g, ".len");
		break;
	default:
		cgen_expr(g, e);
		break;
	}
}

static void cgen_expr_pre(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_CALL: {
		cgen_expr_pre(g, e->call.fn);
		size_t i = 0;
		Constness *constness = e->call.fn->type.fn.constness;
		size_t nparams = arr_len(e->call.fn->type.fn.types)-1;
		arr_foreach(e->call.arg_exprs, Expression, arg) {
			if (!constness || !arg_is_const(arg, constness[i])) {
				cgen_arg_pre(g, arg);
			}
			if (i < nparams-1) /* necessary for varargs */
				++i;
		}
		if (e->type.kind == TYPE_TUPLE) {
			Type *t = &e->type;
			size_t ntypes = arr_len(t->tuple);
			IdentID *ids = err_malloc(ntypes * sizeof *ids);
			for (i = 0; i < ntypes; ++i) {
				ids[i] = ++g->ident_counter;
				cgen_type_pre(g, &t->tuple[i]);
				cgen_write(g, " ");
				cgen_ident_id(g, ids[i]);
				cgen_type_post(g, &t->tuple[i]);
				cgen_write(g, "; ");
			}
			cgen_expr(g, e->call.fn);
			cgen_write(g, "(");
			bool any_args = false;
			i = 0;
			arr_foreach(e->call.arg_exprs, Expression, arg) {
				if (!constness || !arg_is_const(arg, constness[i])) {
					if (any_args) cgen_write(g, ", ");
					any_args = true;
					cgen_arg(g, arg);
				}
				++i;
			}
			for (i = 0; i < ntypes; ++i) {
				if (any_args) cgen_write(g, ", ");
				any_args = true;
				cgen_write(g, "&");
				cgen_ident_id(g, ids[i]);
			}
			cgen_write(g, ");");
		} else if (cgen_uses_ptr(&e->type)) {
			IdentID id = e->cgen.id = ++g->ident_counter;
			cgen_type_pre(g, &e->type);
			cgen_write(g, " ");
			cgen_ident_id(g, id);
			cgen_type_post(g, &e->type);
			cgen_write(g, ";"); cgen_nl(g);
			cgen_expr(g, e->call.fn);
			cgen_write(g, "(");
			bool any_args = false;
			i = 0;
			arr_foreach(e->call.arg_exprs, Expression, arg) {
				if (!constness || !arg_is_const(arg, constness[i])) {
					if (any_args) cgen_write(g, ", ");
					any_args = true;
					cgen_arg(g, arg);
				}
				++i;
			}
			if (any_args) {
				cgen_write(g, ", ");
			}
			cgen_write(g, "&");
			cgen_ident_id(g, e->cgen.id);
			cgen_write(g, ");");
			cgen_nl(g);
		}
	} break;
	case EXPR_UNARY_OP:
		cgen_expr_pre(g, e->unary.of);
		break;
	case EXPR_BINARY_OP:
		cgen_expr_pre(g, e->binary.lhs);
		if (e->binary.op != BINARY_DOT)
			cgen_expr_pre(g, e->binary.rhs);
		break;
	case EXPR_CAST:
		cgen_expr_pre(g, e->cast.expr);
		break;
	case EXPR_SLICE: {
		SliceExpr *s = &e->slice;
		IdentID s_id = e->slice.c.id = ++g->ident_counter;
		IdentID from_id = ++g->ident_counter;
		cgen_expr_pre(g, s->of);
		if (s->from) cgen_expr_pre(g, s->from);
		if (s->to) cgen_expr_pre(g, s->to);
		cgen_write(g, "slice_ ");
		cgen_ident_id(g, s_id);
		cgen_write(g, "; { slice_ of__ = ");
		if (s->of->type.kind == TYPE_SLICE) {
			cgen_expr(g, s->of);
		} else {
			assert(s->of->type.kind == TYPE_ARR);
			cgen_write(g, "mkslice_(");
			cgen_expr(g, s->of);
			cgen_write(g, ", " U64_FMT, s->of->type.arr.n);
			cgen_write(g, ")");
		}
		cgen_write(g, "; i64 ");
		cgen_ident_id(g, from_id);
		cgen_write(g, " = ");
		if (s->from) {
			cgen_expr(g, s->from);
		} else {
			cgen_write(g, "0");
		}
		cgen_write(g, "; ");
		cgen_ident_id(g, s_id);
		cgen_write(g, ".data = (");
		cgen_type_pre(g, e->type.slice);
		cgen_write(g, "(*)");
		cgen_type_post(g, e->type.slice);
		cgen_write(g, ")(of__");
		cgen_write(g, ".data");
		cgen_write(g, ") + ");
		cgen_ident_id(g, from_id);
		cgen_write(g, "; ");
		cgen_ident_id(g, s_id);
		cgen_write(g, ".len = ");
		if (s->to) {
			cgen_expr(g, s->to);
		} else {
			cgen_write(g, "of__.len");
		}
		cgen_write(g, " - ");
		cgen_ident_id(g, from_id);
		cgen_write(g, "; }");
		cgen_nl(g);
	} break;
	case EXPR_VAL:
		/* @TODO: don't make a variable for this if it's not needed */
		if (type_is_compileonly(&e->type))
			break;
		if (!cgen_is_type_simple(&e->type)) {
			cgen_val_pre(g, &e->val, &e->type);
			cgen_type_pre(g, &e->type);
			e->cgen.id = ++g->ident_counter;
			cgen_write(g, " ");
			cgen_ident_id(g, e->cgen.id);
			cgen_type_post(g, &e->type);
			cgen_write(g, " = ");
			cgen_val(g, &e->val, &e->type);
			cgen_writeln(g, ";");
		}
		break;
	case EXPR_TUPLE:
		arr_foreach(e->tuple, Expression, x)
			cgen_expr_pre(g, x);
		break;
	case EXPR_BUILTIN:
		switch (e->builtin.which.val) {
		case BUILTIN_STDOUT:
			cgen_write(g, "extern void *stdout;");
			cgen_nl(g);
			break;
		case BUILTIN_STDERR:
			cgen_write(g, "extern void *stderr;");
			cgen_nl(g);
			break;
		case BUILTIN_STDIN:
			cgen_write(g, "extern void *stdin;");
			cgen_nl(g);
			break;
		default:
			break;
		}
		break;
	case EXPR_LITERAL_INT:
	case EXPR_LITERAL_FLOAT:
	case EXPR_LITERAL_BOOL:
	case EXPR_LITERAL_CHAR:
	case EXPR_LITERAL_STR:
	case EXPR_IDENT:
	case EXPR_FN:
	case EXPR_C:
	case EXPR_TYPE:
	case EXPR_NMS:
		break;
	}
}

static void cgen_expr(CGenerator *g, Expression *e) {
	assert(e->flags & EXPR_FOUND_TYPE);
	switch (e->kind) {
	case EXPR_LITERAL_FLOAT:
		cgen_write(g, "%.16Lf", (long double)e->floatl);
		break;
	case EXPR_LITERAL_INT:
		cgen_write(g, U64_FMT, e->intl);
		break;
	case EXPR_LITERAL_STR: {
		char *p = e->strl.str;
		cgen_write(g, "mkslice_(\"");
		for (size_t i = 0; i < e->strl.len; ++i, ++p) {
			cgen_char(g, *p);
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
			Identifier i = e->ident;
			Declaration *d = i->decl;
			if (d->flags & DECL_IS_CONST) {
				int index = decl_ident_index(d, i);
				Value fn_val = *decl_val_at_index(d, index);
				FnExpr *fn = fn_val.fn;
				cgen_fn_name(g, fn);
				handled = true;
			}
		}
		if (!handled) {
			cgen_ident(g, e->ident);
		}
	} break;
	case EXPR_BINARY_OP: {
		const char *s = NULL;
		Expression *lhs = e->binary.lhs, *rhs = e->binary.rhs;
		Type *lhs_type = &lhs->type;
		BinaryOp op = e->binary.op;
		switch (op) {
		case BINARY_SUB:
			s = "-"; break;
		case BINARY_ADD:
			s = "+"; break;
		case BINARY_MUL:
			s = "*"; break;
		case BINARY_DIV:
			s = "/"; break;
		case BINARY_MOD:
			s = "%"; break;
		case BINARY_SET: 
			cgen_set(g, lhs, NULL, rhs, NULL);
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
		case BINARY_AND:
		case BINARY_OR:
			cgen_write(g, "(");
			cgen_truthiness(g, lhs);
			cgen_write(g, " %s ", op == BINARY_AND ? "&&" : "||");
			cgen_truthiness(g, rhs);
			cgen_write(g, ")");
			break;
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
		case BINARY_SET_MOD:
			s = "%="; break;
		case BINARY_AT_INDEX: {
			bool uses_ptr = false;
			if (lhs_type->kind == TYPE_PTR) {
				uses_ptr = true;
				lhs_type = lhs_type->ptr;
			}
			switch (lhs_type->kind) {
			case TYPE_ARR:
				if (uses_ptr) cgen_write(g, "(*");
				cgen_expr(g, lhs);
				if (uses_ptr) cgen_write(g, ")");
				cgen_write(g, "[");
				cgen_expr(g, rhs);
				cgen_write(g, "]");
				break;
			case TYPE_SLICE:
				cgen_write(g, "((");
				cgen_type_pre(g, &e->type);
				cgen_write(g, "(*)");
				cgen_type_post(g, &e->type);
				cgen_write(g, ")(");
				cgen_expr(g, lhs);
				cgen_write(g, "%sdata))[", uses_ptr ? "->" : ".");
				cgen_expr(g, rhs);
				cgen_write(g, "]");
				break;
			case TYPE_BUILTIN:
				if (lhs_type->builtin == BUILTIN_VARARGS) {
					assert(lhs->kind == EXPR_IDENT);
					assert(rhs->kind == EXPR_VAL);
					assert(type_is_builtin(&rhs->type, BUILTIN_I64));
					I64 i = rhs->val.i64;
					cgen_ident(g, lhs->ident);
					cgen_write(g, I64_FMT "_", i);
				} else assert(0);
				break;
			default:
				assert(0);
				break;
			}
		} break;
		case BINARY_DOT: {
			Type *struct_type = lhs_type;
			if (struct_type->kind == TYPE_PTR) struct_type = struct_type->ptr;
			if (struct_type->kind == TYPE_STRUCT) {
				cgen_write(g, "(");
				cgen_expr(g, lhs);
				bool is_ptr = lhs->type.kind == TYPE_PTR;
				cgen_write(g, is_ptr ? "->" :".");
				cgen_ident_simple(g, e->binary.field->name);
				cgen_write(g, ")");
			} else if (struct_type->kind == TYPE_SLICE) {
				/* access slice data */
				cgen_expr(g, lhs);
				bool is_ptr = lhs->type.kind == TYPE_PTR;
				cgen_write(g, is_ptr ? "->" :".");
				cgen_string(g, rhs->ident_str);
			}
		} break;
		}
		if (lhs_type->kind == TYPE_PTR && type_is_void(lhs_type->ptr)) {
			if (op == BINARY_ADD || op == BINARY_SUB) {
				cgen_write(g, "((");
				cgen_type_pre(g, &e->type);
				cgen_type_post(g, &e->type);
				cgen_write(g, ")((char*)");
				cgen_expr(g, lhs);
				cgen_write(g, "%s", s);
				cgen_expr(g, rhs);
				cgen_write(g, "))");
			} else if (op == BINARY_SET_ADD || op == BINARY_SET_SUB) {
				/* lhs could have side effects so we can't just do lhs = (char *)lhs + rhs */
				cgen_write(g, "{");
					cgen_write(g, "void **t_ = &");
				cgen_expr(g, lhs);
				cgen_write(g, ";");
				cgen_write(g, "*t_ = (char *)*t_ %c ", s[0]);
				cgen_expr(g, rhs);
				cgen_write(g, ";}");
			}
			s = NULL;
		}

		if (!s) break;
		cgen_write(g, "(");
		cgen_expr(g, lhs);
		cgen_write(g, "%s", s);
		cgen_expr(g, rhs);
		cgen_write(g, ")");
	} break;
	case EXPR_UNARY_OP: {
		const char *s = NULL;
		switch (e->unary.op) {
		case UNARY_MINUS:
			s = "-"; break;
		case UNARY_DEREF:
			s = "*"; break;
		case UNARY_ADDRESS:
			s = "&"; break;
		case UNARY_NOT:
			cgen_write(g, "(!");
			cgen_truthiness(g, e->unary.of);
			cgen_write(g, ")");
			break;
		case UNARY_SIZEOF:
		case UNARY_ALIGNOF:
			cgen_write(g, "%s(", e->unary.op == UNARY_SIZEOF ? "sizeof" : "_Alignof");
			assert(e->unary.of->kind == EXPR_VAL);
			cgen_type_pre(g, e->unary.of->val.type);
			cgen_type_post(g, e->unary.of->val.type);
			cgen_write(g, ")");
			break;
		case UNARY_TYPEOF:
		case UNARY_DSIZEOF:
		case UNARY_DALIGNOF:
			assert(0);
			return;
		}
		if (!s) break;
		cgen_write(g, "(");
		cgen_write(g, "%s", s);
		cgen_expr(g, e->unary.of);
		cgen_write(g, ")");
	} break;
	case EXPR_CALL:
		if (e->type.kind == TYPE_TUPLE) {
			/* the only situation in which this could happen is if the return value doesn't matter */
		} else if (cgen_uses_ptr(&e->type)) {
			cgen_ident_id(g, e->cgen.id);
		} else {
			FnType *fn_type = &e->call.fn->type.fn;
			cgen_expr(g, e->call.fn);
			cgen_write(g, "(");
			bool first_arg = true;
			size_t i = 0;
			size_t nparams = arr_len(fn_type->types)-1;
			arr_foreach(e->call.arg_exprs, Expression, arg) {
				if (!fn_type->constness || !arg_is_const(arg, fn_type->constness[i])) {
					if (!first_arg)
						cgen_write(g, ", ");
					first_arg = false;
					cgen_arg(g, arg);
				}
				if (i < nparams-1)
					++i;
			}
			cgen_write(g, ")");
		}
		break;
	case EXPR_C: {
		Expression *code = e->c.code;
		assert(code->kind == EXPR_VAL);
		cgen_indent(g);
		fwrite(code->val.slice.data, 1, (size_t)code->val.slice.len, cgen_writing_to(g));
	} break;
	case EXPR_BUILTIN:
		switch (e->builtin.which.val) {
		case BUILTIN_STDOUT:
			cgen_write(g, "stdout");
			break;
		case BUILTIN_STDERR:
			cgen_write(g, "stderr");
			break;
		case BUILTIN_STDIN:
			cgen_write(g, "stdin");
			break;
		case BUILTIN_COMPILING:
			cgen_write(g, "false");
			break;
		case BUILTIN_PLATFORM:
			cgen_write(g, "platform__");
			break;
		}
		break;
	case EXPR_CAST: {
		Type *from = &e->cast.expr->type;
		Type *to = &e->cast.type;
		if (to->kind == TYPE_ARR) {
			/* can't cast to array type */
			cgen_expr(g, e->cast.expr);
		} else {
			CType *ctype = &e->cast.ctype;
			cgen_write(g, "((");
			if (ctype->kind != CTYPE_NONE) {
				cgen_ctype(g, ctype);
			} else {
				cgen_type_pre(g, to);
				cgen_type_post(g, to);
			}
			cgen_write(g, ")(");
			cgen_expr(g, e->cast.expr);
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
	case EXPR_VAL: {
		Type *t = &e->type;
		if (cgen_is_type_simple(t)) {
			cgen_val(g, &e->val, t);
		} else {
			cgen_ident_id(g, e->cgen.id);
		}
	} break;
	case EXPR_NMS:
		break;
	}
}

static void cgen_block(CGenerator *g, Block *b, U16 flags) {
	Block *prev_block = g->block;
	g->block = b;
	b->deferred = NULL;
	
	if (!(flags & CGEN_BLOCK_NOBRACES)) {
		cgen_writeln(g, "{");
	}

	++g->indent_lvl;
	
	arr_foreach(b->stmts, Statement, s)
		cgen_stmt(g, s);
	if (b->c.cont_lbl) {
		cgen_lbl(g, b->c.cont_lbl);
		cgen_writeln(g, ":;");
	}
	--g->indent_lvl;
	if (!(flags & CGEN_BLOCK_NOBRACES)) {
		cgen_deferred_from_block(g, b);
		arr_clear(b->deferred);
		cgen_write(g, "}");
		if (b->c.break_lbl) {
			cgen_lbl(g, b->c.break_lbl);
			cgen_writeln(g, ":;");
		}
		cgen_nl(g);
	}
	g->block = prev_block;
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
	case TYPE_UNKNOWN:
	case TYPE_TUPLE:
	case TYPE_EXPR:
		assert(0);
		break;
	}
}

/* pass NULL for compile_time_args if there are no compile time arguments.
compile_time_args is needed because we can't determine which_are_const
from just f.
 */
static void cgen_fn(CGenerator *g, FnExpr *f) {
	if (f->flags & FN_EXPR_FOREIGN)
		return; /* handled by cgen_fn_decl */
	if (!cgen_should_gen_fn(f))
		return;
	FnExpr *prev_fn = g->fn;
	Block *prev_block = g->block;
	g->fn = f;
	g->block = &f->body;
	cgen_fn_header(g, f);
	cgen_write(g, " {");
	cgen_nl(g);
	arr_foreach(f->params, Declaration, param) {
		if (param->flags & DECL_FOUND_VAL) {
			int i = 0;
			if (type_is_builtin(&param->type, BUILTIN_VARARGS)) {
				VarArg *vararg = param->val.varargs;
				size_t nvarargs = arr_len(vararg);
				for (size_t v = 0; v < nvarargs; ++v, ++vararg) {
					Type *type = vararg->type;
					Value *arg = &vararg->val;
					if (!type_is_compileonly(type)) {
						cgen_val_pre(g, arg, type);
						cgen_type_pre(g, type);
						cgen_write(g, " const ");
						assert(arr_len(param->idents) == 1);
						cgen_ident(g, param->idents[0]);
						cgen_write(g, "%lu_", (unsigned long)v);
						cgen_type_post(g, type);
						cgen_write(g, " = ");
						cgen_val(g, arg, type);
						cgen_writeln(g, ";");
					}
				}
			} else {
				arr_foreach(param->idents, Identifier, ident) {
					Type *type = decl_type_at_index(param, i);
					Value *arg = decl_val_at_index(param, i);
					if (!type_is_compileonly(type)) {
						cgen_val_pre(g, arg, type);
						cgen_type_pre(g, type);
						cgen_write(g, " const ");
						cgen_ident(g, *ident);
						cgen_type_post(g, type);
						cgen_write(g, " = ");
						cgen_val(g, arg, type);
						cgen_writeln(g, ";");
					}
					++i;
				}
			}
		}
	}

	/* retdecls need to be after compile time arguments to allow fn(x::int) y := x */
	arr_foreach(f->ret_decls, Declaration, d) {
		cgen_decl(g, d);
	}
	
	cgen_block(g, &f->body, CGEN_BLOCK_NOBRACES);
	cgen_writeln(g, "}");
	g->block = prev_block;
	g->fn = prev_fn;
	cgen_nl(g);
	cgen_nl(g);
}

static void cgen_decl(CGenerator *g, Declaration *d) {
	if (!g->block || (g->block->kind == BLOCK_NMS))
		return; /* already dealt with */
	int has_expr = d->flags & DECL_HAS_EXPR;
	if (cgen_fn_is_direct(g, d))
		return; /* dealt with in the loop that defines all the functions in cgen_file */
	if (d->flags & DECL_FOUND_VAL) {
		/* declarations where we use a value */
		for (int idx = 0, nidents = (int)arr_len(d->idents); idx < nidents; ++idx) {
			Identifier i = d->idents[idx];
			if (ident_eq_str(i, "_")) continue;
			Type *type = decl_type_at_index(d, idx);
			if (type_is_compileonly(&d->type)) {
				continue;
			}
			Value *val = decl_val_at_index(d, idx);
			if (has_expr) {
				cgen_val_pre(g, val, type);
			}
			if (d->flags & DECL_IS_CONST)
				cgen_write(g, "static ");
			cgen_type_pre(g, type);
			cgen_write(g, " ");
			cgen_ident(g, i);
			cgen_type_post(g, type);
			if (has_expr) {
				cgen_write(g, " = ");
				cgen_val(g, val, type);
			} else {
				cgen_write(g, " = ");
				cgen_zero_value(g, type);
			}
			cgen_write(g, ";");
			cgen_nl(g);
		}
	} else {
		/* declarations where we use an expression */
		int nidents = (int)arr_len(d->idents);
		for (int idx = 0; idx < nidents; ++idx) {
			Identifier i = d->idents[idx];
			if (ident_eq_str(i, "_")) continue;
			Type *type = decl_type_at_index(d, idx);
			cgen_type_pre(g, type);
			cgen_write(g, " ");
			cgen_ident(g, i);
			cgen_type_post(g, type);
			if (!has_expr) {
				cgen_write(g, " = ");
				cgen_zero_value(g, type);
			}
				
			cgen_write(g, "; ");
		}
		if (has_expr) {
			Expression *expr = &d->expr;
			assert((g->block || g->fn) && !(d->flags & DECL_IS_CONST));
			if (expr->type.kind == TYPE_TUPLE) {
				cgen_set_tuple(g, NULL, d->idents, NULL, expr);
			} else {
				if (expr->kind != EXPR_VAL) cgen_expr_pre(g, expr);
				if (nidents > 1) {
					/* set expr__ first to make sure side effects don't happen twice */
					cgen_write(g, "{");
					cgen_nl(g);
					cgen_type_pre(g, &d->type);
					cgen_write(g, " expr_");
					cgen_type_post(g, &d->type);
					cgen_write(g, "; ");
					cgen_set(g, NULL, "expr_", expr, NULL);
				
					arr_foreach(d->idents, Identifier, i) {
						Expression e;
						if (ident_eq_str(*i, "_")) continue;
						e.flags = EXPR_FOUND_TYPE;
						e.kind = EXPR_IDENT;
						e.type = d->type;
						e.ident = *i;
						cgen_set(g, &e, NULL, NULL, "expr_");
					}
					cgen_write(g, "}");
				} else {
					Identifier ident = d->idents[0];
					if (ident_eq_str(ident, "_")) {
						cgen_expr(g, expr);
						cgen_write(g, ";");
					} else {
						/* set it directly */
						if (expr->kind == EXPR_VAL) {
							/* don't use a temp variable */
							cgen_ident(g, ident);
							cgen_write(g, " = ");
							cgen_val(g, &expr->val, &expr->type);
							cgen_write(g, ";");
						} else {
							Expression e = {0};
							e.kind = EXPR_IDENT;
							e.type = d->type;
							e.flags = EXPR_FOUND_TYPE;
							e.ident = ident;
							cgen_set(g, &e, NULL, expr, NULL);
						}
					}
				}
			}
		}
		cgen_nl(g);
	}
}

static void cgen_ret(CGenerator *g, Block *returning_from, Expression *ret_expr) {
	FnExpr *f = g->fn;
	if (ret_expr) {
		cgen_expr_pre(g, ret_expr);
		if (cgen_uses_ptr(&f->ret_type)) {
			if (f->ret_type.kind == TYPE_TUPLE) {
				cgen_set_tuple(g, NULL, NULL, "*ret__", ret_expr);
			} else {
				cgen_set(g, NULL, "*ret__", ret_expr, NULL);
			}
		} else {
			/* set ret_ to ret_expr */
			cgen_type_pre(g, &ret_expr->type);
			cgen_write(g, " ret_");
			cgen_type_post(g, &ret_expr->type);
			cgen_write(g, "; ");
			cgen_set(g, NULL, "ret_", ret_expr, NULL);
			cgen_nl(g);
		}

	}
	cgen_deferred_up_to(g, returning_from);
	if (f->ret_decls) {
		if (f->ret_type.kind == TYPE_TUPLE) {
			Expression tuple_expr = {0};
			tuple_expr.flags = EXPR_FOUND_TYPE;
			tuple_expr.type = f->ret_type;
			tuple_expr.kind = EXPR_TUPLE;
			tuple_expr.tuple = NULL;
			arr_set_len(tuple_expr.tuple, arr_len(f->ret_type.tuple));
			int idx = 0;
			arr_foreach(f->ret_decls, Declaration, d) {
				arr_foreach(d->idents, Identifier, ident) {
					Expression *e = &tuple_expr.tuple[idx];
					e->flags = EXPR_FOUND_TYPE;
					e->type = f->ret_type.tuple[idx];
					e->kind = EXPR_IDENT;
					e->ident = *ident;
					++idx;
				}
			}
			cgen_set_tuple(g, NULL, NULL, "*ret__", &tuple_expr);
			arr_clear(tuple_expr.tuple);
		} else if (cgen_uses_ptr(&f->ret_type)) {
			Expression expr = {0};
			expr.flags = EXPR_FOUND_TYPE;
			expr.type = f->ret_type;
			expr.kind = EXPR_IDENT;
			expr.ident = f->ret_decls[0].idents[0];
			cgen_set(g, NULL, "*ret__", &expr, NULL);
			cgen_writeln(g, ";");
			cgen_writeln(g, "return;");
		} else {
			cgen_write(g, "return ");
			cgen_ident(g, f->ret_decls[0].idents[0]);
			cgen_writeln(g, ";");
		}
		return;
	}
	if (cgen_uses_ptr(&f->ret_type)) {
	#if 0
		Expression ret = {0};
		ret.kind = EXPR_IDENT;
		ret.ident = ident_get(g->globals, "ret_");
		ret.flags = EXPR_FOUND_TYPE;
		ret.type = f->ret_type;
		#endif
		cgen_writeln(g, " return;");
	} else if (type_is_void(&f->ret_type)) {
		cgen_writeln(g, "return;");
	} else {
		cgen_writeln(g, "return ret_;");
	}
}

static void cgen_stmt(CGenerator *g, Statement *s) {

#ifdef CGEN_EMIT_LINE_NUMBER_COMMENTS
	/* @TODO: add compiler option for this */
	cgen_write(g, "/* %s:%d */", s->where.ctx->filename, s->where.line);
#endif
	switch (s->kind) {
	case STMT_DECL:
		cgen_decl(g, s->decl);
		break;
	case STMT_EXPR: {
		Expression *e = s->expr;
		if ((g->block != NULL || e->kind == EXPR_C) && !type_is_compileonly(&e->type)) {
			cgen_expr_pre(g, e);
			cgen_expr(g, e);
			if (e->kind != EXPR_C)
				cgen_writeln(g, ";");
		}
	} break;
	case STMT_RET: {
		Return *r = s->ret;
		unsigned has_expr = r->flags & RET_HAS_EXPR;
		cgen_ret(g, r->referring_to, has_expr ? &r->expr : NULL);
	} break;
	case STMT_BREAK: {
		Block *b = s->referring_to;
		cgen_deferred_up_to(g, b);
		cgen_write(g, "goto ");
		cgen_lbl(g, b->c.break_lbl);
		cgen_writeln(g, ";");
	} break;
	case STMT_CONT: {
		Block *b = s->referring_to;
		cgen_deferred_up_to_not_including(g, b);
		cgen_write(g, "goto ");
		cgen_lbl(g, b->c.cont_lbl);
		cgen_writeln(g, ";");
	} break;
	case STMT_DEFER:
		arr_add(g->block->deferred, s->defer);
		break;
	case STMT_USE:
	case STMT_MESSAGE:
		break;
	case STMT_INLINE_BLOCK:
		arr_foreach(s->inline_block, Statement, sub)
			cgen_stmt(g, sub);
		break;
	case STMT_IF: {
		If *curr = s->if_;
		while (1) {
			if (curr->cond) {
				cgen_write(g, "if (");
				cgen_truthiness(g, curr->cond);
				cgen_write(g, ") ");
			}
			cgen_block(g, &curr->body, 0);
			if (curr->next_elif) {
				cgen_write(g, " else ");
				curr = curr->next_elif;
			} else break;
		}
	} break;
	case STMT_WHILE: {
		While *w = s->while_;
		cgen_write(g, "while (");
		cgen_expr(g, w->cond);
		cgen_write(g, ") ");
		cgen_block(g, &w->body, 0);
	} break;
	case STMT_FOR: {
		For *fo = s->for_;
		int is_range = fo->flags & FOR_IS_RANGE;
		Declaration *header_decl = &fo->header;
		Identifier val_ident = header_decl->idents[0];
		Identifier index_ident = header_decl->idents[1];
		Type *fo_type = &header_decl->type;
		assert(fo_type->kind == TYPE_TUPLE && arr_len(fo_type->tuple) == 2);
		Type *val_type = &fo_type->tuple[0];
		Type *index_type = &fo_type->tuple[1];
		bool has_val = !ident_eq_str(val_ident, "_");
		bool has_index = !ident_eq_str(index_ident, "_");

		Type *of_type = NULL;
		bool uses_ptr = false;

		if (is_range) {
			cgen_expr_pre(g, fo->range.from);
			if (fo->range.to)
				cgen_expr_pre(g, fo->range.to);
		} else {
			cgen_expr_pre(g, fo->of);
		}
		cgen_write(g, "{");
		if (is_range) {
			if (fo->range.to) {
				/* pre generate to */
				cgen_type_pre(g, val_type);
				cgen_write(g, " to_");
				cgen_type_post(g, val_type);
				cgen_write(g, " = ");
				cgen_expr(g, fo->range.to);
				cgen_write(g, "; ");
			}
			
			/* set value to from */
			if (has_val) {
				cgen_type_pre(g, val_type);
				cgen_write(g, " ");
				cgen_ident(g, val_ident);
				cgen_type_post(g, val_type);
				cgen_write(g, "; ");
				Expression val_expr = {0};
				val_expr.flags = EXPR_FOUND_TYPE;
				val_expr.kind = EXPR_IDENT;
				val_expr.ident = val_ident;
				val_expr.type = *val_type;
				cgen_set(g, &val_expr, NULL, fo->range.from, NULL);
			} else {
				cgen_type_pre(g, val_type);
				cgen_write(g, " val_");
				cgen_type_post(g, val_type);
				cgen_write(g, "; ");
				cgen_set(g, NULL, "val_", fo->range.from, NULL);
			}
		} else {
			of_type = &fo->of->type;
			if (of_type->kind == TYPE_PTR) {
				uses_ptr = true;
				of_type = of_type->ptr;
			}
			
			/* pre-generate of */
			switch (of_type->kind) {
			case TYPE_SLICE:
				cgen_type_pre(g, of_type);
				cgen_write(g, " of_");
				cgen_type_post(g, of_type);
				cgen_write(g, " = ");
				if (uses_ptr) cgen_write(g, "*");
				cgen_expr(g, fo->of);
				cgen_write(g, "; ");
				break;
			case TYPE_ARR:
				cgen_type_pre(g, of_type->arr.of);
				cgen_write(g, " (* of_)");
				cgen_type_post(g, of_type->arr.of);
				cgen_write(g, " = ");
				if (uses_ptr) cgen_write(g, "*");
				cgen_expr(g, fo->of);
				cgen_write(g, "; ");
				break;
			default: assert(0); break;
			}
		}

		if (has_index) {
			cgen_type_pre(g, index_type);
			cgen_write(g, " ");
			cgen_ident(g, index_ident);
			cgen_type_post(g, index_type); /* not needed yet, but keeping it here to prevent potential future problems */
			cgen_write(g, " = 0; ");
		}

		cgen_write(g, "for (");
		
		if (!is_range) {
			cgen_type_pre(g, val_type);
			cgen_write(g, "(%sp_)", uses_ptr ? "" : "*");
			cgen_type_post(g, val_type);
			cgen_write(g, " = ");
			switch (of_type->kind) {
			case TYPE_ARR:
				cgen_write(g, "of_");
				break;
			case TYPE_SLICE:
				cgen_write(g, "of_.data");
				break;
			default: assert(0); break;
			}
			cgen_write(g, ", (*end_) = p_ + ");
			switch (of_type->kind) {
			case TYPE_ARR:
				cgen_write(g, U64_FMT, (U64)of_type->arr.n);
				break;
			case TYPE_SLICE:
				cgen_write(g, "of_.len");
				break;
			default: assert(0); break;
			}
		}

		cgen_write(g, "; ");
		if (!(is_range && !fo->range.to)) { /* if it's finite */
			if (is_range) {
				if (has_val)
					cgen_ident(g, val_ident);
				else
					cgen_write(g, "val_");
				bool positive_step
					= fo->range.stepval == NULL || val_is_nonnegative(*fo->range.stepval, val_type);
				cgen_write(g, " %c= to_", positive_step ? '<' : '>');
			} else {
				cgen_write(g, "p_ != end_");
			}
		}
		cgen_write(g, "; ");
		if (is_range) {
			if (fo->range.stepval) {
				cgen_val_pre(g, fo->range.stepval, val_type);
			}
			if (has_val)
				cgen_ident(g, val_ident);
			else
				cgen_write(g, "val_");
			cgen_write(g, " += ");
			if (fo->range.stepval) {
				cgen_val(g, fo->range.stepval, val_type);
			} else {
				cgen_write(g, "1");
			}
			if (has_index) cgen_write(g, ", ");
		} else {
			cgen_write(g, "++p_");
			if (has_index) cgen_write(g, ", ");
		}
		if (has_index) {
			cgen_write(g, "++");
			cgen_ident(g, index_ident);
		}
		cgen_write(g, ") {");
		cgen_nl(g);
		if (has_val) {
			if (!is_range) {
				cgen_type_pre(g, val_type);
				cgen_write(g, " ");
				cgen_ident(g, val_ident);
				cgen_type_post(g, val_type);
				cgen_write(g, "; ");
				if (uses_ptr) {
					cgen_ident(g, val_ident);
					cgen_write(g, " = p_;");
					cgen_nl(g);
				} else {
					Expression set_expr = {0};
					set_expr.kind = EXPR_IDENT;
					set_expr.ident = val_ident;
					set_expr.type = *val_type;
					set_expr.flags = EXPR_FOUND_TYPE;
				
					cgen_set(g, &set_expr, NULL, NULL, "(*p_)");
					cgen_nl(g);
				}
			}
		}
		cgen_block(g, &fo->body, CGEN_BLOCK_NOBRACES);
		cgen_deferred_from_block(g, &fo->body);
		cgen_write(g, "}}");
		if (fo->body.c.break_lbl) {
			cgen_lbl(g, fo->body.c.break_lbl);
			cgen_writeln(g, ":;");
		}
	} break;
	case STMT_BLOCK:
		cgen_block(g, s->block, 0);
		break;
	case STMT_INCLUDE:
		assert(0);
		break;
	}
}

static void cgen_fn_decl(CGenerator *g, FnExpr *f) {
	if (f->flags & FN_EXPR_FOREIGN) {
		Type *t = &f->foreign.type;
		/* foreign function declaration */
		cgen_write(g, "extern ");
		Type *fn_types = t->fn.types;
		const char *foreign_name = f->foreign.name;
		CType *ctypes = f->foreign.ctypes;
		if (ctypes[0].kind == CTYPE_NONE) {
			cgen_type_pre(g, &fn_types[0]);
		} else {
			cgen_ctype(g, &ctypes[0]);
		}
		cgen_write(g, " %s", foreign_name);
		cgen_write(g, "(");
		
		for (size_t i = 1; i < arr_len(fn_types); ++i) {
			if (i > 1)
				cgen_write(g, ", ");
			CType *csub = &ctypes[i];
			if (csub->kind == CTYPE_NONE) {
				Type *sub = &fn_types[i];
				cgen_type_pre(g, sub);
				cgen_type_post(g, sub);
			} else {
				cgen_ctype(g, csub);
			}
		}
		cgen_write(g, ")");
		if (ctypes[0].kind == CTYPE_NONE)
			cgen_type_post(g, &fn_types[0]);
		cgen_write(g, ";");

		if (!f->c.name || !ident_eq_str(f->c.name, foreign_name) || g->nms != NULL) {
			cgen_write(g, "static ");
			if (ctypes[0].kind == CTYPE_NONE) {
				cgen_type_pre(g, &fn_types[0]);
			} else {
				cgen_ctype(g, &ctypes[0]);
			}
			
			cgen_write(g, " (* const ");
			cgen_fn_name(g, f);
			cgen_write(g, ")(");
			for (size_t i = 1; i < arr_len(fn_types); ++i) {
				if (i > 1)
					cgen_write(g, ", ");
				CType *csub = &ctypes[i];
				if (csub->kind == CTYPE_NONE) {
					Type *sub = &fn_types[i];
					cgen_type_pre(g, sub);
					cgen_type_post(g, sub);
				} else {
					cgen_ctype(g, csub);
				}
			}
			cgen_write(g, ")");
			if (ctypes[0].kind == CTYPE_NONE)
				cgen_type_post(g, &fn_types[0]);
			cgen_write(g, "= %s;", foreign_name);
		}
		cgen_nl(g);
		return;
	}
	if (cgen_should_gen_fn(f)) {
		cgen_fn_header(g, f);
		cgen_write(g, ";");
		cgen_nl(g);
	}
}

static void cgen_global_decl(CGenerator *g, Declaration *d) {
	if (cgen_fn_is_direct(g, d)) {
		/* handled by cgen_fn_decl */
	} else {
		/* global variables */
		for (int i = 0, n_idents = (int)arr_len(d->idents); i < n_idents; ++i) {
			Identifier ident = d->idents[i];
			Type *type = decl_type_at_index(d, i);
			Value *val = NULL;
			if (d->flags & DECL_HAS_EXPR)
				assert(d->flags & DECL_FOUND_VAL);
			if (d->flags & DECL_FOUND_VAL)
				val = decl_val_at_index(d, i);
			if (!type_is_compileonly(type)) {
				if (val) cgen_val_pre(g, val, type);
				if (!(d->flags & DECL_EXPORT))
					cgen_write(g, "static ");
				cgen_type_pre(g, type);
				cgen_write(g, " ");
				cgen_ident(g, ident);
				cgen_type_post(g, type);
				if (val) {
					cgen_write(g, " = ");
					cgen_val(g, val, type);
				} else {
					cgen_write(g, " = ");
					cgen_zero_value(g, type);
				}
				cgen_write(g, ";");
				cgen_nl(g);
			}
		}
	}
}

static void cgen_file(CGenerator *g, ParsedFile *f, Typer *tr) {
	g->block = NULL;
	g->nms = NULL;
	g->fn = NULL;
	g->file = f;

	cgen_write(g, "#include <stdint.h>\n"
			   "#include <stddef.h>\n"
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
			   "typedef struct { void *data; i64 len; } slice_;\n"
			   "#define false ((bool)0)\n"
			   "#define true ((bool)1)\n"
			   "#ifdef __linux__\n" /* see also toc.c */
			   "#define platform__ " stringify(PLATFORM_LINUX) "\n"
			   "#elif defined _WIN32\n"
			   "#define platform__ " stringify(PLATFORM_WINDOWS) "\n"
			   "#elif defined __APPLE__\n"
			   "#define platform__ " stringify(PLATFORM_OSX) "\n"
			   "#elif defined __FreeBSD__\n"
			   "#define platform__ " stringify(PLATFORM_FREEBSD) "\n"
			   "#elif defined __OpenBSD__\n"
			   "#define platform__ " stringify(PLATFORM_OPENBSD) "\n"
			   "#elif defined __unix__\n"
			   "#define platform__ " stringify(PLATFORM_MISC_UNIX) "\n"
			   "#else\n"
			   "#define platform__ " stringify(PLATFORM_OTHER) "\n"
			   "#endif\n"
			   "static slice_ mkslice_(void *data, i64 len) { slice_ ret; ret.data = data; ret.len = len; return ret; }\n");
	cgen_write(g, "/* types */\n");
	/* struct declarations */
	arr_foreach(tr->all_structs, StructDefPtr, sdefp) {
		StructDef *sdef = *sdefp;
		cgen_write(g, "struct ");
		if (!sdef->name) {
			sdef->c.id = ++g->ident_counter;
		} 
		cgen_struct_name(g, sdef);
		cgen_write(g, ";");
		cgen_nl(g);
	}

	/* struct definitions */
	arr_foreach(tr->all_structs, StructDefPtr, sdefp) {
		StructDef *sdef = *sdefp;
		cgen_write(g, "struct ");
		cgen_struct_name(g, sdef);
		cgen_write(g, "{");
		cgen_nl(g);
		++g->indent_lvl;
		arr_foreach(sdef->fields, Field, field) {
			cgen_type_pre(g, field->type);
			cgen_write(g, " ");
			cgen_ident_simple(g, field->name);
			cgen_type_post(g, field->type);
			cgen_write(g, ";");
			cgen_nl(g);
		}
		--g->indent_lvl;
		cgen_write(g, "};");
		cgen_nl(g);
	}
	
	cgen_write(g, "/* declarations */\n");
	/* function declarations */
	arr_foreach(tr->all_fns, FnWithCtx, fn_ctx) {
		g->nms = fn_ctx->nms;
		g->block = fn_ctx->block;
		FnExpr *fn = fn_ctx->fn;
		if (!fn->c.name) {
			fn->c.id = ++g->ident_counter;
		}
		cgen_fn_decl(g, fn);
	}

	/* global (non-function) declarations */
	arr_foreach(tr->all_globals, DeclWithCtx, dctx) {
		g->nms = dctx->nms;
		g->block = dctx->block;
		cgen_global_decl(g, dctx->d);
	}

	cgen_write(g, "/* code */\n");
	cgen_write(g, "int main() {\n\tmain_();\n\treturn 0;\n}\n\n");
	/* function definitions */
	arr_foreach(tr->all_fns, FnWithCtx, fn_ctx) {
		g->nms = fn_ctx->nms;
		g->block = fn_ctx->block;
		cgen_fn(g, fn_ctx->fn);
	}
	g->nms = NULL;
	g->block = NULL;

	arr_foreach(f->stmts, Statement, s) {
		cgen_stmt(g, s);
	}
}
