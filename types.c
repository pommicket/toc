/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static Status types_stmt(Typer *tr, Statement *s);
static Status type_resolve(Typer *tr, Type *t, Location where);


static inline void *typer_malloc(Typer *tr, size_t bytes) {
	return allocr_malloc(tr->allocr, bytes);
}

static inline void *typer_calloc(Typer *tr, size_t n, size_t sz) {
	return allocr_calloc(tr->allocr, n, sz);
}

static inline void *typer_arr_add_(Typer *tr, void **arr, size_t sz) {
	return arr_adda_(arr, sz, tr->allocr);
}

static inline void typer_block_enter(Typer *tr, Block *b) {
	*(Block **)arr_adda(&tr->blocks, tr->allocr) = b;
	tr->block = b;
}

static inline void typer_block_exit(Typer *tr) {
	arr_remove_lasta(&tr->blocks, tr->allocr);
	tr->block = *(Block **)arr_last(tr->blocks);
}


static size_t compiler_sizeof_builtin(BuiltinType b) {
	switch (b) {
	case BUILTIN_I8: return sizeof(I8);
	case BUILTIN_U8: return sizeof(U8);
	case BUILTIN_I16: return sizeof(I16);
	case BUILTIN_U16: return sizeof(U16);
	case BUILTIN_I32: return sizeof(I32);
	case BUILTIN_U32: return sizeof(U32);
	case BUILTIN_I64: return sizeof(I64);
	case BUILTIN_U64: return sizeof(U64);
	case BUILTIN_F32: return sizeof(F32);
	case BUILTIN_F64: return sizeof(F64);
	case BUILTIN_CHAR: return sizeof(char); /* = 1 */
	case BUILTIN_BOOL: return sizeof(bool);
	case BUILTIN_TYPE: return sizeof(Type *);
	case BUILTIN_NMS: return sizeof(Namespace *);
	}
	assert(0);
	return 0;
}
static size_t compiler_alignof_builtin(BuiltinType b) {
	switch (b) {
	case BUILTIN_I8: return toc_alignof(I8);
	case BUILTIN_U8: return toc_alignof(U8);
	case BUILTIN_I16: return toc_alignof(I16);
	case BUILTIN_U16: return toc_alignof(U16);
	case BUILTIN_I32: return toc_alignof(I32);
	case BUILTIN_U32: return toc_alignof(U32);
	case BUILTIN_I64: return toc_alignof(I64);
	case BUILTIN_U64: return toc_alignof(U64);
	case BUILTIN_F32: return toc_alignof(F32);
	case BUILTIN_F64: return toc_alignof(F64);
	case BUILTIN_CHAR: return toc_alignof(char);
	case BUILTIN_BOOL: return toc_alignof(bool);
	case BUILTIN_TYPE: return toc_alignof(Type *);
	case BUILTIN_NMS: return toc_alignof(Namespace *);
	}
	assert(0);
	return 0;
}

/* finds offsets and size */
static Status struct_find_offsets(StructDef *s) {
	/* assume the align of a struct is the greatest align out of its children's */
	if (!(s->flags & STRUCT_DEF_FOUND_OFFSETS)) {
		if (s->flags & STRUCT_DEF_FINDING_OFFSETS) {
			err_print(s->where, "Circular dependency in struct!");
			return false;
		}
		s->flags |= STRUCT_DEF_FINDING_OFFSETS;
		size_t bytes = 0;
		size_t total_align = 1;
		arr_foreach(s->fields, Field, f) {
			size_t size = compiler_sizeof(&f->type);
			if (size == SIZE_MAX) {
				info_print(f->where, "... while descending into this field of a struct.");
				return false;
			}
			size_t falign = compiler_alignof(&f->type);
			if (falign > total_align)
				total_align = falign;
			/* align */
			bytes += ((falign - bytes) % falign + falign) % falign; /* = -bytes mod falign */
			assert(bytes % falign == 0);
			f->offset = bytes;
			/* add size */
			bytes += size;
		}
		bytes += ((total_align - bytes) % total_align + total_align) % total_align; /* = -bytes mod align */
		s->size = bytes;
		s->align = total_align;
		s->flags |= STRUCT_DEF_FOUND_OFFSETS;
	}
	return true;
}

static size_t compiler_alignof(Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_BUILTIN:
		return compiler_alignof_builtin(t->builtin);
	case TYPE_VOID:
		return 1;
	case TYPE_FN:
		return toc_alignof(FnExpr *);
	case TYPE_PTR:
		return toc_alignof(void *);
	case TYPE_TUPLE:
		return toc_alignof(Value *);
	case TYPE_ARR:
		return compiler_alignof(t->arr.of);
	case TYPE_SLICE:
		if (sizeof(void *) > sizeof(size_t))
			return toc_alignof(void *);
		else
			return toc_alignof(size_t);
	case TYPE_STRUCT:
		if (!struct_find_offsets(t->struc))
			return SIZE_MAX;
		return t->struc->align;
	case TYPE_UNKNOWN:
	case TYPE_EXPR:
		break;
	}
	assert(0);
	return 0;
}

/* size of a type at compile time */
static size_t compiler_sizeof(Type *t) {
	Value v;
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_BUILTIN:
		return compiler_sizeof_builtin(t->builtin);
	case TYPE_FN:
		return sizeof v.fn;
	case TYPE_PTR:
		return sizeof v.ptr;
	case TYPE_ARR:
		return t->arr.n * compiler_sizeof(t->arr.of);
	case TYPE_TUPLE:
		return sizeof v.tuple;
	case TYPE_SLICE:
		return sizeof v.slice;
	case TYPE_STRUCT: {
		if (!struct_find_offsets(t->struc))
			return SIZE_MAX;
		return t->struc->size;
	} break;
	case TYPE_VOID:
	case TYPE_UNKNOWN:
		return 0;
	case TYPE_EXPR:
		break;
	}
	assert(0);
	return 0;
}


#define typer_arr_add(tr, a) typer_arr_add_(tr, (void **)(a), sizeof **(a))

static bool type_eq(Type *a, Type *b) {
	if (a->kind == TYPE_UNKNOWN || b->kind == TYPE_UNKNOWN)
		return true; /* allow things such as 3 + #C("5") */
	assert(a->flags & TYPE_IS_RESOLVED);
	assert(b->flags & TYPE_IS_RESOLVED);
	
	if (a->kind != b->kind) return false;
	if (b->flags & TYPE_IS_FLEXIBLE) {
		Type *tmp = a;
		a = b;
		b = tmp;
	}

	if (a->flags & TYPE_IS_FLEXIBLE) {
		if (b->flags & TYPE_IS_FLEXIBLE) return true;
		assert(a->kind == TYPE_BUILTIN);
		
		if (type_builtin_is_float(a->builtin)) {
			return type_builtin_is_float(b->builtin);
		}
		assert(a->builtin == BUILTIN_I64);
		return type_builtin_is_numerical(b->builtin);
	}
	switch (a->kind) {
	case TYPE_VOID: return true;
	case TYPE_UNKNOWN: assert(0); return false;
	case TYPE_BUILTIN:
		return a->builtin == b->builtin;
	case TYPE_STRUCT:
		return a->struc == b->struc;
	case TYPE_FN: {
		if (arr_len(a->fn.types) != arr_len(b->fn.types)) return false;
		Type *a_types = a->fn.types, *b_types = b->fn.types;
		Constness *a_constness = a->fn.constness, *b_constness = b->fn.constness;
		for (size_t i = 0; i < arr_len(a->fn.types); ++i) {
			Constness const_a = CONSTNESS_NO, const_b = CONSTNESS_NO;
			if (a_constness)
				const_a = a_constness[i];
			if (b_constness)
				const_b = b_constness[i];
			if ((const_a == CONSTNESS_NO && const_b == CONSTNESS_YES)
				|| (const_a == CONSTNESS_YES && const_b == CONSTNESS_NO))
				return false;
			if (!type_eq(&a_types[i], &b_types[i]))
				return false;
			
		}
		return true;
	}
	case TYPE_TUPLE: {
		if (arr_len(a->tuple) != arr_len(b->tuple)) return false;
		Type *a_types = a->tuple, *b_types = b->tuple;
		for (size_t i = 0; i < arr_len(a->tuple); ++i) {
			if (!type_eq(&a_types[i], &b_types[i]))
				return false;
		}
		return true;
	}
	case TYPE_ARR:
		if (a->arr.n != b->arr.n) return false;
		return type_eq(a->arr.of, b->arr.of);
	case TYPE_SLICE:
		return type_eq(a->slice, b->slice);
	case TYPE_PTR:
		return type_eq(a->ptr, b->ptr);
	case TYPE_EXPR:
		break;
	}
	assert(0);
	return false;
}

/* expected must equal got, or an error will be produced */
static Status type_must_eq(Location where, Type *expected, Type *got) {
	if (!type_eq(expected, got)) {
		char *str_ex = type_to_str(expected);
		char *str_got = type_to_str(got);
		err_print(where, "Type mismatch: expected %s, but got %s.", str_ex, str_got);
		return false;
	}
	return true;
}

/* prints an error and returns false if the given expression is not an l-value */
static Status expr_must_lval(Expression *e) {
	/* NOTE: make sure you update eval when you change this */
	switch (e->kind) {
	case EXPR_IDENT: {
		Identifier i = e->ident;
		if (i->decl_kind == IDECL_DECL) {
			Declaration *d = i->decl;
			if (d->flags & DECL_IS_CONST) {
				char *istr = ident_to_str(i);
				err_print(e->where, "Use of constant %s as a non-constant expression.", istr);
				info_print(d->where, "%s was declared here.", istr);
				return false;
			}

		}
		return true;
	}
	case EXPR_UNARY_OP:
		if (e->unary.op == UNARY_DEREF) return true;
		if (e->unary.op == UNARY_LEN) {
			Type *of_type = &e->unary.of->type;
			if (of_type->kind != TYPE_PTR && !expr_must_lval(e->unary.of)) { /* can't set length of a non-lvalue slice */
				return false;
			}
			
			return of_type->kind == TYPE_SLICE
				|| (of_type->kind == TYPE_PTR
					&& of_type->kind == TYPE_SLICE);
		}
		err_print(e->where, "Cannot use operator %s as l-value.", unary_op_to_str(e->unary.op));
		return false;
	case EXPR_BINARY_OP:
		switch (e->binary.op) {
		case BINARY_AT_INDEX:
			if (!expr_must_lval(e->binary.lhs))
				return false;
			return true;
		case BINARY_DOT: return true;
		default: break;
		}
		err_print(e->where, "Cannot use operator %s as l-value.", binary_op_to_str(e->binary.op));
		return false;
	case EXPR_TUPLE:
		/* x, y is an lval, but 3, "hello" is not. */
		arr_foreach(e->tuple, Expression, x) {
			if (!expr_must_lval(x)) 
				return false;
		}
		return true;
    default: {
		err_print(e->where, "Cannot use %s as l-value.", expr_kind_to_str(e->kind));
		return false;
	}
	}
	assert(0);
	return false;
}


/* does this type have a Type or a Namespace in it? (e.g. [5]Type, &&Namespace) */
static bool type_is_compileonly(Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_VOID:
	case TYPE_UNKNOWN:
		return false;
	case TYPE_BUILTIN:
		return t->builtin == BUILTIN_TYPE || t->builtin == BUILTIN_NMS;
	case TYPE_PTR:
		return type_is_compileonly(t->ptr);
	case TYPE_SLICE:
		return type_is_compileonly(t->slice);
	case TYPE_ARR:
		return type_is_compileonly(t->arr.of);
	case TYPE_FN:
		arr_foreach(t->fn.types, Type, sub) {
			if (sub->flags & TYPE_IS_RESOLVED) /* for templates */ {
				if (type_is_compileonly(sub))
					return true;
			} else {
				return true;
			}
		}
		return false;
	case TYPE_TUPLE:
		arr_foreach(t->tuple, Type, sub)
			if (type_is_compileonly(sub))
				return true;
		return false;
	case TYPE_STRUCT:
		return false; /* structs can only have non-compileonly members */
	case TYPE_EXPR: break;
	}
	assert(0);
	return false;
}

/* returns NULL if an error occured */
static char *eval_expr_as_cstr(Typer *tr, Expression *e, const char *what_is_this) {
	Value e_val;
	if (!types_expr(tr, e))
		return NULL;
	if (!type_is_slicechar(&e->type)) {
		char *got = type_to_str(&e->type);
		err_print(e->where, "Expected []char for %s, but got %s.", what_is_this, got);
		free(got);
		return NULL;
	}
	if (!eval_expr(tr->evalr, e, &e_val))
		return NULL;
	Slice e_slice = e_val.slice;
	char *str = typer_malloc(tr, (size_t)e_slice.n + 1);
	str[e_slice.n] = 0;
	memcpy(str, e_slice.data, (size_t)e_slice.n);
	return str;
}

enum {
	  /* is f an instance? (changes behaviour a bit) */
	  TYPE_OF_FN_IS_INSTANCE = 0x01
};

static Status type_of_fn(Typer *tr, FnExpr *f, Type *t, U16 flags) {

	if (f->flags & FN_EXPR_FOREIGN) {
		/* we've already mostly determined the type in parse_expr */
		if (!type_resolve(tr, &f->foreign.type, f->where))
			return false;
		*t = f->foreign.type;
		char *name_cstr = eval_expr_as_cstr(tr, f->foreign.name_expr, "foreign name");
		if (!name_cstr)
			return false;
		f->foreign.name = name_cstr;
		if (f->foreign.lib_expr) {
			char *lib_cstr = eval_expr_as_cstr(tr, f->foreign.lib_expr, "foreign library name");
			if (!lib_cstr)
				return false;
			f->foreign.lib = lib_cstr;
		} else {
			f->foreign.lib = NULL;
		}
		return true;
	}

	t->kind = TYPE_FN;
	t->fn.types = NULL;
	t->fn.constness = NULL; /* OPTIM: constness doesn't need to be a dynamic array */
	t->flags = 0;
	bool success = true;
	bool entered_fn = false;
	size_t param_idx;
	FnExpr *prev_fn = tr->fn;
	FnExpr fn_copy = {0};
	
	/* f has compile time params, but it's not an instance! */
	bool generic = !(flags & TYPE_OF_FN_IS_INSTANCE) && fn_has_any_const_params(f);
	if (generic) {
		Copier cop = copier_create(tr->allocr, &f->body);
		copy_fn_expr(&cop, &fn_copy, f, false);
		f = &fn_copy;
	}
	size_t idx = 0;
	bool has_constant_params = false;
	Type *ret_type = typer_arr_add(tr, &t->fn.types);
	tr->fn = f;
	typer_block_enter(tr, &f->body);
	size_t nparams = arr_len(f->params);
	entered_fn = true;
	for (param_idx = 0; param_idx < nparams; ++param_idx) {
		Declaration *param = &f->params[param_idx];
		if (!generic) {
			if (!types_decl(tr, param)) {
				success = false;
				goto ret;
			}
			
			if (param->type.kind == TYPE_TUPLE) {
				err_print(param->where, "Functions can't have tuple parameters.");
				success = false;
				goto ret;
			}
			
			if (param->flags & DECL_HAS_EXPR) {
				if (param->expr.kind != EXPR_VAL) {
					Value val;
					if (!eval_expr(tr->evalr, &param->expr, &val)) {
						info_print(param->where, "Was trying to evaluate default arguments (which must be constants!)");
						success = false;
						goto ret;
					}
					param->expr.kind = EXPR_VAL;
					param->expr.val = val;
					if (param->expr.type.flags & TYPE_IS_FLEXIBLE) {
						/* cast to the annotated type, if one exists */
						if (param->flags & DECL_ANNOTATES_TYPE) {
							val_cast(&param->expr.val, &param->expr.type, &param->expr.val, &param->type);
							param->expr.type = param->type;
						}
					}
				}
			} 
		}
		U32 is_at_all_const = param->flags & (DECL_IS_CONST | DECL_SEMI_CONST);
		if (is_at_all_const) {
			if (!t->fn.constness) {
				has_constant_params = true;
				for (size_t i = 0; i < idx; ++i) {
					*(Constness *)typer_arr_add(tr, &t->fn.constness) = CONSTNESS_NO;
				}
			}
		}
		for (size_t i = 0; i < arr_len(param->idents); ++i) {
			Type *param_type = typer_arr_add(tr, &t->fn.types);
			if (!generic) {
				*param_type = param->type;
			} else {
				param_type->flags = 0;
				param_type->kind = TYPE_UNKNOWN;
			}
			if (has_constant_params) {
				Constness constn;
				if (param->flags & DECL_IS_CONST) {
					constn = CONSTNESS_YES;
				} else if (param->flags & DECL_SEMI_CONST) {
					constn = CONSTNESS_SEMI;
				} else {
					constn = CONSTNESS_NO;
				}
				*(Constness *)typer_arr_add(tr, &t->fn.constness) = constn;
			}
			++idx;
		}
	}
	
	if (f->ret_decls && !generic && f->ret_type.kind == TYPE_VOID /* haven't found return type yet */) {
		/* find return type */

		arr_foreach(f->ret_decls, Declaration, d) {
			if (!types_decl(tr, d)) {
				success = false;
				goto ret;
			}
		}
	
		if (arr_len(f->ret_decls) == 1 && arr_len(f->ret_decls[0].idents) == 1) {
			f->ret_type = f->ret_decls[0].type;
		} else {
			f->ret_type.kind = TYPE_TUPLE;
			f->ret_type.flags = TYPE_IS_RESOLVED;
			f->ret_type.was_expr = NULL;
			f->ret_type.tuple = NULL;
			f->ret_type.where = f->ret_decls[0].where;
			arr_foreach(f->ret_decls, Declaration, d) {
				arr_foreach(d->idents, Identifier, i) {
					*(Type *)arr_add(&f->ret_type.tuple) = d->type;
				}
			}
		}
	}
	if (!generic) {
		if (!type_resolve(tr, &f->ret_type, f->ret_type.where)) {
			success = false;
			goto ret;
		}
		if (type_is_compileonly(&f->ret_type)) {
			/* 
			   a function which returns a compile-only type but has non-constant parameters is weird...
			   but might be useful, so let's warn
			*/
			arr_foreach(f->params, Declaration, param) {
				if (!(param->flags & DECL_IS_CONST)) {
					char *s = type_to_str(&f->ret_type);
					warn_print(param->where, "Non-constant parameter in function which returns %s (which is a type which can only be used at run time).", s);
					free(s);
					break;
				}
			}
		}
		t->flags |= TYPE_IS_RESOLVED;
	}
	*ret_type = f->ret_type;

 ret:
	/* cleanup */
	typer_block_exit(tr);
	if (entered_fn) {
		tr->fn = prev_fn;
	}
	return success;
}

/* may modify ident */
static Status type_of_ident(Typer *tr, Location where, Identifier *ident, Type *t) {
	t->flags = 0;
	Identifier i = *ident;
#if 0
#ifdef TOC_DEBUG
	if (i->idents->scope != tr->block) {
		printf("Ident declaration mismatch for this ident:\n");
		print_location(where);
		printf("Typer is typing:\n");
		print_block_location(tr->block);
		printf("But the identifier's scope is:\n");
		print_block_location(i->idents->scope);
	    abort();
	}
#else
	assert(i->idents->scope == tr->block);
#endif
#endif
	if (i->decl_kind == IDECL_NONE) {
		long nblocks = (long)arr_len(tr->blocks);
		long idx;
		for (idx = nblocks - 1; idx >= 0; --idx) {
			Block *b = tr->blocks[idx];
			/* OPTIM: only hash once */
		    Identifier translated = ident_translate(i, b ? &b->idents : tr->globals);
			if (!translated) continue;
			if (translated->decl_kind != IDECL_NONE) {
				/* printf("translated %s from\n", ident_to_str(i)); */
				/* print_block_location(i->idents->scope); */
				/* printf(" to \n"); */
				/* print_block_location(translated->idents->scope); */
				
				i = *ident = translated;
				break;
			}
		}
		if (idx == -1) {
			char *s = ident_to_str(i);
			err_print(where, "Undeclared identifier: %s", s);
			free(s);
			return false;
		}
	}
	
	switch (i->decl_kind) {
	case IDECL_DECL: {
		Declaration *d = i->decl;
		bool captured = false;
		if (ident_scope(i) != NULL && !(ident_scope(i)->flags & BLOCK_IS_NMS)) {
			Block *decl_scope = ident_scope(i);
			if (!(decl_scope->flags & BLOCK_IS_NMS)) {
				/* go back through scopes */
				for (Block **block = arr_last(tr->blocks); *block && *block != decl_scope; --block) {
					if ((*block)->flags & BLOCK_IS_FN) {
						captured = true;
						break;
					}
				}
			}
		}
		if (captured && !(d->flags & DECL_IS_CONST)) {
			err_print(where, "Variables cannot be captured into inner functions (but constants can).");
			return false;
		}
		if ((d->flags & DECL_HAS_EXPR) && (d->expr.kind == EXPR_TYPE)) {
			/* allow using a type before declaring it */
			t->kind = TYPE_BUILTIN;
			t->builtin = BUILTIN_TYPE;
			t->flags = TYPE_IS_RESOLVED;
			return true;
		}
		 
		/* are we inside this declaration? */
		typedef Declaration *DeclarationPtr;
		arr_foreach(tr->in_decls, DeclarationPtr, in_decl) {
			if (d == *in_decl) {
				assert(d->flags & DECL_HAS_EXPR); /* we can only be in decls with an expr */
				if (d->expr.kind != EXPR_FN) { /* it's okay if a function references itself */
					/* if we've complained about it before when we were figuring out the type, don't complain again */
					if (!(d->flags & DECL_ERRORED_ABOUT_SELF_REFERENCE)) {
						char *s = ident_to_str(i);
						err_print(where, "Use of identifier %s in its own declaration.", s);
						free(s);
						info_print(d->where, "Declaration was here.");
						d->flags |= DECL_ERRORED_ABOUT_SELF_REFERENCE;
					}
					return false;
				}
			}
		}
	
		if (d->flags & DECL_FOUND_TYPE) {
			*t = *decl_type_at_index(d, decl_ident_index(d, i));
			return true;
		} else {
			if ((d->flags & DECL_HAS_EXPR) && (d->expr.kind == EXPR_FN)) {
				/* allow using a function before declaring it */
				if (!type_of_fn(tr, d->expr.fn, &d->expr.type, 0)) return false;
				*t = d->expr.type;
				return true;
			} else {
				if (where.start <= d->where.end) {
					char *s = ident_to_str(i);
					err_print(where, "Use of identifier %s before its declaration.", s);
					info_print(d->where, "%s will be declared here.", s);
					free(s);
				} else {
					/* let's type the declaration, and redo this (for evaling future functions) */
					if (!types_decl(tr, d)) return false;
					return type_of_ident(tr, where, ident, t);
				}
				return false;
			}
		}
	} break;
	case IDECL_EXPR: {
		Expression *e = i->decl_expr;
		/* are we inside this expr? */
		typedef Expression *ExprPtr;
		arr_foreach(tr->in_exprs, ExprPtr, in_e) {
			if (*in_e == e) {
				char *s = ident_to_str(i);
				err_print(where, "Use of identifier %s in its own declaration.", s);
				free(s);
				return false;
			}
		}
		switch (e->kind) {
		case EXPR_FOR: {
			ForExpr *fo = e->for_;
			if (i == fo->index) {
				t->kind = TYPE_BUILTIN;
				t->builtin = BUILTIN_I64;
			} else {
				assert(i == fo->value);
				*t = fo->type;
			}
		} break;
		default: assert(0); break;
		}
	} break;
	case IDECL_NONE: {
		char *s = ident_to_str(i);
		err_print(where, "Undeclared identifier: %s", s);
		free(s);
		return false;
	}
	}
	return true;
}

/* fixes the type (replaces [5+3]int with [8]int, etc.) */
static Status type_resolve(Typer *tr, Type *t, Location where) {
	Evaluator *ev = tr->evalr;
	if (t->flags & TYPE_IS_RESOLVED) return true;
	t->was_expr = NULL;
	switch (t->kind) {
	case TYPE_ARR: {
		/* it's an array */
		Value val;
		Expression *n_expr = t->arr.n_expr;
		if (!types_expr(tr, n_expr)) return false;
		if (n_expr->type.kind == TYPE_UNKNOWN) {
			err_print(n_expr->where, "Cannot determine type of array size at compile time.");
			return false;
		}
		if (n_expr->type.kind != TYPE_BUILTIN || !type_builtin_is_int(n_expr->type.builtin)) {
			char *s = type_to_str(&n_expr->type);
			err_print(n_expr->where, "Cannot use type %s as the size of an array (it's not an integer type).", s);
			free(s);
			return false;
		}
		if (!eval_expr(ev, n_expr, &val))
			return false;

		U64 size;
		if (type_builtin_is_signed(n_expr->type.builtin)) {
			I64 ssize = val_to_i64(&val, n_expr->type.builtin);
			if (ssize < 0) {
				err_print(t->arr.n_expr->where, "Negative array length (" I64_FMT ")", ssize);
				return false;
			}
			size = (U64)ssize;
		} else {
			size = val_to_u64(&val, n_expr->type.builtin);
		}
		t->arr.n = (U64)size;
		if (!type_resolve(tr, t->arr.of, where))
			return false;
	} break;
	case TYPE_FN:
		arr_foreach(t->fn.types, Type, child_type) {
			if (!type_resolve(tr, child_type, where))
				return false;
		}
		break;
	case TYPE_TUPLE:
		arr_foreach(t->tuple, Type, child_type) {
			if (!type_resolve(tr, child_type, where))
				return false;
		}
		break;
	case TYPE_PTR:
		if (!type_resolve(tr, t->ptr, where))
			return false;
		break;
	case TYPE_SLICE:
		if (!type_resolve(tr, t->slice, where))
			return false;
		break;
	case TYPE_STRUCT: {
		if (!(t->struc->flags & STRUCT_DEF_RESOLVED)) {
			typer_block_enter(tr, &t->struc->scope);
			arr_foreach(t->struc->fields, Field, f) {
				if (!type_resolve(tr, &f->type, where)) {
					typer_block_exit(tr);
					return false;
				}
			}
			arr_foreach(t->struc->constants, Declaration, c) {
				if (!types_decl(tr, c)) {
					typer_block_exit(tr);
					return false;
				}
			}
			typer_block_exit(tr);
			assert(tr->block != &t->struc->scope);
			t->struc->instance_id = 0;
			t->struc->flags |= STRUCT_DEF_RESOLVED;
		}
	} break;
	case TYPE_EXPR: {
		Value typeval;
		if (!types_expr(tr, t->expr))
			return false;
		if (t->expr->type.kind == TYPE_UNKNOWN && tr->err_ctx->have_errored)
			return false; /* silently fail (e.g. if a function couldn't be typed) */
		if (!type_is_builtin(&t->expr->type, BUILTIN_TYPE)) {
			err_print(where, "This expression is not a type, but it's being used as one.");
			return false;
		}
		Expression *expr = t->expr;
		if (!eval_expr(tr->evalr, t->expr, &typeval))
			return false;
		*t = *typeval.type;
		if (t->kind == TYPE_STRUCT) {
			Declaration *params = t->struc->params;
			if (params && !(params[0].flags & DECL_FOUND_VAL)) {
				err_print(where, "Expected arguments to structure, but you didn't provide any.");
				info_print(t->struc->where, "Structure was declared here.");
				return false;
			}
		}
		if (!(t->flags & TYPE_IS_RESOLVED)) {
			/* this can happen with functions returning parameterized structs */
			if (!type_resolve(tr, t, where))
				return false;
		}
		t->was_expr = expr;
	} break;
	case TYPE_UNKNOWN:
	case TYPE_VOID:
	case TYPE_BUILTIN:
		break;
	}
	if (t->kind == TYPE_STRUCT && !!(t->struc->params) == !!(t->struc->instance_id)) { /* don't want it to try to deal with templates */
		if (!struct_find_offsets(t->struc))
			return false;
	}
	assert(t->kind != TYPE_EXPR);
	t->flags |= TYPE_IS_RESOLVED;
	return true;
}


static bool type_can_be_truthy(Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_VOID:
	case TYPE_TUPLE:
	case TYPE_ARR:
	case TYPE_STRUCT:
		return false;
	case TYPE_FN:
	case TYPE_UNKNOWN:
	case TYPE_PTR:
	case TYPE_SLICE:
		return true;
	case TYPE_BUILTIN:
		switch (t->builtin) {
		case BUILTIN_TYPE:
		case BUILTIN_NMS:
			return false;
		case BUILTIN_I8:
		case BUILTIN_U8:
		case BUILTIN_I16:
		case BUILTIN_U16:
		case BUILTIN_I32:
		case BUILTIN_U32:
		case BUILTIN_I64:
		case BUILTIN_U64:
		case BUILTIN_F32:
		case BUILTIN_F64:
		case BUILTIN_CHAR:
		case BUILTIN_BOOL:
			return true;
		}
	case TYPE_EXPR:
		break;
	}
	assert(0);
	return false;
}

typedef enum {
			  CAST_STATUS_NONE,
			  CAST_STATUS_WARN,
			  CAST_STATUS_ERR
} CastStatus;

static CastStatus type_cast_status(Type *from, Type *to) {
	assert(from->flags & TYPE_IS_RESOLVED);
	assert(to->flags & TYPE_IS_RESOLVED);
	
	if (to->kind == TYPE_UNKNOWN)
		return CAST_STATUS_NONE;
	switch (from->kind) {
	case TYPE_UNKNOWN: return CAST_STATUS_NONE;
	case TYPE_STRUCT:
	case TYPE_VOID:
		return CAST_STATUS_ERR;
	case TYPE_BUILTIN:
		switch (from->builtin) {
		case BUILTIN_I8:
		case BUILTIN_U8:
		case BUILTIN_I16:
		case BUILTIN_U16:
		case BUILTIN_I32:
		case BUILTIN_U32:
		case BUILTIN_I64:
		case BUILTIN_U64:
			switch (to->kind) {
			case TYPE_BUILTIN:
				switch (to->builtin) {
				case BUILTIN_I8:
				case BUILTIN_U8:
				case BUILTIN_I16:
				case BUILTIN_U16:
				case BUILTIN_I32:
				case BUILTIN_U32:
				case BUILTIN_I64:
				case BUILTIN_U64:
				case BUILTIN_F32:
				case BUILTIN_F64:
				case BUILTIN_BOOL:
				case BUILTIN_CHAR:
					return CAST_STATUS_NONE;
				case BUILTIN_TYPE:
				case BUILTIN_NMS:
					return CAST_STATUS_ERR;
				}
				assert(0);
				break;
			case TYPE_UNKNOWN:
				return CAST_STATUS_NONE;
			case TYPE_PTR:
				return CAST_STATUS_WARN;
			default:
				return CAST_STATUS_ERR;
			}
			break;
		case BUILTIN_F32:
		case BUILTIN_F64:
			if (to->kind != TYPE_BUILTIN) return CAST_STATUS_ERR;
			switch (to->builtin) {
			case BUILTIN_I8:
			case BUILTIN_U8:
			case BUILTIN_I16:
			case BUILTIN_U16:
			case BUILTIN_I32:
			case BUILTIN_U32:
			case BUILTIN_I64:
			case BUILTIN_U64:
			case BUILTIN_F32:
			case BUILTIN_F64:
			case BUILTIN_BOOL:
				return CAST_STATUS_NONE;
			case BUILTIN_CHAR:
			case BUILTIN_TYPE:
			case BUILTIN_NMS:
				return CAST_STATUS_ERR;
			}
			assert(0);
			break;
		case BUILTIN_CHAR:
			if (to->kind == TYPE_BUILTIN && type_builtin_is_int(to->builtin))
				return CAST_STATUS_NONE;
			return CAST_STATUS_ERR;
		case BUILTIN_BOOL:
			return type_can_be_truthy(to) ? CAST_STATUS_NONE : CAST_STATUS_ERR;
		case BUILTIN_TYPE:
		case BUILTIN_NMS:
			return CAST_STATUS_ERR;
		}
		break;
	case TYPE_TUPLE: return CAST_STATUS_ERR;
	case TYPE_FN:
		if (to->kind == TYPE_PTR || to->kind == TYPE_FN)
			return CAST_STATUS_WARN;
		return CAST_STATUS_ERR;
	case TYPE_PTR:
		if (to->kind == TYPE_BUILTIN && type_builtin_is_int(to->builtin))
			return CAST_STATUS_WARN;
		if (to->kind == TYPE_PTR)
			return CAST_STATUS_NONE;
		if (to->kind == TYPE_FN)
			return CAST_STATUS_WARN;
		/* TODO: Cast from ptr to arr */
		return CAST_STATUS_ERR;
	case TYPE_ARR:
		return CAST_STATUS_ERR;
	case TYPE_SLICE:
		if (to->kind == TYPE_PTR && type_eq(from->slice, to->ptr))
			return CAST_STATUS_NONE;
		return CAST_STATUS_ERR;
	case TYPE_EXPR:
		break;
	}
	assert(0);
	return CAST_STATUS_ERR;
}

static bool arg_is_const(Expression *arg, Constness constness) {
	switch (constness) {
	case CONSTNESS_NO: return false;
	case CONSTNESS_SEMI: return expr_is_definitely_const(arg);
	case CONSTNESS_YES: return true;
	}
	assert(0);
	return false;
}


/* pass NULL for instance if this isn't an instance */
static Status types_fn(Typer *tr, FnExpr *f, Type *t, Instance *instance) {
	if (f->flags & FN_EXPR_FOREIGN) return true;
	FnExpr *prev_fn = tr->fn;
	bool success = true;
	Expression *ret_expr;
	Type *ret_type;
	bool has_named_ret_vals;
	assert(t->kind == TYPE_FN);
	if (instance) {
		f = instance->fn;
	} else {
		if (t->fn.constness)
			return true; /* don't type function body yet; we need to do that for every instance */
	}
	
	tr->fn = f;
	if (!types_block(tr, &f->body)) {
		success = false;
		goto ret;
	}
	ret_expr = f->body.ret_expr;
	ret_type = t->fn.types;
	has_named_ret_vals = f->ret_decls != NULL;
	if (ret_expr) {
		if (!type_eq(ret_type, &ret_expr->type)) {
			char *got = type_to_str(&ret_expr->type);
			char *expected = type_to_str(ret_type);
			err_print(ret_expr->where, "Returning type %s, but function returns type %s.", got, expected);
			if (!instance) /* where will only actually be at the function declaration if it isn't
							  an instance. otherwise, where will be at the calling site, which will already be
							  printed */
				info_print(f->where, "Function declaration is here.");
			free(got); free(expected);
			success = false;
			goto ret;
		}
	} else if (ret_type->kind != TYPE_VOID && !has_named_ret_vals) {
		Statement *stmts = f->body.stmts;
		if (arr_len(stmts)) {
			Statement *last_stmt = (Statement *)stmts + (arr_len(stmts) - 1);
			if (last_stmt->kind == STMT_RET) {
				/*
				  last statement is a return, so it doesn't matter that the function has no return value
				  ideally this would handle if foo { return 5; } else { return 6; }
				*/
				success = true;
				goto ret;
			}
		}
		/* TODO: this should really be at the closing brace, and not the function declaration */
		char *expected = type_to_str(ret_type);
		err_print(token_location(f->body.where.file, f->body.where.end), "No return value in function which returns %s.", expected);
		free(expected);
		info_print(f->where, "Function was declared here:");
		success = false;
		goto ret;
	}
 ret:
	tr->fn = prev_fn;
	return success;
}

/* puts a dynamic array of the argument indices of the parameters into order. *order must be freed, even if function fails */
static Status call_arg_param_order(FnExpr *fn, Type *fn_type, Argument *args, Location where, I16 **orderp) {
	*orderp = NULL;
	assert(fn_type->flags & TYPE_IS_RESOLVED);
	size_t nparams = arr_len(fn_type->fn.types)-1;
	size_t nargs = arr_len(args);
	if (nargs > nparams) {
		err_print(where, "Expected at most %lu argument%s to function, but got %lu.",
				  nparams, plural_suffix(nparams), nargs);
		return false;
	}
    
	
	I16 *order = *orderp =
		/* thanks, gcc, for making me do this! (getting erroneous -Walloc-size-larger-than) */
#if defined __GNUC__ && !defined __clang__
		nparams > PTRDIFF_MAX ? NULL :
#endif
		err_malloc(nparams * sizeof *order);
	for (size_t i = 0; i < nparams; ++i)
		order[i] = -1;
	
	if (fn->flags & FN_EXPR_FOREIGN) {
		I16 i = -1;
		arr_foreach(args, Argument, arg) {
			if (arg->name) {
				err_print(arg->where, "Foreign function calls cannot use named arguments.");
				return false;
			}
			*order++ = ++i;
		}
		return true;
	}
	
	int p = 0; /* counter for sequential parameters */
	Declaration *param = fn->params;
	size_t ident_idx = 0;
	I16 arg_idx = -1;
	arr_foreach(args, Argument, arg) {
		++arg_idx;
		bool named = arg->name != NULL;
		int param_idx = -1;
		if (named) {
			/* named argument */
			int index = 0;
			bool found = false;
			arr_foreach(fn->params, Declaration, pa) {
				arr_foreach(pa->idents, Identifier, id) {
					if (ident_eq_str(*id, arg->name)) {
						found = true;
						break;
					}
					++index;
				}
				if (found) break;
			}
			if (!found) {
				char *name_end = arg->name + ident_str_len(arg->name);
				/* temporarily null-terminate string to print it out */
				char before = *name_end;
				*name_end = 0;
				err_print(arg->where, "Argument '%s' does not appear in declaration of function.", arg->name);
				*name_end = before;
				info_print(fn->where, "Declaration is here.");
				return false;
			}
			param_idx = index;
		} else {
			/* move past inferred parameters because they must be named */
			while (param < (Declaration *)arr_end(fn->params) && (param->flags & DECL_INFER)) {
				++p;
				++ident_idx;
				if (ident_idx == arr_len(param->idents)) {
					++param;
					ident_idx = 0;
				}
			}
			if (param > (Declaration *)arr_last(fn->params)) {
				err_print(arg->where, "Too many arguments to function!");
				info_print(fn->where, "Declaration is here.");
				return false;
			}
			param_idx = p;
		}

		if (param_idx != -1) {
			if (order[param_idx] != -1) {
				err_print(arg->where, "Parameter #%d set twice.", param_idx+1);
				info_print(args[order[param_idx]].where, "Parameter was previously set here.");
			}
			order[param_idx] = arg_idx;
		}

		if (!named) {
			/* sequential order of parameters */
			++p;
			++ident_idx;
			if (ident_idx == arr_len(param->idents)) {
				++param;
				ident_idx = 0;
			}
		}
	}
	size_t param_idx = 0;
	arr_foreach(fn->params, Declaration, decl) {
		arr_foreach(decl->idents, Identifier, ident) {
			if (order[param_idx] == -1) {
				if (!(decl->flags & DECL_HAS_EXPR) && !(decl->flags & DECL_INFER)) {
					char *s = ident_to_str(*ident);
					err_print(where, "Parameter #%lu (%s) was not set in function call.", param_idx+1, s);
					free(s);
					return false;
				}
				
			}
			++param_idx;
		}
	}
	return true;
}

/* 
 *order must be freed, regardless of return value. if (*order)[i] == -1, that parameter was not set.
*/
static Status parameterized_struct_arg_order(StructDef *struc, Argument *args, I16 **order, Location where) {
	size_t nargs = arr_len(args);
	
	/* 
	   it would be nice if this code and the code for arguments to normal functions
	   weren't split into two separate functions.
	*/
	size_t nparams = 0;
	arr_foreach(struc->params, Declaration, param)
		nparams += arr_len(param->idents);

	*order = err_malloc(nparams * sizeof **order);
	
	if (nargs > nparams) {
		err_print(args[nparams].where, "Expected at most %lu argument%s to parameterized type, but got %lu.", nparams, plural_suffix(nparams), nargs);
		return false;
	}
	for (size_t i = 0; i < nparams; ++i)
		(*order)[i] = -1;
	int p = 0; /* sequential parameter */
	I16 argno = 0;
	
	arr_foreach(args, Argument, arg) {
		int param_idx;
		if (arg->name) {
			param_idx = 0;
			arr_foreach(struc->params, Declaration, param) {
				arr_foreach(param->idents, Identifier, ident) {
					if (ident_eq_str(*ident, arg->name))
						goto struct_params_done;
					++param_idx;
				}
			}
		struct_params_done:;
		} else {
			param_idx = p;
			++p;
		}
		if ((*order)[param_idx] != -1) {
			Identifier param_name = NULL;
			int counter = param_idx;
			arr_foreach(struc->params, Declaration, param) {
				arr_foreach(param->idents, Identifier, ident) {
					if (--counter < 0) {
						param_name = *ident;
						break;
					}
				}
				if (param_name) break;
			}

			char *s = ident_to_str(param_name);
			err_print(arg->where, "Parameter #%d (%s) set twice in parameterized type instantiation.", param_idx+1, s);
			free(s);
			return false;
		}
	    (*order)[param_idx] = argno;
		++argno;
	}
	
	p = 0;
	arr_foreach(struc->params, Declaration, param) {
		arr_foreach(param->idents, Identifier, ident) {
			if ((*order)[p] == -1 && !(param->flags & DECL_HAS_EXPR)) {
				char *s = ident_to_str(*ident);
				err_print(where, "Parameter #%d (%s) not set in parameterized struct instantiation.", p+1, s);
				free(s);
				return false;
			}
			++p;
		}
	}
	
	return true;
}

static Value get_builtin_val(BuiltinVal val) {
	Value v;
	switch (val) {
	case BUILTIN_STDOUT:
		v.ptr = stdout;
		break;
	case BUILTIN_STDERR:
		v.ptr = stderr;
		break;
	case BUILTIN_STDIN:
		v.ptr = stdin;
		break;
	case BUILTIN_COMPILING:
		v.boolv = true;
		break;
	case BUILTIN_SIZEOF_SHORT:
		v.i64 = (I64)sizeof(short);
		break;
	case BUILTIN_SIZEOF_INT:
		v.i64 = (I64)sizeof(int);
		break;
	case BUILTIN_SIZEOF_LONG:
		v.i64 = (I64)sizeof(long);
		break;
	case BUILTIN_SIZEOF_LONG_LONG:
		v.i64 = (I64)sizeof(long long);
		break;
	case BUILTIN_SIZEOF_FLOAT:
		v.i64 = (I64)sizeof(float);
		break;
	case BUILTIN_SIZEOF_DOUBLE:
		v.i64 = (I64)sizeof(double);
		break;
	case BUILTIN_SIZEOF_LONG_DOUBLE:
		v.i64 = (I64)sizeof(long double);
		break;
	case BUILTIN_SIZEOF_SIZE_T:
		v.i64 = (I64)sizeof(size_t);
		break;
		/* TODO(eventually): fix these for cross compilation */
	case BUILTIN_TSIZEOF_SHORT:
		v.i64 = (I64)sizeof(short);
		break;
	case BUILTIN_TSIZEOF_INT:
		v.i64 = (I64)sizeof(int);
		break;
	case BUILTIN_TSIZEOF_LONG:
		v.i64 = (I64)sizeof(long);
		break;
	case BUILTIN_TSIZEOF_LONG_LONG:
		v.i64 = (I64)sizeof(long long);
		break;
	case BUILTIN_TSIZEOF_FLOAT:
		v.i64 = (I64)sizeof(float);
		break;
	case BUILTIN_TSIZEOF_DOUBLE:
		v.i64 = (I64)sizeof(double);
		break;
	case BUILTIN_TSIZEOF_LONG_DOUBLE:
		v.i64 = (I64)sizeof(long double);
		break;
	case BUILTIN_TSIZEOF_SIZE_T:
		v.i64 =(I64)sizeof(size_t);
		break;
	}
	return v;
}

static void get_builtin_val_type(Allocator *a, BuiltinVal val, Type *t) {
	t->flags = TYPE_IS_RESOLVED;
	switch (val) {
	case BUILTIN_STDOUT:
	case BUILTIN_STDERR:
	case BUILTIN_STDIN:
		/* use &u8 for FILE * */
		t->kind = TYPE_PTR;
		t->ptr = allocr_calloc(a, 1, sizeof *t->ptr);
		t->ptr->flags = TYPE_IS_RESOLVED;
		t->ptr->kind = TYPE_BUILTIN;
		t->ptr->builtin = BUILTIN_U8;
		break;
	case BUILTIN_COMPILING:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_BOOL;
		break;
	case BUILTIN_SIZEOF_SHORT:
	case BUILTIN_SIZEOF_INT:
	case BUILTIN_SIZEOF_LONG:
	case BUILTIN_SIZEOF_LONG_LONG:
	case BUILTIN_SIZEOF_FLOAT:
	case BUILTIN_SIZEOF_DOUBLE:
	case BUILTIN_SIZEOF_LONG_DOUBLE:
	case BUILTIN_TSIZEOF_SHORT:
	case BUILTIN_TSIZEOF_INT:
	case BUILTIN_TSIZEOF_LONG:
	case BUILTIN_TSIZEOF_LONG_LONG:
	case BUILTIN_TSIZEOF_FLOAT:
	case BUILTIN_TSIZEOF_DOUBLE:
	case BUILTIN_TSIZEOF_LONG_DOUBLE:
	case BUILTIN_SIZEOF_SIZE_T:
	case BUILTIN_TSIZEOF_SIZE_T:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_I64;
		break;
	}
}


static Status types_expr(Typer *tr, Expression *e) {
	if (e->flags & EXPR_FOUND_TYPE) return true;
	Type *t = &e->type;
	t->flags = TYPE_IS_RESOLVED;
	t->was_expr = NULL;
	t->kind = TYPE_UNKNOWN; /* default to unknown type (in the case of an error) */
	e->flags |= EXPR_FOUND_TYPE; /* even if failed, pretend we found the type */
	switch (e->kind) {
	case EXPR_FN: {
		if (!type_of_fn(tr, e->fn, &e->type, 0)) {
			return false;
		}
		if (fn_has_any_const_params(e->fn)) {
			HashTable z = {0};
			e->fn->instances = z;
		} else {
			if (!types_fn(tr, e->fn, &e->type, NULL)) {
				return false;
			}
		}
	} break;
	case EXPR_LITERAL_INT:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_I64;
		t->flags |= TYPE_IS_FLEXIBLE;
		break;
	case EXPR_LITERAL_STR:
		t->kind = TYPE_SLICE;
		t->slice = typer_malloc(tr, sizeof *t->slice);
		t->slice->flags = TYPE_IS_RESOLVED;
		t->slice->was_expr = NULL;
		t->slice->kind = TYPE_BUILTIN;
		t->slice->builtin = BUILTIN_CHAR;
		break;
	case EXPR_LITERAL_FLOAT:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_F32;
		t->flags |= TYPE_IS_FLEXIBLE;
		break;
	case EXPR_LITERAL_BOOL:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_BOOL;
		break;
	case EXPR_LITERAL_CHAR:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_CHAR;
		break;
	case EXPR_FOR: {
		ForExpr *fo = e->for_;
		bool in_header = true;
		*(Expression **)typer_arr_add(tr, &tr->in_exprs) = e;
		typer_block_enter(tr, &fo->body); /* while this block is being typed, fo->body will be in tr->blocks twice. hopefully that doesn't mess anything up! */
		if (fo->flags & FOR_IS_RANGE) {
			if (!types_expr(tr, fo->range.from)) goto for_fail;
			{
				Type *ft = &fo->range.from->type;
				if (ft->kind != TYPE_BUILTIN || !type_builtin_is_numerical(ft->builtin)) {
					char *s = type_to_str(ft);
					err_print(e->where, "from expression of for loop must be a builtin numerical type, not %s", s);
					free(s);
					goto for_fail;
				}
			}
			if (fo->range.step) {
				if (!types_expr(tr, fo->range.step)) goto for_fail;
				Type *st = &fo->range.step->type;
				if (st->kind != TYPE_BUILTIN || !type_builtin_is_numerical(st->builtin)) {
					char *s = type_to_str(st);
					err_print(e->where, "step expression of for loop must be a builtin numerical type, not %s", s);
					free(s);
					goto for_fail;
				}
			}
			if (fo->range.to) {
				if (!types_expr(tr, fo->range.to)) goto for_fail;
				Type *tt = &fo->range.to->type;
				if (tt->kind != TYPE_BUILTIN || !type_builtin_is_numerical(tt->builtin)) {
					char *s = type_to_str(tt);
					err_print(e->where, "to expression of for loop must be a builtin numerical type, not %s", s);
					free(s);
				    goto for_fail;
				}
			}

			if (!(fo->flags & FOR_ANNOTATED_TYPE)) {
				fo->type = fo->range.from->type;
			}
			
			if (!type_eq(&fo->type, &fo->range.from->type)) {
				char *exp = type_to_str(&fo->type);
				char *got = type_to_str(&fo->range.from->type);
				err_print(e->where, "Type of for loop does not match the type of the from expression. Expected %s, but got %s.", exp, got);
				free(exp); free(got);
				goto for_fail;
			}
			
			if (fo->range.step && !type_eq(&fo->type, &fo->range.step->type)) {
				char *exp = type_to_str(&fo->type);
				char *got = type_to_str(&fo->range.step->type);
				err_print(e->where, "Type of for loop does not match the type of the step expression. Expected %s, but got %s.", exp, got);
				free(exp); free(got);
				goto for_fail;
			}
			
			if ((fo->type.flags & TYPE_IS_FLEXIBLE) && fo->range.step)
				fo->type = fo->range.step->type;
			
			if (fo->range.to && !type_eq(&fo->type, &fo->range.to->type)) {
				char *exp = type_to_str(&fo->type);
				char *got = type_to_str(&fo->range.to->type);
				err_print(e->where, "Type of for loop does not match the type of the to expression. Expected %s, but got %s.", exp, got);
				free(exp); free(got);
				goto for_fail;
			}
			
			if ((fo->type.flags & TYPE_IS_FLEXIBLE) && fo->range.to)
				fo->type = fo->range.to->type;
			fo->type.flags &= (TypeFlags)~(TypeFlags)TYPE_IS_FLEXIBLE;
		} else {
			if (!types_expr(tr, fo->of))
				goto for_fail;
			Type *iter_type = &fo->of->type;

			bool uses_ptr = false;
			if (iter_type->kind == TYPE_PTR) {
				uses_ptr = true;
				iter_type = iter_type->ptr;
			}
			switch (iter_type->kind) {
			case TYPE_SLICE:
				iter_type = iter_type->slice;
				break;
			case TYPE_ARR:
				iter_type = iter_type->arr.of;
				break;
			default: {
				char *s = type_to_str(&fo->of->type);
				err_print(e->where, "Cannot iterate over non-array non-slice type %s.", s);
				free(s);
				goto for_fail;
			}
			}
			Type ptr_type = {0};
			if (uses_ptr) {
				ptr_type.flags = TYPE_IS_RESOLVED;
				ptr_type.kind = TYPE_PTR;
				ptr_type.ptr = iter_type;
				iter_type = &ptr_type;
			}
			if (fo->flags & FOR_ANNOTATED_TYPE) {
				if (!type_eq(iter_type, &fo->type)) {
					char *exp = type_to_str(iter_type);
					char *got = type_to_str(&fo->type);
					err_print(e->where, "Expected to iterate over type %s, but it was annotated as iterating over type %s.");
					free(exp); free(got);
					goto for_fail;
				}
			} else fo->type = *iter_type;
		}
		if ((fo->flags & FOR_IS_RANGE) && fo->range.step) {
			Value *stepval = typer_malloc(tr, sizeof *fo->range.stepval);
			if (!eval_expr(tr->evalr, fo->range.step, stepval)) {
				info_print(fo->range.step->where, "Note that the step of a for loop must be a compile-time constant.");
				goto for_fail;
			}
			val_cast(stepval, &fo->range.step->type, stepval, &fo->type);
			fo->range.stepval = stepval;
		}
		
		arr_remove_lasta(&tr->in_exprs, tr->allocr);
		in_header = false;
		if (!types_block(tr, &fo->body)) goto for_fail;
		
		if (fo->body.ret_expr) {
			*t = fo->body.ret_expr->type;
		} else {
			t->kind = TYPE_VOID;
			t->flags |= TYPE_IS_RESOLVED;
		}
		typer_block_exit(tr);
		break;
		for_fail:
		if (in_header)
			arr_remove_lasta(&tr->in_exprs, tr->allocr);
		typer_block_exit(tr);
		return false;
	};
	case EXPR_IDENT: {
		if (!type_of_ident(tr, e->where, &e->ident, t)) return false;
	} break;
	case EXPR_CAST: {
		CastExpr *c = &e->cast;
		if (!types_expr(tr, c->expr))
			return false;
		if (!type_resolve(tr, &c->type, e->where))
			return false;
		CastStatus status = type_cast_status(&c->expr->type, &c->type);
		if (status != CAST_STATUS_NONE) {
			char *from = type_to_str(&c->expr->type);
			char *to = type_to_str(&c->type);
			if (status == CAST_STATUS_ERR)

				err_print(e->where, "Cannot cast from type %s to %s.", from, to);
			else
				warn_print(e->where, "Casting from type %s to %s.", from, to);
			free(from);
			free(to);
			if (status == CAST_STATUS_ERR)
				return false;
		}
		*t = c->type;
	} break;
	case EXPR_NEW:
		if (!type_resolve(tr, &e->new.type, e->where))
			return false;
		if (e->new.n) {
			if (!types_expr(tr, e->new.n)) return false;
			if (e->new.n->type.kind != TYPE_BUILTIN || !type_builtin_is_int(e->new.n->type.builtin)) {
				char *got = type_to_str(&e->new.n->type);
				err_print(e->where, "Expected integer as second argument to new, but got %s.", got);
				free(got);
				return false;
			}
			t->kind = TYPE_SLICE;
			t->slice = &e->new.type;
		} else {
			t->kind = TYPE_PTR;
			t->ptr = &e->new.type;
		}
		break;
	case EXPR_IF: {
		IfExpr *i = e->if_;
		IfExpr *curr = i;
		Type *curr_type = t;
		bool has_else = false;
		if (!types_block(tr, &curr->body))
			return false;
		if (curr->body.ret_expr) {
			*t = curr->body.ret_expr->type;
		} else {
			t->kind = TYPE_VOID;
			t->flags |= TYPE_IS_RESOLVED;
		}
		while (1) {
			if (curr->cond) {
				if (!types_expr(tr, curr->cond))
					return false;
				if (!type_can_be_truthy(&curr->cond->type)) {
					char *s = type_to_str(&curr->cond->type);
					err_print(curr->cond->where, "Type %s cannot be the condition of an if statement.", s);
					free(s);
					return false;
				}
			} else {
				has_else = true;
			}
			if (curr->next_elif) {
				IfExpr *nexti = curr->next_elif->if_;
				Type *next_type = &curr->next_elif->type;
				curr->next_elif->flags |= EXPR_FOUND_TYPE;
				if (!types_block(tr, &nexti->body)) {
					return false;
				}
				if (nexti->body.ret_expr) {
					*next_type = nexti->body.ret_expr->type;
				} else {
					next_type->kind = TYPE_VOID;
					next_type->flags = TYPE_IS_RESOLVED;
					next_type->was_expr = NULL;
				}
				if (!type_eq(curr_type, next_type)) {
					char *currstr = type_to_str(curr_type);
					char *nextstr = type_to_str(next_type);
					err_print(curr->next_elif->where, "Mismatched types in if/elif/else chain. Previous block was of type %s, but this one is of type %s.", currstr, nextstr);
					free(currstr);
					free(nextstr);
					return false;
				}
				curr = nexti;
				
			} else {
				break;
			}
		}
		
		if (!has_else && t->kind != TYPE_VOID) {
			err_print(e->where, "Non-void if block with no else.");
			return false;
		}
	} break;
	case EXPR_WHILE: {
		WhileExpr *w = e->while_;
		bool ret = true;
		if (w->cond && !types_expr(tr, w->cond))
			ret = false;
		if (!types_block(tr, &w->body))
			ret = false;
		if (!ret) return false;
		if (w->cond != NULL && w->body.ret_expr != NULL) {
			err_print(e->where, "A finite loop can't have a return expression (for an infinite loop, use while { ... }).");
			return false;
		}
		if (w->body.ret_expr)
			*t = w->body.ret_expr->type;
		else
			t->kind = TYPE_VOID;
	} break;
	case EXPR_CALL: {
		CallExpr *c = &e->call;
		c->instance = NULL;
		Expression *f = c->fn;
		Copier cop = {0};
		FnExpr *fn_decl = NULL;
		if (!types_expr(tr, f)) return false;
		arr_foreach(c->args, Argument, arg) {
			if (!types_expr(tr, &arg->val))
				return false;
		}
		if (f->type.kind == TYPE_UNKNOWN) {
			e->type.kind = TYPE_UNKNOWN;
			return true;
		}
		if (type_is_builtin(&f->type, BUILTIN_TYPE)) {
			/* maybe it's a parameterized type */
		} else if (f->type.kind != TYPE_FN) {
			char *type = type_to_str(&f->type);
			err_print(e->where, "Calling non-function (type %s).", type);
			return false;
		}
		
		if (expr_is_definitely_const(f) || type_is_builtin(&f->type, BUILTIN_TYPE)) {
			Value val;
			
			
			if (!eval_expr(tr->evalr, f, &val))
				return false;
			if (type_is_builtin(&f->type, BUILTIN_TYPE)) {
				Type *base = val.type;
				if (base->kind != TYPE_STRUCT) {
					err_print(e->where, "Cannot pass arguments to non-struct type.");
					return false;
				}
				if (!base->struc->params) {
					err_print(e->where, "Passing arguments to struct, but it doesn't take any.");
					info_print(base->struc->where, "struct was declared here.");
					return false;
				}
			    cop = copier_create(tr->allocr, tr->block);
				HashTable *table = &base->struc->instances;
				StructDef struc;
				copy_struct(&cop, &struc, base->struc);

				size_t nparams = 0;
				arr_foreach(struc.params, Declaration, param)
					nparams += arr_len(param->idents);
				bool already_exists;
				Value args_val = {0};
				Type args_type = {0};
				I16 *order;
				if (!parameterized_struct_arg_order(&struc, c->args, &order, e->where)) {
					free(order);
					return false;
				}
				Type *arg_types = NULL;
				arr_set_len(&arg_types, nparams);
				Value *arg_vals = typer_malloc(tr, nparams * sizeof *arg_vals);
				ErrCtx *err_ctx = tr->err_ctx;
				size_t p = 0;
				arr_foreach(struc.params, Declaration, param) {
					Value param_val = {0};
					bool is_tuple = arr_len(param->idents) > 1;
					int ident_idx = 0;
					/* temporarily add this instance to the stack, while we type the decl, in case you, e.g., pass t = float to struct(t::Type, u::t = "hello") */
					*(Location *)arr_add(&err_ctx->instance_stack) = e->where;
					typer_block_enter(tr, &struc.scope);
				    bool success = types_decl(tr, param);
					arr_remove_last(&err_ctx->instance_stack);
					typer_block_exit(tr);
					if (!success) return false;
					
					arr_foreach(param->idents, Identifier, ident) {
						Type *type = decl_type_at_index(param, ident_idx);
						arg_types[p] = *type;
						Value ident_val;
						if (order[p] == -1) {
						    ident_val = *decl_val_at_index(param, ident_idx);
						} else {
							Argument *arg = &c->args[order[p]];
							assert(arg->val.type.flags & TYPE_IS_RESOLVED);
							assert(type->flags & TYPE_IS_RESOLVED);
							if (!type_eq(&arg->val.type, type)) {
								char *expected = type_to_str(type),
									*got = type_to_str(&arg->val.type);
								err_print(arg->where, "Wrong struct parameter type. Expected %s, but got %s.", expected, got);
								return false;
							}
							if (!eval_expr(tr->evalr, &arg->val, &ident_val))
								return false;
						}
						if (is_tuple)
							*(Value *)arr_adda(&param_val.tuple, tr->allocr) = ident_val;
						else
							param_val = ident_val;
						arg_vals[p] = ident_val;
						++p;
						++ident_idx;
					}
					param->val = param_val;
					param->flags |= DECL_FOUND_VAL;
				}
				free(order);
				args_val.tuple = arg_vals;
				args_type.tuple = arg_types;
				args_type.kind = TYPE_TUPLE;
				args_type.flags = TYPE_IS_RESOLVED;
				Instance *inst = instance_table_adda(tr->allocr, table, args_val, &args_type, &already_exists);
				if (!already_exists) {
					inst->struc = struc;
					size_t i = 0;
					arr_foreach(inst->struc.params, Declaration, param) {
					    param->flags |= DECL_FOUND_VAL;
						if (arr_len(param->idents) == 1) {
							param->val = arg_vals[i];
							++i;
						} else {

							size_t nmembers = arr_len(param->idents);
							param->val.tuple = typer_malloc(tr, nmembers * sizeof *param->val.tuple);
							for (size_t idx = 0; idx < nmembers; ++idx) {
								param->val.tuple[idx] = arg_vals[i];
								++i;
							}
						}
					}
					assert(i == nparams);
					Type struct_t = {0};
					struct_t.kind = TYPE_STRUCT;
					struct_t.struc = &inst->struc;
					*(Location *)arr_add(&err_ctx->instance_stack) = e->where;
					bool success = type_resolve(tr, &struct_t, e->where); /* resolve the struct */
				    arr_remove_last(&err_ctx->instance_stack);
					if (!success) return false;
						
					inst->struc.instance_id = table->n;
				}

				
				/* expression is actually a type */
				e->kind = EXPR_TYPE;
				memset(&e->typeval, 0, sizeof e->typeval);
				e->typeval.kind = TYPE_STRUCT;
				e->typeval.flags = TYPE_IS_RESOLVED;
				e->typeval.struc = &inst->struc;
			    t->kind = TYPE_BUILTIN;
				t->builtin = BUILTIN_TYPE;
				arr_clear(&arg_types);
				goto ret;
			}
			fn_decl = val.fn;
			
		}
		
		Type *ret_type = f->type.fn.types;
		Type *param_types = ret_type + 1;
		Argument *args = c->args;
		size_t nparams = arr_len(f->type.fn.types) - 1;
		size_t nargs = arr_len(c->args);
		Expression *arg_exprs = NULL;
		arr_set_lena(&arg_exprs, nparams, tr->allocr);

		I16 *order = NULL;
		if (fn_decl && !(fn_decl->flags & FN_EXPR_FOREIGN)) {
			if (!call_arg_param_order(fn_decl, &f->type, c->args, e->where, &order)) {
				free(order);
				return false;
			}
			size_t i = 0;
			arr_foreach(fn_decl->params, Declaration, param) {
				arr_foreach(param->idents, Identifier, ident) { 
					I16 arg_idx = order[i];
					if (arg_idx == -1) {
						if (param->flags & DECL_HAS_EXPR) {
							assert(param->expr.kind == EXPR_VAL); /* evaluated in type_of_fn */
							arg_exprs[i].kind = EXPR_VAL;
							arg_exprs[i].flags = param->expr.flags;
							arg_exprs[i].type = param->type;
							arg_exprs[i].val = param->expr.val;
						}
						/* else, it's inferred */
					} else {
						arg_exprs[i] = args[arg_idx].val;
					}
					++i;
				}
			}
		} else {
			if (nargs != nparams) {
				err_print(e->where, "Expected %lu arguments to function call, but got %lu.", (unsigned long)nparams, (unsigned long)nargs);
				return false;
			}
			for (size_t p = 0; p < nargs; ++p) {
				if (args[p].name) {
					err_print(args[p].where, "You can only use named arguments if you directly call a function.");
				}
				arg_exprs[p] = args[p].val;
			}
		}

		FnType *fn_type = &f->type.fn;
		c->arg_exprs = arg_exprs;
		FnExpr *original_fn = NULL;
		Type table_index_type = {0};
		Value table_index = {0};
		FnExpr *fn_copy = NULL;
		cop = copier_create(tr->allocr, tr->block);
		if (fn_type->constness) {
			/* evaluate compile-time arguments + add an instance */
			
			
			/* the function had better be a compile time constant if it has constant params */
			Value fn_val = {0};
			if (!eval_expr(tr->evalr, f, &fn_val))
				return false;

			FnExpr *fn = fn_val.fn;
			/* fn is the instance, original_fn is not */
			original_fn = fn;
			fn_copy = typer_malloc(tr, sizeof *fn_copy);
			copy_fn_expr(&cop, fn_copy, fn, true);
			fn = fn_copy;
			/* keep track of the declaration */
			Declaration *param_decl = fn->params;
			size_t ident_idx = 0;
			size_t i = 0;

			Type **arg_types = NULL;
			Type **decl_types = NULL;
			Identifier *inferred_idents = NULL;

			arr_foreach(fn->params, Declaration, param) {
				arr_foreach(param->idents, Identifier, ident) {
					if (param->flags & DECL_INFER) {
						*(Identifier *)typer_arr_add(tr, &inferred_idents) = *ident;
					} else if ((param->flags & DECL_ANNOTATES_TYPE)
							   && !(param->flags & DECL_HAS_EXPR)) {
						
						if (param->type.kind == TYPE_TUPLE)
							err_print(param->where, "Parameters cannot have tuple types.");
						
						Type **p = typer_arr_add(tr, &decl_types);
						*p = &param->type;
						Type **q = typer_arr_add(tr, &arg_types);
						*q = &arg_exprs[i].type;
					}
					++i;
				}
			}

			size_t ninferred_idents = arr_len(inferred_idents);
			if (ninferred_idents) {
				Value *inferred_vals = err_malloc(ninferred_idents * sizeof *inferred_vals);
				Type *inferred_types = err_malloc(ninferred_idents * sizeof *inferred_types);
				
				if (!infer_ident_vals(tr, decl_types, arg_types, inferred_idents, inferred_vals, inferred_types))
					return false;

				arr_cleara(&inferred_idents, tr->allocr);
				
				{
					Type *type = inferred_types;
					for (i = 0; i < ninferred_idents; ++i) {
						if (type->kind == TYPE_UNKNOWN) {
							long counter = (long)i;
							Declaration *decl = fn->params;
							while (1) {
								counter -= (long)arr_len(decl->idents);
								if (counter < 0) break;
								++decl;
							}
							err_print(decl->where, "Could not infer value of declaration.");
							info_print(e->where, "While processing this call");
							return false;
						}
						++type;
					}
				}
				i = 0;
				arr_foreach(fn->params, Declaration, param) {
					if (param->flags & DECL_INFER) {
						Value *val = &inferred_vals[i];
						Type *type = &inferred_types[i];
						/* if we have an inferred type argument, it shouldn't be flexible */
						if (type_is_builtin(type, BUILTIN_TYPE))
							val->type->flags &= (TypeFlags)~(TypeFlags)TYPE_IS_FLEXIBLE;
						param->val = *val;
						param->type = *type;
						param->flags |= DECL_FOUND_VAL | DECL_FOUND_TYPE;
						++i;
					}
				}
				free(inferred_vals);
				free(inferred_types);
			}
			


			table_index_type.flags = TYPE_IS_RESOLVED;
			table_index_type.kind = TYPE_TUPLE;
			table_index_type.tuple = NULL;
			Type *u64t = typer_arr_add(tr, &table_index_type.tuple);
			u64t->was_expr = NULL;
			u64t->flags = TYPE_IS_RESOLVED;
			u64t->kind = TYPE_BUILTIN;
			u64t->builtin = BUILTIN_U64;
			table_index.tuple = NULL;
			/* we need to keep table_index's memory around because instance_table_add makes a copy of it to compare against. */
			Value *which_are_const_val = typer_arr_add(tr, &table_index.tuple);
			U64 *which_are_const = &which_are_const_val->u64;
			*which_are_const = 0;
			int semi_const_index = 0;
			/* eval compile time arguments */
			for (i = 0; i < nparams; ++i) {
				bool should_be_evald = arg_is_const(&arg_exprs[i], fn_type->constness[i]);
				if (should_be_evald) {
					if (!order || order[i] != -1) {
						Expression *expr = &arg_exprs[i];
						Value *arg_val = typer_arr_add(tr, &table_index.tuple);
						if (!eval_expr(tr->evalr, expr, arg_val)) {
							if (tr->evalr->enabled) {
								info_print(arg_exprs[i].where, "(error occured while trying to evaluate compile-time argument, argument #%lu)", 1+(unsigned long)i);
							}
							return false;
						}
					
						Type *type = &expr->type;
						*(Type *)typer_arr_add(tr, &table_index_type.tuple) = *type;
				
						arg_exprs[i].kind = EXPR_VAL;
						arg_exprs[i].flags = EXPR_FOUND_TYPE;
						copy_val(tr->allocr, &arg_exprs[i].val, arg_val, type);
						arg_exprs[i].val = *arg_val;
						copy_val(tr->allocr, &param_decl->val, arg_val, type);
						param_decl->flags |= DECL_FOUND_VAL;
						if (!(param_decl->flags & DECL_ANNOTATES_TYPE)) {
							param_decl->type = *type;
						}
					} else {
						/* leave gap for this (default argument) */
						typer_arr_add(tr, &table_index.tuple);
						typer_arr_add(tr, &table_index_type.tuple);
					}
				}
				
				if (fn_type->constness[i] == CONSTNESS_SEMI) {
					if (semi_const_index >= 64) {
						err_print(f->where, "You can't have more than 64 semi-constant arguments to a function at the moment (sorry).");
						return false;
					}
					*which_are_const |= ((U64)1) << semi_const_index;
				}
				if (fn_type->constness[i] == CONSTNESS_SEMI) {
					++semi_const_index;
				}
				++ident_idx;
				if (ident_idx >= arr_len(param_decl->idents)) {
					ident_idx = 0;
					++param_decl;
				}
			}
			/* type params, return declarations, etc */
			if (!type_of_fn(tr, fn_copy, &f->type, TYPE_OF_FN_IS_INSTANCE))
				return false;
			
			/* deal with default arguments */
			i = 0;
			arr_foreach(fn->params, Declaration, param) {
				arr_foreach(param->idents, Identifier, ident) {
					if (order && order[i] == -1) {
						if (param->flags & DECL_INFER) {
							arg_exprs[i].kind = EXPR_VAL;
							arg_exprs[i].flags = EXPR_FOUND_TYPE;
							arg_exprs[i].type = table_index_type.tuple[i+1] = param_types[i] = param->type;
							arg_exprs[i].val = table_index.tuple[i+1] = param->val;
							++i;
							continue;
						}
						assert(param->flags & DECL_HAS_EXPR);
						assert(param->expr.kind == EXPR_VAL); /* this was done by type_of_fn */
						arg_exprs[i] = param->expr;
						/* make sure value is copied */
						copy_val(tr->allocr, &arg_exprs[i].val, &param->expr.val, &param->expr.type);
						Value *arg_val = &table_index.tuple[i+1];
						copy_val(tr->allocr, arg_val, &param->expr.val, &param->expr.type);
						table_index_type.tuple[i+1] = param->expr.type;
					}
					++i;
				}
				
			}
			
			ret_type = f->type.fn.types;
			param_types = ret_type + 1;
		}
		
		/* check types of arguments */
		for (size_t p = 0; p < nparams; ++p) {
			Expression *arg = &arg_exprs[p];
			Type *expected = &param_types[p];
			Type *got = &arg->type;
			if (!type_eq(expected, got)) {
				char *estr = type_to_str(expected);
				char *gstr = type_to_str(got);
				err_print(arg->where, "Expected type %s as argument to function, but got %s.", estr, gstr);
				return false;
			}
			if (got->flags & TYPE_IS_FLEXIBLE) {
				/* "cast" */
				*got = *expected;
			}
		}
		
		if (fn_type->constness) {
			bool instance_already_exists;
			c->instance = instance_table_adda(tr->allocr, &original_fn->instances, table_index, &table_index_type, &instance_already_exists);
			if (instance_already_exists) {
				arr_cleara(&table_index_type.tuple, tr->allocr);
				arr_cleara(&table_index.tuple, tr->allocr);
			} else {
				c->instance->fn = fn_copy;
				/* fix parameter and return types (they were kind of problematic before, because we didn't know about the instance) */
				c->instance->c.id = original_fn->instances.n; /* let's help cgen out and assign an ID to this */
				/* type this instance */
				
				/* if anything happens, make sure we let the user know that this happened while generating a fn */
				ErrCtx *err_ctx = e->where.file->ctx;
				*(Location *)typer_arr_add(tr, &err_ctx->instance_stack) = e->where;
				bool success = types_fn(tr, c->instance->fn, &f->type, c->instance);
				arr_remove_lasta(&err_ctx->instance_stack, tr->allocr);
				if (!success) return false;
				arr_cleara(&table_index_type.tuple, tr->allocr);
			}
		}
		free(order);
		*t = *ret_type;
	} break;
	case EXPR_BLOCK: {
		Block *b = e->block;
		if (!types_block(tr, b))
			return false;
		if (b->ret_expr) {
			*t = b->ret_expr->type;
		} else {
			t->kind = TYPE_VOID;
		}
	} break;
	case EXPR_C: {
		Expression *code = e->c.code;
		if (!types_expr(tr, code))
			return false;
		if (!type_is_slicechar(&code->type)) {
			char *s = type_to_str(&code->type);
			err_print(e->where, "Argument to #C directive must of type []char, but got type %s.");
			free(s);
			return false;
		}
		Value code_val;
		if (!eval_expr(tr->evalr, code, &code_val))
			return false;
		code->val = code_val;
		code->kind = EXPR_VAL;
		t->kind = TYPE_UNKNOWN;
	} break;
	case EXPR_BUILTIN: {
		char *builtin_name = eval_expr_as_cstr(tr, e->builtin.which.expr, "#builtin value name");
		if (!builtin_name) return false;
		int which = -1;
		for (BuiltinVal b = 0; b < BUILTIN_VAL_COUNT; b = b + 1) {
			if (strs_equal(builtin_val_names[b], builtin_name)) {
				which = (int)b;
			}
		}
		if (which == -1) {
			err_print(e->where, "Unrecognized builtin value: %s.", builtin_name);
			return false;
		}
		e->builtin.which.val = (BuiltinVal)which;
		get_builtin_val_type(tr->allocr, e->builtin.which.val, t);
		assert(t->flags & TYPE_IS_RESOLVED);
	} break;
	case EXPR_UNARY_OP: {
		Expression *of = e->unary.of;
		Type *of_type = &of->type;
		if (!types_expr(tr, e->unary.of)) return false;
		if (of_type->kind == TYPE_UNKNOWN) {
			return true;
		}
		switch (e->unary.op) {
		case UNARY_MINUS:
			if (of_type->kind != TYPE_BUILTIN || !type_builtin_is_numerical(of_type->builtin)) {
				char *s = type_to_str(of_type);
				err_print(e->where, "Cannot apply unary - to non-numerical type %s.", s);
				free(s);
				return false;
			}
			if (!type_builtin_is_signed(of_type->builtin)) {
				char *s = type_to_str(of_type);
				warn_print(e->where, "Applying unary - to unsigned type %s may cause overflow.", s);
				free(s);
			}
			*t = *of_type;
			break;
		case UNARY_ADDRESS:
			if (type_is_builtin(of_type, BUILTIN_TYPE)) {
				/* oh it's a type! */
				t->kind = TYPE_BUILTIN;
				t->builtin = BUILTIN_TYPE;
				break;
			}
			if (!expr_must_lval(of)) {
				return false;
			}
			if (of_type->kind == TYPE_TUPLE) {
				err_print(e->where, "Cannot take address of tuple.");
				return false;
			}
			t->kind = TYPE_PTR;
			t->ptr = typer_malloc(tr, sizeof *t->ptr);
			*t->ptr = *of_type;
			break;
		case UNARY_DEREF:
			if (of_type->kind != TYPE_PTR) {
				char *s = type_to_str(of_type);
				err_print(e->where, "Cannot dereference non-pointer type %s.", s);
				free(s);
				return false;
			}
			
			*t = *of_type->ptr;
			break;
		case UNARY_DEL:
			if (of_type->kind != TYPE_PTR && of_type->kind != TYPE_SLICE) {
				char *s = type_to_str(of_type);
				err_print(e->where, "Cannot delete non-pointer, non-slice type %s.", s);
				free(s);
				return false;
			}
			t->kind = TYPE_VOID;
			break;
		case UNARY_NOT:
			if (!type_can_be_truthy(of_type)) {
				char *s = type_to_str(of_type);
				err_print(e->where, "Type '%s' cannot be truthy, so the not operator cannot be applied to it.", s);
				free(s);
				return false;
			}
			t->kind = TYPE_BUILTIN;
			t->builtin = BUILTIN_BOOL;
			break;
		case UNARY_LEN:
			t->kind = TYPE_BUILTIN;
			t->builtin = BUILTIN_I64;
			if (of_type->kind != TYPE_SLICE || of_type->kind != TYPE_ARR) {
				char *s = type_to_str(of_type);
				err_print(e->where, "Cannot get length of non-array, non-slice type %s.", s);
				free(s);
				return false;
			}
			break;
		case UNARY_DSIZEOF:
		case UNARY_DALIGNOF: {
			if (!types_expr(tr, of))
				return false;
			Type *queried_type;
			if (type_is_builtin(&of->type, BUILTIN_TYPE)) {
				Value val;
				if (!eval_expr(tr->evalr, of, &val))
					return false;
				queried_type = val.type;
			} else {
				queried_type = &of->type;
			}
			if (e->unary.op == UNARY_DSIZEOF)
				e->val.i64 = (I64)compiler_sizeof(queried_type);
			else
				e->val.i64 = (I64)compiler_alignof(queried_type);
			e->kind = EXPR_VAL;
			t->kind = TYPE_BUILTIN;
			t->builtin = BUILTIN_I64;
		} break;
		} 
	} break;
	case EXPR_BINARY_OP: {
		Expression *lhs = e->binary.lhs;
		Expression *rhs = e->binary.rhs;
		Type *lhs_type = &lhs->type;
		Type *rhs_type = &rhs->type;
			   
		BinaryOp o = e->binary.op;
		if (o != BINARY_DOT) {
			if (!types_expr(tr, lhs)
				|| !types_expr(tr, rhs))
				return false;
			if (lhs_type->kind == TYPE_UNKNOWN || rhs_type->kind == TYPE_UNKNOWN) {
				return true;
			}
		}
		switch (o) {
		case BINARY_SET:
		case BINARY_SET_ADD:
		case BINARY_SET_SUB:
		case BINARY_SET_MUL:
		case BINARY_SET_DIV:
		case BINARY_SET_MOD:
			if (!expr_must_lval(e->binary.lhs)) {
				return false;
			}
			/* fallthrough */
		case BINARY_ADD:
		case BINARY_SUB:
		case BINARY_MUL:
		case BINARY_DIV:
		case BINARY_MOD:
		case BINARY_LT:
		case BINARY_GT:
		case BINARY_LE:
		case BINARY_GE:
		case BINARY_EQ:
		case BINARY_NE: {
			bool valid = false;
			assert(lhs_type->flags & TYPE_IS_RESOLVED);
			assert(rhs_type->flags & TYPE_IS_RESOLVED);
			
			
			if (o == BINARY_SET) {
				valid = type_eq(lhs_type, rhs_type);
			} else {
				/* numerical binary ops */
				if (lhs_type->kind == TYPE_BUILTIN && type_eq(lhs_type, rhs_type)) {
					/* int + int, etc. */
					valid = true;
				}
				if (o == BINARY_ADD || o == BINARY_SUB || o == BINARY_SET_ADD || o == BINARY_SET_SUB) {
					if (lhs_type->kind == TYPE_PTR &&
						rhs_type->kind == TYPE_BUILTIN &&
						type_builtin_is_numerical(rhs_type->builtin)) {
						valid = true;
					}
				}
				if (o == BINARY_LT || o == BINARY_GT || o == BINARY_LE || o == BINARY_GE
					|| o == BINARY_EQ || o == BINARY_NE) {
					/* comparable types */
					if (type_eq(lhs_type, rhs_type)) {
						switch (lhs_type->kind) {
						case TYPE_PTR:
						case TYPE_BUILTIN: /* all builtins are comparable */
							valid = true;
						default:
							break;
						}
					}
				}
			}
			if (valid) {
				switch (o) {
				case BINARY_SET:
					/* type of x = y is always void */
					t->kind = TYPE_VOID;
					break;
				case BINARY_LT:
				case BINARY_GT:
				case BINARY_LE:
				case BINARY_GE:
				case BINARY_EQ:
				case BINARY_NE:
					t->kind = TYPE_BUILTIN;
					t->builtin = BUILTIN_BOOL;
					break;
				default: {
					int lhs_is_flexible = lhs_type->flags & TYPE_IS_FLEXIBLE;
					int rhs_is_flexible = rhs_type->flags & TYPE_IS_FLEXIBLE;
					if (lhs_is_flexible && rhs_is_flexible) {
						/* both flexible */
						*t = *lhs_type;
						if (rhs_type->builtin == BUILTIN_F32) {
							/* promote to float */
							t->builtin = BUILTIN_F32;
						}
						
					} else if (!lhs_is_flexible) {
						/* lhs inflexible, rhs ? */
						*t = *lhs_type;
					} else {
						/* lhs flexible, rhs ? */
						*t = *rhs_type;
					}
					if ((o == BINARY_MOD || o == BINARY_SET_MOD)
						&& type_builtin_is_float(t->builtin)) {
						err_print(e->where, "Cannot use operator % on floating-point numbers.");
						valid = false;
					}
				} break;
				}
			}
			if (!valid) {
				char *s1, *s2;
				s1 = type_to_str(lhs_type);
				s2 = type_to_str(rhs_type);
				const char *op = binary_op_to_str(o);
				err_print(e->where, "Invalid types to operator %s: %s and %s", op, s1, s2);
				return false;
			}
			if (o == BINARY_SET_ADD ||
				o == BINARY_SET_SUB ||
				o == BINARY_SET_MUL ||
				o == BINARY_SET_DIV) {
				t->kind = TYPE_VOID; /* actually, it's just void */
			}
				
			break;
		}
		case BINARY_AT_INDEX:
			if ((lhs_type->kind == TYPE_ARR || lhs_type->kind == TYPE_SLICE) &&
				(rhs_type->kind != TYPE_BUILTIN || !type_builtin_is_numerical(rhs_type->builtin))) {
				err_print(e->where, "The index of an array must be a builtin numerical type.");
				return false;
			}
			if (lhs_type->kind == TYPE_PTR) {
				if (lhs_type->ptr->kind == TYPE_STRUCT
					|| type_is_builtin(lhs_type->ptr, BUILTIN_NMS)) {
					lhs_type = lhs_type->ptr;
				}
			}
			switch (lhs_type->kind) {
			case TYPE_ARR:
				*t = *lhs_type->arr.of;
				break;
			case TYPE_SLICE:
				*t = *lhs_type->slice;
				break;
			case TYPE_STRUCT: {
				/* allow accessing struct members with a string */
				if (!type_is_slicechar(rhs_type)) {
					char *s = type_to_str(rhs_type);
					err_print(e->where, "Expected a string for struct member access with [], but got type %s.", s);
					return false;
				}
				Value field_name;
				
				/* replace with BINARY_DOT */
				e->binary.op = BINARY_DOT;
				bool is_field = false;
				if (!eval_expr(tr->evalr, rhs, &field_name)) return false;
				arr_foreach(lhs_type->struc->fields, Field, f) {
					if (ident_eq_str(f->name, field_name.slice.data)) {
						is_field = true;
						*t = f->type;
						e->binary.dot.field = f;
					}
				}
				if (!is_field) {
					char *fstr = err_malloc((size_t)(field_name.slice.n + 1));
					memcpy(fstr, field_name.slice.data, (size_t)field_name.slice.n);
					fstr[field_name.slice.n] = 0; /* null-terminate */
					char *typestr = type_to_str(lhs_type);
					err_print(e->where, "%s is not a field of structure %s.", fstr, typestr);
					free(fstr); free(typestr);
					return false;
				}
			} break;
			case TYPE_BUILTIN:
				if (lhs_type->builtin == BUILTIN_NMS) {
					if (!type_is_slicechar(rhs_type)) 
					break;
				}
				/* fallthrough */
			default: {
				/* allow accessing namespace members with a string */
				if (!type_is_slicechar(rhs_type)) {
					char *s = type_to_str(rhs_type);
					err_print(e->where, "Expected a string for namsepace member access with [], but got type %s.", s);
					return false;
				}

				Value nms_val;
				if (!eval_expr(tr->evalr, lhs, &nms_val))
					return false;
				Namespace *nms = nms_val.nms;
				lhs->kind = EXPR_VAL;
				lhs->val.nms = nms;
				
				Value member_name;
				if (!eval_expr(tr->evalr, rhs, &member_name)) return false;
				e->binary.op = BINARY_DOT;
				e->binary.rhs->kind = EXPR_IDENT;
				Identifier ident = e->binary.rhs->ident = e->binary.dot.translated_ident =
					ident_get_with_len(&nms->body.idents, member_name.slice.data, (size_t)member_name.slice.n);
				if (!type_of_ident(tr, rhs->where, &ident, t)) {
					return false;
				}
			} break;
			}
			break;
		case BINARY_DOT: {
			if (!types_expr(tr, lhs)) return false;
			Type *struct_type = lhs_type;
			if (struct_type->kind == TYPE_UNKNOWN) return true;
			if (struct_type->kind == TYPE_PTR)
				struct_type = struct_type->ptr;
			if (rhs->kind != EXPR_IDENT) {
				err_print(rhs->where, "Expected identifier for struct member access, but got %s.",
						  expr_kind_to_str(rhs->kind));
				return false;
			}
			if (struct_type->kind == TYPE_STRUCT) {
				bool is_field = false;
				arr_foreach(struct_type->struc->fields, Field, f) {
					if (ident_eq(f->name, rhs->ident)) {
						is_field = true;
						*t = f->type;
						e->binary.dot.field = f;
					}
				}
				if (!is_field) {
#if 0
					Declaration *param = NULL;
				    int ident_idx;
					arr_foreach(struct_type->struc->params, Declaration, p) {
						ident_idx = 0;
						arr_foreach(p->idents, Identifier, ident) {
							if (ident_eq(*ident, rhs->ident)) {
								param = p;
								goto dblbreak_dot;
							}	
							++ident_idx;
						}
					}
				dblbreak_dot:
					if (!param) {
						char *member = ident_to_str(rhs->ident);
						char *struc = type_to_str(struct_type);
						err_print(e->where, "%s is not a member of structure %s.", member, struc);
						return false;
					}
#endif
					Identifier i = ident_translate(rhs->ident, &struct_type->struc->scope.idents);
					if (!i || i->decl_kind == IDECL_NONE) {
						char *member = ident_to_str(rhs->ident);
						char *struc_s = type_to_str(struct_type);
						err_print(e->where, "%s is not a member of structure %s.", member, struc_s);
						free(member);
						free(struc_s);
						return false;
					}
					assert((i->decl_kind == IDECL_DECL) && (i->decl->flags & DECL_IS_CONST));
					/* replace with decl value */
					int ident_idx = decl_ident_index(i->decl, i);
					e->kind = EXPR_VAL;
					e->val = *decl_val_at_index(i->decl, ident_idx);
					*t = *decl_type_at_index(i->decl, ident_idx);
					break;
				}
			} else if (struct_type->kind == TYPE_SLICE || struct_type->kind == TYPE_ARR) {
				if (!ident_eq_str(rhs->ident, "len")) {
					err_print(rhs->where, "Field of array or slice must be .len");
					return false;
				}
				/* length of slice/arr is i64 */
				t->kind = TYPE_BUILTIN;
				t->builtin = BUILTIN_I64;
				/* change expr to UNARY_LEN */
				e->kind = EXPR_UNARY_OP;
				Expression *of = lhs;
				e->unary.op = UNARY_LEN;
				e->unary.of = of;
			} else if (type_is_builtin(struct_type, BUILTIN_NMS)) {
				Value nms_val;
				if (!eval_expr(tr->evalr, lhs, &nms_val))
					return false;
				Namespace *nms = nms_val.nms;
				lhs->kind = EXPR_VAL;
				lhs->val.nms = nms;
				Identifier translated = ident_translate(rhs->ident, &nms->body.idents);
				if (!translated) {
					char *s = ident_to_str(rhs->ident);
					err_print(rhs->where, "%s is not a member of this namespace.", s);
					return false;
				}
				assert(translated->decl_kind != IDECL_NONE);
				if (!type_of_ident(tr, rhs->where, &translated, t)) {
					return false;
				}
				e->binary.dot.translated_ident = translated;
			} else {
				char *s = type_to_str(lhs_type);
				err_print(e->where, "Operator . applied to type %s, which is not a structure or pointer to structure.", s);
				free(s);
				return false;
			}
		} break;
		} break;
	} break;
	case EXPR_TUPLE:
		t->kind = TYPE_TUPLE;
		t->tuple = NULL;
		arr_foreach(e->tuple, Expression, x) {
			Type *x_type = typer_arr_add(tr, &t->tuple);
			if (!types_expr(tr, x))
				return false;
			*x_type = x->type;
		}
		break;
	case EXPR_SLICE: {
		t->kind = TYPE_SLICE;
		SliceExpr *s = &e->slice;
		if (!types_expr(tr, s->of))
			return false;
		if (e->slice.from && !types_expr(tr, s->from))
			return false;
		if (e->slice.to && !types_expr(tr, s->to))
			return false;
		switch (s->of->type.kind) {
		case TYPE_ARR:
			t->slice = s->of->type.arr.of;
			break;
		case TYPE_SLICE:
			t->slice = s->of->type.slice;
			break;
		default: {
			char *str = type_to_str(&s->of->type);
			err_print(e->where, "Cannot take slice of non-array, non-slice type %s.", str);
			free(str);
			return false;
		}
		}
		break;
	}
	case EXPR_TYPE: {
		Type *tval = &e->typeval;
		if (tval->kind == TYPE_STRUCT && tval->struc->params) {
			/* don't try to resolve this */
			t->kind = TYPE_BUILTIN;
			t->builtin = BUILTIN_TYPE;
			break;
		}
		if (!type_resolve(tr, tval, e->where))
			return false;
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_TYPE;
	} break;
	case EXPR_NMS: {
		Namespace *prev_nms = tr->nms;
		Namespace *n = tr->nms = e->nms;
	    n->points_to = NULL;
		n->body.flags |= BLOCK_IS_NMS;
		if (!types_block(tr, &n->body)) {
			tr->nms = prev_nms;
			return false;
		}
		tr->nms = prev_nms;
	    n->associated_ident = NULL; /* set when we type the declaration which contains this namespace */
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_NMS;
	} break;
	case EXPR_VAL:
		assert(0);
		return false;
	}
 ret:
	t->flags |= TYPE_IS_RESOLVED;
	return true;
}


static Status types_block(Typer *tr, Block *b) {
	if (b->flags & BLOCK_FOUND_TYPES)
		return true;

	if (b->flags & BLOCK_FINDING_TYPES) {
		err_print(b->where, "A circular dependency was found when finding types in this block.\n"
				  "You are using recursion in a way that is not allowed by this language. Sorry!");
		return false;
	}
	b->flags |= BLOCK_FINDING_TYPES;
	
	typer_block_enter(tr, b);
	bool success = true;
	arr_foreach(b->stmts, Statement, s) {
		if (!types_stmt(tr, s)) {
			success = false;
			continue;
		}
		if (s->kind == STMT_EXPR && (s->flags & STMT_EXPR_NO_SEMICOLON)) {
			/* not voided */
			Expression *e = &s->expr;
			if (e->type.kind == TYPE_VOID) {
				if (!(e->kind == EXPR_BLOCK
					  || e->kind == EXPR_IF
					  || e->kind == EXPR_WHILE
					  || e->kind == EXPR_FOR)) {
					err_print(e->where, "void expression must be followed by ;");
					success = false;
					goto ret;
				}
			} else {
				if (s != (Statement *)arr_last(b->stmts)) {
					err_print(e->where, "Return value must be the last statement in a block.");
					success = false;
					goto ret;
				}
				b->ret_expr = typer_malloc(tr, sizeof *b->ret_expr);
				*b->ret_expr = *e;
				arr_remove_lasta(&b->stmts, tr->allocr);
			}
		}
		
	}
 ret:
	typer_block_exit(tr);
	b->flags |= BLOCK_FOUND_TYPES;
	b->flags &= (BlockFlags)~(BlockFlags)BLOCK_FINDING_TYPES;
	
	return success;
}

static Status types_decl(Typer *tr, Declaration *d) {
	if (d->flags & DECL_FOUND_TYPE) return true;
	bool success = true;

	if ((d->flags & DECL_HAS_EXPR)
		&& d->expr.kind == EXPR_TYPE
		&& d->expr.typeval.kind == TYPE_STRUCT) {
		d->expr.typeval.struc->name = d->idents[0];
	}
	
	if (d->flags & DECL_INFER) {
		d->type.kind = TYPE_UNKNOWN;
		d->type.flags = 0;
		return true;
	}
	Declaration **dptr = typer_arr_add(tr, &tr->in_decls);
	*dptr = d;
	if (d->flags & DECL_ANNOTATES_TYPE) {
		/* type supplied */
		assert(d->type.kind != TYPE_VOID); /* there's no way to annotate void */
		if (!type_resolve(tr, &d->type, d->where)) {
			success = false;
			goto ret;
		}
	}
	if (d->flags & DECL_HAS_EXPR) {
		if (!types_expr(tr, &d->expr)) {
			success = false;
			goto ret;
		}
		assert(d->expr.type.flags & TYPE_IS_RESOLVED);
		if (d->flags & DECL_ANNOTATES_TYPE) {
			if (!type_must_eq(d->expr.where, &d->type, &d->expr.type)) {
				success = false;
				goto ret;
			}
			d->expr.type = d->type;
		} else {
			if (d->expr.type.kind == TYPE_VOID) {
				/* e.g. x := (fn(){})(); */
				err_print(d->expr.where, "Use of void value.");
				success = false;
				goto ret;
			}
			d->type = d->expr.type;
			d->type.flags &= (TypeFlags)~(TypeFlags)TYPE_IS_FLEXIBLE; /* x := 5; => x is not flexible */
		}
		bool need_value = (d->flags & DECL_IS_CONST) ||
			((tr->block == NULL || (tr->block->flags & BLOCK_IS_NMS)) && tr->fn == NULL);
		
		if (need_value) {
			if (!(d->flags & DECL_FOUND_VAL)) {
				Value val;
				if (!eval_expr(tr->evalr, &d->expr, &val)) {
					success = false;
					goto ret;
				}
				copy_val(tr->allocr, &d->val, &val, &d->type);
				d->flags |= DECL_FOUND_VAL;
			}
		}
				
	}
	
	for (size_t i = 0; i < arr_len(d->idents); ++i) {
		Type *t = d->type.kind == TYPE_TUPLE ? &d->type.tuple[i] : &d->type;
		if (type_is_compileonly(&d->type)) {
			if (!(d->flags & DECL_IS_CONST)) {
				char *s = type_to_str(&d->type);
				err_print(d->where, "Declarations with type %s must be constant.", s);
				free(s);
				success = false;
				goto ret;
			}
		}
		if (type_is_builtin(t, BUILTIN_TYPE)) {
			if (d->flags & DECL_HAS_EXPR) {
				Value *val = d->type.kind == TYPE_TUPLE ? &d->val.tuple[i] : &d->val;
				if (val->type->kind == TYPE_STRUCT && val->type->struc->params) {
					/* don't resolve it because it's not really complete */
				} else {
					if (!type_resolve(tr, val->type, d->where)) return false;
					if (val->type->kind == TYPE_TUPLE) {
						err_print(d->where, "You can't declare a new type to be a tuple.");
						success = false;
						goto ret;
					}
				}
			}
		} else if (!(d->flags & DECL_IS_CONST) && t->kind == TYPE_FN && t->fn.constness) {
			for (size_t p = 0; p < arr_len(t->fn.types)-1; ++p) {
				if (t->fn.constness[p] == CONSTNESS_YES) {
					err_print(d->where, "You can't have a pointer to a function with constant parameters.");
					success = false;
					goto ret;
				}
			}
			/* make constness NULL, so that semi-constant parameters turn into non-constant arguments */
			t->fn.constness = NULL;
		}
	}
	
	size_t n_idents; n_idents = arr_len(d->idents);
	if (d->type.kind == TYPE_TUPLE) {
		if (n_idents != arr_len(d->type.tuple)) {
			err_print(d->where, "Expected to have %lu things declared in declaration, but got %lu.", (unsigned long)arr_len(d->type.tuple), (unsigned long)n_idents);
			success = false;
			goto ret;
		}
	} else if (d->type.kind == TYPE_UNKNOWN) {
		err_print(d->where, "Can't determine type of declaration.");
		success = false;
		goto ret;
	}
	if (d->flags & DECL_IS_CONST) {
		if (d->type.kind == TYPE_PTR) {
			err_print(d->where, "You can't have a constant pointer.");
			success = false;
		    goto ret;
		}
	}
	
	if (n_idents == 1 && (d->flags & DECL_HAS_EXPR) && d->expr.kind == EXPR_NMS) {
		bool is_at_top_level = true;
		typedef Block *BlockPtr;
		arr_foreach(tr->blocks, BlockPtr, b) {
			if (*b && !((*b)->flags & BLOCK_IS_NMS)) {
				is_at_top_level = false;
				break;
			}
		}
		if (is_at_top_level)
			d->expr.nms->associated_ident = d->idents[0];
	}

	if (tr->nms && tr->block == &tr->nms->body) {
		arr_foreach(d->idents, Identifier, ident) {
			(*ident)->nms = tr->nms;
		}
	}

	
 ret:
	/* pretend we found the type even if we didn't to prevent too many errors */
	d->flags |= DECL_FOUND_TYPE;
	if (!success) {
		/* use unknown type if we didn't get the type */
		d->type.flags = TYPE_IS_RESOLVED;
		d->type.was_expr = NULL;
		d->type.kind = TYPE_UNKNOWN;
		tr->evalr->enabled = false; /* disable evaluator completely so that it doesn't accidentally try to access this declaration */
	}
	arr_remove_lasta(&tr->in_decls, tr->allocr);
	return success;
}

static Status types_stmt(Typer *tr, Statement *s) {
	if (s->flags & STMT_TYPED) return true;
	switch (s->kind) {
	case STMT_EXPR:
		if (!types_expr(tr, &s->expr)) {
			return false;
		}

		if (!(s->flags & STMT_EXPR_NO_SEMICOLON)) {
			if (s->expr.kind == EXPR_TUPLE) {
				err_print(s->where, "Statement of a tuple is not allowed. Use a semicolon instead of a comma here.");
				return false;
			}
			Type *t = &s->expr.type;
			if (type_is_compileonly(t)) {
				char *str = type_to_str(t);
				warn_print(s->where, "This expression has a compile-only type (%s), so this statement will not actually be outputted in C code.", str);
				free(str);
			}
		}
		if (tr->block == NULL) {
			if (s->expr.kind != EXPR_C) {
				if (!eval_stmt(tr->evalr, s))
					return false;
			}
		}
		break;
	case STMT_DECL:
		if (!types_decl(tr, s->decl)) {
			return false;
		}
		break;
	case STMT_RET:
		if (!tr->fn) {
			err_print(s->where, "return outside of a function.");
			return false;
		}
		if (s->ret.flags & RET_HAS_EXPR) {
			if (tr->fn->ret_type.kind == TYPE_VOID) {
				err_print(s->where, "Return value in a void function.");
				return false;
			}
			if (tr->fn->ret_decls) {
				err_print(s->where, "Return expression in a function with named return values.");
				return false;
			}
			if (!types_expr(tr, &s->ret.expr))
				return false;
			if (!type_eq(&tr->fn->ret_type, &s->ret.expr.type)) {
				char *got = type_to_str(&s->ret.expr.type);
				char *expected = type_to_str(&tr->fn->ret_type);
				err_print(s->where, "Returning type %s in function which returns %s.", got, expected);
				return false;
			}
		} else {
			if (tr->fn->ret_type.kind != TYPE_VOID
				&& !tr->fn->ret_decls) {
				err_print(s->where, "No return value in non-void function.");
				return false;
			}
		}
		break;
	case STMT_INCLUDE: {
		char *filename = eval_expr_as_cstr(tr, &s->inc.filename, "import filename");
		if (!filename)
			return false;
		size_t filename_len = strlen(filename);
		IncludedFile *inc_f = NULL;
		if (s->flags & STMT_INC_TO_NMS) {
			if (!(s->inc.flags & INC_FORCED)) {
				inc_f = str_hash_table_get(&tr->included_files, filename, filename_len);
				if (inc_f) {
					tr->nms->body.idents = inc_f->main_nms->body.idents;
					tr->nms->body.idents.scope = &tr->nms->body;
					tr->nms->points_to = inc_f->main_nms;
					s->inc.inc_file = inc_f;
					s->inc.stmts = inc_f->stmts;
					break;
				}
			}
			s->inc.inc_file = inc_f = str_hash_table_insert(&tr->included_files, filename, filename_len);
			inc_f->main_nms = tr->nms;
		}
		char *contents = read_file_contents(tr->allocr, filename, s->where);
		if (!contents)
			return false;

		Tokenizer tokr;
		tokr_create(&tokr, tr->err_ctx, tr->allocr);
		File *file = typer_calloc(tr, 1, sizeof *file);
		file->filename = filename;
		file->contents = contents;
		file->ctx = tr->err_ctx;
		if (!tokenize_file(&tokr, file))
			return false;
		Parser parser;
		parser_create(&parser, tr->globals, &tokr, tr->allocr);
		parser.block = tr->block;
		ParsedFile parsed_file;
		if (!parse_file(&parser, &parsed_file)) {
			return false;
		}
		Statement *stmts_inc = parsed_file.stmts;
		if (inc_f) {
			inc_f->stmts = stmts_inc;
		}
	    s->inc.stmts = stmts_inc;
		arr_foreach(stmts_inc, Statement, s_incd) {
			if (!types_stmt(tr, s_incd))
				return false;
		}
		
	} break;
	}
	s->flags |= STMT_TYPED;
	return true;
}

static void typer_create(Typer *tr, Evaluator *ev, ErrCtx *err_ctx, Allocator *allocr, Identifiers *idents) {
	tr->block = NULL;
	tr->blocks = NULL;
	tr->fn = NULL;
	tr->nms = NULL;
	tr->evalr = ev;
	tr->err_ctx = err_ctx;
	tr->in_decls = NULL;
	tr->in_exprs = NULL;
	tr->allocr = allocr;
	tr->globals = idents;
	*(Block **)arr_adda(&tr->blocks, allocr) = NULL;
	str_hash_table_create(&tr->included_files, sizeof(IncludedFile), tr->allocr);
}

static Status types_file(Typer *tr, ParsedFile *f) {
	bool ret = true;
	tr->parsed_file = f;
	arr_foreach(f->stmts, Statement, s) {
		if (!types_stmt(tr, s)) {
			ret = false;
		}
	}
	return ret;
}
