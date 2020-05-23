/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static Status types_stmt(Typer *tr, Statement *s);
static Status types_block(Typer *tr, Block *b);
static Status types_decl(Typer *tr, Declaration *d);
static Status type_resolve(Typer *tr, Type *t, Location where);
static Status eval_expr(Evaluator *ev, Expression *e, Value *v);
static void val_cast(Value *vin, Type *from, Value *vout, Type *to);
static U64 val_to_u64(Value v, BuiltinType v_type);
static I64 val_to_i64(Value v, BuiltinType v_type);
static bool val_truthiness(Value v, Type *t);
static Value val_zero(Type *t); 
static Status eval_stmt(Evaluator *ev, Statement *stmt); 
static Status struct_resolve(Typer *tr, StructDef *s);
static Status expr_must_usable(Typer *tr, Expression *e);

static inline Identifiers *typer_get_idents(Typer *tr) {
	return tr->block == NULL ? tr->globals : &tr->block->idents;
}

static inline void *typer_malloc(Typer *tr, size_t bytes) {
	return allocr_malloc(tr->allocr, bytes);
}

static inline void *typer_calloc(Typer *tr, size_t n, size_t sz) {
	return allocr_calloc(tr->allocr, n, sz);
}

#define typer_arr_add(tr, a, x) arr_adda(a, x, tr->allocr)
#define typer_arr_add_ptr(tr, a) arr_adda_ptr(a, tr->allocr)

static inline void typer_block_enter(Typer *tr, Block *b) {
	tr->block = b;
}

static inline void typer_block_exit(Typer *tr) {
	tr->block = tr->block->parent;
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
	case BUILTIN_VARARGS: return sizeof(VarArg *);
	case BUILTIN_VOID: return 1; /* void ptr arithmetic */
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
	case BUILTIN_VARARGS: return toc_alignof(VarArg *);
	case BUILTIN_VOID: return 1;
	}
	assert(0);
	return 0;
}

static inline char *get_struct_name(StructDef *s) {
	return s->name ? ident_to_str(s->name) : str_dup("anonymous struct");
}

/* adds fields of add to to */
static Status struct_add_used_struct(Typer *tr, StructDef *to, StructDef *add, Declaration *use_decl) {
	Location use_where = use_decl->where;
	if (!struct_resolve(tr, add))
		return false;
	arr_foreach(add->body.stmts, Statement, stmt) {
		assert(stmt->kind == STMT_DECL);
		Declaration *decl = stmt->decl;
		if (decl->flags & DECL_USE) {
			assert(decl->type.kind == TYPE_STRUCT);
			if (!struct_add_used_struct(tr, to, decl->type.struc, decl))
				return false;
		}
		arr_foreach(decl->idents, Identifier, ip) {
			Identifier i = *ip;
			/* @OPTIM: only hash once */
			Identifier previously_existing = ident_translate(i, &to->body.idents);
			if (previously_existing) {
				/* uh oh */
				UsedFrom *uf = previously_existing->used_from;
				char *struct_name = get_struct_name(to);
				char *member_name = ident_to_str(previously_existing);
				if (uf) {
					/* conflicting uses */
					Declaration *first_use = uf->use_decl;
					err_print(first_use->where, "Conflicting used structs, while dealing with %s. %s was imported by this use statement...", struct_name, member_name);
					info_print(use_where, "... and also by this use statement.");
				} else {
					/* declared a field, then used something which contains something of the same name */
					Declaration *first_decl = previously_existing->decl;
					char *used_struct_name = get_struct_name(add);
					err_print(use_where, "used struct conflicts with field %s of %s (%s is also a member of %s).", member_name, struct_name, member_name, used_struct_name);
					info_print(first_decl->where, "%s was declared here as a field.", member_name);
					free(used_struct_name);
				}
				free(struct_name);
				free(member_name);
				return false;
			}
			Identifier new_ident = ident_translate_forced(i, &to->body.idents);
			new_ident->decl = i->decl;
			UsedFrom *uf = new_ident->used_from = typer_malloc(tr, sizeof *new_ident->used_from);
			uf->use_decl = use_decl;
			uf->struc = add;
		}
	}
	return true;
}

/* create s->fields, also check to make sure the struct's statements are valid */
static Status struct_add_stmts(Typer *tr, StructDef *s, Statement *stmts) {
	arr_foreach(stmts, Statement, stmt) {
		StatementKind kind = stmt->kind;
		if (kind == STMT_INLINE_BLOCK) {
			if (!struct_add_stmts(tr, s, stmt->inline_block))
				return false;
			continue;
		}
		if (kind != STMT_DECL) {
			err_print(stmt->where, "structs can only contain declarations.");
			return false;
		}
		Declaration *d = stmt->decl;
		DeclFlags flags = d->flags;
		if (flags & DECL_EXPORT) {
			err_print(d->where, "struct members can't be exported.");
			return false;
		}
		if (flags & DECL_IS_CONST) {
			if (flags & DECL_INFER) {
				err_print(d->where, "struct members can't be inferred.");
				return false;
			}
		} else {
			if (flags & DECL_SEMI_CONST) {
				err_print(d->where, "struct members can't be semi-constant.");
				return false;
			}
			if (flags & DECL_HAS_EXPR) {
				err_print(d->where, "Non-constant struct members can't have initializers.");
				return false;
			}
			int i = 0;
			arr_foreach(d->idents, Identifier, ident) {
				Field *field = typer_arr_add_ptr(tr, s->fields);
				field->where = d->where;
				field->name = *ident;
				field->type = decl_type_at_index(d, i);
				(*ident)->used_from = NULL;
				++i;
			}
		}
		if (flags & DECL_USE) {
			/* add everything in the used struct to the namespace */
			if (flags & DECL_IS_CONST) {
				/* @TODO(eventually) */
				err_print(d->where, "You can't use constant stuff in a struct.");
				return false;
			}
			if (d->type.kind != TYPE_STRUCT) {
				/* i don't think this can ever happen right now */
				err_print(d->where, "You can only use structs inside a struct.");
				return false;
			}
			if (arr_len(d->idents) > 1) {
				err_print(d->where, "use declarations can only declare one thing. Every single used identifier would have conflicting definitions otherwise.");
				return false;
			}
			if (!struct_add_used_struct(tr, s, d->type.struc, d))
				return false;
		}
	}
	return true;
}

/* 
	set the field pointers of the declarations in this struct, so that when we look up the declaration of 
	a member of the struct, we know which Field it refers to.
	this needs to be a different step after struct_add_stmts, because the pointers to Fields can change 
	during struct_add_stmts
*/
static void struct_set_decl_field_ptrs(Typer *tr, StructDef *s, Statement *stmts) {
	Field *field = s->fields;
	arr_foreach(stmts, Statement, stmt) {
		if (stmt->kind == STMT_INLINE_BLOCK) {
			struct_set_decl_field_ptrs(tr, s, stmt->inline_block);
			continue;
		}
		assert(stmt->kind == STMT_DECL);
		Declaration *decl = stmt->decl;
		if (!(decl->flags & DECL_IS_CONST)) {
			assert(!(decl->flags & DECL_HAS_EXPR));
			decl->field = field;
			field += arr_len(decl->idents);
		}
	}
}

static size_t compiler_alignof(Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_BUILTIN:
		return compiler_alignof_builtin(t->builtin);
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
		return (size_t)t->arr.n * compiler_sizeof(t->arr.of);
	case TYPE_TUPLE:
		return sizeof v.tuple;
	case TYPE_SLICE:
		return sizeof v.slice;
	case TYPE_STRUCT: {
		/* these two ifs are purely for struct_resolve, so that it can detect use of future structs in a non-pointery way */
		if (t->struc->flags & STRUCT_DEF_RESOLVING)
			return SIZE_MAX-1;
		if (!(t->struc->flags & STRUCT_DEF_RESOLVED))
			return SIZE_MAX;
		return t->struc->size;
	} break;
	case TYPE_UNKNOWN:
		return 0;
	case TYPE_EXPR:
		break;
	}
	assert(0);
	return 0;
}

static Status struct_resolve(Typer *tr, StructDef *s) {
	if (s->flags & STRUCT_DEF_RESOLVING_FAILED) 
		return false; /* silently fail; do not try to resolve again, because there'll be duplicate errors */
	if (!(s->flags & STRUCT_DEF_RESOLVED)) {
		s->flags |= STRUCT_DEF_RESOLVING;
		typer_arr_add(tr, tr->all_structs, s);
		{ /* resolving stuff */
			Block *body = &s->body;
			if (!types_block(tr, body))
				goto fail;
			s->fields = NULL;
			Statement *stmts = body->stmts;
			if (!struct_add_stmts(tr, s, stmts))
				goto fail;
			struct_set_decl_field_ptrs(tr, s, stmts);
			s->instance_id = 0;
		}
		/* find offsets and size */
		/* assume the align of a struct is the greatest align out of its children's */
		{
			size_t bytes = 0;
			size_t total_align = 1;
			arr_foreach(s->fields, Field, f) {
				size_t size = compiler_sizeof(f->type);
				if (size == SIZE_MAX) {
					err_print(f->where, "Use of type that hasn't been declared yet (but not as a pointer/slice).\n"
						"Either make this field a pointer or put the declaration of its type before this struct.");
					goto fail;
				} else if (size == SIZE_MAX-1) {
					err_print(f->where, "Circular dependency in structs! You will need to make this member a pointer.");
					goto fail;
				}
				size_t falign = compiler_alignof(f->type);
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
		}
		s->flags &= (StructFlags)~(StructFlags)STRUCT_DEF_RESOLVING;
		s->flags |= STRUCT_DEF_RESOLVED;
	}
	return true;
fail:
	s->flags |= STRUCT_DEF_RESOLVING_FAILED;
	return false;
}


/* are a and b EXACTLY equal (not counting flags)? */
static bool type_eq_exact(Type *a, Type *b) {
	assert(a->flags & TYPE_IS_RESOLVED);
	assert(b->flags & TYPE_IS_RESOLVED);
	
	if (a->kind != b->kind) return false;
	switch (a->kind) {
	case TYPE_UNKNOWN: return true;
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
			if (!type_eq_exact(&a_types[i], &b_types[i]))
				return false;
			
		}
		return true;
	}
	case TYPE_TUPLE: {
		if (arr_len(a->tuple) != arr_len(b->tuple)) return false;
		Type *a_types = a->tuple, *b_types = b->tuple;
		for (size_t i = 0; i < arr_len(a->tuple); ++i) {
			if (!type_eq_exact(&a_types[i], &b_types[i]))
				return false;
		}
		return true;
	}
	case TYPE_ARR:
		if (a->arr.n != b->arr.n) return false;
		return type_eq_exact(a->arr.of, b->arr.of);
	case TYPE_SLICE:
		return type_eq_exact(a->slice, b->slice);
	case TYPE_PTR:
		return type_eq_exact(a->ptr, b->ptr);
	case TYPE_EXPR:
		break;
	}
	assert(0);
	return false;
}

/* are a and b equal, allowing implicit conversions? */
static bool type_eq_implicit(Type *a, Type *b) {
	if (a->kind == TYPE_UNKNOWN || b->kind == TYPE_UNKNOWN)
		return true;
	if (a->kind != b->kind) return false;
	if (b->flags & TYPE_IS_FLEXIBLE) {
		Type *tmp = b;
		b = a;
		a = tmp;
	}
	if (a->flags & TYPE_IS_FLEXIBLE) {
		assert(a->kind == TYPE_BUILTIN);
		if (b->flags & TYPE_IS_FLEXIBLE) return true;

		if (type_builtin_is_float(a->builtin)) {
			return type_builtin_is_float(b->builtin);
		}
		assert(a->builtin == BUILTIN_I64);
		return type_builtin_is_numerical(b->builtin);
	}
	if (a->kind == TYPE_PTR) {
		/* &void casts to &anything */
		if (type_is_builtin(a->ptr, BUILTIN_VOID) || type_is_builtin(b->ptr, BUILTIN_VOID))
			return true;
	}
	return type_eq_exact(a, b);
}

/* which is the "overriding" type? i.e. which type should the other one convert to? */
static Type *overriding_type(Type *a, Type *b) {
	if (a->kind == TYPE_UNKNOWN) return b;
	if (b->kind == TYPE_UNKNOWN) return a;
	if (a->flags & TYPE_IS_FLEXIBLE) {
		assert(a->kind == TYPE_BUILTIN);
		if (b->flags & TYPE_IS_FLEXIBLE) {
			if (type_builtin_is_float(a->builtin))
				return a;
		}
		return b;
	}

	if (b->flags & TYPE_IS_FLEXIBLE)
		return a;

	if (a->kind == TYPE_PTR && type_is_builtin(a->ptr, BUILTIN_VOID)) 
		return b;

	/* doesn't matter */
	return a;
}

/* 
prints an error and returns false if the given expression is not an l-value 
purpose is something like "take address of"
*/
static Status expr_must_lval(Expression *e, const char *purpose) {
	/* NOTE: make sure you update eval when you change this */
	assert(e->flags & EXPR_FOUND_TYPE);
	switch (e->kind) {
	case EXPR_IDENT: {
		Identifier i = e->ident;
		Declaration *d = i->decl;
		if (d->flags & DECL_IS_CONST) {
			char *istr = ident_to_str(i);
			err_print(e->where, "Cannot %s constant %s.", purpose, istr);
			info_print(d->where, "%s was declared here.", istr);
			free(istr);
			return false;
		}
		if (type_is_builtin(&d->type, BUILTIN_VARARGS)) {
			char *istr = ident_to_str(i);
			err_print(e->where, "Cannot %s varargs.", purpose);
			info_print(d->where, "%s was declared here.", istr);
			free(istr);
			return false;
		}
		return true;
	}
	case EXPR_UNARY_OP:
		if (e->unary.op == UNARY_DEREF) return true;
		err_print(e->where, "Cannot %s operator %s.", purpose, unary_op_to_str(e->unary.op));
		return false;
	case EXPR_BINARY_OP:
		switch (e->binary.op) {
		case BINARY_AT_INDEX:
			if (!expr_must_lval(e->binary.lhs, purpose))
				return false;
			if (type_is_builtin(&e->binary.lhs->type, BUILTIN_VARARGS)) {
				err_print(e->where, "Cannot set or take address of vararg.");
				return false;
			}
			return true;
		case BINARY_DOT:  
			if (e->type.kind == TYPE_PTR) return true; /* structure->member is always an lvalue */
			return expr_must_lval(e->binary.lhs, purpose);
		default: break;
		}
		err_print(e->where, "Cannot %s operator %s.", purpose, binary_op_to_str(e->binary.op));
		return false;
	case EXPR_TUPLE:
		/* x, y is an lval, but 3, "hello" is not. */
		arr_foreach(e->tuple, Expression, x) {
			if (!expr_must_lval(x, purpose)) 
				return false;
		}
		return true;
	default: {
		err_print(e->where, "Cannot %s %s.", purpose, expr_kind_to_str(e->kind));
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
	case TYPE_UNKNOWN:
		return false;
	case TYPE_BUILTIN:
		return t->builtin == BUILTIN_TYPE || t->builtin == BUILTIN_NMS || t->builtin == BUILTIN_VARARGS;
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
	char *str = typer_malloc(tr, (size_t)e_slice.len + 1);
	str[e_slice.len] = 0;
	memcpy(str, e_slice.data, (size_t)e_slice.len);
	return str;
}

static char *slice_to_cstr(Slice s) {
	char *ret = err_malloc((size_t)s.len + 1);
	memcpy(ret, s.data, (size_t)s.len);
	ret[s.len] = 0;
	return ret;
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
	t->fn.constness = NULL; /* @OPTIM: constness doesn't need to be a dynamic array */
	t->flags = 0;
	bool success = true;
	bool entered_fn = false;
	size_t param_idx;
	FnExpr *prev_fn = tr->fn;

	Declaration *last_param = arr_last_ptr(f->params);
	bool has_varargs = last_param && (last_param->flags & DECL_ANNOTATES_TYPE) && type_is_builtin(&last_param->type, BUILTIN_VARARGS);
	if (has_varargs)
		f->flags |= FN_EXPR_HAS_VARARGS;
	/* f has compile time params/varargs, but it's not an instance! */
	bool generic = !(flags & TYPE_OF_FN_IS_INSTANCE) && (fn_has_any_const_params(f) || has_varargs);
	size_t idx = 0;
	bool has_constant_params = false;
	/* reserve space for return type */
	typer_arr_add_ptr(tr, t->fn.types);
	tr->fn = f;
	Block *prev_block = tr->block;
	tr->block = &f->body;
	f->body.uses = NULL;
	size_t nparams = arr_len(f->params);
	entered_fn = true;
	for (param_idx = 0; param_idx < nparams; ++param_idx) {
		Declaration *param = &f->params[param_idx];
		if (!generic) {
			if (!types_decl(tr, param)) {
				success = false;
				goto ret;
			}

			if (type_is_builtin(&param->type, BUILTIN_VARARGS)) {
				if (param_idx != nparams-1 || arr_len(param->idents) > 1) {
					err_print(param->where, "varargs must be the last parameter to a function.");
					success = false;
					goto ret;
				}
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
					typer_arr_add(tr, t->fn.constness, CONSTNESS_NO);
				}
			}
		}
		for (size_t i = 0; i < arr_len(param->idents); ++i) {
			Type *param_type = typer_arr_add_ptr(tr, t->fn.types);
			if (param->flags & (DECL_ANNOTATES_TYPE|DECL_FOUND_TYPE))
				*param_type = param->type;
			else
				param_type->kind = TYPE_UNKNOWN;
			if (has_constant_params) {
				Constness constn;
				if (param->flags & DECL_IS_CONST) {
					constn = CONSTNESS_YES;
				} else if (param->flags & DECL_SEMI_CONST) {
					constn = CONSTNESS_SEMI;
				} else {
					constn = CONSTNESS_NO;
				}
				typer_arr_add(tr, t->fn.constness, constn);
			}
			++idx;
		}
	}
	
	if (f->ret_decls && !generic && type_is_builtin(&f->ret_type, BUILTIN_VOID) /* haven't found return type yet */) {
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
			f->ret_type.tuple = NULL;
			arr_foreach(f->ret_decls, Declaration, d) {
				arr_foreach(d->idents, Identifier, i) {
					typer_arr_add(tr, f->ret_type.tuple, d->type);
				}
			}
		}
	}
	if (!generic) {
		if (!type_resolve(tr, &f->ret_type, f->where)) {
			success = false;
			goto ret;
		}
		if (type_is_builtin(&f->ret_type, BUILTIN_VARARGS)) {
			err_print(f->where, "Functions cannot return varargs.");
			success = false;
			goto ret;
		}
		if (type_is_compileonly(&f->ret_type)) {
			if (type_is_builtin(&f->ret_type, BUILTIN_NMS)) {
				err_print(f->where, "Functions cannot return namespaces.");
				success = false;
				goto ret;
			}
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
	
	{
		Type *ret_type = &t->fn.types[0];
		*ret_type = f->ret_type;
	}

 ret:
	/* cleanup */
	tr->block = prev_block;
	if (entered_fn) {
		tr->fn = prev_fn;
	}
	return success;
}

/* doesn't do any translation on ident or check if it's declared or anything, so make sure it's in the right scope */
static Status type_of_ident(Typer *tr, Location where, Identifier i, Type *t) {
top:;
	Declaration *d = i->decl;
	assert(d);
	if (!(d->flags & DECL_IS_CONST)) {
		/* check for trying to capture a variable into a function */
		bool captured = false;
		Block *decl_scope = ident_scope(i);
		if (decl_scope && decl_scope->kind != BLOCK_NMS) {
			if (decl_scope->kind != BLOCK_NMS) {
				/* go back through scopes */
				for (Block *block = tr->block; block; block = block->parent) {
					if (block == decl_scope) break;
					if (block->kind == BLOCK_FN) {
						captured = true;
						break;
					}
				}
			}
		}
		if (captured) {
			err_print(where, "Variables cannot be captured into inner functions (but constants can).");
			return false;
		}
	}
	if ((d->flags & DECL_HAS_EXPR) && (d->expr.kind == EXPR_TYPE)) {
		/* allow using a type before declaring it */
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_TYPE;
		t->flags = TYPE_IS_RESOLVED;
		return true;
	}
	 
	/* are we inside this declaration? */
	arr_foreach(tr->in_decls, DeclarationPtr, in_decl) {
		if (d == *in_decl) {
			/* d needn't have an expression, because it could be its type that refers to itself */
			if ((d->flags & DECL_HAS_EXPR) && d->expr.kind == EXPR_FN) {
				/* it's okay if a function references itself */
			} else {
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
		assert(t->flags & TYPE_IS_RESOLVED);
		return true;
	} else {
		if ((d->flags & DECL_HAS_EXPR) && (d->expr.kind == EXPR_FN)) {
			/* allow using a function before declaring it */
			if (!type_of_fn(tr, d->expr.fn, &d->expr.type, 0)) return false;
			*t = d->expr.type;
			t->flags |= TYPE_IS_RESOLVED; /* for function templates */
			return true;
		} else {
			if (d->flags & DECL_INFER) {
				char *s = ident_to_str(i);
				err_print(where, "Use of identifier %s before it has been inferred. You are trying to do stuff with inference which toc doesn't support.", s);
				free(s);
				return false;
			}
			if ((d->flags & DECL_IS_CONST) && (tr->block == NULL)) {
				/* let's type the declaration, and redo this (for evaling future constants) */
				if (!types_decl(tr, d)) return false;
				goto top;
			} else {
				char *s = ident_to_str(i);
				err_print(where, "Use of %s before its declaration.", s);
				info_print(d->where, "%s will be declared here.", s);
				free(s);
				return false;
			}
		}
	}
	return true;
}

/* fixes the type (replaces [5+3]int with [8]int, etc.) */
static Status type_resolve(Typer *tr, Type *t, Location where) {
	Evaluator *ev = tr->evalr;
	if (t->flags & TYPE_IS_RESOLVED) return true;
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
			I64 ssize = val_to_i64(val, n_expr->type.builtin);
			if (ssize < 0) {
				err_print(t->arr.n_expr->where, "Negative array length (" I64_FMT ")", ssize);
				return false;
			}
			size = (U64)ssize;
		} else {
			size = val_to_u64(val, n_expr->type.builtin);
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
		StructDef *s = t->struc;
		if (!struct_resolve(tr, s))
			return false;
	} break;
	case TYPE_EXPR: {
		Value typeval;
		Expression *expr = t->expr;
		if (!types_expr(tr, expr))
			return false;
		if (expr->type.kind == TYPE_UNKNOWN && tr->err_ctx->have_errored)
			return false; /* silently fail (e.g. if a function couldn't be typed) */
		if (!type_is_builtin(&expr->type, BUILTIN_TYPE)) {
			/* ok maybe it's a tuple of types, which we'll convert to a TYPE_TUPLE */
			bool is_tuple_of_types = false;
			if (expr->kind == EXPR_TUPLE) {
				Type *tuple = NULL;
				is_tuple_of_types = true;
				arr_foreach(expr->tuple, Expression, sub) {
					if (!type_is_builtin(&sub->type, BUILTIN_TYPE)) {
						is_tuple_of_types = false;
						break;
					}
					if (!eval_expr(tr->evalr, sub, &typeval))
						return false;
					typer_arr_add(tr, tuple, *typeval.type);
				}
				if (is_tuple_of_types) {
					t->kind = TYPE_TUPLE;
					t->flags = TYPE_IS_RESOLVED;
					t->tuple = tuple;
					break;
				}
			}
			if (!is_tuple_of_types) {
				char *s = type_to_str(&expr->type);
				err_print(expr->where, "This expression is not a type (it's %s %s), but it's being used as a type.", indefinite_article(s), s);
				free(s);
				return false;
			}
		}
		if (!eval_expr(tr->evalr, expr, &typeval))
			return false;
		*t = *typeval.type;
		if (t->kind == TYPE_STRUCT) {
			if (struct_is_template(t->struc)) {
				err_print(where, "Expected arguments to struct, but you didn't provide any.");
				info_print(t->struc->where, "struct was declared here.");
				return false;
			}
		}
		if (!(t->flags & TYPE_IS_RESOLVED)) {
			/* this can happen with functions returning parameterized structs or pointers */
			if (!type_resolve(tr, t, where))
				return false;
		}
	} break;
	case TYPE_UNKNOWN:
	case TYPE_BUILTIN:
		break;
	}
	assert(t->kind != TYPE_EXPR);
	t->flags |= TYPE_IS_RESOLVED;
	return true;
}


static bool type_can_be_truthy(Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
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
		case BUILTIN_VOID:
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
		case BUILTIN_VARARGS:
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
				case BUILTIN_VARARGS:
				case BUILTIN_VOID:
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
			case BUILTIN_VARARGS:
			case BUILTIN_VOID:
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
		case BUILTIN_VARARGS:
		case BUILTIN_VOID:
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
		/* @TODO: Cast from ptr to arr */
		return CAST_STATUS_ERR;
	case TYPE_ARR:
		return CAST_STATUS_ERR;
	case TYPE_SLICE:
		if (to->kind == TYPE_PTR && type_eq_exact(from->slice, to->ptr))
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

/* does this statement no matter what result in a return? */
static Status definitely_returns(Statement *s) {
	if (s->kind == STMT_RET) return true; /* of course */
	if (s->kind == STMT_IF) {
		/* if foo { return 5; } else { return 6; } */
		bool has_else = false;
		for (If *i = s->if_; i; i = i->next_elif) {
			Statement *last_stmt = arr_last_ptr(i->body.stmts);
			if (!definitely_returns(last_stmt))
				return false;
			if (!i->cond) has_else = true;
		}
		return has_else;
	} else if (s->kind == STMT_BLOCK) {
		/* { return 7; } */
		Statement *last_stmt = arr_last_ptr(s->block->stmts);
		return definitely_returns(last_stmt);
	}
	return false;
}

/* pass NULL for instance if this isn't an instance */
static Status types_fn(Typer *tr, FnExpr *f, Type *t, Instance *instance) {
	f->declaration_block = tr->block;
	if (f->flags & FN_EXPR_FOREIGN) {
		FnWithCtx fn_ctx = {f, tr->nms, tr->block};
		typer_arr_add(tr, tr->all_fns, fn_ctx);
		return true;
	}
	FnExpr *prev_fn = tr->fn;
	bool success = true;
	Type *ret_type;
	bool has_named_ret_vals;
	assert(t->kind == TYPE_FN);
	if (instance) {
		f = instance->fn;
	} else {
		if (t->fn.constness)
			return true; /* don't type function body yet; we need to do that for every instance */
	}
	{
		FnWithCtx fn_ctx = {f, tr->nms, tr->block};
		typer_arr_add(tr, tr->all_fns, fn_ctx);
	}
	tr->fn = f;
	if (!types_block(tr, &f->body)) {
		success = false;
		goto ret;
	}
	ret_type = t->fn.types;
	has_named_ret_vals = f->ret_decls != NULL;
	if (!type_is_builtin(ret_type, BUILTIN_VOID) && !has_named_ret_vals) {
		Statement *stmts = f->body.stmts;
		if (arr_len(stmts)) {
			Statement *last_stmt = arr_last_ptr(stmts);
			if (definitely_returns(last_stmt))
				goto ret;
		}
		char *expected = type_to_str(ret_type);
		err_print(token_location(f->body.where.file, &f->body.where.file->tokens[f->body.where.end-1]), 
				"Function which should return %s is missing a return statement (or it does not clearly always return).", expected);
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
	if (nargs > nparams && !(fn->flags & FN_EXPR_HAS_VARARGS)) {
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
		Declaration *this_param;
		if (named) {
			/* named argument */
			int index = 0;
			bool found = false;
			arr_foreach(fn->params, Declaration, pa) {
				arr_foreach(pa->idents, Identifier, id) {
					if (ident_eq_str(*id, arg->name)) {
						if (type_is_builtin(&pa->type, BUILTIN_VARARGS)) {
							err_print(arg->where, "varargs arguments cannot be named.");
							return false;
						}
						this_param = pa;
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
			this_param = param;
			if (param > (Declaration *)arr_last_ptr(fn->params)) {
				err_print(arg->where, "Too many arguments to function!");
				info_print(fn->where, "Declaration is here.");
				return false;
			}
			param_idx = p;
		}

		if (type_is_builtin(&this_param->type, BUILTIN_VARARGS)) {
			if (param_idx < (int)nparams && order[param_idx] == -1) {
				order[param_idx] = arg_idx;
			}
		} else {
			if (param_idx != -1) {
				if (order[param_idx] != -1) {
					err_print(arg->where, "Parameter #%d set twice.", param_idx+1);
					info_print(args[order[param_idx]].where, "Parameter was previously set here.");
					return false;
				}
				order[param_idx] = arg_idx;
			}
		}

		if (!named) {
			/* sequential order of parameters */
			++p;
			if (!type_is_builtin(&param->type, BUILTIN_VARARGS)) {
				++ident_idx;
				if (ident_idx == arr_len(param->idents)) {
					++param;
					ident_idx = 0;
				}
			}
		}
	}
	size_t param_idx = 0;
	arr_foreach(fn->params, Declaration, decl) {
		arr_foreach(decl->idents, Identifier, ident) {
			if (order[param_idx] == -1) {
				if ((decl->flags & DECL_ANNOTATES_TYPE) && type_is_builtin(&decl->type, BUILTIN_VARARGS)) {
					order[param_idx] = (I16)nargs;
				} else if (!(decl->flags & DECL_HAS_EXPR) && !(decl->flags & DECL_INFER)) {
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
	case BUILTIN_PLATFORM:
		v.i64 = platform__;
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
	case BUILTIN_PLATFORM:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_I64;
		break;
	}
}


/* gets a struct's constant or parameter, and puts it into e->val.  */
static Status get_struct_constant(StructDef *struc, String ident, Expression *e) {
	if (struc->params && !(struc->params[0].flags & DECL_FOUND_VAL)) {
		err_print(e->where, "To access constants from a parameterized struct, you must supply its arguments.");
		return false;
	}
	Identifier i = ident_get_with_len(&struc->body.idents, ident.str, ident.len);
	if (!i) {
		char *member_s = cstr(ident.str, ident.len);
		char *struc_s = struc->name ? ident_to_str(struc->name) : "anonymous struct";
		err_print(e->where, "%s is not a member of structure %s.", member_s, struc_s);
		info_print(struc->where, "struct was declared here.");
		free(member_s);
		if (struc->name) free(struc_s);
		return false;
	}
	if (!(i->decl->flags & DECL_IS_CONST)) {
		err_print(e->where, "Struct field being used as if it were a constant.");
		info_print(i->decl->where, "Field was declared here.");
		return false;
	}
	if (i->decl->flags & DECL_FOUND_VAL) {
		/* replace with decl value */
		int ident_idx = decl_ident_index(i->decl, i);
		e->kind = EXPR_VAL;
		e->flags = EXPR_FOUND_TYPE;
		e->val = *decl_val_at_index(i->decl, ident_idx);
		e->type = *decl_type_at_index(i->decl, ident_idx);
		return true;
	} else {
		char *member_s = cstr(ident.str, ident.len);
		char *struc_s = get_struct_name(struc);
		err_print(e->where, "Cannot get value %s from struct %s. Are you missing parameters to this struct?", member_s, struc_s);
		free(member_s);
		free(struc_s);
		return false;
	}
}

static bool fn_type_has_varargs(FnType *f) {
	return type_is_builtin(arr_last_ptr(f->types), BUILTIN_VARARGS);
}

static Status expr_must_usable_(Expression *e) {
	if (e->kind == EXPR_IDENT) return true;
	if (e->kind == EXPR_BINARY_OP && e->binary.op == BINARY_DOT)
		return expr_must_usable_(e->binary.lhs);
	return false;
}

static Status expr_must_usable(Typer *tr, Expression *e) {
	Type *t = &e->type;
	if (t->kind != TYPE_STRUCT && !type_is_builtin(t, BUILTIN_NMS)) {
		if (!(t->kind == TYPE_PTR && t->ptr->kind == TYPE_STRUCT)) {
			if (t->kind == TYPE_UNKNOWN) {
				if (tr->err_ctx->have_errored) {
					/* silently fail; this could've been because of an earlier error */
					return false;
				}
			}
			char *str = type_to_str(&e->type);
			err_print(e->where, "You cannot use something of type %s (only Namespaces and structs).", str);
			free(str);
			return false;
		}
	}
	return expr_must_usable_(e);
}


static Status use_ident(Typer *tr, Identifier i, Type *t, Location where) {
	/* add to uses */
	Use **usep;
	if (tr->block)
		usep = typer_arr_add_ptr(tr, tr->block->uses);
	else
		usep = typer_arr_add_ptr(tr, tr->uses);
	Use *use = *usep = typer_calloc(tr, 1, sizeof *use);
	Expression *used = &use->expr;
	used->kind = EXPR_IDENT;
	used->flags = EXPR_FOUND_TYPE;
	used->type = *t;
	used->ident = i;
	used->where = where;
	if (!expr_must_usable(tr, used))
		return false;
	return true;
}

static void typer_gen_nms_prefix(Typer *tr, Namespace *n) {
	assert(tr->nms != n);
	/* create a C prefix for this namespace */
	const char *prev_prefix = "";
	size_t prev_prefix_len = 0;
	if (tr->nms) {
		prev_prefix = tr->nms->c.prefix;
		assert(prev_prefix);
		prev_prefix_len = strlen(prev_prefix);
	}
	if (n->associated_ident) {
		size_t ident_len = n->associated_ident->len;
		char *prefix = n->c.prefix = typer_malloc(tr, ident_len + prev_prefix_len + 3);
		memcpy(prefix, prev_prefix, prev_prefix_len);
		prefix += prev_prefix_len;
		memcpy(prefix, n->associated_ident->str, ident_len);
		prefix += ident_len;
		*prefix++ = '_';
		*prefix++ = '_';
		*prefix++ = '\0';
	} else {
		size_t bytes = prev_prefix_len + 20;
		char *prefix = n->c.prefix = typer_malloc(tr, bytes);
		snprintf(prefix, bytes, "%sa%lu__", prev_prefix, ++tr->nms_counter);
	}
}

static Status types_expr(Typer *tr, Expression *e) {
	if (e->flags & EXPR_FOUND_TYPE) return true;
	e->flags |= EXPR_FOUND_TYPE; /* even if failed, pretend we found the type */
	if (e->kind == EXPR_VAL) {
		/* can exist, e.g. for null */
		return true; 
	}
	Type *t = &e->type;
	t->flags = TYPE_IS_RESOLVED;
	t->kind = TYPE_UNKNOWN; /* default to unknown type (in the case of an error) */
	switch (e->kind) {
	case EXPR_FN: {
		FnExpr *fn = e->fn;
		if (!type_of_fn(tr, fn, &e->type, 0)) {
			return false;
		}
		if (!(fn->flags & FN_EXPR_FOREIGN) && (fn_has_any_const_params(fn) || fn_type_has_varargs(&e->type.fn))) {
			fn->instances = typer_calloc(tr, 1, sizeof *fn->instances);
			t->flags |= TYPE_IS_RESOLVED; /* pretend this type is resolved, even though its children aren't to fix some assertions */
		} else {
			if (!types_fn(tr, fn, &e->type, NULL)) {
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
	case EXPR_IDENT: {
		Block *b = tr->block;
		String i = e->ident_str;
		char *i_str = i.str;
		size_t i_len = i.len;
		if (str_eq_cstr(i, "_")) {
			err_print(e->where, "You cannot use _ as a variable. It is used for ignoring results of function calls, e.g. _, y := function_which_returns_two_things();");
			return false;
		}
		Identifier final_ident = NULL;
		bool undeclared = true;
		while (1) { /* for each block we are inside... */
			/* @OPTIM: only hash once */
			Identifier translated = ident_get_with_len(b ? &b->idents : tr->globals, i_str, i_len);
			if (translated) {
#if 0
				printf("translated %s from\n", ident_to_str(i));
				print_block_location(i->idents->body);
				printf(" to \n");
				print_block_location(translated->idents->body);
#endif			
				final_ident = translated;
				undeclared = false;
			}
			Use **uses = b ? b->uses : tr->uses;

			Use *previous_use_which_uses_i = NULL;
			arr_foreach(uses, UsePtr, usep) {
				Use *use = *usep;
				Expression *used = &use->expr;
				Identifier translated_use;
				bool was_a_struct = false;
				if (type_is_builtin(&used->type, BUILTIN_NMS)) {
					Value val;
					if (!eval_expr(tr->evalr, used, &val))
						return 0;
					Namespace *nms = val.nms;
					Block *body = &nms->body;
					/* look up identifier in namespace */
					translated_use = ident_get_with_len(&body->idents, i_str, i_len);
				} else {
					/* it's a struct */
					was_a_struct = true;
					Type *struct_type = &used->type;
					if (struct_type->kind == TYPE_PTR)
						struct_type = struct_type->ptr;
					assert(struct_type->kind == TYPE_STRUCT);
					StructDef *struc = struct_type->struc;
					translated_use = ident_get_with_len(&struc->body.idents, i_str, i_len);
				}
				if (translated_use) {
					if (undeclared) {
						previous_use_which_uses_i = use;
						undeclared = false;
						final_ident = translated_use;
					} else {
						char *s = cstr(i_str, i_len);
						err_print(e->where, "Conflicting declarations for identifier %s.", s);
						char *also = "";
						if (previous_use_which_uses_i) {
							/* i was use'd twice */
							info_print(previous_use_which_uses_i->expr.where, "%s was imported by this use statement.", s);
							also = "also ";
						} else {
							/* i was declared then used. */
							info_print(ident_decl_location(translated), "%s was declared here.", s);
						}
						free(s);
						info_print(use->expr.where, "...and %simported by this use statement.", also);
						return false;
					}
					if (was_a_struct) {
						/* change to BINARY_DOT */
						e->kind = EXPR_BINARY_OP;
						e->flags = 0;
						e->binary.op = BINARY_DOT;
						e->binary.lhs = used;
						e->binary.rhs = typer_calloc(tr, 1, sizeof *e->binary.rhs);
						e->binary.rhs->kind = EXPR_IDENT;
						e->binary.rhs->flags = 0;
						e->binary.rhs->ident_str.str = i_str;
						e->binary.rhs->ident_str.len = i_len;
						/* re-type */
						if (!types_expr(tr, e))
							return false;
						return true;
					}
				}
			}
			if (!undeclared) break;
			if (b) {
				b = b->parent;
			} else {
				break;
			}
		}
		if (undeclared) {
			char *s = cstr(e->ident_str.str, e->ident_str.len);
			err_print(e->where, "Undeclared identifier '%s'.", s);
			free(s);
			return false;
		}
		if (b && b->kind == BLOCK_STRUCT) {
			/* this is really necessary if you're trying to access a struct constant from inside a function in the same struct */
			e->kind = EXPR_VAL;
			Declaration *decl = final_ident->decl;
			if (!(decl->flags & DECL_IS_CONST)) {
				/* not sure if this can even happen right now, but might as well have this check here */
				err_print(e->where, "Trying to access non-constant struct member from inside of it. This is not allowed.");
				return false;
			}
			assert(decl->flags & DECL_FOUND_VAL);
			int idx = decl_ident_index(decl, final_ident);
			e->val = *decl_val_at_index(decl, idx);
			e->type = *decl_type_at_index(decl, idx);
			break;
		}
		e->ident = final_ident;
		if (!type_of_ident(tr, e->where, e->ident, t)) {
			return false;
		}
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
	case EXPR_CALL: {
		CallExpr *c = &e->call;
		c->instance = NULL;
		Expression *f = c->fn;
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
		bool has_varargs = f->type.kind == TYPE_FN && fn_type_has_varargs(&f->type.fn);
		
		if (expr_is_definitely_const(f) || type_is_builtin(&f->type, BUILTIN_TYPE) || has_varargs) {
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
				Copier cop = copier_create(tr->allocr, base->struc->body.parent);
				HashTable *table = &base->struc->instances;
				StructDef struc;
				/* @OPTIM: don't copy struct body */
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
				arr_set_len(arg_types, nparams);
				Value *arg_vals = typer_malloc(tr, nparams * sizeof *arg_vals);
				ErrCtx *err_ctx = tr->err_ctx;
				size_t p = 0;
				arr_foreach(struc.params, Declaration, param) {
					Value param_val = {0};
					bool is_tuple = arr_len(param->idents) > 1;
					int ident_idx = 0;
					/* temporarily add this instance to the stack, while we type the decl, in case you, e.g., pass t = float to struct(t::Type, u::t = "hello") */
					arr_add(err_ctx->instance_stack, e->where);
					typer_block_enter(tr, &struc.body);
					bool success = types_decl(tr, param);
					arr_remove_last(err_ctx->instance_stack);
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
							if (!type_eq_implicit(&arg->val.type, type)) {
								char *expected = type_to_str(type),
									*got = type_to_str(&arg->val.type);
								err_print(arg->where, "Wrong struct parameter type. Expected %s, but got %s.", expected, got);
								return false;
							}
							if (!eval_expr(tr->evalr, &arg->val, &ident_val))
								return false;
						}
						if (is_tuple)
							typer_arr_add(tr, param_val.tuple, ident_val);
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
					arr_add(err_ctx->instance_stack, e->where);
					Block *prev_block = tr->block;
					tr->block = &inst->struc.body;
					bool success = type_resolve(tr, &struct_t, e->where); /* resolve the struct */
					tr->block = prev_block;
					arr_remove_last(err_ctx->instance_stack);
					if (!success) return false;
						
					inst->struc.instance_id = table->n;
				}

				
				/* expression is actually a type */
				e->kind = EXPR_TYPE;
				e->typeval = typer_calloc(tr, 1, sizeof *e->typeval);
				e->typeval->kind = TYPE_STRUCT;
				e->typeval->flags = TYPE_IS_RESOLVED;
				e->typeval->struc = &inst->struc;
				t->kind = TYPE_BUILTIN;
				t->builtin = BUILTIN_TYPE;
				arr_clear(arg_types);
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
		size_t narg_exprs = 0;
		bool is_foreign = (fn_decl->flags & FN_EXPR_FOREIGN) != 0;
		I16 *order = NULL;
		if (fn_decl && !is_foreign) {
			if (!call_arg_param_order(fn_decl, &f->type, c->args, e->where, &order)) {
				free(order);
				return false;
			}
		}
		
		size_t nvarargs = 0;
		if (has_varargs) {
			assert(fn_decl);
			/* fn_decl could be foreign, so order could be NULL */
			nvarargs = nargs - (order ? (size_t)order[nparams-1] : nparams-1);
			narg_exprs = nparams-1 + nvarargs;
		} else {
			narg_exprs = nparams;
		}

		arg_exprs = NULL;
		arr_set_lena(arg_exprs, narg_exprs, tr->allocr);
		
		if (fn_decl && !is_foreign) {
			size_t i = 0;
			Declaration *last_param = arr_last_ptr(fn_decl->params);
			arr_foreach(fn_decl->params, Declaration, param) {
				if (has_varargs && param == last_param) continue;
				arr_foreach(param->idents, Identifier, ident) { 
					I16 arg_idx = order[i];
					if (arg_idx == -1) {
						if (param->flags & DECL_HAS_EXPR) {
							arg_exprs[i].kind = EXPR_VAL;
							arg_exprs[i].where = param->where;
							arg_exprs[i].flags = param->expr.flags;
							arg_exprs[i].type = param->type;
							if (has_varargs || f->type.fn.constness) {
								/* param->expr hasn't been typed or evaluated, because we passed type_of_fn a "generic" function */
								/* we actually need to make a copy of this, so that copy_fn_expr still works later */
								Expression default_arg;
								Copier cop = copier_create(tr->allocr, &fn_decl->body);
								copy_expr(&cop, &default_arg, &param->expr);
								if (!types_expr(tr, &default_arg))
									return false;
								if (!eval_expr(tr->evalr, &default_arg, &arg_exprs[i].val))
									return false;
							} else {
								arg_exprs[i].val = param->expr.val;
							}
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
				if (!has_varargs || nargs < nparams-1) {
					err_print(e->where, "Expected %lu arguments to function call, but got %lu.", (unsigned long)nparams, (unsigned long)nargs);
					return false;
				}
			}
			for (size_t p = 0; p < nargs; ++p) {
				if (args[p].name) {
					err_print(args[p].where, "You can only use named arguments if you directly call a function.");
				}
				arg_exprs[p] = args[p].val;
			}
		}
		if (has_varargs) {
			/* deal with varargs (put them at the end of arg_exprs) */
			int idx = order ? order[nparams-1] : (I16)nparams-1;
			assert(idx >= 0);
			Expression *arg_out = &arg_exprs[(int)nparams-1];
			for (; idx < (int)nargs; ++idx) {
				Expression *arg = &args[idx].val;
				if (type_is_builtin(&arg->type, BUILTIN_VARARGS)) {
					/* add each vararg separately */
					assert(arg->kind == EXPR_IDENT);
					Identifier ident = arg->ident;
					Declaration *decl = ident->decl;
					VarArg *varargs_here = decl->val.varargs;
					size_t nvarargs_here = arr_len(varargs_here);
					/* not just += nvarargs-1 to handle nvarargs_here == 0 */
					narg_exprs += nvarargs_here;
					--narg_exprs;
					nvarargs += nvarargs_here;
					--nvarargs;
						
					long arg_out_idx = (long)(arg_out - arg_exprs); /* save and restore arg_out to prevent realloc from causing problems */
					/* add more room (or if nvarargs_here == 0, remove room) for more varargs */
					arr_set_lena(arg_exprs, narg_exprs, tr->allocr);
					arg_out = arg_exprs + arg_out_idx;
					for (size_t i = 0; i < nvarargs_here; ++i) {
						VarArg *vararg = &varargs_here[i];
						Expression *out = arg_out++;
						/* construct varargs_here[i] */
						out->flags = EXPR_FOUND_TYPE;
						out->type = *vararg->type;
						out->where = arg->where;
						out->kind = EXPR_BINARY_OP;
						out->binary.op = BINARY_AT_INDEX;
						Expression *lhs = out->binary.lhs = typer_malloc(tr, sizeof *out->binary.lhs);
						lhs->kind = EXPR_IDENT;
						lhs->flags = EXPR_FOUND_TYPE;
						lhs->type = decl->type;
						lhs->ident = ident;
						lhs->where = arg->where;
						Expression *rhs = out->binary.rhs = typer_malloc(tr, sizeof *out->binary.lhs);
						rhs->kind = EXPR_VAL;
						rhs->flags = EXPR_FOUND_TYPE;
						rhs->type.kind = TYPE_BUILTIN;
						rhs->type.builtin = BUILTIN_I64;
						rhs->type.flags = TYPE_IS_RESOLVED;
						rhs->val.i64 = (I64)i;
						rhs->where = arg->where;
					}
				} else {
					*arg_out++ = *arg;
				}
			}
		}

		FnType *fn_type = &f->type.fn;
		c->arg_exprs = arg_exprs;
		FnExpr *original_fn = NULL;
		FnExpr *fn_copy = NULL;

		if (fn_type->constness || (has_varargs && !is_foreign)) {
			/* eval function, create copy */
			
			
			/* the function had better be a compile time constant if it has constant params */
			Value fn_val = {0};
			if (!eval_expr(tr->evalr, f, &fn_val))
				return false;

			FnExpr *fn = fn_val.fn;
			/* fn is the instance, original_fn is not */
			original_fn = fn;
			fn_copy = typer_malloc(tr, sizeof *fn_copy);
			Copier cop = copier_create(tr->allocr, fn->body.parent);
			copy_fn_expr(&cop, fn_copy, fn, COPY_FN_EXPR_DONT_COPY_BODY);

			if (has_varargs) {
				/* set value of varargs param decl */
				VarArg *varargs = NULL;
				arr_set_lena(varargs, nvarargs, tr->allocr);
				Declaration *varargs_param = arr_last_ptr(fn_copy->params);
				DeclFlags is_const = varargs_param->flags & DECL_IS_CONST;
				varargs_param->val.varargs = varargs;
				for (int v = 0; v < (int)nvarargs; ++v) {
					Expression *arg = &arg_exprs[v+order[nparams-1]];
					VarArg *vararg = &varargs[v];
					if (is_const) {
						Value val;
						if (!eval_expr(tr->evalr, arg, &val))
							return false;
						arg->kind = EXPR_VAL;
						arg->val = val;
						copy_val(tr->allocr, &vararg->val, arg->val, &arg->type);
					}
					vararg->type = &arg->type;
				}
				if (is_const) {
					varargs_param->flags |= DECL_FOUND_VAL;
				}
			}
		}

		if (fn_type->constness) {
			FnExpr *fn = fn_copy;
			/* keep track of the declaration */
			Declaration *param_decl = fn->params;
			size_t ident_idx = 0;
			size_t i = 0;

			Type **arg_types = NULL;
			Type **decl_types = NULL;
			Identifier *inferred_idents = NULL;
			Location *arg_wheres = NULL;

			arr_foreach(fn->params, Declaration, param) {
				arr_foreach(param->idents, Identifier, ident) {
					if (param->flags & DECL_INFER) {
						arr_add(inferred_idents, *ident);
					} else if ((param->flags & DECL_ANNOTATES_TYPE)
							   && !type_is_builtin(&param->type, BUILTIN_VARARGS)) {
						/* add to stuff infer can use */
						/* @OPTIM: don't do this if we're not inferring */
						arr_add(decl_types, &param->type);
						arr_add(arg_types, &arg_exprs[i].type);
						arr_add(arg_wheres, arg_exprs[i].where);
					}
					++i;
				}
			}

			size_t ninferred_idents = arr_len(inferred_idents);
			if (ninferred_idents) {
				Value *inferred_vals = err_malloc(ninferred_idents * sizeof *inferred_vals);
				Type *inferred_types = err_malloc(ninferred_idents * sizeof *inferred_types);
				Block *prev = tr->block;
				tr->block = &fn->body;
				if (!infer_ident_vals(tr, decl_types, arg_types, inferred_idents, inferred_vals, inferred_types, arg_wheres)) {
					tr->block = prev;
					return false;
				}
				tr->block = prev;
				
				
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
			
			arr_clear(inferred_idents);
			arr_clear(arg_types);
			arr_clear(arg_wheres);
			arr_clear(decl_types);

			/* eval compile time arguments */
			for (i = 0; i < nparams; ++i) {
				bool should_be_evald = arg_is_const(&arg_exprs[i], fn_type->constness[i]);
				if (i == nparams-1 && has_varargs) {
					/* handled above */
				} else if (should_be_evald) {
					if (!order || order[i] != -1) {
						Expression *expr = &arg_exprs[i];
						Value arg_val = {0};
						if (!eval_expr(tr->evalr, expr, &arg_val))
							return false;
						Type *type = &expr->type;
						arg_exprs[i].kind = EXPR_VAL;
						arg_exprs[i].flags = EXPR_FOUND_TYPE;
						arg_exprs[i].val = arg_val;
						param_decl->flags |= DECL_FOUND_VAL;
						copy_val(tr->allocr, &param_decl->val, arg_val, type);
						if (!(param_decl->flags & DECL_ANNOTATES_TYPE)) {
							param_decl->type = *type;
						}
					}
				}
				++ident_idx;
				if (ident_idx >= arr_len(param_decl->idents)) {
					ident_idx = 0;
					++param_decl;
				}
			}
		}
		if (fn_type->constness || (has_varargs && !is_foreign)) {
			/* type params, return declarations, etc */
			if (!type_of_fn(tr, fn_copy, &f->type, TYPE_OF_FN_IS_INSTANCE))
				return false;

			if (fn_type->constness) {
				/* deal with default arguments */
				size_t i = 0;
				arr_foreach(fn_copy->params, Declaration, param) {
					arr_foreach(param->idents, Identifier, ident) {
						if (order && order[i] == -1) {
							if (param->flags & DECL_INFER) {
								arg_exprs[i].kind = EXPR_VAL;
								arg_exprs[i].flags = EXPR_FOUND_TYPE;
								arg_exprs[i].type = param_types[i] = param->type;
								arg_exprs[i].val = param->val;
							} else {
								assert(param->flags & DECL_HAS_EXPR);
								assert(param->expr.kind == EXPR_VAL); /* this was done by type_of_fn */
								arg_exprs[i] = param->expr;
								copy_val(tr->allocr, &arg_exprs[i].val, param->expr.val, &param->expr.type);
							}
						}
						++i;
					}	
				}
			}

			ret_type = f->type.fn.types;
			param_types = ret_type + 1;
		}

		
		/* check types of arguments */
		for (size_t p = 0; p < nparams; ++p) {
			if (p != nparams-1 || !has_varargs) {
				Expression *arg = &arg_exprs[p];
				Type *expected = &param_types[p];
				Type *got = &arg->type;
				if (!type_eq_implicit(got, expected)) {
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
		}

		
		if (fn_type->constness || (has_varargs && !is_foreign)) {
			
			Type table_index_type = {0};
			Value table_index = {0};
			
			/* NOTE: we need to keep table_index's memory around because instance_table_add keeps it to compare against. */
			table_index_type.flags = TYPE_IS_RESOLVED;
			table_index_type.kind = TYPE_TUPLE;
			table_index_type.tuple = NULL;
			Type *u64t = typer_arr_add_ptr(tr, table_index_type.tuple);
			construct_resolved_builtin_type(u64t, BUILTIN_U64);
			table_index.tuple = NULL;
			Value *which_are_const_val = typer_arr_add_ptr(tr, table_index.tuple);
			U64 *which_are_const = &which_are_const_val->u64;
			*which_are_const = 0;
			int semi_const_index = 0;
			for (size_t i = 0; i < nparams; ++i) {
				Expression *arg = &arg_exprs[i];
				bool is_const = fn_type->constness && arg_is_const(arg, fn_type->constness[i]);
				bool is_vararg = has_varargs && i == nparams-1;
				Copier cop = copier_create(tr->allocr, tr->block);
				if (is_vararg) {
					/* create one additional table index member for varargs */
					Value *varargs_val = typer_arr_add_ptr(tr, table_index.tuple);
					Type *varargs_type = typer_arr_add_ptr(tr, table_index_type.tuple);
					memset(varargs_type, 0, sizeof *varargs_type);
					varargs_type->flags = TYPE_IS_RESOLVED;
					varargs_type->kind = TYPE_BUILTIN;
					varargs_type->builtin = BUILTIN_VARARGS;
					varargs_val->varargs = NULL;
					for (; i < narg_exprs; ++i) {
						arg = &arg_exprs[i];
						VarArg *varg = typer_arr_add_ptr(tr, varargs_val->varargs);
						varg->type = copy_type_(&cop, &arg->type);
						if (is_const) {
							copy_val(tr->allocr, &varg->val, arg->val, varg->type);
						} else {
							/* use zero value everywhere */
							varg->val = val_zero(varg->type);
						}
					}
				} else if (is_const) {
					assert(arg->kind == EXPR_VAL);
					if (fn_type->constness[i] == CONSTNESS_SEMI) {
						if (semi_const_index >= 64) {
							err_print(f->where, "You can't have more than 64 semi-constant arguments to a function at the moment (sorry).");
							return false;
						}
						*which_are_const |= ((U64)1) << semi_const_index;
						++semi_const_index;
					}
					Value *v = typer_arr_add_ptr(tr, table_index.tuple);
					Type *type = typer_arr_add_ptr(tr, table_index_type.tuple);
					copy_type(&cop, type, &arg->type);
					copy_val(tr->allocr, v, arg->val, type);
				}
			}
			bool instance_already_exists;
			c->instance = instance_table_adda(tr->allocr, original_fn->instances, table_index, &table_index_type, &instance_already_exists);
			if (instance_already_exists) {
				arr_cleara(table_index_type.tuple, tr->allocr);
				arr_cleara(table_index.tuple, tr->allocr);
			} else {
				Copier cop = copier_create(tr->allocr, fn_copy->body.parent);
				copy_block(&cop, &fn_copy->body, &original_fn->body, COPY_BLOCK_DONT_CREATE_IDENTS);
				c->instance->fn = fn_copy;
				/* fix parameter and return types (they were kind of problematic before, because we didn't know about the instance) */
				fn_copy->instance_id = 1+original_fn->instances->n; /* let's help cgen out and assign a non-zero ID to this */
				/* type this instance */
				
				/* if anything happens, make sure we let the user know that this happened while generating a fn */
				ErrCtx *err_ctx = e->where.file->ctx;
				arr_add(err_ctx->instance_stack, e->where);
				Block *prev_block = tr->block;
				tr->block = fn_copy->body.parent;
				bool success = types_fn(tr, c->instance->fn, &f->type, c->instance);
				tr->block = prev_block;
				arr_remove_last(err_ctx->instance_stack);
				if (!success) return false;
			}
			c->fn->kind = EXPR_VAL;
			c->fn->val.fn = c->instance->fn;
			
		}
		free(order);
		*t = *ret_type;
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
			if (streq(builtin_val_names[b], builtin_name)) {
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
		if (!types_expr(tr, of)) return false;
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
			if (!expr_must_lval(of, "take address of")) {
				return false;
			}
			if (of_type->kind == TYPE_TUPLE) {
				/* necessary because x, y (where x and y are variables) is an l-value */
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
		case UNARY_TYPEOF: {
			if (type_is_builtin(&of->type, BUILTIN_VARARGS)) {
				err_print(of->where, "You can't apply typeof to varargs.");
				return false;
			}
			if (of->type.kind == TYPE_TUPLE) {
				err_print(of->where, "You can't apply typeof to a tuple.");
				return false;
			}
			e->kind = EXPR_TYPE;
			e->typeval = &of->type;
			t->kind = TYPE_BUILTIN;
			t->builtin = BUILTIN_TYPE;
		} break;
		case UNARY_DSIZEOF:
		case UNARY_DALIGNOF: {
			Type *queried_type;
			if (!type_is_builtin(&of->type, BUILTIN_TYPE)) {
				char *s = e->unary.op == UNARY_DSIZEOF ? "sizeof" : "alignof";
				err_print(e->where, "Argument of #%s must be a Type. Did you mean #%s(typeof ...)?", s, s);
				return false;
			}
			Value val;
			if (!eval_expr(tr->evalr, of, &val))
				return false;
			queried_type = val.type;
			if (e->unary.op == UNARY_DSIZEOF)
				e->val.i64 = (I64)compiler_sizeof(queried_type);
			else
				e->val.i64 = (I64)compiler_alignof(queried_type);
			e->kind = EXPR_VAL;
			t->kind = TYPE_BUILTIN;
			t->builtin = BUILTIN_I64;
		} break;
		case UNARY_SIZEOF:
		case UNARY_ALIGNOF: {
			/* eval of */
			if (!type_is_builtin(&of->type, BUILTIN_TYPE)) {
				char *s = e->unary.op == UNARY_SIZEOF ? "sizeof" : "alignof";
				err_print(e->where, "Argument of %s must be a Type. Did you mean %s(typeof ...)?", s, s);
				return false;
			}
			Value val;
			if (!eval_expr(tr->evalr, of, &val))
				return false;
			of->kind = EXPR_VAL;
			of->val = val;
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
			bool lhs_success = types_expr(tr, lhs);
			bool rhs_success = types_expr(tr, rhs);
			if (!(lhs_success && rhs_success))
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
			if (!expr_must_lval(e->binary.lhs, "set value of")) {
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
				valid = type_eq_implicit(lhs_type, rhs_type);
			} else {
				/* numerical binary ops */
				if (lhs_type->kind == TYPE_BUILTIN && type_eq_implicit(lhs_type, rhs_type)) {
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
					if (type_eq_implicit(lhs_type, rhs_type)) {
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
					t->kind = TYPE_BUILTIN;
					t->builtin = BUILTIN_VOID;
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
					*t = *overriding_type(lhs_type, rhs_type);
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
				t->kind = TYPE_BUILTIN; t->builtin = BUILTIN_VOID; /* actually, it's just void */
			}
				
			break;
		}
		case BINARY_AND:
		case BINARY_OR: {
			bool success = true;
			if (!type_can_be_truthy(lhs_type)) {
				char *s = type_to_str(lhs_type);
				success = false;
				err_print(lhs->where, "Cannot use operator %s on type %s.", binary_op_to_str(o), s);
				free(s);
			}
			if (!type_can_be_truthy(rhs_type)) {
				char *s = type_to_str(rhs_type);
				success = false;
				err_print(lhs->where, "Cannot use operator %s on type %s.", binary_op_to_str(o), s);
				free(s);
			}
			if (!success) return false;
			t->kind = TYPE_BUILTIN; t->builtin = BUILTIN_BOOL;
		} break;
		case BINARY_AT_INDEX:
			if (type_is_slicechar(rhs_type)) {
				/* switch to BINARY_DOT (point["x"] => point.x) */
				e->binary.op = BINARY_DOT;
				Value val;
				if (!eval_expr(tr->evalr, rhs, &val)) {
					return false;
				}
				rhs->kind = EXPR_IDENT;
				rhs->flags = 0;
				rhs->ident_str.str = val.slice.data;
				rhs->ident_str.len = (size_t)val.slice.len;
				/* re-type with new expression */
				e->flags = (ExprFlags)~(ExprFlags)EXPR_FOUND_TYPE;
				return types_expr(tr, e);
			}
			if (rhs_type->kind != TYPE_BUILTIN || !type_builtin_is_numerical(rhs_type->builtin)) {
				err_print(e->where, "The index of an array must be a builtin numerical type.");
				return false;
			}
			if (lhs_type->kind == TYPE_PTR) {
				lhs_type = lhs_type->ptr;
			}
			switch (lhs_type->kind) {
			case TYPE_ARR:
				*t = *lhs_type->arr.of;
				break;
			case TYPE_SLICE:
				*t = *lhs_type->slice;
				break;
			case TYPE_BUILTIN:
				if (lhs_type->builtin == BUILTIN_VARARGS) {
					assert(lhs->kind == EXPR_IDENT);
					Declaration *decl = lhs->ident->decl;
					assert(decl->flags & DECL_IS_PARAM);
					Value index_val;
					if (!eval_expr(tr->evalr, rhs, &index_val))
						return false;
					/* NOTE: rhs->type was checked above */
					I64 i = val_to_i64(index_val, rhs->type.builtin);
					VarArg *varargs = decl->val.varargs;
					if (i < 0 || i >= (I64)arr_len(varargs)) {
						err_print(e->where, "Index out of bounds for varargs access (index = " I64_FMT ", length = %lu).", i, (unsigned long)arr_len(varargs));
						return 0;
					}
					VarArg *vararg = &varargs[i];
					if (decl->flags & DECL_IS_CONST) {
						/* replace with value */
						e->kind = EXPR_VAL;
						e->type = *vararg->type;
						copy_val(tr->allocr, &e->val, vararg->val, &e->type);
					} else {
						/* just use vararg's type */
						rhs->kind = EXPR_VAL;
						rhs->val.i64 = i;
						rhs->type.builtin = BUILTIN_I64;
						*t = *vararg->type;
					}
					break;
				}
				/* fallthrough */
			default: {
				char *s = type_to_str(lhs_type);
				err_print(e->where, "Cannot subscript type %s", s);
				free(s);
				return false;
			}
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
			if (type_is_builtin(struct_type, BUILTIN_TYPE)) {
				/* accessing struct constant/parameter with a Type */
				Value lval = {0};
				if (!eval_expr(tr->evalr, lhs, &lval))
					return false;
				
				lhs->kind = EXPR_VAL;
				lhs->flags = EXPR_FOUND_TYPE;
				lhs->val = lval;
				Type *struc = lhs->val.type;
				if (struc->kind != TYPE_STRUCT) {
					char *s = type_to_str(struc);
					err_print(lhs->where, "Cannot access member from non-struct type (%s).", s);
					free(s);
					return false;
				}
				if (!struct_resolve(tr, struc->struc)) return false;
				if (!get_struct_constant(struc->struc, rhs->ident_str, e))
					return false;
				break;
			} else if (struct_type->kind == TYPE_STRUCT) {
				StructDef *struc = struct_type->struc;
				if (!struct_resolve(tr, struc)) return false;

				Identifier struct_ident = ident_get_with_len(&struc->body.idents, rhs->ident_str.str, rhs->ident_str.len);
				if (!struct_ident) {
					char *struc_s = get_struct_name(struc);
					char *member_s = str_to_cstr(rhs->ident_str);
					err_print(e->where, "%s is not a member of structure %s.", member_s, struc_s);
					return false;
				}
				UsedFrom *uf = struct_ident->used_from;
				if (uf) {
					/* foo.baz => (foo.bar).baz */
					Expression *old_lhs = lhs;
					lhs = e->binary.lhs = typer_malloc(tr, sizeof *lhs);
					lhs->kind = EXPR_BINARY_OP;
					lhs->flags = 0;
					lhs->binary.op = BINARY_DOT;
					lhs->binary.lhs = old_lhs;
					Expression *middle = lhs->binary.rhs = typer_calloc(tr, 1, sizeof *lhs->binary.rhs);
					middle->kind = EXPR_IDENT;
					middle->flags = 0;
					assert(arr_len(uf->use_decl->idents) == 1);
					middle->ident_str = ident_to_string(uf->use_decl->idents[0]);
					e->flags &= (ExprFlags)~(ExprFlags)EXPR_FOUND_TYPE;
					/* re-type now that we know where it's from */
					return types_expr(tr, e);
				}
				if (struct_ident && !(struct_ident->decl->flags & DECL_IS_CONST)) {
					Field *field = struct_ident->decl->field;
					field += ident_index_in_decl(struct_ident, struct_ident->decl);
					e->binary.field = field;
					*t = *field->type;
				} else {
					if (!get_struct_constant(struct_type->struc, rhs->ident_str, e))
						return false;
				}
				break;
			} else if (struct_type->kind == TYPE_SLICE || struct_type->kind == TYPE_ARR || type_is_builtin(struct_type, BUILTIN_VARARGS)) {
				if (str_eq_cstr(rhs->ident_str, "data") && struct_type->kind == TYPE_SLICE) {
					/* allow access of slice pointer */
					t->kind = TYPE_PTR;
					t->ptr = typer_calloc(tr, 1, sizeof *t->ptr);
					t->ptr->kind = TYPE_BUILTIN;
					t->ptr->builtin = BUILTIN_VOID;
					t->ptr->flags = TYPE_IS_RESOLVED;
					break;
				}
				if (!str_eq_cstr(rhs->ident_str, "len")) {
					char *s = type_to_str(struct_type);
					err_print(rhs->where, "Field of %s must be .len", s);
					free(s);
					return false;
				}
				/* length of slice/arr is i64 */
				t->kind = TYPE_BUILTIN;
				t->builtin = BUILTIN_I64;
				Expression *of = lhs;
				if (type_is_builtin(struct_type, BUILTIN_VARARGS)) {
					/* replace with val */
					assert(of->kind == EXPR_IDENT);
					Identifier ident = of->ident;
					Declaration *decl = ident->decl;
					e->kind = EXPR_VAL;
					e->val.i64 = (I64)arr_len(decl->val.varargs);
				} else if (struct_type->kind == TYPE_ARR) {	
					e->kind = EXPR_VAL;
					e->val.i64 = (I64)struct_type->arr.n;
				}
			} else if (type_is_builtin(struct_type, BUILTIN_NMS)) {
				Value nms_val;
				if (!eval_expr(tr->evalr, lhs, &nms_val))
					return false;
				Namespace *nms = nms_val.nms;
				String str = rhs->ident_str;
				Identifier i = rhs->ident = ident_get_with_len(&nms->body.idents, str.str, str.len);
				if (!i) {
					char *s = cstr(str.str, str.len);
					err_print(e->where, "\"%s\" is not a member of this namespace.", s);
					free(s);
					return false;
				}
				if (!type_of_ident(tr, rhs->where, i, t)) {
					return false;
				}
				e->kind = EXPR_IDENT;
				e->ident = i;
			} else {
				char *s = type_to_str(lhs_type);
				err_print(e->where, "Operator . applied to type %s, which is not a structure or pointer to structure.", s);
				free(s);
				return false;
			}
			if (rhs->kind == EXPR_IDENT)
				assert(!(rhs->flags & EXPR_FOUND_TYPE));
		} break;
		} break;
	} break;
	case EXPR_TUPLE:
		t->kind = TYPE_TUPLE;
		t->tuple = NULL;
		arr_foreach(e->tuple, Expression, x) {
			if (!types_expr(tr, x))
				return false;
			typer_arr_add(tr, t->tuple, x->type);
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
		Type *tval = e->typeval;
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
		Namespace *n = e->nms;
		typer_gen_nms_prefix(tr, n);
		tr->nms = n;
		if (!types_block(tr, &n->body)) {
			tr->nms = prev_nms;
			return false;
		}
		tr->nms = prev_nms;
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_NMS;
	} break;
	case EXPR_VAL:
		assert(0);
		return false;
	}
 ret:
	assert(t->flags & TYPE_IS_RESOLVED);
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

	b->c.break_lbl = 0;
	b->c.cont_lbl = 0;
	
	/* for and fn need to deal with their own useds, because you can use stuff in the header */
	if (b->kind != BLOCK_FOR && b->kind != BLOCK_FN)
		b->uses = NULL;
	
	typer_block_enter(tr, b);
	bool success = true;
	arr_foreach(b->stmts, Statement, s) {
		if (!types_stmt(tr, s)) {
			success = false;
			if (tr->had_include_err) {
				/* stop immediately; prevent too many "undeclared identifier" errors */
				break;
			}
			continue;
		}
	}
	assert(tr->block == b);
	typer_block_exit(tr);
	b->flags |= BLOCK_FOUND_TYPES;
	b->flags &= (BlockFlags)~(BlockFlags)BLOCK_FINDING_TYPES;
	
	return success;
}

static bool is_at_top_level(Typer *tr) {
	for (Block *b = tr->block; b; b = b->parent) {
		if (b && b->kind != BLOCK_NMS) {
			return false;
		}
	}
	return true;
}

static Status types_decl(Typer *tr, Declaration *d) {
	Type *dtype = &d->type;
	if (d->flags & DECL_FOUND_TYPE) return true;
	bool success = true;
	Expression *e = (d->flags & DECL_HAS_EXPR) ? &d->expr : NULL;

	if (d->flags & DECL_INFER) {
		dtype->kind = TYPE_UNKNOWN;
		dtype->flags = 0;
		return true;
	}
	typer_arr_add(tr, tr->in_decls, d);
	if (d->flags & DECL_ANNOTATES_TYPE) {
		/* type supplied */
		if (!type_resolve(tr, dtype, d->where)) {
			success = false;
			goto ret;
		}
	}
	
	size_t n_idents; n_idents = arr_len(d->idents);

	if (e) {
		if (e->kind == EXPR_FN) {
			if (tr->block == NULL || tr->block->kind == BLOCK_NMS) {
				e->fn->c.name = d->idents[0];
			} else if (tr->block->kind == BLOCK_STRUCT && !tr->in_decls /* don't include params */) {
				warn_print(d->where, "This function is in the body of a struct. Are you trying to declare a method, because they don't exist in this language.\n"
					"Try moving the function outside of the struct, otherwise you might run into problems.");
			}
		} else if (e->kind == EXPR_NMS) {
			if (is_at_top_level(tr))
				e->nms->associated_ident = d->idents[0];
		} else if (e->kind == EXPR_TYPE
			&& e->typeval->kind == TYPE_STRUCT
			&& tr->fn == NULL) {
			e->typeval->struc->name = d->idents[0];
		}
		
		if (!types_expr(tr, e)) {
			success = false;
			goto ret;
		}
		assert(d->expr.type.flags & TYPE_IS_RESOLVED);
		if (d->flags & DECL_ANNOTATES_TYPE) {
			if (!type_eq_implicit(&e->type, dtype)) {
				char *decl_type = type_to_str(dtype),
					*expr_type = type_to_str(&e->type);
				err_print(e->where, "Declaration type %s does not match expression type %s.", decl_type, expr_type);
				free(decl_type); free(expr_type);
				success = false;
				goto ret;
			}
		} else {
			if (type_is_void(&e->type)) {
				/* e.g. x := (fn(){})(); */
				err_print(e->where, "Use of void value.");
				success = false;
				goto ret;
			}
			*dtype = e->type;
			dtype->flags &= (TypeFlags)~(TypeFlags)TYPE_IS_FLEXIBLE; /* x := 5; => x is not flexible */
		}
		bool need_value = (d->flags & DECL_IS_CONST) || !tr->block || tr->block->kind == BLOCK_NMS;
		if (need_value) {
			if (!(d->flags & DECL_FOUND_VAL)) {
				Value val;
				if (!eval_expr(tr->evalr, e, &val)) {
					success = false;
					goto ret;
				}
				copy_val(tr->allocr, &d->val, val, dtype);
				d->flags |= DECL_FOUND_VAL;
				if (!(d->flags & DECL_IS_CONST)) {
					/* 
						create a value stack for this declaration so that it can be modified by compile time execution,
						but not permanently (i.e. output will still have old value)
					*/

					Value *copy = typer_malloc(tr, sizeof *copy);
					if (n_idents > 1 && dtype->kind != TYPE_TUPLE) {
						/* actually, make n_idents copies of the value, and put them in a tuple. */
						Value *tuple = copy->tuple = typer_malloc(tr, n_idents * sizeof *copy);
						for (size_t i = 0; i < n_idents; ++i) {
							copy_val(tr->allocr, &tuple[i], val, dtype);
						}
					} else {
						copy_val(tr->allocr, copy, val, dtype);
					}
					typer_arr_add(tr, d->val_stack, copy);
				}
			}
		}
	} else if (!tr->block || tr->block->kind == BLOCK_NMS) {
		/* give global variables without initializers a value stack */
		Value *val = typer_malloc(tr, sizeof *val);
		arr_add(d->val_stack, val);
		if (n_idents > 1 && dtype->kind != TYPE_TUPLE) {
			Value *tuple = val->tuple = typer_malloc(tr, n_idents * sizeof *tuple);
			for (size_t i = 0; i < n_idents; ++i)	
				tuple[i] = val_zero(dtype);
		} else {
			*val = val_zero(dtype);
		}
	}


	if (type_is_compileonly(dtype)) {
		if (!(d->flags & DECL_IS_CONST) && !type_is_builtin(dtype, BUILTIN_VARARGS)) {
			char *s = type_to_str(dtype);
			err_print(d->where, "Declarations with type %s must be constant.", s);
			free(s);
			success = false;
			goto ret;
		}
	}
	
	if (dtype->kind == TYPE_TUPLE) {
		if (n_idents != arr_len(dtype->tuple)) {
			err_print(d->where, "Expected to have %lu things declared in declaration, but got %lu.", (unsigned long)arr_len(dtype->tuple), (unsigned long)n_idents);
			success = false;
			goto ret;
		}
	}
	if (dtype->kind == TYPE_UNKNOWN) {
		if (!d->where.file->ctx->have_errored) /* don't do an error if we haven't already done one, because it might be because of that */
			err_print(d->where, "Can't determine type of declaration.");
		success = false;
		goto ret;
	}
	if (dtype->kind == TYPE_BUILTIN) {
		if (dtype->builtin == BUILTIN_VARARGS && !(d->flags & DECL_IS_PARAM)) {
			err_print(d->where, "Only parameters can be varargs.");
			success = false;
			goto ret;
		} else if (dtype->builtin == BUILTIN_VOID) {
			err_print(d->where, "The type of a declaration can't be void.");
			success = false;
			goto ret;
		}
	}
	if (d->flags & DECL_IS_CONST) {
		if (dtype->kind == TYPE_PTR) {
			err_print(d->where, "You can't have a constant pointer.");
			success = false;
			goto ret;
		}
	}
		
	
	for (int i = 0, len = (int)n_idents; i < len; ++i) {
		Type *t = decl_type_at_index(d, i);
		if (type_is_builtin(t, BUILTIN_TYPE)) {
			if (d->flags & DECL_HAS_EXPR) {
				Value *val = decl_val_at_index(d, i);
				if (val->type->kind == TYPE_STRUCT && val->type->struc->params) {
					/* don't resolve it because it's not really complete */
				} else {
					if (!type_resolve(tr, val->type, d->where)) return false;
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

	if (d->flags & DECL_USE) {
		int idx = 0;
		if (arr_len(d->idents) > 1) {
			err_print(d->where, "Used declarations cannot have more than one identifier (you're trying to use two things at once).");
			return false;
		}
		Identifier i = d->idents[0];
		if (!use_ident(tr, i, decl_type_at_index(d, idx++), d->where))
			return false;
	}

	if (tr->nms && tr->block == &tr->nms->body) {
		arr_foreach(d->idents, Identifier, ident) {
			(*ident)->nms = tr->nms;
		}
	}

	if (d->flags & DECL_EXPORT) {
		if (d->expr.kind == EXPR_FN)
			d->expr.fn->flags |= FN_EXPR_EXPORT;
	}

	if (is_at_top_level(tr)) {
		DeclWithCtx dctx = {d, tr->nms, tr->block};
		typer_arr_add(tr, tr->all_globals, dctx);
	}
	
 ret:
	/* pretend we found the type even if we didn't to prevent too many errors */
	d->flags |= DECL_FOUND_TYPE;
	if (!success) {
		/* use unknown type if we didn't get the type */
		dtype->flags = TYPE_IS_RESOLVED;
		dtype->kind = TYPE_UNKNOWN;
	}
	arr_remove_lasta(tr->in_decls, tr->allocr);
	return success;
}

static Status fix_ident_decls_inline_block(Typer *tr, Statement *stmts) {
	Identifiers *idents = typer_get_idents(tr);
	arr_foreach(stmts, Statement, s) {
		assert(!(s->flags & STMT_TYPED));
		if (s->kind == STMT_DECL) {
			Declaration *d = s->decl;
			arr_foreach(d->idents, Identifier, ident) {
				Identifier i = *ident = ident_translate_forced(*ident, idents);
				if (i->decl) {
					char *istr = ident_to_str(i);
					err_print(d->where, "Redeclaration of identifier %s.", istr);
					info_print(i->decl->where, "%s was also declared here.", istr);
					free(istr);
					return false;
				}
				i->decl = d;
			}
		}
	}
	return true;
}

/* introduce identifiers from stmts into current scope, setting their "nms" field to nms */
static Status include_stmts_link_to_nms(Typer *tr, Namespace *nms, Statement *stmts) {
	Identifiers *idents = typer_get_idents(tr);
	arr_foreach(stmts, Statement, s) {
		if (s->kind == STMT_INLINE_BLOCK) {
			if (!include_stmts_link_to_nms(tr, nms, s->inline_block))
				return false;
		} else if (s->kind == STMT_DECL) {
			Declaration *d = s->decl;
			arr_foreach(d->idents, Identifier, ident) {
				/* @OPTIM: only hash once */
				Identifier preexisting = ident_translate(*ident, idents);
				if (preexisting && preexisting->decl != d) {
					char *istr = ident_to_str(preexisting);
					err_print(d->where, "Redeclaration of identifier %s.", istr);
					info_print(preexisting->decl->where, "%s was first declared here.", istr);
					free(istr);
				}
				Identifier i = ident_translate_forced(*ident, idents);
				i->nms = nms;
				i->decl = d;
			}
		}
	}
	return true;
}

static Status types_stmt(Typer *tr, Statement *s) {
top:
	if (s->flags & STMT_TYPED) return true;
	switch (s->kind) {
	case STMT_EXPR: {
		Expression *e = s->expr;
		if (!types_expr(tr, e)) {
			return false;
		}

		{
			if (e->kind == EXPR_TUPLE) {
				err_print(s->where, "Statement of a tuple is not allowed. Use a semicolon instead of a comma here.");
				return false;
			}
			Type *t = &e->type;
			if (type_is_compileonly(t)) {
				char *str = type_to_str(t);
				warn_print(s->where, "This expression has a compile-only type (%s), so this statement will not actually be outputted in C code.", str);
				free(str);
			}
		}
		if (tr->block == NULL) {
			/* evaluate expression statements at global scope */
			if (e->kind != EXPR_C) {
				if (!eval_stmt(tr->evalr, s))
					return false;
			}
		}
	} break;

	case STMT_FOR: {
		bool in_header = true;{ /* additional block because c++ */
		For *fo = s->for_;
		Declaration *header = &fo->header;
		U32 is_range = fo->flags & FOR_IS_RANGE;
		typer_arr_add(tr, tr->in_decls, header);
		fo->body.uses = NULL;
		typer_block_enter(tr, &fo->body);
		bool annotated_index = true;
		{
			size_t nidents = arr_len(header->idents);
			if (nidents > 2) {
				err_print(header->where, "Expected at most 2 identifiers in for declaration (index and value) but got %lu.", 
					(unsigned long)nidents);
				goto for_fail;
			}
			if (nidents < 2) {
				annotated_index = false;
				assert(nidents == 1);
				/* turn value := arr to value, _ := arr to simplify things */
				typer_arr_add(tr, header->idents, ident_insert_with_len(typer_get_idents(tr), "_", 1));
			}
		}
		
		Type *fo_type_tuple = NULL;
		/* fo_type is (val_type, index_type) */
		arr_set_lena(fo_type_tuple, 2, tr->allocr);
		memset(fo_type_tuple, 0, 2*sizeof *fo_type_tuple);
		Type *val_type = &fo_type_tuple[0];
		Type *index_type = &fo_type_tuple[1];

		if (header->flags & DECL_ANNOTATES_TYPE) {
			Type *header_type = &header->type;
			if (!type_resolve(tr, header_type, header->where))
				goto for_fail;
			if (annotated_index) {
				if (header_type->kind != TYPE_TUPLE || arr_len(header_type->tuple) != 2) {
					char *str = type_to_str(header_type);
					err_print(header->where, "Expected annotated for loop type to be (val_type, index_type), but got %s instead.", str);
					free(str);
					goto for_fail;
				}
				fo_type_tuple = header_type->tuple;
				val_type = &fo_type_tuple[0];
				index_type = &fo_type_tuple[1];
				assert(val_type->flags & TYPE_IS_RESOLVED);
				assert(index_type->flags & TYPE_IS_RESOLVED);
				if (!type_is_int(index_type)) {
					char *str = type_to_str(index_type);
					err_print(header->where, "Expected index type of for loop to be a builtin integer type, but it's %s.", str);
					free(s);
					goto for_fail;
				}
			} else {
				*val_type = header->type;
			}
		}
		Type *fo_type = &header->type;
		fo_type->flags = TYPE_IS_RESOLVED;
		fo_type->kind = TYPE_TUPLE;
		fo_type->tuple = fo_type_tuple;

		assert(fo_type->flags & TYPE_IS_RESOLVED);
		
		if (!index_type->flags) {
			construct_resolved_builtin_type(index_type, BUILTIN_I64);
		}

		if (is_range) {
			if (!types_expr(tr, fo->range.from)) goto for_fail;
			{
				Type *ft = &fo->range.from->type;

				if (ft->kind != TYPE_BUILTIN || !type_builtin_is_numerical(ft->builtin)) {
					char *str = type_to_str(ft);
					err_print(s->where, "from expression of for loop must be a builtin numerical type, not %s", str);
					free(str);
					goto for_fail;
				}
			}
			if (fo->range.step) {
				if (!types_expr(tr, fo->range.step)) goto for_fail;
				Type *st = &fo->range.step->type;
				if (st->kind != TYPE_BUILTIN || !type_builtin_is_numerical(st->builtin)) {
					char *str = type_to_str(st);
					err_print(s->where, "step expression of for loop must be a builtin numerical type, not %s", str);
					free(str);
					goto for_fail;
				}
			}
			if (fo->range.to) {
				if (!types_expr(tr, fo->range.to)) goto for_fail;
				Type *tt = &fo->range.to->type;
				if (tt->kind != TYPE_BUILTIN || !type_builtin_is_numerical(tt->builtin)) {
					char *str = type_to_str(tt);
					err_print(s->where, "to expression of for loop must be a builtin numerical type, not %s", str);
					free(str);
					goto for_fail;
				}
			}

			if (val_type->flags) {
				if (type_eq_implicit(&fo->range.from->type, val_type)) {
					*val_type = *overriding_type(&fo->range.from->type, val_type);
				} else {
					char *exp = type_to_str(val_type);
					char *got = type_to_str(&fo->range.from->type);
					err_print(s->where, "Type of from expression does not match type of for loop. Expected %s, but got %s.", exp, got);
					free(exp); free(got);
					goto for_fail;
				}
			} else {
				*val_type = fo->range.from->type;
			}

			if (fo->range.step) {
				if (type_eq_implicit(&fo->range.step->type, val_type)) {
					*val_type = *overriding_type(&fo->range.step->type, val_type);
				} else {
					char *exp = type_to_str(val_type);
					char *got = type_to_str(&fo->range.step->type);
					err_print(s->where, "Type of step expression does not match type of for loop. Expected %s, but got %s.", exp, got);
					free(exp); free(got);
					goto for_fail;
				}
			}

			if (fo->range.to) {
				if (type_eq_implicit(&fo->range.to->type, val_type)) {
					*val_type = *overriding_type(&fo->range.to->type, val_type);
				} else {
					char *exp = type_to_str(val_type);
					char *got = type_to_str(&fo->range.to->type);
					err_print(s->where, "Type of to expression does not match type of for loop. Expected %s, but got %s.", exp, got);
					free(exp); free(got);
					goto for_fail;
				}
			}
			
			val_type->flags &= (TypeFlags)~(TypeFlags)TYPE_IS_FLEXIBLE;
			
			if (fo->range.step) {
				/* we can't put this above because *val_type might have changed. */
				Value *stepval = typer_malloc(tr, sizeof *fo->range.stepval);
				if (!eval_expr(tr->evalr, fo->range.step, stepval)) {
					info_print(fo->range.step->where, "Note that the step of a for loop must be a compile-time constant.");
					goto for_fail;
				}
				val_cast(stepval, &fo->range.step->type, stepval, val_type);
				fo->range.stepval = stepval;
			}
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
			case TYPE_BUILTIN:
				switch (iter_type->builtin) {
				case BUILTIN_VARARGS: {
					/* exit for body */
					typer_block_exit(tr);
					arr_remove_lasta(tr->in_decls, tr->allocr);
					/* create one block, containing a block for each vararg */
					/* e.g. for x := varargs { total += x; } => { { x := varargs[0]; total += x; } { x := varargs[0]; total += x; } } */
					assert(fo->of->kind == EXPR_IDENT);
					Identifier varargs_ident = fo->of->ident;
					Declaration *idecl = varargs_ident->decl;
					VarArg *varargs = idecl->val.varargs;
					size_t nvarargs = arr_len(varargs);
					/* create surrounding block */
					s->kind = STMT_BLOCK;
					Block *b = s->block = typer_calloc(tr, 1, sizeof *s->block);
					idents_create(&b->idents, tr->allocr, b);
					b->stmts = NULL;
					b->parent = tr->block;
					b->where = s->where;
					arr_set_lena(b->stmts, nvarargs, tr->allocr);
					Statement *stmt = b->stmts;
					size_t nstmts = arr_len(fo->body.stmts);
					Declaration *header_decl = &fo->header;
					
					assert(arr_len(header_decl->idents) == 2);
					Identifier val_ident = header_decl->idents[0];
					Identifier index_ident = header_decl->idents[1];
					bool has_val = !ident_eq_str(val_ident, "_");
					bool has_index = !ident_eq_str(index_ident, "_");
					
					for (size_t i = 0; i < nvarargs; ++i, ++stmt) {
						/* create sub-block #i */
						memset(stmt, 0, sizeof *stmt);
						stmt->kind = STMT_BLOCK;
						Block *sub = stmt->block = typer_calloc(tr, 1, sizeof *sub);
						sub->parent = b;
						idents_create(&sub->idents, tr->allocr, sub);
						sub->stmts = NULL;
						sub->where = s->where;
						size_t total_nstmts = nstmts + has_val + has_index;
						arr_set_lena(sub->stmts, total_nstmts, tr->allocr);
						Copier copier = copier_create(tr->allocr, sub);
						if (has_val) {
							/* @TODO(eventually): don't put a decl in each block, just put one at the start */
							Statement *decl_stmt = &sub->stmts[0];
							decl_stmt->flags = 0;
							decl_stmt->kind = STMT_DECL;
							decl_stmt->where = s->where;

							/* declare value */
							Declaration *decl = decl_stmt->decl = typer_calloc(tr, 1, sizeof *decl);
							decl->where = fo->of->where;
							Identifier ident = ident_translate_forced(val_ident, &sub->idents);
							typer_arr_add(tr, decl->idents, ident);
							ident->decl = decl;
							
							decl->flags |= DECL_HAS_EXPR;
							decl->expr.kind = EXPR_BINARY_OP;
							decl->expr.binary.op = BINARY_AT_INDEX;
							decl->expr.binary.lhs = fo->of;
							decl->expr.where = fo->of->where;
							Expression *index = decl->expr.binary.rhs = typer_calloc(tr, 1, sizeof *decl->expr.binary.rhs);
							index->kind = EXPR_LITERAL_INT;
							index->intl = (U64)i;
							index->where = fo->of->where;
						}
						if (has_index) {
							/* @TODO(eventually): don't put a decl in each block, just put one at the start */
							Statement *decl_stmt = &sub->stmts[has_val];
							decl_stmt->flags = 0;
							decl_stmt->kind = STMT_DECL;
							decl_stmt->where = s->where;

							/* declare value */
							Declaration *decl = decl_stmt->decl = typer_calloc(tr, 1, sizeof *decl);
							decl->where = fo->of->where;
							Identifier ident = ident_translate_forced(index_ident, &sub->idents);
							typer_arr_add(tr, decl->idents, ident);
							ident->decl = decl;
							
							decl->flags |= DECL_HAS_EXPR;
							decl->expr.kind = EXPR_LITERAL_INT;
							decl->expr.intl = (U64)i;
							decl->expr.where = fo->of->where;
						}
						
						size_t start = total_nstmts - nstmts;
						for (size_t idx = start; idx < total_nstmts; ++idx) {
							copy_stmt(&copier, &sub->stmts[idx], &fo->body.stmts[idx-start]);
						}
					}
					goto top;
				}
				default: break;
				}
				/* fallthrough */
			default: {
				if (fo->of->type.kind == TYPE_UNKNOWN && tr->err_ctx->have_errored) {
					/* silently fail */
					goto for_fail;
				}
				char *str = type_to_str(&fo->of->type);
				err_print(s->where, "Cannot iterate over non-array non-slice type %s.", str);
				free(str);
				goto for_fail;
			}
			}
			Type *ptr_type = typer_calloc(tr, 1, sizeof *ptr_type);
			if (uses_ptr) {
				ptr_type->flags = TYPE_IS_RESOLVED;
				ptr_type->kind = TYPE_PTR;
				ptr_type->ptr = iter_type;
				iter_type = ptr_type;
			}
			if (header->flags & DECL_ANNOTATES_TYPE) {
				if (type_eq_implicit(iter_type, val_type)) {
					*val_type = *overriding_type(iter_type, val_type);
				} else {
					char *exp = type_to_str(iter_type);
					char *got = type_to_str(val_type);
					err_print(s->where, "Expected to iterate over type %s, but it was annotated as iterating over type %s.", exp, got);
					free(exp); free(got);
					goto for_fail;
				}
			} else *val_type = *iter_type;
		}
		
		arr_remove_lasta(tr->in_decls, tr->allocr);
		in_header = false;
		
		assert(header->type.flags & TYPE_IS_RESOLVED);
		assert(index_type->flags & TYPE_IS_RESOLVED);
		assert(val_type->flags & TYPE_IS_RESOLVED);

		header->flags |= DECL_FOUND_TYPE;
			
		if (header->flags & DECL_USE) {
			if (ident_eq_str(header->idents[0], "_")) {
				err_print(header->where, "You have to name your for loop variable in order to use it (sorry).");
				goto for_fail;
			}
			if (!use_ident(tr, header->idents[0], val_type, header->where)) {
				goto for_fail;
			}
		}

		typer_block_exit(tr);
		if (!types_block(tr, &fo->body)) goto for_fail;
		
		} break;
		for_fail:
		if (in_header)
			arr_remove_lasta(tr->in_decls, tr->allocr);
		typer_block_exit(tr);
		return false;
	}
	case STMT_IF: {
		If *i = s->if_;
		If *curr = i;
		if (curr->flags & IF_STATIC) {
			/* handle #if */
			while (1) {
				Expression *cond = curr->cond;
				If *next = curr->next_elif;
				Value v;
				if (cond) {
					if (!types_expr(tr, cond))
						return false;
					if (!eval_expr(tr->evalr, cond, &v))
						return false;
				}
				if (!cond || val_truthiness(v, &cond->type)) {
					Block *true_block = &curr->body;
					s->kind = STMT_INLINE_BLOCK;
					s->inline_block = true_block->stmts;
					if (!fix_ident_decls_inline_block(tr, s->inline_block))
						return false;
					/* 
						erase identifiers in old block - we don't want anyone to think that
						stuff is actually declared in the old block.
						this isn't ideal, but oh well
					*/
					idents_create(&true_block->idents, tr->allocr, true_block);
					bool success = true;
					arr_foreach(s->inline_block, Statement, sub) {
						if (!types_stmt(tr, sub)) {
							success = false;
						}
					}
					if (!success) return false;
					goto success;
				}
				if (!next) break;
				curr = next;
			}
			if (s->kind == STMT_IF) {
				/* all conds were false */
				/* empty inline block */
				s->kind = STMT_INLINE_BLOCK;
				s->inline_block = NULL;
			}
			break;
		}

		if (!types_block(tr, &curr->body))
			return false;

		while (1) {
			if (curr->cond) {
				if (!types_expr(tr, curr->cond))
					return false;
				if (!type_can_be_truthy(&curr->cond->type)) {
					char *str = type_to_str(&curr->cond->type);
					err_print(curr->cond->where, "Type %s cannot be the condition of an if statement.", str);
					free(str);
					return false;
				}
			}
			if (curr->next_elif) {
				If *nexti = curr->next_elif;
				if (!types_block(tr, &nexti->body)) {
					return false;
				}
				curr = nexti;
			} else {
				break;
			}
		}
	} break;
	case STMT_BLOCK: {
		Block *b = s->block;
		if (!types_block(tr, b))
			return false;
	} break;
	case STMT_WHILE: {
		While *w = s->while_;
		bool ret = true;
		if (!types_expr(tr, w->cond))
			ret = false;
		if (!types_block(tr, &w->body))
			ret = false;
		if (!ret) return false;
	} break;
	case STMT_DECL:
		if (!types_decl(tr, s->decl)) {
			return false;
		}
		break;
	case STMT_RET: {
		Return *r = s->ret;
		if (!tr->fn) {
			err_print(s->where, "return outside of a function.");
			return false;
		}
	    r->referring_to = &tr->fn->body;
		if (r->flags & RET_HAS_EXPR) {
			if (type_is_void(&tr->fn->ret_type)) {
				err_print(s->where, "Return value in a void function.");
				return false;
			}
			if (tr->fn->ret_decls) {
				err_print(s->where, "Return expression in a function with named return values.");
				return false;
			}
			if (!types_expr(tr, &r->expr))
				return false;
			if (!type_eq_implicit(&tr->fn->ret_type, &r->expr.type)) {
				char *got = type_to_str(&r->expr.type);
				char *expected = type_to_str(&tr->fn->ret_type);
				err_print(s->where, "Returning type %s in function which returns %s.", got, expected);
				return false;
			}
			/* e.g. return #C("3+6"); */
			if (r->expr.type.kind == TYPE_UNKNOWN) {
				r->expr.type = tr->fn->ret_type;
			}
		} else {
			if (!type_is_void(&tr->fn->ret_type) || tr->fn->ret_decls) {
				err_print(s->where, "No return value in non-void function.");
				return false;
			}
		}
	} break;
	case STMT_INCLUDE: {
		Include *inc = s->inc;
		char *filename = eval_expr_as_cstr(tr, &inc->filename, "import filename");
		if (!filename)
			return false;
		Namespace *prev_nms = tr->nms;
		IncludedFile *inc_f = NULL;
		Namespace *inc_nms = NULL; /* non-NULL if this is an include to nms */
		bool success = true;
		if (inc->nms) {
			inc_nms = typer_calloc(tr, 1, sizeof *inc_nms);
			
			Block *body = &inc_nms->body;
			body->kind = BLOCK_NMS;
			body->where = s->where;
			idents_create(&body->idents, tr->allocr, body);
			body->parent = tr->block;

			inc_nms->inc_file = inc_f;
			/* turn #include "foo", bar into bar ::= nms { ... } */
			s->kind = STMT_DECL;
			Declaration *d = s->decl = typer_calloc(tr, 1, sizeof *d);
			d->flags = DECL_FOUND_TYPE | DECL_HAS_EXPR | DECL_IS_CONST | DECL_FOUND_VAL;
			construct_resolved_builtin_type(&d->type, BUILTIN_NMS);
			char *ident_str = inc->nms;
			Identifier i = ident_insert(typer_get_idents(tr), &ident_str);
			if (i->decl) {
				Declaration *d2 = i->decl;
				/* maybe they included it twice into one namespace */
				if ((d2->flags & DECL_HAS_EXPR) && (d2->expr.kind == EXPR_NMS) && 
					(d2->expr.nms->inc_file == inc_f)) {
					/* that's okay; get rid of this declaration */
					s->kind = STMT_INLINE_BLOCK;
					s->inline_block = NULL;
					break;
				} else {
					char *istr = ident_to_str(i);
					err_print(s->where, "Redeclaration of identifier %s.", istr);
					info_print(ident_decl_location(i), "Previous declaration was here.");
					free(istr);
					return false; /* NOT goto inc_fail; */
				}
			}
			typer_arr_add(tr, d->idents, i);
			i->decl = d;
			if (is_at_top_level(tr)) inc_nms->associated_ident = i;
			typer_gen_nms_prefix(tr, inc_nms);

			d->expr.kind = EXPR_NMS;
			d->expr.nms = inc_nms;
			d->expr.flags = EXPR_FOUND_TYPE;
			d->expr.type = d->type;
			d->val.nms = inc_nms;
			d->where = d->expr.where = s->where;

			/* go inside namespace and block (it'll help to be there later on) */
			tr->nms = inc_nms;
			typer_block_enter(tr, &inc_nms->body);
		} else {
			s->kind = STMT_INLINE_BLOCK;
		}

		if (!(inc->flags & INC_FORCED)) {
			size_t filename_len = strlen(filename);
			if (streq(filename, tr->main_file->filename)) {
				err_print(s->where, "Circular #include detected. You can add #force to this #include to force it to be included.");
				success = false; goto nms_done;
			}
			inc_f = str_hash_table_get(&tr->included_files, filename, filename_len);
			if (inc_f) {
				/* has already been included */
				if (inc_f->flags & INC_FILE_INCLUDING) {
					err_print(s->where, "Circular #include detected. You can add #force to this #include to force it to be included.");
					success = false; goto nms_done;
				}
				if (s->kind == STMT_INLINE_BLOCK) s->inline_block = NULL; /* nothing needed here */
				/* just set ident declarations */
				if (!include_stmts_link_to_nms(tr, inc_f->main_nms, inc_f->stmts)) {
					success = false; goto nms_done;
				}
				goto nms_done;
			}
			inc_f = str_hash_table_insert(&tr->included_files, filename, filename_len);
			inc_f->flags |= INC_FILE_INCLUDING;
			inc_f->main_nms = tr->nms;
		}
		{
			char *contents = read_file_contents(tr->allocr, filename, s->where);
			if (!contents) {
				tr->had_include_err = true;
				success = false; goto nms_done;
			}

			Tokenizer tokr;
			tokr_create(&tokr, tr->err_ctx, tr->allocr);
			File *file = typer_calloc(tr, 1, sizeof *file);
			file->filename = filename;
			file->contents = contents;
			file->ctx = tr->err_ctx;
			
			if (!tokenize_file(&tokr, file)) {
				success = false; goto nms_done;
			}
			Parser parser;
			parser_create(&parser, tr->globals, &tokr, tr->allocr);
			parser.block = tr->block;
			ParsedFile parsed_file;
			if (!parse_file(&parser, &parsed_file)) {
				success = false; goto nms_done;
			}
			Statement *stmts_inc = parsed_file.stmts;
			if (inc_f) {
				inc_f->stmts = stmts_inc;
			}
			if (s->kind == STMT_INLINE_BLOCK) s->inline_block = stmts_inc;
			arr_foreach(stmts_inc, Statement, s_incd) {
				if (!types_stmt(tr, s_incd)) {
					success = false; goto nms_done;
				}
			}
			if (inc_nms) {
				inc_nms->body.stmts = stmts_inc;
			}
		}
	nms_done:
		if (inc_nms) {
			tr->nms = prev_nms;
			typer_block_exit(tr);
		}
		if (inc_f) inc_f->flags &= (IncFileFlags)~(IncFileFlags)INC_FILE_INCLUDING;
		if (!success) return false;
	} break;
	case STMT_MESSAGE: {
		Message *m = s->message;
	    char *text = eval_expr_as_cstr(tr, &m->text, "message");
		if (!text)
			return false;
		switch (m->kind) {
		case MESSAGE_INFO:
			info_print(s->where, "%s", text);
			break;
		case MESSAGE_WARN:
			warn_print(s->where, "%s", text);
			break;
		case MESSAGE_ERROR:
			err_print(s->where, "%s", text);
			return false;
		}
	} break;
	case STMT_BREAK:
	case STMT_CONT: {
		/* make sure we are actually in a loop */
		Block *block;
		for (block = tr->block; block; block = block->parent) {
			if (block->kind == BLOCK_FOR || block->kind == BLOCK_WHILE) {
				s->referring_to = block;
				if (s->kind == STMT_BREAK) {
					if (!block->c.break_lbl) {
						block->c.break_lbl = ++tr->lbl_counter;
					}
				} else {	
					assert(s->kind == STMT_CONT);
					if (!block->c.cont_lbl) {
						block->c.cont_lbl = ++tr->lbl_counter;
					}
				}
				break;
			}
		}
		if (!block) {
			err_print(s->where, "%s not in loop.", s->kind == STMT_BREAK ? "break" : "continue");
			return false;
		}
	} break;
	case STMT_DEFER:
		if (tr->block == NULL) {
			err_print(s->where, "You can't defer something at global scope.");
			return false;
		}
		if (!types_stmt(tr, s->defer))
			return false;
		if (s->defer->kind == STMT_DEFER) {
			err_print(s->where, "You can't defer a defer!");
			return false;
		}
		if (s->defer->kind == STMT_DECL) {
			err_print(s->where, "Deferring a declaration doesn't make sense!");
			return false;
		}
		break;
	case STMT_USE: {
		Use *u = s->use;
		Expression *e = &u->expr;
		if (!types_expr(tr, e))
			return false;
		if (!expr_must_usable(tr, e)) {
			return false;
		}
		if (tr->block)
			typer_arr_add(tr, tr->block->uses, u);
		else
			typer_arr_add(tr, tr->uses, u);
	} break;
	case STMT_INLINE_BLOCK:
		assert(0); /* only exists after typing */
		break;
	}
success:
	s->flags |= STMT_TYPED;
	return true;
}

static void typer_create(Typer *tr, Evaluator *ev, ErrCtx *err_ctx, Allocator *allocr, Identifiers *idents, File *main_file) {
	memset(tr, 0, sizeof *tr);
	tr->evalr = ev;
	tr->main_file = main_file;
	tr->err_ctx = err_ctx;
	tr->allocr = allocr;
	tr->globals = idents;
	str_hash_table_create(&tr->included_files, sizeof(IncludedFile), tr->allocr);
}

static Status types_file(Typer *tr, ParsedFile *f) {
	bool ret = true;
	tr->parsed_file = f;
	tr->uses = NULL;
	arr_foreach(f->stmts, Statement, s) {
		if (!types_stmt(tr, s)) {
			if (tr->had_include_err) {
				/* stop immediately; prevent too many "undeclared identifier" errors */
				return false;
			}
			ret = false;
		}
	}
	assert(tr->block == NULL);
	return ret;
}


