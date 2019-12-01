static bool types_stmt(Typer *tr, Statement *s);
static bool types_decl(Typer *tr, Declaration *d);
static bool types_expr(Typer *tr, Expression *e);
static bool types_block(Typer *tr, Block *b);
static bool type_resolve(Typer *tr, Type *t, Location where);

static inline void *typer_malloc(Typer *tr, size_t bytes) {
	return allocr_malloc(tr->allocr, bytes);
}

static inline void *typer_calloc(Typer *tr, size_t n, size_t sz) {
	return allocr_calloc(tr->allocr, n, sz);
}

static inline void *typer_arr_add_(Typer *tr, void **arr, size_t sz) {
	return arr_adda_(arr, sz, tr->allocr);
}

static inline bool type_is_builtin(Type *t, BuiltinType b) {
	return t->kind == TYPE_BUILTIN && t->builtin == b;
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
	case TYPE_TYPE: return true;
	case TYPE_BUILTIN:
		return a->builtin == b->builtin;
	case TYPE_STRUCT: return a->struc == b->struc;
	case TYPE_FN: {
		if (arr_len(a->fn.types) != arr_len(b->fn.types)) return false;
		Type *a_types = a->fn.types, *b_types = b->fn.types;
		Constness *a_constness = a->fn.constness, *b_constness = b->fn.constness;
		for (size_t i = 0; i < arr_len(a->fn.types); i++) {
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
		for (size_t i = 0; i < arr_len(a->tuple); i++) {
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
static bool type_must_eq(Location where, Type *expected, Type *got) {
	if (!type_eq(expected, got)) {
		char *str_ex = type_to_str(expected);
		char *str_got = type_to_str(got);
		err_print(where, "Type mismatch: expected %s, but got %s.", str_ex, str_got);
		return false;
	}
	return true;
}

/* prints an error and returns false if the given expression is not an l-value */
static bool expr_must_lval(Expression *e) {
	/* NOTE: make sure you update eval when you change this */
	switch (e->kind) {
	case EXPR_IDENT: {
		IdentDecl *id_decl = ident_decl(e->ident);
		assert(id_decl);
		if (id_decl->kind == IDECL_DECL) {
			Declaration *d = id_decl->decl;
			if (d->flags & DECL_IS_CONST) {
				char *istr = ident_to_str(e->ident);
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
	case EXPR_CAST:
	case EXPR_NEW:
	case EXPR_FN:
	case EXPR_LITERAL_FLOAT:
	case EXPR_LITERAL_CHAR:
	case EXPR_LITERAL_STR:
	case EXPR_LITERAL_INT:
	case EXPR_LITERAL_BOOL:
	case EXPR_IF:
	case EXPR_WHILE:
	case EXPR_EACH:
	case EXPR_CALL:
	case EXPR_C:
	case EXPR_DALIGNOF:
	case EXPR_DSIZEOF:
	case EXPR_BLOCK:
	case EXPR_SLICE:
	case EXPR_TYPE:
	case EXPR_VAL: {
		err_print(e->where, "Cannot use %s as l-value.", expr_kind_to_str(e->kind));
		return false;
	}
	}
	assert(0);
	return false;
}

enum {
	  TYPE_OF_FN_NO_COPY_EVEN_IF_CONST = 0x01,
};

static bool type_of_fn(Typer *tr, FnExpr *f, Location where, Type *t, U16 flags) {
	t->kind = TYPE_FN;
	t->fn.types = NULL;
	t->fn.constness = NULL; /* OPTIM: constness doesn't need to be a dynamic array */
	bool success = true;
	bool entered_fn = false;
	bool added_param_decls = false;
	FnExpr *prev_fn = tr->fn;
	
	FnExpr fn_copy;
	if (!(flags & TYPE_OF_FN_NO_COPY_EVEN_IF_CONST) && fn_has_any_const_params(f)) {
		Copier cop = copier_create(tr->allocr, tr->block);
		copy_fn_expr(&cop, &fn_copy, f, false);
		f = &fn_copy;
	}
	
	size_t idx = 0;
	bool has_constant_params = false;
	Type *ret_type = typer_arr_add(tr, &t->fn.types);
	if (!fn_enter(f, SCOPE_CHECK_REDECL))
		return false;
	tr->fn = f;
	
	entered_fn = true;
	arr_foreach(f->params, Declaration, decl) {
		if (!types_decl(tr, decl)) return false;
		if (decl->type.kind == TYPE_TUPLE) {
			err_print(decl->where, "Functions can't have tuple parameters.");
			return false;
		}
			
		if (!type_resolve(tr, &decl->type, where))
			return false;
		U32 is_at_all_const = decl->flags & (DECL_IS_CONST | DECL_SEMI_CONST);
		if (is_at_all_const) {
			if (!t->fn.constness) {
				has_constant_params = true;
				for (size_t i = 0; i < idx; i++) {
					*(Constness *)typer_arr_add(tr, &t->fn.constness) = CONSTNESS_NO;
				}
			}
		}
		if (decl->flags & DECL_HAS_EXPR) {
			if (decl->expr.kind != EXPR_VAL) {
				Value val;
				if (!eval_expr(tr->evalr, &decl->expr, &val)) {
					info_print(decl->where, "Was trying to evaluate default arguments (which must be constants!)");
					for (Declaration *p = f->params; p != decl; p++) {
						if (p->flags & DECL_IS_CONST)
							arr_foreach(p->idents, Identifier, ident)
								arr_remove_last(&(*ident)->decls);
					}
					success = false;
					goto ret;
				}
				decl->expr.kind = EXPR_VAL;
				decl->expr.val = val;
			}
		}
		for (size_t i = 0; i < arr_len(decl->idents); i++) {
			Type *param_type = typer_arr_add(tr, &t->fn.types);
			*param_type = decl->type;
			if (has_constant_params) {
				Constness constn;
				if (decl->flags & DECL_IS_CONST) {
					constn = CONSTNESS_YES;
				} else if (decl->flags & DECL_SEMI_CONST) {
					constn = CONSTNESS_SEMI;
				} else {
					constn = CONSTNESS_NO;
				}
				*(Constness *)typer_arr_add(tr, &t->fn.constness) = constn;
			}
			idx++;
		}

		if (decl->flags & DECL_IS_CONST) {
			/* allow constant declarations to be used in other parameters, e.g. fn(x @ int, y := x) */
			arr_foreach(decl->idents, Identifier, ident) {
				ident_add_decl(*ident, decl, &f->body);
			}
		}
	}
	added_param_decls = true;

	
	if (f->ret_decls && f->ret_type.kind == TYPE_VOID /* haven't found return type yet */) {
		/* find return type */
		arr_foreach(f->ret_decls, Declaration, d) {
			if (!types_decl(tr, d))
				return false;
		}
		if (arr_len(f->ret_decls) == 1 && arr_len(f->ret_decls[0].idents) == 1) {
			f->ret_type = f->ret_decls[0].type;
		} else {
			f->ret_type.kind = TYPE_TUPLE;
			f->ret_type.flags = TYPE_IS_RESOLVED;
			f->ret_type.tuple = NULL;
			arr_foreach(f->ret_decls, Declaration, d) {
				arr_foreach(d->idents, Identifier, i) {
					*(Type *)arr_add(&f->ret_type.tuple) = d->type;
				}
			}
		}
	}
	if (!type_resolve(tr, &f->ret_type, where)) {
		success = false;
		goto ret;
	}
	*ret_type = f->ret_type;
    
	
	arr_foreach(f->ret_decls, Declaration, decl) {
		if (!types_decl(tr, decl)) {
			success = false;
			goto ret;
		}
	}
 ret:
	/* cleanup */
	
	if (entered_fn) {
		fn_exit(f);
		tr->fn = prev_fn;
		
		if (added_param_decls) {
			/* remove constant parameter ident decls */
			arr_foreach(f->params, Declaration, param) {
				if (param->flags & DECL_IS_CONST) {
					arr_foreach(param->idents, Identifier, ident)
						arr_remove_last(&(*ident)->decls);
				}
			}
		}
	}
    return success;
}

static bool type_of_ident(Typer *tr, Location where, Identifier i, Type *t) {
	t->flags = 0;
	IdentDecl *decl = ident_decl(i);
	if (!decl) {
		char *s = ident_to_str(i);
		err_print(where, "Undeclared identifier: %s", s);
		free(s);
		return false;
	}
	switch (decl->kind) {
	case IDECL_DECL: {
		Declaration *d = decl->decl;
		bool captured = false;
		if (decl->scope != NULL)
			for (Block *block = tr->block; block && block != decl->scope; block = block->parent) {
				if (block->flags & BLOCK_IS_FN) {
					captured = true;
					break;
				}
			}
		if (captured && !(d->flags & DECL_IS_CONST)) {
			err_print(where, "Variables cannot be captured into inner functions (but constants can).");
			return false;
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
				if (!type_of_fn(tr, &d->expr.fn, d->expr.where, t, 0)) return false;
				return true;
			} else {
				if (location_after(d->where, where)) {
					char *s = ident_to_str(i);
					err_print(where, "Use of identifier %s before its declaration.\nNote that it is only possible to use a constant function before it is directly declared (e.g. x @= fn() {}).", s);
					info_print(d->where, "%s will be declared here.", s);
					free(s);
				} else {
					/* let's type the declaration, and redo this (for evaling future functions) */
					if (!types_decl(tr, d)) return false;
					return type_of_ident(tr, where, i, t);
				}
				return false;
			}
		}
	} break;
	case IDECL_EXPR: {
		Expression *e = decl->expr;
		/* are we inside this expression? */
		typedef Expression *ExpressionPtr;
		arr_foreach(tr->in_expr_decls, ExpressionPtr, in_e) {
			if (*in_e == e) {
				char *s = ident_to_str(i);
				err_print(where, "Use of identifier %s in its own declaration.", s);
				free(s);
				return false;
			}
		}
		
		switch (e->kind) {
		case EXPR_EACH:
			if (i == e->each.index) {
				t->kind = TYPE_BUILTIN;
				t->builtin = BUILTIN_I64;
			} else {
				assert(i == e->each.value);
				*t = e->each.type;
			}
			break;
		default: assert(0); return false;
		}
	} break;
	}
	return true;
}

/* fixes the type (replaces [5+3]int with [8]int, etc.) */
static bool type_resolve(Typer *tr, Type *t, Location where) {
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
				err_print(t->arr.n_expr->where, "Negative array length (" INTEGER_FMT ")", ssize);
				return false;
			}
			size = (U64)ssize;
		} else {
			size = val_to_u64(&val, n_expr->type.builtin);
		}
		t->arr.n = (UInteger)size;
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
	case TYPE_STRUCT:
		arr_foreach(t->struc->fields, Field, f) {
			if (!type_resolve(tr, f->type, where))
				return false;
		}
		break;
	case TYPE_EXPR: {
		Value typeval;
		if (!types_expr(tr, t->expr))
			return false;
		t->was_expr = t->expr;
		if (!eval_expr(tr->evalr, t->expr, &typeval))
			return false;
		*t = *typeval.type;
		assert(t->flags & TYPE_IS_RESOLVED);
	} break;
	case TYPE_UNKNOWN:
	case TYPE_VOID:
	case TYPE_TYPE:
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
	case TYPE_VOID:
	case TYPE_TUPLE:
	case TYPE_ARR:
	case TYPE_TYPE:
	case TYPE_STRUCT:
		return false;
	case TYPE_FN:
	case TYPE_UNKNOWN:
	case TYPE_BUILTIN:
	case TYPE_PTR:
	case TYPE_SLICE:
		return true;
	case TYPE_EXPR:
		break;
	}
	assert(0);
	return false;
}

typedef enum {
			  STATUS_NONE,
			  STATUS_WARN,
			  STATUS_ERR
} Status;

static Status type_cast_status(Type *from, Type *to) {
	assert(from->flags & TYPE_IS_RESOLVED);
	assert(to->flags & TYPE_IS_RESOLVED);
	
	if (to->kind == TYPE_UNKNOWN)
		return STATUS_NONE;
	switch (from->kind) {
	case TYPE_UNKNOWN: return STATUS_NONE;
	case TYPE_STRUCT:
	case TYPE_TYPE:
	case TYPE_VOID:
		return STATUS_ERR;
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
			case TYPE_UNKNOWN:
				return STATUS_NONE;
			case TYPE_PTR:
				return STATUS_WARN;
			case TYPE_FN:
			case TYPE_TYPE:
			case TYPE_TUPLE:
			case TYPE_SLICE:
			case TYPE_STRUCT:
			case TYPE_ARR:
			case TYPE_VOID:
				return STATUS_ERR;
			case TYPE_EXPR:
				assert(0);
			}
			break;
		case BUILTIN_F32:
		case BUILTIN_F64:
			if (to->kind == TYPE_BUILTIN && to->builtin != BUILTIN_CHAR)
				return STATUS_NONE;
			return STATUS_ERR;
		case BUILTIN_CHAR:
			if (to->kind == TYPE_BUILTIN && type_builtin_is_int(to->builtin))
				return STATUS_NONE;
			return STATUS_ERR;
		case BUILTIN_BOOL:
			return type_can_be_truthy(to) ? STATUS_NONE : STATUS_ERR;
		}
		break;
	case TYPE_TUPLE: return STATUS_ERR;
	case TYPE_FN:
		if (to->kind == TYPE_PTR || to->kind == TYPE_FN)
			return STATUS_WARN;
		return STATUS_ERR;
	case TYPE_PTR:
		if (to->kind == TYPE_BUILTIN && type_builtin_is_int(to->builtin))
			return STATUS_WARN;
		if (to->kind == TYPE_PTR)
			return STATUS_NONE;
		if (to->kind == TYPE_FN)
			return STATUS_WARN;
		/* TODO: Cast from ptr to arr */
		return STATUS_ERR;
	case TYPE_ARR:
		return STATUS_ERR;
	case TYPE_SLICE:
		if (to->kind == TYPE_PTR && type_eq(from->slice, to->ptr))
			return STATUS_NONE;
	    return STATUS_ERR;
	case TYPE_EXPR:
		break;
	}
	assert(0);
    return STATUS_ERR;
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
static bool types_fn(Typer *tr, FnExpr *f, Type *t, Location where,
					 Instance *instance) {
	FnExpr *prev_fn = tr->fn;
	bool success = true;
	ErrCtx *err_ctx = where.ctx;
	bool entered_fn = false;
	assert(t->kind == TYPE_FN);
	if (instance) {
		*(Location *)typer_arr_add(tr, &err_ctx->instance_stack) = where;
		f = &instance->fn;
	} else {
		if (t->fn.constness)
			return true; /* don't type function body yet; we need to do that for every instance */
	}
	
	tr->fn = f;
	if (!fn_enter(f, SCOPE_CHECK_REDECL)) {
		success = false;
		goto ret;
	}
	entered_fn = true;
	if (!types_block(tr, &f->body)) {
		success = false;
		goto ret;
	}
	Expression *ret_expr = f->body.ret_expr;
	Type *ret_type = t->fn.types;
	bool has_named_ret_vals = f->ret_decls != NULL;
	if (ret_expr) {
		if (!type_eq(ret_type, &ret_expr->type)) {
			char *got = type_to_str(&ret_expr->type);
			char *expected = type_to_str(ret_type);
			err_print(ret_expr->where, "Returning type %s, but function returns type %s.", got, expected);
			if (!instance) /* where will only actually be at the function declaration if it isn't
							  an instance. otherwise, where will be at the calling site, which will already be
							  printed */
				info_print(where, "Function declaration is here.");
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
		err_print(f->body.end, "No return value in function which returns %s.", expected);
		free(expected);
		info_print(where, "Function was declared here:");
		success = false;
		goto ret;
	}
 ret:
	if (entered_fn)
		fn_exit(f);
	if (instance)
		arr_remove_last(&err_ctx->instance_stack);
	tr->fn = prev_fn;
	return success;
}

static bool types_expr(Typer *tr, Expression *e) {
	if (e->flags & EXPR_FOUND_TYPE) return true;
	Type *t = &e->type;
	t->flags = 0;
	t->was_expr = NULL;
	t->kind = TYPE_UNKNOWN; /* default to unknown type (in the case of an error) */
	e->flags |= EXPR_FOUND_TYPE; /* even if failed, pretend we found the type */
	switch (e->kind) {
	case EXPR_FN: {
		if (!type_of_fn(tr, &e->fn, e->where, &e->type, 0))
			return false;
		if (fn_has_any_const_params(&e->fn)) {
			HashTable z = {0};
			e->fn.instances = z;
		} else {
			if (!types_fn(tr, &e->fn, &e->type, e->where, NULL))
				return false;
		}
	} break;
	case EXPR_LITERAL_INT:
	    t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_I64;
		t->flags |= TYPE_IS_FLEXIBLE | TYPE_IS_RESOLVED;
		break;
	case EXPR_LITERAL_STR:
		t->kind = TYPE_SLICE;
		t->slice = typer_malloc(tr, sizeof *t->slice);
		t->slice->flags = TYPE_IS_RESOLVED;
		t->slice->was_expr = NULL;
		t->slice->kind = TYPE_BUILTIN;
		t->slice->builtin = BUILTIN_CHAR;
		t->flags |= TYPE_IS_RESOLVED;
		break;
	case EXPR_LITERAL_FLOAT:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_F32;
		t->flags |= TYPE_IS_FLEXIBLE | TYPE_IS_RESOLVED;
		break;
	case EXPR_LITERAL_BOOL:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_BOOL;
		t->flags |= TYPE_IS_RESOLVED;
		break;
	case EXPR_LITERAL_CHAR:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_CHAR;
		t->flags |= TYPE_IS_RESOLVED;
		break;
	case EXPR_EACH: {
		EachExpr *ea = &e->each;
		*(Expression **)arr_add(&tr->in_expr_decls) = e;
		if (!each_enter(e)) return false;
		if (ea->flags & EACH_IS_RANGE) {
			/* TODO: allow user-defined numerical types */
			if (!types_expr(tr, ea->range.from)) return false;
			{
				Type *ft = &ea->range.from->type;
				if (ft->kind != TYPE_BUILTIN || !type_builtin_is_numerical(ft->builtin)) {
					char *s = type_to_str(ft);
					err_print(e->where, "from expression of each must be a builtin numerical type, not %s", s);
					free(s);
				}
			}
			if (ea->range.step) {
				if (!types_expr(tr, ea->range.step)) return false;
				Type *st = &ea->range.step->type;
				if (st->kind != TYPE_BUILTIN || !type_builtin_is_numerical(st->builtin)) {
					char *s = type_to_str(st);
					err_print(e->where, "step expression of each must be a builtin numerical type, not %s", s);
					free(s);
				}
			}
			if (ea->range.to) {
				if (!types_expr(tr, ea->range.to)) return false;
				Type *tt = &ea->range.to->type;
				if (tt->kind != TYPE_BUILTIN || !type_builtin_is_numerical(tt->builtin)) {
					char *s = type_to_str(tt);
					err_print(e->where, "to expression of each must be a builtin numerical type, not %s", s);
					free(s);
				}
			}

			if (!(ea->flags & EACH_ANNOTATED_TYPE)) {
			    ea->type = ea->range.from->type;
			}
			
			if (!type_eq(&ea->type, &ea->range.from->type)) {
				char *exp = type_to_str(&ea->type);
				char *got = type_to_str(&ea->range.from->type);
				err_print(e->where, "Type of each does not match the type of the from expression. Expected %s, but got %s.", exp, got);
				free(exp); free(got);
				return false;
			}
			
			if (ea->range.step && !type_eq(&ea->type, &ea->range.step->type)) {
				char *exp = type_to_str(&ea->type);
				char *got = type_to_str(&ea->range.step->type);
				err_print(e->where, "Type of each does not match the type of the step expression. Expected %s, but got %s.", exp, got);
				free(exp); free(got);
				return false;
			}
			
			if ((ea->type.flags & TYPE_IS_FLEXIBLE) && ea->range.step)
				ea->type = ea->range.step->type;
			
			if (ea->range.to && !type_eq(&ea->type, &ea->range.to->type)) {
				char *exp = type_to_str(&ea->type);
				char *got = type_to_str(&ea->range.to->type);
				err_print(e->where, "Type of each does not match the type of the to expression. Expected %s, but got %s.", exp, got);
				free(exp); free(got);
				return false;
			}
			
			if ((ea->type.flags & TYPE_IS_FLEXIBLE) && ea->range.to)
				ea->type = ea->range.to->type;
			
		} else {
			if (!types_expr(tr, ea->of))
				return false;
			Type *iter_type = &ea->of->type;

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
				char *s = type_to_str(&ea->of->type);
				err_print(e->where, "Cannot iterate over non-array non-slice type %s.", s);
				free(s);
				return false;
			}
			}
			Type ptr_type = {0};
			if (uses_ptr) {
				ptr_type.flags = TYPE_IS_RESOLVED;
				ptr_type.kind = TYPE_PTR;
				ptr_type.ptr = iter_type;
				iter_type = &ptr_type;
			}
			if (ea->flags & EACH_ANNOTATED_TYPE) {
				if (!type_eq(iter_type, &ea->type)) {
					char *exp = type_to_str(iter_type);
					char *got = type_to_str(&ea->type);
					err_print(e->where, "Expected to iterate over type %s, but it was annotated as iterating over type %s.");
					free(exp); free(got);
					return false;
				}
			} else ea->type = *iter_type;
		}
		if ((ea->flags & EACH_IS_RANGE) && ea->range.step) {
			Value *stepval = typer_malloc(tr, sizeof *ea->range.stepval);
			if (!eval_expr(tr->evalr, ea->range.step, stepval)) {
				info_print(ea->range.step->where, "Note that the step of an each loop must be a compile-time constant.");
				return false;
			}
			val_cast(stepval, &ea->range.step->type, stepval, &ea->type);
			ea->range.stepval = stepval;
		}
		
		arr_remove_last(&tr->in_expr_decls);
		
		if (!types_block(tr, &ea->body)) return false;
		each_exit(e);
		
		if (ea->body.ret_expr) {
			*t = ea->body.ret_expr->type;
		} else {
			t->kind = TYPE_VOID;
			t->flags |= TYPE_IS_RESOLVED;
		}
	} break;
	case EXPR_IDENT: {
		if (!type_of_ident(tr, e->where, e->ident, t)) return false;
	} break;
	case EXPR_CAST: {
		CastExpr *c = &e->cast;
		if (!types_expr(tr, c->expr))
			return false;
		if (!type_resolve(tr, &c->type, e->where))
			return false;
		Status status = type_cast_status(&c->expr->type, &c->type);
		if (status != STATUS_NONE) {
			char *from = type_to_str(&c->expr->type);
			char *to = type_to_str(&c->type);
			if (status == STATUS_ERR)

				err_print(e->where, "Cannot cast from type %s to %s.", from, to);
			else
				warn_print(e->where, "Casting from type %s to %s.", from, to);
			free(from);
			free(to);
			if (status == STATUS_ERR)
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
		IfExpr *i = &e->if_;
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
				IfExpr *nexti = &curr->next_elif->if_;
				Type *next_type = &curr->next_elif->type;
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
		WhileExpr *w = &e->while_;
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
		if (f->type.kind != TYPE_FN) {
			char *type = type_to_str(&f->type);
			err_print(e->where, "Calling non-function (type %s).", type);
			return false;
		}
		Type *ret_type = f->type.fn.types;
		Type *param_types = ret_type + 1;
		Argument *args = c->args;
		size_t nparams = arr_len(f->type.fn.types) - 1;
		size_t nargs = arr_len(c->args);
		bool ret = true;
		Expression *new_args = NULL;
		arr_set_lena(&new_args, nparams, tr->allocr);
		bool *params_set = nparams ? typer_calloc(tr, nparams, sizeof *params_set) : NULL;
		if (f->kind == EXPR_IDENT) {
			IdentDecl *decl = ident_decl(f->ident);
			assert(decl);
			if (decl->kind == IDECL_DECL) {
				if (decl->decl->flags & DECL_HAS_EXPR) {
					Expression *expr = &decl->decl->expr;
					if (expr->kind == EXPR_FN)
						fn_decl = &decl->decl->expr.fn;
				}
			}
		}
		if (!fn_decl && nargs != nparams) {
			err_print(e->where, "Expected %lu arguments to function call, but got %lu.", (unsigned long)nparams, (unsigned long)nargs);
			return false;
		}
		bool had_named_arg = false;
		for (size_t p = 0; p < nargs; p++) {
			if (args[p].name) {
				if (!fn_decl) {
					err_print(args[p].where, "You must call a function directly by its name to use named arguments.");
					return false;
				}
				had_named_arg = true;
				long index = 0;
				long arg_index = -1;
				arr_foreach(fn_decl->params, Declaration, param) {
					arr_foreach(param->idents, Identifier, ident) {
						if (*ident == args[p].name) {
							arg_index = index;
							break;
						}
						index++;
					}
					if (arg_index != -1) break;
				}
				if (arg_index == -1) {
					char *s = ident_to_str(args[p].name);
					err_print(args[p].where, "Argument '%s' does not appear in declaration of function.", s);
					free(s);
					info_print(idecl_where(ident_decl(f->ident)), "Declaration is here.");
					return false;
				}
				new_args[arg_index] = args[p].val;
				params_set[arg_index] = true;
				continue;
			}
			if (had_named_arg) {
				err_print(args[p].where, "Unnamed argument after named argument.");
				return false;
			}
			new_args[p] = args[p].val;
			/* we will check the type of the argument later */
			params_set[p] = true;
		}
		if (!ret) return false;

		FnType *fn_type = &f->type.fn;
		for (size_t i = 0; i < nparams; i++) {
			if (!params_set[i]) {
				size_t index = 0;
				assert(fn_decl); /* we can only miss an arg if we're using named/optional args */
				
				arr_foreach(fn_decl->params, Declaration, param) {
					bool is_required = !(param->flags & DECL_HAS_EXPR);
					int ident_idx = 0;
					assert(param->type.kind != TYPE_TUPLE);

					arr_foreach(param->idents, Identifier, ident) {
						if (index == i) {
							if (is_required) {
								char *s = ident_to_str(*ident);
								err_print(e->where, "Argument %lu (%s) not set in function call.", 1+(unsigned long)i, s);
								free(s);
								return false;
							} else {
								Value default_val;
								if (fn_type->constness) {
									/* TODO: evaluate once per decl, not once per ident */
									Expression copy;
									/* make a copy of the default argument, and type and evaluate it. */
									Copier cop = copier_create(tr->allocr, tr->block);
									copy_expr(&cop, &copy, &param->expr);
									if (!types_expr(tr, &copy))
										return false;
									if (!eval_expr(tr->evalr, &copy, &default_val))
										return false;
									assert(copy.type.kind != TYPE_TUPLE);
									new_args[i].kind = EXPR_VAL;
									new_args[i].flags = copy.flags;
									new_args[i].type = copy.type;
									
									copy_val(tr->allocr, &new_args[i].val,
											 &default_val, &new_args[i].type);
								} else {
									/* it's already been evaluated */
									assert(param->expr.kind == EXPR_VAL); /* evaluated in type_of_fn */
									new_args[i].kind = EXPR_VAL;
									new_args[i].flags = param->expr.flags;
									new_args[i].type = param->type;
									new_args[i].val = param->expr.val;
								}
							}
						}
						ident_idx++;
						index++;
					}
				}
			}
		}
		if (fn_type->constness) {
			/* evaluate compile-time arguments + add an instance */
			
			/* the function had better be a compile time constant if it has constant params */
			Value fn_val = {0};
			if (!eval_expr(tr->evalr, f, &fn_val))
				return false;

			FnExpr *fn = fn_val.fn;
			/* fn is the instance, original_fn is not */
			FnExpr *original_fn = fn;
			FnExpr fn_copy;
			Copier cop = copier_create(tr->allocr, tr->block);
			/* TODO: somehow don't do all of this if we've already generated this instance */
			copy_fn_expr(&cop, &fn_copy, fn, true);
			fn = &fn_copy;
			
			Type table_index_type = {0};
			table_index_type.flags = TYPE_IS_RESOLVED;
			table_index_type.kind = TYPE_TUPLE;
			table_index_type.tuple = NULL;
			Type *u64t = arr_add(&table_index_type.tuple);
			u64t->was_expr = NULL;
			u64t->flags = TYPE_IS_RESOLVED;
			u64t->kind = TYPE_BUILTIN;
			u64t->builtin = BUILTIN_U64;
			Value table_index;
			table_index.tuple = NULL;
			/* we need to keep table_index's memory around because instance_table_add makes a copy of it to compare against. */
		    Value *which_are_const_val = typer_arr_add(tr, &table_index.tuple);
			U64 *which_are_const = &which_are_const_val->u64;
			*which_are_const = 0;
			int semi_const_index = 0;
			/* keep track of the declaration so we can add values */
			Declaration *param_decl = fn->params;
			size_t ident_idx = 0;
			for (size_t i = 0; i < arr_len(fn_type->types)-1; i++) {
				bool should_be_evald = arg_is_const(&new_args[i], fn_type->constness[i]);
				if (should_be_evald) {
					Value *arg_val = typer_arr_add(tr, &table_index.tuple);

					if (!eval_expr(tr->evalr, &new_args[i], arg_val)) {
						if (tr->evalr->enabled) {
							info_print(new_args[i].where, "(error occured while trying to evaluate compile-time argument, argument #%lu)", 1+(unsigned long)i);
						}
						return false;
					}

					Type *type = arr_add(&table_index_type.tuple);
					*type = fn_type->types[i+1];
					/* we need to check the type here so copy_val doesn't mess up */
					Type *expected = type;
					Type *got = &new_args[i].type;
					if (!type_eq(type, &new_args[i].type)) {
						char *estr = type_to_str(expected);
						char *gstr = type_to_str(got);
						err_print(new_args[i].where, "Expected type %s as %lu%s argument to function, but got %s.", estr, 1+(unsigned long)i, ordinals(1+i), gstr);
						return false;
					}
				
					new_args[i].kind = EXPR_VAL;
					new_args[i].flags = EXPR_FOUND_TYPE;
					copy_val(tr->allocr, &new_args[i].val, arg_val, type);
					new_args[i].val = *arg_val;
					new_args[i].type = *type;
					copy_val(tr->allocr, &param_decl->val, arg_val, type);
					param_decl->flags |= DECL_FOUND_VAL;

					if (fn_type->constness[i] == CONSTNESS_SEMI) {
						if (semi_const_index >= 64) {
							err_print(new_args[i].where, "You can't have more than 64 semi-constant arguments to a function at the moment (sorry).");
							return false;
						}
						*which_are_const |= ((U64)1) << semi_const_index;
					}
				}
				if (fn_type->constness[i] == CONSTNESS_SEMI) {
					semi_const_index++;
				}
				ident_idx++;
				if (ident_idx >= arr_len(param_decl->idents)) {
					ident_idx = 0;
					param_decl++;
				}
			}

			bool instance_already_exists;
			c->instance = instance_table_adda(tr->allocr, &original_fn->instances, table_index, &table_index_type, &instance_already_exists);
			if (!instance_already_exists) {
				c->instance->fn = fn_copy;
				/* type param declarations, etc */
				if (!type_of_fn(tr, &c->instance->fn, e->where, &f->type, TYPE_OF_FN_NO_COPY_EVEN_IF_CONST))
					return false;
				/* fix parameter and return types (they were kind of problematic before, because we didn't know about the instance) */
				ret_type = f->type.fn.types;
				param_types = ret_type + 1;
				c->instance->c.id = original_fn->instances.n; /* let's help cgen out and assign an ID to this */
				/* type this instance */
				if (!types_fn(tr, fn, &f->type, e->where, c->instance))
					return false;
				arr_clear(&table_index_type.tuple);
			}
		}

		/* check types of arguments */
		for (size_t p = 0; p < nparams; p++) {
			Expression *arg = &new_args[p];
			Type *expected = &param_types[p];
			Type *got = &arg->type;
			if (!type_eq(expected, got)) {
				ret = false;
				char *estr = type_to_str(expected);
				char *gstr = type_to_str(got);
				err_print(arg->where, "Expected type %s as %lu%s argument to function, but got %s.", estr, 1+(unsigned long)p, ordinals(1+p), gstr);
			}
		}
		if (!ret) return false;
		*t = *ret_type;
		c->arg_exprs = new_args;
		break;
	}
	case EXPR_BLOCK: {
		Block *b = &e->block;
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
		if (code->type.kind != TYPE_SLICE
			|| !type_is_builtin(code->type.slice, BUILTIN_CHAR)) {
			char *s = type_to_str(&code->type);
			err_print(e->where, "Argument to #C directive must be a string, but got type %s.");
			free(s);
			return false;
		}
		t->kind = TYPE_UNKNOWN;
	} break;
	case EXPR_DSIZEOF: {
		if (!types_expr(tr, e->dsizeof.of))
			return false;
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_I64;
	} break;
	case EXPR_DALIGNOF: {
		if (!types_expr(tr, e->dalignof.of))
			return false;
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_I64;
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
			if (of_type->kind == TYPE_TYPE) {
				/* oh it's a type! */
				t->kind = TYPE_TYPE;
				break;
			}
			if (!expr_must_lval(of)) {
				err_print(e->where, "Cannot take address of non-lvalue."); /* FEATURE: better err */
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
			if (!expr_must_lval(e->binary.lhs)) {
				return false;
			}
			/* fallthrough */
		case BINARY_ADD:
		case BINARY_SUB:
		case BINARY_MUL:
		case BINARY_DIV:
		case BINARY_LT:
		case BINARY_GT:
		case BINARY_LE:
		case BINARY_GE:
		case BINARY_EQ:
		case BINARY_NE: {
			bool valid = false;
			if (o == BINARY_SET) {
				valid = type_eq(lhs_type, rhs_type);
				if (lhs_type->kind == TYPE_TYPE) {
					err_print(e->where, "Cannot set type.");
					return false;
				}
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
			/* TODO(eventually): support non-builtin numerical (or even perhaps non-numerical) indices */
			if (rhs_type->kind != TYPE_BUILTIN || !type_builtin_is_numerical(rhs_type->builtin)) {
				err_print(e->where, "The index of an array must be a builtin numerical type.");
				return false;
			}
			switch (lhs_type->kind) {
			case TYPE_ARR:
				*t = *lhs_type->arr.of;
				break;
			case TYPE_SLICE:
				*t = *lhs_type->slice;
				break;
			default: {
		    	char *s = type_to_str(lhs_type);
				err_print(e->where, "Trying to take index of non-array type %s.", s);
				free(s);
				return false;
			}
			}
			break;
		case BINARY_DOT: {
			if (!types_expr(tr, lhs)) return false;
			Type *struct_type = lhs_type;
			if (struct_type->kind == TYPE_PTR)
				struct_type = struct_type->ptr;
			
			if (struct_type->kind == TYPE_STRUCT) {
				bool is_field = false;
				if (rhs->kind == EXPR_IDENT) {
					/* maybe accessing a field? */
					arr_foreach(struct_type->struc->fields, Field, f) {
						if (f->name == rhs->ident) {
							is_field = true;
							*t = *f->type;
							e->binary.field = f;
						}
					}
				}

				if (!is_field) {
					/* allow some_struct."foo" */
					Value field_name;
					if (!types_expr(tr, rhs)) return false;
					if (rhs_type->kind != TYPE_SLICE || !type_is_builtin(rhs_type->slice, BUILTIN_CHAR)) {
						char *struct_typestr = type_to_str(lhs_type);
						if (rhs->kind == EXPR_IDENT) {
							char *fstr = ident_to_str(rhs->ident);
							err_print(e->where, "%s is not a field of structure %s.", fstr, struct_typestr);
							free(fstr);
						} else {
							char *field_typestr = type_to_str(rhs_type);
							err_print(e->where, "Invalid type %s for field of structure %s .", rhs_type, struct_typestr);
							free(field_typestr);
						}
						free(struct_typestr);
						return false;
						
					}
					if (!eval_expr(tr->evalr, rhs, &field_name)) return false;
					arr_foreach(struct_type->struc->fields, Field, f) {
						if (ident_eq_str(f->name, field_name.slice.data)) {
							is_field = true;
							*t = *f->type;
							e->binary.field = f;
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
				}
			} else if (struct_type->kind == TYPE_SLICE || struct_type->kind == TYPE_ARR) {
				if (!(rhs->kind == EXPR_IDENT && ident_eq_str(rhs->ident, "len"))) {
						
					Value field_name;
					if (!types_expr(tr, rhs)) return false;
					if (rhs_type->kind != TYPE_SLICE || !type_is_builtin(rhs_type->slice, BUILTIN_CHAR)) {
						err_print(e->where, "Invalid field of type %s.");
						return false;
						
					}
					if (!eval_expr(tr->evalr, rhs, &field_name)) return false;
					char *str = field_name.slice.data;
					if (field_name.slice.n != 3	|| strcmp(str, "len") != 0) {					
						char *fstr = err_malloc((size_t)(field_name.slice.n + 1));
						memcpy(fstr, field_name.slice.data, (size_t)field_name.slice.n);
						fstr[field_name.slice.n] = 0; /* null-terminate */
						char *typestr = type_to_str(lhs_type);
						err_print(e->where, "%s is not a field of type %s.", fstr, typestr);
						free(fstr); free(typestr);
						return false;
					}
				}
			
				/* length of slice/arr */
				t->kind = TYPE_BUILTIN;
				t->builtin = BUILTIN_I64;
				/* change expr to UNARY_LEN */
				e->kind = EXPR_UNARY_OP;
				Expression *of = lhs;
				e->unary.op = UNARY_LEN;
				e->unary.of = of;
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
	case EXPR_TYPE:
		if (!type_resolve(tr, &e->typeval, e->where))
			return false;
		t->kind = TYPE_TYPE;
		break;
	case EXPR_VAL:
		assert(0);
		return false;
	}
    t->flags |= TYPE_IS_RESOLVED;
	return true;
}

static bool types_block(Typer *tr, Block *b) {
	if (b->flags & BLOCK_FOUND_TYPES)
		return true;
	bool success = true;
	Block *prev_block = tr->block;
	tr->block = b;
	if (!block_enter(b, b->stmts, SCOPE_CHECK_REDECL)) return false;
	arr_foreach(b->stmts, Statement, s) {
		if (!types_stmt(tr, s))
			success = false;
	}
	if (success && b->ret_expr) {
		if (!types_expr(tr, b->ret_expr))
			success = false;
		if (b->ret_expr->type.kind == TYPE_VOID) {
			err_print(b->ret_expr->where, "Cannot return void value.");
		    success = false;
		}
	}
	block_exit(b, b->stmts);
	tr->block = prev_block;
	b->flags |= BLOCK_FOUND_TYPES;
	return success;
}

static bool types_decl(Typer *tr, Declaration *d) {
	bool success = true;
	if (d->flags & DECL_FOUND_TYPE) return true;
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
		if (d->flags & DECL_ANNOTATES_TYPE) {
			if (!type_must_eq(d->expr.where, &d->type, &d->expr.type)) {
				success = false;
				goto ret;
			}
		} else {
			if (d->expr.type.kind == TYPE_VOID) {
				/* e.g. x := (fn(){})(); */
				err_print(d->expr.where, "Use of void value.");
				success = false;
				goto ret;
			}
			d->type = d->expr.type;
			d->type.flags &= (DeclFlags)~(DeclFlags)TYPE_IS_FLEXIBLE; /* x := 5; => x is not flexible */
		}
		if ((d->flags & DECL_IS_CONST) || (tr->block == NULL && tr->fn == NULL)) {
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
		for (size_t i = 0; i < arr_len(d->idents); i++) {
			Type *t = d->type.kind == TYPE_TUPLE ? &d->type.tuple[i] : &d->type;
			Value *val = d->type.kind == TYPE_TUPLE ? &d->val.tuple[i] : &d->val;
			if (t->kind == TYPE_TYPE) {
				if (!(d->flags & DECL_IS_CONST)) {
					err_print(d->where, "Cannot declare non-constant type.");
					success = false;
					goto ret;
				}
				if (!type_resolve(tr, val->type, d->where)) return false;
				if (val->type->kind == TYPE_TUPLE) {
					err_print(d->where, "You can't declare a new type to be a tuple.");
					success = false;
					goto ret;
				}
			} else if (!(d->flags & DECL_IS_CONST) && t->kind == TYPE_FN && t->fn.constness) {
				for (size_t p = 0; p < arr_len(t->fn.types)-1; p++) {
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

				
	}
	size_t n_idents = arr_len(d->idents);
	if (d->type.kind == TYPE_TUPLE) {
		if (n_idents != arr_len(d->type.tuple)) {
			err_print(d->where, "Expected to have %lu things declared in declaration, but got %lu.", (unsigned long)arr_len(d->type.tuple), (unsigned long)n_idents);
			success = false;
			goto ret;
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
	arr_remove_last(&tr->in_decls);
	return success;
}

static bool types_stmt(Typer *tr, Statement *s) {
	switch (s->kind) {
	case STMT_EXPR:
		if (!types_expr(tr, &s->expr)) {
			return false;
		}
		if (s->expr.type.kind == TYPE_TUPLE) {
			err_print(s->where, "Statement of a tuple is not allowed. Use a semicolon instead of a comma here.");
			return false;
		}
		break;
	case STMT_DECL:
		if (!types_decl(tr, &s->decl))
			return false;
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
	}
	return true;
}

static void typer_create(Typer *tr, Evaluator *ev, Allocator *allocr) {
	tr->block = NULL;
	tr->fn = NULL;
	tr->evalr = ev;
	tr->in_decls = NULL;
	tr->in_expr_decls = NULL;
	tr->allocr = allocr;
	
}

static bool types_file(Typer *tr, ParsedFile *f) {
	bool ret = true;
	arr_foreach(f->stmts, Statement, s) {
		if (!types_stmt(tr, s)) {
			ret = false;
		}
	}
	return ret;
}

