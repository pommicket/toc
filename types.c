typedef struct {
	Array in_decls;	/* array of declarations we are currently inside */
	Block *block;
	Type *ret_type;	/* the return type of the function we're currently parsing. NULL for none. */
} Typer;

static bool types_stmt(Typer *tr, Statement *s);
static bool types_decl(Typer *tr, Declaration *d);
static bool types_expr(Typer *tr, Expression *e);
static bool types_block(Typer *tr, Block *b);
static bool type_resolve(Typer *tr, Type *t);

static bool add_ident_decls(Block *b, Declaration *d) {
	bool ret = true;
	arr_foreach(&d->idents, Identifier, ident) {
		Array *decls = &(*ident)->decls;
		if (decls->len) {
			/* check that it hasn't been declared in this block */
			IdentDecl *prev = arr_last(decls);
			if (prev->scope == b) {
				err_print(d->where, "Re-declaration of identifier in the same block.");
				info_print(prev->decl->where, "Previous declaration was here.");
				ret = false;
				continue;
			}
		}
		ident_add_decl(*ident, d, b);
	}
	return ret;
}

static void remove_ident_decls(Block *b, Declaration *d) {
	arr_foreach(&d->idents, Identifier, ident) {
		IdentTree *id_info = *ident;
		Array *decls = &id_info->decls;
		assert(decls->item_sz);
		IdentDecl *last_decl = arr_last(decls);
		if (last_decl && last_decl->scope == b) {
			arr_remove_last(decls); /* remove that declaration */
		}
	}
}

/* pass NULL for block for global scope */
static bool block_enter(Block *b, Array *stmts) {
	bool ret = true;
	arr_foreach(stmts, Statement, stmt) {
		if (stmt->kind == STMT_DECL) {
			Declaration *decl = &stmt->decl;
			if (!add_ident_decls(b, decl))
				ret = false;
		}
	}
	return ret;
}

static void block_exit(Block *b, Array *stmts) {
	/* OPTIM: figure out some way of not re-iterating over everything */
	arr_foreach(stmts, Statement, stmt) {
		if (stmt->kind == STMT_DECL) {
			Declaration *decl = &stmt->decl;
			remove_ident_decls(b, decl);
		}
	}
}

static bool type_eq(Type *a, Type *b) {
	if (a->kind == TYPE_UNKNOWN || b->kind == TYPE_UNKNOWN)
		return true; /* allow things such as 3 + #C("5") */
	if (a->kind != b->kind) return false;
	if (a->flags & TYPE_FLAG_FLEXIBLE) {
		if (b->flags & TYPE_FLAG_FLEXIBLE) return true;
		assert(a->kind == TYPE_BUILTIN);
		
		if (type_builtin_is_floating(a->builtin)) {
			return type_builtin_is_floating(b->builtin);
		}
		assert(a->builtin == BUILTIN_I64);
		return type_builtin_is_numerical(b->builtin);
	}
	if (b->flags & TYPE_FLAG_FLEXIBLE) {
		return type_eq(b, a); /* OPTIM? */
	}
	switch (a->kind) {
	case TYPE_VOID: return true;
	case TYPE_UNKNOWN: assert(0); return false;
	case TYPE_BUILTIN:
		return a->builtin == b->builtin;
	case TYPE_FN: {
		
		if (a->fn.types.len != b->fn.types.len) return false;
		Type *a_types = a->fn.types.data, *b_types = b->fn.types.data;
		for (size_t i = 0; i < a->fn.types.len; i++) {
			if (!type_eq(&a_types[i], &b_types[i]))
				return false;
			
		}
		return true;
	}
	case TYPE_TUPLE:
		if (a->tuple.len != b->tuple.len) return false;
		Type *a_types = a->tuple.data, *b_types = b->tuple.data;
		for (size_t i = 0; i < a->tuple.len; i++) {
			if (!type_eq(&a_types[i], &b_types[i]))
				return false;
		}
		return true;
	case TYPE_ARR:
		if (a->arr.n != b->arr.n) return false;
		return type_eq(a->arr.of, b->arr.of);
	case TYPE_PTR:
		return type_eq(a->ptr.of, b->ptr.of);
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

/* sometimes prints an error and returns false if the given expression is not an l-value */
static bool expr_must_lval(Expression *e) {
	switch (e->kind) {
	case EXPR_IDENT: {
		IdentDecl *id_decl = ident_decl(e->ident);
		assert(id_decl);
		Declaration *d = id_decl->decl;
		if (d->flags & DECL_FLAG_CONST) {
			char *istr = ident_to_str(e->ident);
			err_print(e->where, "Use of constant %s as a non-constant expression.", istr);
			info_print(d->where, "%s was declared here.", istr);
			return false;
		}
		
		return true;
	}
	case EXPR_UNARY_OP:
		if (e->unary.op == UNARY_DEREF) return true;
		break;
	case EXPR_BINARY_OP:
		switch (e->binary.op) {
		case BINARY_AT_INDEX: return true;
		case BINARY_COMMA:
			/* x, y is an lval, but 3, "hello" is not. */
			return expr_must_lval(e->binary.lhs) && expr_must_lval(e->binary.rhs);
		default: break;
		}
		break;
	default:
		break;
	}
	return false;
}

static bool type_of_fn(Typer *tr, FnExpr *f, Type *t) {
	t->kind = TYPE_FN;
	arr_create(&t->fn.types, sizeof(Type));
	Type *ret_type = arr_add(&t->fn.types);
	if (!type_resolve(tr, &f->ret_type))
		return false;
	*ret_type = f->ret_type;
	arr_foreach(&f->params, Declaration, decl) {
		if (!types_decl(tr, decl)) return false;
		if (!type_resolve(tr, &decl->type))
			return false;
		for (size_t i = 0; i < decl->idents.len; i++) {
			Type *param_type = arr_add(&t->fn.types);
			*param_type = decl->type;
		}
	}
	return true;
}

static bool type_of_ident(Typer *tr, Location where, Identifier i, Type *t) {
	IdentDecl *decl = ident_decl(i);
	if (!decl) {
		char *s = ident_to_str(i);
		err_print(where, "Undeclared identifier: %s", s);
		free(s);
		return false;
	}
	Declaration *d = decl->decl;
	bool captured = false;
	if (decl->scope != NULL)
		for (Block *block = tr->block; block != decl->scope; block = block->parent) {
			if (block->flags & BLOCK_FLAG_FN) {
				captured = true;
				break;
			}
		}
	if (captured && !(d->flags & DECL_FLAG_CONST)) {
		err_print(where, "Variables cannot be captured into inner functions (but constants can).");
		return false;
	}
	/* are we inside this declaration? */
	typedef Declaration *DeclarationPtr;
	arr_foreach(&tr->in_decls, DeclarationPtr, in_decl) {
		if (d == *in_decl) {
			assert(d->flags & DECL_FLAG_HAS_EXPR); /* we can only be in decls with an expr */
			if (d->expr.kind != EXPR_FN) { /* it's okay if a function references itself */
				/* if we've complained about it before when we were figuring out the type, don't complain again */
				if (!(d->flags & DECL_FLAG_ERRORED_ABOUT_SELF_REFERENCE)) {
					char *s = ident_to_str(i);
					err_print(where, "Use of identifier %s within its own declaration.", s);
					free(s);
					info_print(d->where, "Declaration was here.");
					d->flags |= DECL_FLAG_ERRORED_ABOUT_SELF_REFERENCE;
				}
				return false;
			}
		}
	}
	
	if (d->flags & DECL_FLAG_FOUND_TYPE) {
		*t = d->type;
		return true;
	} else {
		if ((d->flags & DECL_FLAG_HAS_EXPR) && (d->expr.kind == EXPR_FN)) {
			/* allow using a function before declaring it */
			if (!type_of_fn(tr, &d->expr.fn, t)) return false;
			return true;
		} else {
			if (location_after(d->where, where)) {
				char *s = ident_to_str(i);
				err_print(where, "Use of identifier %s before its declaration.\nNote that it is only possible to use a constant function before it is directly declared (e.g. x @= fn() {}).", s);
				info_print(d->where, "%s will be declared here.", s);
				free(s);
			} /* else, there should have been an error earlier */
			return false;
		}
	}
}

/* fixes the type (replaces [5+3]int with [8]int, etc.) */
static bool type_resolve(Typer *tr, Type *t) {
	if (t->flags & TYPE_FLAG_RESOLVED) return true;
	switch (t->kind) {
	case TYPE_ARR: {
		/* it's an array */
		if (!type_resolve(tr, t->arr.of)) return false; /* resolve inner type */
		Value val;
		Expression *n_expr = t->arr.n_expr;
		if (!types_expr(tr, n_expr)) return false;
		if (n_expr->type.kind != TYPE_BUILTIN || !type_builtin_is_integer(n_expr->type.builtin)) {
			char *s = type_to_str(&n_expr->type);
			err_print(n_expr->where, "Cannot use type %s as the size of an array (it's not an integer type).", s);
			free(s);
			return false;
		}
		if (!eval_expr(n_expr, &val)) return false; /* resolve N */
		Integer size = val.intv;
		if (size < 0)
			err_print(t->arr.n_expr->where, "Negative array length (" INTEGER_FMT ")", size);
		t->arr.n = (UInteger)size;
	} break;
	case TYPE_FN:
		arr_foreach(&t->fn.types, Type, child_type) {
			if (!type_resolve(tr, child_type))
				return false;
		}
		break;
	case TYPE_TUPLE:
		arr_foreach(&t->tuple, Type, child_type) {
			if (!type_resolve(tr, child_type))
				return false;
		}
		break;
	default: break;
	}
	t->flags |= TYPE_FLAG_RESOLVED;
	return true;
}


static bool type_can_be_truthy(Type *t) {
	switch (t->kind) {
	case TYPE_VOID:
		return false;
	case TYPE_UNKNOWN:
		return true;
	case TYPE_BUILTIN:
		return true;
	case TYPE_FN:
		return true;
	case TYPE_TUPLE:
		return false;
	case TYPE_ARR:
		return false;
	case TYPE_PTR:
		return true;
	}
	assert(0);
	return false;
}

static bool types_expr(Typer *tr, Expression *e) {
	if (e->flags & EXPR_FLAG_FOUND_TYPE) return true;
	Type *prev_ret_type = tr->ret_type;
	
	Type *t = &e->type;
	t->flags = 0;
	t->kind = TYPE_UNKNOWN; /* default to unknown type (in the case of an error) */
	e->flags |= EXPR_FLAG_FOUND_TYPE; /* even if failed, pretend we found the type */
	bool success = true;
	switch (e->kind) {
	case EXPR_FN: {
		FnExpr *f = &e->fn;
		if (!type_of_fn(tr, f, t)) {
			success = false;
			goto fn_ret;
		}
		tr->ret_type = t->fn.types.data;
		arr_foreach(&f->params, Declaration, decl)
			add_ident_decls(&f->body, decl);
		bool block_success = true;
		block_success = types_block(tr, &e->fn.body);
		arr_foreach(&f->params, Declaration, decl)
			remove_ident_decls(&f->body, decl);
		if (!block_success) {
			success = false;
			goto fn_ret;
		}
		Expression *ret_expr = f->body.ret_expr;
		assert(t->kind == TYPE_FN);
		Type *ret_type = t->fn.types.data;
		if (ret_expr) {
			if (!types_expr(tr, ret_expr)) {
				success = false;
				goto fn_ret;
			}
			if (!type_eq(ret_type, &ret_expr->type)) {
				char *got = type_to_str(&ret_expr->type);
				char *expected = type_to_str(ret_type);
				err_print(ret_expr->where, "Returning type %s, but function returns type %s.", got, expected);
				info_print(e->where, "Function declaration is here.");
				free(got); free(expected);
				success = false;
				goto fn_ret;
			}
		} else if (ret_type->kind != TYPE_VOID) {
			Array stmts = e->fn.body.stmts;
			if (stmts.len) {
				Statement *last_stmt = (Statement *)stmts.data + (stmts.len - 1);
				if (last_stmt->kind == STMT_RET) {
					/*
					  last statement is a return, so it doesn't matter that the function has no return value
					ideally this would handle if foo { return 5; } else { return 6; } */
					success = true;
					goto fn_ret;
				}
			}
			/* TODO: this should really be at the closing brace, and not the function declaration */
			char *expected = type_to_str(ret_type);
			err_print(f->body.end, "No return value in function which returns %s.", expected);
			free(expected);
			info_print(e->where, "Function was declared here:");
			success = false;
			goto fn_ret;
		}
		fn_ret:
		tr->ret_type = prev_ret_type;
		if (!success) return false;
	} break;
	case EXPR_LITERAL_INT:
	    t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_I64;
		t->flags |= TYPE_FLAG_FLEXIBLE;
		break;
	case EXPR_LITERAL_STR:
		t->kind = TYPE_UNKNOWN;	/* TODO */
		break;
	case EXPR_LITERAL_FLOAT:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_F32;
		t->flags |= TYPE_FLAG_FLEXIBLE;
		break;
	case EXPR_LITERAL_BOOL:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_BOOL;
		break;
	case EXPR_IDENT: {
		if (!type_of_ident(tr, e->where, e->ident, t)) return false;
	} break;
	case EXPR_CAST: {
		/* TODO: forbid certain casts */
		CastExpr *c = &e->cast;
		if (!types_expr(tr, c->expr))
			return false;
		*t = c->type;
	} break;
	case EXPR_IF: {
		IfExpr *i = &e->if_;
		IfExpr *curr = i;
		Type *curr_type = t;
		bool has_else = false;
		if (!types_block(tr, &curr->body))
			return false;
		*t = curr->body.ret_expr->type;
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
				*next_type = nexti->body.ret_expr->type;
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
		if (!types_expr(tr, w->cond))
			ret = false;
		if (!types_block(tr, &w->body))
			ret = false;
		if (!ret) return false;
		*t = w->body.ret_expr->type;
	} break;
	case EXPR_CALL: {
		CallExpr *c = &e->call;
		Expression *f = c->fn;
		if (f->kind == EXPR_IDENT) {
			/* allow calling a function before declaring it */
			if (!type_of_ident(tr, f->where, f->ident, &f->type)) return false;
		} else {
			if (!types_expr(tr, f)) return false;
		}
		arr_foreach(&c->args, Expression, arg) {
			if (!types_expr(tr, arg))
				return false;
		}
		if (f->type.kind != TYPE_FN) {
			char *type = type_to_str(&f->type);
			err_print(e->where, "Calling non-function (type %s).", type);
			return false;
		}
		Type *ret_type = (Type *)f->type.fn.types.data;
		Type *param_types = ret_type + 1;
		Expression *args = c->args.data;
		size_t nparams = f->type.fn.types.len - 1;
		if (nparams != c->args.len) {
			err_print(e->where, "Expected %lu arguments to function, but got %lu.", (unsigned long)nparams, (unsigned long)c->args.len);
			return false;
		}
		bool ret = true;
		for (size_t p = 0; p < nparams; p++) {
			Type *expected = &param_types[p];
			Type *got = &args[p].type;
			if (!type_eq(expected, got)) {
				ret = false;
				char *estr = type_to_str(expected);
				char *gstr = type_to_str(got);
				err_print(args[p].where, "Expected type %s as %lu%s argument to function, but got %s.", estr, 1+(unsigned long)p, ordinals(1+p), gstr);
			}
		}
		if (!ret) return false;
		*t = *ret_type;
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
	case EXPR_DIRECT:
		t->kind = TYPE_UNKNOWN;
	    arr_foreach(&e->direct.args, Expression, arg) {
			if (!types_expr(tr, arg))
				return false;
		}
		switch (e->direct.which) {
		case DIRECT_C: {
			size_t n_args = e->direct.args.len;
			if (n_args != 1) {
				err_print(e->where, "#C call should have one string argument (got %lu arguments).", (unsigned long)n_args);
				return false;
			}
			/* TODO: when string types are added, check */
		} break;
		case DIRECT_COUNT: assert(0); return false;
		}
		break;
	case EXPR_UNARY_OP: {
		Expression *of = e->unary.of;
		Type *of_type = &of->type;
	    if (!types_expr(tr, e->unary.of)) return false;
		switch (e->unary.op) {
		case UNARY_MINUS:
			if (of_type->kind != TYPE_BUILTIN || !type_builtin_is_numerical(of_type->builtin)) {
				char *s = type_to_str(of_type);
				err_print(e->where, "Cannot apply unary - to non-numerical type %s.", s);
				free(s);
				return false;
			}
			*t = *of_type;
			break;
		case UNARY_ADDRESS:
			if (!expr_must_lval(of)) {
				err_print(e->where, "Cannot take address of non-lvalue."); /* FEATURE: better err */
				return false;
			}
			t->kind = TYPE_PTR;
			t->ptr.of = err_malloc(sizeof *t->ptr.of); /* OPTIM */
			*t->ptr.of = *of_type;
			break;
		case UNARY_DEREF:
			if (of_type->kind != TYPE_PTR) {
				char *s = type_to_str(of_type);
				err_print(e->where, "Cannot dereference non-pointer type %s.", s);
				free(s);
				return false;
			}
			*t = *of_type->ptr.of;
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
		}
	} break;
	case EXPR_BINARY_OP: {
		Type *lhs_type = &e->binary.lhs->type;
		Type *rhs_type = &e->binary.rhs->type;
		if (!types_expr(tr, e->binary.lhs)
			|| !types_expr(tr, e->binary.rhs))
			return false;
		switch (e->binary.op) {
		case BINARY_SET:
			if (!expr_must_lval(e->binary.lhs)) {
				err_print(e->where, "You can only assign to an lvalue."); /* FEATURE: better err */
				return false;
			}
			/* fallthrough */
		case BINARY_PLUS:
		case BINARY_MINUS:
		case BINARY_MUL:
		case BINARY_DIV:
		case BINARY_LT:
		case BINARY_GT:
		case BINARY_LE:
		case BINARY_GE:
		case BINARY_EQ:
		case BINARY_NE: {
			bool match = true;
			if (e->binary.op != BINARY_SET) {
				/* numerical binary ops */
				if (lhs_type->kind != rhs_type->kind) {
					match = false;
				} else if (lhs_type->kind != TYPE_BUILTIN) {
					match = false;
				} else if (!type_builtin_is_numerical(lhs_type->builtin) || !type_builtin_is_numerical(rhs_type->builtin)) {
					match = false;
				}
			}
			if (!type_eq(lhs_type, rhs_type)) match = false;
			if (match) {
				switch (e->binary.op) {
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
					int lhs_is_flexible = lhs_type->flags & TYPE_FLAG_FLEXIBLE;
					int rhs_is_flexible = rhs_type->flags & TYPE_FLAG_FLEXIBLE;
					if (lhs_is_flexible && rhs_is_flexible) {
						*t = *lhs_type;
						if (rhs_type->builtin == BUILTIN_F32) {
							/* promote to float */
							t->builtin = BUILTIN_F32;
						}
					} else if (!lhs_is_flexible)
						*t = *lhs_type;
					else
						*t = *rhs_type;
				} break;
				}
			}
			if (!match) {
				char *s1, *s2;
				s1 = type_to_str(lhs_type);
				s2 = type_to_str(rhs_type);
				const char *op = binary_op_to_str(e->binary.op);
				err_print(e->where, "Mismatched types to operator %s: %s and %s", op, s1, s2);
				return false;
			}
			break;
		}
		case BINARY_AT_INDEX:
			/* TODO(eventually): support non-builtin numerical (or even perhaps non-numerical) indices */
			if (rhs_type->kind != TYPE_BUILTIN || !type_builtin_is_numerical(rhs_type->builtin)) {
				err_print(e->where, "The index of an array must be a builtin numerical type.");
				return false;
			}
			if (lhs_type->kind != TYPE_ARR) {
				err_print(e->where, "Trying to take index of non-array.");
				return false;
			}
			*t = *lhs_type->arr.of;
			break;
		case BINARY_COMMA: {
			t->kind = TYPE_TUPLE;
			Array *tup_types = &t->tuple;
			arr_create(tup_types, sizeof(Type));
			if (lhs_type->kind == TYPE_TUPLE) {
				/* tuple, x => tuple */
				arr_foreach(&lhs_type->tuple, Type, child) {
					*(Type*)arr_add(tup_types) = *child;
				}
			} else {
				*(Type*)arr_add(tup_types) = *lhs_type;
			}
			
			if (rhs_type->kind == TYPE_TUPLE) {
				/* x, tuple => tuple */
				arr_foreach(&rhs_type->tuple, Type, child) {
					*(Type*)arr_add(tup_types) = *child;
				}
			} else {
				*(Type*)arr_add(tup_types) = *rhs_type;
			}
		} break;
		}
	} break;
	}
	return true;
}

static bool types_block(Typer *tr, Block *b) {
	bool success = true;
	Block *prev_block = tr->block;
	tr->block = b;
	if (!block_enter(b, &b->stmts)) return false;
	arr_foreach(&b->stmts, Statement, s) {
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
	block_exit(b, &b->stmts);
	tr->block = prev_block;
	return success;
}

static bool types_decl(Typer *tr, Declaration *d) {
	bool success = true;
	if (d->flags & DECL_FLAG_FOUND_TYPE) goto ret;
	Declaration **dptr = arr_add(&tr->in_decls);
	*dptr = d;
	if (d->flags & DECL_FLAG_ANNOTATES_TYPE) {
		/* type supplied */
		assert(d->type.kind != TYPE_VOID); /* there's no way to annotate void */
		if (!type_resolve(tr, &d->type)) {
			success = false;
			goto ret;
		}
	}
	if (d->flags & DECL_FLAG_HAS_EXPR) {
		if (!types_expr(tr, &d->expr)) {
			success = false;
			goto ret;
		}
		if (d->flags & DECL_FLAG_ANNOTATES_TYPE) {
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
			d->type.flags &= ~TYPE_FLAG_FLEXIBLE; /* x := 5; => x is not flexible */
		}
		if (d->flags & DECL_FLAG_CONST) {
			if (!d->val) {
				d->val = err_malloc(sizeof *d->val); /* OPTIM */
				if (!eval_expr(&d->expr, d->val)) {
					success = false;
					goto ret;
				}
			}
		}
	}
	d->flags |= DECL_FLAG_FOUND_TYPE;
	if (d->type.kind == TYPE_TUPLE) {
		/* TODO(eventually): Should this be allowed? */
		err_print(d->where, "Declaring a tuple is not allowed.");
		return false;
	}
 ret:
	arr_remove_last(&tr->in_decls);
	return success;
}

static bool types_stmt(Typer *tr, Statement *s) {
	switch (s->kind) {
	case STMT_EXPR:
		if (!types_expr(tr, &s->expr)) {
			return false;
		}
		break;
	case STMT_DECL:
		if (!types_decl(tr, &s->decl))
			return false;
		break;
	case STMT_RET:
		if (!tr->ret_type) {
			err_print(s->where, "return outside of a function.");
			return false;
		}
		if (s->ret.flags & RET_FLAG_EXPR) {
			if (tr->ret_type->kind == TYPE_VOID) {
				err_print(s->where, "Return value in void function.");
				return false;
			}
			if (!types_expr(tr, &s->ret.expr))
				return false;
		} else {
			if (tr->ret_type->kind != TYPE_VOID) {
				err_print(s->where, "No return value in non-void function.");
				return false;
			}
		}
		break;
	}
	return true;
}

static void typer_create(Typer *tr) {
	tr->block = NULL;
	arr_create(&tr->in_decls, sizeof(Declaration *));
}

static bool types_file(ParsedFile *f) {
	Typer tr;
	typer_create(&tr);
	arr_foreach(&f->stmts, Statement, s) {
		if (!types_stmt(&tr, s)) {
			return false;
		}
	}
	return true;
}