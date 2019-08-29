static bool block_enter(Block *b) {
	bool ret = true;
	arr_foreach(&b->stmts, Statement, stmt) {
		if (stmt->kind == STMT_DECL) {
			Declaration *decl = &stmt->decl;
			arr_foreach(&decl->idents, Identifier, ident) {
				Array *decls = &(*ident)->decls;
				if (decls->len) {
					/* check that it hasn't been declared in this block */
					IdentDecl *prev = arr_last(decls);
					if (prev->scope == b) {
						err_print(decl->where, "Re-declaration of identifier in the same block.");
						info_print(prev->decl->where, "Previous declaration was here.");
						ret = false;
						continue;
					}
				} else {
					/* array not initialized yet */
					arr_create(&(*ident)->decls, sizeof(IdentDecl));
				}
				
				IdentDecl *ident_decl = arr_add(decls);
				ident_decl->decl = decl;
				ident_decl->scope = b;
			}
		}
	}
	return ret;
}

static bool block_exit(Block *b) {
	/* OPTIM: figure out some way of not re-iterating over everything */
	bool ret = true;
	arr_foreach(&b->stmts, Statement, stmt) {
		if (stmt->kind == STMT_DECL) {
			Declaration *decl = &stmt->decl;
			arr_foreach(&decl->idents, Identifier, ident) {
				Array *decls = &(*ident)->decls;
				assert(decls->item_sz);
				IdentDecl *last_decl = arr_last(decls);
				if (last_decl->scope == b) {
					arr_remove_last(decls); /* remove that declaration */
				}
				
			}
		}
	}
	return ret;
}

static bool type_eq(Type *a, Type *b) {
	if (a->kind != b->kind) return false;
	if (a->flags & TYPE_FLAG_FLEXIBLE) {
		if (b->flags & TYPE_FLAG_FLEXIBLE) return true;
		assert(a->kind == TYPE_BUILTIN);
		
		if (a->builtin == BUILTIN_FLOAT) {
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
	case TYPE_ARR:
		if (a->arr.n != b->arr.n) return false;
		return type_eq(a->arr.of, b->arr.of);
	}
	assert(0);
	return false;
}

/* expected must equal got, or an error will be produced */
static bool type_must_eq(Location where, Type *expected, Type *got) {
	if (!type_eq(expected, got)) {
		char str_ex[128];
		char str_got[128];
		type_to_str(expected, str_ex, sizeof str_ex);
		type_to_str(got, str_got, sizeof str_got);
		err_print(where, "Type mismatch: expected %s, but got %s.", str_ex, str_got);
		return false;
	}
	return true;
}

/* NOTE: this does descend into un/binary ops, etc. but NOT into functions  */
static bool type_of_expr(Expression *e, Type *t) {
	t->flags = 0;
	
	switch (e->kind) {
	case EXPR_FN: {
		FnExpr *f = &e->fn;
		t->kind = TYPE_FN;
		arr_create(&t->fn.types, sizeof(Type));
		Type *ret_type = arr_add(&t->fn.types);
		*ret_type = f->ret_type;
		arr_foreach(&f->params, Param, param) {
			Type *param_type = arr_add(&t->fn.types);
			*param_type = param->type;
		}
	} break;
	case EXPR_INT_LITERAL:
	    t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_I64;
		t->flags |= TYPE_FLAG_FLEXIBLE;
		break;
	case EXPR_FLOAT_LITERAL:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_FLOAT;
		t->flags |= TYPE_FLAG_FLEXIBLE;
		break;
	case EXPR_IDENT: {
		IdentDecl *decl = ident_decl(e->ident);
		if (!decl) {
			char *s = ident_to_str(e->ident);
			err_print(e->where, "Undeclared identifier: %s", s);
			free(s);
		}
		Declaration *d = decl->decl;
		/* TODO: Check self-referential declarations */
		if (d->where.code > e->where.code) {
			char *s = ident_to_str(e->ident);
			err_print(e->where, "Use of identifier %s before its declaration.", s);
			info_print(d->where, "%s will be declared here.", s);
			free(s);
		}
		*t = d->type;
	} break;
	case EXPR_CALL: {
		Expression *f = e->call.fn;
		Type fn_type;
		if (f->kind == EXPR_IDENT) {
			/* allow calling a function before declaring it */
			IdentDecl *decl = ident_decl(f->ident);
			if (!decl) {
				char *s = ident_to_str(e->ident);
				err_print(e->where, "Undeclared identifier: %s", s);
				free(s);
			}
			if (!type_of_expr(&decl->decl->expr, &fn_type)) return false;
		} else {
			if (!type_of_expr(e->call.fn, &fn_type)) return false;
		}
		if (fn_type.kind != TYPE_FN) {
			char type[128];
			type_to_str(&fn_type, type, sizeof type);
			err_print(e->where, "Calling non-function (type %s).", type);
			return false;
		}
		/* TODO: Make sure args match fn type */
		*t = *(Type*)fn_type.fn.types.data;
		break;
	}
	case EXPR_UNARY_OP: {
		Type *of_type = &e->unary.of->type;
	    if (!type_of_expr(e->unary.of, of_type)) return false;
		switch (e->unary.op) {
		case UNARY_MINUS:
			if (of_type->kind != TYPE_BUILTIN || !type_builtin_is_numerical(of_type->builtin)) {
				char s[128];
				type_to_str(of_type, s, sizeof s);
				err_print(e->where, "Cannot apply unary - to non-numerical type %s.", s);
				return false;
			}
			*t = *of_type;
			break;
		}
	} break;
	case EXPR_BINARY_OP: {
		switch (e->binary.op) {
		case BINARY_PLUS:
		case BINARY_MINUS:
		case BINARY_SET: {
			Type *lhs_type = &e->binary.lhs->type;
			Type *rhs_type = &e->binary.rhs->type;
			if (!type_of_expr(e->binary.lhs, lhs_type)
				|| !type_of_expr(e->binary.rhs, rhs_type))
				return false;
			bool match = true;
			if (lhs_type->kind != rhs_type->kind) {
				match = false;
			} else if (lhs_type->kind != TYPE_BUILTIN) {
				match = false;
			} else if (!type_builtin_is_numerical(lhs_type->builtin) || !type_builtin_is_numerical(rhs_type->builtin)) {
				match = false;
			} else {
				if (e->binary.op == BINARY_SET) {
					/* type of x = y is always void */
					t->kind = TYPE_VOID;
					return true; 
				}
				int lhs_is_flexible = lhs_type->flags & TYPE_FLAG_FLEXIBLE;
				int rhs_is_flexible = rhs_type->flags & TYPE_FLAG_FLEXIBLE;
				if (lhs_is_flexible && rhs_is_flexible) {
					*t = *lhs_type;
					if (rhs_type->builtin == BUILTIN_FLOAT) {
						/* promote to float */
						t->builtin = BUILTIN_FLOAT;
					}
				} else if (type_eq(lhs_type, rhs_type)) {
					if (!lhs_is_flexible)
						*t = *lhs_type;
					else
						*t = *rhs_type;
				}
			}
			if (!match) {
				char s1[128], s2[128];
				type_to_str(lhs_type, s1, sizeof s1);
				type_to_str(rhs_type, s2, sizeof s2);
				const char *op = binary_op_to_str(e->binary.op);
				err_print(e->where, "Mismatched types to operator %s: %s and %s", op, s1, s2);
				return false;
			}
			return true;
		}
		}
	} break;
	}
	return true;
}

/* fixes the type (replaces [5+3]int with [8]int, etc.) */
static bool type_resolve(Type *t) {
	if (t->flags & TYPE_FLAG_RESOLVED) return true;
	switch (t->kind) {
	case TYPE_ARR: {
		/* it's an array */
		if (!type_resolve(t->arr.of)) return false; /* resolve inner type */
		Integer size;
		if (!eval_expr_as_int(t->arr.n_expr, &size)) return false; /* resolve N */
		if (size < 0)
			err_print(t->arr.n_expr->where, "Negative array length (" INTEGER_FMT ")", size);
		t->arr.n = (UInteger)size;
	} break;
	case TYPE_FN:
		arr_foreach(&t->fn.types, Type, child_type) {
			if (!type_resolve(child_type))
				return false;
		}
		break;
	default: break;
	}
	t->flags |= TYPE_FLAG_RESOLVED;
	return true;
}

static bool types_stmt(Statement *s);

static bool types_block(Block *b) {
	bool ret = true;
	block_enter(b);
	arr_foreach(&b->stmts, Statement, s) {
		if (!types_stmt(s)) ret = false;
	}
	block_exit(b);
	return ret;
}

static bool types_expr(Expression *e) {
	Type *t = &e->type;
	if (!type_of_expr(e, t)) return false;
	switch (e->kind) {
	case EXPR_FN:
		return types_block(&e->fn.body);
	case EXPR_CALL: {
		bool ret = true;
		arr_foreach(&e->call.args, Expression, arg) {
			if (!types_expr(arg)) ret = false;
		}
	    return ret;
	} break;
	default: break;
	}
	return true;
}


static bool types_decl(Declaration *d) {
	if (d->flags & DECL_FLAG_FOUND_TYPE) return true;
	if (!(d->flags & DECL_FLAG_INFER_TYPE)) {
		/* type supplied */
		if (!type_resolve(&d->type))
			return false;
	}
	if (d->flags & DECL_FLAG_HAS_EXPR) {
		if (!types_expr(&d->expr)) return false;
		if (d->flags & DECL_FLAG_INFER_TYPE) {
			d->type = d->expr.type;
		} else {
			if (!type_must_eq(d->expr.where, &d->type, &d->expr.type))
				return false;
		}
	}
	d->flags |= DECL_FLAG_FOUND_TYPE;
	return true;
}

static bool types_stmt(Statement *s) {
	switch (s->kind) {
	case STMT_EXPR:
		if (!types_expr(&s->expr)) {
			return false;
		}
		break;
	case STMT_DECL:
		if (!types_decl(&s->decl))
			return false;
		break;
	}
	return true;
}

static bool types_file(ParsedFile *f) {
	arr_foreach(&f->stmts, Statement, s) {
		if (!types_stmt(s)) {
			return false;
		}
	}
	return true;
}
