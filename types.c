static bool block_enter(Block *b) {
	bool ret = true;
	arr_foreach(&b->stmts, Statement, stmt) {
		if (stmt->kind == STMT_DECL) {
			Declaration *decl = &stmt->decl;
			arr_foreach(&decl->idents, Identifier, ident) {
				IdentTree *id_info = *ident;
				Array *decls = &id_info->decls;
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
					arr_create(decls, sizeof(IdentDecl));
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
				IdentTree *id_info = *ident;
				Array *decls = &id_info->decls;
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

/* Prints an error and returns false if the given expression is not an l-value */
static bool expr_must_lval(Expression *e) {
	switch (e->kind) {
	case EXPR_IDENT: {
		IdentDecl *id_decl = ident_decl(e->ident);
		if (!id_decl)
			err_print(e->where, "Undeclared identifier.");
		Declaration *d = id_decl->decl;
		if (d->flags & DECL_FLAG_CONST) {
			char *istr = ident_to_str(e->ident);
			err_print(e->where, "Use of constant %s as a non-constant expression.", istr);
			info_print(d->where, "%s was declared here.", istr);
			return false;
		}
		
		return true;
	}
	case EXPR_BINARY_OP:
		if (e->binary.op == BINARY_AT_INDEX) return true;
		break;
	default:
		break;
	}
	err_print(e->where, "Cannot assign to non-lvalue."); 
	return false;
}

static bool type_of_expr(Expression *e, Type *t);
static bool type_of_ident(Location where, Identifier i, Type *t, bool allow_use_before_decl) {
	IdentDecl *decl = ident_decl(i);
	if (!decl) {
		char *s = ident_to_str(i);
		err_print(where, "Undeclared identifier: %s", s);
		free(s);
		return false;
	}
	Declaration *d = decl->decl;
	if (!allow_use_before_decl) {
		/* TODO: Check self-referential declarations  */
		if (d->where.code > where.code) {
			char *s = ident_to_str(i);
			err_print(where, "Use of identifier %s before its declaration.", s);
			info_print(d->where, "%s will be declared here.", s);
			free(s);
			return false;
		}
	}

	/* OPTIM: you don't always need to do so much copying */
	Type decl_type;
	if (d->flags & DECL_FLAG_ANNOTATES_TYPE) {
		decl_type = d->type;
	} else {
		if (!type_of_expr(&d->expr, &decl_type))
			return false;
	}
	
	if (d->idents.len > 1) {
		/* it's a tuple! */
		
		arr_foreach(&d->idents, Identifier, decl_i) {
			if (*decl_i == i) {
				long index = (long)(decl_i - (Identifier*)d->idents.data);
				*t = ((Type*)d->type.tuple.data)[index];
				return true;
			}
		}
		assert(0);
		return false;
	} else {
		*t = decl_type;
		return true;
	}
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
	case TYPE_TUPLE:
		arr_foreach(&t->tuple, Type, child_type) {
			if (!type_resolve(child_type))
				return false;
		}
		break;
	default: break;
	}
	t->flags |= TYPE_FLAG_RESOLVED;
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
		type_resolve(&f->ret_type);
		*ret_type = f->ret_type;
		arr_foreach(&f->params, Param, param) {
			Type *param_type = arr_add(&t->fn.types);
			type_resolve(&param->type);
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
		if (!type_of_ident(e->where, e->ident, t, false)) return false;
				
	} break;
	case EXPR_CALL: {
		Expression *f = e->call.fn;
		Type fn_type;
		if (f->kind == EXPR_IDENT) {
			/* allow calling a function before declaring it */
			if (!type_of_ident(f->where, f->ident, &fn_type, true)) return false;
		} else {
			if (!type_of_expr(f, &fn_type)) return false;
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
		Type *lhs_type = &e->binary.lhs->type;
		Type *rhs_type = &e->binary.rhs->type;
		if (!type_of_expr(e->binary.lhs, lhs_type)
			|| !type_of_expr(e->binary.rhs, rhs_type))
			return false;
		switch (e->binary.op) {
		case BINARY_SET:
			if (!expr_must_lval(e->binary.lhs)) return false;
			/* fallthrough */
		case BINARY_PLUS:
		case BINARY_MINUS: {
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
			if (match) {
				if (e->binary.op == BINARY_SET) {
					/* type of x = y is always void */
					t->kind = TYPE_VOID;
					break;
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
				} else {
					match = false;
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

static bool types_stmt(Statement *s);

static bool types_block(Block *b) {
	bool ret = true;
	if (!block_enter(b)) return false;
	arr_foreach(&b->stmts, Statement, s) {
		if (!types_stmt(s)) ret = false;
	}
	if (!block_exit(b)) return false;
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
	if (d->flags & DECL_FLAG_ANNOTATES_TYPE) {
		/* type supplied */
		assert(d->type.kind != TYPE_VOID); /* there's no way to annotate void */
		if (!type_resolve(&d->type))
			return false;
	}
	if (d->flags & DECL_FLAG_HAS_EXPR) {
		if (!types_expr(&d->expr)) {
			return false;
		}
		if (d->flags & DECL_FLAG_ANNOTATES_TYPE) {
			if (!type_must_eq(d->expr.where, &d->type, &d->expr.type))
				return false;
		} else {
			if (d->expr.type.kind == TYPE_VOID) {
				/* e.g. x := (fn(){})(); */
				err_print(d->expr.where, "Used return value of function which does not return anything.");
				return false;
			}
			d->type = d->expr.type;
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
