static bool call_arg_param_order(FnExpr *fn, Type *fn_type, Argument *args, Location where, I16 **orderp);
static bool parameterized_struct_arg_order(StructDef *struc, Argument *args, I16 **order, Location where);
static bool types_expr(Typer *tr, Expression *e);

/* resolved_to should have the same value as to, but not consist of any identifiers which aren't in scope right now */
/* TODO: is resolved_to necessary? */
static bool infer_from_expr(Typer *tr, Expression *match, Expression *to, Expression *resolved_to, Identifier *idents, Value *vals, Type *types) {
#if 0
	printf("Matching ");
	fprint_expr(stdout, match);
	printf(" to ");
	fprint_expr(stdout, to);
	printf("\n");
#endif
	
	assert(!(match->flags & EXPR_FOUND_TYPE));
	assert(to->flags & EXPR_FOUND_TYPE);
	switch (match->kind) {
	case EXPR_IDENT:
		/* an identifier! maybe it's one of idents... */
		arr_foreach(idents, Identifier, ident) {
			if (*ident == match->ident) {
				long idx = ident - idents;
				types[idx] = to->type;
				if (!eval_expr(tr->evalr, resolved_to, &vals[idx]))
					return false;
				Copier c = copier_create(tr->allocr, tr->block);
				Value new_val;
				copy_val_full(&c, &new_val, &vals[idx], &to->type);
				vals[idx] = new_val;
				break;
			}
		}
		break;
	case EXPR_CALL: {
		
		if (to->kind == EXPR_TYPE && to->typeval.kind == TYPE_STRUCT) {
			/* maybe it's a parameterized struct? */
			/* it might not be possible that it's not, but might as well keep that possibility around. */
			Value fn_val = {0};
			if (!types_expr(tr, match->call.fn)) {
				return false;
			}
			if (type_is_builtin(&match->call.fn->type, BUILTIN_TYPE)) {
				/* it's a parameterized struct */
				if (!eval_expr(tr->evalr, match->call.fn, &fn_val)) {
					return false;
				}
				
				assert(fn_val.type->kind == TYPE_STRUCT);

				I16 *order;
				if (!parameterized_struct_arg_order(fn_val.type->struc, match->call.args, &order, match->where)) {
					free(order);
					return false;
				}
				Declaration *params = to->typeval.struc->params;
				int arg_idx = 0;
				arr_foreach(params, Declaration, param) {
					int ident_idx = 0;
					arr_foreach(param->idents, Identifier, i) {
						if (order[arg_idx] != -1) {
							Expression *arg = &match->call.args[order[arg_idx]].val;
							Value val = *decl_val_at_index(param, ident_idx);
							Expression val_expr = {0};
							val_expr.kind = EXPR_VAL;
							val_expr.val = val;
							val_expr.type = *decl_type_at_index(param, ident_idx);
							val_expr.flags = EXPR_FOUND_TYPE;
							if (!infer_from_expr(tr, arg, &val_expr, &val_expr, idents, vals, types)) {
								free(order);
								return false;
							}
						}
						++arg_idx;
						++ident_idx;
					}
				}
				free(order);
			}
		}
		
		while (to->kind == EXPR_IDENT) {
			Identifier i = to->ident;
			if (i->decl_kind == IDECL_DECL) {
				Declaration *decl = i->decl;
				int index = ident_index_in_decl(i, decl);
				Expression *expr = NULL;
				if (decl->type.kind == TYPE_TUPLE) {
					if (decl->expr.kind == EXPR_TUPLE) {
						expr = &decl->expr.tuple[index];
					}
				} else {
					expr = &decl->expr;
				}
				if (expr) to = expr;
			} else break;
		}
		if (to->kind != EXPR_CALL) {
			if (to->kind == EXPR_TYPE) {
				to = to->typeval.was_expr;
			}
			if (!to || to->kind != EXPR_CALL)
				return true; /* give up */
		}
		
		Argument *m_args = match->call.args;
		size_t nargs = arr_len(m_args);
		Expression *t_args = to->call.arg_exprs;
		I16 *order = NULL;
		Expression *f = match->call.fn;
		Identifier ident = f->ident;
		bool is_direct_fn = f->kind == EXPR_IDENT && ident->decl_kind == IDECL_DECL && (ident->decl->flags & DECL_HAS_EXPR) && ident->decl->expr.kind == EXPR_FN;
		if (!types_expr(tr, f))
			return false;
		if (f->type.kind != TYPE_FN) {
			char *s = type_to_str(&f->type);
			err_print(f->where, "Calling non-function type %s.", s);
			return false;
		}
		if (is_direct_fn) {
			FnExpr *fn_decl = ident->decl->expr.fn;
			if (!call_arg_param_order(fn_decl, &f->type, m_args, match->where, &order)) {
				free(order);
				return false;
			}
		}
		size_t nparams = arr_len(f->type.fn.types) - 1;
		if (!order && nparams != nargs) {
			/* wrong number of parameters? let typing deal with it... */
			free(order);
			return true;
		}
		for (size_t i = 0; i < nparams; ++i) {
			if (!order || order[i] != -1) {
				Argument *m_arg = &m_args[order ? (size_t)order[i] : i];
				Expression *t_arg;
				if (is_direct_fn) {
					t_arg = &t_args[i];
				} else {
					t_arg = &t_args[i];
				}
				if (t_arg->kind == EXPR_VAL) {
					/* was evaluated, because it's const */
					if (!infer_from_expr(tr, &m_arg->val, t_arg, t_arg, idents, vals, types))
						return false;
				}
			}
		}
		free(order);
	} break;
	default: break;
	}
	return true;
}

/* if match is not the same kind of type as to, returns true */
static bool infer_from_type(Typer *tr, Type *match, Type *to, Identifier *idents, Value *vals, Type *types) {
	assert(to->flags & TYPE_IS_RESOLVED);
	assert(!(match->flags & TYPE_IS_RESOLVED));
	switch (match->kind) {
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_BUILTIN:
		break; /* nothing we can do here */
	case TYPE_TUPLE: {
		if (to->kind != TYPE_TUPLE) return true;
		if (arr_len(match->tuple) != arr_len(to->tuple)) return true;
		Type *b = to->tuple;
		arr_foreach(match->tuple, Type, a) {
			if (!infer_from_type(tr, a, b, idents, vals, types))
				return false;
			++b;
		}
	} break;
	case TYPE_FN: {
		if (match->fn.constness || to->fn.constness) {
			return true;
		}
		if (to->kind != TYPE_FN) return true;
		if (arr_len(match->fn.types) != arr_len(to->fn.types)) return true;
		size_t i, len = arr_len(match->fn.types);
		for (i = 0; i < len; ++i) {
			if (!infer_from_type(tr, &match->fn.types[i], &to->fn.types[i], idents, vals, types))
				return false;
		}
	} break;
	case TYPE_PTR:
		if (to->kind != TYPE_PTR) return true;
		if (!infer_from_type(tr, match->ptr, to->ptr, idents, vals, types))
			return false;
		break;
	case TYPE_SLICE:
		if (to->kind != TYPE_SLICE) return true;
		if (!infer_from_type(tr, match->slice, to->slice, idents, vals, types))
			return false;
		break;
	case TYPE_STRUCT: {
		if (to->kind != TYPE_STRUCT) return true;
		Field *fields_m = match->struc->fields;
		Field *fields_t = to->struc->fields;
		size_t i, len = arr_len(fields_m);
		if (len != arr_len(fields_t)) return true;
		for (i = 0; i < len; ++i) {
			if (!infer_from_type(tr, &fields_m[i].type, &fields_t[i].type, idents, vals, types))
				return false;
		}
	} break;
	case TYPE_EXPR: {
		Expression *to_expr = to->was_expr;
		Expression e = {0};
		e.kind = EXPR_TYPE;
		e.typeval = *to;
		e.flags = EXPR_FOUND_TYPE;
		Type *type = &e.type;
		type->flags = TYPE_IS_RESOLVED;
		type->kind = TYPE_BUILTIN;
		type->builtin = BUILTIN_TYPE;
		if (!to_expr) {
			to_expr = &e;
		}
		if (!infer_from_expr(tr, match->expr, to_expr, &e, idents, vals, types))
			return false;
	} break;
	case TYPE_ARR: {
		if (to->kind != TYPE_ARR) return true;
		Expression to_n_expr = {0};
		to_n_expr.kind = EXPR_LITERAL_INT;
		to_n_expr.intl = to->arr.n;
		to_n_expr.flags = EXPR_FOUND_TYPE;
		Type *n_type = &to_n_expr.type;
		n_type->kind = TYPE_BUILTIN;
		n_type->builtin = BUILTIN_I64;
		n_type->flags = TYPE_IS_RESOLVED;
		if (!infer_from_expr(tr, match->arr.n_expr, &to_n_expr, &to_n_expr, idents, vals, types))
			return false;
		if (!infer_from_type(tr, match->arr.of, to->arr.of, idents, vals, types))
			return false;
	} break;
	}
	return true;
}

/* 
match and to are dynamic arrays of equal size
idents is a dyn array of distinct identifiers
find the value of each ident by matching match[i] to to[i], i = 0..arr_len(match)-1
all the types in match must be unresolved, and all the types in to must be resolved
*/
static bool infer_ident_vals(Typer *tr, Type **match, Type **to, Identifier *idents, Value *vals, Type *types) {
	size_t ntypes = arr_len(match);
	size_t i;
	size_t nidents = arr_len(idents);
	
	Type *t = types;
			
	for (i = 0; i < nidents; ++i) {
		t->kind = TYPE_UNKNOWN;
		++t;
	}
	
	for (i = 0; i < ntypes; ++i) {
		if (!infer_from_type(tr, *match, *to, idents, vals, types))
			return false;
		++match, ++to;
	}
	return true;
}

