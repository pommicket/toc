/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static bool call_arg_param_order(FnExpr *fn, Type *fn_type, Argument *args, Location where, I16 **orderp);
static bool parameterized_struct_arg_order(StructDef *struc, Argument *args, I16 **order, Location where);
static bool types_expr(Typer *tr, Expression *e);

static bool infer_from_expr(Typer *tr, Expression *match, Value to, Type *to_type, Location to_where, Identifier *idents, Value *vals, Type *types) {
#if 0
	printf("Matching ");
	fprint_expr(stdout, match);
	printf(" to ");
	fprint_expr(stdout, to);
	printf("\n");
#endif
	
	assert(!(match->flags & EXPR_FOUND_TYPE));
	assert(to_type->flags & TYPE_IS_RESOLVED);
	switch (match->kind) {
	case EXPR_IDENT:
		/* an identifier! maybe it's one of idents... */
		arr_foreach(idents, Identifier, ident) {
			if (ident_eq_string(*ident, match->ident_str)) {
				long idx = (long)(ident - idents);
				types[idx] = *to_type;
				vals[idx] = to;
				break;
			}
		}
		break;
	case EXPR_CALL: {
		if (!types_expr(tr, match->call.fn))
			return false;
		if (type_is_builtin(&match->call.fn->type, BUILTIN_TYPE)) {
			/* it's a parameterized struct */
			if (!type_is_builtin(to_type, BUILTIN_TYPE) || to.type->kind != TYPE_STRUCT) {
				err_print(to_where, "Wrong argument type. Expected this to be a struct, but it's not.");
				info_print(match->where, "Parameter was declared here.");
				return false;
			}
			Type *fn_type = to.type;
			I16 *order;
			if (!parameterized_struct_arg_order(fn_type->struc, match->call.args, &order, match->where)) {
				free(order);
				return false;
			}
			Declaration *params = fn_type->struc->params;
			int arg_idx = 0;
			arr_foreach(params, Declaration, param) {
				int ident_idx = 0;
				arr_foreach(param->idents, Identifier, i) {
					if (order[arg_idx] != -1) {
						Expression *arg = &match->call.args[order[arg_idx]].val;
						Value val = *decl_val_at_index(param, ident_idx);
						if (!infer_from_expr(tr, arg, val, decl_type_at_index(param, ident_idx), param->where, idents, vals, types)) {
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
		/* don't try to match other kinds of function calls. it's impossible to get any information out of it. */
	} break;
	default: break;
	}
	return true;
}

static bool infer_from_type(Typer *tr, Type *match, Type *to, Identifier *idents, Value *vals, Type *types, Location where) {
	assert(to->flags & TYPE_IS_RESOLVED);
	assert(!(match->flags & TYPE_IS_RESOLVED));
	if (match->kind != TYPE_UNKNOWN && match->kind != TYPE_EXPR && to->kind != TYPE_UNKNOWN) {
		if (match->kind != to->kind) {
			if (to->kind != TYPE_TUPLE) {
				char *m = type_to_str(match), *t = type_to_str(to);
				err_print(where, "Wrong argument type. Expected %s, but got %s.", m, t);
				free(m); free(t);
				return false;
			}
		}
	}
	switch (match->kind) {
	case TYPE_UNKNOWN:
	case TYPE_BUILTIN:
		break; /* nothing we can do here */
	case TYPE_TUPLE: {
		if (arr_len(match->tuple) != arr_len(to->tuple)) return true;
		Type *b = to->tuple;
		arr_foreach(match->tuple, Type, a) {
			if (!infer_from_type(tr, a, b, idents, vals, types, where))
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
			if (!infer_from_type(tr, &match->fn.types[i], &to->fn.types[i], idents, vals, types, where))
				return false;
		}
	} break;
	case TYPE_PTR:
		if (to->kind != TYPE_PTR) return true;
		if (!infer_from_type(tr, match->ptr, to->ptr, idents, vals, types, where))
			return false;
		break;
	case TYPE_SLICE:
		if (to->kind != TYPE_SLICE) return true;
		if (!infer_from_type(tr, match->slice, to->slice, idents, vals, types, where))
			return false;
		break;
	case TYPE_STRUCT:
		/* this would be difficult because match could contain #ifs and 
		   no sane person will ever write something that needs this */
		break;
	case TYPE_EXPR: {
		Type type;
		construct_resolved_builtin_type(&type, BUILTIN_TYPE);
		Value val = {0};
		val.type = to;
		if (!infer_from_expr(tr, match->expr, val, &type, where, idents, vals, types))
			return false;
	} break;
	case TYPE_ARR: {
		if (to->kind != TYPE_ARR) return true;
		Type n_type;
		construct_resolved_builtin_type(&n_type, BUILTIN_I64);
		Value val;
		val.i64 = (I64)to->arr.n;
		/* try to match match's n expr to to's value */
		if (!infer_from_expr(tr, match->arr.n_expr, val, &n_type, where, idents, vals, types))
			return false;
		if (!infer_from_type(tr, match->arr.of, to->arr.of, idents, vals, types, where))
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
static bool infer_ident_vals(Typer *tr, Type **match, Type **to, Identifier *idents, Value *vals, Type *types, Location *wheres) {
	size_t ntypes = arr_len(match);
	size_t i;
	size_t nidents = arr_len(idents);
	
	Type *t = types;
			
	for (i = 0; i < nidents; ++i) {
		memset(t, 0, sizeof *t);
		t->flags |= TYPE_IS_RESOLVED;
		t->kind = TYPE_UNKNOWN;
		++t;
	}
	
	for (i = 0; i < ntypes; ++i) {
		Location where = wheres[i];
		if (!infer_from_type(tr, *match, *to, idents, vals, types, where))
			return false;
		++match, ++to;
	}
	return true;
}

