static bool infer_from_expr(Expression *match, Expression *to, Identifier *idents, Value *vals, Type *types) {
	return true;
}

/* if match is not the same kind of type as to, returns true */
static bool infer_from_type(Type *match, Type *to, Identifier *idents, Value *vals, Type *types) {
    assert(to->flags & TYPE_IS_RESOLVED);
	
	switch (to->kind) {
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_BUILTIN:
	case TYPE_TYPE:
		break; /* nothing we can do here */
	case TYPE_TUPLE: {
		if (match->kind != TYPE_TUPLE) return true;
		if (arr_len(match->tuple) != arr_len(to->tuple)) return true;
	    Type *b = to->tuple;
		arr_foreach(match->tuple, Type, a) {
			if (!infer_from_type(a, b, idents, vals, types))
				return false;
			++b;
		}
	} break;
	case TYPE_FN: {
		if (match->kind != TYPE_FN) return true;
		if (arr_len(match->fn.types) != arr_len(to->fn.types)) return true;
	    size_t i, len = arr_len(match->fn.types);
		for (i = 0; i < len; ++i) {
			if (match->fn.constness[i] != to->fn.constness[i])
				return true;
			if (!infer_from_type(&match->fn.types[i], &to->fn.types[i], idents, vals, types))
				return false;
		}
	} break;
	case TYPE_PTR:
		if (match->kind != TYPE_PTR) return true;
		if (!infer_from_type(match->ptr, to->ptr, idents, vals, types))
			return false;
	    break;
	case TYPE_SLICE:
		if (match->kind != TYPE_SLICE) return true;
		if (!infer_from_type(match->slice, to->slice, idents, vals, types))
			return false;
		break;
	case TYPE_STRUCT: {
		if (match->kind != TYPE_STRUCT) return true;
		Field *fields_m = match->struc->fields;
		Field *fields_t = to->struc->fields;
		size_t i, len = arr_len(fields_m);
		if (len != arr_len(fields_t)) return true;
		for (i = 0; i < len; ++i) {
			if (!infer_from_type(fields_m[i].type, fields_t[i].type, idents, vals, types))
				return false;
		}
	} break;
	case TYPE_EXPR:
		if (to->was_expr)
			if (!infer_from_expr(match->expr, to->was_expr, idents, vals, types))
				return false;
		break;
	case TYPE_ARR: {
		if (match->kind != TYPE_ARR) return true;
		Expression match_n_expr = {0};
		match_n_expr.kind = EXPR_LITERAL_INT;
		match_n_expr.intl = match->arr.n;
		match_n_expr.flags = EXPR_FOUND_TYPE;
		Type *n_type = &match_n_expr.type;
		n_type->kind = TYPE_BUILTIN;
		n_type->builtin = BUILTIN_I64;
		n_type->flags = TYPE_IS_RESOLVED;
		if (!infer_from_expr(&match_n_expr, to->arr.n_expr, idents, vals, types))
			return false;
		if (!infer_from_type(match->arr.of, to->slice, idents, vals, types))
			return false;
	} break;
	}
	return true;
}

/* 
match and to are dynamic arrays of equal size
idents is a dyn array
find the value of each ident by matching match[i] to to[i], i = 0..arr_len(match)-1
all the types in match must be resolved, and all the types in to must be unresolved
*/
static bool infer_ident_vals(Type **match, Type **to, Identifier *idents, Value *vals, Type *types) {
	size_t ntypes = arr_len(match);
	size_t i;
	size_t nidents = arr_len(idents);
	
	Type *t = types;
			
	for (i = 0; i < nidents; ++i) {
		t->kind = TYPE_UNKNOWN;
		++t;
	}
	
	for (i = 0; i < ntypes; ++i) {
		if (!infer_from_type(*match, *to, idents, vals, types))
			return false;
		++match, ++to;
	}



#if 0 /* TODO DELME */
	Value *val = vals;
	Type *type = types;
	val->type = calloc(1,sizeof(Type));
	val->type->flags = TYPE_IS_RESOLVED;
	val->type->kind = TYPE_BUILTIN;
	val->type->builtin = BUILTIN_I64;
	type->flags = TYPE_IS_RESOLVED;
	type->kind = TYPE_TYPE;
	type->was_expr = NULL;
#endif
	return true;
}

