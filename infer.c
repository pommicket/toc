static bool infer_from_type(Type *match, Type *to, Identifier *idents, Value *vals, Type *types) {
	/* match resolved, to unresolved */
	switch (to->kind) {
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_BUILTIN:
	case TYPE_TYPE:
		break; /* nothing we can do here */
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

