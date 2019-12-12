/* 
match and to are dynamic arrays of equal size
idents is a dyn array
find the value of each ident by matching match[i] to to[i], i = 0..arr_len(match)-1
all the types in match must be resolved, and all the types in to must be unresolved
*/
static bool infer(Type **match, Type **to, Identifier *idents, Value *vals, Type *types) {
	Value *val = vals;
	Type *type = types;
	val->type = calloc(1,sizeof(Type));
	val->type->flags = TYPE_IS_RESOLVED;
	val->type->kind = TYPE_BUILTIN;
	val->type->builtin = BUILTIN_I64;
	type->flags = TYPE_IS_RESOLVED;
	type->kind = TYPE_TYPE;
	type->was_expr = NULL;
	return true;
}

