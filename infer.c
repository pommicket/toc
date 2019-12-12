/* 
match and to are dynamic arrays of equal size
find the value (and put it into val) of ident by matching match[i] to to[i], i = 0..arr_len(match)-1
all the types in match must be resolved, and all the types in to must be unresolved
*/
static bool infer(Type **match, Type **to, Identifier ident, Value *val, Type *type) {
	val->type = calloc(1,sizeof(Type));
	val->type->flags = TYPE_IS_RESOLVED;
	val->type->kind = TYPE_BUILTIN;
	val->type->builtin = BUILTIN_I64;
	type->flags = TYPE_IS_RESOLVED;
	type->kind = TYPE_TYPE;
	type->was_expr = NULL;
	return true;
}

