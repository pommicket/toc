/* infers */
static bool infer_expr(Typer *tr, Expression *e, Declaration *decls,
					  Expression *exprs) {
	e->kind = EXPR_VAL;
	Value *val = &e->val;
	val->type = malloc(sizeof *val->type);
	memset(val->type, 0, sizeof *val->type);
	val->type->kind = TYPE_BUILTIN;
	val->type->builtin = BUILTIN_I64;
	val->type->flags = TYPE_IS_RESOLVED;
	memset(&e->type, 0, sizeof e->type);
	e->type.kind = TYPE_TYPE;
	e->type.flags = TYPE_IS_RESOLVED;
	
	return true;
}
