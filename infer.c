/* if check_curr, this will check against the current value of t */
static bool infer_expr(Expression *e) {
	Type *t = &e->type;
	switch (e->kind) {
	case EXPR_INT_LITERAL:
	    t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_I64;
		break;
	case EXPR_FLOAT_LITERAL:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_FLOAT;
		break;
	}
	return true;
}

static bool type_eq(Type *a, Type *b) {
	return true; /* TODO */
}
	
static bool infer_decl(Declaration *d) {
	if (d->flags & DECL_FLAG_FOUND_TYPE) return true;
	if (!infer_expr(&d->expr)) return false;
	if (d->flags & DECL_FLAG_INFER_TYPE) {
		d->type = d->expr.type;
	} else {
		if (!type_eq(&d->type, &d->expr.type)) {
			/* TODO more helpful error */
			err_print(d->where, "Type mismatch");
			return false;
		}
	}
	d->flags |= DECL_FLAG_FOUND_TYPE;
	return true;
}
