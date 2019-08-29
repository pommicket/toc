static bool eval_expr_as_float(Expression *e, FloatLiteral *f) {
	switch (e->kind) {
	case EXPR_FLOAT_LITERAL:
		*f = e->floatl;
		return true;
	case EXPR_INT_LITERAL:
		*f = (FloatLiteral)e->intl;
		return true;
	}
	err_print(e->where, "Not implemented yet");
	return false;
}

static bool eval_expr_as_int(Expression *e, IntLiteral *i) {
	switch (e->kind) {
	case EXPR_FLOAT_LITERAL:
		err_print(e->where, "Expected integer, but found floating-point literal.");
		return false;
	case EXPR_INT_LITERAL:
		*i = e->intl;
		return true;
	}
	err_print(e->where, "Not implemented yet");
	return false;
}
