#if COMPILE_TIME_FOREIGN_FN_SUPPORT
static bool foreign_call(FnExpr *fn, Type *fn_type, Value *args, Location call_where) {
	assert(fn->flags & FN_EXPR_FOREIGN);
	/* TODO */
	return true;
}
#else
static bool foreign_call(FnExpr *fn, Type *fn_type, Value *args, Location call_where) {
	(void)fn; (void)fn_type; (void)args;
	err_print(call_where, "You have not compiled toc with compile time foreign function support.");
	return false;
}
#endif
