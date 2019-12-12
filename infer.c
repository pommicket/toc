/* infers the expression of decls[idx], according to the types of the other decls */
static bool infer_expr(Typer *tr, size_t idx, Declaration *decls) {
	Declaration *decl = decls + idx;
	if (decl->flags & DECL_HAS_EXPR) return true; /* already did it */

	decl->expr.kind = EXPR_VAL;
	decl->expr.type.flags = TYPE_IS_RESOLVED;
	decl->expr.type.kind = TYPE_TYPE;
	
	decl->expr.val.type = calloc(1,sizeof (Type));
	decl->expr.val.type->kind = TYPE_BUILTIN;	
	decl->expr.val.type->builtin = BUILTIN_I64;
	decl->expr.val.type->flags = TYPE_IS_RESOLVED;
	
	decl->type = decl->expr.type;
	decl->flags |= DECL_FOUND_TYPE;
	decl->flags |= DECL_HAS_EXPR;
	return true;
}

/* infers ALL of the expressions in  the declarations */
static bool infer_exprs_in_decls(Typer *tr, Declaration *decls) {
	for (size_t idx = 0; idx < arr_len(decls); ++idx) {
		if (decls[idx].flags & DECL_INFER)
			if (!infer_expr(tr, idx, decls))
				return false;
	}
	return true;
}
