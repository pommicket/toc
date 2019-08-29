/* static bool eval_expr_as_float(Expression *e, FloatLiteral *f) { */
/* 	switch (e->kind) { */
/* 	case EXPR_FLOAT_LITERAL: */
/* 		*f = e->floatl; */
/* 		return true; */
/* 	case EXPR_INT_LITERAL: */
/* 		*f = (FloatLiteral)e->intl; */
/* 		return true; */
/* 	} */
/* 	err_print(e->where, "Not implemented yet"); */
/* 	return false; */
/* } */

static bool eval_expr_as_int(Expression *e, Integer *i) {
	/* OPTIM: cache eval'd expression values? (probably only for declarations) */
	switch (e->kind) {
	case EXPR_FLOAT_LITERAL:
		err_print(e->where, "Expected integer, but found floating-point literal.");
		return false;
	case EXPR_INT_LITERAL:
		if (e->intl > (UInteger)INTEGER_MAX) { /* TODO: FIXME */
			err_print(e->where, "Overflow when evaluating integer.");
			return false;
		}
		*i = (Integer)e->intl;
		return true;
	case EXPR_STR_LITERAL:
		err_print(e->where, "Expected integer, but found string literal.");
		return false;
	case EXPR_UNARY_OP:
		switch (e->unary.op) {
		case UNARY_MINUS: {
			Integer of;
			if (!eval_expr_as_int(e->unary.of, &of)) return false;
			*i = -of;
			return true;
		}
		}
		break;
	case EXPR_BINARY_OP: {
			
		switch (e->binary.op) {
		case BINARY_PLUS:
		case BINARY_MINUS: {
			Integer lhs, rhs;
			if (!eval_expr_as_int(e->binary.lhs, &lhs)) return false;
			if (!eval_expr_as_int(e->binary.rhs, &rhs)) return false;
			switch (e->binary.op) {
			case BINARY_PLUS:
				*i = lhs + rhs;
				return true;
			case BINARY_MINUS:
				*i = lhs - rhs;
				return true;
			}
		}
		}
	} break;
	case EXPR_IDENT: {
		Identifier id = e->ident;
		IdentDecl *id_decl = ident_decl(id);
		if (!id_decl) {
			char *id_str = ident_to_str(id);
			err_print(e->where, "Undeclared identifier: %s", id_str);
			free(id_str);
			return false;
		}
		Declaration *d = id_decl->decl;
		if (!(d->flags & DECL_FLAG_CONST)) {
			err_print(e->where, "Use of non-constant identifier in a constant expression.");
			info_print(d->where, "Declaration was here.");
			return false;
		}
		if (d->type.kind != TYPE_BUILTIN || !type_builtin_is_integer(d->type.builtin)) {
			char type_str[128];
		    type_to_str(&d->type, type_str, sizeof type_str);
			err_print(e->where, "Expected integer, but identifier has type %s.", type_str);
			info_print(d->where, "Declaration was here.");
			return false;
		}
		eval_expr_as_int(&d->expr, i);
		
		return true;
	} break;
	case EXPR_FN:
		err_print(e->where, "Expected integer, but found function.");
		return false;
	}
	err_print(e->where, "Not implemented yet");
	return false;
}
