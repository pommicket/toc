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
	case EXPR_LITERAL_FLOAT:
		err_print(e->where, "Expected integer, but found floating-point literal.");
		return false;
	case EXPR_LITERAL_INT:
		if (e->intl > (UInteger)INTEGER_MAX) { /* TODO: FIXME */
			err_print(e->where, "Overflow when evaluating integer.");
			return false;
		}
		*i = (Integer)e->intl;
		return true;
	case EXPR_LITERAL_STR:
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
		case BINARY_MINUS:
		case BINARY_MUL:
		case BINARY_DIV: {
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
			case BINARY_MUL:
				*i = lhs * rhs;
				return true;
			case BINARY_DIV:
				*i = lhs / rhs;
				return true;
			default: assert(0); return false;
			}
		}
	    case BINARY_SET:
		case BINARY_COMMA:
			err_print(e->where, "Expected operator which returns an integer, but got %s", binary_op_to_str(e->binary.op));
			return false;
		case BINARY_AT_INDEX:
			err_print(e->where, "Cannot get index of array at compile time yet.");
			return false;
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
		if (location_after(d->where, e->where)) {
			err_print(e->where, "Use of constant before its declaration.");
			info_print(d->where, "Declaration will be here.");
			return false;
		}
		if (!(d->flags & DECL_FLAG_CONST)) {
			err_print(e->where, "Use of non-constant identifier in a constant expression.");
			info_print(d->where, "Declaration was here.");
			return false;
		}
		if (d->type.kind != TYPE_BUILTIN || !type_builtin_is_integer(d->type.builtin)) {
			char *type_str = type_to_str(&d->type);
			err_print(e->where, "Expected integer, but identifier has type %s.", type_str);
			info_print(d->where, "Declaration was here.");
			free(type_str);
			return false;
		}
		/* TODO: tuples */
		eval_expr_as_int(&d->expr, i);
		
		return true;
	} break;
	case EXPR_FN:
		err_print(e->where, "Expected integer, but found function.");
		return false;
	case EXPR_CALL:
		err_print(e->where, "Compile time function calling not supported yet."); /* TODO */
		break;
	case EXPR_BLOCK:
		err_print(e->where, "Block eval not supported yet."); /* TODO */
		break;
	case EXPR_DIRECT:
		switch (e->direct.which) {
		case DIRECT_C:
			err_print(e->where, "Can't run C code at compile time.");
			return false;
		case DIRECT_COUNT: assert(0); break;
		}
		break;
	}
	err_print(e->where, "Not implemented yet");
	return false;
}
