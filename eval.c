typedef struct Value {
	union {
		Integer intv;
		Floating floatv;
		struct Value *points_to;
		FnExpr fn;
	};
} Value;

/* NOTE: expr must be typed before it can be evaluated */
static bool eval_expr(Expression *e, Value *v) { 
	/* TODO: cache eval'd expression values (probably only needed for declarations) */
	switch (e->kind) {
	case EXPR_LITERAL_FLOAT:
		v->floatv = e->floatl;
		return false;
	case EXPR_LITERAL_INT:
		if (e->intl > (UInteger)INTEGER_MAX) { /* TODO: FIXME */
			err_print(e->where, "Overflow when evaluating integer.");
			return false;
		}
		v->intv = (Integer)e->intl;
		return true;
	case EXPR_LITERAL_STR:
		err_print(e->where, "not implemented yet"); /* TODO */
		return false;
	case EXPR_UNARY_OP: {
		Expression *of_expr = e->unary.of;
		switch (e->unary.op) {
		case UNARY_MINUS: {
			Value of;
			if (!eval_expr(of_expr, &of)) return false;
		    assert(e->type.kind != TYPE_BUILTIN);
			if (type_builtin_is_integer(e->type.builtin)) {
				v->intv = -of.intv;
			} else if (type_builtin_is_floating(e->type.builtin)) {
				v->floatv = -of.floatv;
			} else {
				err_print(e->where, "negation of non-numerical types not supported in evaluator yet.");
				return false;
			}
			return true;
		}
		case UNARY_ADDRESS:
			v->points_to = malloc(sizeof *v->points_to); /* OPTIM */
			return eval_expr(e->unary.of, v->points_to);
		case UNARY_DEREF: {
			Value ptr;
			if (!eval_expr(of_expr, &ptr)) return false;
			*v = *ptr.points_to;
			return true;
		} break;
		}
	} break;
	case EXPR_BINARY_OP: {
		Value lhs, rhs;
		/* NOTE: this will need to change for short-circuiting */
		if (!eval_expr(e->binary.lhs, &lhs)) return false;
		if (!eval_expr(e->binary.rhs, &rhs)) return false;
		bool is_int = type_builtin_is_integer(e->type.builtin);
		bool is_float = type_builtin_is_floating(e->type.builtin);
		switch (e->binary.op) {
		case BINARY_PLUS:
			if (is_int) {
				v->intv = lhs.intv + rhs.intv;
			} else if (is_float) {
				v->floatv = lhs.floatv + rhs.floatv;
			} else assert(0);
			return true;
		case BINARY_MINUS:
			if (is_int) {
				v->intv = lhs.intv - rhs.intv;
			} else if (is_float) {
				v->floatv = lhs.floatv - rhs.floatv;
			} else assert(0);
			return true;
		case BINARY_MUL:
			if (is_int) {
				v->intv = lhs.intv * rhs.intv;
			} else if (is_float) {
				v->floatv = lhs.floatv * rhs.floatv;
			} else assert(0);
			return true;
		case BINARY_DIV:
			/* TODO(eventually): check div by 0 */
			if (is_int) {
				v->intv = lhs.intv / rhs.intv;
			} else if (is_float) {
				v->floatv = lhs.floatv / rhs.floatv;
			} else assert(0);
			return true;
	    case BINARY_SET:
			return true;
		case BINARY_COMMA:
			err_print(e->where, "tuples not supported at compile time yet.");
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
		/* TODO: tuples */
		if (!d->val) {
			d->val = err_malloc(sizeof *d->val); /* OPTIM */
			if (!eval_expr(&d->expr, d->val))
				return false;
		}
		*v = *d->val;
		return true;
	} break;
	case EXPR_FN:
		v->fn = *e->fn;
		return true;
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
