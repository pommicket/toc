typedef enum {
			  VAL_VOID,
			  VAL_INT,
			  VAL_FLOAT,
			  VAL_PTR,
			  VAL_FN,
			  VAL_BOOL
} ValueKind;

#define VAL_FLAG_FN_NULL 0x01 /* is this function null? */

typedef double FloatVal;

typedef struct Value {
	ValueKind kind;
	union {
		Integer intv;
	    FloatVal floatv;
		struct Value *points_to;
		FnExpr *fn;
		bool boolv;
	};
} Value;

static bool eval_truthiness(Value *v) {
	switch (v->kind) {
	case VAL_VOID:
		assert(0);
		return false;
	case VAL_INT:
		return v->intv != 0;
	case VAL_FLOAT:
		return v->floatv != 0;
	case VAL_PTR:
		return v->points_to != NULL;
	case VAL_FN:
		return v->fn != NULL;
	case VAL_BOOL:
		return v->boolv;
	}
	assert(0);
	return false;
}

/* NOTE: expr must be typed before it can be evaluated */
static bool eval_expr(Expression *e, Value *v) {
	switch (e->kind) {
	case EXPR_LITERAL_FLOAT:
		v->kind = VAL_FLOAT;
		v->floatv = (FloatVal)e->floatl;
		return true;
	case EXPR_LITERAL_INT:
		v->kind = VAL_INT;
		if (e->intl > (UInteger)INTEGER_MAX) { /* TODO: FIXME */
			err_print(e->where, "Overflow when evaluating integer.");
			return false;
		}
		v->intv = (Integer)e->intl;
		return true;
	case EXPR_LITERAL_BOOL:
		v->kind = VAL_BOOL;
		v->boolv = e->booll;
		return true;
	case EXPR_UNARY_OP: {
		Expression *of_expr = e->unary.of;
		switch (e->unary.op) {
		case UNARY_MINUS: {
			Value of;
			if (!eval_expr(of_expr, &of)) return false;
		    assert(e->type.kind != TYPE_BUILTIN);
			v->kind = of.kind;
			if (v->kind == VAL_INT) {
				v->intv = -of.intv;
			} else if (v->kind == VAL_FLOAT) {
				v->floatv = -of.floatv;
			} else {
			    assert(0);
				return false;
			}
			return true;
		}
		case UNARY_NOT:
			v->kind = VAL_BOOL;
			v->boolv = !eval_truthiness(v);
		    return true;
		case UNARY_ADDRESS:
			v->kind = VAL_PTR;
			v->points_to = err_malloc(sizeof *v->points_to); /* OPTIM */
			return eval_expr(e->unary.of, v->points_to);
		case UNARY_DEREF: {
			Value ptr;
			if (!eval_expr(of_expr, &ptr)) return false;
			*v = *ptr.points_to;
			return true;
		} break;
		case UNARY_DEL:	/* TODO */
			assert(0);
			break;
		}
	} break;
	case EXPR_BINARY_OP: {
		Value lhs, rhs;
		/* NOTE: this will need to change for short-circuiting */
		if (!eval_expr(e->binary.lhs, &lhs)) return false;
		if (!eval_expr(e->binary.rhs, &rhs)) return false;
		if (e->type.kind != TYPE_BUILTIN) {
			err_print(e->where, "Operators can only be applied to builtin types.");
			return false;
		}
		bool is_int = type_builtin_is_integer(e->type.builtin);
		bool is_float = type_builtin_is_floating(e->type.builtin);
		bool is_bool = e->type.builtin == BUILTIN_BOOL;
		switch (e->binary.op) {
		case BINARY_PLUS:
			v->kind = lhs.kind;
			if (is_int) {
				v->intv = lhs.intv + rhs.intv;
			} else if (is_float) {
				v->floatv = lhs.floatv + rhs.floatv;
			} else assert(0);
			return true;
		case BINARY_MINUS:
			v->kind = lhs.kind;
			if (is_int) {
				v->intv = lhs.intv - rhs.intv;
			} else if (is_float) {
				v->floatv = lhs.floatv - rhs.floatv;
			} else assert(0);
			return true;
		case BINARY_MUL:
			v->kind = lhs.kind;
			if (is_int) {
				v->intv = lhs.intv * rhs.intv;
			} else if (is_float) {
				v->floatv = lhs.floatv * rhs.floatv;
			} else assert(0);
			return true;
		case BINARY_DIV:
			v->kind = lhs.kind;
			/* TODO(eventually): check div by 0 */
			if (is_int) {
				v->intv = lhs.intv / rhs.intv;
			} else if (is_float) {
				v->floatv = lhs.floatv / rhs.floatv;
			} else assert(0);
			return true;
		case BINARY_EQ:
			v->kind = VAL_BOOL;
			if (is_int) {
				v->boolv = lhs.intv == rhs.intv;
			} else if (is_float) {
				v->boolv = lhs.floatv == rhs.floatv;
			} else if (is_bool) {
				v->boolv = lhs.boolv == rhs.boolv;
			} else assert(0);
			return true;
		case BINARY_NE:
			v->kind = VAL_BOOL;
			if (is_int) {
				v->boolv = lhs.intv != rhs.intv;
			} else if (is_float) {
				v->boolv = lhs.floatv != rhs.floatv;
			} else if (is_bool) {
				v->boolv = lhs.boolv != rhs.boolv;
			} else assert(0);
			return true;
		case BINARY_GT:
			v->kind = VAL_BOOL;
			if (is_int) {
				v->boolv = lhs.intv > rhs.intv;
			} else if (is_float) {
				v->boolv = lhs.floatv > rhs.floatv;
			} else assert(0);
			return true;
		case BINARY_GE:
			v->kind = VAL_BOOL;
			if (is_int) {
				v->boolv = lhs.intv >= rhs.intv;
			} else if (is_float) {
				v->boolv = lhs.floatv >= rhs.floatv;
			} else assert(0);
			return true;
		case BINARY_LT:
			v->kind = VAL_BOOL;
			if (is_int) {
				v->boolv = lhs.intv < rhs.intv;
			} else if (is_float) {
				v->boolv = lhs.floatv < rhs.floatv;
			} else assert(0);
			return true;
		case BINARY_LE:
			v->kind = VAL_BOOL;
			if (is_int) {
				v->boolv = lhs.intv <= rhs.intv;
			} else if (is_float) {
				v->boolv = lhs.floatv <= rhs.floatv;
			} else assert(0);
			return true;
	    case BINARY_SET:
			v->kind = VAL_VOID;
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
		if (!d->val) {
			d->val = err_malloc(sizeof *d->val); /* OPTIM */
			if (!eval_expr(&d->expr, d->val))
				return false;
		}
		*v = *d->val;
		return true;
	} break;
	case EXPR_FN:
		v->kind = VAL_FN;
		v->fn = &e->fn;
		return true;
	case EXPR_CAST:
	case EXPR_IF:
	case EXPR_WHILE:
	case EXPR_CALL:
	case EXPR_BLOCK:
	case EXPR_LITERAL_STR:
	case EXPR_LITERAL_CHAR:
	case EXPR_NEW:{
		err_print(e->where, "operation not supported at compile time yet."); /* TODO */
	} break;
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
