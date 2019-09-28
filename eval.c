typedef enum {
			  VAL_VOID,
			  VAL_INT,
			  VAL_UINT,
			  VAL_CHAR,
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
		UInteger uintv;
	    FloatVal floatv;
		bool boolv;
		char charv;
		struct Value *points_to;
		FnExpr *fn;
	};
} Value;

static bool eval_truthiness(Value *v) {
	switch (v->kind) {
	case VAL_VOID:
		assert(0);
		return false;
	case VAL_INT:
		return v->intv != 0;
	case VAL_UINT:
		return v->uintv != 0;
	case VAL_FLOAT:
		return v->floatv != 0;
	case VAL_PTR:
		return v->points_to != NULL;
	case VAL_FN:
		return v->fn != NULL;
	case VAL_BOOL:
		return v->boolv;
	case VAL_CHAR:
		return v->charv != 0;
	}
	assert(0);
	return false;
}

static inline bool val_eq(Value a, Value b) {
	if (a.kind != b.kind) return false;
	switch (a.kind) {
	case VAL_UINT: return a.uintv == b.uintv;
	case VAL_INT: return a.intv == b.intv;
	case VAL_FLOAT: return a.floatv == b.floatv;
	case VAL_BOOL: return a.boolv == b.boolv;
	case VAL_FN: return a.fn == b.fn;
	case VAL_PTR: return a.points_to == b.points_to;
	case VAL_CHAR: return a.charv == b.charv;
	case VAL_VOID: assert(0);
	}
	return false;
}

static inline bool val_lt(Value a, Value b) {
	if (a.kind != b.kind) return false;
	switch (a.kind) {
	case VAL_UINT: return a.uintv < b.uintv;
	case VAL_INT: return a.intv < b.intv;
	case VAL_FLOAT: return a.floatv < b.floatv;
	case VAL_BOOL: return a.boolv < b.boolv;
	case VAL_FN: return a.fn < b.fn;
	case VAL_PTR: return a.points_to < b.points_to;
	case VAL_CHAR: return a.charv < b.charv;
	case VAL_VOID: assert(0);
	}
	return false;
}

static bool val_cast(Location where, Value *out, Value cast, Type *type) {
	switch (type->kind) {
	case TYPE_VOID:
		out->kind = VAL_VOID;
		return true;
	case TYPE_BUILTIN:
		switch (type->builtin) {
		case BUILTIN_U8:
		case BUILTIN_U16:
		case BUILTIN_U32:
		case BUILTIN_U64:
			out->kind = VAL_UINT;
			switch (cast.kind) {
			case VAL_UINT:
				out->uintv = cast.uintv; break;
			case VAL_INT:
				out->uintv = (UInteger)cast.intv; break;
			case VAL_FLOAT:
				out->uintv = (UInteger)cast.floatv;	break;
			case VAL_BOOL:
				out->uintv = cast.boolv ? 1 : 0; break;
			case VAL_FN:
				out->uintv = (UInteger)cast.fn;	break;
			case VAL_PTR:
				out->uintv = (UInteger)cast.points_to; break;
			case VAL_CHAR:
				out->uintv = (UInteger)cast.charv; break;
			case VAL_VOID: assert(0);
			}
			break;
		case BUILTIN_I8:
		case BUILTIN_I16:
		case BUILTIN_I32:
		case BUILTIN_I64:
			out->kind = VAL_INT;
			switch (cast.kind) {
			case VAL_UINT:
				out->intv = (Integer)cast.uintv; break;
			case VAL_INT:
				out->intv = cast.intv; break;
			case VAL_FLOAT:
				out->intv = (Integer)cast.floatv;	break;
			case VAL_BOOL:
				out->intv = cast.boolv ? 1 : 0; break;
			case VAL_FN:
				out->intv = (Integer)cast.fn;	break;
			case VAL_PTR:
				out->intv = (Integer)cast.points_to; break;
			case VAL_CHAR:
				out->intv = (Integer)cast.charv; break;
			case VAL_VOID: assert(0);
			}
			break;
		case BUILTIN_F32:
		case BUILTIN_F64:
			out->kind = VAL_FLOAT;
			switch (cast.kind) {
			case VAL_UINT:
				out->floatv = (FloatVal)cast.uintv; break;
			case VAL_INT:
				out->floatv = (FloatVal)cast.intv; break;
			case VAL_FLOAT:
				out->floatv = cast.floatv; break;
			case VAL_BOOL:
				out->floatv = cast.boolv ? 1.0 : 0.0; break;
			case VAL_FN:
			case VAL_PTR:
				/* TODO: errors at type checking */
				assert(0);
				return false;
			case VAL_CHAR:
				out->floatv = (FloatVal)cast.charv; break;
			case VAL_VOID: assert(0);
			}
			break;
		case BUILTIN_BOOL:
			out->kind = VAL_BOOL;
			out->boolv = eval_truthiness(&cast);
			break;
		case BUILTIN_CHAR:
			out->kind = VAL_CHAR;
			switch (cast.kind) {
			case VAL_UINT:
				out->charv = (char)cast.uintv; break;
			case VAL_INT:
				out->charv = (char)cast.intv; break;
			case VAL_FLOAT:
				out->charv = (char)cast.floatv; break;
			case VAL_BOOL:
				out->charv = cast.boolv ? 1 : 0; break;
			case VAL_FN:
				out->charv = (char)cast.fn; break;
			case VAL_PTR:
				out->charv = (char)cast.points_to; break;
			case VAL_CHAR:
				out->charv = cast.charv; break;
			case VAL_VOID: assert(0);
			}
			break;
		case BUILTIN_TYPE_COUNT: assert(0); return false;
		}
		break;
	case TYPE_FN:
		out->kind = VAL_FN;
		switch (cast.kind) {
		case VAL_UINT:
			out->fn = (FnExpr *)cast.uintv; break;
		case VAL_INT:
			out->fn = (FnExpr *)cast.intv; break;
		case VAL_FN:
			out->fn = cast.fn; break;
		case VAL_PTR:
			out->fn = (FnExpr *)cast.points_to; break;
		case VAL_CHAR:
		case VAL_FLOAT:
		case VAL_BOOL:
			assert(0); return false;
		case VAL_VOID: assert(0);
		}
		break;
	case TYPE_TUPLE:
	case TYPE_ARR:
		/* TODO: errors at type checking */
		assert(0);
		return false;
	case TYPE_UNKNOWN:
		err_print(where, "Cannot cast to unknown type.");
		return false;
	}
	return true;
}

static bool eval_block(Block *b, Value *v) {
	v->kind = VAL_UINT;
	v->uintv = (UInteger)b;
	(void)b,(void)v;
	return true;
}

static inline void val_promote_to_float(Value *v) {
	switch (v->kind) {
	case VAL_INT:
		v->kind = VAL_FLOAT;
		v->floatv = (FloatVal)v->intv;
		break;
	case VAL_UINT:
		v->kind = VAL_FLOAT;
		v->floatv = (FloatVal)v->uintv;
		break;
	case VAL_FLOAT: break;
	default: assert(0); break;
	}
	   
}

/* NOTE: expr must be typed before it can be evaluated */
static bool eval_expr(Expression *e, Value *v) {
	switch (e->kind) {
	case EXPR_LITERAL_FLOAT:
		v->kind = VAL_FLOAT;
		v->floatv = (FloatVal)e->floatl;
		return true;
	case EXPR_LITERAL_INT:
		if (e->intl > (UInteger)INTEGER_MAX) {
			v->kind = VAL_UINT;
			v->uintv = e->intl;
		} else {
			v->kind = VAL_INT;
			v->intv = (Integer)e->intl;
		}
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
		    assert(e->type.kind == TYPE_BUILTIN);
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
		bool is_uint = type_builtin_is_uint(e->type.builtin);
		bool is_int = type_builtin_is_int(e->type.builtin);
		bool is_float = type_builtin_is_float(e->type.builtin);
		if (is_float) {
			val_promote_to_float(&lhs);
			val_promote_to_float(&rhs);
		}
		switch (e->binary.op) {
		case BINARY_PLUS:
			v->kind = lhs.kind;
			if (is_uint) {
				v->uintv = lhs.uintv + rhs.uintv;
			} else if (is_int) {
				v->intv = lhs.intv + rhs.intv;
			} else if (is_float) {
				v->floatv = lhs.floatv + rhs.floatv;
			} else assert(0);
			break;
		case BINARY_MINUS:
			v->kind = lhs.kind;
			if (is_uint) {
				v->uintv = lhs.uintv - rhs.uintv; /* TODO: will u64 - u64 => i64 be possible? */
			} else if (is_int) {
				v->intv = lhs.intv - rhs.intv;
			} else if (is_float) {
				v->floatv = lhs.floatv - rhs.floatv;
			} else assert(0);
			break;
		case BINARY_MUL:
			v->kind = lhs.kind;
			if (is_uint) {
				v->uintv = lhs.uintv * rhs.uintv;
			} else if (is_int) {
				v->intv = lhs.intv * rhs.intv;
			} else if (is_float) {
				v->floatv = lhs.floatv * rhs.floatv;
			} else assert(0);
			break;
		case BINARY_DIV:
			v->kind = lhs.kind;
			/* TODO(eventually): check div by 0 */
			if (is_uint) {
				v->uintv = lhs.uintv / rhs.uintv;
			} else if (is_int) {
				v->intv = lhs.intv / rhs.intv;
			} else if (is_float) {
				v->floatv = lhs.floatv / rhs.floatv;
			} else assert(0);
			break;
		case BINARY_EQ:
			v->kind = VAL_BOOL;
			v->boolv = val_eq(lhs, rhs);
			break;
		case BINARY_NE:
			v->kind = VAL_BOOL;
			v->boolv = !val_eq(lhs, rhs);
			break;
		case BINARY_GT:
			v->kind = VAL_BOOL;
			v->boolv = val_lt(rhs, lhs);
			break;
		case BINARY_GE:
			v->kind = VAL_BOOL;
			v->boolv = val_lt(rhs, lhs) || val_eq(lhs, rhs);
			break;
		case BINARY_LT:
			v->kind = VAL_BOOL;
			v->boolv = val_lt(lhs, rhs);
			break;
		case BINARY_LE:
			v->boolv = val_lt(lhs, rhs) || val_eq(lhs, rhs);
			break;
	    case BINARY_SET:
			v->kind = VAL_VOID;
			break;
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
		break;
	} break;
	case EXPR_FN:
		v->kind = VAL_FN;
		v->fn = &e->fn;
		break;
	case EXPR_IF: {
		IfExpr *i = &e->if_;
		if (!i->cond) {
			if (!eval_block(&i->body, v)) return false;
			break;
		}
		Value cond;
		if (!eval_expr(i->cond, &cond))
			return false;
		if (eval_truthiness(&cond)) {
			if (!eval_block(&i->body, v)) return false;
		} else if (i->next_elif) {
			if (!eval_expr(i->next_elif, v)) return false;
		} else {
			v->kind = VAL_VOID;
		}
		break;
	}
	case EXPR_BLOCK:
		return eval_block(&e->block, v);
	case EXPR_LITERAL_CHAR:
		v->kind = VAL_CHAR;
		v->charv = e->charl;
	    break;
	case EXPR_CAST: {
		Value cast_val;
		if (!eval_expr(e->cast.expr, &cast_val)) return false;
		if (!val_cast(e->where, v, cast_val, &e->cast.type)) return false;
		break;
	}
	case EXPR_DIRECT:
		switch (e->direct.which) {
		case DIRECT_C:
			err_print(e->where, "Can't run C code at compile time.");
			return false;
		case DIRECT_COUNT: assert(0); break;
		}
		break;
	case EXPR_NEW:
	case EXPR_WHILE:
	case EXPR_CALL:
	case EXPR_LITERAL_STR:
		err_print(e->where, "Not implemented yet");
		return false;
	}
	return true;
}
