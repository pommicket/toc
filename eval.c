typedef enum {
			  VAL_VOID,
			  VAL_INT,
			  VAL_UINT,
			  VAL_CHAR,
			  VAL_FLOAT,
			  VAL_PTR,
			  VAL_FN,
			  VAL_BOOL,
			  VAL_ARR
} ValueKind;

#define VAL_FLAG_FN_NULL 0x01 /* is this function null? */

typedef struct {
	Allocator allocr;
} Evaluator;

static void evalr_create(Evaluator *ev) {
	allocr_create(&ev->allocr);
}

static void evalr_free(Evaluator *ev) {
	allocr_free_all(&ev->allocr);
}

static inline void *evalr_malloc(Evaluator *ev, size_t bytes) {
	return allocr_malloc(&ev->allocr, bytes);
}

typedef double FloatVal;

typedef struct {
	void *data;
	UInteger n;
} ArrVal;

typedef struct Value {
    ValueKind kind;
	union {
		ValueKind arr_kind;
		ValueKind ptr_kind;
	};
	union {
		Integer intv;
		UInteger uintv;
	    FloatVal floatv;
		bool boolv;
		char charv;
		void *ptr;
		FnExpr *fn;
		ArrVal arr;
	};
} Value;

size_t sizeof_val_kind(ValueKind k) {
	Value v;
	switch (k) {
	case VAL_VOID:
		return 0;
	case VAL_INT: return sizeof v.intv;
	case VAL_UINT: return sizeof v.uintv;
	case VAL_FLOAT: return sizeof v.floatv;
	case VAL_BOOL: return sizeof v.boolv;
	case VAL_CHAR: return sizeof v.charv;
	case VAL_PTR: return sizeof v.ptr;
	case VAL_FN: return sizeof v.fn;
	case VAL_ARR: return sizeof v.arr;
	}
	assert(0);
	return 0;
}

static ValueKind type_to_val_kind(Type *t) {
	switch (t->kind) {
	case TYPE_VOID: return VAL_VOID;
	case TYPE_FN: return VAL_FN;
	case TYPE_PTR: return VAL_PTR;
	case TYPE_ARR: return VAL_ARR;
	case TYPE_BUILTIN:
		switch (t->builtin) {
		case BUILTIN_I8:
		case BUILTIN_I16:
		case BUILTIN_I32:
		case BUILTIN_I64:
			return VAL_INT;
		case BUILTIN_U8:
		case BUILTIN_U16:
		case BUILTIN_U32:
		case BUILTIN_U64:
			return VAL_UINT;
		case BUILTIN_F32:
		case BUILTIN_F64:
			return VAL_FLOAT;
		case BUILTIN_CHAR:
			return VAL_CHAR;
		case BUILTIN_BOOL:
			return VAL_BOOL;
		case BUILTIN_TYPE_COUNT: break;
		}
		break;
	case TYPE_TUPLE: /* TODO */
	case TYPE_UNKNOWN:
		break;
	}
	assert(0);
	return VAL_VOID;
}

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
		return v->ptr != NULL;
	case VAL_FN:
		return v->fn != NULL;
	case VAL_BOOL:
		return v->boolv;
	case VAL_CHAR:
		return v->charv != 0;
	case VAL_ARR:
		return v->arr.n != 0;
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
	case VAL_PTR: return a.ptr == b.ptr;
	case VAL_CHAR: return a.charv == b.charv;
	case VAL_ARR:
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
	case VAL_PTR: return a.ptr < b.ptr;
	case VAL_CHAR: return a.charv < b.charv;
	case VAL_ARR:
	case VAL_VOID: assert(0);
	}
	return false;
}

static bool val_cast(Location where, Value *out, Value cast, Type *type) {
	/* TODO: errors at type checking */
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
				out->uintv = (UInteger)cast.ptr; break;
			case VAL_CHAR:
				out->uintv = (UInteger)cast.charv; break;
			case VAL_ARR:
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
				out->intv = (Integer)cast.ptr; break;
			case VAL_CHAR:
				out->intv = (Integer)cast.charv; break;
			case VAL_ARR:
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
			case VAL_CHAR:
				out->floatv = (FloatVal)cast.charv; break;
			case VAL_VOID:
			case VAL_FN:
			case VAL_ARR:
			case VAL_PTR:
				assert(0);
				return false;
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
			case VAL_BOOL:
				out->charv = cast.boolv ? 1 : 0; break;
			case VAL_CHAR:
				out->charv = cast.charv; break;
			case VAL_FLOAT:
			case VAL_FN:
			case VAL_PTR:
			case VAL_ARR:
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
			out->fn = (FnExpr *)cast.ptr; break;
		case VAL_CHAR:
		case VAL_FLOAT:
		case VAL_BOOL:
		case VAL_ARR:
		case VAL_VOID:
			assert(0); return false;
		}
		break;
	case TYPE_PTR:
		out->kind = VAL_PTR;
		out->ptr_kind = type_to_val_kind(type->ptr.of);
		switch (cast.kind) {
		case VAL_INT:
			out->ptr = (void *)cast.intv; break;
		case VAL_UINT:
			out->ptr = (void *)cast.uintv; break;
		case VAL_FN:
			out->ptr = (void *)cast.fn; break;
		case VAL_PTR:
			out->ptr = cast.ptr; break;
		case VAL_ARR:
			out->ptr = cast.arr.data; break;
		case VAL_CHAR:
		case VAL_VOID:
		case VAL_FLOAT:
		case VAL_BOOL:
			assert(0); return false;
		}
		return true;
	case TYPE_ARR:
		out->kind = VAL_ARR;
		out->arr_kind = type_to_val_kind(type->arr.of);
		out->arr.n = type->arr.n;
		switch (cast.kind) {
		case VAL_PTR:
			out->arr.data = cast.ptr;
			break;
		case VAL_ARR:
			out->arr.data = cast.arr.data;
			break;
		case VAL_FN:
		case VAL_INT:
		case VAL_UINT:
		case VAL_CHAR:
		case VAL_VOID:
		case VAL_FLOAT:
		case VAL_BOOL:
			assert(0); return false;
		}
		return true;
	case TYPE_TUPLE:
		/* TODO: error at type checking */
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
static bool eval_expr(Evaluator *ev, Expression *e, Value *v) {
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
		break;
	case EXPR_LITERAL_BOOL:
		v->kind = VAL_BOOL;
		v->boolv = e->booll;
		break;
	case EXPR_LITERAL_CHAR:
		v->kind = VAL_CHAR;
		v->charv = e->charl;
	    break;
	case EXPR_LITERAL_STR:
		v->kind = VAL_ARR;
		v->arr_kind = VAL_CHAR;
		v->arr.n = e->strl.len;
		v->arr.data = e->strl.str;
		break;
	case EXPR_UNARY_OP: {
		Expression *of_expr = e->unary.of;
		Value of;
		if (!eval_expr(ev, of_expr, &of)) return false;
		switch (e->unary.op) {
		case UNARY_MINUS: {
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
			break;
		}
		case UNARY_NOT:
			v->kind = VAL_BOOL;
			v->boolv = !eval_truthiness(&of);
		    return true;
		case UNARY_ADDRESS: {
			v->kind = VAL_PTR;
			get_ptr:
			switch (of_expr->kind) {
			case EXPR_UNARY_OP:
				switch (of_expr->unary.op) {
				case UNARY_DEREF:
					/* &*x */
					of_expr = of_expr->unary.of;
					goto get_ptr;
				default: assert(0); return false;
				}
				break;
			case EXPR_IDENT:
				err_print(e->where, "Cannot get address of identifiers at compile time yet.");
			    return false;
			case EXPR_BINARY_OP:
				switch (of_expr->binary.op) {
				case BINARY_AT_INDEX: {
					Expression *lhs_expr = of_expr->binary.lhs;
					Expression *rhs_expr = of_expr->binary.rhs;
					Value lhs, rhs;
					if (!eval_expr(ev, lhs_expr, &lhs)
						|| !eval_expr(ev, rhs_expr, &rhs))
						return false;
					assert(lhs.kind == VAL_ARR && (rhs.kind == VAL_INT || rhs.kind == VAL_UINT));
					v->ptr = (char *)lhs.arr.data + (Integer)sizeof_val_kind(lhs.arr_kind)
						* (rhs.kind == VAL_INT ? rhs.intv : (Integer)rhs.uintv);
				} break;
				default: assert(0); return false;
				}
				break;
				
			default: assert(0); return false;
			}
		} break;
		case UNARY_DEREF: {
			v->kind = type_to_val_kind(&e->type);
			switch (v->kind) {
			case VAL_INT:
				v->intv = *(Integer *)of.ptr; break;
			case VAL_UINT:
				v->uintv = *(UInteger *)of.ptr; break;
			case VAL_FLOAT:
				v->floatv = *(FloatVal *)of.ptr; break;
			case VAL_BOOL:
				v->boolv = *(bool *)of.ptr; break;
			case VAL_CHAR:
				v->charv = *(char *)of.ptr; break;
			case VAL_ARR:
				v->arr = *(ArrVal *)of.ptr; break;
			case VAL_PTR:
				v->ptr = *(void **)of.ptr; break;
			case VAL_FN:
				v->fn = *(FnExpr **)of.ptr; break;
			case VAL_VOID:
				assert(0); return false;
			/* case VAL_INT: */
			/* 	v->intv = *(Integer *)of.ptr; */
			/* 	break; */
					
			}
		    break;
		} break;
		case UNARY_DEL:	/* TODO */
			assert(0);
			break;
		}
	} break;
	case EXPR_BINARY_OP: {
		Value lhs, rhs;
		/* NOTE: this will need to change for short-circuiting */
		if (!eval_expr(ev, e->binary.lhs, &lhs)) return false;
		if (!eval_expr(ev, e->binary.rhs, &rhs)) return false;
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
		case BINARY_AT_INDEX: {
			assert(lhs.kind == VAL_ARR);
			assert(rhs.kind == VAL_INT || rhs.kind == VAL_UINT);
			/* TODO: optional bounds checking */
			UInteger index = rhs.kind == VAL_INT ? (UInteger)rhs.intv : rhs.uintv;
			void *arr = lhs.arr.data;
			v->kind = lhs.arr_kind;
			switch (lhs.arr_kind) {
			case VAL_INT:
				v->intv = ((Integer *)arr)[index]; break;
			case VAL_UINT:
				v->uintv = ((UInteger *)arr)[index]; break;
			case VAL_FLOAT:
				v->floatv = ((FloatVal *)arr)[index]; break;
			case VAL_CHAR:
				v->charv = ((char *)arr)[index]; break;
			case VAL_BOOL:
				v->boolv = ((bool *)arr)[index]; break;
			case VAL_PTR:
				v->ptr = ((Value **)arr)[index]; break;
			case VAL_ARR:
				v->arr = ((ArrVal *)arr)[index]; break;
			case VAL_FN:
				v->fn = ((FnExpr **)arr)[index]; break;
			case VAL_VOID:
				assert(0);
				return false;
			}
		    break;
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
			d->val = evalr_malloc(ev, sizeof *d->val); /* OPTIM */
			if (!eval_expr(ev, &d->expr, d->val))
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
		if (!eval_expr(ev, i->cond, &cond))
			return false;
		if (eval_truthiness(&cond)) {
			if (!eval_block(&i->body, v)) return false;
		} else if (i->next_elif) {
			if (!eval_expr(ev, i->next_elif, v)) return false;
		} else {
			v->kind = VAL_VOID;
		}
		break;
	}
	case EXPR_BLOCK:
		return eval_block(&e->block, v);
	case EXPR_CAST: {
		Value cast_val;
		if (!eval_expr(ev, e->cast.expr, &cast_val)) return false;
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
		v->kind = VAL_PTR;
		v->ptr_kind = type_to_val_kind(&e->new.type);
		v->ptr = err_calloc(1, sizeof_val_kind(v->ptr_kind)); /* TODO(eventually): get this to work even if NULL ptrs aren't 0 */
		break;
	case EXPR_WHILE:
	case EXPR_CALL:
		err_print(e->where, "Not implemented yet");
		return false;
	}
	return true;
}

