static bool eval_block(Evaluator *ev, Block *b, Value *v);
static bool eval_expr(Evaluator *ev, Expression *e, Value *v);
static bool block_enter(Block *b, Statement *stmts);
static void block_exit(Block *b, Statement *stmts);

static void evalr_create(Evaluator *ev) {
	allocr_create(&ev->allocr);
}

static void evalr_free(Evaluator *ev) {
	allocr_free_all(&ev->allocr);
}

static inline void *evalr_malloc(Evaluator *ev, size_t bytes) {
	return allocr_malloc(&ev->allocr, bytes);
}

static size_t compiler_sizeof_builtin(BuiltinType b) {
	switch (b) {
	case BUILTIN_I8: return sizeof(I8);
	case BUILTIN_U8: return sizeof(U8);
	case BUILTIN_I16: return sizeof(I16);
	case BUILTIN_U16: return sizeof(U16);
	case BUILTIN_I32: return sizeof(I32);
	case BUILTIN_U32: return sizeof(U32);
	case BUILTIN_I64: return sizeof(I64);
	case BUILTIN_U64: return sizeof(U64);
	case BUILTIN_F32: return sizeof(F32);
	case BUILTIN_F64: return sizeof(F64);
	case BUILTIN_CHAR: return sizeof(char); /* = 1 */
	case BUILTIN_BOOL: return sizeof(bool);
	}
	assert(0);
	return 0;
}

/* size of a type at compile time */
static size_t compiler_sizeof(Type *t) {
	switch (t->kind) {
	case TYPE_BUILTIN:
		return compiler_sizeof_builtin(t->builtin);
	case TYPE_FN:
		return sizeof t->fn;
	case TYPE_PTR:
		return sizeof t->ptr;
	case TYPE_ARR:
		return compiler_sizeof(t->arr.of) * t->arr.n;
	case TYPE_TUPLE:
		return sizeof t->tuple;
	case TYPE_SLICE:
		return sizeof t->slice;
	case TYPE_VOID:
	case TYPE_UNKNOWN:
		return 0;
	}
	assert(0);
	return 0;
}

static bool builtin_truthiness(Value *v, BuiltinType b) {
	switch (b) {
	case BUILTIN_I8: return v->i8 != 0;
	case BUILTIN_I16: return v->i16 != 0;
	case BUILTIN_I32: return v->i32 != 0;
	case BUILTIN_I64: return v->i64 != 0;
	case BUILTIN_U8: return v->u8 != 0;
	case BUILTIN_U16: return v->u16 != 0;
	case BUILTIN_U32: return v->u32 != 0;
	case BUILTIN_U64: return v->u64 != 0;
	case BUILTIN_F32: return v->f32 != 0;
	case BUILTIN_F64: return v->f64 != 0;
	case BUILTIN_BOOL: return v->boolv;
	case BUILTIN_CHAR: return v->charv != 0;
	}
	assert(0); return false;
}

static bool val_truthiness(Value *v, Type *t) {
	switch (t->kind) {
	case TYPE_VOID: return false;
	case TYPE_UNKNOWN: assert(0); return false;
	case TYPE_BUILTIN: return builtin_truthiness(v, t->builtin);
	case TYPE_PTR: return v->ptr != NULL;
	case TYPE_FN: return v->fn != NULL;
	case TYPE_ARR: return t->arr.n > 0;
	case TYPE_SLICE: return v->slice.n > 0;
	case TYPE_TUPLE: break;
	}
	assert(0);
	return false;
}

static I64 val_to_i64(Value *v, BuiltinType v_type) {
	switch (v_type) {
	case BUILTIN_I8: return (I64)v->i8;
	case BUILTIN_I16: return (I64)v->i16;
	case BUILTIN_I32: return (I64)v->i32;
	case BUILTIN_I64: return (I64)v->i64;
	case BUILTIN_U8: return (I64)v->u8;
	case BUILTIN_U16: return (I64)v->u16;
	case BUILTIN_U32: return (I64)v->u32;
	case BUILTIN_U64: return (I64)v->u64;
	default: break;
	}
	assert(0);
	return 0;
}

static U64 val_to_u64(Value *v, BuiltinType v_type) {
	if (v_type == BUILTIN_U64) return v->u64;
	return (U64)val_to_i64(v, v_type);
}

static void i64_to_val(Value *v, BuiltinType v_type, I64 x) {
	switch (v_type) {
	case BUILTIN_I8:
		v->i8 = (I8)x; break;
	case BUILTIN_I16:
		v->i16 = (I16)x; break;
	case BUILTIN_I32:
		v->i32 = (I32)x; break;
	case BUILTIN_I64:
		v->i64 = (I64)x; break;
	case BUILTIN_U8:
		v->u8 = (U8)x; break;
	case BUILTIN_U16:
		v->u16 = (U16)x; break;
	case BUILTIN_U32:
		v->u32 = (U32)x; break;
	case BUILTIN_U64:
		v->u64 = (U64)x; break;
	default: assert(0); break;
	}
}

static void u64_to_val(Value *v, BuiltinType v_type, U64 x) {
	if (v_type == BUILTIN_U64)
		v->u64 = x;
	else
		i64_to_val(v, v_type, (I64)x);
}

#define builtin_casts_to_int(x)					\
	case BUILTIN_I8:							\
	vout->i8 = (I8)vin->x; break;				\
	case BUILTIN_I16:							\
	vout->i16 = (I16)vin->x; break;				\
	case BUILTIN_I32:							\
	vout->i32 = (I32)vin->x; break;				\
	case BUILTIN_I64:							\
	vout->i64 = (I64)vin->x; break;				\
	case BUILTIN_U8:							\
	vout->u8 = (U8)vin->x; break;				\
	case BUILTIN_U16:							\
	vout->u16 = (U16)vin->x; break;				\
	case BUILTIN_U32:							\
	vout->u32 = (U32)vin->x; break;				\
	case BUILTIN_U64:							\
	vout->u64 = (U64)vin->x; break

#define builtin_casts_to_num(x)					\
	builtin_casts_to_int(x);					\
	case BUILTIN_F32:							\
	vout->f32 = (F32)vin->x; break;				\
	case BUILTIN_F64:							\
	vout->f64 = (F64)vin->x; break

#define builtin_int_casts(low, up)										\
	case BUILTIN_##up:													\
	switch (to) {														\
		builtin_casts_to_num(low);										\
	case BUILTIN_CHAR: vout->charv = (char)vin->low; break;				\
	case BUILTIN_BOOL: vout->boolv = vin->low != 0; break;				\
	} break

#define builtin_float_casts(low, up)									\
	case BUILTIN_##up:													\
	switch (to) {														\
	builtin_casts_to_num(low);											\
	case BUILTIN_BOOL: vout->boolv = vin->low != 0.0f; break;			\
	case BUILTIN_CHAR:													\
		assert(0); break;												\
	} break
	
static void val_builtin_cast(Value *vin, BuiltinType from, Value *vout, BuiltinType to) {
	if (from == to) {
		*vout = *vin;
		return;
	}
	switch (from) {
		builtin_int_casts(i8, I8);
		builtin_int_casts(i16, I16);
		builtin_int_casts(i32, I32);
		builtin_int_casts(i64, I64);
		builtin_int_casts(u8, U8);
		builtin_int_casts(u16, U16);
		builtin_int_casts(u32, U32);
		builtin_int_casts(u64, U64);
		builtin_float_casts(f32, F32);
		builtin_float_casts(f64, F64);

	case BUILTIN_BOOL: vout->boolv = builtin_truthiness(vin, from); break;
	case BUILTIN_CHAR:
		switch (to) {
			builtin_casts_to_int(charv);
		case BUILTIN_CHAR: /* handled at top of func */
		case BUILTIN_F32:
		case BUILTIN_F64:
		case BUILTIN_BOOL:
			assert(0); break;
		}
		break;
	}
}

static void val_cast(Value *vin, Type *from, Value *vout, Type *to) {
	if (to->kind == TYPE_BUILTIN && to->builtin == BUILTIN_BOOL) {
		vout->boolv = val_truthiness(vin, from);
		return;
	}
	switch (from->kind) {
	case TYPE_VOID: assert(0); break;
	case TYPE_UNKNOWN: assert(0); break;
	case TYPE_TUPLE: assert(0); break;

	case TYPE_BUILTIN:
		switch (to->kind) {
		case TYPE_BUILTIN:
			val_builtin_cast(vin, from->builtin, vout, to->builtin);
			break;
		case TYPE_PTR:
			switch (from->builtin) {
			case BUILTIN_I8: vout->ptr = (void *)(U64)vin->i8; break;
			case BUILTIN_I16: vout->ptr = (void *)(U64)vin->i16; break;
			case BUILTIN_I32: vout->ptr = (void *)(U64)vin->i32; break;
			case BUILTIN_I64: vout->ptr = (void *)(U64)vin->i64; break;
			case BUILTIN_U8: vout->ptr = (void *)(U64)vin->u8; break;
			case BUILTIN_U16: vout->ptr = (void *)(U64)vin->u16; break;
			case BUILTIN_U32: vout->ptr = (void *)(U64)vin->u32; break;
			case BUILTIN_U64: vout->ptr = (void *)(U64)vin->u64; break;
			default: assert(0); break;
			}
			break;
		case TYPE_SLICE:
		case TYPE_VOID:
		case TYPE_UNKNOWN:
		case TYPE_TUPLE:
		case TYPE_FN:
		case TYPE_ARR:
			assert(0);
			break;
		}
		break;
		
	case TYPE_FN:
		switch (to->kind) {
		case TYPE_PTR:
			vout->ptr = (void *)vin->fn;
			break;
		case TYPE_FN:
			vout->fn = vin->fn;
			break;
		case TYPE_SLICE:
		case TYPE_UNKNOWN:
		case TYPE_TUPLE:
		case TYPE_VOID:
		case TYPE_ARR:
		case TYPE_BUILTIN:
			assert(0); break;
		}
		break;

	case TYPE_PTR:
		switch (to->kind) {
		case TYPE_BUILTIN:
			switch (to->builtin) {
				builtin_casts_to_int(ptr);
			case BUILTIN_BOOL:
			case BUILTIN_CHAR:
			case BUILTIN_F32:
			case BUILTIN_F64:
				assert(0); break;
			}
			break;
		case TYPE_ARR:
			vout->arr = vin->ptr;
			break;
		case TYPE_PTR:
			vout->ptr = vin->ptr;
			break;
		case TYPE_FN:
			vout->fn = vin->ptr;
			break;
		case TYPE_SLICE:
		case TYPE_UNKNOWN:
		case TYPE_TUPLE:
		case TYPE_VOID:
			assert(0);
			break;
		}
		break;

	case TYPE_ARR:
		switch (to->kind) {
		case TYPE_PTR:
			vout->ptr = vin->arr;
			break;
		case TYPE_ARR:
			vout->arr = vin->arr;
			break;
		case TYPE_SLICE:
		case TYPE_FN:
		case TYPE_UNKNOWN:
		case TYPE_TUPLE:
		case TYPE_VOID:
		case TYPE_BUILTIN:
			assert(0); break;
		}
		break;
	case TYPE_SLICE:
		switch (to->kind) {
		case TYPE_PTR:
			vout->ptr = vin->slice.data;
			break;
		case TYPE_ARR:
			vout->arr = vin->slice.data;
			break;
		case TYPE_SLICE:
			vout->slice = vin->slice;
			break;
		case TYPE_FN:
		case TYPE_UNKNOWN:
		case TYPE_TUPLE:
		case TYPE_VOID:
		case TYPE_BUILTIN:
			assert(0); break;
		}
		break;
	}
}

/* type is the underlying type, not the pointer type. */
static void eval_deref(Value *v, void *ptr, Type *type) {
	switch (type->kind) {
	case TYPE_PTR: v->ptr = *(void **)ptr; break;
	case TYPE_ARR: v->arr = *(void **)ptr; break;
	case TYPE_FN: v->fn = *(FnExpr **)ptr; break;
	case TYPE_TUPLE: v->tuple = *(Value **)ptr; break;
	case TYPE_BUILTIN:
		switch (type->builtin) {
		case BUILTIN_I8: v->i8 = *(I8 *)ptr; break;
		case BUILTIN_U8: v->u8 = *(U8 *)ptr; break;
		case BUILTIN_I16: v->i16 = *(I16 *)ptr; break;
		case BUILTIN_U16: v->u16 = *(U16 *)ptr; break;
		case BUILTIN_I32: v->i32 = *(I32 *)ptr; break;
		case BUILTIN_U32: v->u32 = *(U32 *)ptr; break;
		case BUILTIN_I64: v->i64 = *(I64 *)ptr; break;
		case BUILTIN_U64: v->u64 = *(U64 *)ptr; break;
		case BUILTIN_F32: v->f32 = *(F32 *)ptr; break;
		case BUILTIN_F64: v->f64 = *(F64 *)ptr; break;
		case BUILTIN_CHAR: v->charv = *(char *)ptr; break;
		case BUILTIN_BOOL: v->boolv = *(bool *)ptr; break;
		}
		break;
	case TYPE_SLICE:
		v->slice = *(Slice *)ptr;
		break;
	case TYPE_VOID:
	case TYPE_UNKNOWN:
		assert(0);
		break;
	}
}
/* inverse of eval_deref */
static void eval_deref_set(void *set, Value *to, Type *type) {
	switch (type->kind) {
	case TYPE_PTR: *(void **)set = to->ptr; break;
	case TYPE_ARR: *(void **)set = to->arr; break;
	case TYPE_FN: *(FnExpr **)set = to->fn; break;
	case TYPE_TUPLE: *(Value **)set = to->tuple; break;
	case TYPE_BUILTIN:
		switch (type->builtin) {
		case BUILTIN_I8: *(I8 *)set = to->i8; break;
		case BUILTIN_U8: *(U8 *)set = to->u8; break;
		case BUILTIN_I16: *(I16 *)set = to->i16; break;
		case BUILTIN_U16: *(U16 *)set = to->u16; break;
		case BUILTIN_I32: *(I32 *)set = to->i32; break;
		case BUILTIN_U32: *(U32 *)set = to->u32; break;
		case BUILTIN_I64: *(I64 *)set = to->i64; break;
		case BUILTIN_U64: *(U64 *)set = to->u64; break;
		case BUILTIN_F32: *(F32 *)set = to->f32; break;
		case BUILTIN_F64: *(F64 *)set = to->f64; break;
		case BUILTIN_CHAR: *(char *)set = to->charv; break;
		case BUILTIN_BOOL: *(bool *)set = to->boolv; break;
		}
		break;
	case TYPE_SLICE:
		*(Slice *)set = to->slice;
		break;
	case TYPE_VOID:
	case TYPE_UNKNOWN:
		assert(0);
		break;
	}
}

static bool eval_set(Evaluator *ev, Expression *set, Value *to) {
	switch (set->kind) {
	case EXPR_IDENT: {
		IdentDecl *id = ident_decl(set->ident);
		if (!(id->flags & IDECL_FLAG_HAS_VAL)) {
			err_print(set->where, "Cannot set value of run time variable at compile time.");
			return false;
		}
		id->val = *to;
	} break;
	case EXPR_UNARY_OP:
		switch (set->unary.op) {
		case UNARY_DEREF: {
			Value ptr;
			if (!eval_expr(ev, set->unary.of, &ptr)) return false;
			eval_deref_set(ptr.ptr, to, &set->type);
		} break;
		default: assert(0); break;
		}
		break;
	case EXPR_BINARY_OP:
		switch (set->binary.op) {
		case BINARY_AT_INDEX: {
			/* TODO */
		    Value arr;
			if (!eval_expr(ev, set->binary.lhs, &arr)) return false;
			Value index;
			if (!eval_expr(ev, set->binary.rhs, &index)) return false;
			U64 i;
			U64 arr_sz = set->binary.lhs->type.arr.n;
			assert(set->binary.rhs->type.kind == TYPE_BUILTIN);
			if (set->binary.rhs->type.builtin == BUILTIN_U64) {
				i = index.u64;
			} else {
				I64 signed_index = val_to_i64(&index, set->binary.rhs->type.builtin);
				if (signed_index < 0) {
					err_print(set->where, "Array out of bounds (%ld, array size = %lu)\n", (long)signed_index, (unsigned long)arr_sz);
					return false;
				}
				i = (U64)signed_index;
			}
			if (i >= arr_sz) {
				err_print(set->where, "Array out of bounds (%lu, array size = %lu)\n", (unsigned long)i, (unsigned long)arr_sz);
				return false;
			}
			eval_deref_set((char *)arr.arr + compiler_sizeof(set->binary.lhs->type.arr.of) * i, to, &set->binary.lhs->type);
		} break;
		default: break;
		}
	case EXPR_TUPLE:
		/* TODO */
		break;
	default:
		assert(0);
		break;
	}
	return true;
}

static bool eval_expr(Evaluator *ev, Expression *e, Value *v) {
	/* WARNING: macros ahead */
#define eval_unary_op_one(low, up, op)			\
	case BUILTIN_##up:							\
		v->low = (up)(op of.low); break
#define eval_unary_op_nums(builtin, op)			\
	eval_unary_op_one(i8, I8, op);				\
	eval_unary_op_one(i16, I16, op);			\
	eval_unary_op_one(i32, I32, op);			\
	eval_unary_op_one(i64, I64, op);			\
	eval_unary_op_one(u8, U8, op);				\
	eval_unary_op_one(u16, U16, op);			\
	eval_unary_op_one(u32, U32, op);			\
	eval_unary_op_one(u64, U64, op);			\
	eval_unary_op_one(f32, F32, op);			\
	eval_unary_op_one(f64, F64, op);	    

#define eval_unary_op_nums_only(op)				\
	switch (builtin) {							\
		eval_unary_op_nums(builtin, op);		\
	default: assert(0); break;					\
	}

#define eval_binary_op_one(low, up, op)			\
	case BUILTIN_##up:							\
		v->low = (up)(lhs.low op rhs.low); break
	
#define eval_binary_op_nums(builtin, op)		\
	eval_binary_op_one(i8, I8, op);				\
	eval_binary_op_one(i16, I16, op);			\
	eval_binary_op_one(i32, I32, op);			\
	eval_binary_op_one(i64, I64, op);			\
	eval_binary_op_one(u8, U8, op);				\
	eval_binary_op_one(u16, U16, op);			\
	eval_binary_op_one(u32, U32, op);			\
	eval_binary_op_one(u64, U64, op);			\
	eval_binary_op_one(f32, F32, op);			\
	eval_binary_op_one(f64, F64, op)

	
#define eval_binary_op_nums_only(op)						\
	/* fix casting to bool */
	val_cast(&lhs, &e->binary.lhs->type, &lhs, &e->type);	\
	val_cast(&rhs, &e->binary.rhs->type, &rhs, &e->type);	\
	assert(e->type.kind == TYPE_BUILTIN);					\
	switch (builtin) {										\
		eval_binary_op_nums(builtin, op);					\
	default: assert(0); break;								\
	}


#define eval_binary_bool_op_one(low, up, op)	\
	case BUILTIN_##up:							\
	v->boolv = lhs.low op rhs.low; break

#define eval_binary_bool_op_nums(builtin, op)			\
	eval_binary_bool_op_one(i8, I8, op);				\
	eval_binary_bool_op_one(i16, I16, op);				\
	eval_binary_bool_op_one(i32, I32, op);				\
	eval_binary_bool_op_one(i64, I64, op);				\
	eval_binary_bool_op_one(u8, U8, op);				\
	eval_binary_bool_op_one(u16, U16, op);				\
	eval_binary_bool_op_one(u32, U32, op);				\
	eval_binary_bool_op_one(u64, U64, op);				\
	eval_binary_bool_op_one(f32, F32, op);				\
	eval_binary_bool_op_one(f64, F64, op);

#define eval_binary_bool_op_nums_only(op)					\
	val_cast(&lhs, &e->binary.lhs->type, &lhs, &e->type);	\
	val_cast(&rhs, &e->binary.rhs->type, &rhs, &e->type);	\
	assert(e->type.kind == TYPE_BUILTIN);					\
	switch (builtin) {							\
		eval_binary_bool_op_nums(builtin, op);	\
	default:printf("%d\n",(int)builtin);		\
	assert(!"Invalid builtin to "#op); break;	\
	}
		
    
	
	switch (e->kind) {
	case EXPR_UNARY_OP: {
		Value of;
		if (e->unary.op != UNARY_ADDRESS) {
			if (!eval_expr(ev, e->unary.of, &of)) return false;
		}
		switch (e->unary.op) {
		case UNARY_ADDRESS: {
			Expression *o = e->unary.of;
			switch (o->kind) {
			case EXPR_IDENT: {
				IdentDecl *id = ident_decl(o->ident);
				if (!(id->flags & IDECL_FLAG_HAS_VAL)) {
					err_print(e->where, "Cannot take address of run time variable at compile time.");
					return false;
				}
			    v->ptr = &id->val;
			} break;
			case EXPR_UNARY_OP:
				switch (o->unary.op) {
				case UNARY_DEREF: {
					Value ptr;
					if (!eval_expr(ev, o, &ptr)) return false;
					v->ptr = ptr.ptr;
				} break;
				default: assert(0); break;
				}
				break;
			case EXPR_BINARY_OP:
				switch (o->binary.op) {
				case BINARY_AT_INDEX: {
					Value arr;
					if (!eval_expr(ev, o->binary.lhs, &arr)) return false;
					Value index;
					if (!eval_expr(ev, o->binary.rhs, &index)) return false;
					U64 i;
					U64 arr_sz = o->binary.lhs->type.arr.n;
					assert(o->binary.rhs->type.kind == TYPE_BUILTIN);
					if (o->binary.rhs->type.builtin == BUILTIN_U64) {
						i = index.u64;
					} else {
						I64 signed_index = val_to_i64(&index, o->binary.rhs->type.builtin);
						if (signed_index < 0) {
							err_print(o->where, "Array out of bounds (%ld, array size = %lu)\n", (long)signed_index, (unsigned long)arr_sz);
							return false;
						}
						i = (U64)signed_index;
					}
					if (i >= arr_sz) {
						err_print(o->where, "Array out of bounds (%lu, array size = %lu)\n", (unsigned long)i, (unsigned long)arr_sz);
						return false;
					}
				    v->ptr = ((char *)arr.arr) + compiler_sizeof(o->binary.lhs->type.arr.of) * i;
					printf("%p\n",v->ptr);
				} break;
				default: break;
				}
				break;
			default:
				assert(0);
				break;
			}
		} break;
		case UNARY_DEREF:
			eval_deref(v, of.ptr, &e->type);
			break;
		case UNARY_MINUS: {
			BuiltinType builtin = e->type.builtin;
			assert(e->type.kind == TYPE_BUILTIN);
			eval_unary_op_nums_only(-);
		} break;
		case UNARY_NOT:
			v->boolv = !val_truthiness(v, &e->unary.of->type);
			break;
		case UNARY_DEL:
			if (e->unary.of->type.kind == TYPE_PTR)
				free(of.ptr);
			else {
				assert(e->unary.of->type.kind == TYPE_ARR);
				free(of.arr);
			}
			break;
		}
	} break;
	case EXPR_BINARY_OP: {
		Value lhs, rhs;
		/* TODO(eventually): short-circuiting */
		if (e->binary.op != BINARY_SET)
			if (!eval_expr(ev, e->binary.lhs, &lhs)) return false;
		if (!eval_expr(ev, e->binary.rhs, &rhs)) return false;
		BuiltinType builtin = e->binary.lhs->type.builtin;
		switch (e->binary.op) {
		case BINARY_ADD:
			eval_binary_op_nums_only(+); break;
		case BINARY_SUB:
			eval_binary_op_nums_only(-);
			printf("%p %p\n", lhs.ptr, rhs.ptr);
			break;
		case BINARY_MUL:
			eval_binary_op_nums_only(*); break;
		case BINARY_DIV:
			eval_binary_op_nums_only(/); break;
		case BINARY_LT:
			eval_binary_bool_op_nums_only(<); break;
		case BINARY_LE:
			eval_binary_bool_op_nums_only(<=); break;
		case BINARY_GT:
			eval_binary_bool_op_nums_only(>); break;
		case BINARY_GE:
			eval_binary_bool_op_nums_only(>=); break;
		case BINARY_EQ:
			eval_binary_bool_op_nums_only(==); break;
		case BINARY_NE:
			eval_binary_bool_op_nums_only(!=); break;
		case BINARY_SET:
			if (!eval_set(ev, e->binary.lhs, &rhs)) return false;
			break;
		case BINARY_AT_INDEX: {
			U64 index;
			U64 arr_sz = e->binary.lhs->type.arr.n;
			assert(e->binary.rhs->type.kind == TYPE_BUILTIN);
			if (e->binary.rhs->type.builtin == BUILTIN_U64) {
				index = rhs.u64;
			} else {
				I64 signed_index = val_to_i64(&rhs, e->binary.rhs->type.builtin);
				if (signed_index < 0) {
					err_print(e->where, "Array out of bounds (%ld, array size = %lu)\n", (long)signed_index, (unsigned long)arr_sz);
					return false;
				}
				index = (U64)signed_index;
			}
			if (index >= arr_sz) {
				err_print(e->where, "Array out of bounds (%lu, array size = %lu)\n", (unsigned long)index, (unsigned long)arr_sz);
				return false;
			}
			eval_deref(v, (void *)((char *)(lhs.arr) + index * compiler_sizeof(&e->type)), &e->type);
		} break;
		}	
	} break;
	case EXPR_LITERAL_INT:
		assert(e->type.kind == TYPE_BUILTIN);
		u64_to_val(v, e->type.builtin, e->intl);
		break;
	case EXPR_LITERAL_FLOAT:
		assert(e->type.kind == TYPE_BUILTIN);
		if (e->type.builtin == BUILTIN_F32) {
			v->f32 = (F32)e->floatl;
		} else if (e->type.builtin == BUILTIN_F64) {
			v->f64 = (F64)e->floatl;
		} else {
			assert(0);
		}
		break;
	case EXPR_IF: {
		IfExpr *i = &e->if_;
		if (i->cond) {
			Value cond;
			if (!eval_expr(ev, i->cond, &cond)) return false;
			if (val_truthiness(&cond, &i->cond->type)) {
				if (!eval_block(ev, &i->body, v)) return false;
			} else if (i->next_elif) {
				if (!eval_expr(ev, i->next_elif, v)) return false;
			}
		} else {
			if (!eval_block(ev, &i->body, v)) return false;
		}
	} break;
	case EXPR_WHILE: {
		Value cond;
		WhileExpr *w = &e->while_;
		while (1) {
			if (w->cond) {
				if (!eval_expr(ev, w->cond, &cond)) return false;
				if (!val_truthiness(&cond, &w->cond->type))
					break;
			}
			if (!eval_block(ev, &w->body, v)) return false;
		}
	} break;
	case EXPR_BLOCK:
		if (!eval_block(ev, &e->block, v)) return false;
		break;
	case EXPR_LITERAL_BOOL:
		v->boolv = e->booll;
		break;
	case EXPR_LITERAL_CHAR:
		v->charv = e->charl;
		break;
	case EXPR_LITERAL_STR:
		v->arr = e->strl.str;
		break;
	case EXPR_CAST: {
		Value casted;
		if (!eval_expr(ev, e->cast.expr, &casted)) return false;
		val_cast(&casted, &e->cast.expr->type, v, &e->cast.type);
	} break;
	case EXPR_FN:
		v->fn = &e->fn;
		break;
	case EXPR_IDENT: {
		IdentDecl *idecl = ident_decl(e->ident);
		Declaration *d = idecl->decl;
		if (idecl->flags & IDECL_FLAG_HAS_VAL) {
			*v = idecl->val;
		} else if (d->flags & DECL_FLAG_CONST) {
			if (!(d->flags & DECL_FLAG_FOUND_VAL)) {
				if (!eval_expr(ev, &d->expr, &d->val)) return false;
				d->flags |= DECL_FLAG_FOUND_VAL;
			}
			if (d->type.kind == TYPE_TUPLE) {
				long index = 0;
				arr_foreach(d->idents, Identifier, decl_i) {
					if (*decl_i == e->ident) {
						break;
					}
					index++;
					assert(index < (long)arr_len(d->idents)); /* identifier got its declaration set to here, but it's not here */
				}
				*v = d->val.tuple[index];
			} else {
				*v = d->val;
			}
		} else {
			char *s = ident_to_str(e->ident);
			err_print(e->where, "Cannot evaluate non-constant '%s' at compile time.", s);
			free(s);
			return false;
		}
	} break;
	case EXPR_TUPLE: {
		size_t i, n = arr_len(e->tuple);
		v->tuple = evalr_malloc(ev, n * sizeof *v->tuple);
		for (i = 0; i < n; i++) {
			if (!eval_expr(ev, &e->tuple[i], &v->tuple[i]))
				return false;
		}
	} break;
	case EXPR_DIRECT: {
		DirectExpr *d = &e->direct;
		switch (d->which) {
		case DIRECT_C:
			err_print(e->where, "Cannot run C code at compile time.");
			return false;
		case DIRECT_COUNT: assert(0); return false;
		}
	} break;
	case EXPR_NEW:
		/* it's not strictly necessary to do the if here */
		if (e->new.n) {
			Value n;
			if (!eval_expr(ev, e->new.n, &n))
				return false;
			U64 n64 = val_to_u64(&n, e->new.n->type.builtin);
			v->slice.data = err_calloc(n64, compiler_sizeof(&e->new.type));
			v->slice.n = n64;
		} else {
			v->ptr = err_calloc(1, compiler_sizeof(&e->new.type));
		}
		break;
	case EXPR_CALL: {
		Value fnv;
		if (!eval_expr(ev, e->call.fn, &fnv))
			return false;
		FnExpr *fn = fnv.fn;
		/* set parameter declaration values */
		long arg = 0;
		Declaration *params = fn->params;
		fn_enter(fn);
		arr_foreach(params, Declaration, p) {
			arr_foreach(p->idents, Identifier, i) {
				IdentDecl *id = ident_decl(*i);
				Value *paramval = &id->val;
				if (!eval_expr(ev, &e->call.arg_exprs[arg], paramval))
					return false;
				id->flags |= IDECL_FLAG_HAS_VAL;
				arg++;
			}
		}
		if (!eval_block(ev, &fn->body, v)) {
			fn_exit(fn);
			return false;
		}
		fn_exit(fn);
	} break;
	}
	return true;
}

static bool eval_decl(Evaluator *ev, Declaration *d) {
	Value val = {0};
	int has_expr = d->flags & DECL_FLAG_HAS_EXPR;
	if (has_expr) {
		if (!eval_expr(ev, &d->expr, &val))
			return false;
		d->flags |= DECL_FLAG_HAS_EXPR;
	}
	long index = 0;
	arr_foreach(d->idents, Identifier, i) {
		IdentDecl *id = ident_decl(*i);
		if (has_expr && d->expr.kind == EXPR_TUPLE) {
			id->val = val.tuple[index++];
		} else if (!has_expr && d->type.kind == TYPE_ARR) {
			/* "stack" array */
			id->val.arr = err_calloc(d->type.arr.n, compiler_sizeof(d->type.arr.of));
		} else {
			id->val = val;
		}
		id->flags |= IDECL_FLAG_HAS_VAL;
	}
	return true;
}

static bool eval_stmt(Evaluator *ev, Statement *stmt) {
	switch (stmt->kind) {
	case STMT_DECL:
		if (!eval_decl(ev, &stmt->decl)) return false;
		break;
	case STMT_EXPR: {
		Value unused;
		if (!eval_expr(ev, &stmt->expr, &unused))
			return false;
	} break;
	case STMT_RET:
		/* TODO */
		break;
	}
	return true;
}

static bool eval_block(Evaluator *ev, Block *b, Value *v) {
	block_enter(b, b->stmts);
	arr_foreach(b->stmts, Statement, stmt) {
		if (!eval_stmt(ev, stmt))
			return false;
	}
	if (b->ret_expr) {
		if (!eval_expr(ev, b->ret_expr, v))
			return false;
	}
	block_exit(b, b->stmts);
	
	return true;
}
