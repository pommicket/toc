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

typedef union Value {
	U8 u8;
	U16 u16;
	U32 u32;
	U64 u64;
	I8 i8;
	I16 i16;
	I32 i32;
	I64 i64;
	bool boolv;
	char charv;
	float f32;
	double f64;
	FnExpr *fn;
	void *arr;
	void *ptr;
} Value;

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
	case BUILTIN_TYPE_COUNT: assert(0); return false;
	}
}

static bool val_truthiness(Value *v, Type *t) {
	switch (t->kind) {
	case TYPE_VOID: return false;
	case TYPE_UNKNOWN: assert(0); return false;
	case TYPE_BUILTIN: return builtin_truthiness(v, t->builtin);
	case TYPE_PTR: return v->ptr != NULL;
	case TYPE_FN: return v->fn != NULL;
	case TYPE_ARR: return t->arr.n > 0;
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
	case BUILTIN_TYPE_COUNT: assert(0); break;							\
	} break

#define builtin_float_casts(low, up)									\
	case BUILTIN_##up:													\
	switch (to) {														\
	builtin_casts_to_num(low);											\
	case BUILTIN_BOOL: vout->boolv = vin->low != 0.0f; break;			\
	case BUILTIN_CHAR:													\
	case BUILTIN_TYPE_COUNT: assert(0); break;							\
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

	case BUILTIN_BOOL: vout->boolv = builtin_truthiness(vin, from);
	case BUILTIN_CHAR:
		switch (to) {
			builtin_casts_to_int(charv);
		case BUILTIN_CHAR: /* handled at top of func */
		case BUILTIN_F32:
		case BUILTIN_F64:
		case BUILTIN_BOOL:
		case BUILTIN_TYPE_COUNT:
			assert(0); break;
		}
		break;
	case BUILTIN_TYPE_COUNT: assert(0); break;
	}
}

static void val_cast(Value *vin, Type *from, Value *vout, Type *to) {
	switch (from->kind) {
	case TYPE_VOID: assert(0); break;
	case TYPE_UNKNOWN: assert(0); break;
	case TYPE_TUPLE: assert(0); break;

	case TYPE_BUILTIN:
		switch (to->kind) {
		case TYPE_VOID: assert(0); break;
		case TYPE_UNKNOWN: assert(0); break;
		case TYPE_TUPLE: assert(0); break;
		case TYPE_BUILTIN:
			val_builtin_cast(vin, from->builtin, vout, to->builtin);
			break;
		}
	}
}

static void eval_expr(Evaluator *ev, Expression *e, Value *v) {
	/* WARNING: macros ahead */
#define eval_unary_op_one(low, up, op)			\
	case BUILTIN_##up:							\
	v->low = op of.low; break
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
		v->low = lhs.low op rhs.low; break
	
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

#define eval_binary_op_nums_only(op)			\
	switch (builtin) {							\
		eval_binary_op_nums(builtin, op);		\
	default: assert(0); break;					\
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

#define eval_binary_bool_op_nums_only(op)		\
	switch (builtin) {							\
		eval_binary_bool_op_nums(builtin, op);	\
		default: assert(0); break;				\
	}
		
    
	
	switch (e->kind) {
	case EXPR_UNARY_OP: {
		Value of;
		eval_expr(ev, e->unary.of, &of);
		switch (e->unary.op) {
		case UNARY_MINUS: {
			BuiltinType builtin = e->type.builtin;
			assert(e->type.kind == TYPE_BUILTIN);
			eval_unary_op_nums_only(-);
		} break;
		case UNARY_NOT:
			v->boolv = !val_truthiness(v, &e->unary.of->type);
			break;
		}
	} break;
	case EXPR_BINARY_OP: {
		Value lhs, rhs;
		/* TODO(eventually): short-circuiting */
		eval_expr(ev, e->binary.lhs, &lhs);
		eval_expr(ev, e->binary.rhs, &rhs);
		/* OPTIM: this is not ideal, but 5+3.7 will be 5:int+3.7:f32 right now */
		val_cast(&lhs, &e->binary.lhs->type, &lhs, &e->type);
		val_cast(&rhs, &e->binary.rhs->type, &rhs, &e->type);
		BuiltinType builtin = e->type.builtin;
		assert(e->type.kind == TYPE_BUILTIN);
		switch (e->binary.op) {
		case BINARY_ADD:
			eval_binary_op_nums_only(+); break;
		case BINARY_SUB:
			eval_binary_op_nums_only(-); break;
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
		eval_expr(ev, e->cast.expr, &casted);
		val_cast(&casted, &e->cast.expr->type, v, &e->cast.type);
	} break;
	}
}
