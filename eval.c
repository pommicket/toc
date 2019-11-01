static bool types_block(Typer *tr, Block *b);
static bool types_decl(Typer *tr, Declaration *d);
static size_t compiler_sizeof(Type *t);
static bool eval_block(Evaluator *ev, Block *b, Type *t, Value *v);
static bool eval_expr(Evaluator *ev, Expression *e, Value *v);
static bool block_enter(Block *b, Statement *stmts, U32 flags);
static void block_exit(Block *b, Statement *stmts);

static void evalr_create(Evaluator *ev, Typer *tr) {
	allocr_create(&ev->allocr);
	ev->returning = NULL;
	ev->to_free = NULL;
	ev->typer = tr;
	ev->enabled = true;
}

static void evalr_free(Evaluator *ev) {
	allocr_free_all(&ev->allocr);
	typedef void *VoidPtr;
	arr_foreach(ev->to_free, VoidPtr, f)
		free(*f);
	arr_clear(&ev->to_free);
}

static inline void *evalr_malloc(Evaluator *ev, size_t bytes) {
	return allocr_malloc(&ev->allocr, bytes);
}
static inline void *evalr_calloc(Evaluator *ev, size_t n, size_t bytes) {
	return allocr_calloc(&ev->allocr, n, bytes);
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

static size_t compiler_alignof(Type *t) {
	switch (t->kind) {
	case TYPE_BUILTIN:
		return compiler_sizeof_builtin(t->builtin);
	case TYPE_VOID:
		return 1;
	case TYPE_FN:
		return sizeof(FnExpr *);
	case TYPE_PTR:
		return sizeof(void *);
	case TYPE_TUPLE:
		return sizeof(Value *);
	case TYPE_ARR:
		return compiler_alignof(t->arr.of);
	case TYPE_SLICE:
		if (sizeof(void *) > sizeof(size_t))
			return sizeof(void *);
		else
			return sizeof(size_t);
	case TYPE_TYPE:
		return sizeof(Type *);
	case TYPE_USER:
		return compiler_alignof(ident_typeval(t->user.name));
	case TYPE_STRUCT: {
		/* assume the align of a struct is (at most) the greatest align out of its children's */
		size_t align = 1;
		arr_foreach(t->struc.fields, Field, f) {
			size_t falign = compiler_alignof(f->type);
			if (falign > align) align = falign;
		}
		return align;
	}
	case TYPE_UNKNOWN:
		break;
	}
	assert(0);
	return 0;
}

/* finds offsets and size */
/* OPTIM: don't do this once per Type, but once per struct */
static void eval_struct_find_offsets(Type *t) {
	assert(t->kind == TYPE_STRUCT);
	if (!(t->flags & TYPE_FLAG_STRUCT_FOUND_OFFSETS)) {
		size_t bytes = 0;
		arr_foreach(t->struc.fields, Field, f) {
			size_t falign = compiler_alignof(f->type);
			/* align */
			bytes += ((falign - bytes) % falign + falign) % falign; /* = -bytes mod falign */
			assert(bytes % falign == 0);
			f->offset = bytes;
			/* add size */
			bytes += compiler_sizeof(f->type);
		}
		/* final align */
		size_t align = compiler_alignof(t);
		bytes += ((align - bytes) % align + align) % align; /* = -bytes mod align */
		t->struc.size = bytes;
		t->flags |= TYPE_FLAG_STRUCT_FOUND_OFFSETS;
	}
}

/* size of a type at compile time */
static size_t compiler_sizeof(Type *t) {
	switch (t->kind) {
	case TYPE_BUILTIN:
		return compiler_sizeof_builtin(t->builtin);
	case TYPE_FN:
		return sizeof(FnExpr *);
	case TYPE_PTR:
		return sizeof(void *);
	case TYPE_ARR:
		return t->arr.n * compiler_sizeof(t->arr.of);
	case TYPE_TUPLE:
		return sizeof(Value *);
	case TYPE_SLICE:
		return sizeof(Slice);
	case TYPE_TYPE:
		return sizeof(Type *);
	case TYPE_USER:
		return compiler_sizeof(ident_typeval(t->user.name));
	case TYPE_STRUCT: {
		eval_struct_find_offsets(t);
		return t->struc.size;
	} break;
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
	case TYPE_USER:
	case TYPE_TYPE:
	case TYPE_TUPLE:
	case TYPE_STRUCT:
		break;
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

/* 
IMPORTANT: Only pass an evaluator if you want it to use its allocator.
Otherwise, pass NULL.
*/
static void val_copy(Evaluator *ev, Value *dest, Value *src, Type *t) {
	switch (t->kind) {
	case TYPE_BUILTIN:
	case TYPE_FN:
	case TYPE_PTR:
	case TYPE_SLICE:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TYPE:
		*dest = *src;
		break;
	case TYPE_ARR: {
		size_t bytes = t->arr.n * compiler_sizeof(t->arr.of);
		if (ev)
			dest->arr = evalr_malloc(ev, bytes);
		else
			dest->arr = err_malloc(bytes);
		memcpy(dest->arr, src->arr, bytes);
	} break;
	case TYPE_TUPLE: {
		size_t bytes = arr_len(t->tuple) * sizeof(*dest->tuple);
		if (ev)
			dest->tuple = evalr_malloc(ev, bytes);
		else
			dest->tuple = err_malloc(bytes);
		memcpy(dest->tuple, src->tuple, bytes);
	} break;
	case TYPE_STRUCT: {
		size_t bytes = compiler_sizeof(t);
		if (ev)
			dest->struc = evalr_malloc(ev, bytes);
		else
			dest->struc = err_malloc(bytes);
		memcpy(dest->struc, src->struc, bytes);
	} break;
	case TYPE_USER:
		val_copy(ev, dest, src, ident_typeval(t->user.name));
		break;
	}
}

static void *val_ptr_to_free(Value *v, Type *t) {
	switch (t->kind) {
	case TYPE_BUILTIN:
	case TYPE_FN:
	case TYPE_PTR:
	case TYPE_SLICE:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TYPE:
		return NULL;
	case TYPE_ARR:
		return v->arr;
	case TYPE_TUPLE:
		return v->tuple;
	case TYPE_STRUCT:
		return v->struc;
	case TYPE_USER:
		return val_ptr_to_free(v, ident_typeval(t->user.name));
	}
	assert(0); return NULL;
}

static void val_free(Value *v, Type *t) {
	free(val_ptr_to_free(v, t));
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
	if (from->kind == TYPE_USER || to->kind == TYPE_USER) {
		*vout = *vin;
		return;
	}
	
	switch (from->kind) {
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TUPLE:
	case TYPE_USER:
	case TYPE_TYPE:
	case TYPE_STRUCT:
		assert(0); break;
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
		case TYPE_USER:
		case TYPE_STRUCT:
		case TYPE_SLICE:
		case TYPE_VOID:
		case TYPE_UNKNOWN:
		case TYPE_TUPLE:
		case TYPE_FN:
		case TYPE_ARR:
		case TYPE_TYPE:
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
		case TYPE_USER:
			*vout = *vin;
			break;
		case TYPE_SLICE:
		case TYPE_UNKNOWN:
		case TYPE_TUPLE:
		case TYPE_VOID:
		case TYPE_ARR:
		case TYPE_BUILTIN:
		case TYPE_TYPE:
		case TYPE_STRUCT:
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
		case TYPE_USER:
			*vout = *vin;
			break;
		case TYPE_SLICE:
		case TYPE_UNKNOWN:
		case TYPE_TUPLE:
		case TYPE_VOID:
		case TYPE_TYPE:
		case TYPE_STRUCT:
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
		case TYPE_USER:
			*vout = *vin;
			break;
		case TYPE_SLICE:
		case TYPE_FN:
		case TYPE_UNKNOWN:
		case TYPE_TUPLE:
		case TYPE_VOID:
		case TYPE_BUILTIN:
		case TYPE_TYPE:
		case TYPE_STRUCT:
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
		case TYPE_USER:
			*vout = *vin;
			break;
		case TYPE_FN:
		case TYPE_UNKNOWN:
		case TYPE_TUPLE:
		case TYPE_VOID:
		case TYPE_BUILTIN:
		case TYPE_TYPE:
		case TYPE_STRUCT:
			assert(0); break;
		}
		break;
	}
}

/* type is the underlying type, not the pointer type. */
static void eval_deref(Value *v, void *ptr, Type *type) {
	switch (type->kind) {
	case TYPE_PTR: v->ptr = *(void **)ptr; break;
	case TYPE_ARR: v->arr = ptr; break; /* when we have a pointer to an array, it points directly to the data in that array. */
	case TYPE_STRUCT: v->struc = ptr; break; /* same for structs */
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
	case TYPE_TYPE:
		v->type = *(Type **)ptr;
		break;
	case TYPE_USER:
		eval_deref(v, ptr, ident_typeval(type->user.name));
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
	case TYPE_ARR: memcpy(set, to->arr, compiler_sizeof(type)); break; /* TODO: test this */
	case TYPE_STRUCT: memcpy(set, to->struc, compiler_sizeof(type)); break;
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
	case TYPE_TYPE:
		*(Type **)set = to->type;
		break;
	case TYPE_USER:
		eval_deref_set(set, to, ident_typeval(type->user.name));
		break;
	case TYPE_VOID:
	case TYPE_UNKNOWN:
		assert(0);
		break;
	}
}

static bool eval_val_ptr_at_index(Evaluator *ev, Location where, Value *arr, U64 i, Type *arr_type, Type *idx_type, void **ptr, Type **type) {
	switch (arr_type->kind) {
	case TYPE_ARR: {
		U64 arr_sz = arr_type->arr.n;
		if (i >= arr_sz) {
			err_print(where, "Array out of bounds (%lu, array size = %lu)\n", (unsigned long)i, (unsigned long)arr_sz);
			return false;
		}
		*ptr = (char *)arr->arr + compiler_sizeof(arr_type->arr.of) * i;
		if (type) *type = arr_type->arr.of;
	} break;
	case TYPE_SLICE: {
		U64 slice_sz = arr->slice.n;
		if (i >= slice_sz) {
			err_print(where, "Slice out of bounds (%lu, slice size = %lu)\n", (unsigned long)i, (unsigned long)slice_sz);
			return false;
		}
		*ptr = (char *)arr->slice.data + compiler_sizeof(arr_type->slice) * i;
		if (type) *type = arr_type->slice;
	} break;
	default: assert(0); break;
	}
	return true;
}

static bool eval_expr_ptr_at_index(Evaluator *ev, Expression *e, void **ptr, Type **type) {
	Value arr;
	if (!eval_expr(ev, e->binary.lhs, &arr)) return false;
	Value index;
	if (!eval_expr(ev, e->binary.rhs, &index)) return false;
	Type *ltype = &e->binary.lhs->type;
	Type *rtype = &e->binary.rhs->type;
	U64 i;
	assert(rtype->kind == TYPE_BUILTIN);
	if (rtype->builtin == BUILTIN_U64) {
		i = index.u64;
	} else {
		I64 signed_index = val_to_i64(&index, rtype->builtin);
		if (signed_index < 0) {
			err_print(e->where, "Array or slice out of bounds (index = %ld)\n", (long)signed_index);
			return false;
		}
		i = (U64)signed_index;
	}
	return eval_val_ptr_at_index(ev, e->where, &arr, i, ltype, rtype, ptr, type);
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
			void *ptr;
			Type *type;
			/* get pointer to x[i] */
			if (!eval_expr_ptr_at_index(ev, set, &ptr, &type))
				return false;
			/* set it to to */
			eval_deref_set(ptr, to, type);
		} break;
		case BINARY_DOT: {
			Value struc;
			if (!eval_expr(ev, set->binary.lhs, &struc))
				return false;
			eval_struct_find_offsets(type_inner(&set->binary.lhs->type));
			void *ptr = (char *)struc.struc + set->binary.field->offset;
			eval_deref_set(ptr, to, set->binary.field->type);
		} break;
		default: assert(0); break;
		}
		break;
	case EXPR_TUPLE:
		for (size_t i = 0; i < arr_len(set->tuple); i++) {
			if (!eval_set(ev, &set->tuple[i], &to->tuple[i]))
				return false;
		}
		break;
	default:
		assert(0);
		break;
	}
	return true;
}

static bool eval_expr(Evaluator *ev, Expression *e, Value *v) {
	if (!ev->enabled) return false; /* silently fail */
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
	eval_binary_bool_op_one(f64, F64, op);				\
	eval_binary_bool_op_one(boolv, BOOL, op);			\
	eval_binary_bool_op_one(charv, CHAR, op);

#define eval_binary_bool_op_nums_only(op)					\
	{Type *ltype=&e->binary.lhs->type,						\
	 *rtype=&e->binary.rhs->type;							\
	 Type *cast_to =	ltype->flags & TYPE_FLAG_FLEXIBLE ?	\
	 rtype : ltype;											\
	 val_cast(&lhs, ltype, &lhs, cast_to);					\
	 val_cast(&rhs, rtype, &rhs, cast_to);					\
	 assert(e->binary.lhs->type.kind == TYPE_BUILTIN);	  	\
	 switch (builtin) {										\
		 eval_binary_bool_op_nums(builtin, op);				\
	 default:												\
		 assert(!("Invalid builtin to "#op)[0]); break;		\
	 }}
	
#define eval_binary_bool_op(op)					\
	if (e->binary.lhs->type.kind == TYPE_PTR)	\
		v->boolv = lhs.ptr op rhs.ptr;			\
	else { eval_binary_bool_op_nums_only(op); }
	
	switch (e->kind) {
	case EXPR_UNARY_OP: {
		Value of;
		if (e->unary.op != UNARY_ADDRESS) {
			if (!eval_expr(ev, e->unary.of, &of)) return false;
		}
		switch (e->unary.op) {
		case UNARY_ADDRESS: {
			Expression *o = e->unary.of;
			if (o->type.kind == TYPE_TYPE) {
				if (!eval_expr(ev, e->unary.of, &of)) return false;
				/* "address" of type (pointer to type) */
				v->type = evalr_calloc(ev, 1, sizeof *v->type); /* TODO: this might be bad in the future; should free this at some point */
				/* v->type->flags = 0; */
				v->type->kind = TYPE_PTR;
				v->type->ptr = of.type;
				break;
			}
			switch (o->kind) {
			case EXPR_IDENT: {
				IdentDecl *id = ident_decl(o->ident);
				if (!(id->flags & IDECL_FLAG_HAS_VAL)) {
					err_print(e->where, "Cannot take address of run time variable at compile time.");
					return false;
				}
				if (o->type.kind == TYPE_ARR)
					v->ptr = id->val.arr; /* point directly to data */
				else if (o->type.kind == TYPE_STRUCT)
					v->ptr = id->val.struc;
				else
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
					void *ptr;
					if (!eval_expr_ptr_at_index(ev, o, &ptr, NULL))
						return false;
					v->ptr = ptr;
				} break;
				case BINARY_DOT: {
					Value struc;
					if (!eval_expr(ev, o->binary.lhs, &struc))
						return false;
					eval_struct_find_offsets(type_inner(&o->binary.lhs->type));
					v->ptr = (char *)struc.struc + o->binary.field->offset;
				} break;
				default: assert(0); break;
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
				assert(e->unary.of->type.kind == TYPE_SLICE);
				free(of.slice.data);
			}
			break;
		}
	} break;
	case EXPR_BINARY_OP: {
		Value lhs, rhs;
		Expression *lhs_expr = e->binary.lhs, *rhs_expr = e->binary.rhs;
		if (e->binary.op != BINARY_SET)
			if (!eval_expr(ev, lhs_expr, &lhs)) return false;
		if (e->binary.op != BINARY_DOT)
			if (!eval_expr(ev, rhs_expr, &rhs)) return false;
		
		BuiltinType builtin = e->binary.lhs->type.builtin;
		switch (e->binary.op) {
		case BINARY_DOT: {
			Type *inner_type = &e->binary.lhs->type;
			while (inner_type->kind == TYPE_USER)
				inner_type = ident_typeval(inner_type->user.name);
			eval_struct_find_offsets(inner_type);
			
			eval_deref(v, (char *)lhs.struc + e->binary.field->offset, &e->type);
		} break;
		case BINARY_ADD:
			if (e->binary.lhs->type.kind == TYPE_PTR) {
				v->ptr = (char *)lhs.ptr + val_to_i64(&rhs, e->binary.rhs->type.builtin)
					* (I64)compiler_sizeof(e->binary.lhs->type.ptr);
			} else {
				eval_binary_op_nums_only(+);
			}
			break;
		case BINARY_SUB:
			if (e->binary.lhs->type.kind == TYPE_PTR) {
				v->ptr = (char *)lhs.ptr - val_to_i64(&rhs, e->binary.rhs->type.builtin)
					* (I64)compiler_sizeof(e->binary.lhs->type.ptr);
			} else {
				eval_binary_op_nums_only(-);
			}
			break;
		case BINARY_MUL:
			eval_binary_op_nums_only(*); break;
		case BINARY_DIV:
			eval_binary_op_nums_only(/); break;
		case BINARY_LT:
			eval_binary_bool_op(<); break;
		case BINARY_LE:
			eval_binary_bool_op(<=); break;
		case BINARY_GT:
			eval_binary_bool_op(>); break;
		case BINARY_GE:
			eval_binary_bool_op(>=); break;
		case BINARY_EQ:
			eval_binary_bool_op(==); break;
		case BINARY_NE:
			eval_binary_bool_op(!=); break;
		case BINARY_SET:
			if (!eval_set(ev, e->binary.lhs, &rhs)) return false;
			break;
		case BINARY_AT_INDEX: {
			void *ptr;
			Type *type;
			eval_expr_ptr_at_index(ev, e, &ptr, &type);
			eval_deref(v, ptr, type);
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
				if (!eval_block(ev, &i->body, &e->type, v)) return false;
			} else if (i->next_elif) {
				if (!eval_expr(ev, i->next_elif, v)) return false;
			}
		} else {
			if (!eval_block(ev, &i->body, &e->type, v)) return false;
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
			if (!eval_block(ev, &w->body, &e->type, v)) return false;
		}
	} break;
	case EXPR_BLOCK:
		if (!eval_block(ev, &e->block, &e->type, v)) return false;
		break;
	case EXPR_LITERAL_BOOL:
		v->boolv = e->booll;
		break;
	case EXPR_LITERAL_CHAR:
		v->charv = e->charl;
		break;
	case EXPR_LITERAL_STR:
		v->slice.data = e->strl.str;
		v->slice.n = e->strl.len;
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
		if (!types_decl(ev->typer, d)) return false;
		if (idecl->flags & IDECL_FLAG_HAS_VAL) {
			*v = idecl->val;
		} else if (d->flags & DECL_FLAG_CONST) {
			if (!(d->flags & DECL_FLAG_FOUND_VAL)) {
				if (!eval_expr(ev, &d->expr, &d->val)) return false;
				d->flags |= DECL_FLAG_FOUND_VAL;
			}
			
			long index = 0;
			arr_foreach(d->idents, Identifier, decl_i) {
				if (*decl_i == e->ident) {
					break;
				}
				index++;
				assert(index < (long)arr_len(d->idents)); /* identifier got its declaration set to here, but it's not here */
			}
			if (e->type.kind == TYPE_TYPE) {
				/* set v to a user type, not the underlying type */
				v->type = evalr_malloc(ev, sizeof *v->type); /* TODO: fix this (free eventually) */
				v->type->flags = 0;
				v->type->kind = TYPE_USER;
				v->type->user.name = d->idents[index];
			} else {
				*v = d->type.kind == TYPE_TUPLE ? d->val.tuple[index] : d->val;
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
		v->tuple = err_malloc(n * sizeof *v->tuple);
		*(void **)arr_add(&ev->to_free) = v->tuple;
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
		/* make sure function body is typed before calling it */
		if (!types_block(ev->typer, &fn->body))
			return false;
		/* set parameter declaration values */
		Declaration *params = fn->params;
		/* OPTIM (NOTE: currently needed for recursion) */
		Value *args = NULL;
		arr_resv(&args, arr_len(e->call.arg_exprs));
		for (size_t i = 0; i < arr_len(e->call.arg_exprs); i++) {
			if (!eval_expr(ev, &e->call.arg_exprs[i], &args[i]))
				return false;
		}
		fn_enter(fn, 0);
		long arg = 0;
		arr_foreach(params, Declaration, p) {
			long idx = 0;
			arr_foreach(p->idents, Identifier, i) {
				Type *type = p->type.kind == TYPE_TUPLE ? &p->type.tuple[idx++] : &p->type;
				IdentDecl *id = ident_decl(*i);
				val_copy(NULL, &id->val, &args[arg], type);
				id->flags |= IDECL_FLAG_HAS_VAL;
				arg++;
			}
		}
		arr_clear(&args);
		if (!eval_block(ev, &fn->body, &e->type, v)) {
			fn_exit(fn);
			return false;
		}
		if (ev->returning) {
			*v = ev->ret_val;
			ev->returning = false;
		}
		fn_exit(fn);
	} break;
	case EXPR_SLICE: {
		SliceExpr *s = &e->slice;
	    Value ofv;
		Type *of_type = &s->of->type;
		if (!eval_expr(ev, s->of, &ofv))
			return false;
		U64 n = of_type->kind == TYPE_ARR ? of_type->arr.n : ofv.slice.n;
		U64 from, to;
		if (s->from) {
			Value fromv;
			if (!eval_expr(ev, s->from, &fromv))
				return false;
			assert(s->from->type.kind == TYPE_BUILTIN);
			from = val_to_u64(&fromv, s->from->type.builtin);
		} else {
			from = 0;
		}
		if (s->to) {
			Value tov;
			if (!eval_expr(ev, s->to, &tov))
				return false;
			assert(s->to->type.kind == TYPE_BUILTIN);
			to = val_to_u64(&tov, s->to->type.builtin);
		} else {
			to = n - 1;
		}
		/* TODO: is this the best check? (Go also checks if from > to) */
		if (to > n) {
			err_print(e->where, "Slice index out of bounds (to = %lu, length = %lu).", (unsigned long)to, (unsigned long)n);
			return false;
		}
		void *ptr1, *ptr2;
		if (from < to) {
			if (!eval_val_ptr_at_index(ev, e->where, &ofv, from, of_type, &s->from->type, &ptr1, NULL))
				return false;
			if (!eval_val_ptr_at_index(ev, e->where, &ofv, to, of_type, &s->to->type, &ptr2, NULL))
				return false;
			v->slice.data = ptr1;
			v->slice.n = to - from;
		} else {
			v->slice.data = NULL;
			v->slice.n = 0;
		}
	} break;
	case EXPR_TYPE:
		v->type = &e->typeval;
	}
	return true;
}

static bool eval_decl(Evaluator *ev, Declaration *d) {
	Value val = {0};
	int has_expr = d->flags & DECL_FLAG_HAS_EXPR;
	if (has_expr) {
		if (d->flags & DECL_FLAG_CONST) {
			if (!(d->flags & DECL_FLAG_FOUND_VAL)) {
				if (!eval_expr(ev, &d->expr, &d->val))
					return false;
				d->flags |= DECL_FLAG_FOUND_VAL;
			}
			val = d->val;
		} else {
			if (!eval_expr(ev, &d->expr, &val))
				return false;
		}
	}
	long index = 0;
	arr_foreach(d->idents, Identifier, i) {
		IdentDecl *id = ident_decl(*i);
		Type *type = d->type.kind == TYPE_TUPLE ? &d->type.tuple[index] : &d->type;
		Value *thisval = NULL;
		if (has_expr)
			thisval = d->type.kind == TYPE_TUPLE ? &val.tuple[index] : &val;
		Type *inner_type = type;
		while (inner_type->kind == TYPE_USER)
			inner_type = ident_typeval(type->user.name);
		if (inner_type->kind == TYPE_STRUCT) {
			id->val.struc = err_calloc(1, compiler_sizeof(inner_type));
		} else if (inner_type->kind == TYPE_ARR) {
			id->val.arr = err_calloc(inner_type->arr.n, compiler_sizeof(inner_type->arr.of));
		}

		if (has_expr)
			val_copy(d->flags & DECL_FLAG_CONST ? ev : NULL, &id->val, thisval, type);
		index++;
		id->flags |= IDECL_FLAG_HAS_VAL;
	}
	if (has_expr && d->expr.kind == EXPR_TUPLE) {
		val_free(&val, &d->type); /* free the tuple */
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
	case STMT_RET: {
		Value r;
		if (!eval_expr(ev, &stmt->ret.expr, &r))
			return false;
		val_copy(NULL, &ev->ret_val, &r, &stmt->ret.expr.type);
	} break;
	}
	return true;
}

/* t is the type of the block. */
static bool eval_block(Evaluator *ev, Block *b, Type *t, Value *v) {
	void **prev_to_free = ev->to_free;
	ev->to_free = NULL;
	block_enter(b, b->stmts, 0);
	arr_foreach(b->stmts, Statement, stmt) {
		if (!eval_stmt(ev, stmt))
			return false;
		if (ev->returning) break;
	}
	if (!ev->returning && b->ret_expr) {
		Value r;
		if (!eval_expr(ev, b->ret_expr, &r))
			return false;
		/* make a copy so that r's data isn't freed when we exit the block */
		val_copy(NULL, v, &r, &b->ret_expr->type);
		void *free_ptr = val_ptr_to_free(v, t);
		if (free_ptr)
			*(void **)arr_add(&prev_to_free) = free_ptr;
	}
	block_exit(b, b->stmts);
	typedef void *VoidPtr;
	arr_foreach(ev->to_free, VoidPtr, f) {
		free(*f);
	}
	arr_clear(&ev->to_free);
	ev->to_free = prev_to_free;
	return true;
}
