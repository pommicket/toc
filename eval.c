/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

static Status types_block(Typer *tr, Block *b);
static Status types_decl(Typer *tr, Declaration *d);
static Status type_resolve(Typer *tr, Type *t, Location where);
static Status eval_block(Evaluator *ev, Block *b, Value *v);
static Status eval_expr(Evaluator *ev, Expression *e, Value *v);
static Value get_builtin_val(BuiltinVal val);

static void evalr_create(Evaluator *ev, Typer *tr, Allocator *allocr) {
	ev->returning = NULL;
	ev->typer = tr;
	ev->enabled = true;
	ev->allocr = allocr;
	ffmgr_create(&ev->ffmgr, ev->allocr);
}

static inline void *evalr_malloc(Evaluator *ev, size_t bytes) {
	return allocr_malloc(ev->allocr, bytes);
}
static inline void *evalr_calloc(Evaluator *ev, size_t n, size_t bytes) {
	return allocr_calloc(ev->allocr, n, bytes);
}

static bool builtin_truthiness(Value v, BuiltinType b) {
	switch (b) {
	case BUILTIN_I8: return v.i8 != 0;
	case BUILTIN_I16: return v.i16 != 0;
	case BUILTIN_I32: return v.i32 != 0;
	case BUILTIN_I64: return v.i64 != 0;
	case BUILTIN_U8: return v.u8 != 0;
	case BUILTIN_U16: return v.u16 != 0;
	case BUILTIN_U32: return v.u32 != 0;
	case BUILTIN_U64: return v.u64 != 0;
	case BUILTIN_F32: return v.f32 != 0;
	case BUILTIN_F64: return v.f64 != 0;
	case BUILTIN_BOOL: return v.boolv;
	case BUILTIN_CHAR: return v.charv != 0;
	case BUILTIN_VARARGS: return arr_len(v.varargs) != 0;
	case BUILTIN_TYPE:
	case BUILTIN_NMS:
		break;
	}
	assert(0); return false;
}

static bool val_truthiness(Value v, Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_VOID: return false;
	case TYPE_UNKNOWN: assert(0); return false;
	case TYPE_BUILTIN: return builtin_truthiness(v, t->builtin);
	case TYPE_PTR: return v.ptr != NULL;
	case TYPE_FN: return v.fn != NULL;
	case TYPE_ARR: return t->arr.n > 0;
	case TYPE_SLICE: return v.slice.n > 0;
	case TYPE_TUPLE:
	case TYPE_STRUCT:
	case TYPE_EXPR:
		break;
	}
	assert(0);
	return false;
}



static I64 val_to_i64(Value v, BuiltinType v_type) {
	switch (v_type) {
	case BUILTIN_I8: return (I64)v.i8;
	case BUILTIN_I16: return (I64)v.i16;
	case BUILTIN_I32: return (I64)v.i32;
	case BUILTIN_I64: return (I64)v.i64;
	case BUILTIN_U8: return (I64)v.u8;
	case BUILTIN_U16: return (I64)v.u16;
	case BUILTIN_U32: return (I64)v.u32;
	case BUILTIN_U64: return (I64)v.u64;
	default: break;
	}
	assert(0);
	return 0;
}



static U64 val_to_u64(Value v, BuiltinType v_type) {
	if (v_type == BUILTIN_U64) return v.u64;
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
	case BUILTIN_F32:
		v->f32 = (F32)x; break;
	case BUILTIN_F64:
		v->f64 = (F64)x; break;
	default: assert(0); break;
	}
}

static void u64_to_val(Value *v, BuiltinType v_type, U64 x) {
	switch (v_type) {
	case BUILTIN_U64:
		v->u64 = x;
		break;
	case BUILTIN_F32:
		v->f32 = (F32)x;
		break;
	case BUILTIN_F64:
		v->f64 = (F64)x;
		break;
	default:
		i64_to_val(v, v_type, (I64)x);
		break;
	}
}

/* rerturns a pointer to the underlying data of v, e.g. an I64 * if t is the builtin BUILTIN_I64 */
static void *val_get_ptr(Value *v, Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_PTR:
	case TYPE_BUILTIN:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_FN:
	case TYPE_SLICE:
	case TYPE_TUPLE:
		return v;
	case TYPE_ARR:
		return v->arr;
	case TYPE_STRUCT:
		return v->struc;
	case TYPE_EXPR: break;
	}
	assert(0);
	return NULL;
}

static void fprint_val_ptr(FILE *f, void *p, Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_VOID:
		fprintf(f, "(void)");
		break;
	case TYPE_UNKNOWN:
		fprintf(f, "???");
		break;
	case TYPE_BUILTIN:
		switch (t->builtin) {
		case BUILTIN_I8: fprintf(f, I8_FMT, *(I8 *)p); break;
		case BUILTIN_U8: fprintf(f, U8_FMT, *(U8 *)p); break;
		case BUILTIN_I16: fprintf(f, I16_FMT, *(I16 *)p); break;
		case BUILTIN_U16: fprintf(f, U16_FMT, *(U16 *)p); break;
		case BUILTIN_I32: fprintf(f, I32_FMT, *(I32 *)p); break;
		case BUILTIN_U32: fprintf(f, U32_FMT, *(U32 *)p); break;
		case BUILTIN_I64: fprintf(f, I64_FMT, *(I64 *)p); break;
		case BUILTIN_U64: fprintf(f, U64_FMT, *(U64 *)p); break;
		case BUILTIN_F32: fprintf(f, F32_FMT, *(F32 *)p); break;
		case BUILTIN_F64: fprintf(f, F64_FMT, *(F64 *)p); break;
		case BUILTIN_CHAR: fprint_char_literal(f, *(char *)p); break;
		case BUILTIN_BOOL: fprintf(f, "%s", *(bool *)p ? "true" : "false"); break;
		case BUILTIN_VARARGS:
			fprintf(f, "...(");
			arr_foreach(*(VarArg **)p, VarArg, varg) {
				fprint_val(f, varg->val, varg->type);
			}
			fprintf(f, ")");
			break;
		case BUILTIN_TYPE:
			fprint_type(f, *(Type **)p);
			break;
		case BUILTIN_NMS:
			fprint_nms(f, *(Namespace **)p);
			break;
		}
		break;
	case TYPE_FN:
		fprintf(f, "<function @ %p>", (void *)*(FnExpr **)p);
		break;
	case TYPE_TUPLE: {
		Value *tuple = *(Value **)p;
		fprintf(f, "(");
		for (size_t i = 0; i < arr_len(t->tuple); ++i) {
			if (i) fprintf(f, ", ");
			fprint_val(f, tuple[i], &t->tuple[i]);
		}
		fprintf(f, ")");
	} break;
	case TYPE_ARR: {
		fprintf(f, "["); /* TODO: change? when array initializers are added */
		size_t n = t->arr.n;
		if (n > 5) n = 5;
		for (size_t i = 0; i < n; ++i) {
			if (i) fprintf(f, ", ");
			fprint_val_ptr(f, (char *)p + i * compiler_sizeof(t->arr.of), t->arr.of);
		}
		if (t->arr.n > n) {
			fprintf(f, ", ...");
		}
		fprintf(f, "]");
	} break;
	case TYPE_PTR:
		fprintf(f, "<pointer: %p>", *(void **)p);
		break;
	case TYPE_SLICE: {
		fprintf(f, "["); /* TODO: change? when slice initializers are added */
		Slice slice = *(Slice *)p;
		I64 n = slice.n;
		if (n > 5) n = 5;
		for (I64 i = 0; i < n; ++i) {
			if (i) fprintf(f, ", ");
			fprint_val_ptr(f, (char *)slice.data + i * (I64)compiler_sizeof(t->arr.of), t->arr.of);
		}
		if (slice.n > n) {
			fprintf(f, ", ...");
		}
		fprintf(f, "]");
	} break;
	case TYPE_STRUCT:
		fprintf(f, "["); /* TODO: change? when struct initializers are added */
		arr_foreach(t->struc->fields, Field, fi) {
			if (fi != t->struc->fields)
				fprintf(f, ", ");
			fprint_ident_debug(f, fi->name);
			fprintf(f, ": ");
			fprint_val_ptr(f, (char *)p + fi->offset, &fi->type);
		}
		fprintf(f, "]");
		break;
	case TYPE_EXPR: 
		assert(0);
		break;
	}
}

static void fprint_val(FILE *f, Value v, Type *t) {
	fprint_val_ptr(f, val_get_ptr(&v, t), t);
}

static void print_val(Value v, Type *t) {
	fprint_val(stdout, v, t);
	printf("\n");
}

static void *val_ptr_to_free(Value *v, Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_BUILTIN:
		if (t->builtin == BUILTIN_VARARGS)
			return v->varargs ? arr_hdr(v->varargs) : NULL;
		return NULL;
	case TYPE_FN:
	case TYPE_PTR:
	case TYPE_SLICE:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
		return NULL;
	case TYPE_ARR:
		return v->arr;
	case TYPE_TUPLE:
		return v->tuple;
	case TYPE_STRUCT:
		return v->struc;
	case TYPE_EXPR:
		break;
	}
	assert(0); return NULL;
}

static inline void val_free(Value *v, Type *t) {
	free(val_ptr_to_free(v, t));
}

static inline void val_free_ptr(Value *v, Type *t) {
	val_free(v, t);
	free(v);
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

#define builtin_int_casts(low, up)							\
	case BUILTIN_##up:										\
	switch (to) {											\
		builtin_casts_to_num(low);							\
	case BUILTIN_CHAR: vout->charv = (char)vin->low; break;	\
	case BUILTIN_BOOL: vout->boolv = vin->low != 0; break;	\
	case BUILTIN_NMS:										\
	case BUILTIN_TYPE:										\
	case BUILTIN_VARARGS:									\
		assert(0); break;									\
	} break

#define builtin_float_casts(low, up)							\
	case BUILTIN_##up:											\
	switch (to) {												\
		builtin_casts_to_num(low);								\
	case BUILTIN_BOOL: vout->boolv = vin->low != 0.0f; break;	\
	case BUILTIN_CHAR:											\
	case BUILTIN_TYPE:											\
	case BUILTIN_NMS:											\
	case BUILTIN_VARARGS:										\
		assert(0); break;										\
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

	case BUILTIN_BOOL: vout->boolv = builtin_truthiness(*vin, from); break;
	case BUILTIN_CHAR:
		switch (to) {
			builtin_casts_to_int(charv);
		case BUILTIN_CHAR: /* handled at top of func */
		case BUILTIN_F32:
		case BUILTIN_F64:
		case BUILTIN_BOOL:
		case BUILTIN_TYPE:
		case BUILTIN_NMS:
		case BUILTIN_VARARGS:
			assert(0); break;
		}
		break;
	case BUILTIN_TYPE:
	case BUILTIN_NMS:
	case BUILTIN_VARARGS:
		assert(0);
		break;
	}
}

static void val_cast(Value *vin, Type *from, Value *vout, Type *to) {
	assert(from->flags & TYPE_IS_RESOLVED);
	assert(to->flags & TYPE_IS_RESOLVED);
	
	if (to->kind == TYPE_BUILTIN && to->builtin == BUILTIN_BOOL) {
		vout->boolv = val_truthiness(*vin, from);
		return;
	}
	
	switch (from->kind) {
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TUPLE:
	case TYPE_STRUCT:
	case TYPE_EXPR:
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
		case TYPE_EXPR:
		case TYPE_STRUCT:
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
		default:
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
			case BUILTIN_TYPE:
			case BUILTIN_NMS:
			case BUILTIN_VARARGS:
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
		default:
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
		default:
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
		default:
			assert(0); break;
		}
		break;
	}
}

/* type is the underlying type, not the pointer type. */
static void eval_deref(Value *v, void *ptr, Type *type) {
	assert(type->flags & TYPE_IS_RESOLVED);
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
		case BUILTIN_NMS: v->nms = *(Namespace **)ptr; break;
		case BUILTIN_TYPE:
			v->type = *(Type **)ptr;
			break;
		case BUILTIN_VARARGS:
			assert(0);
			break;
		}
		break;
	case TYPE_SLICE:
		v->slice = *(Slice *)ptr;
		break;
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_EXPR:
		assert(0);
		break;
	}
}
/* inverse of eval_deref */
static void eval_deref_set(void *set, Value *to, Type *type) {
	assert(type->flags & TYPE_IS_RESOLVED);
	switch (type->kind) {
	case TYPE_PTR: *(void **)set = to->ptr; break;
	case TYPE_ARR: memcpy(set, to->arr, compiler_sizeof(type)); break;
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
		case BUILTIN_NMS: *(Namespace **)set = to->nms; break;
		case BUILTIN_TYPE:
			*(Type **)set = to->type;
			break;
		case BUILTIN_VARARGS:
			assert(0);
			break;
		}
		break;
	case TYPE_SLICE:
		*(Slice *)set = to->slice;
		break;
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_EXPR:
		assert(0);
		break;
	}
}

static Status eval_val_ptr_at_index(Location where, Value *arr, U64 i, Type *arr_type, void **ptr, Type **type) {
	switch (arr_type->kind) {
	case TYPE_ARR: {
		U64 arr_sz = (U64)arr_type->arr.n;
		if (i >= arr_sz) {
			err_print(where, "Array out of bounds (index = %lu, array size = %lu)\n", (unsigned long)i, (unsigned long)arr_sz);
			return false;
		}
		*ptr = (char *)arr->arr + compiler_sizeof(arr_type->arr.of) * i;
		if (type) *type = arr_type->arr.of;
	} break;
	case TYPE_SLICE: {
		U64 slice_sz = (U64)arr->slice.n;
		if (i >= slice_sz) {
			err_print(where, "Slice out of bounds (index = %lu, slice size = %lu)\n", (unsigned long)i, (unsigned long)slice_sz);
			return false;
		}
		if (!arr->slice.data) {
			err_print(where, "Indexing null slice.");
			return false;
		}
		*ptr = (char *)arr->slice.data + compiler_sizeof(arr_type->slice) * i;
		if (type) *type = arr_type->slice;
	} break;
	case TYPE_BUILTIN:
		if (arr_type->builtin == BUILTIN_VARARGS) {
			if (i >= (U64)arr_len(arr->varargs)) {
				err_print(where, "Varargs out of bounds (index = %lu, varargs size = %lu)\n", (unsigned long)i, (unsigned long)arr_len(arr->varargs));
				return false;
			}
			VarArg *vararg = &arr->varargs[i];
			*ptr = &vararg->val;
			*type = vararg->type;
			break;
		}
		/* fallthrough */
	default: assert(0); break;
	}
	return true;
}

static Status eval_expr_ptr_at_index(Evaluator *ev, Expression *e, void **ptr, Type **type) {
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
		I64 signed_index = val_to_i64(index, rtype->builtin);
		if (signed_index < 0) {
			err_print(e->where, "Array or slice out of bounds (index = %ld)\n", (long)signed_index);
			return false;
		}
		i = (U64)signed_index;
	}
	return eval_val_ptr_at_index(e->where, &arr, i, ltype, ptr, type);
}

static Value *ident_val(Identifier i) {
	switch (i->decl_kind) {
	case IDECL_EXPR: {
		switch (i->decl_expr->kind) {
		case EXPR_FOR: {
			ForExpr *fo = i->decl_expr->for_;
			Value *v = *(Value **)arr_last(fo->val_stack);
			if (i == fo->index) {
				if (fo->value)
					v = &v->tuple[0];
			} else {
				if (fo->index)
					v = &v->tuple[1];
			}
			return v;
		}
		default: assert(0); return NULL;
		}
	}
	case IDECL_DECL: {
		Declaration *decl = i->decl;
		int idx = decl_ident_index(decl, i);
		if (decl->flags & DECL_IS_PARAM) {
			if (decl->val_stack) {
				Value *valp = *(Value **)arr_last(decl->val_stack);
				if (arr_len(decl->idents) > 1)
					return &valp->tuple[idx];
				else
					return valp;
			} else {
				if (!(decl->flags & DECL_FOUND_VAL)) {
					return NULL;
				}
				/* struct parameter */
				if (arr_len(decl->idents) > 1)
					return &decl->val.tuple[idx];
				else
					return &decl->val;
			}
		} else if (decl->flags & DECL_IS_CONST) {
			return decl_val_at_index(decl, idx);
		} else if (decl->val_stack) {
			Value *valp = *(Value **)arr_last(decl->val_stack);
			if (arr_len(decl->idents) > 1)
				return &valp->tuple[idx];
			else
				return valp;
		} else
			return NULL; /* uh oh... this is a runtime-only variable */
	}
	case IDECL_NONE: break;
	}
	assert(0);
	return NULL;
	
}

static inline bool eval_address_of_ident(Identifier i, Location where, Type *type, void **ptr) {
	Value *val = ident_val(i);
	if (!val) {
		char *s = ident_to_str(i);
		err_print(where, "Cannot access value of variable %s at compile time.", s);
		return false;
	}
	*ptr = val_get_ptr(val, type);
	return true;
}

static Status eval_ptr_to_struct_field(Evaluator *ev, Expression *dot_expr, void **p) {
	Type *struct_type = &dot_expr->binary.lhs->type;
	bool is_ptr = struct_type->kind == TYPE_PTR;
	if (is_ptr) {
		struct_type = struct_type->ptr;
	}
	if (struct_type->kind == TYPE_STRUCT) {
		Value struc;
		if (!eval_expr(ev, dot_expr->binary.lhs, &struc))
			return false;
		void *struc_data;
		if (is_ptr) {
			struc_data = struc.ptr;
			if (struc_data == NULL) {
				err_print(dot_expr->where, "Attempt to dereference NULL pointer.");
				return false;
			}
		} else {
			struc_data = struc.struc;
		}
		*p = (char *)struc_data + dot_expr->binary.dot.field->offset;
	} else {
		void *ptr;
		Identifier ident = dot_expr->binary.rhs->ident;
		assert(type_is_builtin(struct_type, BUILTIN_NMS));
		if (!eval_address_of_ident(ident, dot_expr->where, &dot_expr->type, &ptr))
			return false;
		*p = ptr;
	}
	return true;
}

static Status eval_address_of(Evaluator *ev, Expression *e, void **ptr) {
	switch (e->kind) {
	case EXPR_IDENT: {
		if (!eval_address_of_ident(e->ident, e->where, &e->type, ptr))
			return false;
	} break;
	case EXPR_UNARY_OP:
		switch (e->unary.op) {
		case UNARY_DEREF: {
			Value v;
			if (!eval_expr(ev, e, &v)) return false;
			*ptr = v.ptr;
		} break;
		case UNARY_LEN: {
			Value slice;
			if (!eval_expr(ev, e, &slice)) return false;
			*ptr = &slice.slice.n;
		} break;
		default: assert(0); return false;
		}
		break;
	case EXPR_BINARY_OP:
		switch (e->binary.op) {
		case BINARY_AT_INDEX: {
			if (!eval_expr_ptr_at_index(ev, e, ptr, NULL))
				return false;
		} break;
		case BINARY_DOT: {
			Value struc;
			if (!eval_expr(ev, e->binary.lhs, &struc))
				return false;
			if (!eval_ptr_to_struct_field(ev, e, ptr))
				return false;
		} break;
		default: assert(0); return false;
		}
		break;
	default:
		assert(0);
		return false;
	}
	return true;
}

static Status eval_set(Evaluator *ev, Expression *set, Value *to) {
	switch (set->kind) {
	case EXPR_IDENT: {
		Identifier i = set->ident;
		Value *ival = ident_val(i);
		if (!ival) {
			err_print(set->where, "Cannot set value of run time variable at compile time.");
			return false;
		}
		*ival = *to;
	} break;
	case EXPR_UNARY_OP:
		switch (set->unary.op) {
		case UNARY_DEREF: {
			Value ptr;
			if (!eval_expr(ev, set->unary.of, &ptr)) return false;
			eval_deref_set(ptr.ptr, to, &set->type);
		} break;
		case UNARY_LEN: {
			Type *of_type = &set->unary.of->type;
			if (of_type->kind == TYPE_PTR) {
				/* if it's a pointer, we can just eval it and set its length */
				Value of;
				if (!eval_expr(ev, set->unary.of, &of)) return false;
				((Slice *)of.ptr)->n = to->i64;
			} else {
				/* otherwise, we need a pointer to the slice */
				void *p;
				if (!eval_address_of(ev, set->unary.of, &p))
					return false;
				((Slice *)p)->n = to->i64;
			}
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
			void *ptr;
			if (!eval_ptr_to_struct_field(ev, set, &ptr))
				return false;
			eval_deref_set(ptr, to, &set->binary.dot.field->type);
		} break;
		default: assert(0); break;
		}
		break;
	case EXPR_TUPLE:
		for (size_t i = 0; i < arr_len(set->tuple); ++i) {
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

static void eval_numerical_bin_op(Value lhs, Type *lhs_type, BinaryOp op, Value rhs, Type *rhs_type, Value *out, Type *out_type) {
	/* WARNING: macros ahead */

#define eval_unary_op_nums_only(op)				\
	switch (builtin) {							\
		eval_unary_op_nums(builtin, op);		\
	default: assert(0); break;					\
	}

#define eval_binary_op_one(low, up, op)				\
	case BUILTIN_##up:								\
		out->low = (up)(lhs.low op rhs.low); break
	
#define eval_binary_op_ints(builtin, op)		\
	eval_binary_op_one(i8, I8, op);				\
	eval_binary_op_one(i16, I16, op);			\
	eval_binary_op_one(i32, I32, op);			\
	eval_binary_op_one(i64, I64, op);			\
	eval_binary_op_one(u8, U8, op);				\
	eval_binary_op_one(u16, U16, op);			\
	eval_binary_op_one(u32, U32, op);			\
	eval_binary_op_one(u64, U64, op);			
	
#define eval_binary_op_nums(builtin, op)		\
	eval_binary_op_ints(builtin, op);			\
	eval_binary_op_one(f32, F32, op);			\
	eval_binary_op_one(f64, F64, op)

	
#define eval_binary_op_nums_only(op)			\
	val_cast(&lhs, lhs_type, &lhs, out_type);	\
	val_cast(&rhs, rhs_type, &rhs, out_type);	\
	assert(out_type->kind == TYPE_BUILTIN);		\
	switch (builtin) {							\
		eval_binary_op_nums(builtin, op);		\
	default: assert(0); break;					\
	}

#define eval_binary_op_ints_only(op)			\
	val_cast(&lhs, lhs_type, &lhs, out_type);	\
	val_cast(&rhs, rhs_type, &rhs, out_type);	\
	assert(out_type->kind == TYPE_BUILTIN);		\
	switch (builtin) {							\
		eval_binary_op_ints(builtin, op);		\
	default: assert(0); break;					\
	}


#define eval_binary_bool_op_one(low, up, op)	\
	case BUILTIN_##up:							\
		out->boolv = lhs.low op rhs.low; break

#define eval_binary_bool_op_nums(builtin, op)	\
	eval_binary_bool_op_one(i8, I8, op);		\
	eval_binary_bool_op_one(i16, I16, op);		\
	eval_binary_bool_op_one(i32, I32, op);		\
	eval_binary_bool_op_one(i64, I64, op);		\
	eval_binary_bool_op_one(u8, U8, op);		\
	eval_binary_bool_op_one(u16, U16, op);		\
	eval_binary_bool_op_one(u32, U32, op);		\
	eval_binary_bool_op_one(u64, U64, op);		\
	eval_binary_bool_op_one(f32, F32, op);		\
	eval_binary_bool_op_one(f64, F64, op);		\
	eval_binary_bool_op_one(boolv, BOOL, op);	\
	eval_binary_bool_op_one(charv, CHAR, op);

#define eval_binary_bool_op_nums_only(op)						\
	{Type *cast_to =	lhs_type->flags & TYPE_IS_FLEXIBLE ?	\
			rhs_type : lhs_type;								\
		val_cast(&lhs, lhs_type, &lhs, cast_to);				\
		val_cast(&rhs, rhs_type, &rhs, cast_to);				\
		assert(lhs_type->kind == TYPE_BUILTIN);					\
		switch (cast_to->builtin) {								\
			eval_binary_bool_op_nums(builtin, op);				\
		default:												\
			assert(!("Invalid builtin to "#op)[0]); break;		\
		}}
	
#define eval_binary_bool_op(op)					\
	if (lhs_type->kind == TYPE_PTR)				\
		out->boolv = lhs.ptr op rhs.ptr;		\
	else { eval_binary_bool_op_nums_only(op); }
	
	BuiltinType builtin = out_type->builtin;
	switch (op) {
	case BINARY_ADD:
		if (lhs_type->kind == TYPE_PTR) {
			out->ptr = (char *)lhs.ptr + val_to_i64(rhs, rhs_type->builtin)
				* (I64)compiler_sizeof(lhs_type->ptr);
		} else {
			eval_binary_op_nums_only(+);
		}
		break;
	case BINARY_SUB:
		if (lhs_type->kind == TYPE_PTR) {
			out->ptr = (char *)lhs.ptr - val_to_i64(rhs, rhs_type->builtin)
				* (I64)compiler_sizeof(lhs_type->ptr);
		} else {
			eval_binary_op_nums_only(-);
		}
		break;
	case BINARY_MUL:
		eval_binary_op_nums_only(*); break;
	case BINARY_DIV:
		eval_binary_op_nums_only(/); break;
	case BINARY_MOD:
		eval_binary_op_ints_only(%); break;
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
	default: assert(0); break;
	}
}

static Value val_zero(Type *t) {
	Value val = {0};
	switch (t->kind) {
	case TYPE_STRUCT:
		val.struc = err_calloc(1, compiler_sizeof(t));
		break;
	case TYPE_ARR:
		val.arr = err_calloc(t->arr.n, compiler_sizeof(t->arr.of));
		break;
	default:
		break;
	}
	return val;
}

static bool val_is_nonnegative(Value v, Type *t) {
	switch (t->builtin) {
	case BUILTIN_BOOL: assert(0); return false;
	case BUILTIN_CHAR: return v.charv >= 0;
	case BUILTIN_F32: return v.f32 >= 0;
	case BUILTIN_F64: return v.f64 >= 0;
	default: break;
	}
	if (!type_builtin_is_signed(t->builtin))
		return true;
	return val_to_i64(v, t->builtin) >= 0;
}

static Status eval_ident(Evaluator *ev, Identifier ident, Value *v, Location where) {
	if (ident->decl_kind == IDECL_NONE) {
		char *s = ident_to_str(ident);
		err_print(where, "Undeclared identifier: %s.", s);
		free(s);
		return false;
	}
	bool is_decl = ident->decl_kind == IDECL_DECL;
	Declaration *d = NULL;
	if (is_decl) {
		d = ident->decl;
		if ((d->flags & DECL_HAS_EXPR) && d->expr.kind == EXPR_TYPE && d->expr.typeval->kind == TYPE_STRUCT) {
			v->type = allocr_malloc(ev->allocr, sizeof *v->type);
			v->type->flags = TYPE_IS_RESOLVED;
			v->type->kind = TYPE_STRUCT;
			v->type->struc = d->expr.typeval->struc;
			return true;
		} else {
			if (!types_decl(ev->typer, d)) return false;
			assert(d->type.flags & TYPE_IS_RESOLVED);
		}
	}
	Value *ival = ident_val(ident);
	if (ival) {
		*v = *ival;
	} else {
		char *s = ident_to_str(ident);
			
		err_print(where, "Cannot evaluate non-constant '%s' at compile time.", s);
		free(s);
		return false;
	}
	return true;
}

static Value *decl_add_val(Declaration *d) {
	Value **valpp = arr_add(&d->val_stack);
	Value *valp = *valpp = err_malloc(sizeof *valp);
	if (arr_len(d->idents) > 1) {
		valp->tuple = err_malloc(arr_len(d->idents) * sizeof *valp->tuple);
	}
	return valp;
}
	
static void decl_remove_val(Declaration *d) {
	assert(arr_len(d->val_stack));
	Value **valpp = arr_last(d->val_stack);
	Value *valp = *valpp;
	if (arr_len(d->idents) == 1 || d->type.kind == TYPE_TUPLE) {
		val_free_ptr(valp, &d->type);
	} else {
		long idx = 0;
		arr_foreach(d->idents, Identifier, i)
			val_free(&valp->tuple[idx++], &d->type);
		free(valp->tuple);
		free(valp);
	}
	arr_remove_last(&d->val_stack);
}

static Status eval_expr(Evaluator *ev, Expression *e, Value *v) {
	
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
	
	if (!ev->enabled) return false; /* silently fail */	
	switch (e->kind) {
	case EXPR_UNARY_OP: {
		Value of;
		if (e->unary.op != UNARY_ADDRESS) {
			if (!eval_expr(ev, e->unary.of, &of)) return false;
		}
		Type *of_type = &e->unary.of->type;
		switch (e->unary.op) {
		case UNARY_ADDRESS: {
			Expression *o = e->unary.of;
			if (type_is_builtin(&o->type, BUILTIN_TYPE)) {
				if (!eval_expr(ev, e->unary.of, &of)) return false;
				/* "address" of type (pointer to type) */
				v->type = evalr_malloc(ev, sizeof *v->type);
				v->type->flags = 0;
				v->type->kind = TYPE_PTR;
				v->type->ptr = of.type;
				break;
			}
			if (!eval_address_of(ev, o, &v->ptr))
				return false;
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
			v->boolv = !val_truthiness(of, &e->unary.of->type);
			break;
		case UNARY_DEL:
			if (of_type->kind == TYPE_PTR)
				free(of.ptr);
			else {
				assert(of_type->kind == TYPE_SLICE);
				free(of.slice.data);
			}
			break;
		case UNARY_LEN:
			if (of_type->kind == TYPE_PTR) {
				/* dereference of */
				eval_deref(&of, of.ptr, of_type->ptr);
				of_type = of_type->ptr;
			}
			switch (of_type->kind) {
			case TYPE_SLICE:
				v->i64 = of.slice.n;
				break;
			case TYPE_ARR:
				v->i64 = (I64)of_type->arr.n;
				break;
			default: assert(0); break;
			}
			break;
		case UNARY_DSIZEOF:
		case UNARY_DALIGNOF:
		case UNARY_TYPEOF:
			assert(0);
			return false;
		}
	} break;
	case EXPR_BINARY_OP: {
		Value lhs, rhs;
		Expression *lhs_expr = e->binary.lhs, *rhs_expr = e->binary.rhs;
		if (e->binary.op != BINARY_SET)
			if (!eval_expr(ev, lhs_expr, &lhs)) return false;
		if (e->binary.op != BINARY_DOT)
			if (!eval_expr(ev, rhs_expr, &rhs)) return false;
		switch (e->binary.op) {
		case BINARY_DOT: {
			void *ptr;
			if (!eval_ptr_to_struct_field(ev, e, &ptr))
				return false;
			eval_deref(v, ptr, &e->type);
		} break;
		case BINARY_ADD:
		case BINARY_SUB:
		case BINARY_MUL:
		case BINARY_DIV:
		case BINARY_MOD:
		case BINARY_LT:
		case BINARY_LE:
		case BINARY_GT:
		case BINARY_GE:
		case BINARY_EQ:
		case BINARY_NE:
			eval_numerical_bin_op(lhs, &e->binary.lhs->type, e->binary.op, rhs, &e->binary.rhs->type, v, &e->type);
			break;
		case BINARY_SET:
			if (!eval_set(ev, e->binary.lhs, &rhs)) return false;
			break;
		case BINARY_SET_ADD:
		case BINARY_SET_SUB:
		case BINARY_SET_MUL:
		case BINARY_SET_DIV:
		case BINARY_SET_MOD: {
			BinaryOp subop = (BinaryOp)0;
			switch (e->binary.op) {
			case BINARY_SET_ADD: subop = BINARY_ADD; break;
			case BINARY_SET_SUB: subop = BINARY_SUB; break;
			case BINARY_SET_MUL: subop = BINARY_MUL; break;
			case BINARY_SET_DIV: subop = BINARY_DIV; break;
			case BINARY_SET_MOD: subop = BINARY_MOD; break;
			default: assert(0);
			}
			eval_numerical_bin_op(lhs, &e->binary.lhs->type, subop, rhs, &e->binary.rhs->type, v, &e->binary.lhs->type);
			if (!eval_set(ev, e->binary.lhs, v)) return false;
			break;
		} break;
		case BINARY_AT_INDEX: {
			void *ptr;
			Type *type;
			if (!eval_expr_ptr_at_index(ev, e, &ptr, &type))
				return false;
			eval_deref(v, ptr, type);
		} break;
		}	
	} break;
	case EXPR_LITERAL_INT:
		assert(e->type.kind == TYPE_BUILTIN);
		u64_to_val(v, e->type.builtin, (U64)e->intl);
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
		IfExpr *i = e->if_;
		if (i->cond) {
			Value cond;
			if (!eval_expr(ev, i->cond, &cond)) return false;
			if (val_truthiness(cond, &i->cond->type)) {
				if (!eval_block(ev, &i->body, v)) return false;
			} else if (i->next_elif && !ev->returning) {
				if (!eval_expr(ev, i->next_elif, v)) return false;
			}
		} else {
			if (!eval_block(ev, &i->body, v)) return false;
		}
	} break;
	case EXPR_WHILE: {
		Value cond;
		WhileExpr *w = e->while_;
		while (1) {
			if (w->cond) {
				if (!eval_expr(ev, w->cond, &cond)) return false;
				Type *cond_type = &w->cond->type;
				if (!val_truthiness(cond, cond_type))
					break;
			}
			if (!eval_block(ev, &w->body, v)) return false;
			if (ev->returning) {
				if (ev->returning == &w->body) {
					ev->returning = NULL;
					if (ev->is_break)
						break;
				} else break;
			}
		}
	} break;
	case EXPR_FOR: {
		ForExpr *fo = e->for_;
		Value *index_val;
		Value *value_val;
		Value **for_valpp = arr_add(&fo->val_stack);
		Value *for_valp = *for_valpp = err_malloc(sizeof *for_valp);
		if (fo->index && fo->value) {
			/* make a tuple */
			for_valp->tuple = err_malloc(2 * sizeof *for_valp->tuple);
		}
		if (fo->index) {
			index_val = fo->value ? &for_valp->tuple[0] : for_valp;
		} else {
			index_val = NULL;
		}
		if (fo->value) {
			value_val = fo->index ? &for_valp->tuple[1] : for_valp;
		} else {
			value_val = NULL;
		}
		if (fo->flags & FOR_IS_RANGE) {
			Value from, to;
			Value stepval;
			stepval.i64 = 1;
			Type i64t = {0};
			i64t.flags = TYPE_IS_RESOLVED;
			i64t.kind = TYPE_BUILTIN;
			i64t.builtin = BUILTIN_I64;
			if (!eval_expr(ev, fo->range.from, &from)) return false;
			if (fo->range.to && !eval_expr(ev, fo->range.to, &to)) return false;
			if (fo->range.stepval)
				stepval = *fo->range.stepval;
			Value x = from;
			bool step_is_negative = fo->range.stepval && !val_is_nonnegative(stepval, &fo->type);
			if (index_val) index_val->i64 = 0;
			while (1) {
				if (fo->range.to) {
					/* check if loop has ended */
					Value lhs = x;
					Value rhs = to;
					assert(fo->type.kind == TYPE_BUILTIN);
					Type boolt = {0};
					boolt.flags = TYPE_IS_RESOLVED;
					boolt.kind = TYPE_BUILTIN;
					boolt.builtin = BUILTIN_BOOL;
					Value cont;
					
					eval_numerical_bin_op(lhs, &fo->type, step_is_negative ? BINARY_GE : BINARY_LE, rhs, &fo->range.to->type, &cont, &boolt);
					if (!cont.boolv) break;
				}
				if (value_val) *value_val = x;

				if (!eval_block(ev, &fo->body, v)) return false;
				
				if (ev->returning) {
					if (ev->returning == &fo->body) {
						ev->returning = NULL;
						if (ev->is_break)
							break;
					} else break;
				}
				if (index_val) {
					++index_val->i64;
				}
				eval_numerical_bin_op(x, &fo->type, BINARY_ADD, stepval, fo->range.stepval ? &fo->type : &i64t, &x, &fo->type);
			}
				
		} else {
			Value x;
			Value *index = index_val ? index_val : &x;
			Value of;
			if (!eval_expr(ev, fo->of, &of)) return false;
			I64 len;
			bool uses_ptr = false;
			Type *of_type = &fo->of->type;
			if (of_type->kind == TYPE_PTR) {
				uses_ptr = true;
				of_type = of_type->ptr;
			}
			switch (of_type->kind) {
			case TYPE_ARR:
				len = (I64)of_type->arr.n;
				if (uses_ptr) {
					of.arr = of.ptr;
				}
				
				break;
			case TYPE_SLICE:
				if (uses_ptr) {
					of.slice = *(Slice *)of.ptr;
				}
				len = of.slice.n;
				break;
			default: assert(0); return false;
			}
			
			index->i64 = 0;
			while (index->i64 < len) {
				void *ptr = NULL;
				if (!eval_val_ptr_at_index(e->where, &of, (U64)index->i64, of_type, &ptr, NULL))
					return false;
				if (uses_ptr)
					value_val->ptr = ptr;
				else
					eval_deref(value_val, ptr, &fo->type);
				if (!eval_block(ev, &fo->body, v))
					return false;
				if (ev->returning) {
					if (ev->returning == &fo->body) {
						ev->returning = NULL;
						if (ev->is_break)
							break;
					} else break;
				}
				++index->i64;
			}
		}
		arr_remove_last(&fo->val_stack);
		if (fo->index && fo->value) {
			free(for_valp->tuple);
		}
		free(for_valp);
	} break;
	case EXPR_BLOCK:
		if (!eval_block(ev, e->block, v)) return false;
		break;
	case EXPR_LITERAL_BOOL:
		v->boolv = e->booll;
		break;
	case EXPR_LITERAL_CHAR:
		v->charv = e->charl;
		break;
	case EXPR_LITERAL_STR:
		v->slice.data = e->strl.str;
		v->slice.n = (I64)e->strl.len;
		break;
	case EXPR_CAST: {
		Value casted;
		if (!eval_expr(ev, e->cast.expr, &casted)) return false;
		val_cast(&casted, &e->cast.expr->type, v, &e->cast.type);
	} break;
	case EXPR_FN:
		v->fn = e->fn;
		break;
	case EXPR_IDENT:
		if (!eval_ident(ev, e->ident, v, e->where))
			return false;
		break;
	case EXPR_TUPLE: {
		size_t i, n = arr_len(e->tuple);
		v->tuple = err_malloc(n * sizeof *v->tuple);
		for (i = 0; i < n; ++i) {
			if (!eval_expr(ev, &e->tuple[i], &v->tuple[i]))
				return false;
		}
	} break;
	case EXPR_C:
		err_print(e->where, "Cannot run C code at compile time.");
		return false;
	case EXPR_BUILTIN:
		*v = get_builtin_val(e->builtin.which.val);
		break;
	case EXPR_NEW:
		/* it's not strictly necessary to do the if here */
		if (e->new.n) {
			Value n;
			if (!eval_expr(ev, e->new.n, &n))
				return false;
			U64 n64 = val_to_u64(n, e->new.n->type.builtin);
			v->slice.data = err_calloc(n64, compiler_sizeof(&e->new.type));
			v->slice.n = (I64)n64;
		} else {
			v->ptr = err_calloc(1, compiler_sizeof(&e->new.type));
		}
		break;
	case EXPR_CALL: {
		FnExpr *fn;
		if (e->call.instance) {
			fn = e->call.instance->fn;
		} else {
			Value fnv;
			if (!eval_expr(ev, e->call.fn, &fnv)) {
				return false;
			}
			fn = fnv.fn;
		}
		size_t nargs = arr_len(e->call.arg_exprs);
		if (fn->flags & FN_EXPR_FOREIGN) {
			Value *args = err_malloc(nargs * sizeof *args);
			for (size_t i = 0; i < nargs; ++i) {
				if (!eval_expr(ev, &e->call.arg_exprs[i], &args[i]))
					return false;
			}
			Type *ret_type = &e->call.fn->type.fn.types[0];
			bool success = foreign_call(&ev->ffmgr, fn, ret_type, &e->call.arg_exprs[0].type, sizeof(Expression), args, nargs, e->where, v);
			free(args);
			if (!success)
				return false;
			break;
		}
		
		/* set parameter values */
		Declaration *params = fn->params;
		Expression *arg = e->call.arg_exprs;
		arr_foreach(params, Declaration, p) {
			int idx = 0;
			Value *pval = decl_add_val(p);
			--arr_hdr(p->val_stack)->len;
			bool multiple_idents = arr_len(p->idents) > 1;
			bool is_tuple = p->type.kind == TYPE_TUPLE;
			if (type_is_builtin(&p->type, BUILTIN_VARARGS)) {
				Expression *args_end = e->call.arg_exprs + nargs;
				/* set varargs */
				pval->varargs = NULL;
				for (; arg != args_end; ++arg) {
					VarArg *varg = arr_add(&pval->varargs);
					if (!eval_expr(ev, arg, &varg->val))
						return false;
					varg->type = &arg->type;
				}
			} else {
				arr_foreach(p->idents, Identifier, i) {
					Value arg_val;
					if (!eval_expr(ev, arg, &arg_val))
						return false;
					Type *type = is_tuple ? &p->type.tuple[idx] : &p->type;
					Value *ival = multiple_idents ? &pval->tuple[idx] : pval;
					copy_val(NULL, ival, arg_val, type);
					++arg;
					++idx;
				}
			}
			++arr_hdr(p->val_stack)->len;
		}

		arr_foreach(fn->ret_decls, Declaration, d) {
			int idx = 0;
			Value ret_decl_val;
			if (d->flags & DECL_HAS_EXPR)
				if (!eval_expr(ev, &d->expr, &ret_decl_val))
					return false;
			Value *dval = decl_add_val(d);
			bool multiple_idents = arr_len(d->idents) > 1;
			bool is_tuple = d->type.kind == TYPE_TUPLE;
			arr_foreach(d->idents, Identifier, i) {
				Value *ival = multiple_idents ? &dval->tuple[idx] : dval;
				Type *type = is_tuple ? &d->type.tuple[idx] : &d->type;
				if (d->flags & DECL_HAS_EXPR) {
					*ival = d->type.kind == TYPE_TUPLE ? ret_decl_val.tuple[idx] : ret_decl_val;
				} else {
					*ival = val_zero(type);
				}
				++idx;
			}
		}
			
		/* make sure function body is typed before calling it */
		if (!types_block(ev->typer, &fn->body))
			return false;
		
		if (!eval_block(ev, &fn->body, v)) {
			return false;
		}
		if (fn->ret_decls) {
			Value *tuple = NULL;
			arr_foreach(fn->ret_decls, Declaration, d) {
				int i = 0;
				arr_foreach(d->idents, Identifier, ident) {
					Value this_one;
					Expression expr;
					expr.flags = EXPR_FOUND_TYPE;
					expr.kind = EXPR_IDENT;
					expr.ident = *ident;
					if (!eval_expr(ev, &expr, &this_one))
						return false;
					Value *element = arr_add(&tuple);
					Type *type = decl_type_at_index(d, i);
					copy_val(NULL, element, this_one, type);
					++i;
				}
			}
			if (arr_len(tuple) == 1) {
				*v = tuple[0];
				arr_clear(&tuple);
			} else {
				v->tuple = tuple;
			}
		}
		if (ev->returning) {
			if (fn->ret_type.kind != TYPE_VOID && !fn->ret_decls)
				*v = ev->ret_val;
			ev->returning = NULL;
		}
		arr_foreach(fn->params, Declaration, p)
			decl_remove_val(p);
		arr_foreach(fn->ret_decls, Declaration, d)
			decl_remove_val(d);
	} break;
	case EXPR_SLICE: {
		SliceExpr *s = &e->slice;
		Value ofv;
		Type *of_type = &s->of->type;
		if (!eval_expr(ev, s->of, &ofv))
			return false;
		U64 n = of_type->kind == TYPE_ARR ? of_type->arr.n : (U64)ofv.slice.n;
		U64 from, to;
		if (s->from) {
			Value fromv;
			if (!eval_expr(ev, s->from, &fromv))
				return false;
			assert(s->from->type.kind == TYPE_BUILTIN);
			from = val_to_u64(fromv, s->from->type.builtin);
		} else {
			from = 0;
		}
		if (s->to) {
			Value tov;
			if (!eval_expr(ev, s->to, &tov))
				return false;
			assert(s->to->type.kind == TYPE_BUILTIN);
			to = val_to_u64(tov, s->to->type.builtin);
		} else {
			to = n;
		}
		/* TODO: is this the best check? (Go also checks if from > to) */
		if (to > n) {
			err_print(e->where, "Slice index out of bounds (to = %lu, length = %lu).", (unsigned long)to, (unsigned long)n);
			return false;
		}
		if (from < to) {
			void *ptr_start = NULL;
			if (!eval_val_ptr_at_index(e->where, &ofv, from, of_type, &ptr_start, NULL))
				return false;
			v->slice.data = ptr_start;
			v->slice.n = (I64)(to - from);
		} else {
			v->slice.data = NULL;
			v->slice.n = 0;
		}
	} break;
	case EXPR_TYPE:
		v->type = e->typeval;
		break;
	case EXPR_NMS:
		v->nms = e->nms;
		break;
	case EXPR_VAL:
		*v = e->val;
		break;
	}
	return true;
}

static Status eval_decl(Evaluator *ev, Declaration *d) {
	unsigned has_expr = d->flags & DECL_HAS_EXPR;
	unsigned is_const = d->flags & DECL_IS_CONST;
	Value val = {0};

	if (has_expr) {
		if (is_const) {
			if (!(d->flags & DECL_FOUND_VAL)) {
				if (!eval_expr(ev, &d->expr, &d->val))
					return false;
				d->flags |= DECL_FOUND_VAL;
			}
		} else {
			if (!eval_expr(ev, &d->expr, &val))
				return false;
		}
	}
	
	if (!is_const) {
		int index = 0;
		Value *dval = decl_add_val(d);
		bool is_tuple = d->type.kind == TYPE_TUPLE;
		bool multiple_idents = arr_len(d->idents) > 1;
		bool need_to_copy = d->expr.kind != EXPR_CALL; /* don't copy if it's a return value */
		
		arr_foreach(d->idents, Identifier, ip) {
			Value *ival = multiple_idents ? &dval->tuple[index] : dval;
			Type *type = decl_type_at_index(d, index);
			if (!is_const) {
				if (has_expr) {
					Value v = is_tuple ? val.tuple[index] : val;
					if (need_to_copy) {
						copy_val(NULL, ival, v, type);
					} else {
						*ival = v;
					}
				} else {
					*ival = val_zero(type);
				}
			}
			++index;
		}
		if (d->type.kind == TYPE_TUPLE)
			free(val.tuple);
	}
	return true;
}


static void eval_exit_stmts(Statement *stmts, Statement *last_reached) {
	if (stmts) {
		for (Statement *s = stmts; s <= last_reached; ++s) {
			if (s->kind == STMT_DECL && !(s->decl->flags & DECL_IS_CONST)) {
				Declaration *d = s->decl;
				decl_remove_val(d);
			}
			/* STMT_INCLUDEs are handled by eval_stmt; don't worry */
		}
	}
}

static Status eval_stmt(Evaluator *ev, Statement *stmt) {
	switch (stmt->kind) {
	case STMT_DECL:
		if (!eval_decl(ev, stmt->decl)) return false;
		break;
	case STMT_EXPR: {
		Value unused;
		if (!eval_expr(ev, &stmt->expr, &unused))
			return false;
	} break;
	case STMT_RET: {
		if (stmt->ret.flags & RET_HAS_EXPR) {
			Value r;
			if (!eval_expr(ev, &stmt->ret.expr, &r))
				return false;
			copy_val(NULL, &ev->ret_val, r, &stmt->ret.expr.type);
		}
		ev->returning = stmt->ret.referring_to;
	} break;
	case STMT_BREAK:
		ev->returning = stmt->referring_to;
		ev->is_break = true;
		break;
	case STMT_CONT:
		ev->returning = stmt->referring_to;
		ev->is_break = false;
		break;
	case STMT_INCLUDE: {
		Statement *last_reached = arr_last(stmt->inc.stmts);
		arr_foreach(stmt->inc.stmts, Statement, sub) {
			if (!eval_stmt(ev, sub))
				return false;
			if (ev->returning) {
				last_reached = sub;
				break;
			}
		}
		eval_exit_stmts(stmt->inc.stmts, last_reached);
	} break;
	case STMT_MESSAGE:
		break;
	}
	return true;
}

static Status eval_block(Evaluator *ev, Block *b, Value *v) {
	Block *prev = ev->typer->block;
	ev->typer->block = b;
	bool success = true;
	Statement *last_reached = arr_last(b->stmts);
	arr_foreach(b->stmts, Statement, stmt) {
		if (!eval_stmt(ev, stmt)) {
			success = false;
			goto ret;
		}
		if (ev->returning) {
			last_reached = stmt;
			break;
		}
	}
	if (!ev->returning && b->ret_expr) {
		Value r;
		if (!eval_expr(ev, b->ret_expr, &r)) {
			success = false;
			goto ret;
		}
		if (!type_is_builtin(&b->ret_expr->type, BUILTIN_TYPE)) {
			/* make a copy so that r's data isn't freed when we exit the block */
			copy_val(NULL, v, r, &b->ret_expr->type);
			if (b->ret_expr->kind == EXPR_TUPLE)
				free(r.tuple);
		} else {
			*v = r;
		}
	}
	eval_exit_stmts(b->stmts, last_reached);
 ret:
	ev->typer->block = prev;
	return success;
}
