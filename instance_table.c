/*
  Copyright (C) 2019 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
/* 
   TODO: better hash functions, especially for integers 
   (right now, nearby integers are close together in hash 
   space, which is bad with the way these hash tables
   are designed)
*/

/* NOTE: any time you see 0x then 16 hexadecimal digits, that's probably a random number for hashing */

/*
  hash tables are initialized by setting them to {0}, e.g.
  HashTable x = {0};
*/
static void *val_get_ptr(Value *v, Type *t);
static U64 val_hash(Value v, Type *t);
static size_t compiler_sizeof(Type *t);
static bool val_eq(Value u, Value v, Type *t);
static bool type_eq(Type *t1, Type *t2);

static U64 f32_hash(F32 f) {
	/* OPTIM */
	U64 hash = 0;
	if (f < 0) {
		hash = 0x9a6db29edcba8af4;
		f = -f;
	}
	F32 last = f;
	int exponent = 0;
	while (f > 1.0f) {
		++exponent;
		f /= 10;
		if (f == last) {
			/* +/- infinity probably */
			hash ^= 0x78bf61a81e80b9f2;
			return hash;
		}
		last = f;
	}
	for (int i = 0; i < F32_MANT_DIG; ++i) {
		f *= 10;
	}
	hash ^= (U64)exponent + (U64)F32_DIG * (U64)f;
	return hash;
}

static U64 f64_hash(F64 f) {
	/* OPTIM */
	U64 hash = 0;
	if (f < 0) {
		hash = 0x9a6db29edcba8af4;
		f = -f;
	}
	F64 last = f;
	int exponent = 0;
	while (f > 1.0) {
		++exponent;
		f /= 10;
		if (f == last) {
			/* +/- infinity probably */
			hash ^= 0x78bf61a81e80b9f2;
			return hash;
		}
		last = f;
	}
	for (int i = 0; i < F64_MANT_DIG; ++i) {
		f *= 10;
	}
	hash ^= (U64)exponent + (U64)F64_DIG * (U64)f;
	return hash;
}

static U64 type_hash(Type *t) {
	static const U64 starters[TYPE_COUNT] = {
											 0x40d8675d0f148df7,
											 0xee4db91f31fdf2e1,
											 0x94963ca71f7f0df4,
											 0x95b9d36f45f27cf2,
											 0x68d393d7cade0570,
											 0x8191b5178d728e8c,
											 0x50da97f1211b2423,
											 0xc3977306abd0ae6c,
											 0x87ea684427e1c521,
											 0xcee5fd6d6cbdfe23,
											 0xd80dd2469d6e7c1b
	};
	U64 hash = starters[t->kind];
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_BUILTIN:
		return hash + (U64)t->builtin * 0x1307787dfff73417;
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TYPE:
		return hash;
	case TYPE_TUPLE:
		arr_foreach(t->tuple, Type, sub)
			hash = hash * type_hash(sub) + 0x16225b0aa9993299;
		return hash;
	case TYPE_FN:
		arr_foreach(t->fn.types, Type, sub)
			hash = hash * type_hash(sub) + 0x2092d851ab2008de;
		return hash;
	case TYPE_PTR:
		hash += type_hash(t->ptr) * 0x277caae472151119 + 0xf5c6ae7b4dae3bcf;
		return hash;
	case TYPE_SLICE:
		hash += type_hash(t->slice) * 0x67a571620f9a5d6a + 0xc3f91e92c844ab1f;
		return hash;
	case TYPE_STRUCT:
		hash += (U64)t->struc;
		return hash;
	case TYPE_ARR:
		hash += type_hash(t->arr.of) * 0x3b6256104800a414 + 0xa901e68bbd8968a1
			+ 0xbf79c81a3e68e504 * t->arr.n;
		return hash;
	case TYPE_EXPR: break;
	}
	assert(0); return 0;
}

/* Note that for these value hashing functions, values of different types may collide */
static U64 val_ptr_hash(void *v, Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_VOID: return 0;
	case TYPE_UNKNOWN: return 0;
	case TYPE_BUILTIN:
		switch (t->builtin) {
		case BUILTIN_I8: return (U64)*(I8 *)v;
		case BUILTIN_U8: return (U64)*(U8 *)v;
		case BUILTIN_I16: return (U64)*(I16 *)v;
		case BUILTIN_U16: return (U64)*(U16 *)v;
		case BUILTIN_I32: return (U64)*(I32 *)v;
		case BUILTIN_U32: return (U64)*(U32 *)v;
		case BUILTIN_I64: return (U64)*(I64 *)v;
		case BUILTIN_U64: return (U64)*(U64 *)v;
		case BUILTIN_F32: return f32_hash(*(F32 *)v);
		case BUILTIN_F64: return f64_hash(*(F64 *)v);
		case BUILTIN_CHAR: return (U64)*(char *)v;
		case BUILTIN_BOOL: return (U64)*(bool *)v;
		}
		assert(0);
		return 0;
	case TYPE_FN: return (U64)*(FnExpr **)v;
	case TYPE_TUPLE: {
		U64 hash = 0;
		Value *elems = *(Value **)v;
		U32 x = 1;
		for (U64 i = 0; i < (U64)arr_len(t->tuple); ++i) {
			hash += (U64)x * val_hash(elems[i], &t->tuple[i]);
			x = rand_u32(x);
		}
		return hash;
	}
	case TYPE_PTR: return (U64)*(void **)v;
	case TYPE_TYPE:
		return type_hash(*(Type **)v);
	case TYPE_ARR: {
		U32 x = 1;
		U64 hash = 0;
		U64 size = (U64)compiler_sizeof(t->arr.of);
		for (U64 i = 0; i < (U64)t->arr.n; ++i) {
			hash += (U64)x * val_ptr_hash((char *)v + i * size, t->arr.of);
			x = rand_u32(x);
		}
		return hash;
	}
	case TYPE_SLICE: {
		U32 x = 1;
		U64 hash = 0;
		Slice *s = v;
		U64 size = (U64)compiler_sizeof(t->slice);
		for (U64 i = 0; i < (U64)s->n; ++i) {
			hash += (U64)x * val_ptr_hash((char *)s->data + i * size, t->slice);
			x = rand_u32(x);
		}
		return hash;
	}
	case TYPE_STRUCT: {
		U32 x = 1;
		U64 hash = 0;
		arr_foreach(t->struc->fields, Field, f) {
			hash += (U64)x * val_ptr_hash((char *)v + f->offset, f->type);
			x = rand_u32(x);
		}
		return hash;
	}
	case TYPE_EXPR: break;
	}
	assert(0);
	return 0;
}

static U64 val_hash(Value v, Type *t) {
	return val_ptr_hash(val_get_ptr(&v, t), t);
}

static bool val_ptr_eq(void *u, void *v, Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_BUILTIN:
		switch (t->builtin) {
		case BUILTIN_I8: return *(I8 *)u == *(I8 *)v;
		case BUILTIN_U8: return *(U8 *)u == *(U8 *)v;
		case BUILTIN_I16: return *(I16 *)u == *(I16 *)v;
		case BUILTIN_U16: return *(U16 *)u == *(U16 *)v;
		case BUILTIN_I32: return *(I32 *)u == *(I32 *)v;
		case BUILTIN_U32: return *(U32 *)u == *(U32 *)v;
		case BUILTIN_I64: return *(I64 *)u == *(I64 *)v;
		case BUILTIN_U64: return *(U64 *)u == *(U64 *)v;
		case BUILTIN_F32: return *(F32 *)u == *(F32 *)v;
		case BUILTIN_F64: return *(F64 *)u == *(F64 *)v;
		case BUILTIN_BOOL: return *(bool *)u == *(bool *)v;
		case BUILTIN_CHAR: return *(char *)u == *(char *)v;
		}
		break;
	case TYPE_VOID:
		return true;
	case TYPE_UNKNOWN:
		return false;
	case TYPE_FN:
		return *(FnExpr **)u == *(FnExpr **)v;
	case TYPE_PTR:
		return *(void **)u == *(void **)v;
	case TYPE_TYPE: {
		bool ret = type_eq(*(Type **)u, *(Type **)v);
		return ret;
	}
	case TYPE_TUPLE: {
		Value *us = *(Value **)u;
		Value *vs = *(Value **)v;
	    for (size_t i = 0; i < arr_len(t->tuple); ++i) {
			if (!val_eq(us[i], vs[i], &t->tuple[i]))
				return false;
		}
	    return true;
	}
	case TYPE_ARR: {
		U64 size = (U64)compiler_sizeof(t->arr.of);
		char *uptr = u, *vptr = v;
		for (U64 i = 0; i < t->arr.n; ++i) {
			if (!val_ptr_eq(uptr, vptr, t->arr.of))
				return false;
			uptr += size;
			vptr += size;
		}
		return true;
	}
	case TYPE_SLICE: {
		U64 size = (U64)compiler_sizeof(t->arr.of);
		Slice *r = u;
		Slice *s = v;
		if (r->n != s->n) return false;
		char *sptr = r->data, *tptr = s->data;
		for (U64 i = 0; i < (U64)s->n; ++i) {
			if (!val_ptr_eq(sptr, tptr, t->slice))
				return false;
			sptr += size;
			tptr += size;
		}
		return true;
	}
	case TYPE_STRUCT:
		arr_foreach(t->struc->fields, Field, f) {
			if (!val_ptr_eq((char *)u + f->offset, (char *)v + f->offset, f->type))
				return false;
		}
	    return true;
	case TYPE_EXPR: break;
	}
	assert(0);
	return false;
}

static bool val_eq(Value u, Value v, Type *t) {
	return val_ptr_eq(val_get_ptr(&u, t), val_get_ptr(&v, t), t);
}

/*
  if already_exists is not NULL, this will create the instance if it does not exist,
  and set already_exists accordingly
*/
/* OPTIM: store instances in a block array (remember that the pointers need to stay valid!) */
static Instance *instance_table_adda(Allocator *a, HashTable *h, Value v, Type *t,
									 bool *already_exists) {
	if (h->n * 2 >= h->cap) {
		U64 new_cap = h->cap * 2 + 3;
		Instance **new_data = allocr_malloc(a, (size_t)new_cap * sizeof *new_data);
		bool *new_occupied = allocr_calloc(a, (size_t)new_cap, sizeof *new_occupied);
		Instance **old_data = h->data;
		bool *old_occupied = h->occupied;
		for (U64 i = 0; i < h->cap; ++i) {
			/* re-hash */
			if (old_occupied[i]) {
				U64 index = val_hash(old_data[i]->val, t) % new_cap;
				while (new_occupied[index]) {
					++index;
					if (index >= new_cap)
						index -= new_cap;
				}
				new_data[index] = old_data[i];
			    new_occupied[index] = true;
			}
		}
		h->data = new_data;
		h->occupied = new_occupied;
		allocr_free(a, old_occupied, h->cap * sizeof *old_occupied);
		allocr_free(a, old_data, h->cap * sizeof *old_data);
		h->cap = new_cap;
	}
	Instance **data = h->data;
	U64 index = val_hash(v, t) % h->cap;
	while (1) {
		if (h->occupied[index]) {
			if (val_eq(v, data[index]->val, t)) {
				*already_exists = true;
				return data[index];
			}
		} else break;
		++index;
		if (index >= h->cap)
			index -= h->cap;
	}
	if (already_exists) {
		/* create, because it doesn't exist */
		*already_exists = false;
		data[index] = allocr_malloc(a, sizeof *data[index]);
		data[index]->val = v;
		h->occupied[index] = true;
		++h->n;
		return data[index];
	}
	return NULL;
}


#if 0
/* only call if you're not using an allocator */
static void hash_table_free(HashTable *h) {
	free(h->data);
	free(h->occupied);
}
#endif
