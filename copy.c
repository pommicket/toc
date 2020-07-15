/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.


  these copy functions MUST be used before typing!!!! (except for copy_val)

  -----
  IMPORTANT:
  These functions are like memcpy, in that in and out must not overlap!
  -----
*/

typedef struct {
	Allocator *allocr;
	Block *block;
} Copier;

static Expression *copy_expr_(Copier *c, Expression *in);
static void copy_expr(Copier *c, Expression *out, Expression *in);
static void copy_decl(Copier *c, Declaration *out, Declaration *in);
enum {
	  COPY_BLOCK_DONT_CREATE_IDENTS = 0x01
};
static void copy_block(Copier *c, Block *out, Block *in, U8 flags);
static void copy_type(Copier *c, Type *out, Type *in);
static Type *copy_type_(Copier *c, Type *in);

static Copier copier_create(Allocator *a, Block *b) {
	Copier c;
	c.allocr = a;
	c.block = b;
	return c;
}

static inline void *copier_malloc(Copier *c, size_t n) {
	return allocr_malloc(c->allocr, n);
}

static void copy_val(Allocator *a, Value *out, Value in, Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_BUILTIN:
		if (t->builtin == BUILTIN_VARARGS) {
			size_t n = arr_len(in.varargs);
			out->varargs = NULL;
			arr_set_lena(out->varargs, n, a);
			for (size_t i = 0; i < n; ++i) {
				Copier c = copier_create(a, NULL); // since the type is resolved, it doesn't matter that the block is wrong
				out->varargs[i].type = copy_type_(&c, in.varargs[i].type);
				out->varargs[i].val = in.varargs[i].val;
			}
			break;
		}
		// fallthrough
	case TYPE_FN:
	case TYPE_PTR:
	case TYPE_SLICE:
	case TYPE_UNKNOWN:
		*out = in;
		break;
	case TYPE_ARR: {
		size_t bytes = (size_t)t->arr->n * compiler_sizeof(t->arr->of);
		out->arr = allocr_malloc(a, bytes);
		memcpy(out->arr, in.arr, bytes);
	} break;
	case TYPE_TUPLE: {
		size_t bytes = arr_len(t->tuple) * sizeof(*out->tuple);
		out->tuple = allocr_malloc(a, bytes);
		memcpy(out->tuple, in.tuple, bytes);
	} break;
	case TYPE_STRUCT: {
		size_t bytes = compiler_sizeof(t);
		out->struc = allocr_malloc(a, bytes);
		memcpy(out->struc, in.struc, bytes);
	} break;
	case TYPE_EXPR:
		assert(0);
		break;
	}
}

#if 0
static void copy_val_full(Copier *c, Value *out, Value in, Type *t) {
	if (type_is_builtin(t, BUILTIN_TYPE)) {
		Type *new_type = allocr_malloc(c->allocr, sizeof *new_type);
		copy_type(c, new_type, in.type);
		out->type = new_type;
	} else {
		copy_val(c->allocr, out, in, t);
	}
}
#endif

static void copy_struct(Copier *c, StructDef *out, StructDef *in) {
	*out = *in;
	out->body = in->body;
	idents_create(&out->body.idents, c->allocr, &out->body);
	
	Block *prev = c->block;
	copy_block(c, &out->body, &in->body, 0);
	c->block = &out->body;
	if (in->flags & STRUCT_DEF_RESOLVED) {
		size_t nfields = arr_len(in->fields);
		out->fields = NULL;

		arr_set_lena(out->fields, nfields, c->allocr);
		for (size_t i = 0; i < nfields; ++i) {
			Field *fout = &out->fields[i];
			Field *fin = &in->fields[i];
			*fout = *fin;
			fout->type = copy_type_(c, fin->type);
		}
	}
	size_t nparams = arr_len(in->params);
	out->params = NULL;
	arr_set_lena(out->params, nparams, c->allocr);
	for (size_t i = 0; i < nparams; ++i) {
		copy_decl(c, &out->params[i], &in->params[i]);
	}
	c->block = prev;
}

// works on unresolved and resolved types (for inference)
static void copy_type(Copier *c, Type *out, Type *in) {
	*out = *in;
	switch (in->kind) {
	case TYPE_BUILTIN:
	case TYPE_UNKNOWN:
		break;
	case TYPE_EXPR:
		out->expr = copy_expr_(c, in->expr);
		break;
	case TYPE_FN: {
		FnType *outfn = copier_malloc(c, sizeof *outfn), *infn = in->fn;
		size_t ntypes = arr_len(infn->types);
		*outfn = *infn;
		size_t constness_bytes = (ntypes-1) * sizeof *outfn->constness;
		outfn->constness = allocr_malloc(c->allocr, constness_bytes);
	gcc_no_bounds_warnings_start
		memmove(outfn->constness, infn->constness, constness_bytes);
	gcc_no_bounds_warnings_end

		outfn->types = NULL;
		arr_set_lena(outfn->types, ntypes, c->allocr);
		for (size_t i = 0; i < ntypes; ++i) {
			copy_type(c, &outfn->types[i], &infn->types[i]);
		}
	} break;
	case TYPE_TUPLE: {
		size_t ntypes = arr_len(in->tuple);
		out->tuple = NULL;
		arr_set_lena(out->tuple, ntypes, c->allocr);
		for (size_t i = 0; i < ntypes; ++i) {
			copy_type(c, &out->tuple[i], &in->tuple[i]);
		}
	} break;
	case TYPE_ARR:{
		ArrType *oarr = out->arr = copier_malloc(c, sizeof *oarr), *iarr = in->arr;
		*oarr = *iarr;
		if (!(in->flags & TYPE_IS_RESOLVED)) {
			oarr->n_expr = copy_expr_(c, iarr->n_expr);
		}
		oarr->of = copy_type_(c, in->arr->of);
	} break;
	case TYPE_PTR:
		out->ptr = copy_type_(c, in->ptr);
		break;
	case TYPE_SLICE:
		out->slice = copy_type_(c, in->slice);
		break;
	case TYPE_STRUCT: {
		if (in->flags & TYPE_IS_RESOLVED) {
			// we don't actually need to make a copy of the struct here
		} else {
			/*
			   it's okay to copy the struct definition here, because before resolving,
			   only one thing can point to a given StructDef
			*/
			out->struc = allocr_malloc(c->allocr, sizeof *out->struc);
			copy_struct(c, out->struc, in->struc);
		}
	} break;
	}
}

static Type *copy_type_(Copier *c, Type *in) {
	Type *out = allocr_malloc(c->allocr, sizeof *out);
	copy_type(c, out, in);
	return out;
}

enum {
	  COPY_FN_EXPR_DONT_COPY_BODY = 0x01
};

static void copy_fn_expr(Copier *c, FnExpr *fout, FnExpr *fin, U8 flags) {
	*fout = *fin;
	bool copy_body = (flags & COPY_FN_EXPR_DONT_COPY_BODY) == 0;
	
	if (fin->flags & FN_EXPR_FOREIGN) {
		copy_expr(c, fout->foreign.name_expr = copier_malloc(c, sizeof *fin->foreign.name_expr), fin->foreign.name_expr);
		copy_expr(c, fout->foreign.lib_expr = copier_malloc(c, sizeof *fin->foreign.lib_expr), fin->foreign.lib_expr);
		copy_type(c, &fout->foreign.type, &fin->foreign.type);
		size_t nctypes = arr_len(fin->foreign.type.fn->types);
		fout->foreign.ctypes = copier_malloc(c, nctypes * sizeof(CType));
		memcpy(fout->foreign.ctypes, fin->foreign.ctypes, nctypes * sizeof(CType));
	} else {
		Block *prev = c->block;
		c->block = &fout->body;
		idents_create(&fout->body.idents, c->allocr, &fout->body);
		size_t i;
		size_t nparam_decls = arr_len(fin->params);
		fout->params = NULL;
		arr_set_lena(fout->params, nparam_decls, c->allocr);
		for (i = 0; i < nparam_decls; ++i)
			copy_decl(c, fout->params + i, fin->params + i);
		size_t nret_decls = arr_len(fin->ret_decls);
		if (fin->ret_decls) {
			fout->ret_decls = NULL;
			arr_set_lena(fout->ret_decls, nret_decls, c->allocr);
			for (i = 0; i < nret_decls; ++i)
				copy_decl(c, fout->ret_decls + i, fin->ret_decls + i);
		}
		copy_type(c, &fout->ret_type, &fin->ret_type);
		c->block = prev;
		if (copy_body) {
			copy_block(c, &fout->body, &fin->body, COPY_BLOCK_DONT_CREATE_IDENTS);
		}
	}
}

static inline void copier_ident_translate(Copier *c, Identifier *i) {
	assert(c->block);
	assert(c->block->idents.scope == c->block);
	*i = ident_translate_forced(*i, &c->block->idents);
}

// in must be untyped!
static void copy_expr(Copier *c, Expression *out, Expression *in) {
	Allocator *a = c->allocr;
	*out = *in; /* NOTE : if in the future you are removing this, make sure in->type is still copied for EXPR_VAL,
		which sometimes exists before typing, e.g. for null */
	assert(!(in->flags & EXPR_FOUND_TYPE));
	switch (in->kind) {
	case EXPR_LITERAL_FLOAT:
	case EXPR_LITERAL_INT:
	case EXPR_LITERAL_STR:
	case EXPR_LITERAL_CHAR:
	case EXPR_LITERAL_BOOL:
	case EXPR_IDENT:
		break;
	case EXPR_UNARY_OP:
		out->unary.of = copy_expr_(c, in->unary.of);
		break;
	case EXPR_BINARY_OP:
		out->binary.lhs = copy_expr_(c, in->binary.lhs);
		out->binary.rhs = copy_expr_(c, in->binary.rhs);
		break;
	case EXPR_FN:
		copy_fn_expr(c, out->fn = allocr_malloc(a, sizeof *out->fn), in->fn, 0);
		break;
	case EXPR_CAST: {
		CastExpr *cin = &in->cast;
		CastExpr *cout = &out->cast;
		copy_type(c, &cout->type, &cin->type);
		cout->expr = copy_expr_(c, cin->expr);
	} break;
	case EXPR_CALL: {
		CallExpr *cin = &in->call;
		CallExpr *cout = &out->call;
		copy_expr(c, cout->fn = allocr_malloc(a, sizeof *cout->fn), cin->fn);
		size_t nargs = arr_len(cin->args);
		cout->arg_exprs = NULL;
		arr_set_lena(cout->args, nargs, a);
		for (size_t i = 0; i < nargs; ++i) {
			Argument *arg_in = &cin->args[i];
			Argument *arg_out = &cout->args[i];
			*arg_out = *arg_in;
			copy_expr(c, &arg_out->val, &arg_in->val);
		}
	} break;
	case EXPR_TUPLE: {
		size_t nexprs = arr_len(in->tuple);
		out->tuple = NULL;
		arr_set_lena(out->tuple, nexprs, a);
		for (size_t i = 0; i < nexprs; ++i)
			copy_expr(c, out->tuple + i, in->tuple + i);
	} break;
	case EXPR_C:
		out->c.code = copy_expr_(c, in->c.code);
		break;
	case EXPR_BUILTIN:
		out->builtin.which.expr = copy_expr_(c, in->builtin.which.expr);
		break;
	case EXPR_SLICE: {
		SliceExpr *sin = &in->slice;
		SliceExpr *sout = &out->slice;
		copy_expr(c, sout->of = allocr_malloc(a, sizeof *sout->of), sin->of);
		if (sin->from)
			sout->from = copy_expr_(c, sin->from);
		if (sin->to)
			sout->to = copy_expr_(c, sin->to);
	} break;
	case EXPR_TYPE:
		copy_type(c, out->typeval = copier_malloc(c, sizeof *out->typeval), in->typeval);
		break;
	case EXPR_VAL:
		copy_val(a, &out->val, in->val, &in->type);
		break;
	case EXPR_NMS:
		out->nms = allocr_malloc(a, sizeof *out->nms);
		*out->nms = *in->nms;
		copy_block(c, &out->nms->body, &in->nms->body, 0);
		break;
	}
}

static Expression *copy_expr_(Copier *c, Expression *in) {
	Expression *out = allocr_malloc(c->allocr, sizeof *out);
	copy_expr(c, out, in);
	return out;
}

static void copy_decl(Copier *c, Declaration *out, Declaration *in) {
	*out = *in;
	assert(!(in->flags & DECL_FOUND_TYPE));
	
	if (in->flags & DECL_HAS_EXPR)
		copy_expr(c, &out->expr, &in->expr);
	if (in->flags & DECL_FOUND_VAL) {
		copy_val(c->allocr, &out->val, in->val, &in->type);
	}
	if (in->flags & DECL_ANNOTATES_TYPE)
		copy_type(c, &out->type, &in->type);
	out->idents = NULL;
	size_t nidents = arr_len(in->idents);
	arr_set_lena(out->idents, nidents, c->allocr);
	for (size_t i = 0; i < nidents; ++i) {
		out->idents[i] = in->idents[i];
		assert(c->block);
		copier_ident_translate(c, &out->idents[i]);
		out->idents[i]->decl = out;
	}
	
}

static void copy_stmt(Copier *c, Statement *out, Statement *in) {
	*out = *in;
	assert(!(in->flags & STMT_TYPED));
	switch (in->kind) {
	case STMT_RET:
		out->ret = copier_malloc(c, sizeof *out->ret);
		*out->ret = *in->ret;
		if (in->ret->flags & RET_HAS_EXPR)
			copy_expr(c, &out->ret->expr, &in->ret->expr);
		break;
	case STMT_INCLUDE:
		out->inc = copier_malloc(c, sizeof *out->inc);
		*out->inc = *in->inc;
		break;
	case STMT_EXPR:
		out->expr = copy_expr_(c, in->expr);
		break;
	case STMT_DECL:
		copy_decl(c, out->decl = allocr_malloc(c->allocr, sizeof *out->decl), in->decl);
		break;
	case STMT_MESSAGE:
		out->message = copier_malloc(c, sizeof *out->message);
		*out->message = *in->message;
		copy_expr(c, &out->message->text, &in->message->text);
		break;
	case STMT_DEFER:
		copy_stmt(c, out->defer = allocr_malloc(c->allocr, sizeof *out->defer), in->defer);
		break;
	case STMT_BREAK:
	case STMT_CONT:
		break;
	case STMT_USE:
		out->use = copier_malloc(c, sizeof *in->use);
		*out->use = *in->use;
		copy_expr(c, &out->use->expr, &in->use->expr);
		break;
	case STMT_IF: {
		If *iin = in->if_;
		If *iout = out->if_ = copier_malloc(c, sizeof *iout);
		while (1) {
			*iout = *iin;
			
			if (iin->cond)
				iout->cond = copy_expr_(c, iin->cond);
			copy_block(c, &iout->body, &iin->body, 0);

			if (iin->next_elif) {
				iout->next_elif = copier_malloc(c, sizeof *iout->next_elif);
				iout = iout->next_elif;
				iin = iin->next_elif;
			} else {
				break;
			}
		}
	} break;
	case STMT_WHILE: {
		While *win = in->while_;
		While *wout = out->while_ = copier_malloc(c, sizeof *wout);
		*wout = *win;
		wout->cond = copy_expr_(c, win->cond);
		copy_block(c, &wout->body, &win->body, 0);
	} break;
	case STMT_FOR: {
		For *fin = in->for_;
		For *fout = out->for_ = copier_malloc(c, sizeof *fout);
		*fout = *fin;
		Block *prev = c->block;
		c->block = &fout->body;
		idents_create(&fout->body.idents, c->allocr, &fout->body);
		copy_decl(c, &fout->header, &fin->header);
		if (fin->flags & FOR_IS_RANGE) {
			fout->range.from = copy_expr_(c, fin->range.from);
			if (fin->range.to) fout->range.to = copy_expr_(c, fin->range.to);
			if (fin->range.step) fout->range.step = copy_expr_(c, fin->range.step);
		} else {
			fout->of = copy_expr_(c, fin->of);
		}
		c->block = prev;
		copy_block(c, &fout->body, &fin->body, COPY_BLOCK_DONT_CREATE_IDENTS);
	} break;
	case STMT_BLOCK:
		copy_block(c, out->block = copier_malloc(c, sizeof *out->block), in->block, 0);
		break;
	case STMT_INLINE_BLOCK:
		assert(0); // only exists after typing
		break;
	}
}

// COPY_BLOCK_DONT_CREATE_IDENTS is for copy_fn_expr, etc.
static void copy_block(Copier *c, Block *out, Block *in, U8 flags) {
	assert(!(in->flags & BLOCK_FINDING_TYPES));
	out->flags = in->flags;
	out->kind = in->kind;
	out->parent = c->block;
	out->where = in->where;
	size_t nstmts = arr_len(in->stmts);
	out->stmts = NULL;
	Block *prev = c->block;
	c->block = out;
	if (!(flags & COPY_BLOCK_DONT_CREATE_IDENTS))
		idents_create(&out->idents, c->allocr, out);
	arr_set_lena(out->stmts, nstmts, c->allocr);
	for (size_t i = 0; i < nstmts; ++i) {
		copy_stmt(c, &out->stmts[i], &in->stmts[i]);
	}
	c->block = prev;
}
