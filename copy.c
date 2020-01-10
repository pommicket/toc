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

static void copy_expr(Copier *c, Expression *out, Expression *in);
static void copy_decl(Copier *c, Declaration *out, Declaration *in);
static void copy_block(Copier *c, Block *out, Block *in);
static void copy_type(Copier *c, Type *out, Type *in);

static Copier copier_create(Allocator *a, Block *b) {
	Copier c;
	c.allocr = a;
	c.block = b;
	return c;
}

static void copy_val(Allocator *a, Value *out, Value *in, Type *t) {
	assert(t->flags & TYPE_IS_RESOLVED);
	switch (t->kind) {
	case TYPE_BUILTIN:
	case TYPE_FN:
	case TYPE_PTR:
	case TYPE_SLICE:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
		*out = *in;
		break;
	case TYPE_ARR: {
		size_t bytes = t->arr.n * compiler_sizeof(t->arr.of);
		out->arr = allocr_malloc(a, bytes);
		memcpy(out->arr, in->arr, bytes);
	} break;
	case TYPE_TUPLE: {
		size_t bytes = arr_len(t->tuple) * sizeof(*out->tuple);
		out->tuple = allocr_malloc(a, bytes);
		memcpy(out->tuple, in->tuple, bytes);
	} break;
	case TYPE_STRUCT: {
		size_t bytes = compiler_sizeof(t);
		out->struc = allocr_malloc(a, bytes);
		memcpy(out->struc, in->struc, bytes);
	} break;
	case TYPE_EXPR:
		assert(0);
	}
}

static void copy_val_full(Copier *c, Value *out, Value *in, Type *t) {
	if (type_is_builtin(t, BUILTIN_TYPE)) {
		Type *new_type = allocr_malloc(c->allocr, sizeof *new_type);
		copy_type(c, new_type, in->type);
		out->type = new_type;
	} else {
		copy_val(c->allocr, out, in, t);
	}
}

/* only works on unresolved and resolved types */
static void copy_type(Copier *c, Type *out, Type *in) {
	*out = *in;
	switch (in->kind) {
	case TYPE_BUILTIN:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
		break;
	case TYPE_EXPR:
		copy_expr(c, out->expr = allocr_malloc(c->allocr, sizeof *out->expr), in->expr);
		break;
	case TYPE_FN: {
		size_t ntypes = arr_len(in->fn.types);
		out->fn.types = NULL;
		arr_set_lena(&out->fn.types, ntypes, c->allocr);
		for (size_t i = 0; i < ntypes; ++i) {
			copy_type(c, &out->fn.types[i], &in->fn.types[i]);
		}
	} break;
	case TYPE_TUPLE: {
		size_t ntypes = arr_len(in->tuple);
		out->tuple = NULL;
		arr_set_lena(&out->tuple, ntypes, c->allocr);
		for (size_t i = 0; i < ntypes; ++i) {
			copy_type(c, &out->tuple[i], &in->tuple[i]);
		}
	} break;
	case TYPE_ARR:
		if (in->flags & TYPE_IS_RESOLVED) {
			out->arr.n = in->arr.n;
		} else {
			out->arr.n_expr = allocr_malloc(c->allocr, sizeof *out->arr.n_expr);
			copy_expr(c, out->arr.n_expr, in->arr.n_expr);
		}
		out->arr.of = allocr_malloc(c->allocr, sizeof *out->arr.of);
		copy_type(c, out->arr.of, in->arr.of);
		break;
	case TYPE_PTR:
		out->ptr = allocr_malloc(c->allocr, sizeof *out->ptr);
		copy_type(c, out->ptr, in->ptr);
		break;
	case TYPE_SLICE:
		out->ptr = allocr_malloc(c->allocr, sizeof *out->slice);
		copy_type(c, out->slice, in->slice);
		break;
	case TYPE_STRUCT: {
		out->struc = allocr_malloc(c->allocr, sizeof *out->struc);
		*out->struc = *in->struc;
		size_t nfields = arr_len(in->struc->fields);
		out->struc->fields = NULL;

		arr_set_lena(&out->struc->fields, nfields, c->allocr);
		for (size_t i = 0; i < nfields; ++i) {
			Field *fout = &out->struc->fields[i];
			Field *fin = &in->struc->fields[i];
			*fout = *fin;
			copy_type(c, fout->type = allocr_malloc(c->allocr, sizeof *fout->type), fin->type);
		}
	} break;
	}
}

static void copy_fn_expr(Copier *c, FnExpr *fout, FnExpr *fin, bool copy_body) {
	*fout = *fin;
	size_t i;
	size_t nparam_decls = arr_len(fin->params);
	fout->params = NULL;
	arr_set_lena(&fout->params, nparam_decls, c->allocr);
	for (i = 0; i < nparam_decls; ++i)
		copy_decl(c, fout->params + i, fin->params + i);
	size_t nret_decls = arr_len(fin->ret_decls);
	if (fin->ret_decls) {
		fout->ret_decls = NULL;
		arr_set_lena(&fout->ret_decls, nret_decls, c->allocr);
		for (i = 0; i < nret_decls; ++i)
			copy_decl(c, fout->ret_decls + i, fin->ret_decls + i);
	}
	copy_type(c, &fout->ret_type, &fin->ret_type);
	if (copy_body)
		copy_block(c, &fout->body, &fin->body);
}

static void copy_expr(Copier *c, Expression *out, Expression *in) {
	Allocator *a = c->allocr;
	*out = *in;
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
		copy_expr(c, out->unary.of = allocr_malloc(a, sizeof *out->unary.of), in->unary.of);
		break;
	case EXPR_BINARY_OP:
		copy_expr(c, out->binary.lhs = allocr_malloc(a, sizeof *out->binary.lhs), in->binary.lhs);
		copy_expr(c, out->binary.rhs = allocr_malloc(a, sizeof *out->binary.rhs), in->binary.rhs);
		break;
	case EXPR_IF: {
		IfExpr *iin = &in->if_;
		IfExpr *iout = &out->if_;
		if (iin->cond)
			copy_expr(c, iout->cond = allocr_malloc(a, sizeof *iout->cond), iin->cond);
		if (iin->next_elif)
			copy_expr(c, iout->next_elif = allocr_malloc(a, sizeof *iout->next_elif), iin->next_elif);
		copy_block(c, &iout->body, &iin->body);
	} break;
	case EXPR_WHILE: {
		WhileExpr *win = &in->while_;
		WhileExpr *wout = &out->while_;
		if (win->cond)
			copy_expr(c, wout->cond = allocr_malloc(a, sizeof *wout->cond), win->cond);
		copy_block(c, &wout->body, &win->body);
	} break;
	case EXPR_EACH: {
		EachExpr *ein = in->each;
		EachExpr *eout = allocr_malloc(a, sizeof *eout);
		out->each = eout;
		*eout = *ein;
		if (ein->flags & EACH_ANNOTATED_TYPE)
			copy_type(c, &eout->type, &ein->type);
		if (ein->flags & EACH_IS_RANGE) {
			copy_expr(c, eout->range.from = allocr_malloc(a, sizeof *eout->range.from), ein->range.from);
			if (ein->range.to)
				copy_expr(c, eout->range.to = allocr_malloc(a, sizeof *eout->range.to), ein->range.to);
			if (ein->range.step)
				copy_expr(c, eout->range.step = allocr_malloc(a, sizeof *eout->range.step), ein->range.step);
		} else {
			copy_expr(c, eout->of = allocr_malloc(a, sizeof *eout->of), ein->of);
		}
		copy_block(c, &eout->body, &ein->body);
	} break;
	case EXPR_FN:
		copy_fn_expr(c, out->fn = allocr_malloc(a, sizeof *out->fn), in->fn, true);
		break;
	case EXPR_CAST: {
		CastExpr *cin = &in->cast;
		CastExpr *cout = &out->cast;
		copy_type(c, &cout->type, &cin->type);
		copy_expr(c, cout->expr = allocr_malloc(a, sizeof *cout->expr), cin->expr);
	} break;
	case EXPR_NEW: {
		NewExpr *nin = &in->new;
		NewExpr *nout = &out->new;
		copy_type(c, &nout->type, &nin->type);
		if (nin->n) copy_expr(c, nout->n = allocr_malloc(a, sizeof *nout->n), nin->n);
	} break;
	case EXPR_CALL: {
		CallExpr *cin = &in->call;
		CallExpr *cout = &out->call;
		copy_expr(c, cout->fn = allocr_malloc(a, sizeof *cout->fn), cin->fn);
		size_t nargs = arr_len(cin->args);
		cout->arg_exprs = NULL;
		arr_set_lena(&cout->args, nargs, a);
		for (size_t i = 0; i < nargs; ++i) {
			Argument *arg_in = &cin->args[i];
			Argument *arg_out = &cout->args[i];
			*arg_out = *arg_in;
			copy_expr(c, &arg_out->val, &arg_in->val);
		}
	} break;
	case EXPR_BLOCK:
		copy_block(c, &out->block, &in->block);
		break;
	case EXPR_TUPLE: {
		size_t nexprs = arr_len(in->tuple);
		out->tuple = NULL;
		arr_set_lena(&out->tuple, nexprs, a);
		for (size_t i = 0; i < nexprs; ++i)
			copy_expr(c, out->tuple + i, in->tuple + i);
	} break;
	case EXPR_C:
		copy_expr(c, out->c.code = allocr_malloc(a, sizeof *out->c.code), in->c.code);
		break;
	case EXPR_DSIZEOF:
		copy_expr(c, out->dsizeof.of = allocr_malloc(a, sizeof *out->dsizeof.of), in->dsizeof.of);
		break;
	case EXPR_DALIGNOF:
		copy_expr(c, out->dalignof.of = allocr_malloc(a, sizeof *out->dalignof.of), in->dalignof.of);
		break;
	case EXPR_SLICE: {
		SliceExpr *sin = &in->slice;
		SliceExpr *sout = &out->slice;
		copy_expr(c, sout->of = allocr_malloc(a, sizeof *sout->of), sin->of);
		if (sin->from)
			copy_expr(c, sout->from = allocr_malloc(a, sizeof *sout->from), sin->from);
		if (sin->to)
			copy_expr(c, sout->to = allocr_malloc(a, sizeof *sout->to), sin->to);
	} break;
	case EXPR_PKG:
		copy_expr(c, out->pkg.name_expr = allocr_malloc(a, sizeof *out->pkg.name_expr), in->pkg.name_expr);
		break;
	case EXPR_TYPE:
		copy_type(c, &out->typeval, &in->typeval);
		break;
	case EXPR_VAL:
		copy_val(a, &out->val, &in->val, &in->type);
		break;
	}
}

static void copy_decl(Copier *c, Declaration *out, Declaration *in) {
	*out = *in;
	assert(!(in->flags & DECL_FOUND_TYPE));
		
	if (in->flags & DECL_HAS_EXPR)
		copy_expr(c, &out->expr, &in->expr);
	if (in->flags & DECL_FOUND_VAL) {
		copy_val(c->allocr, &out->val, &in->val, &in->type);
	}
	if (in->flags & DECL_ANNOTATES_TYPE)
		copy_type(c, &out->type, &in->type);
	
}

static void copy_stmt(Copier *c, Statement *out, Statement *in) {
	*out = *in;
	switch (in->kind) {
	case STMT_RET:
		if (in->ret.flags & RET_HAS_EXPR)
			copy_expr(c, &out->ret.expr, &in->ret.expr);
		break;
	case STMT_EXPR:
		copy_expr(c, &out->expr, &in->expr);
		break;
	case STMT_DECL:
		copy_decl(c, &out->decl, &in->decl);
		break;
	}
}

static void copy_block(Copier *c, Block *out, Block *in) {
	*out = *in;
	size_t nstmts = arr_len(in->stmts);
	out->stmts = NULL;
	Block *prev = c->block;
	c->block = out;
	if (in->ret_expr)
		copy_expr(c, out->ret_expr = allocr_malloc(c->allocr, sizeof *out->ret_expr), in->ret_expr);
	
	arr_set_lena(&out->stmts, nstmts, c->allocr);
	for (size_t i = 0; i < nstmts; ++i) {
		copy_stmt(c, &out->stmts[i], &in->stmts[i]);
	}
	c->block = prev;
}
