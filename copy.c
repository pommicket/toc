/* these copy functions MUST be used before typing!!!! (except for copy_val) */

static void copy_expr(Allocator *a, Expression *out, Expression *in);
static void copy_decl(Allocator *a, Declaration *out, Declaration *in);
static void copy_block(Allocator *a, Block *out, Block *in);

static void copy_val(Allocator *allocr, Value *out, Value *in, Type *t) {
	switch (t->kind) {
	case TYPE_BUILTIN:
	case TYPE_FN:
	case TYPE_PTR:
	case TYPE_SLICE:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_TYPE:
		*out = *in;
		break;
	case TYPE_ARR: {
		size_t bytes = t->arr.n * compiler_sizeof(t->arr.of);
		out->arr = allocr_malloc(allocr, bytes);
		memcpy(out->arr, in->arr, bytes);
	} break;
	case TYPE_TUPLE: {
		size_t bytes = arr_len(t->tuple) * sizeof(*out->tuple);
		out->tuple = allocr_malloc(allocr, bytes);
		memcpy(out->tuple, in->tuple, bytes);
	} break;
	case TYPE_STRUCT: {
		size_t bytes = compiler_sizeof(t);
		out->struc = allocr_malloc(allocr, bytes);
		memcpy(out->struc, in->struc, bytes);
	} break;
	case TYPE_USER:
		copy_val(allocr, out, in, type_user_underlying(t));
		break;
	}
}

static void copy_type(Allocator *a, Type *out, Type *in) {
	assert(!(in->flags & TYPE_IS_RESOLVED));
	*out = *in;
	switch (in->kind) {
	case TYPE_BUILTIN:
	case TYPE_TYPE:
	case TYPE_VOID:
	case TYPE_UNKNOWN:
	case TYPE_USER:
		break;
	case TYPE_FN: {
		size_t ntypes = arr_len(in->fn.types);
		out->fn.types = NULL;
		arr_set_lena(&out->fn.types, ntypes, a);
		for (size_t i = 0; i < ntypes; i++) {
			copy_type(a, &out->fn.types[i], &in->fn.types[i]);
		}
	} break;
	case TYPE_TUPLE: {
		size_t ntypes = arr_len(in->tuple);
		out->tuple = NULL;
		arr_set_lena(&out->tuple, ntypes, a);
		for (size_t i = 0; i < ntypes; i++) {
			copy_type(a, &out->tuple[i], &in->tuple[i]);
		}
	} break;
	case TYPE_ARR:
		out->arr.n_expr = allocr_malloc(a, sizeof *out->arr.n_expr);
		copy_expr(a, out->arr.n_expr, in->arr.n_expr);
		out->arr.of = allocr_malloc(a, sizeof *out->arr.of);
		copy_type(a, out->arr.of, in->arr.of);
		break;
	case TYPE_PTR:
		out->ptr = allocr_malloc(a, sizeof *out->ptr);
		copy_type(a, out->ptr, in->ptr);
		break;
	case TYPE_SLICE:
		out->ptr = allocr_malloc(a, sizeof *out->slice);
		copy_type(a, out->slice, in->slice);
		break;
	case TYPE_STRUCT: {
		size_t nfields = arr_len(in->struc.fields);
		out->struc.fields = NULL;
		arr_set_lena(&out->struc.fields, nfields, a);
		for (size_t i = 0; i < nfields; i++) {
			Field *fout = &out->struc.fields[i];
			Field *fin = &in->struc.fields[i];
			*fout = *fin;
			copy_type(a, fout->type, fin->type);
		}
	} break;
	}
}

static void copy_fn_expr(Allocator *a, FnExpr *fout, FnExpr *fin, bool copy_body) {
	size_t i;
	size_t nparam_decls = arr_len(fin->params);
	fout->params = NULL;
	arr_set_lena(&fout->params, nparam_decls, a);
	for (i = 0; i < nparam_decls; i++)
		copy_decl(a, fout->params + i, fin->params + i);
	size_t nret_decls = arr_len(fin->ret_decls);
	fout->ret_decls = NULL;
	arr_set_lena(&fout->ret_decls, nret_decls, a);
	for (i = 0; i < nret_decls; i++)
		copy_decl(a, fout->ret_decls + i, fin->ret_decls + i);
	copy_type(a, &fout->ret_type, &fin->ret_type);
	if (copy_body)
		copy_block(a, &fout->body, &fin->body);
}

static void copy_expr(Allocator *a, Expression *out, Expression *in) {
	*out = *in;
	switch (in->kind) {
	case EXPR_LITERAL_FLOAT:
	case EXPR_LITERAL_INT:
	case EXPR_LITERAL_STR:
	case EXPR_LITERAL_CHAR:
	case EXPR_LITERAL_BOOL:
	case EXPR_IDENT:
		break;
	case EXPR_UNARY_OP:
		copy_expr(a, out->unary.of = allocr_malloc(a, sizeof *out->unary.of), in->unary.of);
		break;
	case EXPR_BINARY_OP:
		copy_expr(a, out->binary.lhs = allocr_malloc(a, sizeof *out->binary.lhs), in->binary.lhs);
		copy_expr(a, out->binary.rhs = allocr_malloc(a, sizeof *out->binary.rhs), in->binary.rhs);
		break;
	case EXPR_IF: {
		IfExpr *iin = &in->if_;
		IfExpr *iout = &out->if_;
		if (iin->cond)
			copy_expr(a, iout->cond = allocr_malloc(a, sizeof *iout->cond), iin->cond);
		if (iin->next_elif)
			copy_expr(a, iout->next_elif = allocr_malloc(a, sizeof *iout->next_elif), iin->next_elif);
		copy_block(a, &iout->body, &iin->body);
	} break;
	case EXPR_WHILE: {
		WhileExpr *win = &in->while_;
		WhileExpr *wout = &out->while_;
		if (win->cond)
			copy_expr(a, wout->cond = allocr_malloc(a, sizeof *wout->cond), win->cond);
		copy_block(a, &wout->body, &win->body);
	} break;
	case EXPR_EACH: {
		EachExpr *ein = &in->each;
		EachExpr *eout = &out->each;
		copy_type(a, &eout->type, &ein->type);
		if (ein->flags & EACH_IS_RANGE) {
			copy_expr(a, eout->range.from = allocr_malloc(a, sizeof *eout->range.from), ein->range.from);
			if (ein->range.to)
				copy_expr(a, eout->range.to = allocr_malloc(a, sizeof *eout->range.to), ein->range.to);
			if (ein->range.step)
				copy_expr(a, eout->range.step = allocr_malloc(a, sizeof *eout->range.step), ein->range.step);
		} else {
			copy_expr(a, eout->of = allocr_malloc(a, sizeof *eout->of), ein->of);
		}
		copy_block(a, &eout->body, &ein->body);
	} break;
	case EXPR_FN:
		copy_fn_expr(a, &out->fn, &in->fn, true);
		break;
	case EXPR_CAST: {
		CastExpr *cin = &in->cast;
		CastExpr *cout = &out->cast;
		copy_type(a, &cout->type, &cin->type);
		copy_expr(a, cout->expr = allocr_malloc(a, sizeof *cout->expr), cin->expr);
	} break;
	case EXPR_NEW: {
		NewExpr *nin = &in->new;
		NewExpr *nout = &out->new;
		copy_type(a, &nout->type, &nin->type);
		if (nin->n) copy_expr(a, nout->n = allocr_malloc(a, sizeof *nout->n), nin->n);
	} break;
	case EXPR_CALL: {
		CallExpr *cin = &in->call;
		CallExpr *cout = &out->call;
		copy_expr(a, cout->fn = allocr_malloc(a, sizeof *cout->fn), cin->fn);
		size_t nargs = arr_len(cin->arg_exprs);
		cout->arg_exprs = NULL;
		arr_set_lena(&cout->arg_exprs, nargs, a);
		for (size_t i = 0; i < nargs; i++) {
			copy_expr(a, cout->arg_exprs + i, cin->arg_exprs + i);
		}
	} break;
	case EXPR_BLOCK:
		copy_block(a, &out->block, &in->block);
		break;
	case EXPR_TUPLE: {
		size_t nexprs = arr_len(in->tuple);
		out->tuple = NULL;
		arr_set_lena(&out->tuple, nexprs, a);
		for (size_t i = 0; i < nexprs; i++)
			copy_expr(a, out->tuple + i, in->tuple + i);
	} break;
	case EXPR_C:
		copy_expr(a, out->c.code = allocr_malloc(a, sizeof *out->c.code), in->c.code);
		break;
	case EXPR_DSIZEOF:
		copy_expr(a, out->dsizeof.of = allocr_malloc(a, sizeof *out->dsizeof.of), in->dsizeof.of);
		break;
	case EXPR_DALIGNOF:
		copy_expr(a, out->dalignof.of = allocr_malloc(a, sizeof *out->dalignof.of), in->dalignof.of);
		break;
	case EXPR_SLICE: {
		SliceExpr *sin = &in->slice;
		SliceExpr *sout = &out->slice;
		copy_expr(a, sout->of = allocr_malloc(a, sizeof *sout->of), sin->of);
		if (sin->from)
			copy_expr(a, sout->from = allocr_malloc(a, sizeof *sout->from), sin->from);
		if (sin->to)
			copy_expr(a, sout->to = allocr_malloc(a, sizeof *sout->to), sin->to);
	} break;
	case EXPR_TYPE:
		copy_type(a, &out->typeval, &in->typeval);
		break;
	case EXPR_VAL:
		copy_val(a, &out->val, &in->val, &in->type);
		break;
	}
}

static void copy_decl(Allocator *a, Declaration *out, Declaration *in) {
	*out = *in;
	if (in->flags & DECL_HAS_EXPR)
		copy_expr(a, &out->expr, &in->expr);
	if (in->flags & DECL_FOUND_VAL) {
		copy_val(a, &out->val, &in->val, &in->type);
	}
	if (in->flags & DECL_ANNOTATES_TYPE)
		copy_type(a, &out->type, &in->type);
	
}

static void copy_stmt(Allocator *a, Statement *out, Statement *in) {
	*out = *in;
	assert(!(in->decl.flags & DECL_FOUND_TYPE));
	switch (in->kind) {
	case STMT_RET:
		if (in->flags & RET_HAS_EXPR)
			copy_expr(a, &out->ret.expr, &in->ret.expr);
		break;
	case STMT_EXPR:
		copy_expr(a, &out->expr, &in->expr);
		break;
	case STMT_DECL:
		copy_decl(a, &out->decl, &in->decl);
		break;
	}
}

static void copy_block(Allocator *a, Block *out, Block *in) {
	*out = *in;
	size_t nstmts = arr_len(in->stmts);
	out->stmts = NULL;
	arr_set_lena(&out->stmts, nstmts, a);
	for (size_t i = 0; i < nstmts; i++) {
		copy_stmt(a, &out->stmts[i], &in->stmts[i]);
	}
}
