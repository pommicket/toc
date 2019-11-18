/* these copy functions MUST be used before typing!!!! (except for copy_val) */

static void copy_expr(Allocator *a, Expression *out, Expression *in);
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
	case TYPE_STRUCT:
		out->struc.fields = NULL;
		size_t nfields = arr_len(in->struc.fields);
		arr_set_lena(&out->struc.fields, nfields, a);
		for (size_t i = 0; i < nfields; i++) {
			Field *fout = &out->struc.fields[i];
			Field *fin = &in->struc.fields[i];
			*fout = *fin;
			copy_type(a, fout->type, fin->type);
		}
		break;
	}
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
	}
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
		copy_expr(a, &out->decl.expr, &in->decl.expr);
		if (in->decl.flags & DECL_FOUND_VAL) {
			copy_val(a, &out->decl.val, &in->decl.val, &in->decl.type);
		}
		if (in->decl.flags & DECL_ANNOTATES_TYPE)
			copy_type(a, &out->decl.type, &in->decl.type);
		break;
	}
}

static void copy_block(Allocator *a, Block *out, Block *in) {
	*out = *in;
	size_t nstmts = arr_len(in->stmts);
	arr_set_lena(&out->stmts, nstmts, a);
	for (size_t i = 0; i < nstmts; i++) {
		copy_stmt(a, &out->stmts[i], &in->stmts[i]);
	}
}
