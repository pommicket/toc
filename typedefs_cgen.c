static bool typedefs_stmt(CGenerator *g, Statement *s);

static bool typedefs_block(CGenerator *g, Block *b) {
	Block *prev = g->block;
	if (!cgen_block_enter(g, b))
		return false;
	arr_foreach(b->stmts, Statement, s)
		typedefs_stmt(g, s);
	cgen_block_exit(g, prev);
	return true;
}

static bool typedefs_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_UNARY_OP:
		if (!typedefs_expr(g, e->unary.of))
			return false;
		break;
	case EXPR_BINARY_OP:
		if (!typedefs_expr(g, e->binary.lhs))
			return false;
		
		if (e->binary.op != BINARY_DOT)
			if (!typedefs_expr(g, e->binary.rhs))
				return false;
		break;
	case EXPR_CAST:
		if (!typedefs_expr(g, e->cast.expr))
			return false;
		break;
	case EXPR_CALL:
		if (!typedefs_expr(g, e->call.fn))
			return false;
		arr_foreach(e->call.arg_exprs, Expression, a)
			if (!typedefs_expr(g, a))
				return false;
		break;
	case EXPR_BLOCK:
		if (!typedefs_block(g, &e->block))
			return false;
		break;
	case EXPR_IF:
		if (e->if_.cond)
			if (!typedefs_expr(g, e->if_.cond))
				return false;
		if (!typedefs_block(g, &e->if_.body))
			return false;
		if (e->if_.next_elif)
			if (!typedefs_expr(g, e->if_.next_elif))
				return false;
		break;
	case EXPR_WHILE:
		if (e->while_.cond)
			if (!typedefs_expr(g, e->while_.cond))
				return false;
		if (!typedefs_block(g, &e->while_.body))
			return false;
		break;
	case EXPR_EACH: {
		EachExpr *ea = &e->each;
		if (ea->flags & EACH_IS_RANGE) {
			if (!typedefs_expr(g, ea->range.from))
				return false;
			if (ea->range.to && !typedefs_expr(g, ea->range.to))
				return false;
			/* step is a value, not an expression */
		} else {
			if (!typedefs_expr(g, ea->of))
				return false;
		}
	} break;
	case EXPR_TUPLE:
		arr_foreach(e->tuple, Expression, x)
			if (!typedefs_expr(g, x))
				return false;
		break;
	case EXPR_SLICE:
		if (!typedefs_expr(g, e->slice.of)) return false;
		if (e->slice.from && !typedefs_expr(g, e->slice.from)) return false;
		if (e->slice.to && !typedefs_expr(g, e->slice.to)) return false;
		break;
	case EXPR_FN:
		fn_enter(&e->fn, 0);
		if (!typedefs_block(g, &e->fn.body))
			return false;
		fn_exit(&e->fn);
		break;
	case EXPR_NEW:
		if (e->new.n && !typedefs_expr(g, e->new.n))
			return false;
		break;
	case EXPR_TYPE:
	case EXPR_C:
	case EXPR_DSIZEOF:
	case EXPR_DALIGNOF:
	case EXPR_IDENT:
	case EXPR_LITERAL_BOOL:
	case EXPR_LITERAL_INT:
	case EXPR_LITERAL_STR:
	case EXPR_LITERAL_CHAR:
	case EXPR_LITERAL_FLOAT:
		break;
	}
	return true;

}

static bool typedefs_decl(CGenerator *g, Declaration *d) {
	d->c.ids = NULL;
	for (int idx = 0; idx < (int)arr_len(d->idents); idx++) {
		Identifier i = d->idents[idx];
		Type *type = decl_type_at_index(d, idx);
		Value *val = decl_val_at_index(d, idx);
		if (type->kind == TYPE_TYPE) {
			if (d->c.ids == NULL)
				d->c.ids = calloc(arr_len(d->idents), sizeof *d->c.ids);
			/* generate typedef */
			IdentID id = 0;
			if (g->block != NULL) id = d->c.ids[idx] = g->ident_counter++;
			if (val->type->kind == TYPE_STRUCT) continue; /* we don't need to typedef this; we can just use its tag */
			cgen_write(g, "typedef ");
			if (!cgen_type_pre(g, val->type, d->where)) return false;
			cgen_write(g, " ");
			if (g->block == NULL) {
				/* we can refer to this by its name */
				cgen_ident(g, i);
			} else {
				/* we need to use an ID ): */
				cgen_ident_id(g, id);
			}
			if (val->type->kind != TYPE_STRUCT) {
				if (!cgen_type_post(g, val->type, d->where)) return false;
			}
			cgen_write(g, ";");
			cgen_nl(g);
		}
	}
	if (d->flags & DECL_FLAG_HAS_EXPR)
		if (!typedefs_expr(g, &d->expr))
			return false;
	return true;
}

static bool typedefs_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_DECL:
		if (!typedefs_decl(g, &s->decl))
			return false;
		break;
	case STMT_EXPR:
		if (!typedefs_expr(g, &s->expr))
			return false;
		break;
	case STMT_RET:
		if (s->ret.flags & RET_HAS_EXPR)
			if (!typedefs_expr(g, &s->ret.expr))
				return false;
		break;
	}
	return true;
}

static bool typedefs_file(CGenerator *g, ParsedFile *f) {
	arr_foreach(f->stmts, Statement, s) {
		if (!typedefs_stmt(g, s))
			return false;
	}
	return true;
}
