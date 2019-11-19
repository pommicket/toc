static bool cgen_decls_stmt(CGenerator *g, Statement *s);
static bool cgen_decls_block(CGenerator *g, Block *b);
static bool cgen_decls_decl(CGenerator *g, Declaration *d);

static bool cgen_decls_fn_instances(CGenerator *g, Expression *e) {
	assert(e->kind == EXPR_FN);
	FnExpr *f = &e->fn;
	FnType *type = &e->type.fn;
	assert(type->constness);
	Instance **data = f->instances.data;
	for (U64 i = 0; i < f->instances.cap; i++) {
		if (f->instances.occupied[i]) {
			(*data)->fn.c.name = f->c.name;
			(*data)->fn.c.id = f->c.id;
		
			if (!cgen_fn_header(g, &(*data)->fn, e->where, (*data)->c.id, (*data)->val.tuple[0].u64))
				return false;
			cgen_write(g, ";");
			cgen_nl(g);
		}
		data++;
	}
	return true;
}

static bool cgen_decls_expr(CGenerator *g, Expression *e) {
	cgen_recurse_subexprs(g, e, cgen_decls_expr, cgen_decls_block, cgen_decls_decl);
	switch (e->kind) {
	case EXPR_FN: {
		FnExpr *f = &e->fn;
		f->c.name = NULL;
		if (!f->c.id)
			f->c.id = g->ident_counter++;
		FnType *fn_type = &e->type.fn;
		if (fn_type->constness) {
			if (!cgen_decls_fn_instances(g, e))
				return false;
		} else {
			fn_enter(&e->fn, 0);
			if (!cgen_fn_header(g, &e->fn, e->where, 0, 0))
				return false;
			cgen_write(g, ";");
			cgen_nl(g);
			fn_exit(&e->fn);
		}
	} break;	
	default:
		break;
	}
	
	return true;
}

static bool cgen_decls_block(CGenerator *g, Block *b) {
	Block *prev = g->block;
	if (!cgen_block_enter(g, b))
		return false;
	arr_foreach(b->stmts, Statement, s)
		if (!cgen_decls_stmt(g, s))
			return false;
	cgen_block_exit(g, prev);
	return true;
}

static bool cgen_decls_decl(CGenerator *g, Declaration *d) {
	if (cgen_fn_is_direct(g, d)) {
		d->expr.fn.c.name = d->idents[0];
		if (d->expr.type.fn.constness) {
			if (!cgen_decls_fn_instances(g, &d->expr))
				return false;
		} else {
			fn_enter(&d->expr.fn, 0);
			if (!cgen_fn_header(g, &d->expr.fn, d->where, 0, 0))
				return false;
			cgen_write(g, ";");
			fn_exit(&d->expr.fn);
		}
		cgen_recurse_subexprs(g, (&d->expr), cgen_decls_expr, cgen_decls_block, cgen_decls_decl);
	} else if (d->flags & DECL_HAS_EXPR) {
		if (d->flags & DECL_IS_CONST) {
			for (size_t idx = 0; idx < arr_len(d->idents); idx++) {
				Identifier i = d->idents[idx];
				Type *type = d->type.kind == TYPE_TUPLE ? &d->type.tuple[idx] : &d->type;
				if (type->kind == TYPE_TYPE) {
					Value *val = d->type.kind == TYPE_TUPLE ? &d->val.tuple[idx] : &d->val;
					if (val->type->kind == TYPE_STRUCT) {
						/* generate struct definition */
						cgen_write(g, "struct ");
						if (g->block == NULL)
							cgen_ident(g, i);
						else
							cgen_ident_id(g, d->c.ids[idx]);
						cgen_write(g, "{");
						cgen_nl(g);
						g->indent_lvl++;
						arr_foreach(val->type->struc.fields, Field, f) {
							if (!cgen_type_pre(g, f->type, d->where)) return false;
							cgen_write(g, " ");
							cgen_ident(g, f->name);
							if (!cgen_type_post(g, f->type, d->where)) return false;
							cgen_write(g, ";");
							cgen_nl(g);
						}
						g->indent_lvl--;
						cgen_write(g, "};");
						cgen_nl(g);
					}
				}
			}
		}
		if (!(d->flags & DECL_IS_CONST) || (d->expr.kind == EXPR_FN)) {
			if (!cgen_decls_expr(g, &d->expr))
				return false;
		}
	}
	return true;
}

static bool cgen_decls_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_DECL:
		if (!cgen_decls_decl(g, &s->decl))
			return false;
		break;
	case STMT_EXPR:
		if (!cgen_decls_expr(g, &s->expr))
			return false;
		break;
	case STMT_RET:
		if (s->ret.flags & RET_HAS_EXPR)
			if (!cgen_decls_expr(g, &s->ret.expr))
				return false;
		break;
	}
	return true;
}

static bool cgen_decls_file(CGenerator *g, ParsedFile *f) {
	cgen_write(g, "/* declarations */\n");
	arr_foreach(f->stmts, Statement, s) {
		if (!cgen_decls_stmt(g, s))
			return false;
	}
	return true;
}
