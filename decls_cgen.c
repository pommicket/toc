static bool cgen_decls_stmt(CGenerator *g, Statement *s);
static bool cgen_decls_block(CGenerator *g, Block *b);

static bool cgen_decls_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_CALL:
		if (e->call.fn->kind == EXPR_IDENT) {
			IdentDecl *idecl = ident_decl(e->call.fn->ident);
			Block *prev = g->block;
			/* temporarily set g->block so that cgen_fn_is_direct works */
			g->block = idecl->scope;
			if (idecl->kind == IDECL_DECL &&
				cgen_fn_is_direct(g, idecl->decl)) {
				g->block = prev;
				FnExpr *f = &idecl->decl->expr.fn;
				f->c.name = idecl->decl->idents[0];
				/* directly calling a function; might need to generate a copy of this function */

				/* OPTIM should we really be constructing a tuple & type every time? */
				Value *compile_time_args = NULL;
				Type *tuple_types = NULL;
				size_t i = 0;
				arr_foreach(f->params, Declaration, param) {
					if (param->flags & DECL_IS_CONST) {
						arr_foreach(param->idents, Identifier, ident) {
							Expression *arg = &e->call.arg_exprs[i];
							assert(arg->kind == EXPR_VAL); /* should have been evaluated by types.c */
							*(Value *)arr_adda(&compile_time_args, g->allocr) = arg->val;
							*(Type *)arr_add(&tuple_types) = arg->type;
							i++;
						}
					} else {
						i += arr_len(param->idents);
					}
				}
				if (compile_time_args) {
					Value tuple;
					Type tuple_type;
					tuple.tuple = compile_time_args;
					tuple_type.kind = TYPE_TUPLE;
					tuple_type.flags = TYPE_IS_RESOLVED;
					tuple_type.tuple = tuple_types;
					if (!f->c.instances) {
						f->c.instances = allocr_calloc(g->allocr, 1, sizeof *f->c.instances);
					}
					/* lookup compile time arguments */
					I64 instance_number = (I64)f->c.instances->n + 1;
					bool already_generated_decl = val_hash_table_adda(g->allocr, f->c.instances, tuple, &tuple_type, &instance_number);
					if (!already_generated_decl) {
						/* generate a copy of this function */
						if (!cgen_fn_header(g, f, e->where, instance_number))
							return false;
						cgen_write(g, ";");
						cgen_nl(g);
					}
					arr_clear(&tuple_types);
				}	/* else, there are no compile time arguments; we don't need to generate a separate declaration for this call */
			}
		}
		break;
	case EXPR_FN:
		e->fn.c.name = NULL;
		e->fn.c.id = g->ident_counter++;
		fn_enter(&e->fn, 0);
		if (!cgen_fn_header(g, &e->fn, e->where, 0))
			return false;
		cgen_write(g, ";");
		cgen_nl(g);
		fn_exit(&e->fn);
		break;
	default:
		break;
	}
	cgen_recurse_subexprs(g, e, cgen_decls_expr, cgen_decls_block);
	
	return true;
}

static bool cgen_decls_block(CGenerator *g, Block *b) {
	Block *prev = g->block;
	if (!cgen_block_enter(g, b))
		return false;
	arr_foreach(b->stmts, Statement, s)
		cgen_decls_stmt(g, s);
	cgen_block_exit(g, prev);
	return true;
}

static bool cgen_decls_decl(CGenerator *g, Declaration *d) {
	if (cgen_fn_is_direct(g, d)) {
		if (!fn_has_any_const_params(&d->expr.fn)) {
			d->expr.fn.c.name = d->idents[0];
			fn_enter(&d->expr.fn, 0);
			if (!cgen_fn_header(g, &d->expr.fn, d->where, 0))
				return false;
			cgen_write(g, ";");
			cgen_nl(g);
		}
		if (!cgen_decls_block(g, &d->expr.fn.body))
			return false;
		fn_exit(&d->expr.fn);
	} else if ((d->flags & DECL_HAS_EXPR) && !(d->flags & DECL_IS_CONST)) {
		if (!cgen_decls_expr(g, &d->expr))
			return false;
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
