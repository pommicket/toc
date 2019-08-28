/* pass NULL for file */
static bool block_enter(Block *b) {
	bool ret = true;
	arr_foreach(&b->stmts, Statement, stmt) {
		if (stmt->kind == STMT_DECL) {
			Declaration *decl = &stmt->decl;
			arr_foreach(&decl->idents, Identifier, ident) {
				Array *decls = &(*ident)->decls;
				if (decls->len) {
					/* check that it hasn't been declared in this block */
					IdentDecl *prev = decls->last;
					if (prev->scope == b) {
						err_print(decl->where, "Re-declaration of identifier in the same block.");
						info_print(prev->decl->where, "Previous declaration was here.");
						ret = false;
						continue;
					}
				} else {
					/* array not initialized yet */
					arr_create(&(*ident)->decls, sizeof(IdentDecl));
				}
				
				IdentDecl *ident_decl = arr_add(decls);
				ident_decl->decl = decl;
				ident_decl->scope = b;
			}
		}
	}
	return ret;
}

static bool block_exit(Block *b) {
	/* OPTIM: figure out some way of not re-iterating over everything */
	bool ret = true;
	arr_foreach(&b->stmts, Statement, stmt) {
		if (stmt->kind == STMT_DECL) {
			Declaration *decl = &stmt->decl;
			arr_foreach(&decl->idents, Identifier, ident) {
				Array *decls = &(*ident)->decls;
				assert(decls->item_sz);
				IdentDecl *last_decl = decls->last;
				if (last_decl->scope == b) {
					arr_remove_last(decls); /* remove that declaration */
				}
				
			}
		}
	}
	return ret;
}


/* returns the number of characters written, not including the null character */
static size_t type_to_string(Type *a, char *buffer, size_t bufsize) {
	switch (a->kind) {
	case TYPE_VOID:
		return str_copy(buffer, bufsize, "void");
	case TYPE_BUILTIN: {
		const char *s = keywords[builtin_type_to_kw(a->builtin)];
		return str_copy(buffer, bufsize, s);
	}
	case TYPE_FN: {
		/* number of chars written */
		size_t written = str_copy(buffer, bufsize, "fn (");
		Type *ret_type = a->fn.types.data;
		Type *param_types = ret_type + 1;
		size_t nparams = a->fn.types.len - 1;
		for (size_t i = 0; i < nparams; i++) {
			if (i > 0)
				written += str_copy(buffer + written, bufsize - written, ", ");
			written += type_to_string(&param_types[i], buffer + written, bufsize - written);
		}
		written += str_copy(buffer + written, bufsize - written, ")");
		if (ret_type->kind != TYPE_VOID) {
			written += str_copy(buffer + written, bufsize - written, " ");
			written += type_to_string(ret_type, buffer + written, bufsize - written);
		}
		return written;
	} break;
	}

	assert(0);
	return 0;
}


static bool type_eq(Type *a, Type *b) {
	if (a->kind != b->kind) return false;
	switch (a->kind) {
	case TYPE_VOID: return true;
	case TYPE_BUILTIN:
		return a->builtin == b->builtin;
	case TYPE_FN: {
		
		if (a->fn.types.len != b->fn.types.len) return false;
		Type *a_types = a->fn.types.data, *b_types = b->fn.types.data;
		for (size_t i = 0; i < a->fn.types.len; i++)
			if (!type_eq(&a_types[i], &b_types[i]))
				return false;
		return true;
	}
	}
	assert(0);
	return false;
}

/* expected must equal got, or an error will be produced */
static bool type_must_eq(Location where, Type *expected, Type *got) {
	if (!type_eq(expected, got)) {
		char str_ex[128];
		char str_got[128];
		type_to_string(expected, str_ex, sizeof str_ex);
		type_to_string(got, str_got, sizeof str_got);
		err_print(where, "Type mismatch: expected %s, but got %s.", str_ex, str_got);
		return false;
	}
	return true;
}

static bool types_stmt(Statement *s);
static bool types_decl(Declaration *d);


static bool types_expr(Expression *e) {
	Type *t = &e->type;
	switch (e->kind) {
	case EXPR_FN: {
		FnExpr *f = &e->fn;
		t->kind = TYPE_FN;
		arr_create(&t->fn.types, sizeof(Type));
		Type *ret_type = arr_add(&t->fn.types);
		*ret_type = f->ret_type;
		arr_foreach(&f->params, Param, param) {
			Type *param_type = arr_add(&t->fn.types);
			*param_type = param->type;
		}
		block_enter(&f->body);
		arr_foreach(&f->body.stmts, Statement, s) {
			if (!types_stmt(s))
				return false;
		}
		block_exit(&f->body);
	} break;
	case EXPR_INT_LITERAL:
	    t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_I64;
		break;
	case EXPR_FLOAT_LITERAL:
		t->kind = TYPE_BUILTIN;
		t->builtin = BUILTIN_FLOAT;
		break;
	case EXPR_IDENT: {
		IdentDecl *decl = ident_decl(e->ident);
		if (!decl) {
			char *s = ident_to_str(e->ident);
			err_print(e->where, "Undeclared identifier: %s", s);
			free(s);
		}
		*t = decl->decl->type;
	} break;
		/* TODO */
	}
	return true;
}

static bool types_decl(Declaration *d) {
	if (d->flags & DECL_FLAG_FOUND_TYPE) return true;
	if (!types_expr(&d->expr)) return false;
	if (d->flags & DECL_FLAG_INFER_TYPE) {
		d->type = d->expr.type;
	} else {
		if (!type_must_eq(d->expr.where, &d->type, &d->expr.type)) {
			return false;
		}
	}
	d->flags |= DECL_FLAG_FOUND_TYPE;
		
	return types_expr(&d->expr);
}

static bool types_stmt(Statement *s) {
	switch (s->kind) {
	case STMT_EXPR:
		if (!types_expr(&s->expr))
			return false;
		break;
	case STMT_DECL:
		if (!types_decl(&s->decl))
			return false;
		break;
	}
	return true;
}

static bool types_file(ParsedFile *f) {
	arr_foreach(&f->stmts, Statement, s) {
		if (!types_stmt(s)) {
			return false;
		}
	}
	return true;
}
