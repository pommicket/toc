static bool add_ident_decls(Block *b, Declaration *d) {
	bool ret = true;
	arr_foreach(d->idents, Identifier, ident) {
		IdentDecl **decls = &(*ident)->decls;
		if (arr_len(*decls)) {
			/* check that it hasn't been declared in this block */
			IdentDecl *prev = arr_last(*decls);
			if (prev->scope == b) {
				err_print(d->where, "Re-declaration of identifier in the same block.");
				info_print(prev->decl->where, "Previous declaration was here.");
				ret = false;
				continue;
			}
		}
		ident_add_decl(*ident, d, b);
	}
	return ret;
}

static void remove_ident_decls(Block *b, Declaration *d) {
	arr_foreach(d->idents, Identifier, ident) {
		IdentTree *id_info = *ident;
	    IdentDecl **decls = &id_info->decls;
		IdentDecl *last_decl = arr_last(*decls);
		if (last_decl && last_decl->scope == b) {
			if ((last_decl->flags & IDECL_FLAG_HAS_VAL)
				&& last_decl->decl->type.kind == TYPE_ARR) {
				/* free array on stack */
				free(last_decl->val.arr);
			}
			arr_remove_last(decls); /* remove that declaration */
		}
	}
}

/* pass NULL for block for global scope */
static bool block_enter(Block *b, Statement *stmts) {
	bool ret = true;
	arr_foreach(stmts, Statement, stmt) {
		if (stmt->kind == STMT_DECL) {
			Declaration *decl = &stmt->decl;
			if (!add_ident_decls(b, decl))
				ret = false;
		}
	}
	return ret;
}

static void block_exit(Block *b, Statement *stmts) {
	arr_foreach(stmts, Statement, stmt) {
		if (stmt->kind == STMT_DECL) {
			Declaration *decl = &stmt->decl;
			remove_ident_decls(b, decl);
		}
	}
}

/* does NOT enter function's block body */
static void fn_enter(FnExpr *f) {
	arr_foreach(f->params, Declaration, decl)
		add_ident_decls(&f->body, decl);
	arr_foreach(f->ret_decls, Declaration, decl)
		add_ident_decls(&f->body, decl);
}

static void fn_exit(FnExpr *f) {
	arr_foreach(f->params, Declaration, decl)
		remove_ident_decls(&f->body, decl);
	arr_foreach(f->ret_decls, Declaration, decl)
		remove_ident_decls(&f->body, decl);
}