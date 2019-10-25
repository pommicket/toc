#define SCOPE_FLAG_CHECK_REDECL 0x0001


static void val_free(Value *v, Type *t);

static bool add_ident_decls(Block *b, Declaration *d, U32 flags) {
	bool ret = true;
	arr_foreach(d->idents, Identifier, ident) {
		IdentDecl **decls = &(*ident)->decls;
		if ((flags & SCOPE_FLAG_CHECK_REDECL) && arr_len(*decls)) {
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
	U64 i = 0;
	arr_foreach(d->idents, Identifier, ident) {
		IdentTree *id_info = *ident;
	    IdentDecl **decls = &id_info->decls;
		IdentDecl *last_decl = arr_last(*decls);
		if (last_decl && last_decl->scope == b) {
			if ((last_decl->flags & IDECL_FLAG_HAS_VAL)
				/* don't free const vals (there's only one per decl) */
				&& !(last_decl->decl->flags & DECL_FLAG_CONST)) {
				val_free(&last_decl->decl->val, d->type.kind == TYPE_TUPLE ? &d->type.tuple[i++] : &d->type);
			}
			arr_remove_last(decls); /* remove that declaration */
		}
	}
}

/* pass NULL for block for global scope */
static bool block_enter(Block *b, Statement *stmts, uint32_t flags) {
	bool ret = true;
	arr_foreach(stmts, Statement, stmt) {
		if (stmt->kind == STMT_DECL) {
			Declaration *decl = &stmt->decl;
			if (!add_ident_decls(b, decl, flags))
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
static void fn_enter(FnExpr *f, U32 flags) {
	arr_foreach(f->params, Declaration, decl)
		add_ident_decls(&f->body, decl, flags);
	arr_foreach(f->ret_decls, Declaration, decl)
		add_ident_decls(&f->body, decl, flags);
}

static void fn_exit(FnExpr *f) {
	arr_foreach(f->params, Declaration, decl)
		remove_ident_decls(&f->body, decl);
	arr_foreach(f->ret_decls, Declaration, decl)
		remove_ident_decls(&f->body, decl);
}
