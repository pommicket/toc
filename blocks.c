/* identifies identifiers in this block */
static bool block_enter(Block *b) {
	bool ret = true;
	arr_foreach(&b->stmts, Statement, stmt) {
		if (stmt->kind == STMT_DECL) {
			Declaration *decl = &stmt->decl;
			arr_foreach(&decl->idents, Identifier, ident) {
				Array *decls = &(*ident)->decls;
				if (decls->item_sz) {
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

/* de-identifies identifiers in this block */
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
				if (last_decl->scope == b)
					arr_remove_last(decls); /* remove that declaration */
				
			}
		}
	}
	return ret;
}
