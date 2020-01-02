/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
enum {
	  SCOPE_CHECK_REDECL = 0x01,
};


static void val_free(Value *v, Type *t);

static bool DEBUG_UNDERSCORE(add_ident_decls)(SOURCE_LOCATION_PARAMS Block *b, Declaration *d, U16 flags) {
	bool ret = true;
	arr_foreach(d->idents, Identifier, ident) {
		IdentDecl *decls = (*ident)->decls;
		if ((flags & SCOPE_CHECK_REDECL) && arr_len(decls)) {
			/* check that it hasn't been declared in this block */
			IdentDecl *prev = arr_last(decls);
			if (prev->scope == b) {
				err_print(d->where, "Re-declaration of identifier in the same block.");
				info_print(prev->decl->where, "Previous declaration was here.");
#ifdef TOC_DEBUG
				info_print(d->where, "First declaration was done by %s:%d, second was done by %s:%d.", prev->src_file, prev->src_line, src_file, src_line);
#endif				
				ret = false;
				continue;
			}
		}
		IdentDecl *idecl = ident_add_decl(*ident, d, b);
#ifdef TOC_DEBUG
		idecl->src_file = src_file;
		idecl->src_line = src_line;
#else
		(void)idecl;
#endif
	}
	return ret;
}

static void remove_ident_decls(Block *b, Declaration *d) {
	U64 i = 0;
	bool is_tuple = d->type.kind == TYPE_TUPLE;
	arr_foreach(d->idents, Identifier, ident) {
		IdentTree *id_info = *ident;
		IdentDecl **decls = &id_info->decls;
		IdentDecl *last_decl = arr_last(*decls);
		if (last_decl && last_decl->scope == b) {
			if ((last_decl->flags & IDECL_HAS_VAL)
				/* don't free const vals (there's only one per decl) */
				&& !(last_decl->decl->flags & DECL_IS_CONST)) {
				val_free(&last_decl->val, is_tuple ? &d->type.tuple[i++] : &d->type);
			}
			arr_remove_last(decls); /* remove that declaration */
		}
	}
}

/* pass NULL for block for global scope */
static bool block_enter(Block *b, Statement *stmts, U16 flags) {
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
static bool fn_enter(FnExpr *f, U16 flags) {
	arr_foreach(f->params, Declaration, decl)
		if (!add_ident_decls(&f->body, decl, flags))
			return false;
	arr_foreach(f->ret_decls, Declaration, decl)
		if (!add_ident_decls(&f->body, decl, flags))
			return false;
	return true;
}

static void fn_exit(FnExpr *f) {
	arr_foreach(f->params, Declaration, decl)
		remove_ident_decls(&f->body, decl);
	arr_foreach(f->ret_decls, Declaration, decl)
		remove_ident_decls(&f->body, decl);
}

static bool each_enter(Expression *e) {
	assert(e->kind == EXPR_EACH);
	EachExpr *ea = e->each;
    if (ea->index && ea->index == ea->value) {
		err_print(e->where, "The identifier for the index of an each loop must be different from the identifier for the value.");
		return false;
	}
	if (ea->index) {
		IdentDecl *id = arr_add(&ea->index->decls);
		id->flags = 0;
		id->kind = IDECL_EXPR;
		id->scope = &ea->body;
		id->expr = e;
	}
	if (ea->value) {
		IdentDecl *id = arr_add(&ea->value->decls);
		id->flags = 0;
		id->kind = IDECL_EXPR;
		id->scope = &ea->body;
		id->expr = e;
	}
	return true;
}

static void each_exit(Expression *e) {
	assert(e->kind == EXPR_EACH);
	EachExpr *ea = e->each;
	if (ea->index) {
		arr_remove_last(&ea->index->decls);
	}
	if (ea->value) {
		arr_remove_last(&ea->value->decls);
	}
}
