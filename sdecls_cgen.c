/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static void cgen_sdecls_stmt(CGenerator *g, Statement *s);
static void cgen_sdecls_decl(CGenerator *g, Declaration *d);
static void cgen_sdecls_expr(CGenerator *g, Expression *e);

/* i is the name for this type, NULL if not available */
static void cgen_sdecls_type(CGenerator *g, Type *type) {
	if (!(type->flags & TYPE_IS_RESOLVED)) /* non-instance constant fn parameter type */
		return;
	if (type->kind == TYPE_STRUCT) {
		StructDef *sdef = type->struc;
		/* we'll actually define the struct later; here we can just declare it */
	
		if (sdef->flags & STRUCT_DEF_CGEN_DECLARED) {
			/* we've already done this */
		} else {
			cgen_write(g, "struct ");
			if (!sdef->name) {
				sdef->c.id = ++g->ident_counter;
			} 
			cgen_struct_name(g, sdef);
			cgen_write(g, ";");
			cgen_nl(g);
			sdef->flags |= STRUCT_DEF_CGEN_DECLARED;
		}
	}
	cgen_recurse_subtypes(cgen_sdecls_type, g, type);
}

static void cgen_sdecls_block(CGenerator *g, Block *b) {
	Block *prev_block = g->block;
	g->block = b;
	
	arr_foreach(b->stmts, Statement, s)
		cgen_sdecls_stmt(g, s);
	if (b->ret_expr)
		cgen_sdecls_expr(g, b->ret_expr);
	g->block = prev_block;
}

static char *cgen_nms_prefix_part(CGenerator *g, Namespace *n) {
	char *s;
	while (n->points_to) {
		n = n->points_to;
	}
	if (n->associated_ident) {
		size_t ident_len = n->associated_ident->len;
		s = malloc(ident_len + 3);
		memcpy(s, n->associated_ident->str, ident_len);
		s[ident_len] = '_';
		s[ident_len+1] = '_';
		s[ident_len+2] = '\0';
	} else {
		s = calloc(CGEN_IDENT_ID_STR_SIZE + 3, 1);
		cgen_ident_id_to_str(s, ++g->ident_counter);
		size_t len = strlen(s);
		s[len] = '_';
		s[len+1] = '_';
		s[len+2] = '\0';
	}
	return s;
}

static void cgen_sdecls_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_CAST:
		cgen_sdecls_type(g, &e->cast.type);
		break;
	case EXPR_FN:
		/* needs to go before decls_cgen.c... */
		e->fn->c.id = ++g->ident_counter;
		break;
	case EXPR_TYPE:
		cgen_sdecls_type(g, e->typeval);
		break;
	case EXPR_NMS: {
		char *prefix_part = cgen_nms_prefix_part(g, e->nms);
		size_t prefix_part_len = strlen(prefix_part);
		char const *prev_prefix = g->nms_prefixes ? *(char const **)arr_last(g->nms_prefixes)
			: "";
		size_t prev_prefix_len = strlen(prev_prefix);
		char *new_prefix = cgen_malloc(g, prev_prefix_len + prefix_part_len + 1);
		memcpy(new_prefix, prev_prefix, prev_prefix_len);
		memcpy(new_prefix + prev_prefix_len, prefix_part, prefix_part_len);
		free(prefix_part);
		*(char const **)arr_add(&g->nms_prefixes) = new_prefix;
		new_prefix[prev_prefix_len + prefix_part_len] = 0;
		e->nms->c.prefix = new_prefix;
	} break;
	default: break;
	}
	if (e->kind != EXPR_IDENT) {
		cgen_recurse_subexprs(g, e, cgen_sdecls_expr, cgen_sdecls_block, cgen_sdecls_decl);
	}
	if (e->kind == EXPR_NMS) {
		arr_remove_last(&g->nms_prefixes);
	}
}


static void cgen_sdecls_decl(CGenerator *g, Declaration *d) {
	cgen_sdecls_type(g, &d->type);
	if (cgen_fn_is_direct(g, d)) {
		d->expr.fn->c.name = d->idents[0];
	}
	for (int idx = 0; idx < (int)arr_len(d->idents); ++idx) {
		Type *type = decl_type_at_index(d, idx);

		if (type_is_builtin(type, BUILTIN_TYPE) && !(d->flags & DECL_IS_PARAM)) {
			Value *val = decl_val_at_index(d, idx);
			cgen_sdecls_type(g, val->type);
		}
	}
	if (d->flags & DECL_HAS_EXPR) {
		cgen_sdecls_expr(g, &d->expr);
		if (d->flags & DECL_EXPORT) {
			if (d->expr.kind == EXPR_FN)
				d->expr.fn->flags |= FN_EXPR_EXPORT;
		}
	}
}

static void cgen_sdecls_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_DECL:
		cgen_sdecls_decl(g, s->decl);
		break;
	case STMT_EXPR:
		cgen_sdecls_expr(g, &s->expr);
		break;
	case STMT_RET:
		if (s->ret.flags & RET_HAS_EXPR)
			cgen_sdecls_expr(g, &s->ret.expr);
		break;
	case STMT_INCLUDE:
		
		if (s->inc.inc_file && (s->inc.inc_file->flags & INC_FILE_CGEND_SDECLS)) {
			/* already generated */
		} else {
			if (s->inc.inc_file) s->inc.inc_file->flags |= INC_FILE_CGEND_SDECLS;
			arr_foreach(s->inc.stmts, Statement, sub)
				cgen_sdecls_stmt(g, sub);
		}
		break;
	case STMT_MESSAGE:
		break;
	}
}

static void cgen_sdecls_file(CGenerator *g, ParsedFile *f) {
	arr_foreach(f->stmts, Statement, s) {
		cgen_sdecls_stmt(g, s);
	}
}
