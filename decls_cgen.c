/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static void cgen_sdecls_stmt(CGenerator *g, Statement *s);
static void cgen_sdecls_decl(CGenerator *g, Declaration *d);
static void cgen_sdecls_expr(CGenerator *g, Expression *e);
static void cgen_decls_stmt(CGenerator *g, Statement *s);
static void cgen_decls_block(CGenerator *g, Block *b);
static void cgen_decls_decl(CGenerator *g, Declaration *d);

/* i is the name for this type, NULL if not available */
static void cgen_sdecls_type(CGenerator *g, Type *type) {
	if (type->kind == TYPE_STRUCT) {
		StructDef *sdef = type->struc;
		/* we'll actually define the struct later; here we can just declare it */
	
		if ((sdef->flags & STRUCT_DEF_RESOLVED) && !(sdef->flags & STRUCT_DEF_CGEN_DECLARED)) {
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
	b->c.break_lbl = 0;
	b->c.cont_lbl = 0;
	
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
	case STMT_BREAK:
		if (!s->referring_to->c.break_lbl) {
			s->referring_to->c.break_lbl = ++g->lbl_counter;
		}
		break;
	case STMT_CONT:
		if (!s->referring_to->c.cont_lbl) {
			s->referring_to->c.cont_lbl = ++g->lbl_counter;
		}
		break;
	case STMT_MESSAGE:
		break;
	case STMT_DEFER:
		cgen_sdecls_stmt(g, s->defer);
		break;
	}
}

static void cgen_sdecls_file(CGenerator *g, ParsedFile *f) {
	arr_foreach(f->stmts, Statement, s) {
		cgen_sdecls_stmt(g, s);
	}
}

static void cgen_decls_type(CGenerator *g, Type *type) {
	if (type->kind == TYPE_STRUCT) {
		StructDef *sdef = type->struc;
		if ((sdef->flags & STRUCT_DEF_RESOLVED) && !(sdef->flags & STRUCT_DEF_CGEN_DEFINED)) {
			/* generate struct definition */
			cgen_write(g, "struct ");
			cgen_struct_name(g, sdef);
			cgen_write(g, "{");
			cgen_nl(g);
			++g->indent_lvl;
			arr_foreach(sdef->fields, Field, f) {
				cgen_type_pre(g, &f->type);
				cgen_write(g, " ");
				cgen_ident_simple(g, f->name);
				cgen_type_post(g, &f->type);
				cgen_write(g, ";");
				cgen_nl(g);
			}
			--g->indent_lvl;
			cgen_write(g, "};");
			cgen_nl(g);
			sdef->flags |= STRUCT_DEF_CGEN_DEFINED;
		}
	}
	cgen_recurse_subtypes(cgen_decls_type, g, type);
}

static void cgen_single_fn_decl(CGenerator *g, FnExpr *f, U64 which_are_const) {
	if (cgen_should_gen_fn(f)) {
		cgen_fn_header(g, f, which_are_const);
		cgen_write(g, ";");
		cgen_nl(g);
	}
}


static void cgen_decls_fn_instances(CGenerator *g, FnExpr *f) {
	Instance **data = f->instances->data;
	for (U64 i = 0; i < f->instances->cap; ++i) {
		if (f->instances->occupied[i]) {
			if (cgen_should_gen_fn((*data)->fn)) {
				(*data)->fn->c.name = f->c.name;
				(*data)->fn->c.id = f->c.id;
				cgen_single_fn_decl(g, (*data)->fn, (*data)->val.tuple[0].u64);
				cgen_write(g, ";");
				cgen_nl(g);
			}
		}
		++data;
	}
}

static void cgen_ctype(CGenerator *g, CType *c) {
	if (c->kind & CTYPE_UNSIGNED) {
		c->kind &= (CTypeKind)~(CTypeKind)CTYPE_UNSIGNED;
		cgen_write(g, "unsigned ");
	}
	switch (c->kind) {
	case CTYPE_CHAR:
		cgen_write(g, "char");
		break;
	case CTYPE_SIGNED_CHAR:
		cgen_write(g, "signed char");
		break;
	case CTYPE_SHORT:
		cgen_write(g, "short");
		break;
	case CTYPE_INT:
		cgen_write(g, "int");
		break;
	case CTYPE_LONG:
		cgen_write(g, "long");
		break;
	case CTYPE_LONGLONG:
		cgen_write(g, "long long");
		break;
	case CTYPE_PTR:
		cgen_write(g, "%s *", c->points_to);
		break;
	case CTYPE_FLOAT:
		cgen_write(g, "float");
		break;
	case CTYPE_DOUBLE:
		cgen_write(g, "double");
		break;
	case CTYPE_SIZE_T:
		cgen_write(g, "size_t");
		break;
	case CTYPE_VARARGS:
		cgen_write(g, "...");
		break;
	default:
		assert(0);
		break;
	}
}

static void cgen_fn_decl(CGenerator *g, FnExpr *f, Type *t) {
	if (f->flags & FN_EXPR_FOREIGN) {
		
		/* foreign function declaration */
		cgen_write(g, "extern ");
		Type *fn_types = t->fn.types;
		const char *foreign_name = f->foreign.name;
		CType *ctypes = f->foreign.ctypes;
		if (ctypes[0].kind == CTYPE_NONE) {
			cgen_type_pre(g, &fn_types[0]);
		} else {
			cgen_ctype(g, &ctypes[0]);
		}
		cgen_write(g, " %s", foreign_name);
		cgen_write(g, "(");
		
		for (size_t i = 1; i < arr_len(fn_types); ++i) {
			if (i > 1)
				cgen_write(g, ", ");
			CType *csub = &ctypes[i];
			if (csub->kind == CTYPE_NONE) {
				Type *sub = &fn_types[i];
				cgen_type_pre(g, sub);
				cgen_type_post(g, sub);
			} else {
				cgen_ctype(g, csub);
			}
		}
		cgen_write(g, ")");
		if (ctypes[0].kind == CTYPE_NONE)
			cgen_type_post(g, &fn_types[0]);
		cgen_write(g, ";");
		if (!f->c.name || !ident_eq_str(f->c.name, foreign_name) || g->nms != NULL) {
			cgen_write(g, "static ");
			if (ctypes[0].kind == CTYPE_NONE) {
				cgen_type_pre(g, &fn_types[0]);
			} else {
				cgen_ctype(g, &ctypes[0]);
			}
			
			cgen_write(g, " (* const ");
			cgen_fn_name(g, f);
			cgen_write(g, ")(");
			for (size_t i = 1; i < arr_len(fn_types); ++i) {
				if (i > 1)
					cgen_write(g, ", ");
				CType *csub = &ctypes[i];
				if (csub->kind == CTYPE_NONE) {
					Type *sub = &fn_types[i];
					cgen_type_pre(g, sub);
					cgen_type_post(g, sub);
				} else {
					cgen_ctype(g, csub);
				}
			}
			cgen_write(g, ") = %s;", foreign_name);
		}
		cgen_nl(g);
		return;
	}
	if (fn_has_instances(f)) {
		cgen_decls_fn_instances(g, f);
	} else {
		cgen_single_fn_decl(g, f, 0);
	}
}

static void cgen_decls_expr(CGenerator *g, Expression *e) {
	assert(e->flags & EXPR_FOUND_TYPE);
	cgen_recurse_subexprs(g, e, cgen_decls_expr, cgen_decls_block, cgen_decls_decl);
	switch (e->kind) {
	case EXPR_FN: {
		FnExpr *f = e->fn;
		f->c.name = NULL;
		if (!f->c.id)
			f->c.id = ++g->ident_counter;
		cgen_fn_decl(g, e->fn, &e->type);
	} break;
	case EXPR_TYPE: {
		Type *type = e->typeval;
		cgen_decls_type(g, type);
	} break;
	case EXPR_CAST:
		cgen_decls_type(g, &e->cast.type);
		break;
	case EXPR_BINARY_OP: {
		Type *lhs_type = &e->binary.lhs->type;
		if (lhs_type->kind == TYPE_PTR)
			lhs_type = lhs_type->ptr;
	} break;
	default:
		break;
	}
}

static void cgen_decls_block(CGenerator *g, Block *b) {
	Block *prev_block = g->block;
	g->block = b;
	arr_foreach(b->stmts, Statement, s)
		cgen_decls_stmt(g, s);
	if (b->ret_expr) cgen_decls_expr(g, b->ret_expr);
	g->block = prev_block;
}

static void cgen_decls_decl(CGenerator *g, Declaration *d) {
	cgen_decls_type(g, &d->type);
	if (cgen_fn_is_direct(g, d)) {
		cgen_fn_decl(g, d->expr.fn, &d->expr.type);
		cgen_recurse_subexprs(g, (&d->expr), cgen_decls_expr, cgen_decls_block, cgen_decls_decl);
	} else {
		if (d->flags & DECL_HAS_EXPR) {
			cgen_decls_expr(g, &d->expr);
		}
		if ((g->block == NULL || (g->block->flags & BLOCK_IS_NMS)) && g->fn == NULL) {
			for (int i = 0, n_idents = (int)arr_len(d->idents); i < n_idents; ++i) {
				Identifier ident = d->idents[i];
				Type *type = decl_type_at_index(d, i);
				if (!type_is_compileonly(type)) {
					if (!(d->flags & DECL_EXPORT))
						cgen_write(g, "static ");
					cgen_type_pre(g, type);
					cgen_write(g, " ");
					cgen_ident(g, ident);
					cgen_type_post(g, type);
					if (d->flags & DECL_HAS_EXPR) {
						Value *val = decl_val_at_index(d, i);
						cgen_write(g, " = ");
						cgen_val(g, val, type);
					} else {
						cgen_write(g, " = ");
						cgen_zero_value(g, type);
					}
					cgen_write(g, ";");
					cgen_nl(g);
				}
			}
		}
	}
}

static void cgen_decls_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_DECL:
		cgen_decls_decl(g, s->decl);
		break;
	case STMT_EXPR:
		cgen_decls_expr(g, &s->expr);
		break;
	case STMT_RET:
		if (s->ret.flags & RET_HAS_EXPR)
			cgen_decls_expr(g, &s->ret.expr);
		break;
	case STMT_INCLUDE:
		if (s->inc.inc_file && (s->inc.inc_file->flags & INC_FILE_CGEND_DECLS)) {
			/* already generated */
		} else {
			if (s->inc.inc_file) s->inc.inc_file->flags |= INC_FILE_CGEND_DECLS;
			arr_foreach(s->inc.stmts, Statement, sub)
				cgen_decls_stmt(g, sub);
		}
		break;
	case STMT_BREAK:
	case STMT_CONT:
	case STMT_MESSAGE:
		break;
	case STMT_DEFER:
		cgen_decls_stmt(g, s->defer);
		break;
	}
}

static void cgen_decls_file(CGenerator *g, ParsedFile *f) {
	cgen_write(g, "/* declarations */\n");
	arr_foreach(f->stmts, Statement, s) {
		cgen_decls_stmt(g, s);
	}
}
