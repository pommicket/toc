/* figures out types and writes function prototypes */
/* TODO: check ferror */
typedef enum {
	  CGEN_WRITING_TO_H,
	  CGEN_WRITING_TO_C
} CGenWritingTo;
typedef struct {
	FILE *c_out;
	FILE *h_out;
	unsigned long anon_fn_count;
	Block *block;
	int indent_level;
	bool indent_next; /* should the next thing written be indented? */
	CGenWritingTo writing_to;
} CGenerator;

static FILE *cgen_writing_to(CGenerator *g) {
	switch (g->writing_to) {
	case CGEN_WRITING_TO_H:
		return g->h_out;
	case CGEN_WRITING_TO_C:
		return g->c_out;
	}
	assert(0);
	return NULL;
}

static void cgen_indent(CGenerator *g) {
	if (g->indent_next) {
		for (int i = 0; i < g->indent_level; i++)
			fprintf(cgen_writing_to(g), "\t");
		g->indent_next = false;
	}
}

static void cgen_vwrite(CGenerator *g, const char *fmt, va_list args) {
	cgen_indent(g);
	vfprintf(cgen_writing_to(g), fmt, args);
}

static void cgen_write(CGenerator *g, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	cgen_vwrite(g, fmt, args);
	va_end(args);
}

/* Used to write an UNNECESSARY space */
static void cgen_write_space(CGenerator *g) {
	cgen_write(g, " ");
}

/* Used to write something followed by an UNNECESSARY newline */
static void cgen_writeln(CGenerator *g, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	cgen_vwrite(g, fmt, args);
	va_end(args);
	cgen_write(g, "\n");
	g->indent_next = true;
}
	
static void cgen_write_comment(CGenerator *g, const char *fmt, ...) {
	cgen_write(g, "/* ");
	va_list args;
	va_start(args, fmt);
	cgen_vwrite(g, fmt, args);
	va_end(args);
	cgen_write(g, " */");
}

static void cgen_write_line_comment(CGenerator *g, const char *fmt, ...) {
	/* could switch to // for c99 */
	cgen_write(g, "/* ");
	va_list args;
	va_start(args, fmt);
	cgen_vwrite(g, fmt, args);
	va_end(args);
	cgen_write(g, " */\n");
}

static void cgen_create(CGenerator *g, FILE *c_out, FILE *h_out, const char *h_filename) {
	g->c_out = c_out;
	g->h_out = h_out;
	g->anon_fn_count = 0;
	g->indent_level = 0;
	g->block = NULL;
    g->indent_next = true;
	
	g->writing_to = CGEN_WRITING_TO_H;
	cgen_write(g, "#include <stdint.h>\n");
	
	g->writing_to = CGEN_WRITING_TO_C;
	cgen_write(g, "#include \"%s\"\n", h_filename);
	cgen_writeln(g, ""); /* extra newline between includes and code */
}


/* Pass NULL for where if you don't want to check if it's declared */
static bool cgen_fn_name(CGenerator *g, FnExpr *f, Location *where);
static bool cgen_ident(CGenerator *g, Identifier i, Location *where) {
	if (where) {
		IdentDecl *id_decl = ident_decl(i);
		if (!id_decl) {
			err_print(*where, "Identifier undeclared: %s", ident_to_str(i));
			return false;
		}
		Declaration *decl = id_decl->decl;
		if (decl->expr.kind == EXPR_FN) {
			cgen_fn_name(g, &decl->expr.fn, NULL);
			return true;
		}
		if (decl->where.line == where->line) {
			/* e.g. x: int = x; */
			err_print(*where, "Use of identifier \"%s\" in its own declaration.", ident_to_str(i));
			return false;
		} else if (decl->where.line > where->line) {
			/* x used before declared */
			char *str = ident_to_str(i);
			err_print(*where, "Use of identifier \"%s\" before its declaration.", str);
			info_print(decl->where, "%s will be declared here.", str);
			return false;
		}
	}
	cgen_indent(g);
	fprint_ident(cgen_writing_to(g), i);
	return true;
}

static const char *builtin_type_to_str(BuiltinType b) {
	/* TODO: make this return int/long/etc. if stdint.h is not available */
	switch (b) {
	case BUILTIN_INT: return "int64_t";
	case BUILTIN_I8: return "int8_t";
	case BUILTIN_I16: return "int16_t";
	case BUILTIN_I32: return "int32_t";
	case BUILTIN_I64: return "int64_t";
	case BUILTIN_U8: return "uint8_t";
	case BUILTIN_U16: return "uint16_t";
	case BUILTIN_U32: return "uint32_t";
	case BUILTIN_U64: return "uint64_t";
	case BUILTIN_FLOAT: return "float";
	case BUILTIN_DOUBLE: return "double";
	case BUILTIN_TYPE_COUNT: break;
	}
	assert(0);
	return NULL;
}

static bool cgen_type_pre(CGenerator *g, Type *t) {
	switch (t->kind) {
	case TYPE_VOID:
		cgen_write(g, "void ");
		break;
	case TYPE_BUILTIN:
		cgen_write(g, "%s ", builtin_type_to_str(t->builtin));
		break;
	case TYPE_FN: {
		Type *types = t->fn.types.data;
		Type *ret_type = &types[0];
		if (!cgen_type_pre(g, ret_type)) return false;
		cgen_write(g, "(*");
	} break;
	}
	return true;
}

static bool cgen_type_post(CGenerator *g, Type *t) {
	switch (t->kind) {
	case TYPE_VOID:
	case TYPE_BUILTIN:
		break;
	case TYPE_FN: {
		Type *types = t->fn.types.data;
		Type *ret_type = &types[0];
		Type *param_types = types + 1;
		assert(t->fn.types.len > 0);
		size_t nparams = t->fn.types.len-1;
		cgen_write(g, ")(");
		if (nparams) {
			for (size_t i = 0; i < nparams; i++) {
				if (!cgen_type_pre(g, &param_types[i])) return true;
				if (!cgen_type_post(g, &param_types[i])) return true;
				cgen_write(g, ",");
				cgen_write_space(g);
			}
		} else {
			cgen_write(g, "void");
		}
		cgen_write(g, ")");
		if (!cgen_type_post(g, ret_type)) return false;
	} break;
	}
	return true;
}

static bool cgen_fn_name(CGenerator *g, FnExpr *f, Location *where) {
	if (f->name) {
		if (ident_eq_str(f->name, "main"))
			cgen_write(g, "main__");
		else
			return cgen_ident(g, f->name, where);
	} else {
		cgen_write(g, "a___");
	}
	
	if (f->id != 0)
		cgen_write(g, "%lu", f->id);
	return true;
}

static bool cgen_fn_header(CGenerator *g, FnExpr *f) {	
	if (!f->name || g->block != NULL) {
		cgen_write(g, "static "); /* anonymous functions only exist in this translation unit */
	}
	if (!cgen_type_pre(g, &f->ret_type)) return false;
	cgen_fn_name(g, f, NULL);
	if (!cgen_type_post(g, &f->ret_type)) return false;
	cgen_write(g, "(");
	if (f->params.len) {
		arr_foreach(&f->params, Param, p) {
			if (p != f->params.data) {
				cgen_write(g, ",");
				cgen_write_space(g);
			}
			if (!cgen_type_pre(g, &p->type))
				return false;
			cgen_ident(g, p->name, NULL);
			if (!cgen_type_post(g, &p->type))
				return false;
		}
	} else {
		cgen_write(g, "void");
	}
	cgen_write(g, ")");
	return true;
}

static bool cgen_block_enter(CGenerator *g, Block *b) {
	bool ret = true;
	g->block = b;
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
				if (infer_decl(decl)) {
					IdentDecl *ident_decl = arr_add(decls);
					ident_decl->decl = decl;
					ident_decl->scope = b;
				} else {
					ret = false;
				}
			}
		}
	}
	return ret;
}

static bool cgen_block_exit(CGenerator *g, Block *into) {
	/* OPTIM: figure out some way of not re-iterating over everything */
	bool ret = true;
	Block *b = g->block;
	g->block = into;
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

