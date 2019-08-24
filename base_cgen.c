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

static void cgen_vwrite(CGenerator *g, const char *fmt, va_list args) {
	if (g->indent_next) {
		for (int i = 0; i < g->indent_level; i++)
			fprintf(cgen_writing_to(g), "\t");
		g->indent_next = false;
	}
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
	g->writing_to = CGEN_WRITING_TO_C;
	g->indent_next = true;
	
	cgen_write(g, "#include \"%s\"\n", h_filename);
	cgen_write(g, "#include <stdint.h>\n");
	cgen_writeln(g, ""); /* extra newline between includes and code */
}

static void cgen_ident(CGenerator *g, Identifier i) {
	fprint_ident(cgen_writing_to(g), i);
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

/* NOTE: this will eventually be split into two functions when functions/arrays are added */
static bool cgen_type(CGenerator *g, Type *t) {
	switch (t->kind) {
	case TYPE_VOID:
		cgen_write(g, "void");
		break;
	case TYPE_BUILTIN:
		cgen_write(g, "%s", builtin_type_to_str(t->builtin));
		break;
	}
	return true;
}

static void cgen_fn_name(CGenerator *g, FnExpr *f) {
	if (f->name)
		cgen_ident(g, f->name);
	else
		cgen_write(g, "a___");
	
	if (f->id != 0)
		cgen_write(g, "%lu", f->id);
}

static bool cgen_fn_header(CGenerator *g, FnExpr *f) {
	CGenWritingTo writing_to_before = g->writing_to;
	if (!f->name || g->block != NULL) {
		cgen_write(g, "static "); /* anonymous functions only exist in this translation unit */
	}
	if (!cgen_type(g, &f->ret_type)) return false;
	cgen_write(g, " ");
	cgen_fn_name(g, f);
	cgen_write(g, "(");
	arr_foreach(&f->params, Param, p) {
		if (p != f->params.data) {
			cgen_write(g, ",");
			cgen_write_space(g);
		}
		if (!cgen_type(g, &p->type))
			return false;
		cgen_write(g, " ");
		cgen_ident(g, p->name);
	}
	cgen_write(g, ")");
	g->writing_to = writing_to_before;
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

