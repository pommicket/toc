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
	unsigned long anon_var_count;
	Block *block;
	int indent_level;
	bool indent_next; /* should the next thing written be indented? */
	CGenWritingTo writing_to;
	Identifier main_ident;
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
	
/* static void cgen_write_comment(CGenerator *g, const char *fmt, ...) { */
/* 	cgen_write(g, "/\* "); */
/* 	va_list args; */
/* 	va_start(args, fmt); */
/* 	cgen_vwrite(g, fmt, args); */
/* 	va_end(args); */
/* 	cgen_write(g, " *\/"); */
/* } */

static void cgen_write_line_comment(CGenerator *g, const char *fmt, ...) {
	/* could switch to // for c99 */
	cgen_write(g, "/* ");
	va_list args;
	va_start(args, fmt);
	cgen_vwrite(g, fmt, args);
	va_end(args);
	cgen_write(g, " */\n");
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
		if ((decl->flags & DECL_FLAG_HAS_EXPR) && (decl->flags & DECL_FLAG_CONST)) {
			if (decl->expr.kind == EXPR_FN) {
				cgen_fn_name(g, &decl->expr.fn, NULL);
				return true;
			}
		}
	}
	cgen_indent(g);
	fprint_ident_reduced_charset(cgen_writing_to(g), i);
	return true;
}

static const char *builtin_type_to_str(BuiltinType b) {
	/* TODO: make this return int/long/etc. if stdint.h is not available */
	switch (b) {
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

/* will this function use a pointer parameter for output? (e.g. fn()[3]int => void(int (*x)[3]) */
static bool cgen_fn_uses_out_param(Type *fn_ret_type) {
	switch (fn_ret_type->kind) {
	case TYPE_TUPLE:
	case TYPE_ARR:
		return true;
	default:
		return false;
	}
}

static void cgen_type_pre(CGenerator *g, Type *t) {
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
		if (cgen_fn_uses_out_param(ret_type)) {
			cgen_write(g, "void ");
		} else {
			cgen_type_pre(g, ret_type);
		}
		cgen_write(g, "(*");
	} break;
	case TYPE_TUPLE:
		assert(0);
		return;
	case TYPE_UNKNOWN:
		err_print(t->where, "Type of unknown-typed expression required (x := #C(\"...\") will not work; you need to annotate the type of x).");
	    abort();
	case TYPE_ARR:
		cgen_type_pre(g, t->arr.of);
		break;
	}
}

static void cgen_type_post(CGenerator *g, Type *t);
/* either pass NULL for param_types (x)or for params */
static void cgen_fn_params(CGenerator *g, Type *param_types, Param *params, size_t nparams, Type *ret_type) {
	bool uses_out_param = cgen_fn_uses_out_param(ret_type);
	   
	cgen_write(g, "(");
	if (nparams) {
		for (size_t i = 0; i < nparams; i++) {
			if (i) {
				cgen_write(g, ",");
				cgen_write_space(g);
			}
			if (param_types) {
				cgen_type_pre(g, &param_types[i]);
				cgen_type_post(g, &param_types[i]);
			} else {
				Param *p = &params[i];
				cgen_type_pre(g, &p->type);
				cgen_ident(g, p->name, NULL);
				cgen_type_post(g, &p->type);
			}
		}
	} else {
		if (!uses_out_param)
			cgen_write(g, "void");
	}
	if (uses_out_param) {
		if (nparams) {
			cgen_write(g, ",");
			cgen_write_space(g);
		}
		/* write out param */
		cgen_type_pre(g, ret_type);
		cgen_write(g, "(*out__)"); /* TODO: fix this for named return values */
		cgen_type_post(g, ret_type);
			
	}
	cgen_write(g, ")");
}

static void cgen_type_post(CGenerator *g, Type *t) {
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
		bool uses_out_param = cgen_fn_uses_out_param(ret_type);
		cgen_write(g, ")");
		cgen_fn_params(g, param_types, NULL, nparams, ret_type);
		if (!uses_out_param) {
			cgen_type_post(g, ret_type);
		}
		cgen_write_space(g);
	} break;
	case TYPE_TUPLE:
		assert(0);
		return;
	case TYPE_ARR:
		cgen_write(g, "[%lu]", t->arr.n);
		cgen_type_post(g, t->arr.of);
		break;
	case TYPE_UNKNOWN: /* we should always do pre first */
		assert(0);
		break;
	}
}

static bool cgen_fn_name(CGenerator *g, FnExpr *f, Location *where) {
	if (f->name) {
		if (f->name == g->main_ident) {
			cgen_write(g, "main__");
		} else {
			return cgen_ident(g, f->name, where);
		}
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
	
	bool uses_out_param = cgen_fn_uses_out_param(&f->ret_type);
	size_t nparams = f->params.len;
	if (uses_out_param) {
		cgen_write(g, "void ");
		cgen_fn_name(g, f, NULL);
		cgen_fn_params(g, NULL, (Param*)f->params.data, nparams, &f->ret_type);
	
	} else {
	    cgen_type_pre(g, &f->ret_type);
		cgen_fn_name(g, f, NULL);
		cgen_fn_params(g, NULL, (Param*)f->params.data, nparams, &f->ret_type);
		cgen_type_post(g, &f->ret_type);
	}
	
	return true;
}

static bool cgen_block_enter(CGenerator *g, Block *b) {
	g->block = b;
	return block_enter(b, &b->stmts);
}

static bool cgen_block_exit(CGenerator *g, Block *into) {
	Block *b = g->block;
	g->block = into;
	return block_exit(b, &b->stmts);
}

