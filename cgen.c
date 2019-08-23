/* the generation of C code */
/* TODO: check ferror */
typedef struct {
	FILE *out;
	unsigned long anon_fn_count;
} CGenerator;


static void cgen_vwrite(CGenerator *g, const char *fmt, va_list args) {
	vfprintf(g->out, fmt, args);
}

static void cgen_write(CGenerator *g, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	cgen_vwrite(g, fmt, args);
	va_end(args);
}

static void cgen_writeln(CGenerator *g, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	cgen_vwrite(g, fmt, args);
	va_end(args);
	cgen_write(g, "\n");
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

static void cgen_create(CGenerator *g, FILE *out) {
	g->out = out;
	g->anon_fn_count = 0;
}

static void cgen_ident(CGenerator *g, Identifier i) {
	ident_fprint(g->out, i);
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
	case BUILTIN_F32: return "float";
	case BUILTIN_F64: return "double";
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

static bool cgen_expr(CGenerator *g, Expression *e) {
	switch (e->kind) {
	case EXPR_INT_LITERAL:
		cgen_write(g, "%lld", e->intl);
		break;
	case EXPR_FLOAT_LITERAL:
		/* TODO: more precision */
		cgen_write(g, "%f", e->floatl);
		break;
	case EXPR_STR_LITERAL:
		cgen_write(g, "\"");
		/* OPTIM: Maybe don't use i? this will probably be optimized by the compiler though... */
		for (size_t i = 0; i < e->strl.len; i++) {
			/* TODO: Print ordinary characters nicely */
			cgen_write(g, "\\x%02x", e->strl.str[i]);
		}
		cgen_write(g, "\"");
		break;
	case EXPR_IDENT:
		/* TODO: check if declared */
		cgen_ident(g, e->ident); 
		break;
	case EXPR_BINARY_OP:
		cgen_write(g, "(");
		cgen_expr(g, e->binary.lhs);
		switch (e->binary.op) {
		case BINARY_PLUS:
			cgen_write(g, "+");
			break;
		case BINARY_MINUS:
			cgen_write(g, "-");
			break;
		}
		cgen_expr(g, e->binary.rhs);
		cgen_write(g, ")");
		break;
	case EXPR_UNARY_OP:
		cgen_write(g, "(");
		switch (e->unary.op) {
		case UNARY_MINUS:
			cgen_write(g, "-");
			break;
		}
		cgen_expr(g, e->unary.of);
		cgen_write(g, ")");
		break;
	case EXPR_FN:
		err_print(e->where, "Function expression not part of declaration or call.");
		return false;
	}
	return true;
}

/* b = NULL => file */
static bool cgen_block_enter(Array stmts, Block *b) {
	bool ret = true;
	
	arr_foreach(&stmts, Statement, stmt) {
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
				if (infer_decl(decl)) {
					IdentDecl *ident_decl = arr_add(decls);
					ident_decl->decl = decl;
					ident_decl->scope = b;
				} else {
					ret = false;
				}
			}
			if (decl->expr.kind == EXPR_FN) {
				/* TODO */
			}
		}
	}
	return ret;
}

static bool cgen_block_exit(Array stmts, Block *b) {
	/* OPTIM: figure out some way of not re-iterating over everything */
	bool ret = true;
	arr_foreach(&stmts, Statement, stmt) {
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

static bool cgen_decl(CGenerator *g, Declaration *d) {
	/* TODO */
	return true;
}

static bool cgen_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_EXPR:
		if (!cgen_expr(g, &s->expr))
			return false;
		cgen_writeln(g, ";");
		break;
	case STMT_DECL:
		return cgen_decl(g, &s->decl);
	}
	return true;
}

/* 

because functions can have circular dependencies, we need two passes:
one declares the functions, and one defines them.

*/

static bool cgen_fns_stmt(CGenerator *g, Statement *s, bool def);

static bool cgen_fns_expr(CGenerator *g, Expression *e, Identifier fn_name, bool def) {
	switch (e->kind) {
	case EXPR_FN: {
		bool ret = true;
		FnExpr *f = &e->fn;
		cgen_type(g, &f->ret_type);
		if (!def) {
			/* get id for function */
			if (fn_name) {
				f->id = fn_name->c_fn_reps++;
			} else {
				f->id = g->anon_fn_count++;
			}
		}
		cgen_write(g, " ");
		if (fn_name) {
			cgen_ident(g, fn_name);
		} else {
			cgen_write(g, "a__");
		}
		if (f->id != 0)
			cgen_write(g, "%lu", f->id);
		cgen_write(g, "(");
		arr_foreach(&f->params, Param, p) {
			if (p != f->params.data)
				cgen_write(g, ", ");
			cgen_type(g, &p->type);
			cgen_write(g, " ");
			cgen_ident(g, p->name);
		}
		cgen_write(g, ")");
		if (def) {
			cgen_writeln(g, " {");
			arr_foreach(&f->body.stmts, Statement, s) {
				if (!cgen_stmt(g, s)) ret = false;
			}
			cgen_writeln(g, "}");
		} else {
			cgen_writeln(g, ";");
		}

		arr_foreach(&f->body.stmts, Statement, s) {
			if (!cgen_fns_stmt(g, s, def))
				ret = false;
		}
		return ret;
	}
	case EXPR_CALL:
		cgen_fns_expr(g, e->call.fn, NULL, def);
		break;
	default: break;
	}
	return true;
}

static bool cgen_fns_stmt(CGenerator *g, Statement *s, bool def) {
	switch (s->kind) {
	case STMT_EXPR:
		if (!cgen_fns_expr(g, &s->expr, NULL, def)) return false;
		break;
	case STMT_DECL:
		if (s->decl.flags & DECL_FLAG_HAS_EXPR) {
			if (!cgen_fns_expr(g, &s->decl.expr, *(Identifier*)s->decl.idents.data, def))
				return false;
		}
		break;
			
	}
	return true;
}

static bool cgen_fns(ParsedFile *f, CGenerator *g, bool def) {
	arr_foreach(&f->stmts, Statement, s) {
		cgen_fns_stmt(g, s, def);
	}
	return true;
}

static bool cgen_file(CGenerator *g, ParsedFile *f) {
	cgen_write_line_comment(g, "toc");
	bool ret = true;
	if (!cgen_fns(f, g, false)) return false;
	if (!cgen_fns(f, g, true)) return false;
	arr_foreach(&f->stmts, Statement, stmt) {
		if (stmt->kind == STMT_EXPR) {
			/* TODO: eventually make this an error / compile-time statement */
			warn_print(stmt->where, "Expression statement at top level.");
		}
		if (!cgen_stmt(g, stmt))
			ret = false;
	}
	return ret;
}
