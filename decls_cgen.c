static void cgen_decls_stmt(CGenerator *g, Statement *s) {
	switch (s->kind) {
	case STMT_DECL:
		break;
	case STMT_EXPR:
		break;
	case STMT_RET:
		break;
	}
}

static void cgen_decls_file(CGenerator *g, ParsedFile *f) {
	cgen_write(g, "/* declarations */\n");
	arr_foreach(f->stmts, Statement, s) {
		cgen_decls_stmt(g, s);
	}
}
