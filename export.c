static void exptr_create(Exporter *exptr, FILE *out) {
	exptr->out = out;
}

static bool export_decl(Exporter *ex, Declaration *d) {
	if (!ex) {
		err_print(d->where, "Trying to export declaration, but a package output was not specified.");
		return false;
	}
	putc((int)(d->idents[0]->id % 256), ex->out);
	return true;
}
