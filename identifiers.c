/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
#if CHAR_MAX - CHAR_MIN > 255
#error "Currently only systems with 8-bit characters can compile toc."
/* TODO: maybe do a run-time error for large characters? */
#endif

/* can this character be used in an identifier? */
static int is_ident(int c) {
	if (c >= 'a' && c <= 'z')
		return 1;
	if (c >= 'A' && c <= 'Z')
		return 1;
	if (c >= '0' && c <= '9')
		return 1;
	if (c == '_') return 1;
#if CHAR_MIN < 0
	if (c < 0) /* on systems where char = signed char, UTF-8 characters are probably < 0? */
		return 1;
#endif
	if (c > 127) /* UTF-8 */
		return 1;
	return 0;
}


static Identifier ident_new_anonymous(Identifiers *ids) {
	IdentTree *node = block_arr_add(&ids->trees);
	memset(node, 0, sizeof *node);
	node->anonymous = true;
	return node;
}

/* used internally to allocate identifiers */
static Identifier ident_new(Identifiers *ids, Identifier parent, unsigned char index_in_parent) {
	IdentTree *node = ident_new_anonymous(ids);
	node->parent = parent;
	if (parent)
		node->depth = (uint16_t)(parent->depth + 1);
	node->index_in_parent = index_in_parent;
	node->anonymous = false;
	return node;
}


/* Initialize Identifiers. */
static void idents_create(Identifiers *ids) {
	block_arr_create(&ids->trees, 10, sizeof(IdentTree)); /* blocks of 1 << 10 = 1024 */
	ids->root = ident_new(ids, NULL, 0); /* create root tree */
	ids->nidents = 0;
}

#if CHAR_MIN < 0
#define ident_char_to_uchar(c) ((c) < 0 ? (256 + (c)) : (c))
#else
#define ident_char_to_uchar(c) (c)
#endif

#if CHAR_MIN < 0
#define ident_uchar_to_char(c) ((c) > 127 ? ((c) - 256) : (c))
#else
#define ident_uchar_to_char(c) (c)
#endif

/* moves s to the char after the identifier */
/* inserts if does not exist. reads until non-ident char is found. */
/* advances past identifier */
static Identifier ident_insert(Identifiers *ids, char **s) {
	IdentTree *tree = ids->root;
	while (1) {
		if (!is_ident(**s)) {
			return tree;
		}
		int c = ident_char_to_uchar(**s);
		assert(c >= 0 && c <= 255);
		unsigned char c_low = (unsigned char)(c & 0xf);
		unsigned char c_high = (unsigned char)(c >> 4);
		if (!tree->children[c_low]) {
			tree->children[c_low] = ident_new(ids, tree, c_low);
		}
		tree = tree->children[c_low];
		
		if (!tree->children[c_high]) {
			tree->children[c_high] = ident_new(ids, tree, c_high);
		}
		tree = tree->children[c_high];
		++(*s);
	}
}

static inline size_t ident_len(Identifier i) {
	if (i->anonymous) return 3;
	return (size_t)(i->depth / 2);
}

static char *ident_to_str(Identifier i) {
	size_t i_len = ident_len(i);
	char *str = err_malloc(i_len + 1);
	if (i->anonymous) {
		strcpy(str, "???");
		return str;
	}
	str += i_len;
	*str = 0;
	while (i->parent) {
		--str;
		unsigned char c_high = i->index_in_parent;
		unsigned char c_low = i->parent->index_in_parent;
		char c = (char)ident_uchar_to_char((int)c_low + ((int)c_high << 4));
		*str = c;
		i = i->parent->parent; /* go to grandparent (prev char) */
	}
	
	return str;
}


static void fprint_ident(FILE *out, Identifier id) {
	char *str = ident_to_str(id);
	fprintf(out, "%s", str);
	free(str);
}

static void fprint_ident_debug(FILE *out, Identifier id) {
#ifdef TOC_DEBUG
	if (id->export_id)
		printf(U64_FMT "-", id->export_id);
#endif
	fprint_ident(out, id);
}

static void print_ident(Identifier id) {
	fprint_ident_debug(stdout, id);
	printf("\n");
}

/* reduced charset = a-z, A-Z, 0-9, _ */
static void fprint_ident_reduced_charset(FILE *out, Identifier id) {
	assert(id);
	if (id->anonymous) {
		fprintf(out, "x___");
	}
	if (id->parent == NULL) return; /* at root */
	fprint_ident_reduced_charset(out, id->parent->parent); /* to go up one character, we need to go to the grandparent */
	int c_low = id->parent->index_in_parent;
	int c_high = id->index_in_parent;
	int c = c_low + (c_high << 4);
	if (c > 127) {
		fprintf(out, "x__%x",c);
	} else {
		char chr = (char)ident_uchar_to_char(c);
		fputc(chr, out);
	}
}

/* NULL = no such identifier */
static Identifier ident_get(Identifiers *ids, const char *s) {
	IdentTree *tree = ids->root;
	while (*s) {
		int c = ident_char_to_uchar(*s);
		assert(c >= 0 && c <= 255);
		unsigned char c_low = (unsigned char)(c & 0xf);
		unsigned char c_high = (unsigned char)(c >> 4);
		tree = tree->children[c_low];
		if (!tree) return NULL;
		tree = tree->children[c_high];
		if (!tree) return NULL;
		++s;
	}
	return tree;
}

static Identifier ident_translate(Identifier i, Identifiers *to_idents) {
	/* OPTIM */
	if (!i || i->anonymous) return NULL;
	char *s = ident_to_str(i);
	Identifier new_ident = ident_get(to_idents, s);
	free(s);
	return new_ident;
}

static IdentDecl *ident_add_decl(Identifier i, struct Declaration *d, struct Block *b) {
	IdentDecl *id_decl = arr_add(&i->decls);
	id_decl->decl = d;
	id_decl->scope = b;
	id_decl->flags = 0;
	id_decl->kind = IDECL_DECL;
	return id_decl;
}

static IdentDecl *ident_decl(Identifier i) {
	return (IdentDecl *)arr_last(i->decls);
}

static void ident_tree_free(IdentTree *id) {
	if (!id) return;
	arr_clear(&id->decls);
	for (int i = 0; i < TREE_NCHILDREN; ++i)
		ident_tree_free(id->children[i]);
}

static void idents_free(Identifiers *ids) {
	ident_tree_free(ids->root);
	block_arr_free(&ids->trees);
}

static void idents_test(void) {
	Identifiers ids;
	char b[] = "foo_variable bar";
	char *s = b;
	idents_create(&ids);
	ident_insert(&ids, &s);
	assert(strcmp(s, " bar") == 0);
	idents_free(&ids);
}

static int ident_index_in_decl(Identifier i, Declaration *d) {
	int index = 0;
	arr_foreach(d->idents, Identifier, j) {
		if (i == *j)
			return index;
		++index;
	}
	return -1;
}

static bool ident_eq_str(Identifier i, const char *s) {
	const char *t = s + (strlen(s) - 1);
	while (1) {
		if (!i->parent) {
			return false;
		}
		int c_low = i->parent->index_in_parent;
		int c_high = i->index_in_parent;
		int c = ident_uchar_to_char(c_low + (c_high << 4));
		if (c != *t) return false;
		i = i->parent->parent;
		if (t > s) --t;
		else break;
	}
	if (i->parent) return false; /* not at root */
	return true;
}

static Location idecl_where(IdentDecl *id) {
	
	switch (id->kind) {
	case IDECL_DECL:
		return id->decl->where;
	case IDECL_EXPR:
		return id->expr->where;
	}
	assert(0);
	Location def = {0};
	return def;
}
