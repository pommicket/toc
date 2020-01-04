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
static int isident(int c) {
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


/* used internally to allocate identifiers */
static Identifier ident_new(Identifiers *ids, Identifier parent, unsigned char index_in_parent) {
	IdentTree *tree = block_arr_add(&ids->trees);
	memset(tree, 0, sizeof *tree); /* use zero value of IdentTree */
#ifdef NONZERO_NULL_PTRS
	tree->parent = NULL;
	for (size_t i = 0; i < TREE_NCHILDREN; ++i)
		tree->children[i] = NULL;
	tree->decls = NULL;
#endif
	tree->parent = parent;
	if (parent)
		tree->depth = (uint16_t)(parent->depth + 1);
	tree->index_in_parent = index_in_parent;
	tree->id = 0;
	return tree;
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
		if (!isident(**s)) {
			if (!tree->id)
				tree->id = ++ids->nidents;
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


static void fprint_ident(FILE *out, Identifier id) {
	Identifier i = id;
	size_t chars = 0;
	while (i->parent) {
	    i = i->parent->parent; /* to go up one character, we need to go to the grandparent */
		++chars;
	}
	printf("%lu-",(unsigned long)id->id);
	char *s = malloc(chars + 1);
	char *p = s + chars;
	i = id;
	*p-- = '\0';
	while (i->parent) {
		int c_low = i->parent->index_in_parent;
		int c_high = i->index_in_parent;
	    char c = (char)ident_uchar_to_char(c_low + (c_high << 4)); 
		*p-- = c;
		i = i->parent->parent;
	}
	fprintf(out, "%s", s);
	free(s);
}

static void print_ident(Identifier id) {
	fprint_ident(stdout, id);
	printf("\n");
}

/* reduced charset = a-z, A-Z, 0-9, _ */
static void fprint_ident_reduced_charset(FILE *out, Identifier id) {
	assert(id);
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

static char *ident_to_str(Identifier i) {
	size_t i_len = (size_t)(i->depth / 2); /* length = depth / 2 */
	char *str = err_malloc(i_len + 1);
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
