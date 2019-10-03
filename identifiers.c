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
	for (size_t i = 0; i < TREE_NCHILDREN; i++)
		tree->children[i] = NULL;
	tree->decls.data = NULL;
#endif
	tree->parent = parent;
	if (parent)
		tree->depth = (uint16_t)(parent->depth + 1);
	tree->index_in_parent = index_in_parent;
	return tree;
}

/* Initialize Identifiers. */
static void idents_create(Identifiers *ids) {
	block_arr_create(&ids->trees, 10, sizeof(IdentTree)); /* blocks of 1 << 10 = 1024 */
	ids->root = ident_new(ids, NULL, 0); /* create root tree */
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
		(*s)++;
	}
}


static void fprint_ident(FILE *out, Identifier id) {
	assert(id);
	if (id->parent == NULL) return; /* at root */
	fprint_ident(out, id->parent->parent); /* to go up one character, we need to go to the grandparent */
	int c_low = id->parent->index_in_parent;
	int c_high = id->index_in_parent;
	int c = ident_uchar_to_char(c_low + (c_high << 4)); 
	fputc(c, out);
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
		s++;
	}
	return tree;
}

static char *ident_to_str(Identifier i) {
	size_t i_len = (size_t)(i->depth / 2); /* length = depth / 2 */
	char *str = err_malloc(i_len + 1);
	str += i_len;
	*str = 0;
	while (i->parent) {
		str--;
		unsigned char c_high = i->index_in_parent;
		unsigned char c_low = i->parent->index_in_parent;
		char c = (char)ident_uchar_to_char((int)c_low + ((int)c_high << 4));
		*str = c;
		i = i->parent->parent; /* go to grandparent (prev char) */
	}
	
	return str;
}

static void ident_add_decl(Identifier i, struct Declaration *d, struct Block *b) {
	IdentDecl *id_decl = arr_add(&i->decls);
	id_decl->decl = d;
	id_decl->scope = b;
}

static IdentDecl *ident_decl(Identifier i) {
    return (IdentDecl *)arr_last(i->decls);
}

static void ident_tree_free(IdentTree *id) {
	if (!id) return;
	arr_clear(&id->decls);
	for (int i = 0; i < TREE_NCHILDREN; i++)
		ident_tree_free(id->children[i]);
}

static void idents_free(Identifiers *ids) {
	ident_tree_free(ids->root);
	block_arr_free(&ids->trees);
}
