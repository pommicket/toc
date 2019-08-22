static char identifier_chars[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.";
#define NIDENTIFIER_CHARS ((int)((sizeof identifier_chars) - 1)) /* -1 for null char */

/* returns -1 if c is not a valid identifier character, its index in identifier_chars otherwise */
static int ident_char_index(int c) {
	if (c >= 'a' && c <= 'z')
		return c - 'a';	
	if (c >= 'A' && c <= 'Z')
		return c - 'A' + 26;
	if (c >= '0' && c <= '9')
		return c - '0' + 52;
	if (c == '_') return 62;
	if (c == '.') return 63;
	return -1;
}

/* can this character be used in an identifier? */
static int isident(int c) {
	/* NOTE: . is only used internally in identifiers */
	return ident_char_index(c) != -1 && c != '.'; /* OPTIM: Write separate function */
}

/* can this character be used as the first character in an identifier? */
static int isidentstart(int c) {
	return isident(c);
}

typedef struct {
	struct Block *scope; /* NULL for file scope */
	struct Declaration *decl;
} IdentDecl;

typedef struct IdentTree {
	/* zero value is an empty trie */
	long id;
	int len; /* length of identifier = depth in tree */
	struct IdentTree *children;
	struct IdentTree *parent;
	Array decls; /* array of declarations of this identifier */
} IdentTree;

typedef IdentTree *Identifier;

static IdentTree ident_base_tree;
static long ident_curr_id; /* NOTE: you should eventually add something to reset this */

/* moves s to the char after the identifier */
static Identifier ident_tree_insert(IdentTree *t, char **s) {
	while (1) {
		char c = **s;
		if (!isident(c)) {
			if (t->id == 0) t->id = ++ident_curr_id;
			return t;
		}
		
		if (!t->children) {
			/* allocate children */
			t->children = err_calloc(NIDENTIFIER_CHARS, sizeof *t->children);
			for (int i = 0; i < NIDENTIFIER_CHARS; i++)
				t->children[i].parent = t; /* child's parent = self */
		}
		t = &t->children[ident_char_index(c)];
		(*s)++;
	}
}

/* inserts if does not exist. reads until non-ident char is found. */
/* advances past identifier */
static Identifier ident_insert(char **s) {
	return ident_tree_insert(&ident_base_tree, s);
}


static void ident_fprint(FILE *out, Identifier id) {
	if (id->parent == NULL) return; /* at root */
	/* OPTIM: Use malloc(id->len)???? */
	ident_fprint(out, id->parent);
	fputc(identifier_chars[id - id->parent->children /* index of self in parent */], out);
}

static void idents_free_tree(IdentTree *tree) {
	if (!tree->children) return;
	for (int i = 0; i < NIDENTIFIER_CHARS; i++)
		idents_free_tree(&tree->children[i]);
	free(tree->children);
}

static void idents_free(void) {
	idents_free_tree(&ident_base_tree);
}
