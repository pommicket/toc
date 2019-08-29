typedef struct {
	struct Block *scope; /* NULL for file scope */
	struct Declaration *decl;
} IdentDecl;

/* OPTIM: This is not ideal. There should be one dynamic array of tree nodes. */
typedef struct IdentTree {
	/* zero value is an empty trie */
	long id;
	int len; /* length of identifier = depth in tree */
	struct IdentTree *children;
	struct IdentTree *parent;
	Array decls; /* array of declarations of this identifier */
	unsigned long c_fn_reps; /* number of repetitions of this identifier in the C output--only used for functions */
	size_t depth;
} IdentTree;

typedef IdentTree *Identifier;

static IdentTree ident_base_tree;
static long ident_curr_id; /* NOTE: you should eventually add something to reset this */
static char identifier_chars[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

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
	return -1;
}

/* can this character be used in an identifier? */
static int isident(int c) {
	return ident_char_index(c) != -1; /* OPTIM: Write separate function */
}

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
			for (int i = 0; i < NIDENTIFIER_CHARS; i++) {
				t->children[i].parent = t; /* child's parent = self */
				t->children[i].depth = t->depth + 1;
			}
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


static void fprint_ident(FILE *out, Identifier id) {
	if (id->parent == NULL) return; /* at root */
	/* OPTIM: Use malloc(id->len)???? */
	fprint_ident(out, id->parent);
	fputc(identifier_chars[id - id->parent->children /* index of self in parent */], out);
}

static bool ident_eq_str(Identifier i, const char *s) {
	size_t len = strlen(s);
	if (i->depth != len) return false;
	s += len - 1;
	while (i->parent != NULL) {
		if (identifier_chars[i - i->parent->children /* index of self in parent */] != *s)
			return false;
		i = i->parent;
		if (i->parent != NULL)
			s--;
	}
	return true;
}

static char *ident_to_str(Identifier i) {
	char *str = malloc(i->depth + 1);
	str += i->depth;
	*str = 0;
	while (i->parent != NULL) {
		str--;
		*str = identifier_chars[i - i->parent->children];
		i = i->parent;
	}
	return str;
}

static IdentDecl *ident_decl(Identifier i) {
	return (IdentDecl*)arr_last(&i->decls);
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
