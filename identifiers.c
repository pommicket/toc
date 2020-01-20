/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
#if CHAR_MAX - CHAR_MIN > 255
#error "Currently only systems with 8-bit characters can compile toc."
/* TODO: not necessary anymore */
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

/* Initialize Identifiers. */
static void idents_create(Identifiers *ids) {
	str_hash_table_create(&ids->table, sizeof(IdentSlot) - sizeof(StrHashTableSlot), NULL);
	ids->rseed = 0x27182818;
}

/* advances s until a non-identifier character is reached, then returns the number of characters advanced */
static size_t ident_str_len(char **s) {
	char *original = *s;
	while (is_ident(**s)) {
		++*s;
	}
	return (size_t)(*s - original);
}

static U64 ident_hash(char **s) {
	char *original = *s;
	return str_hash(original, ident_str_len(s));
}

/* are these strings equal, up to the first non-ident character? */
static bool ident_str_eq_str(const char *s, const char *t) {
	while (is_ident(*s) && is_ident(*t)) {
		if (*s != *t) return false;
		++s, ++t;
	}
	return !is_ident(*s) && !is_ident(*t);
}

static inline bool ident_eq_str(Identifier i, const char *s) {
	if (i->anonymous) return false;
	return ident_str_eq_str(i->str, s);
}



static IdentSlot **ident_slots_insert(IdentSlot **slots, char *s, size_t i) {
	IdentSlot **slot;
	size_t nslots = arr_len(slots);
	while (1) {
		slot = &slots[i];
		if (!*slot) break;
		if (s && ident_eq_str(*slot, s))
			break;
		i = (i+1) % nslots;
	}
	return slot;
}

static Identifier ident_new_anonymous(Identifiers *ids) {
	U32 idx = rand_u32(ids->rseed);
	ids->rseed = idx;
	IdentSlot *slot = (IdentSlot *)str_hash_table_insert_anonymous_(&ids->table);
	slot->anonymous = true;
	return slot;
}

/* moves s to the char after the identifier */
/* inserts if does not exist. reads until non-ident char is found. */
/* advances past identifier */
static Identifier ident_insert(Identifiers *ids, char **s) {
	char *original = *s;
	size_t len = ident_str_len(s);
	return (Identifier)str_hash_table_insert_(&ids->table, original, len);
}

static char *ident_to_str(Identifier i) {
	char *str = err_malloc(i->len + 1);
	/* for some reason, GCC thinks that i->len is -1 when this is called from type_to_str_ (in release mode)
	   TODO: test this now (some stuff was changed)
	*/

#if !defined(__clang__) && defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-overflow"
#pragma GCC diagnostic ignored "-Wrestrict"
#pragma GCC diagnostic ignored "-Warray-bounds"
#endif
	memcpy(str, i->str, i->len);
	

#if !defined(__clang__) && defined(__GNUC__)
#pragma GCC diagnostic pop
#endif
	str[i->len] = 0;
	return str;
}


static void fprint_ident(FILE *out, Identifier id) {
	fwrite(id->str, 1, id->len, out);
}

static void fprint_ident_debug(FILE *out, Identifier id) {
	if (id->anonymous) {
		fprintf(out, "???");
		return;
	}
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
		/* hack to generate unique C identifiers */
		fprintf(out, "a%p__",(void *)id);
		return;
	}
	for (const char *s = id->str; is_ident(*s); ++s) {
		int c = (unsigned char)(*s);
		if (c > 127) {
			fprintf(out, "x__%x", c);
		} else {
		    putc(*s, out);
		}
	}
}

/* NULL = no such identifier. returns identifier "foo" for both "foo\0" and "foo+92384324..." */
static Identifier ident_get(Identifiers *ids, char *s) {
	char *ptr = s;
	size_t len = ident_str_len(&ptr);
	return (Identifier)str_hash_table_get_(&ids->table, s, len);
}

static Identifier ident_translate(Identifier i, Identifiers *to_idents) {
	if (!i || i->anonymous) return NULL;
	Identifier new_ident = ident_get(to_idents, i->str);
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

/* returns true if i and j are equal, even if they're not in the same table */
static bool ident_eq(Identifier i, Identifier j) {
	return i->len == j->len && memcmp(i->str, j->str, i->len) == 0;
}

static void idents_free(Identifiers *ids) {
	arr_foreach(ids->table.slots, StrHashTableSlotPtr, slotp) {
		IdentSlot *slot = *(IdentSlot **)slotp;
		if (slot) arr_clear(&slot->decls);
	}
	str_hash_table_free(&ids->table);
}

#ifdef TOC_DEBUG
static void idents_test(void) {
	Identifiers ids;
	char b[] = "foo_variable bar";
	char *s = b;
	idents_create(&ids);
	Identifier i1 = ident_insert(&ids, &s);
	assert(strcmp(s, " bar") == 0);
	char b2[] = "foo_variable+6";
	s = b2;
	Identifier i2 = ident_insert(&ids, &s);
	assert(strcmp(s, "+6") == 0);
	assert(i1 == i2);
	
	idents_free(&ids);
}
#endif

static int ident_index_in_decl(Identifier i, Declaration *d) {
	int index = 0;
	arr_foreach(d->idents, Identifier, j) {
		if (i == *j)
			return index;
		++index;
	}
	return -1;
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
