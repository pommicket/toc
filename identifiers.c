/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/

// can this character be used in an identifier?
static int is_ident(int c) {
	if (c >= 'a' && c <= 'z')
		return 1;
	if (c >= 'A' && c <= 'Z')
		return 1;
	if (c >= '0' && c <= '9')
		return 1;
	if (c == '_') return 1;
#if CHAR_MIN < 0
	if (c < 0) // on systems where char = signed char, UTF-8 characters are probably < 0?
		return 1;
#endif
	if (c > 127) // UTF-8
		return 1;
	return 0;
}

// Initialize Identifiers.
static void idents_create(Identifiers *ids, Allocator *allocr, Block *scope) {
	str_hash_table_create(&ids->table, sizeof(IdentSlot) - sizeof(StrHashTableSlot), allocr);
	ids->rseed = 0x27182818;
	ids->scope = scope;
}

// advances s until a non-identifier character is reached, then returns the number of characters advanced
static size_t ident_str_len_advance(char **s) {
	char *original = *s;
	while (is_ident(**s)) {
		++*s;
	}
	return (size_t)(*s - original);
}

static size_t ident_str_len(char *s) {
	return ident_str_len_advance(&s);
}

// are these strings equal, up to the first non-ident character?
static bool ident_str_eq_str(const char *s, const char *t) {
	while (is_ident(*s) && is_ident(*t)) {
		if (*s != *t) return false;
		++s, ++t;
	}
	return !is_ident(*s) && !is_ident(*t);
}

static inline bool ident_eq_str(Identifier i, const char *s) {
	return ident_str_eq_str(i->str, s);
}

static inline bool ident_eq_string(Identifier i, String s) {
	if (i->len != s.len) return false;
	if (memcmp(i->str, s.str, s.len) != 0) return false;
	return true;
}

static inline Identifier ident_insert_with_len(Identifiers *ids, char *s, size_t len) {
	IdentSlot *slot = (IdentSlot *)str_hash_table_insert_(&ids->table, s, len);
	slot->idents = ids;
	return slot;
}

// moves s to the char after the identifier
// inserts if does not exist. reads until non-ident char is found.
// advances past identifier
static inline Identifier ident_insert(Identifiers *ids, char **s) {
	char *original = *s;
	size_t len = ident_str_len_advance(s);
	return ident_insert_with_len(ids, original, len);
}

static char *ident_to_str(Identifier i) {
	char *str = err_malloc(i->len + 1);
	// for some reason, GCC thinks that i->len is -1 when this is called from type_to_str_ (in release mode)

gcc_no_bounds_warnings_start
	memcpy(str, i->str, i->len);
gcc_no_bounds_warnings_end
	str[i->len] = 0;
	return str;
}

static String ident_to_string(Identifier i) {
	String ret;
	ret.str = i->str;
	ret.len = i->len;
	return ret;
}

static inline void fprint_ident_str(FILE *out, char *s) {
	fwrite(s, 1, ident_str_len(s), out);
}

static void fprint_ident(FILE *out, Identifier id) {
	fwrite(id->str, 1, id->len, out);
}

static void fprint_ident_debug(FILE *out, Identifier id) {
	fprint_ident(out, id);
}

static void print_ident(Identifier id) {
	fprint_ident_debug(stdout, id);
	printf("\n");
}

// reduced charset = a-z, A-Z, 0-9, _
static void fprint_ident_reduced_charset(FILE *out, Identifier id) {
	assert(id);
	for (const char *s = id->str; is_ident(*s); ++s) {
		int c = (unsigned char)(*s);
		if (c > 127) {
			fprintf(out, "x__%02x", c);
		} else {
			putc(*s, out);
		}
	}
}

static char *ident_to_str_reduced_charset(Identifier id) {
	assert(id);
	size_t nchars = 0;
	for (const char *s = id->str; is_ident(*s); ++s) {
		int c = (unsigned char)(*s);
		if (c > 127)
			nchars += 5;
		else
			++nchars;
	}
	char *ret = err_malloc(nchars+1);
	char *p = ret;
	for (const char *s = id->str; is_ident(*s); ++s) {
		int c = (unsigned char)(*s);
		if (c > 127)
			sprintf(p, "x__%02x", c);
		else
			*p = (char)c;
		++p;
	}
	*p = 0;
	assert(p == ret + nchars);
	return ret;
}



static inline Identifier ident_get_with_len(Identifiers *ids, char *s, size_t len) {
	return (Identifier)str_hash_table_get_(&ids->table, s, len);
}

// NULL = no such identifier. returns identifier "foo" for both "foo\0" and "foo+92384324..."
static inline Identifier ident_get(Identifiers *ids, char *s) {
	size_t len = ident_str_len(s);
	return ident_get_with_len(ids, s, len);
}

// translate and insert if not already there
static inline Identifier ident_translate_forced(Identifier i, Identifiers *to_idents) {
	char *p = i->str;
	Identifier translated = ident_insert(to_idents, &p);
	assert(translated->idents == to_idents);
	return translated;
}

// translate but don't add it if it's not there
static inline Identifier ident_translate(Identifier i, Identifiers *to_idents) {
	return ident_get(to_idents, i->str);
}

// returns true if i and j are equal, even if they're not in the same table
static inline bool ident_eq(Identifier i, Identifier j) {
	return i->len == j->len && memcmp(i->str, j->str, i->len) == 0;
}

static inline Block *ident_scope(Identifier i) {
	return i->idents->scope;
}

#if RUN_TESTS
static void idents_test(void) {
	Identifiers ids;
	char b[] = "foo_variable bar";
	char *s = b;
	Allocator a;
	allocr_create(&a);
	idents_create(&ids, &a, NULL);
	Identifier i1 = ident_insert(&ids, &s);
	assert(streq(s, " bar"));
	char b2[] = "foo_variable+6";
	s = b2;
	Identifier i2 = ident_insert(&ids, &s);
	assert(streq(s, "+6"));
	assert(i1 == i2);
	allocr_free_all(&a);
	
}
#endif

static void idents_print_all(Identifiers *idents) {
	str_hash_table_print(&idents->table);
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

static Location ident_decl_location(Identifier i) {
	return i->decl->where;
}

