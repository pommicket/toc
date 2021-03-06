/*
toc's types. Note that although these types are in the public domain,
the code which uses them (i.e. most of the rest of toc) is not necessarily.

This is free and unencumbered software released into the public domain.
Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.
In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
For more information, please refer to <http://unlicense.org/>
*/
// NOTE: make sure you edit copy.c and cgen_recurse_subexprs/types when you make a change to expression-related types or type-related types in this file!

typedef double Floating;


#if __STDC_VERSION__ >= 199901 || defined _MSC_VER
#define LONGLONG_AVAILABLE 1
typedef long long longlong;
typedef unsigned long long ulonglong;
#define LONGLONG_FMT "%lld"
#define ULONGLONG_FMT "%llu"
#else
#define LONGLONG_AVAILABLE 0
typedef long longlong;
typedef unsigned long ulonglong;
#define LONGLONG_FMT "%ld"
#define ULONGLONG_FMT "%lu"
#endif

// generic function pointer
typedef void (*FnPtr)(void);

// try to find the type with the strictest alignment
typedef union {
	double floating;
	void *ptr;
	longlong integer;
	FnPtr fn_ptr;
} MaxAlign;

typedef uint8_t U8;
#define U8_MAX UINT8_MAX
typedef uint16_t U16;
#define U16_MAX UINT16_MAX
typedef uint32_t U32;
#define U32_MAX UINT32_MAX
typedef uint64_t U64;
#define U64_MAX UINT64_MAX
#define U8_FMT "%" PRIu8
#define U16_FMT "%" PRIu16
#define U32_FMT "%" PRIu32
#define U64_FMT "%" PRIu64

typedef int8_t I8;
#define I8_MAX INT8_MAX
typedef int16_t I16;
#define I16_MAX INT16_MAX
typedef int32_t I32;
#define I32_MAX INT32_MAX
typedef int64_t I64;
#define I64_MAX INT64_MAX
#define I8_FMT "%" PRId8
#define I16_FMT "%" PRId16
#define I32_FMT "%" PRId32
#define I64_FMT "%" PRId64

#if __STDC_VERSION__ >= 199901
#include <stdbool.h>
#elif defined __cplusplus
#else
typedef U8 bool;
#define false ((bool)0)
#define true ((bool)1)
#endif

#if defined __GNUC__ && !defined NO_WARN_UNUSED_RESULT
#define WarnUnusedResult __attribute__((warn_unused_result))
#else
#define WarnUnusedResult
#endif

#define Status bool WarnUnusedResult

// NOTE: if you change these, make sure you change hash_tables.c
typedef float F32;
typedef double F64;
#define F32_MANT_DIG FLT_MANT_DIG
#define F32_DIG FLT_DIG
#define F64_MANT_DIG DBL_MANT_DIG
#define F64_DIG DBL_DIG

#define F32_FMT "%.10f"
#define F64_FMT "%.18f"

typedef U32 IdentID; // identifier ID for cgen (anonymous variables).

typedef struct ErrCtx {
	bool enabled;
	bool color_enabled;
	bool have_errored;
	struct Location *instance_stack; // stack of locations which generate the instances we're dealing with
} ErrCtx;


typedef struct Page {
	struct Page *next;
	size_t used; // number MaxAligns used, not bytes
	MaxAlign data[];
} Page;

typedef struct Allocator {
	Page *first;
	Page *last;
} Allocator;

// initialize to 0
typedef struct HashTable {
	void *data;
	bool *occupied;
	U64 n;
	U64 cap;
} HashTable;

typedef struct Slice {
	I64 len;
	void *data;
} Slice;

typedef union Value {
	U8 u8;
	U16 u16;
	U32 u32;
	U64 u64;
	I8 i8;
	I16 i16;
	I32 i32;
	I64 i64;
	bool boolv;
	char charv;
	F32 f32;
	F64 f64;
	struct FnExpr *fn;
	void *arr;
	void *ptr;
	void *struc;
	union Value *tuple;
	Slice slice;
	struct Type *type;
	struct Namespace *nms;
	struct VarArg *varargs; // dynamic array
} Value;
typedef Value *ValuePtr;

typedef struct VarArg {
	struct Type *type;
	Value val;
} VarArg;

typedef struct {
	struct StructDef *struc;
	struct Declaration *use_decl; // field declaration which uses the identifier
} UsedFrom;

typedef struct IdentSlot {
	char *str;
	size_t len;
	// where this identifier was declared
	struct Declaration *decl; // if NULL, a declaration hasn't been found for it yet
	struct Identifiers *idents;
	union {
		struct Namespace *nms; // only exists after typing, and only for namespace-level declarations (i.e. not local variables inside namespaces)
		UsedFrom *used_from; // for stuff used inside structs -- NULL if this is actually in the struct body
	};
} IdentSlot;

typedef struct StrHashTableSlot {
	const char *str;
	size_t len;
	MaxAlign data[];
} StrHashTableSlot;

typedef StrHashTableSlot *StrHashTableSlotPtr;

typedef struct StrHashTable {
	StrHashTableSlot **slots;
	Allocator *allocr;
	U32 rand_seed;
	size_t data_size;
	size_t nentries; // # of filled slots
} StrHashTable;

typedef IdentSlot *Identifier;

typedef IdentSlot *IdentSlotPtr;
typedef struct Identifiers {
	StrHashTable table;
	U32 rseed;
	struct Block *scope; // NULL for file scope
} Identifiers;

typedef enum {
	TOKEN_KW,
	TOKEN_IDENT,
	TOKEN_DIRECT,
	TOKEN_LITERAL_INT,
	TOKEN_LITERAL_FLOAT,
	TOKEN_LITERAL_CHAR,
	TOKEN_LITERAL_STR,
	TOKEN_EOF
} TokenKind;

typedef enum {
	DIRECT_C,
	DIRECT_SIZEOF,
	DIRECT_ALIGNOF,
	DIRECT_EXPORT,
	DIRECT_FOREIGN,
	DIRECT_BUILTIN,
	DIRECT_INCLUDE,
	DIRECT_FORCE,
	DIRECT_IF,
	DIRECT_ERROR,
	DIRECT_WARN,
	DIRECT_INFO,
	DIRECT_NO_WARN,
	DIRECT_INIT,
	DIRECT_COUNT
} Directive;

static const char *directives[DIRECT_COUNT] = {
	"C", "sizeof", "alignof", "export", "foreign", "builtin", "include", "force", "if",
	"error", "warn", "info", "no_warn", "init"
};

typedef enum {
	KW_SEMICOLON,
	KW_COLON,
	KW_LPAREN,
	KW_RPAREN,
	KW_LBRACE,
	KW_RBRACE,
	KW_LSQUARE,
	KW_RSQUARE,
	KW_EQ_EQ,
	KW_PLUS_EQ,
	KW_MINUS_EQ,
	KW_ASTERISK_EQ,
	KW_SLASH_EQ,
	KW_PERCENT_EQ,
	KW_NE,
	KW_LE,
	KW_LT,
	KW_GE,
	KW_GT,
	KW_ANDAND,
	KW_OROR,
	KW_PLUS,
	KW_MINUS,
	KW_ASTERISK,
	KW_EXCLAMATION,
	KW_AMPERSAND,
	KW_SLASH,
	KW_PERCENT,
	KW_DOTDOT,
	KW_DOTCOMMA,
	KW_COMMADOT,
	KW_COMMACOMMA,
	KW_COMMA,
	KW_DOT,
	KW_EQ,
	KW_LAST_SYMBOL = KW_EQ, // last one entirely consisting of symbols
	KW_IF,
	KW_ELIF,
	KW_ELSE,
	KW_WHILE,
	KW_FOR,
	KW_RETURN,
	KW_BREAK,
	KW_CONTINUE,
	KW_DEFER,
	KW_FN,
	KW_AS,
	KW_STRUCT,
	KW_INT,
	KW_I8,
	KW_I16,
	KW_I32,
	KW_I64,
	KW_U8,
	KW_U16,
	KW_U32,
	KW_U64,
	KW_FLOAT,
	KW_F32,
	KW_F64,
	KW_VOID,
	KW_TYPE,
	KW_NAMESPACE,
	KW_CHAR,
	KW_BOOL,
	KW_TRUE,
	KW_FALSE,
	KW_NMS,
	KW_USE,
	KW_TYPEOF,
	KW_SIZEOF,
	KW_ALIGNOF,
	KW_NULL,
	KW_COUNT
} Keyword;

static const char *const keywords[KW_COUNT] = {
	";", ":", "(", ")", "{", "}", "[", "]", "==",
	"+=", "-=", "*=", "/=", "%=",
	"!=", "<=", "<", ">=", ">", "&&", "||",
	"+", "-", "*", "!", "&", "/", "%", "..", ".,", ",.", ",,", ",", ".",
	"=",
	"if", "elif", "else", "while", "for", "return", "break",
	"continue", "defer", "fn", "as", "struct",
	"int", "i8", "i16", "i32", "i64",
	"u8", "u16", "u32", "u64", "float", "f32", "f64", "void",
	"Type",	"Namespace", "char", "bool", "true", "false", "nms", "use",
	"typeof", "sizeof", "alignof", "null"
};

typedef struct String {
	char *str;
	size_t len;
} String;

typedef String StrLiteral;

typedef struct {
	U32 line;
	U32 start; // index in file->contents
	U32 end; // exclusive
} SourcePos;

typedef struct Token {
	TokenKind kind;
	SourcePos pos;
	union {
		Keyword kw;
		Directive direct;
		char *ident;
		U64 intl;
		Floating floatl;
		char chr;
		StrLiteral str;
	};
} Token;

typedef struct {
	ErrCtx *ctx;
	const char *filename;
	char *contents;
	Token *tokens;
	U32 *no_warn_lines; // in sorted order; right now we do a binary search
} File;

typedef struct Location {
	File *file;
	U32 start; // index of first token
	U32 end; // index of one past last token
} Location;


typedef struct Tokenizer {
	Allocator *allocr;
	Token *tokens;
	File *file;
	char *s; // string being parsed
	ErrCtx *err_ctx;
	U32 line;
	Token *token; // token currently being processed
	Identifiers *globals;
} Tokenizer;


typedef enum {
	// if you change this enum, make sure you change std/types.toc (preferrably, add stuff to the end of the enum)
	TYPE_UNKNOWN = 0,
	TYPE_BUILTIN = 1,
	TYPE_FN = 2,
	TYPE_TUPLE = 3,
	TYPE_ARR = 4,
	TYPE_PTR = 5,
	TYPE_SLICE = 6,
	TYPE_EXPR = 7, // just use this expression as the type. this kind of type doesn't exist after resolving.
	TYPE_STRUCT = 8
#define TYPE_COUNT 9
} TypeKind;


typedef enum {
	// if you change this enum, make sure you change std/types.toc (preferrably, add stuff to the end of the enum)
	BUILTIN_I8 = 0,
	BUILTIN_U8 = 1,
	BUILTIN_I16 = 2,
	BUILTIN_U16 = 3,
	BUILTIN_I32 = 4,
	BUILTIN_U32 = 5,
	BUILTIN_I64 = 6,
	BUILTIN_U64 = 7,
	BUILTIN_F32 = 8,
	BUILTIN_F64 = 9,
	BUILTIN_CHAR = 10,
	BUILTIN_BOOL = 11,
	BUILTIN_TYPE = 12,
	BUILTIN_VARARGS = 13,
	BUILTIN_NMS = 14,
	BUILTIN_VOID = 15
} BuiltinType;


typedef U8 Constness;

#define CONSTNESS_NO ((Constness)0)
#define CONSTNESS_SEMI ((Constness)1)
#define CONSTNESS_YES ((Constness)2)

typedef struct FnType {
	struct Type *types; // dynamic array [0] = ret_type, [1:] = param_types
	Constness *constness; // [i] = constness of param #i. iff no parameters are constant, this is NULL. don't use it as a dynamic array, because it might not always be.
} FnType;

typedef struct {
	struct Type *of;
	union {
		U64 n; // after resolving
		struct Expression *n_expr; // before resolving
	};
} ArrType;

enum {
	TYPE_IS_FLEXIBLE = 0x01,
	TYPE_IS_RESOLVED = 0x02,
};
typedef U8 TypeFlags;
typedef struct Type {
	TypeKind kind;
	TypeFlags flags;
	union {
		// NOTE: if you modify this, please make sure type information still works (see BINARY_DOT under types_expr)
		BuiltinType builtin;
		FnType *fn;
		struct Type *tuple;
		ArrType *arr;
		struct Type *ptr;
		struct Type *slice;
		struct StructDef *struc; // multiple resolved types can refer to the same struct
		struct Expression *expr;
	};
} Type;


// field of a struct
typedef struct Field {
	Location where;
	Identifier name;
	Type *type;
	size_t offset; // offset during compile time
} Field;


enum {
	BLOCK_FINDING_TYPES = 0x01,
	BLOCK_FOUND_TYPES = 0x02,
};
typedef enum {
	BLOCK_OTHER,
	BLOCK_FN,
	BLOCK_NMS,
	BLOCK_FOR,
	BLOCK_WHILE,
	BLOCK_STRUCT
} BlockKind;

typedef U8 BlockFlags;
typedef struct Block {
	// NOTE: make sure you check copy.c when you add something to this
	BlockFlags flags;
	// set during the parsing phase, but don't access while this specific block is being
	// parsed, because sometimes it's set after parse_block
	BlockKind kind;
	struct {
		IdentID break_lbl, cont_lbl; // initially 0, set to non-zero values if needed (tr->lbl_counter); set during typing
	} c;
	Location where;
	Identifiers idents;
	struct Statement *stmts;
	struct Block *parent;
	struct Statement **deferred; // deferred stuff from this block; used by both eval and cgen
	struct Use **uses; // use statements (for types.c)
} Block;

typedef Block *BlockPtr;

enum {
	STRUCT_DEF_FOUND_OFFSETS = 0x01,
	STRUCT_DEF_CGENERATED = 0x02,
	STRUCT_DEF_RESOLVED = 0x04,
	STRUCT_DEF_RESOLVING = 0x08,
	STRUCT_DEF_RESOLVING_FAILED = 0x10
};
typedef U8 StructFlags;
typedef struct StructDef {
	// these two only exist after resolving (before then, it's scope.stmts)
	Field *fields;
	Location where;
	/*
		use this instead of fields when looking up a field, because it will include "use"d things.
		this only consists of statements which are declarations after typing (and not #ifs,
		for example)
	*/
	Block body;
	union {
		HashTable instances;
		struct {
			size_t size; // size of this struct during compile time
			size_t align;
			U64 instance_id; // ID of instance
		};
	};
	Identifier name;
	struct Declaration *params;
	struct {
		// if name is NULL, use this
		IdentID id;
	} c;
	StructFlags flags;
} StructDef;
typedef StructDef *StructDefPtr;

typedef enum {
	EXPR_LITERAL_FLOAT,
	EXPR_LITERAL_INT,
	EXPR_LITERAL_STR,
	EXPR_LITERAL_BOOL,
	EXPR_LITERAL_CHAR,
	EXPR_IDENT, // variable or constant
	EXPR_BINARY_OP,
	EXPR_UNARY_OP,
	EXPR_FN,
	EXPR_CAST,
	EXPR_CALL,
	EXPR_TUPLE,
	EXPR_C,
	EXPR_BUILTIN,
	EXPR_SLICE,
	EXPR_TYPE,
	EXPR_NMS,
	/*
	   a value (it's useful to have this).
	   right now they don't work with cgen_set_tuple
	   (as of yet, that is unneeded)
	 */
	EXPR_VAL
} ExprKind;

typedef enum {
	UNARY_MINUS,
	UNARY_ADDRESS, // &x
	UNARY_DEREF, // *x
	UNARY_NOT, // !x
	UNARY_TYPEOF, // typeof x
	UNARY_DSIZEOF,
	UNARY_DALIGNOF,
	UNARY_SIZEOF,
	UNARY_ALIGNOF
} UnaryOp;

typedef enum {
	BINARY_SET, // e.g. x = y
	BINARY_ADD,
	BINARY_SUB,
	BINARY_MUL,
	BINARY_DIV,
	BINARY_MOD,
	BINARY_SET_ADD, // e.g. x += y
	BINARY_SET_SUB,
	BINARY_SET_MUL,
	BINARY_SET_DIV,
	BINARY_SET_MOD,
	BINARY_GT,
	BINARY_LT,
	BINARY_GE,
	BINARY_LE,
	BINARY_EQ,
	BINARY_NE,
	BINARY_AT_INDEX, // e.g. x[i]
	BINARY_DOT,
	BINARY_AND, // &&
	BINARY_OR // ||
} BinaryOp;

typedef struct CallExpr {
	struct Expression *fn;
	union {
		struct Argument *args; // before typing
		struct Expression *arg_exprs; // after typing
	};
	struct Instance *instance; // NULL = ordinary function, no compile time args
} CallExpr;


typedef enum {
	CTYPE_NONE = 0x00,
	CTYPE_CHAR = 0x01,
	CTYPE_SHORT = 0x02,
	CTYPE_INT = 0x03,
	CTYPE_LONG = 0x04,
	CTYPE_LONGLONG = 0x05,
	CTYPE_SIGNED_CHAR = 0x06,
	CTYPE_UNSIGNED = 0x08,
	CTYPE_UNSIGNED_CHAR = CTYPE_UNSIGNED|CTYPE_CHAR,
	CTYPE_UNSIGNED_SHORT = CTYPE_UNSIGNED|CTYPE_SHORT,
	CTYPE_UNSIGNED_INT = CTYPE_UNSIGNED|CTYPE_INT,
	CTYPE_UNSIGNED_LONG = CTYPE_UNSIGNED|CTYPE_LONG,
	CTYPE_UNSIGNED_LONGLONG = CTYPE_UNSIGNED|CTYPE_LONGLONG,
	// things that can't be unsigned
	CTYPE_PTR = 0x10,
	CTYPE_FLOAT,
	CTYPE_DOUBLE,
	CTYPE_SIZE_T,
	CTYPE_VARARGS
} CTypeKind;
typedef struct {
	CTypeKind kind;
	char *points_to; // if kind == CTYPE_PTR, ident string of C type which it points to
} CType;

enum {
	FN_EXPR_FOREIGN = 0x01,
	FN_EXPR_EXPORT = 0x02, // set during typing
	FN_EXPR_HAS_VARARGS = 0x04
};
typedef struct FnExpr {
	Location where;
	Block *declaration_block; // block wherein this function is declared
	U64 instance_id; // 0 if not an instance. needs to be available even for #foreign functions
	union {
		struct {
			struct Declaration *params; // declarations of the parameters to this function
			struct Declaration *ret_decls; // array of decls, if this has named return values. otherwise, NULL
			Type ret_type;
			Block body;
		};
		struct {
			Type type; // type of this function
			CType *ctypes; // ctypes[i] = CTYPE_NONE if this isn't a ctype, or the specified CType. don't use this as a dynamic array.
			const char *name;
			// name of foreign function and dynamic library file it comes from (dll/so)
			struct Expression *name_expr; // STILL VALID even after running type_of_fn, because sometimes we run type_of_fn multiple times on a function
			const char *lib;
			struct Expression *lib_expr; // see name_expr
			FnPtr fn_ptr;
		} foreign;
	};
	HashTable *instances; /* for fns with constant parameters. the key is a tuple where
							 the first element is a u64 value whose ith bit (1<<i) is 1
							 if the ith semi-constant parameter is constant.
						  */
	struct {
		// if name = NULL, this is an anonymous function, and id will be the ID of the fn.
		Identifier name;
		IdentID id;
	} c;
	U8 flags;
} FnExpr; // an expression such as fn(x: int) int { return 2 * x; }
typedef FnExpr *FnExprPtr;

typedef struct Instance {
	Value val; // key into hash table
	union {
		FnExpr *fn; // the typed function
		StructDef struc;
	};
} Instance;

typedef struct CastExpr {
	struct Expression *expr;
	Type type;
	CType ctype;
} CastExpr;

typedef struct NewExpr {
	Type type;
	struct Expression *n; // e.g. for new(int, 5)
} NewExpr;

typedef struct SliceExpr {
	struct Expression *of; // required
	struct Expression *from; // optional
	struct Expression *to; // optional
	struct {
		IdentID id;
	} c;
} SliceExpr;

#define PLATFORM_OTHER 0
#define PLATFORM_LINUX 1
#define PLATFORM_WINDOWS 2
#define PLATFORM_OSX 3
#define PLATFORM_FREEBSD 4
#define PLATFORM_OPENBSD 5
#define PLATFORM_MISC_UNIX 6

typedef enum {
	BUILTIN_STDOUT,
	BUILTIN_STDERR,
	BUILTIN_STDIN,
	BUILTIN_COMPILING,
	BUILTIN_DEBUG,
	BUILTIN_PLATFORM
#define BUILTIN_VAL_COUNT (BUILTIN_PLATFORM+1)
} BuiltinVal;

const char *const builtin_val_names[BUILTIN_VAL_COUNT] =
	{"stdout", "stderr", "stdin", "compiling", "debug", "platform"};

typedef struct Namespace {
	Block body;
	Identifier associated_ident; // if this is foo ::= nms { ... }, then associated_ident is foo; can be NULL. used by cgen. only non-null if the namespace isn't in a non-namespace block
	struct IncludedFile *inc_file; // NULL if this is not generated from an include to nms
	struct {
		char *prefix; // generated during typing
	} c;
} Namespace;
typedef Namespace *NamespacePtr;

enum {
	EXPR_FOUND_TYPE = 0x01
};

typedef U8 ExprFlags;

typedef struct Expression {
	Type type;
	Location where;
	ExprKind kind;
	ExprFlags flags;
	struct {
		IdentID id; // cgen ID used for this expression
	} cgen;
	union {
		Floating floatl;
		U64 intl;
		StrLiteral strl;
		bool booll;
		char charl;
		struct {
			UnaryOp op;
			struct Expression *of;
		} unary;
		struct {
			BinaryOp op;
			struct Expression *lhs;
			union {
				struct Expression *rhs;
				Field *field; // for struct., after resolving
			};
		} binary;
		CallExpr call;
		struct {
			struct Expression *code;
		} c;
		struct {
			union {
				struct Expression *expr;
				BuiltinVal val;
			} which;
		} builtin;
		String ident_str; // before typing
		Identifier ident; // after typing
		NewExpr new;
		Namespace *nms;
		struct {
			Type type;
		} del;
		FnExpr *fn;
		CastExpr cast;
		SliceExpr slice;
		Block *block;
		struct Expression *tuple; // dynamic array, even after typing
		Type *typeval;
		Value val;
	};
} Expression;


typedef struct Argument {
	Location where;
	char *name; // NULL = no name
	Expression val;
} Argument;

enum {
	DECL_ANNOTATES_TYPE = 0x0001,
	DECL_IS_CONST = 0x0002,
	DECL_SEMI_CONST = 0x0004,
	DECL_HAS_EXPR = 0x0008,
	DECL_FOUND_TYPE = 0x0010,
	DECL_ERRORED_ABOUT_SELF_REFERENCE = 0x0020, // has there been an error about this decl referencing itself?
	DECL_FOUND_VAL = 0x0040,
	DECL_INFER = 0x0080, // infer the value (e.g. fn(t::Type=, x:t))
	DECL_EXPORT = 0x0100,
	DECL_IS_PARAM = 0x0200,
	DECL_USE = 0x0400 // e.g. use p: Point
};

typedef U16 DeclFlags;

typedef struct Declaration {
	Location where;
	Identifier *idents;
	Type type;
	DeclFlags flags;
	union {
		Expression expr;
		Field *field; // pointer to the field which the first identifier in this decl refers to
		struct {
			union {
				struct {
					// only exist before typing
					Expression *name;
					Expression *lib;
				};
				// only set for non-functions
				const char *name_str;
			};
		} foreign;
	};
	Value val; // only for constant decls, non-constant globals, and varargs.

	/*
		for eval, for non-constant decls
		the pointers to values need to be fixed, which is why this isn't just Value *.
		no, this can't be a union with val, because of global variables and varargs
	*/
	Value **val_stack;
} Declaration;
typedef Declaration *DeclarationPtr;

enum {
	IF_STATIC = 0x01
};

typedef struct If {
	U8 flags;
	Expression *cond; // NULL = this is an else
	struct If *next_elif; // next elif/else of this statement
	Block body;
} If;

typedef struct While {
	Expression *cond;
	Block body;
} While;

enum {
	FOR_IS_RANGE = 0x01,
	FOR_INCLUDES_FROM = 0x02,
	FOR_INCLUDES_TO = 0x04
};
typedef U8 ForFlags;
typedef struct {
	Expression *from; // can't be null
	Expression *to; // can be null
	union {
		// (either) can be null
		Expression *step; // before typing
		Value *stepval; /* after typing. the type of this is header.type.tuple[0] (i.e. the value type for this for loop),
NOTE: this might be different from the original ForExpr.step.type, because of implicit type conversions. */
	};
} RangeFor;
typedef struct For {
	ForFlags flags;
	Declaration header;
	Block body;
	union {
		RangeFor range;
		Expression *of;
	};
} For;

enum {
	RET_HAS_EXPR = 0x01
};
typedef struct Return {
	U8 flags;
	Block *referring_to; // eval uses this; it's the function body we're returning from
	Expression expr;
} Return;

enum {
	INC_FILE_INCLUDING = 0x01, // are we currently in the process of including this file (i.e. during typing)?
	INC_FILE_CGEND_SDECLS = 0x10,
	INC_FILE_CGEND_DECLS = 0x20,
	INC_FILE_CGEND_DEFS = 0x40,
	INC_FILE_CGEND = 0x80
};
typedef U8 IncFileFlags;
typedef struct IncludedFile {
	IncFileFlags flags;
	Namespace *main_nms; // namespace of first inclusion
	struct Statement *stmts;
} IncludedFile;

enum {
	INC_FORCED = 0x01,
	INC_TO_NMS = 0x02
};

typedef struct {
	U8 flags;
	String filename;
	char *nms; // NULL if this is just a plain old #include, otherwise string which can be used with ident_get
} Include;

typedef enum {
	MESSAGE_ERROR,
	MESSAGE_WARN,
	MESSAGE_INFO
} MessageKind;

typedef struct {
	MessageKind kind;
	Expression text;
} Message;

typedef struct Use {
	Expression expr;
} Use;
typedef Use *UsePtr;

typedef enum {
	STMT_DECL,
	STMT_EXPR,
	STMT_RET,
	STMT_BREAK,
	STMT_CONT,
	STMT_INCLUDE, // turns into STMT_INLINE_BLOCK after typing
	STMT_MESSAGE,
	STMT_DEFER,
	STMT_USE,
	STMT_IF,
	STMT_FOR,
	STMT_WHILE,
	STMT_BLOCK,
	STMT_INLINE_BLOCK // a group of statements acting as one statement, e.g. all the statements from a #include
} StatementKind;
enum {
	STMT_TYPED = 0x01,
	STMT_IS_INIT = 0x02 // is this an initialization statement? if so, during typing it will be added to gctx->inits (this can't be done at parsing because the statements array is changing)
};
typedef struct Statement {
	Location where;
	StatementKind kind;
	U8 flags;
	union {
		Declaration *decl; // we want the pointer to be fixed so that we can refer to it from an identifier
		Expression *expr;
		Return *ret;
		Include *inc;
		Message *message; // #error, #warn, #info
		Block *referring_to; // for break/continue; set during typing
		struct Statement *defer;
		struct Statement *inline_block; // statements in an inline block (dynamic array)
		Use *use;
		If *if_;
		While *while_;
		For *for_;
		Block *block;
	};
} Statement;

typedef Statement *StatementPtr;
/*
	Statements to be run before any code in main is called.
	This is mainly for the standard library, so you don't have to do something weird
	like io.init();
*/
typedef struct {
	Statement *stmt;
} Initialization;

typedef struct ParsedFile {
	Statement *stmts;
} ParsedFile;

typedef struct {
	Statement *stmt;
	Block *block;
	Namespace *nms;
} StatementWithCtx;

typedef struct {
	/*
		statements to be run before main function is called.
		these are in order of appearance (which is the order in which they are called)
	*/
	Initialization *inits;
	StrHashTable included_files; // maps to IncludedFile.
	File *main_file; // this is the file which the compiler is invoked on. needed for checking for circular includes.
	StatementWithCtx *static_ifs; // all the #ifs
	ErrCtx *err_ctx;
	bool debug_build;
} GlobalCtx;

typedef struct Parser {
	Tokenizer *tokr;
	Allocator *allocr;
	Identifiers *globals;
	File *file;
	Block *block; // which block are we in? NULL = file scope
	ParsedFile *parsed_file;
	GlobalCtx *gctx;
} Parser;

typedef struct {
	Allocator *allocr;
	StrHashTable libs_loaded; // of Library (NOTE: Library is defined in foreign_something.c)
} ForeignFnManager;

typedef struct Evaluator {
	Allocator *allocr;
	struct Typer *typer;
	Block *returning; // function body from which we are returning OR loop body in which we are continuing/breaking
	bool is_break; // is returning because of a break, as opposed to a continue?
	void **to_free; // array of pointers to free once block is exited
	Declaration **decls_given_values; // array of declarations whose last value in their val stacks should be removed when the block is exited
	Value ret_val;
	ForeignFnManager ffmgr;
	bool evaluating_struct_member_type; // we only allow references to future structs if it's within another struct (so that circularly dependent structs can exist). hence, this keeps track of if we're evaluating the type of a declaration inside a struct.
} Evaluator;


/*
	so there are loops in cgen that generate all the function declarations/definitions
	and they need to know what namespace they're in (because we name mangle stuff in namespaces)
*/
typedef struct {
	FnExpr *fn;
	Namespace *nms;
	Block *block;
} FnWithCtx;

typedef struct {
	Declaration *d;
	Namespace *nms;
	Block *block;
} DeclWithCtx;

typedef struct Typer {
	Allocator *allocr;
	Evaluator *evalr;
	Identifiers *globals;
	Use **uses; // global used things
	Declaration **in_decls; // array of declarations we are currently inside
	Block *block;
	FnExpr *fn; // the function we're currently parsing.
	GlobalCtx *gctx;
	ParsedFile *parsed_file;
	Namespace *nms;
	FnWithCtx *all_fns; // does not include templates
	StructDef **all_structs;
	DeclWithCtx *all_globals; // includes stuff in namespaces, as long as it's not in a function
	IdentID lbl_counter;
	unsigned long nms_counter; // counter for namespace IDs
	StrHashTable included_files; // maps to IncludedFile
	// did we get an error which is bad enough that we should stop typing?
	bool had_fatal_err;
} Typer;

typedef struct CGenerator {
	Allocator *allocr;
	FILE *outc;
	IdentID ident_counter, lbl_counter;
	U16 indent_lvl; // how many levels of indentation?
	bool will_indent; // will the next thing be indented?
	ParsedFile *file;
	Block *block;
	Namespace *nms;
	FnExpr *fn; // which function are we in? (NULL for none) - not used during decls
	Identifier main_ident;
	Identifiers *globals;
	char **nms_prefixes; // dynamic (null-terminated) array of namespace prefixes
	GlobalCtx *gctx;
} CGenerator;
