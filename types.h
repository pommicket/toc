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
/* NOTE: make sure you edit copy.c and cgen_recurse_subexprs/types when you make a change to expression-related types or type-related types in this file! */

typedef double Floating;


#if __STDC_VERSION__ >= 199901 || defined _MSC_VER
#define LONGLONG_AVAILABLE 1
typedef long long longlong;
#else
#define LONGLONG_AVAILABLE 0
typedef long longlong;
#endif


/* try to find the type with the strictest alignment */
typedef union {
	double floating;
	void *ptr;
	longlong integer;
	void (*fn_ptr)(void);
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

/* NOTE: if you change these, make sure you change hash_tables.c */
typedef float F32;
typedef double F64;
#define F32_MANT_DIG FLT_MANT_DIG
#define F32_DIG FLT_DIG
#define F64_MANT_DIG DBL_MANT_DIG
#define F64_DIG DBL_DIG

#define F32_FMT "%.16f"
#define F64_FMT "%.16f"

typedef U32 IdentID; /* identifier ID for cgen (anonymous variables). not to be confused with IdentTree.id */

/* for keeping track of whence something came */
#ifdef TOC_DEBUG
#define SOURCE_LOCATION char *src_file; int src_line;
#define SOURCE_LOCATION_PARAMS char *src_file, int src_line,
#define DEBUG_UNDERSCORE(x) x##_
#else
#define SOURCE_LOCATION
#define SOURCE_LOCATION_PARAMS
#define DEBUG_UNDERSCORE(x) x
#endif


typedef struct ErrCtx {
	bool enabled;
	bool color_enabled;
	bool have_errored;
	struct Location *instance_stack; /* stack of locations which generate the instances we're dealing with */
} ErrCtx;


typedef struct Page {
	struct Page *next;
	size_t used; /* number MaxAligns used, not bytes */
	MaxAlign data[];
} Page;

typedef struct Allocator {
	Page *first;
	Page *last;
} Allocator;

/* initialize to 0 */
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
	struct VarArg *varargs; /* dynamic array */
} Value;

typedef struct VarArg {
	struct Type *type;
	Value val;
} VarArg;

typedef struct IdentSlot {
	char *str;
	size_t len;
	/* where this identifier was declared */
	struct Declaration *decl; /* if NULL, a declaration hasn't been found for it yet */
	struct Identifiers *idents;
	struct Namespace *nms; /* only exists after typing, and only for namespace-level declarations (i.e. not local variables) */
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
	size_t nentries; /* # of filled slots */
} StrHashTable;

typedef IdentSlot *Identifier;

typedef IdentSlot *IdentSlotPtr;
typedef struct Identifiers {
	StrHashTable table;
	U32 rseed;
	struct Block *scope; /* NULL for file scope */
} Identifiers;

typedef enum {
	TOKEN_KW,
	TOKEN_IDENT,
	TOKEN_DIRECT,
	TOKEN_LITERAL_NUM,
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
	DIRECT_COUNT
} Directive;

static const char *directives[DIRECT_COUNT] = {
	"C", "sizeof", "alignof", "export", "foreign", "builtin", "include", "force", "if", "error", "warn",
	"info"
};

typedef enum {
	KW_SEMICOLON,
	KW_COLON,
	KW_COMMA,
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
	KW_PLUS,
	KW_MINUS,
	KW_ASTERISK,
	KW_EXCLAMATION,
	KW_AMPERSAND,
	KW_SLASH,
	KW_PERCENT,
	KW_DOTDOT,
	KW_DOT,
	KW_EQ,
	KW_LAST_SYMBOL = KW_EQ, /* last one entirely consisting of symbols */
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
	KW_COUNT
} Keyword;

static const char *const keywords[KW_COUNT] = {
	";", ":", ",", "(", ")", "{", "}", "[", "]", "==",
	"+=", "-=", "*=", "/=", "%=",
	"!=", "<=", "<", ">=", ">",
	"+", "-", "*", "!", "&", "/", "%", "..", ".",
	"=",
	"if", "elif", "else", "while", "for", "return", "break",
	"continue", "defer", "fn", "as", "struct",
	"int", "i8", "i16", "i32", "i64",
	"u8", "u16", "u32", "u64", "float", "f32", "f64", "Type",
	"Namespace", "char", "bool", "true", "false", "nms", "use",
	"typeof", "sizeof", "alignof"
};

typedef enum {
	NUM_LITERAL_INT,
	NUM_LITERAL_FLOAT
} NumLiteralKind;

typedef struct NumLiteral {
	NumLiteralKind kind;
	union {
		U64 intval;
		Floating floatval;
	};
} NumLiteral;

typedef struct String {
	char *str;
	size_t len;
} String;

typedef String StrLiteral;

typedef struct {
	U32 line;
	U32 start; /* index in ctx->str */
	U32 end; /* exclusive */
} SourcePos;

typedef struct Token {
	TokenKind kind;
	SourcePos pos;
	union {
		Keyword kw;
		Directive direct;
		char *ident;
		NumLiteral num;
		char chr;
		StrLiteral str;
	};
} Token;

typedef struct {
	ErrCtx *ctx;
	const char *filename;
	char *contents;
	Token *tokens;
} File;

typedef struct Location {
	File *file;
	U32 start; /* index of first token */
	U32 end; /* index of one past last token */
} Location;


typedef struct Tokenizer {
	Allocator *allocr;
	Token *tokens;
	File *file;
	char *s; /* string being parsed */
	ErrCtx *err_ctx;
	U32 line;
	Token *token; /* token currently being processed */
	Identifiers *globals;
} Tokenizer;


typedef enum {
	TYPE_UNKNOWN,
	TYPE_VOID,
	TYPE_BUILTIN,
	TYPE_FN,
	TYPE_TUPLE,
	TYPE_ARR,
	TYPE_PTR,
	TYPE_SLICE,
	TYPE_EXPR, /* just use this expression as the type. this kind of type doesn't exist after resolving. */
	TYPE_STRUCT
#define TYPE_COUNT (TYPE_STRUCT+1)
} TypeKind;


typedef enum {
	BUILTIN_I8,
	BUILTIN_I16,
	BUILTIN_I32,
	BUILTIN_I64,
	BUILTIN_U8,
	BUILTIN_U16,
	BUILTIN_U32,
	BUILTIN_U64,
	BUILTIN_F32,
	BUILTIN_F64,
	BUILTIN_CHAR,
	BUILTIN_BOOL,
	BUILTIN_TYPE,
	BUILTIN_VARARGS,
	BUILTIN_NMS
} BuiltinType;


typedef U8 Constness;

#define CONSTNESS_NO ((Constness)0)
#define CONSTNESS_SEMI ((Constness)1)
#define CONSTNESS_YES ((Constness)2)

typedef struct FnType {
	struct Type *types; /* dynamic array [0] = ret_type, [1:] = param_types  */
	Constness *constness; /* [i] = constness of param #i. iff no parameters are constant, this is NULL. don't use it as a dynamic array, because eventually it might not be. */
} FnType;

enum {
	TYPE_IS_FLEXIBLE = 0x01,
	TYPE_IS_RESOLVED = 0x02,
};
typedef U8 TypeFlags;
typedef struct Type {
	TypeKind kind;
	TypeFlags flags;
	union {
		BuiltinType builtin;
		FnType fn;
		struct Type *tuple;
		struct {
			struct Type *of;
			union {
				U64 n; /* after resolving */
				struct Expression *n_expr; /* before resolving */
			};
		} arr;
		struct Type *ptr;
		struct Type *slice;
		struct StructDef *struc; /* multiple resolved types can refer to the same struct */
		struct Expression *expr;
	};
} Type;


/* field of a struct */
typedef struct Field {
	Location where;
	Identifier name;
	Type *type;
	size_t offset; /* offset during compile time */
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
	BLOCK_WHILE
} BlockKind;

typedef U8 BlockFlags;
typedef struct Block {
	/* NOTE: make sure you check copy.c when you add something to this */
	BlockFlags flags;
	BlockKind kind;
	struct {
		IdentID break_lbl, cont_lbl; /* initially 0, set to non-zero values if needed (++g->lbl_counter); set by sdecls_cgen. */
	} c;
	Location where;
	Identifiers idents;
	struct Statement *stmts;
	struct Expression *ret_expr; /* the return expression of this block, e.g. {foo(); 3} => 3  NULL for no expression. */
	struct Block *parent;
	struct Statement **deferred; /* deferred stuff from this block; used by both eval and cgen */
	struct Use **uses; /* use statements (for types.c) */
} Block;
typedef Block *BlockPtr;

enum {
	STRUCT_DEF_FOUND_OFFSETS = 0x01,
	STRUCT_DEF_CGEN_DECLARED = 0x02,
	STRUCT_DEF_CGEN_DEFINED = 0x04,
	STRUCT_DEF_RESOLVED = 0x08,
	STRUCT_DEF_RESOLVING = 0x10	
};
typedef U8 StructFlags;
typedef struct StructDef {
	/* these two only exist after resolving (before then, it's scope.stmts) */
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
			size_t size; /* size of this struct during compile time */
			size_t align;
			U64 instance_id; /* ID of instance */
		};
	};
	Identifier name;
	struct Declaration *params;
	struct {
		/* if name is NULL, use this */
		IdentID id;
	} c;
	StructFlags flags;
} StructDef;


typedef enum {
	EXPR_LITERAL_FLOAT,
	EXPR_LITERAL_INT,
	EXPR_LITERAL_STR,
	EXPR_LITERAL_BOOL,
	EXPR_LITERAL_CHAR,
	EXPR_IDENT, /* variable or constant */
	EXPR_BINARY_OP,
	EXPR_UNARY_OP,
	EXPR_IF,
	EXPR_WHILE,
	EXPR_FOR,
	EXPR_FN,
	EXPR_CAST,
	EXPR_CALL,
	EXPR_BLOCK,
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
	UNARY_ADDRESS, /* &x */
	UNARY_DEREF, /* *x */
	UNARY_NOT, /* !x */
	UNARY_TYPEOF, /* typeof x */
	UNARY_LEN, /* x.len ; replaces BINARY_DOT len when typing  */
	UNARY_DSIZEOF,
	UNARY_DALIGNOF,
	UNARY_SIZEOF,
	UNARY_ALIGNOF
} UnaryOp;

typedef enum {
	BINARY_SET, /* e.g. x = y */
	BINARY_ADD,
	BINARY_SUB,
	BINARY_MUL,
	BINARY_DIV,
	BINARY_MOD,
	BINARY_SET_ADD, /* e.g. x += y */
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
	BINARY_AT_INDEX, /* e.g. x[i] */
	BINARY_DOT
} BinaryOp;

typedef struct CallExpr {
	struct Expression *fn;
	union {
		struct Argument *args; /* before typing */
		struct Expression *arg_exprs; /* after typing */
	};
	struct Instance *instance; /* NULL = ordinary function, no compile time args */
} CallExpr;

enum {
	IF_STATIC = 0x01
};

typedef struct IfExpr {
	U8 flags;
	struct Expression *cond; /* NULL = this is an else */
	struct Expression *next_elif; /* next elif/else of this statement */
	Block body;
} IfExpr;

typedef struct WhileExpr {
	struct Expression *cond;
	Block body;
} WhileExpr;


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
	/* things that can't be unsigned */
	CTYPE_PTR = 0x10,
	CTYPE_FLOAT,
	CTYPE_DOUBLE,
	CTYPE_SIZE_T,
	CTYPE_VARARGS
} CTypeKind;
typedef struct {
	CTypeKind kind;
	char *points_to; /* if kind == CTYPE_PTR, ident string of C type which it points to */
} CType;

typedef struct FnExpr {
	Location where;
	union {
		struct {
			struct Declaration *params; /* declarations of the parameters to this function */
			struct Declaration *ret_decls; /* array of decls, if this has named return values. otherwise, NULL */
			U64 instance_id;
			Type ret_type;	
			Block body;
		};
		struct {
			Type type; /* type of this function */
			CType *ctypes; /* ctypes[i] = CTYPE_NONE if this isn't a ctype, or the specified CType. don't use this as a dynamic array. */
			union {
				const char *name;
				struct Expression *name_expr; /* before typing */
			};
			union {
				const char *lib;
				struct Expression *lib_expr;
			};
			void (*fn_ptr)();
		} foreign;
	};
	HashTable *instances; /* for fns with constant parameters. the key is a tuple where
							 the first element is a u64 value whose ith bit (1<<i) is 1
							 if the ith semi-constant parameter is constant.
							 cgen relies on this being here even for foreign fns.
						  */
	struct {
		/* if name = NULL, this is an anonymous function, and id will be the ID of the fn. */
		Identifier name;
		IdentID id;
	} c;
	U8 flags;
} FnExpr; /* an expression such as fn(x: int) int { 2 * x } */

typedef struct Instance {
	Value val; /* key into hash table */
	union {
		FnExpr *fn; /* the typed function */
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
	struct Expression *n; /* e.g. for new(int, 5) */
} NewExpr;

typedef struct SliceExpr {
	struct Expression *of; /* required */
	struct Expression *from; /* optional */
	struct Expression *to; /* optional */
	struct {
		IdentID id;
	} c;
} SliceExpr;

typedef enum {
	BUILTIN_STDOUT,
	BUILTIN_STDERR,
	BUILTIN_STDIN,
	BUILTIN_SIZEOF_SHORT,
	BUILTIN_TSIZEOF_SHORT, /* target sizeof(short) */
	BUILTIN_SIZEOF_INT,
	BUILTIN_TSIZEOF_INT,
	BUILTIN_SIZEOF_LONG,
	BUILTIN_TSIZEOF_LONG,
	BUILTIN_SIZEOF_LONG_LONG,
	BUILTIN_TSIZEOF_LONG_LONG,
	BUILTIN_SIZEOF_SIZE_T,
	BUILTIN_TSIZEOF_SIZE_T,
	BUILTIN_SIZEOF_FLOAT,
	BUILTIN_TSIZEOF_FLOAT,
	BUILTIN_SIZEOF_DOUBLE,
	BUILTIN_TSIZEOF_DOUBLE,
	BUILTIN_SIZEOF_LONG_DOUBLE,
	BUILTIN_TSIZEOF_LONG_DOUBLE,
	BUILTIN_COMPILING
#define BUILTIN_VAL_COUNT (BUILTIN_COMPILING+1)
} BuiltinVal;

const char *const builtin_val_names[BUILTIN_VAL_COUNT] =
	{"stdout", "stderr", "stdin", "sizeof short", "target sizeof short",
	 "sizeof int", "target sizeof int", "sizeof long", "target sizeof long",
	 "sizeof long long", "target sizeof long long", "sizeof size_t", "target sizeof size_t",
	 "sizeof float", "target sizeof float", "sizeof double", "target sizeof double",
	 "sizeof long double", "target sizeof long double", "compiling"};

typedef struct Namespace {
	Block body;
	Identifier associated_ident; /* if this is foo ::= nms { ... }, then associated_ident is foo; can be NULL. used by cgen. only non-null if the namespace isn't in a non-namespace block */
	struct Namespace *points_to; /* if not NULL, this namespace just points to another namespace, because something has been included twice */
	struct {
		char *prefix; /* generated during sdecls_cgen */
	} c;
} Namespace;


enum {
	EXPR_FOUND_TYPE = 0x01
};


typedef U8 ExprFlags;

typedef struct Expression {
	Type type;
	Location where;
	ExprKind kind : 8;
	ExprFlags flags;
	struct {
		IdentID id; /* cgen ID used for this expression */
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
				Field *field; /* for struct., after resolving */
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
		String ident_str; /* before typing */
		Identifier ident; /* after typing */
		NewExpr new;
		Namespace *nms;
		struct {
			Type type;
		} del;
		IfExpr *if_;
		WhileExpr *while_;
		struct ForExpr *for_;
		FnExpr *fn;
		CastExpr cast;
		SliceExpr slice;
		Block *block;
		struct Expression *tuple; /* dynamic array, even after typing */
		Type *typeval;
		Value val;
	};
} Expression;


typedef struct Argument {
	Location where;
	char *name; /* NULL = no name */
	Expression val;
} Argument;

enum {
	DECL_ANNOTATES_TYPE = 0x0001,
	DECL_IS_CONST = 0x0002,
	DECL_SEMI_CONST = 0x0004,
	DECL_HAS_EXPR = 0x0008,
	DECL_FOUND_TYPE = 0x0010,
	DECL_ERRORED_ABOUT_SELF_REFERENCE = 0x0020, /* has there been an error about this decl referencing itself? */
	DECL_FOUND_VAL = 0x0040,
	DECL_INFER = 0x0080, /* infer the value (e.g. fn(t::Type=, x:t)) */
	DECL_EXPORT = 0x0100,
	DECL_IS_PARAM = 0x0200,
	DECL_USE = 0x0400 /* e.g. use p: Point */
};

typedef U16 DeclFlags;

typedef struct Declaration {
	Location where;
	Identifier *idents;
	Type type;
	DeclFlags flags;
	union {
		Expression expr;
		Field *field; /* pointer to the field which the first identifier in this decl refers to */
		struct {
			union {
				struct {
					/* only exist before typing */
					Expression *name;
					Expression *lib;
				};
				/* only set for non-functions */
				const char *name_str;
			};
		} foreign;
	};
	Value val; /* only for constant decls, non-constant globals, and varargs. */

	/* for eval, for non-constant local decls: */
	/* the pointers to values need to be fixed, which is why this isn't just Value *.  */
	/* @OPTIM: some block array of values somewhere which we can just use a pointer to, which is freed when the block is exited? */
	Value **val_stack;
} Declaration;
typedef Declaration *DeclarationPtr;

enum {
	FOR_IS_RANGE = 0x01
};
enum {
	FN_EXPR_FOREIGN = 0x01,
	FN_EXPR_EXPORT = 0x02, /* set by sdecls_cgen.c */
	FN_EXPR_HAS_VARARGS = 0x04
};
typedef struct ForExpr {
	U8 flags;
	Declaration header;
	Block body;
	union {
		struct {
			struct Expression *from; /* can't be null */
			struct Expression *to; /* can be null */
			union {
				/* (either) can be null */
				struct Expression *step; /* before typing */
				Value *stepval; /* after typing. the type of this is header.type.tuple[0] (i.e. the value type for this for loop),
					NOTE: this might be different from the original ForExpr.step.type, because of implicit type conversions. */
			};
		} range;
		struct Expression *of;
	};
} ForExpr;

typedef enum {
	STMT_DECL,
	STMT_EXPR,
	STMT_RET,
	STMT_BREAK,
	STMT_CONT,
	STMT_INCLUDE,
	STMT_MESSAGE,
	STMT_DEFER,
	STMT_USE
} StatementKind;

enum {
	RET_HAS_EXPR = 0x01
};
typedef struct Return {
	U8 flags;
	Block *referring_to; /* eval uses this; it's the function body we're returning from */
	Expression expr;
} Return;

enum {
	INC_FILE_CGEND_SDECLS = 0x01,
	INC_FILE_CGEND_DECLS = 0x02,
	INC_FILE_CGEND_DEFS = 0x04,
	INC_FILE_CGEND = 0x08
};

typedef struct {
	U8 flags;
	Namespace *main_nms; /* namespace of first inclusion */
	struct Statement *stmts;
} IncludedFile;

enum {
	INC_FORCED = 0x01
};

typedef union {
	U8 flags;
	union {
		Expression filename; /* before typing */
		struct Statement *stmts; /* after typing */
	};
	IncludedFile *inc_file;
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

enum {
	STMT_EXPR_NO_SEMICOLON = 0x01,
	STMT_INC_TO_NMS = 0x01,
	STMT_TYPED = 0x02
};
typedef struct Statement {
	Location where;
	StatementKind kind;
	U8 flags;
	union {
		Declaration *decl; /* we want the pointer to be fixed so that we can refer to it from an identifier */
		Expression *expr;
		Return *ret;
		Include *inc;
		Message *message; /* #error, #warn, #info */
		Block *referring_to; /* for break/continue; set during typing */
		struct Statement *defer;
		Use *use;
	};
} Statement;

typedef Statement *StatementPtr;

typedef struct ParsedFile {
	Statement *stmts;
} ParsedFile;

typedef struct Parser {
	Tokenizer *tokr;
	Allocator *allocr;
	Identifiers *globals;
	File *file;
	Block *block; /* which block are we in? NULL = file scope */
	ParsedFile *parsed_file;
} Parser;

#if COMPILE_TIME_FOREIGN_FN_SUPPORT
typedef struct {
	void *handle;
} Library;
#endif

typedef struct {
#if COMPILE_TIME_FOREIGN_FN_SUPPORT
	StrHashTable libs_loaded; /* of Library */
#else
	char unused;
#endif
} ForeignFnManager;

typedef struct Evaluator {
	Allocator *allocr;
	struct Typer *typer;
    Block *returning; /* function body from which we are returning OR loop body in which we are continuing/breaking */
	bool is_break; /* is returning because of a break, as opposed to a continue? */
	Value ret_val;
	ForeignFnManager ffmgr;
} Evaluator;

typedef struct Typer {
	Allocator *allocr;
	Evaluator *evalr;
	Identifiers *globals;
	Use **uses; /* global used things */
	Declaration **in_decls; /* array of declarations we are currently inside */
	Block *block;
	Block **blocks; /* dyn array of all the block's we're in ([0] = NULL for global scope) */
	FnExpr *fn; /* the function we're currently parsing. */
	ErrCtx *err_ctx;
	ParsedFile *parsed_file;
	Namespace *nms;
	StrHashTable included_files; /* maps to IncludedFile */
	File *file;
	/* 
		have we had an error because we couldn't find a file that was #include'd 
		(so that we can stop compiling immediately)
	*/
	bool had_include_err; 
} Typer;

typedef struct CGenerator {
	Allocator *allocr;
	FILE *outc;
	IdentID ident_counter, lbl_counter;
	U16 indent_lvl; /* how many levels of indentation? */
	bool will_indent; /* will the next thing be indented? */
	ParsedFile *file;
	Block *block;
	Namespace *nms;
	FnExpr *fn; /* which function are we in? (NULL for none) - not used during decls */
	Identifier main_ident;
	Identifiers *globals;
	char const **nms_prefixes; /* dynamic (null-terminated) array of namespace prefixes */
} CGenerator;
