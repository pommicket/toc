/*
toc's types. Note that although these types are in the public domain,
the code which uses them (i.e. most of the rest of toc) is not.

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

typedef long double Floating; /* OPTIM: Switch to double, but make sure floating-point literals are right */

#if __STDC_VERSION__ < 201112
/* try to find the type with the strictest alignment */
typedef union {
	long double floating;
	void *ptr;
	#if __STDC_VERSION__ >= 199901
	long
	#endif
	long integer;
	void (*fn_ptr)(void);
} MaxAlign;
#else
typedef max_align_t MaxAlign;
#endif

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

#if __STDC_VERSION__ >= 199901
#include <stdbool.h>
#elif defined __cplusplus
#else
typedef U8 bool;
#endif


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

typedef struct ArrBlock {
	void *data;
	size_t n; /* number of things in this block so far */
} ArrBlock;

typedef struct BlockArr {
	size_t item_sz;
	int lg_block_sz;
	ArrBlock *blocks;
} BlockArr;

typedef struct HashTable {
	void *data;
	bool *occupied;
	U64 n;
	U64 cap;
} HashTable;

typedef struct Slice {
	I64 n;
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
	float f32;
	double f64;
	struct FnExpr *fn;
	void *arr;
	void *ptr;
	void *struc;
	union Value *tuple;
	Slice slice;
	struct Type *type;
	struct Namespace *nms;
} Value;

typedef enum {
			  IDECL_NONE,
			  IDECL_DECL,
			  IDECL_FOR
} IdentDeclKind;


typedef struct IdentSlot {
	char *str;
	size_t len;
	/* where this identifier was declared */
	IdentDeclKind decl_kind;	
	union {
		struct Declaration *decl;
		struct ForExpr *for_;
	};
	struct Identifiers *idents;
	SOURCE_LOCATION
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
			  DIRECT_COUNT
} Directive;

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
			  KW_FN,
			  KW_AS,
			  KW_NEW,
			  KW_DEL,
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
			  KW_COUNT
} Keyword;


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

typedef struct StrLiteral {
	char *str;
	size_t len;
} StrLiteral;

typedef struct {
	U32 line;
	U32 start; /* index in ctx->str */
	U32 end; /* exclusive */
} SourcePos;

/* NOTE: Location is typedef'd in util/err.c */
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
} File;

typedef struct Location {
	File *file;
	/* if start is NULL, simple_location will be used. */
	Token *start;
	union {
		Token *end; /* Exclusive */
	    struct {
		    U32 line; /* if 0, this is a null location */
		} simple_location;
	};
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
			  TYPE_VOID,
			  TYPE_UNKNOWN,
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
	Location where;
	struct Expression *was_expr; /* if non-NULL, indicates that this type used to be an expression (TYPE_EXPR) */
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
		struct StructDef *struc; /* it's a pointer so that multiple Types can reference the same struct definition */
		struct Expression *expr;
	};
} Type;


/* field of a struct */
typedef struct Field {
	Identifier name;
	Type type;
	size_t offset; /* offset during compile time */
} Field;

enum {
	  STRUCT_DEF_FOUND_OFFSETS = 0x01,
	  STRUCT_DEF_CGEN_DECLARED = 0x02,
	  STRUCT_DEF_CGEN_DEFINED = 0x04
};

typedef struct StructDef {
	Field *fields;
	Location where;
	U8 flags;
	size_t size; /* size of this struct during compile time */
	size_t align;
	Identifier name;
	struct {
		/* if name is NULL, use this */
		IdentID id;
	} c;
} StructDef;


enum {
	  BLOCK_IS_FN = 0x01,
	  BLOCK_IS_NMS = 0x02,
	  BLOCK_FOUND_TYPES = 0x04
};
typedef struct Block {
	U8 flags;
	Location where;
	Identifiers idents;
	struct Statement *stmts;
	struct Expression *ret_expr; /* the return expression of this block, e.g. {foo(); 3} => 3  NULL for no expression. */
} Block;

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
			  EXPR_NEW,
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
			  UNARY_DEL, /* del x */
			  UNARY_LEN, /* x.len ; replaces BINARY_DOT len when typing  */
			  UNARY_DSIZEOF,
			  UNARY_DALIGNOF
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
	struct {
		IdentID id;
	} c;
} CallExpr;

typedef struct IfExpr {
	struct Expression *cond; /* NULL = this is an else */
	struct Expression *next_elif; /* next elif/else of this statement */
	struct {
		IdentID id;
	} c;
	Block body;
} IfExpr;

typedef struct WhileExpr {
	struct Expression *cond;
	struct {
		IdentID id;
	} c;
	Block body;
} WhileExpr;


enum {
	  FOR_IS_RANGE = 0x01,
	  FOR_ANNOTATED_TYPE = 0x02,
};

typedef struct ForExpr {
	U8 flags;
	struct {
		IdentID id;
	} c;
	Type type; /* uninitialized unless typed or flags & FOR_ANNOTATED_TYPE */
	Identifier index; /* NULL = no index */
	Identifier value; /* NULL = no value */
	Block body;
	union {
		struct {
			struct Expression *from; /* can't be null */
			struct Expression *to; /* can be null */
			union {
				/* (either) can be null */
				struct Expression *step; /* before typing */
				Value *stepval; /* after typing */
			};
		} range;
		struct Expression *of;
	};
	Value **val_stack; /* see Declaration for comments */
} ForExpr;


enum {
	  FN_EXPR_FOREIGN = 0x01,
	  FN_EXPR_EXPORT = 0x02 /* set by sdecls_cgen.c */
};

typedef struct FnExpr {
	struct Declaration *params; /* declarations of the parameters to this function */
	struct Declaration *ret_decls; /* array of decls, if this has named return values. otherwise, NULL */
	Location where;
	Type ret_type;	
	union {
		Block body;
		struct {
			const char *name;
			const char *lib;
			void (*fn_ptr)();
		} foreign;
	};
	HashTable instances; /* for fns with constant parameters. the key is a tuple where
							the first element is a u64 value whose ith bit (1<<i) is 1
							if the ith semi-constant parameter is constant.
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
	FnExpr *fn; /* the typed function */
	struct {
		U64 id;
	} c;
} Instance;

typedef struct CastExpr {
	struct Expression *expr;
	Type type;
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
	Identifier associated_ident; /* if this is foo ::= nms { ... }, then associated_ident is foo; can be NULL */
	struct {
		IdentID id; /* used as prefix if prefix is NULL */
	} c;
} Namespace;

enum {
	  EXPR_FOUND_TYPE = 0x01
};

typedef struct Expression {
	Type type;
	Location where;
	ExprKind kind;
	U8 flags;
	union {
		Floating floatl;
		/* Floating floatl; */
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
			struct Expression *rhs;
			union {
				Field *field; /* for struct. */
				Identifier translated_ident; /* for nms. */
			} dot;
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
		Identifier ident;
		NewExpr new;
		Namespace nms;
		struct {
			Type type;
		} del;
		IfExpr if_;
		WhileExpr while_;
		ForExpr *for_;
		FnExpr *fn;
		CastExpr cast;
		SliceExpr slice;
		struct {
			Block block;
			IdentID block_ret_id;
		};
		struct Expression *tuple;
		Type typeval;
		struct {
			Value val;
			IdentID val_c_id;
		};
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
	  DECL_FOREIGN = 0x0200
};

typedef U16 DeclFlags;

typedef struct Declaration {
	Location where;
	Identifier *idents;
	Type type;
	DeclFlags flags;
	union {
		Expression expr;
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
	union {
		Value val; /* only for constant decls. */

		/* for eval, for non-constant decls.: */
		/* the pointers to values need to be fixed, which is why this isn't just Value *.  */
		/* OPTIM: some freeable block array of values somewhere which we can just use a pointer to? */
		Value **val_stack;
	};	
} Declaration;

typedef enum {
			  STMT_DECL,
			  STMT_EXPR,
			  STMT_RET,
			  STMT_INCLUDE
} StatementKind;

enum {
	  RET_HAS_EXPR = 0x01,
};
typedef struct Return {
	U8 flags;
	Expression expr;
} Return;

enum {
	  STMT_EXPR_NO_SEMICOLON = 0x01,
	  STMT_TYPED = 0x02
};
typedef union {
	Expression filename; /* before typing */
	struct Statement *stmts; /* after typing */
} Include;

typedef struct Statement {
	Location where;
	StatementKind kind;
	U8 flags;
	union {
		Declaration *decl; /* we want the pointer to be fixed so that we can refer to it from an identifier */
		Expression expr;
		Return ret;
		Include inc;
	};
} Statement;

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

typedef enum {
			  DECL_END_SEMICOLON,
			  DECL_END_RPAREN_COMMA,
			  DECL_END_LBRACE_COMMA
} DeclEndKind;

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
	bool returning;
	Value ret_val;
	void **to_free; /* an array of data to free for this scope. */
	bool enabled;
	ForeignFnManager ffmgr;
} Evaluator;

typedef struct Typer {
	Allocator *allocr;
	Evaluator *evalr;
	Identifiers *globals;
	ForExpr **in_fors; /* which fors we are currently inside the header of */
	Declaration **in_decls; /* array of declarations we are currently inside */
	Block *block;
	Block **blocks; /* dyn array of all the block's we're in ([0] = NULL for global scope) */
	FnExpr *fn; /* the function we're currently parsing. */
	ErrCtx *err_ctx;
	/* for checking for problematic struct circular dependencies */
	bool *is_reference_stack;
	ParsedFile *parsed_file;
	Namespace *nms;
} Typer;

typedef struct CGenerator {
	Allocator *allocr;
	FILE *outc;
	IdentID ident_counter;
	int indent_lvl; /* how many levels of indentation? */
	bool will_indent; /* will the next thing be indented? */
	ParsedFile *file;
	Block *block;
	Namespace *nms;
	FnExpr *fn; /* which function are we in? (NULL for none) - not used during decls */
	Evaluator *evalr;
	Identifier main_ident;
	Identifiers *globals;
	char *nms_prefix; /* dynamic (null-terminated) array of characters, the current namespace C prefix (e.g. "foo__bar__") */
} CGenerator;
