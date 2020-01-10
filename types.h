/*
  Copyright (C) 2019, 2020 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
/* NOTE: make sure you edit copy.c and package.c and cgen_recurse_subexprs/types when you make a change to expression-related types or type-related types in this file! */

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
	const char *filename;
	char *str; /* file contents, or NULL if none are available */
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
	void *last; /* last one of them */
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
	struct Package *pkg;
} Value;

enum {
	  IDECL_HAS_VAL = 0x01,
};

typedef enum {
			  IDECL_DECL,
			  IDECL_EXPR
} IdentDeclKind;

typedef struct IdentDecl {
	union {
		struct Declaration *decl;
		struct Expression *expr; /* for example, this identifier is declared in an each expression */
	};
	struct Block *scope; /* NULL for file scope */
	Value val;
	SOURCE_LOCATION
	IdentDeclKind kind;
	U16 flags;
} IdentDecl;

/* 
The way you search an identifier in a tree is:
root.children[low part of 1st char].children[high part of 1st char]
.children[low part of 2nd char]...

*/

#define TREE_NCHILDREN 16
typedef struct IdentTree {
	/* zero value is an empty trie */
	uint16_t depth;
	unsigned char index_in_parent; /* index of this in .parent.children */
	bool export_name; /* is this identifier's name important? */
	bool anonymous; /* is this identifier not part of a tree? */
	U64 export_id; /* 0 if there's no exported identifier here, otherwise unique positive integer associated with this identifier */
	struct Package *pkg; /* NULL if this is not associated with a package */
	struct IdentTree *parent;
	struct IdentTree *children[TREE_NCHILDREN];
	IdentDecl *decls; /* array of declarations of this identifier */
} IdentTree;

typedef IdentTree *Identifier;

typedef struct Identifiers {
	BlockArr trees;
	IdentTree *root;
	U64 nidents;
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
			  KW_DOTDOT,
			  KW_DOT,
			  KW_EQ,
			  KW_LAST_SYMBOL = KW_EQ, /* last one entirely consisting of symbols */
			  KW_IF,
			  KW_ELIF,
			  KW_ELSE,
			  KW_WHILE,
			  KW_EACH,
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
			  KW_PACKAGE,
			  KW_CHAR,
			  KW_BOOL,
			  KW_TRUE,
			  KW_FALSE,
			  KW_PKG,
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
	ErrCtx *ctx;
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
		Identifier ident;
		NumLiteral num;
		char chr;
		StrLiteral str;
	};
} Token;


typedef struct Location {
	/* if start is NULL, simple_location will be used. */
	Token *start;
	union {
		Token *end; /* Exclusive */
	    struct {
			ErrCtx *ctx;
			U32 line;
		} *simple_location;
	};
} Location;


typedef struct Tokenizer {
	Allocator *allocr;
	Token *tokens;
	char *s; /* string being parsed */
	ErrCtx *err_ctx;
	U32 line;
	Token *token; /* token currently being processed */
	Identifiers *idents;
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
			  BUILTIN_PKG
} BuiltinType;

/* field of a struct */
typedef struct Field {
	Identifier name;
	struct Type *type;
	size_t offset; /* offset during compile time */
} Field;


typedef U8 Constness;

#define CONSTNESS_NO ((Constness)0)
#define CONSTNESS_SEMI ((Constness)1)
#define CONSTNESS_YES ((Constness)2)

typedef struct FnType {
	struct Type *types; /* dynamic array [0] = ret_type, [1:] = param_types  */
	Constness *constness; /* [i] = constness of param #i. iff no parameters are constant, this is NULL. don't use it as a dynamic array, because eventually it might not be. */
} FnType;

enum {
	  STRUCT_DEF_FOUND_OFFSETS = 0x01,
	  STRUCT_DEF_CGENERATED = 0x02,
};

typedef struct {
	Field *fields;
	Location where;
	U16 flags;
	size_t size; /* size of this struct during compile time */
	size_t align;
	struct {
		Identifier name;
		IdentID id;
	} c;
	struct {
		U32 id; /* (index into exptr->exported_structs) + 1, or 0 if hasn't been exported */
	} export;
} StructDef;

enum {
	  TYPE_IS_FLEXIBLE = 0x01,
	  TYPE_IS_RESOLVED = 0x02,
};
typedef U16 TypeFlags;
typedef struct Type {
	Location where;
	TypeKind kind;
	TypeFlags flags;
	struct Expression *was_expr; /* if non-NULL, indicates that this type used to be an expression (TYPE_EXPR) */
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
		StructDef *struc; /* it's a pointer so that multiple Types can reference the same struct definition */
		struct Expression *expr;
	};
} Type;

enum {
	  BLOCK_IS_FN = 0x01,
	  BLOCK_FOUND_TYPES = 0x02,
};
typedef struct Block {
	U16 flags;
	Location where;
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
			  EXPR_EACH,
			  EXPR_FN,
			  EXPR_CAST,
			  EXPR_NEW,
			  EXPR_CALL,
			  EXPR_BLOCK,
			  EXPR_TUPLE,
			  EXPR_C,
			  EXPR_DSIZEOF,
			  EXPR_DALIGNOF,
			  EXPR_SLICE,
			  EXPR_TYPE,
			  EXPR_PKG,
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
			  UNARY_LEN	 /* x.len ; replaces BINARY_DOT len when typing  */
} UnaryOp;

typedef enum {
			  BINARY_SET, /* e.g. x = y */
			  BINARY_ADD,
			  BINARY_SUB,
			  BINARY_MUL,
			  BINARY_DIV,
			  BINARY_SET_ADD, /* e.g. x += y */
			  BINARY_SET_SUB,
			  BINARY_SET_MUL,
			  BINARY_SET_DIV,
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
		struct Argument *args;
		struct Expression *arg_exprs;
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
	  EACH_IS_RANGE = 0x01,
	  EACH_ANNOTATED_TYPE = 0x02,
};

typedef struct EachExpr {
	U8 flags;
	struct {
		IdentID id;
	} c;
	Type type;
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
} EachExpr;


typedef struct FnExpr {
	struct Declaration *params; /* declarations of the parameters to this function */
	struct Declaration *ret_decls; /* array of decls, if this has named return values. otherwise, NULL */
	Type ret_type;
	Block body;
	HashTable instances; /* for fns with constant parameters. the key is a tuple where
							the first element is a u64 value whose ith bit (1<<i) is 1
							if the ith semi-constant parameter is constant.
						 */
	struct {
		U32 id; /* (index of function in ex->exported_fns) + 1, 
				   or 0 if this function has not been
				   added to the exporting array yet */
	} export;
	struct {
		/* if name = NULL, this is an anonymous function, and id will be the ID of the fn. */
		Identifier name;
		IdentID id;
	} c;
} FnExpr; /* an expression such as fn(x: int) int { 2 * x } */

typedef struct Instance {
	Value val; /* key into hash table */
	FnExpr fn; /* the typed function */
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

enum {
	  EXPR_FOUND_TYPE = 0x01
};

typedef struct Expression {
	Type type;
	Location where;
	ExprKind kind;
	U16 flags;
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
			Field *field; /* for . only */
		} binary;
		CallExpr call;
		struct {
			struct Expression *code;
		} c;
		struct {
			struct Expression *of;
		} dsizeof; /* #sizeof directive */
		struct {
			struct Expression *of;
		} dalignof; /* #alignof directive */
		Identifier ident;
		NewExpr new;
		struct {
			Type type;
		} del;
		union {
			struct Expression *name_expr;
			Identifier name_ident;
		} pkg;
		IfExpr if_;
		WhileExpr while_;
		EachExpr *each;
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
	Identifier name; /* NULL = no name */
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
	  DECL_IS_PARAM = 0x0080,
	  DECL_INFER = 0x0100, /* infer the value (e.g. fn(t::Type=, x:t)) */
	  DECL_EXPORT = 0x0200
};

typedef U16 DeclFlags;

/* OPTIM: Instead of using dynamic arrays, do two passes. */
typedef struct Declaration {
	Location where;
	Identifier *idents;
	Type type;
	DeclFlags flags;
	Expression expr;
	Value val; /* only for constant decls. */
} Declaration;

typedef enum {
			  STMT_DECL,
			  STMT_EXPR,
			  STMT_RET
} StatementKind;

enum {
	  RET_HAS_EXPR = 0x01,
};
typedef struct Return {
	U8 flags; /* if this changes, go to package.c */
	Expression expr;
} Return;

enum {
	  STMT_EXPR_NO_SEMICOLON = 0x01,
};
typedef struct Statement {
	Location where;
	StatementKind kind;
	U16 flags;
	union {
		Declaration decl;
		Expression expr;
		Return ret;
	};
} Statement;

typedef struct ParsedFile {
	Statement *stmts;
	Expression *pkg_name;
} ParsedFile;

typedef struct Parser {
	Tokenizer *tokr;
	Allocator *allocr;
	Block *block; /* which block are we in? NULL = file scope */
	ParsedFile *file;
} Parser;

typedef enum {
			  DECL_END_SEMICOLON,
			  DECL_END_RPAREN_COMMA,
			  DECL_END_LBRACE_COMMA
} DeclEndKind;

typedef struct Evaluator {
	Allocator *allocr;
	struct Typer *typer;
	bool returning;
	Value ret_val;
	void **to_free; /* an array of data to free for this scope. */
	bool enabled;
} Evaluator;

typedef struct Typer {
	Allocator *allocr;
	Evaluator *evalr;
	Identifiers *idents;
	struct Exporter *exptr;
	Expression **in_expr_decls; /* an array of expressions whose declarations (e.g. each **x := foo**) we are currently inside */
	Declaration **in_decls; /* array of declarations we are currently inside */
	Block *block;
	Block **blocks; /* dyn array of all the block's we're in ([0] = NULL for global scope) */
	FnExpr *fn; /* the function we're currently parsing. */
	char *pkg_name;
	ErrCtx *err_ctx;
	/* for checking for problematic struct circular dependencies */
	bool *is_reference_stack;
} Typer;

typedef struct Package {
	Identifier name;
	Identifiers idents;
} Package;

typedef struct Exporter {
	FILE *out; /* .top (toc package) to output to */
	bool export_locations;
	bool started;
	U64 ident_id;
	FnExpr **exported_fns;
	StructDef **exported_structs;
	Identifier *exported_idents; /* (only those whose names are exported) */
	const char *code;
} Exporter;

typedef struct Importer {
	FILE *in;
	Package *pkg;
	Allocator *allocr;
	Identifier *ident_map; /* [i] = value of identifier with ID i */
	ErrCtx err_ctx;
	Declaration *decls;
} Importer;

typedef struct CGenerator {
	Allocator *allocr;
	FILE *outc;
	IdentID ident_counter;
	int indent_lvl; /* how many levels of indentation? */
	bool will_indent; /* will the next thing be indented? */
	ParsedFile *file;
	Block *block;
	FnExpr *fn; /* which function are we in? (NULL for none) - not used during decls */
	Evaluator *evalr;
	Exporter *exptr;
	Identifier main_ident;
	Identifiers *idents;
} CGenerator;


#ifdef TOC_DEBUG
#define add_ident_decls(b, d, flags) add_ident_decls_(__FILE__, __LINE__, b, d, flags)
#endif
