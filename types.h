typedef int64_t Integer;
typedef uint64_t UInteger;
typedef long double Floating; /* OPTIM: Switch to double, but make sure floating-point literals are right */

#if __STDC_VERSION__ < 201112
/* assume long double has the strictest alignment */
typedef long double max_align_t;
#endif

#define INTEGER_MAX INT64_MAX
#define UINTEGER_MAX UINT64_MAX
#define INTEGER_FMT "%"PRId64
#define UINTEGER_FMT "%"PRIu64

typedef uint8_t U8;
typedef uint16_t U16;
typedef uint32_t U32;
typedef uint64_t U64;

typedef int8_t I8;
typedef int16_t I16;
typedef int32_t I32;
typedef int64_t I64;

typedef float F32;
typedef double F64;

#define F32_FMT "%.16f"
#define F64_FMT "%.16f"

typedef U32 IdentID; /* identifier ID for cgen (anonymous variables) */

typedef uint32_t LineNo;

typedef struct {
	const char *filename;
	bool enabled;
} ErrCtx;

typedef struct {
	LineNo line;
	char *code;
	ErrCtx *ctx;
} Location;

typedef struct Page {
	struct Page *next;
	size_t used; /* number of max_align_t's used, not bytes */
	max_align_t data[];
} Page;

typedef struct {
	Page *first;
	Page *last;
} Allocator;

typedef struct {
	void *data;
	size_t n; /* number of things in this block so far */
    void *last; /* last one of them */
} ArrBlock;

typedef struct {
	size_t item_sz;
	int lg_block_sz;
	/* NOTE: dynamic array tends to over-allocate, so we're using our own */
    ArrBlock *blocks;
} BlockArr;

typedef struct {
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
} Value;

#define IDECL_HAS_VAL 0x01

typedef enum {
			  IDECL_DECL,
			  IDECL_EXPR
} IdentDeclKind;

typedef struct {
	union {
		struct Declaration *decl;
		struct Expression *expr; /* for example, this identifier is declared in an each expression */
	};
	struct Block *scope; /* NULL for file scope */
	Value val;
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
	struct IdentTree *parent;
	struct IdentTree *children[TREE_NCHILDREN];
    IdentDecl *decls; /* array of declarations of this identifier */
} IdentTree;

typedef IdentTree *Identifier;

typedef struct {
	BlockArr trees;
	IdentTree *root;
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
			  DIRECT_COUNT
} Directive;

typedef enum {
			  KW_SEMICOLON,
			  KW_COLON,
			  KW_AT,
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
			  KW_CHAR,
			  KW_BOOL,
			  KW_TRUE,
			  KW_FALSE,
			  KW_COUNT
} Keyword;


typedef enum {
			  NUM_LITERAL_INT,
			  NUM_LITERAL_FLOAT
} NumLiteralKind;

typedef struct {
	NumLiteralKind kind;
	union {
		UInteger intval;
		Floating floatval;
	};
} NumLiteral;

typedef struct {
	char *str;
	size_t len;
} StrLiteral;

/* NOTE: Location is typedef'd in util/err.c */
typedef struct {
	TokenKind kind;
	Location where;
	union {
		Keyword kw;
		Directive direct;
		Identifier ident;
		NumLiteral num;
		char chr;
		StrLiteral str;
	};
} Token;

typedef struct {
	Allocator allocr;
	Token *tokens;
	char *s; /* string being parsed */
	ErrCtx *err_ctx;
	LineNo line;
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
			  TYPE_TYPE,
			  TYPE_USER, /* user-defined type */
			  TYPE_STRUCT
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
			  BUILTIN_BOOL
} BuiltinType;

/* field of a struct */
typedef struct {
	Identifier name;
	struct Type *type;
	size_t offset; /* offset during compile time */
} Field;

#define TYPE_FLAG_FLEXIBLE 0x01
#define TYPE_FLAG_RESOLVED 0x02
#define TYPE_FLAG_STRUCT_FOUND_OFFSETS 0x04

typedef struct Type {
	Location where;
	TypeKind kind;
	uint16_t flags;
	union {
	    BuiltinType builtin;
		struct {
			struct Type *types; /* [0] = ret_type, [1..] = param_types */
		} fn;
		struct Type *tuple;
		struct {
			struct Type *of;
			union {
				UInteger n; /* this is NOT set by parse_type; it will be handled by types.c */
				struct Expression *n_expr;
			};
		} arr;
	    struct Type *ptr;
		struct Type *slice;
		struct {
			union {
				struct {
					struct Declaration *decl;
					int index; /* index in decl */
				};
				Identifier ident;
			};
			
		} user;
		struct {
		    Field *fields;
			size_t size; /* size of this struct during compile time */
		} struc;
	};
} Type;

#define BLOCK_FLAG_FN 0x01
#define BLOCK_FLAG_FOUND_TYPES 0x02
typedef struct Block {
	U16 flags;
	Location start;
	Location end;
	struct Statement *stmts;
	struct Block *parent;
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
			  EXPR_VAL /* a value (it's useful to have this). for now, tuples are not supported. see cgen_set_tuple */
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

typedef struct {
	struct Expression *fn;
    union {
	    struct Argument *args;
		struct Expression *arg_exprs;
	};
	struct {
		IdentID id;
	} c;
} CallExpr;

typedef struct {
	struct Expression *cond; /* NULL = this is an else */
	struct Expression *next_elif; /* next elif/else of this statement */
	struct {
		IdentID id;
	} c;
	Block body;
} IfExpr;

typedef struct {
	struct Expression *cond;
	struct {
		IdentID id;
	} c;
	Block body;
} WhileExpr;


#define EACH_IS_RANGE 0x01
#define EACH_ANNOTATED_TYPE 0x02

typedef struct EachExpr {
	U16 flags;
	struct {
		IdentID id;
	} c;
	Type type;
	Identifier index; /* NULL = no index */
	Identifier value; /* NULL = no value */
	Block body;
	union {
		struct {
			struct Expression *from;
			struct Expression *to;
			union {
				struct Expression *step;
				Value *stepval;
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
	struct {
		/* if name = NULL, this is an anonymous function, and id will be the ID of the fn. */
		Identifier name;
	    IdentID id;
	} c;
} FnExpr; /* an expression such as fn(x: int) int { 2 * x } */

typedef struct {
	Type type;
	struct Expression *expr;
} CastExpr;

typedef struct {
	Type type;
	struct Expression *n; /* e.g. for new(int, 5) */
} NewExpr;

typedef struct {
	struct Expression *of;
	struct Expression *from;
	struct Expression *to;
	struct {
		IdentID id;
	} c;
} SliceExpr;

#define EXPR_FLAG_FOUND_TYPE 0x01

typedef struct Expression {
	Type type;
	Location where;
	ExprKind kind;
	U16 flags;
	union {
		Floating floatl;
		/* Floating floatl; */
		UInteger intl;
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
		IfExpr if_;
		WhileExpr while_;
	    EachExpr each;
		FnExpr fn;
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

#define DECL_FLAG_ANNOTATES_TYPE 0x01
#define DECL_FLAG_CONST 0x02
#define DECL_FLAG_HAS_EXPR 0x04
#define DECL_FLAG_FOUND_TYPE 0x08
#define DECL_FLAG_ERRORED_ABOUT_SELF_REFERENCE 0x10 /* has there been an error about this decl referencing itself? */
#define DECL_FLAG_FOUND_VAL 0x20
#define DECL_FLAG_PARAM 0x40 /* is this a parameter declaration? (needed because parameters are immutable) */

/* OPTIM: Instead of using dynamic arrays, do two passes. */
typedef struct Declaration {
	Location where;
	Identifier *idents;
	Type type;
    U16 flags;
	Expression expr;
	Value val; /* only for constant decls. */
	
	struct {
		IdentID *ids; /* array of IDs used in place of ident names. unfortunately needed for user defined types. this is NOT a dynamic array, but is of length arr_len(idents). */
	} c;
} Declaration;

typedef enum {
			  STMT_DECL,
			  STMT_EXPR,
			  STMT_RET
} StatementKind;

#define RET_HAS_EXPR 0x01
typedef struct {
	uint16_t flags;
	Expression expr;
} Return;

#define STMT_FLAG_VOIDED_EXPR 0x01 /* the "4;" in fn () { 4; } is a voided expression, but the "4" in fn () int { 4 } is not */
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

typedef struct {
	Statement *stmts;
} ParsedFile;

typedef struct {
	Tokenizer *tokr;
	Allocator allocr;
	Block *block; /* which block are we in? NULL = file scope */
} Parser;

typedef enum {
			  DECL_END_SEMICOLON,
			  DECL_END_RPAREN_COMMA,
			  DECL_END_LBRACE_COMMA
} DeclEndKind;

typedef struct {
	Allocator allocr;
	struct Typer *typer;
	bool returning;
	Value ret_val;
	void **to_free; /* an array of data to free for this scope. */
	bool enabled;
} Evaluator;

typedef struct Typer {
	Allocator allocr;
	Evaluator *evalr;
	Expression **in_expr_decls; /* an array of expressions whose declarations (e.g. each **x := foo**) we are currently inside */
	Declaration **in_decls; /* array of declarations we are currently inside */
	Block *block;
	bool can_ret;
	Type ret_type; /* the return type of the function we're currently parsing. */
} Typer;

typedef struct {
	FILE *outc;
	IdentID ident_counter;
	int indent_lvl; /* how many levels of indentation? */
	bool will_indent; /* will the next thing be indented? */
	ParsedFile *file;
	Block *block;
	FnExpr *fn; /* which function are we in? (NULL for none) - not used during decls */
	Expression **anon_fns; /* array of pointers to expressions of anonymous functions */
	Evaluator *evalr;
	Identifier main_ident;
	Identifiers *idents;
} CGenerator;
