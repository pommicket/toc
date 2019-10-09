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

typedef uint32_t LineNo;
typedef struct {
	LineNo line;
	char *code;
	const char *filename;
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
	union Value *tuple;
} Value;

#define IDECL_FLAG_HAS_VAL 0x01
typedef struct {
	struct Declaration *decl;
	struct Block *scope; /* NULL for file scope */
	Value val;
	uint16_t flags;
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
	uint16_t flags;
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
			  TOKEN_NUM_LITERAL,
			  TOKEN_CHAR_LITERAL,
			  TOKEN_STR_LITERAL,
			  TOKEN_EOF
} TokenKind;

typedef enum {
			  DIRECT_C,
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
			  KW_EQEQ,
			  KW_NE,
			  KW_LT,
			  KW_LE,
			  KW_GT,
			  KW_GE,
			  KW_PLUS,
			  KW_MINUS,
			  KW_ASTERISK,
			  KW_EXCLAMATION,
			  KW_AMPERSAND,
			  KW_SLASH,
			  KW_EQ,
			  KW_LAST_SYMBOL = KW_EQ, /* last one entirely consisting of symbols */
			  KW_IF,
			  KW_ELIF,
			  KW_ELSE,
			  KW_WHILE,
			  KW_RETURN,
			  KW_FN,
			  KW_AS,
			  KW_NEW,
			  KW_DEL,
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
	const char *filename;
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
			  TYPE_PTR
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
			  BUILTIN_TYPE_COUNT
} BuiltinType;

#define TYPE_FLAG_FLEXIBLE 0x01
#define TYPE_FLAG_RESOLVED 0x02

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
		struct {
			struct Type *of;
		} ptr;
	};
} Type;

#define BLOCK_FLAG_FN 0x01

typedef struct Block {
	uint16_t flags;
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
			  EXPR_FN,
			  EXPR_CAST,
			  EXPR_NEW,
			  EXPR_CALL,
			  EXPR_BLOCK,
			  EXPR_TUPLE,
			  EXPR_DIRECT
} ExprKind;

typedef enum {
			  UNARY_MINUS,
			  UNARY_ADDRESS, /* &x */
			  UNARY_DEREF, /* *x */
			  UNARY_NOT, /* !x */
			  UNARY_DEL
} UnaryOp;

typedef enum {
			  BINARY_SET, /* e.g. x = y */
			  BINARY_ADD,
			  BINARY_SUB,
			  BINARY_MUL,
			  BINARY_DIV,
			  BINARY_GT,
			  BINARY_LT,
			  BINARY_GE,
			  BINARY_LE,
			  BINARY_EQ,
			  BINARY_NE,
			  BINARY_AT_INDEX /* e.g. x[i] */
} BinaryOp;

typedef struct {
	Directive which;
	struct Expression *args;
} DirectExpr;


typedef struct {
	struct Expression *fn;
    union {
	    struct Argument *args;
		struct Expression *arg_exprs;
	};
} CallExpr;

typedef struct {
	struct Expression *cond; /* NULL = this is an else */
	struct Expression *next_elif; /* next elif/else of this statement */
	Block body;
} IfExpr;

typedef struct {
	struct Expression *cond;
	Block body;
} WhileExpr;

typedef struct FnExpr {
    struct Declaration *params; /* declarations of the parameters to this function */
    struct Declaration *ret_decls; /* array of decls, if this has named return values. otherwise, NULL */
	Type ret_type;
	Block body;
} FnExpr; /* an expression such as fn(x: int) int { 2 * x } */

typedef struct {
	Type type;
	struct Expression *expr;
} CastExpr;

#define EXPR_FLAG_FOUND_TYPE 0x01

typedef struct Expression {
	Location where;
	ExprKind kind;
	uint16_t flags;
	Type type;
	union {
		Floating floatl;
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
		} binary;
		CallExpr call;
	    DirectExpr direct;
		Identifier ident;
		struct {
			Type type;
		} new;
		struct {
			Type type;
		} del;
		IfExpr if_;
		WhileExpr while_;
		FnExpr fn;
		CastExpr cast;
		Block block;
		struct Expression *tuple;
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

/* OPTIM: Instead of using dynamic arrays, do two passes. */
typedef struct Declaration {
	Location where;
	Identifier *idents;
	Type type;
	uint16_t flags;
	Expression expr;
	Value val; /* only for constant decls. */
} Declaration;

typedef enum {
			  STMT_DECL,
			  STMT_EXPR,
			  STMT_RET
} StatementKind;

#define RET_FLAG_EXPR 0x01
typedef struct {
	uint16_t flags;
	Expression expr;
} Return;

#define STMT_FLAG_VOIDED_EXPR 0x01 /* the "4;" in fn () { 4; } is a voided expression, but the "4" in fn () int { 4 } is not */
typedef struct Statement {
	Location where;
	StatementKind kind;
	unsigned short flags;
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
} Evaluator;

typedef struct Typer {
	Allocator allocr;
	Evaluator *evalr;
	Declaration **in_decls; /* array of declarations we are currently inside */
	Block *block;
	bool can_ret;
	Type ret_type; /* the return type of the function we're currently parsing. */
} Typer;

