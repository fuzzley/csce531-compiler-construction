#ifndef TREE_H
#define TREE_H

#include "defs.h"
#include "types.h"
#include "symtab.h"


/* Some struct types for the semantic stack */

typedef struct id_node {
    ST_ID           id;
    struct id_node *next;
} ID_NODE, *ID_LIST;

typedef struct {
    struct exprnode * expr;
    ST_ID id;
} EXPR_ID;

typedef struct {
    ST_ID	id;
    TYPE	type;
} PTR_OBJ, FUNC_HEAD;

typedef struct {
    char * name;
    int    offset;
} NAME_OFFSET;


/* Structures for syntax tree nodes (EXPR and EXPR_LIST) */

typedef enum {
    INTCONST, REALCONST, STRCONST, GID, LVAR, LFUN, NULLOP, UNOP, BINOP,
    FCALL, ERROR
} EXPR_TAG;

typedef enum {
    NULL_EOF_OP, NULL_EOLN_OP, NIL_OP
} EXPR_NULLOP;

typedef enum {
    CONVERT_OP, DEREF_OP, NEG_OP, ORD_OP, CHR_OP, UN_SUCC_OP, UN_PRED_OP,
    NOT_OP, ABS_OP, SQR_OP, SIN_OP, COS_OP, EXP_OP, LN_OP, SQRT_OP, ARCTAN_OP,
    ARG_OP, TRUNC_OP, ROUND_OP, CARD_OP, ODD_OP, EMPTY_OP, POSITION_OP,
    LASTPOSITION_OP, LENGTH_OP, TRIM_OP, BINDING_OP, DATE_OP, TIME_OP,
    UN_EOF_OP, UN_EOLN_OP, INDIR_OP, UPLUS_OP, NEW_OP, DISPOSE_OP, ADDRESS_OP,
    SET_RETURN_OP
} EXPR_UNOP;

typedef enum {
    ADD_OP, SUB_OP, MUL_OP, DIV_OP, MOD_OP, REALDIV_OP, EQ_OP, LESS_OP, LE_OP,
    NE_OP, GE_OP, GREATER_OP, SYMDIFF_OP, OR_OP, XOR_OP, AND_OP, BIN_SUCC_OP,
    BIN_PRED_OP, ASSIGN_OP
} EXPR_BINOP;

/* Used for lists of actual arguments to functions/procedures */
typedef struct exprlistnode {
    struct exprnode * expr;
    struct exprlistnode * next;
} EXPR_LIST_NODE, * EXPR_LIST;

typedef struct exprnode {
    EXPR_TAG tag;
    TYPE type;
    union {
	long intval;
	double realval;
	char * strval;
	ST_ID gid;	/* For global variables and global functions */
	struct {
	    BOOLEAN is_ref;
	    int offset;
	    int link_count;
	} lvar;
	struct {
	    char * global_name;
	    int link_count;
	} lfun;
	struct {
	    EXPR_NULLOP op;
	} nullop;
	struct {
	    EXPR_UNOP op;
	    struct exprnode * operand;
	} unop;
	struct {
	    EXPR_BINOP op;
	    struct exprnode * left, * right;
	} binop;
	struct {
	    struct exprnode * function;
	    EXPR_LIST args;
	} fcall;
    } u;
} EXPR_NODE, * EXPR;


typedef enum { DIR_EXTERNAL, DIR_FORWARD } DIRECTIVE;


/* Records the current function identifier to detect return value assigns */
extern ST_ID func_id_stack[BS_DEPTH];
extern int fi_top;


/**** Prototypes ****/

/* Project 1 */

ID_LIST id_prepend(ST_ID id, ID_LIST list);
ID_LIST id_reverse(ID_LIST list);
INDEX_LIST index_append(INDEX_LIST indices, TYPE type);
TYPE check_typename(ST_ID id);
void resolve_ptr_types();
void make_type(ST_ID id, TYPE type);
TYPE make_array(TYPE object, INDEX_LIST indices);
PARAM_LIST check_params(PARAM_LIST params);
PARAM_LIST make_params(ID_LIST ids, TYPE type, BOOLEAN is_ref);
PARAM_LIST param_concat(PARAM_LIST first, PARAM_LIST second);
BOOLEAN is_simple_type(TYPETAG tag);
BOOLEAN is_ordinal_type(TYPETAG tag);
BOOLEAN is_index_type(TYPE type);
BOOLEAN is_data_type(TYPETAG tag);
unsigned int size_of(TYPE type);

/* Project 2 */

EXPR_LIST expr_list_reverse(EXPR_LIST list);
EXPR_LIST expr_prepend(EXPR expr, EXPR_LIST list);
int process_var_decl(ID_LIST ids, TYPE type, int cur_offset);
TYPE check_subrange(EXPR lo, EXPR hi);
void build_func_decl(ST_ID id, TYPE type, DIRECTIVE dir);
char * enter_function(ST_ID id, TYPE type, int * local_var_offset);
EXPR make_intconst_expr(long val, TYPE type);
EXPR make_realconst_expr(double val);
EXPR make_strconst_expr(char * str);
EXPR make_id_expr(ST_ID id);
EXPR make_null_expr(EXPR_NULLOP op);
EXPR make_un_expr(EXPR_UNOP op, EXPR sub);
EXPR make_bin_expr(EXPR_BINOP op, EXPR left, EXPR right);
EXPR make_fcall_expr(EXPR func, EXPR_LIST args);
EXPR make_error_expr();
EXPR check_assign_or_proc_call(EXPR lhs, ST_ID id, EXPR rhs);
BOOLEAN is_lval(EXPR expr);
void expr_free(EXPR expr);
void expr_list_free(EXPR_LIST list);

#endif
