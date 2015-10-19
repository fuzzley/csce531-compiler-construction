#ifndef TREE_H
#define TREE_H

#include "defs.h"
#include "types.h"
#include "symtab.h"

//Records the current function identifier to detect return value assigns
extern ST_ID func_id_stack[BS_DEPTH];
extern int fi_top;

//Reference to stack variables declared in gram.y
extern int base_offset_stack[BS_DEPTH];
extern int bo_top;

//keep track of labels in control flow (mostly "end/exit" labels)
extern char *ctrl_flow_labels[200];
extern int ctrl_flow_labels_top;

/* Some struct types for the semantic stack */
typedef struct id_node {
    ST_ID           id;
    struct id_node *next, *prev;
} ID_NODE, *ID_LIST;

typedef struct {
    ST_ID	id;
    TYPE	type;
} PTR_OBJ, FUNC_HEAD;

typedef struct {
    struct exprnode * expr;
    ST_ID id;
} EXPR_ID;

typedef struct {
    char * name;
    int    offset;
} NAME_OFFSET;

typedef struct val_node {
    long lo, hi;
    TYPETAG type;
    struct val_node *prev, *next;
} VAL_LIST_REC, *VAL_LIST;

/* Structures for syntax tree nodes (EXPR and EXPR_LIST) */
typedef enum {
    INTCONST, REALCONST, STRCONST, GID, LVAR, LFUN, NULLOP, UNOP, BINOP,
    FCALL, ERROR, ARRAY_ACCESS //MODIFIED (added ARRAY_ACCESS)
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
    struct exprnode *expr;
    struct exprlistnode *next, *prev;
} EXPR_LIST_NODE, *EXPR_LIST;

/* The syntax tree node for an expression
   (includes the type of the expression)
*/
typedef struct exprnode {
    EXPR_TAG tag;
    TYPE type;
    union {
	long intval;
	double realval;
	char * strval;
	ST_ID gid;	    /* For global variables and global functions */
	struct {            /* For local variables and formal parameters */
	    BOOLEAN is_ref; /* TRUE if expr is a VAR (reference) parameter */
	    int offset;     /* storage location relative to frame pointer */
	    int link_count; /* Number of ref links to follow to find the var */
	} lvar;
	struct {            /* For local functions */
	    char * global_name; /* The assembler entry point label */
	    int link_count; /* Number of ref links to follow to find the fcn */
	} lfun;
	struct {            /* For nullary operators */
	    EXPR_NULLOP op;
	} nullop;
	struct {            /* For unary operators */
	    EXPR_UNOP op;
	    struct exprnode * operand;
	} unop;
	struct {            /* For binary operators */
	    EXPR_BINOP op;
	    struct exprnode * left, * right;
	} binop;
	struct {            /* For procedure and function calls */
	    struct exprnode * function;
	    EXPR_LIST args;
	} fcall;
	struct {
	    struct exprnode * gid;
	    EXPR_LIST index_list;
	} array_access;
    } u;
} EXPR_NODE, *EXPR;

typedef enum { DIR_EXTERNAL, DIR_FORWARD } DIRECTIVE;

typedef struct {
    TYPETAG	type;
    char * 	label;
    VAL_LIST	values;
} CASE_RECORD;

/* Records the current function identifier to detect return value assigns */
extern ST_ID func_id_stack[BS_DEPTH];
extern int fi_top;

/**** Prototypes ****/

/* Project 1 */

//id list
ID_LIST id_create_node(ST_ID id);
INDEX_LIST index_get_head(INDEX_LIST list);
ID_LIST id_append_id(ID_LIST list, ST_ID id);

//param
PARAM_LIST param_create_node(ID_LIST id_list, TYPE type, BOOLEAN is_ref);
PARAM_LIST param_get_head(PARAM_LIST list);
PARAM_LIST param_append_param(PARAM_LIST list, PARAM_LIST param);

//index
INDEX_LIST index_create_node(TYPE type);
INDEX_LIST index_append_type(INDEX_LIST list, TYPE type);

//create/install structures
void make_type(ST_ID id, TYPE type);
void make_var(ID_LIST list, TYPE type);
TYPE make_array(INDEX_LIST index_list, TYPE type);
TYPE make_func(PARAM_LIST list, TYPE return_type);
TYPE make_subrange(EXPR lo, EXPR hi);

//utils
void resolve_unresolved_types();

/* Project 2 */

//expression list
EXPR_LIST expr_get_head(EXPR_LIST list);
EXPR_LIST expr_prepend(EXPR_LIST list, EXPR expr);
EXPR_LIST expr_append(EXPR_LIST list, EXPR expr);
void expr_free(EXPR expr);
void expr_list_free(EXPR_LIST list);

//make expression node (by type)
////simple types
EXPR make_error_expr();
EXPR make_intconst_expr(long val, TYPE type);
EXPR make_realconst_expr(double val);
EXPR make_sign_number_expr(EXPR_UNOP op, EXPR num);
EXPR make_id_expr(ST_ID id);
EXPR make_strconst_expr(char *str);
EXPR make_null_expr(EXPR_NULLOP op);
////more complicated types
EXPR make_convert_expr(EXPR sub_expr, TYPE type);
EXPR make_un_expr(EXPR_UNOP op, EXPR sub);
EXPR make_bin_expr(EXPR_BINOP op, EXPR left, EXPR right);
EXPR make_fcall_expr(EXPR func, EXPR_LIST args);
EXPR expr_fold_unop(EXPR expr);
EXPR expr_fold_binop(EXPR expr);


//defining functions
char *get_global_func_name(ST_ID id);
void build_func_decl(ST_ID id, TYPE type, DIRECTIVE directive);
int process_var_decl(ID_LIST ids, TYPE type, int cur_offset);
int enter_function(ST_ID id, TYPE type, char * global_func_name); //AG

//utilities
EXPR check_assign_or_proc_call(EXPR left, ST_ID id, EXPR right);
EXPR check_assign(EXPR assign);
EXPR upcast_int_expr(EXPR expr);
BOOLEAN is_lval(EXPR expr);
EXPR expr_fold_unop(EXPR expr);
EXPR expr_fold_binop(EXPR expr);

/* Project 3 */

//keep track of case elements
extern CASE_RECORD case_records[200];
extern int case_records_top;
VAL_LIST new_case_value(TYPETAG type, long lo, long hi);
BOOLEAN check_case_values(TYPETAG type, VAL_LIST vals, VAL_LIST prev_vals);
BOOLEAN get_case_value(EXPR expr, long *val, TYPETAG *type);
EXPR make_array_access_expr(EXPR expr, EXPR_LIST expr_list);
BOOLEAN check_for_loop_errors(EXPR identifier, EXPR from_limit, EXPR to_limit);

//utilities
VAL_LIST val_get_tail(VAL_LIST val);
BOOLEAN is_bool(EXPR expr);
BOOLEAN check_case_const(VAL_LIST vals, TYPETAG tag);

#endif
