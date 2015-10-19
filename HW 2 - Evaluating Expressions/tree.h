#include <stdbool.h>

typedef enum {
	TN_UNOP,
	TN_BINOP,
	TN_CONST,
	TN_VAR
} tn_type;

typedef enum {
	U_PLUS,
	U_MINUS
} unop_type;

typedef enum {
	BI_PLUS,
	BI_MINUS,
	BI_MULTIPLY,
	BI_DIVISION,
	BI_MODULO
} binop_type;

typedef enum {
	TN_EVALUATED,
	TN_EVALUATING,
	TN_UNEVALUATED
} tn_val_state;

typedef struct tn {
	tn_type type;

	union {
		int const_val;
		char var_name;
		struct {
			unop_type op; //+,-
			struct tn *right;		
		} unop;
		struct {
			binop_type op; //+,-,*,/,%
			struct tn *left, *right;
		} binop;
	} info;

	int cached_val;
	tn_val_state cached_val_state;

} TREE_NODE, *TN;

TN make_binop_node(binop_type, TN, TN);
TN make_unop_node(unop_type, TN);
TN make_const_node(int);
TN make_var_node(char);
