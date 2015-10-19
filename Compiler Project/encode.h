#ifndef ENCODE_H
#define ENCODE_H
#include <stdlib.h>
#include <stdio.h>
#include "backend-x86.h"
#include "tree.h"

#if 1
/* Records the base offset (from %fp) for all local variables in a function */
extern int base_offset_stack[BS_DEPTH];
extern int bo_top;

/* keep track of labels in control flow (mostly "end/exit" labels) */
extern char *ctrl_flow_labels[200];
extern int ctrl_flow_labels_top;

extern CASE_RECORD case_records[200];
extern int case_records_top;
#endif

//Project part 1

int get_type_alignment(TYPE type);
int get_type_size(TYPE type);
int get_simple_typetag_size(TYPETAG tag);

//Project part 2

//proxy functions
void enter_main_body();
void exit_main_body();
int get_local_var_offset();
int get_formal_param_offset(TYPETAG tag);

//encoding
void encode_expr(EXPR expr);
void encode_unop_expr(EXPR_UNOP op, EXPR expr);
void encode_binop_expr(EXPR_BINOP op, EXPR expr);
void encode_fcall_expr(EXPR func, EXPR_LIST args);
void encode_array_access_expr(EXPR expr, EXPR_LIST expr_list);
//function/procedure
void enter_func_body(char *global_func_name, TYPE type, int loc_var_size);
void exit_func_body(char *global_func_name, TYPE type);
int get_local_var_offset();
/*
void encode_global_var_decl(char *id, int align, unsigned int size);
*/

//Project part 3
void encode_dispatch(VAL_LIST vals, char *label);
char *encode_for_loop(EXPR identifier, EXPR from_limit, int direction, EXPR to_limit, char *exit_lbl);

#endif
