#include "types.h"
#include "symtab.h"
#include "tree.h"

#if 0
/* Records the base offset (from %fp) for all local variables in a function */
extern int base_offset_stack[BS_DEPTH];
extern int bo_top;
#endif

void encode_global_var_decl(char *id, int align, unsigned int size);
int get_local_var_offset();
void enter_func_body(char * global_func_name, TYPE type, int loc_var_size);
void exit_func_body(char * global_func_name, TYPE type);
void enter_main_body();
void exit_main_body();
void encode_expr(EXPR expr);
