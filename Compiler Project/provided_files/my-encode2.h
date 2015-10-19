#include "types.h"
#include "symtab.h"
#include "tree.h"

#if 0
/* Records the base offset (from %fp) for all local variables in a function */
/* Used inherited attributes instead, passing the offsets around on the
   semantic stack.  Both approaches work. */
extern int base_offset_stack[BS_DEPTH];
extern int bo_top;
#endif


/* Prototypes */

/* If function is called from the grammar, the first line of the comment
   gives the head(s) of the production(s) where it is called, preceded by
   "gram:". */


/* Emits assembly code to allocate labeled space for a global variable.
   Calls b_global_decl() then b_skip() with the appropriate given arguments */
void encode_global_var_decl(char *id, int align, unsigned int size);

/* Wrapper function for b_get_local_var_offset() */
int get_local_var_offset();

/* gram: function_declaration (2nd production -- intermediate action before statement_part
   Prepares for the body of a function: emits code to store formal parameters
   and allocatate space for return value (if any) and local variables.

   Parameters:
      global_func_name: the label to be used for the function entry point;
      type: the type of the function (tag TYFUNC);
      loc_var_offset: the minimum offset of any variable local to the function

   1. Calls b_func_prologue with the global function name
   2. If this is a local function, calls b_store_formal_param(TYPTR) to store
      the reference link.  The offset returned is the same as FUNK_LINK_OFFSET.
   3. For each formal parameter in order, calls b_store_formal_param(tag),
      where tag is the type tag of the parameter (for subranges, use the tag
      of the base type) for value parameters; for VAR parameters, tag should
      be TYPTR.
   4. If this is a Pascal function (non-void return type), calls
      b_alloc_return_value() to allocate space for the return value.
   5. Calls b_alloc_local_vars(size), where size is the distance in bytes
      from loc_var_offset and what get_local_var_offset() returns.  This
      allocates enough space for local variables.
*/
void enter_func_body(char * global_func_name, TYPE type, int loc_var_offset);

/* gram: function_declaration (2nd production -- final reduce action)
   Emits code to end a function body.  Also exits the scope of the function.
   Parameters are the same as the first two parameters of enter_func_body().
   1. Pops the function id (ST_ID) from the global stack of function id's.
   2. For Pascal functions (non-void return type), calls b_prepare_return()
      with the tag of the return type of the function (for subranges, use
      the tag of the base type).  This emits code to move the return value
      to its proper place according the the gcc calling conventions.
   3. Calls b_func_epilogue() with the global function name.
   4. Calls st_exit_block(), which removes all installations in the block
      and reverts back the the previous block.
*/
void exit_func_body(char * global_func_name, TYPE type);

/* gram: main_program_declaration (intermediate action before statement_part)
   Calls b_func_prologue("main") to prepare the entry point of the program.
   Defined here because backend.h is not included in gram.y.
*/
void enter_main_body();

/* gram: main_program_declaration (final reduce action)
   Calls b_func_epilogue("main"). */
void exit_main_body();

/* gram: simple_statement (3rd and 4th productions)
   Emits assembly code to evaluate an expression.  The net result of the
   evaluation is that the value of the expression is pushed onto the control
   stack (%sp decreases by 8 bytes; it is always 8-byte aligned).  There are
   two exceptions to this: nothing is pushed as the result of an assignment
   or of an invoation of New.  Both of these "expressions" are considered
   statements in Pascal, with no resulting value.  The backend routines
   used for these two expressions do push a value onto the stack, and so
   that value must be explicitly popped by calling b_pop().

   The function is a switch on the type of expr node (expr->tag).  For most
   atomic expressions (those with no subexpressions), all that is needed
   is a single call to a backend routine.  For example, for a REALCONST,
   calls b_push_const_double() with the value.  For a GID, calls
   b_push_ext_addr() with the id string.
      For nonatomic expressions (UNOP, BINOP, and FCALL), helper functions
   are called.  Those for UNOP and BINOP include recursive calls to
   encode_expr() for the subexpressions, then switches on the operator.
      The only nullary operator is Nil, which is the same as NULL in C,
   and for this b_push_const_int(0) is called.

   To encode an LVAR, a number of reference links must be followed equal to
   the variable's link_count to find the activation record containing the
   variable (if the variable is declared in the current scope, then this
   value will be zero).  Once the activation record is found, the offset
   is used to find the actual address of the variable, and this address
   is pushed onto the stack.  LVARs can be either local variables or formal
   parameters.  If the LVAR is a value parameter or local variable
   (is_ref == FALSE), then nothing further is done.  Otherwise, the value
   on the stack is then dereferenced (calling b_deref(TYPTR)) to push the
   address of the actual datum onto the stack.  In either case, the net
   result is that the address of the datum is pushed on the stack (this
   is considered an l-value).

   Encoding an FCALL varies somewhat depending on whether the function is
   global (GID) or local (LFUN).
   1. Finds the global name of the function: if GID, then this is the same
      as the function name itself; if LFUN, this is its global_name.
   2. Calculates the size of the argument list: each argument besides double
      adds 4 bytes to the size, and double adds 8 bytes.  If LFUN, then
      a reference link is passed as the first actual argument (shadow
      parameter), which counts for 4 additional bytes (for a pointer).
   3. Calls b_alloc_arglist() with the size calculated in (2).
   4. For each actual argument, calls encode_expr() recursively to push its
      value onto the stack.
      a) For VAR parameters: an l-value is expected and the types must match.
         If so, load the l-value (a pointer) with b_load_arg(TYPTR),
         regardless of the TYPE of the argument.
      b) For non-VAR (i.e., VALUE) parameters: an r-value is expected.  Call
         b_deref() if necessary to convert an l-value into an r-value, then
         promote chars (signed or unsigned) to signed longs and floats to
         doubles, then call b_load_arg() with the resulting type tag.
      In either case, b_load_arg() moves the value on top of the stack to
      is proper location based on the gcc calling conventions.
   5. Calls b_funcall_by_name() with the name and return type tag of the
      function.
*/
void encode_expr(EXPR expr);
