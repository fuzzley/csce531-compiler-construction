#ifndef TREE_H
#define TREE_H

#include "defs.h"
#include "types.h"
#include "symtab.h"


/* Some struct types for the semantic stack */

/* Used for nonterminals: optinal_par_id_list, id_list */
typedef struct id_node {
    ST_ID           id;
    struct id_node *next;
} ID_NODE, *ID_LIST;

/* Used for nonterminal: variable_or_function_access_maybe_assignment

   This could be any number of things: a simple variable (identifier) on
   the left-hand side of an assignment; a stand-alone procedure call (with
   no assignment); an expression involving an array index or indirection
   operator (^) indicating an l-value on the left-hand side of an assignment;
   a function name on the left-hand side of an assignment (indicating a
   return value assignment).  All uses should produce an expression, but
   the last one also requires the ST_ID.  We don't know at this production
   what will be used until we see the rest of the statement, so we use this
   structure to pass back all available relevant information.  (If the
   production is to variable_or_function_access_no_id, then the id field
   should be set to NULL).
*/
typedef struct {
    struct exprnode * expr;
    ST_ID id;
} EXPR_ID;

/* Dual purpose:
     PTR_OBJ used for nonterminal: pointer_domain_type (to deal with
       possibly unresolved pointers)
     FUNC_HEAD used for nonterminal: function_heading

   For FUNC_HEAD, we can't st_install a function at the function_heading
   production, because we don't know what kind of function declaration
   it will be used for (a forward declaration, external declaration, or
   actual function definition), so we pass back the relevant information
   so that it can be installed later.
*/
typedef struct {
    ST_ID	id;
    TYPE	type;
} PTR_OBJ, FUNC_HEAD;


/* Structures for syntax tree nodes (EXPR and EXPR_LIST) */

/* Possible expression types (tags) */
typedef enum {
    INTCONST, REALCONST, STRCONST, GID, LVAR, LFUN, NULLOP, UNOP, BINOP,
    FCALL, ERROR
} EXPR_TAG;

/* Possible nullary operators (tags) */
typedef enum {
    NULL_EOF_OP, NULL_EOLN_OP, NIL_OP
} EXPR_NULLOP;

/* Possible unary operators (tags) */
typedef enum {
    CONVERT_OP, DEREF_OP, NEG_OP, ORD_OP, CHR_OP, UN_SUCC_OP, UN_PRED_OP,
    NOT_OP, ABS_OP, SQR_OP, SIN_OP, COS_OP, EXP_OP, LN_OP, SQRT_OP, ARCTAN_OP,
    ARG_OP, TRUNC_OP, ROUND_OP, CARD_OP, ODD_OP, EMPTY_OP, POSITION_OP,
    LASTPOSITION_OP, LENGTH_OP, TRIM_OP, BINDING_OP, DATE_OP, TIME_OP,
    UN_EOF_OP, UN_EOLN_OP, INDIR_OP, UPLUS_OP, NEW_OP, DISPOSE_OP, ADDRESS_OP,
    SET_RETURN_OP
} EXPR_UNOP;

/* Possible binary operators (tags) */
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
    } u;
} EXPR_NODE, * EXPR;


/* Procedure and function prototype directives */
typedef enum { DIR_EXTERNAL, DIR_FORWARD } DIRECTIVE;


/* Records the current function identifier to detect return value assigns */
extern ST_ID func_id_stack[BS_DEPTH];
extern int fi_top;



/**** Prototypes ****/

/* The first line of each comment gives the head(s) of the production(s)
   where the function is called, if any, preceeded by "gram:". */


/* Project 1 */

/* gram: id_list
   Prepends a new ST_ID onto the front of a list of ST_IDs.
   Returns the altered list. */
ID_LIST id_prepend(ST_ID id, ID_LIST list);

/* Reverses a list of ST_IDs.  Returns the reversed list. */
ID_LIST id_reverse(ID_LIST list);

/* gram: array_index_list
   Appends a new index TYPE onto the end of an INDEX_LIST of index TYPEs.
   Returns the altered list. */
INDEX_LIST index_append(INDEX_LIST indices, TYPE type);

/* gram: typename
   Checks that the LEX_ID is already installed in the symbol table as a
   TYPENAME.
   Returns the corresponding TYPE */
TYPE check_typename(ST_ID id);

/* gram: type_definition_part, variable_declaration
   Resolves all currently unresolved pointer types. */
void resolve_ptr_types();

/* gram: type_definition
   Installs the TYPE in the symbol table as a TYPENAME */
void make_type(ST_ID id, TYPE type);

/* gram: array_type
   Checks that object is a data type (i.e., not a procedure or function type)
   and calls ty_build_array, returning the result. */
TYPE make_array(TYPE object, INDEX_LIST indices);

/* gram: optional_procedural_type_formal_parameter_list, optional_par_formal_parameter_list
   Checks that each parameter in the list is a simple type and that there are
   no duplicate id's in the list.
   Returns the parameter list unaltered. */
PARAM_LIST check_params(PARAM_LIST params);

/* gram: procedural_type_formal_parameter, formal_parameter
   Returns a PARAM_LIST built from the given ID_LIST with the type and
   reference info */
PARAM_LIST make_params(ID_LIST ids, TYPE type, BOOLEAN is_ref);

/* gram: procedural_type_formal_parameter_list, formal_parameter_list
   Concatenates the two given PARAM_LISTs into one, returning the result. */
PARAM_LIST param_concat(PARAM_LIST first, PARAM_LIST second);

/* gram: functiontype
   Returns TRUE iff the type is simple (not function, array, struct, union,
   or error). */
BOOLEAN is_simple_type(TYPETAG tag);

/* Returns TRUE iff the type is an ordinal (integral) type.  An ordinal type
   is a nonvoid simple type that is not a pointer, float, double, long double,
   or set  */
BOOLEAN is_ordinal_type(TYPETAG tag);

/* Returns TRUE iff type is suitable for an array index.  For our restricted
   purposes, this means an ordinal type that is not "too long".  chars and
   shorts (signed or unsigned) and enums are not too long, but ints and longs
   are.  Subranges are ok as long as they are nonempty (low <= high). */
BOOLEAN is_index_type(TYPE type);

/* Returns TRUE iff type is a data type, i.e., not a function or procedure
   type (void and error types are also excluded). */
BOOLEAN is_data_type(TYPETAG tag);

/* Returns the size required by a datum of the given type.  Func and error
types return 0. */
unsigned int size_of(TYPE type);


/* Project 2 */

/* gram: optional_par_actual_parameter_list, variable_or_function_access_no_id
   Reverses a list of EXPRs.
   Returns the reversed list. */
EXPR_LIST expr_list_reverse(EXPR_LIST list);

/* gram: actual_parameter_list
   Prepends an EXPR onto the front of an EXPR_LIST.
   Returns the altered EXPR_LIST. */
EXPR_LIST expr_prepend(EXPR expr, EXPR_LIST list);

/* gram: variable_declaration
   Checks the TYPE (must be a data type).
   Behavior then depends on whether this is a global or local var declaration
   (global decls occur when the current block number is <= 1, otherwise
   local decls).
   If global:
      Installs each id in the symbol table as a GDECL with the given TYPE,
      NO_SC, and is_ref FALSE (offset is unused).
   If local:
   1. Computes size and alignment requirement of the given TYPE
   2. Decreases cur_offset to the alignment, if necessary
   3. For each id in the ID_LIST:
      a) decreases cur_offset by the size of the TYPE
      b) installs id as an LDECL with the given TYPE, NO_SC, is_ref FALSE,
         and offset being the cur_offset.
   Returns the altered value of cur_offset.
*/
int process_var_decl(ID_LIST ids, TYPE type, int cur_offset);

/* gram.y: subrange_type
   Checks that both lo and hi are INTCONSTs of the same type
   (ty_test_equality).  Returns the new subrange type (ty_build_subrange). */
TYPE check_subrange(EXPR lo, EXPR hi);

/* gram.y: function_declaration (1st production)
   Installs the id in the symbol table as a GDECL with the given type
   with either NO_SC or EXTERN_SC, depending on the DIRECTIVE.  If external,
   then alter type (which should be a function type) to set the check_args
   flag to FALSE.
*/
void build_func_decl(ST_ID id, TYPE type, DIRECTIVE dir);

/* gram: function_declaration (2nd production, intermediate action before any_delaration_part)
   1. Installs the id as a function with the given function TYPE:
         Calls st_lookup to see if the id is previously installed in the
         current block.  If so, previous decl must be a GDECL of a function
         with the same type (ty_test_equality) and NO_SC as storage class.
         In this case, we just change the tag of the ST_DR from GDECL to
         FDECL.  If the id is not previously installed in this block, we
         install it as a new FDECL with the given TYPE.global_func_name.
         In either case, we also set the global_func_name.  Anything else
         is an error.
   2. Pushes id onto a global stack of function ids.  This will be used to
      detect return assignments within the body of the function.
   3. Calls st_enter_block() to enter the local scope of the function.
   4. Calls b_init_formal_param_offset() to initialize the formal parameter
      offset calculation.
   5. If this is a local function, then the first parameter is the reference
      link (shadow parameter); call b_get_formal_param_offset(TYPTR).
   6. Installs each parameter (in order) as a new PDECL with the given TYPE
      (for subranges, use the base type instead), is_ref value, and offset
      provided by a call to b_get_formal_param_offset(tag), where tag is
      either the type tag of the parameter (for value parameters) or TYPTR
      (for var parameters, i.e., with is_ref == TRUE).  (Var parameters are
      always passed as pointers, regardless of their actual types.)
   7. Calls get_local_var_offset(), which returns the initial offset
      for use in declaring variables local to the function.  If the return
      type of the function is nonvoid, then 8 is subtracted from the offset
      to reserve space for the return value.
   8. The (possibly altered initial offset is returned.
*/
int enter_function(ST_ID id, TYPE type, char * global_func_name);

/* gram: unsigned_number (1st production), predefined_literal (2nd and 3rd productions)
   Returns a new INTCONST node with the given TYPE and value. */
EXPR make_intconst_expr(long val, TYPE type);

/* gram: unsigned_number (2nd production)
   Returns a new REALCONST node (with type ty_build_basic(TYDOUBLE)) with
   the given value. */
EXPR make_realconst_expr(double val);

/* gram: constant_literal (1st production)
   Returns a new STRCONST node (with type pointer to unsigned char) with
   the given str. */
EXPR make_strconst_expr(char * str);

/* gram: variable_or_function_access_maybe_assignment (1st production), variable_access_or_typename (2nd production), variable_or_function_access_no_standard_function (1st production)
   1. Calls st_lookup to find the information for the id (error if none
      or if the id was installed as a TYPENAME).
   2. Returns a new EXPR node with fields filled depending on the ST_DR tag
      (switch statement):
      a) If GDECL, then tag is GID and gid is set
      b) If LDECL or PDECL, then tag is LVAR and
           i) is_ref is TRUE iff id is a var parameter
          ii) link_count is current block number minus block number where
              id was installed (this is tne number of reference links to
              follow to find the activation record containing the variable)
         iii) offset is copied from the ST_DR
      c) If FDECL, then
           i) If id was installed in block <= 1, then same as in (a) above
          ii) Otherwise, tag is LFUN, copy the global_func_name, and
              link_count is as in (b)(iii).
*/
EXPR make_id_expr(ST_ID id);

/* gram: predefined_literal (1st production), standard_functions (2nd production if $2 is NULL)
   Returns a new NULLOP node with given op and TYPE depending on the op:
   For Nil, use void type; for Eof and Eoln (optional) use signed char. */
EXPR make_null_expr(EXPR_NULLOP op);

/* gram: standard_procedure_statement (9th production), signed_primary (2nd production), signed_factor (2nd production), factor (5th and 6th productions -- optional), variable_or_function_access_no_id (5th and 8th productions), standard_functions (1st, 2nd (optional), and 3rd production (if only one parameter))
   Returns a new UNOP node based on the op and the sub(expression):
   1. If op expects an r-value and sub is an l-value, then a DEREF node is
      added to sub to make it an r-value (the only Pascal unary ops that
      expect l-values instead of r-values are the New operator and the
      Address-of operator, which is optional).
   2. The subexpression is unary-converted (the only unary conversions that
      require us adding an explicit node are float->double and
      subrange->base type).  Only r-values are converted.
   3. The rest of the behavior depends on the op (switch statement), including
      error-checking and constant-folding.
   NOTE: String constants of length 1 can be converted to char if necessary.
   This happens, e.g., with the Ord function ("Ord('A')").
*/
EXPR make_un_expr(EXPR_UNOP op, EXPR sub);

/* gram: expression (1st production), simple_expression (2nd production; 3rd, 4th, and 5th productions are optional), term (2nd production; 3rd is optional), standard_functions (3rd production -- if 2 arguments (optional))
   Returns a new BINOP node based on the op and the two subexpressions:
   1. If op expects r-value(s), then DEREF nodes are added as needed (the only
      binary op that expects an l-value is assignment, which expects an
      l-value on the left and an r-value on the right).
   2. Both left and right are unary-converted as above.
   3. Both left and right are binary converted (the only binary conversion
      requiring a convert node is long int->double).
   4. Currently, no binary operations are allowed on string constants except
      = and <> (EQ and NE), so if both subexpressions are string constants,
      they are converted to chars if possible (only if they are length 1).
      If one argument is a string constant and the other is of type char,
      then also try to convert the string constant.
   5. The rest of the behavior depends on the op (switch statement) and the
      typetags of the subexpressions, including error-checking and
      constant-folding (for example, arithmetic ops can't act on nonnumeric
      types).
*/
EXPR make_bin_expr(EXPR_BINOP op, EXPR left, EXPR right);

/* gram: factor (1st production -- if $1 is a function (parameterless call)), variable_or_function_access_no_id (7th production)
   1. Checks that func is of function type
   2. If the check_args flag is FALSE, then (this is likely an external
         function): make r-values of all the arguments and unary-convert them.
         Since the only external functions we are linking with are C functions,
         and C allows more unary conversions than Pascal does, more unary
         conversions are called for here: convert both TYSIGNEDCHAR and
         TYUNSIGNEDCHAR to either TYSIGNEDINT or TYSIGNEDLONGINT (it doesn't
         matter which), as well as TYFLOAT to TYDOUBLE.
      Otherwise, for each argument:
         If the formal parameter is a VAR parameter (is_ref == TRUE), then
         actual arg must be an l-value whose type matches the type of the
         formal parameter (ty_test_equality).
         Otherwise, make the actual arg an r-value, unary-convert it, then
         (try to) convert it to the type of the formal parameter.  If the
         conversion fails, error.  If the number of formal parameters and
         actual arguments differ, then error.
   3. Return a new FCALL node whose type is the return type of the function
      and whose args are the (suitably altered) args.
*/
EXPR make_fcall_expr(EXPR func, EXPR_LIST args);

/* Returns a new ERROR node with type ty_build_basic(TYERROR).  (This is
   used internally in many places to pass back something to signal that an
   error has occurred.  Once this happens, further building of the syntax
   tree can be abandoned by just passing the error node up. */
EXPR make_error_expr();

/* gram: assignment_or_call_statement
   If lhs is a simple identifier, then id is the corresponding ST_ID.
   Case 1 -- rhs is non-NULL: Then this is probably an assignment statement --
      returns a new BINOP with tag ASSIGN_OP and lhs and rhs as operands.
      EXCEPTION: if id is the id of the current function (the one whose body
      we are in), then this is a return value assignment; checks that the
      function has non-void return type, then returns a new UNOP with type
      the return type of the function, and op SET_RETURN_OP.
   Case 2 -- rhs is NULL: Then this is not an assignment and id is ignored.
      If lhs is either New or Dispose, then this is the entire expression
      and so just return lhs.  Otherwise the behavior depends on the lhs tag:
      a) If GID or LFUN, then check that lhs is a Pascal procedure
         (else error), whence this is a procedure call without arguments;
         return a new FCALL node.
      b) If FCALL, then this should be a procedure call (with arguments).
         Check that the type (the return type of the function) tag is void;
	 else error (a Pascal function call cannot stand alone as a
	 statement).  If ok, then return the FCALL node.
      c) Any other tag is an error.
*/
EXPR check_assign_or_proc_call(EXPR lhs, ST_ID id, EXPR rhs);

/* Returns TRUE iff expr is an l-value.  This can be determined from the
   tag of the expr and some other information.  All LVARs are l-values; a
   GID is an l-value iff its type is a data type (rather than a function
   type); the only other l-value is the indirection operator (^) (a UNOP). */
BOOLEAN is_lval(EXPR expr);

/* Deallocates an expression tree.  Subexpressions and other subobjects
   are deallocated recursively, postorder. */
void expr_free(EXPR expr);

/* gram: standard_functions (3rd production)
   Deallocates a list of expressions.  Called by expr_free in the case of
   an FCALL expression.  Calles expr_free for each expr in the list. */
void expr_list_free(EXPR_LIST list);

/* gram: function_declaration (2nd production -- intermediate action ($3))
   If the current block is global ( <= 1), then the string of the id is
   returned unaltered.  Otherwise, a new string is returned, suitable for
   an assembler label, that is guaranteed not to conflict with any other
   Pascal identifier or assembler label.

   The return value should be used as the entry point of the given function,
   and so the id should be that of the function.
*/
char * get_global_func_name(ST_ID id);

#endif
