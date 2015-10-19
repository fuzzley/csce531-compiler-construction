#include <stdlib.h>
#include <stdio.h>
#include "encode.h"
#include "backend-x86.h"

//Project part 1

int get_type_alignment(TYPE type) //align size
{
  INDEX_LIST list;
  long low, high;
  TYPETAG tag;
  int alignment;
  
  //get tag for this type
  tag = ty_query(type);
  
  //start off by assuming it's a simple type
  alignment = get_simple_typetag_size(tag);
  
  //handle special cases (array, subrange)
  if (tag == TYARRAY) {
    //array's alignment is the element type size
    alignment = get_type_alignment(ty_query_array(type, &list));
  }
  if (tag == TYSUBRANGE) {
    //subrange's alignment is the base type size
      alignment = get_type_alignment(ty_query_subrange(type, &low, &high));
  }
  
  return alignment;
}

int get_type_size(TYPE type) //skip size
{
  INDEX_LIST list;
  long low, high;
  TYPETAG tag;
  int size;
  
  //get tag for this type
  tag = ty_query(type);
  
  //start off by assuming it's a simple type
  size = get_simple_typetag_size(tag);
  
  //handle special cases (array, subrange)
  if (tag == TYARRAY) {
    //array's size is the (base type size * #num entries)
    TYPE elem_type;
    
    //find element's size
    elem_type = ty_query_array(type, &list);
    size = get_type_size(elem_type);
    
    //go through list of indeces to find out how many entries are in each INDEX_LIST
    while (list != NULL) {
      if (list->type == NULL) {
	bug("INDEX_LIST's type should not be NULL in get_type_size.");
      }
      if (ty_query(list->type) != TYSUBRANGE) {
	bug("INDEX_LIST's type should be a subrange (but it is not) in get_type_size.");
      }
      
      //assume type is a subrange
      ty_query_subrange(list->type, &low, &high); 
       //find actual range of type and multiply through size
      size *= (high - low + 1);
      list = list->next;
    }
  }
  if (tag == TYSUBRANGE) {
    //subrange's size is the base type size
      size = get_type_size(ty_query_subrange(type, &low, &high));
  }
  
  return size;
}

int get_simple_typetag_size(TYPETAG tag)
{
  //size depends on tag
  switch (tag) {
    //TYARRAY and TYSUBRANGE aren't "simple" so return invalid number
    case TYARRAY:
    case TYSUBRANGE:
      return -1;
      break;
    //the rest are all "simple" types
    case TYPTR:
      return sizeof(char *);
      break;
    case TYDOUBLE:
      return sizeof(double);
      break;
   case TYFLOAT:
    return sizeof(float);
    break;
   case TYLONGDOUBLE:
    return sizeof(long double);
    break;
   case TYSIGNEDLONGINT:
    return sizeof(signed long int);
    break;
   case TYSIGNEDSHORTINT:
    return sizeof(signed short int);
    break;
   case TYSIGNEDINT:
    return sizeof(signed int);
    break;
   case TYUNSIGNEDLONGINT:
    return sizeof(unsigned long int);
    break;
   case TYUNSIGNEDSHORTINT:
    return sizeof(unsigned short int);
    break;
   case TYUNSIGNEDINT:
    return sizeof(unsigned int);
    break;
   case TYUNSIGNEDCHAR:
    return sizeof(unsigned char);
    break;
   case TYSIGNEDCHAR:
    return sizeof(signed char);
    break;
  }
}

//Project part 2

//proxy functions
void enter_main_body()
{
  b_func_prologue("main");
}

void exit_main_body()
{
  b_func_epilogue("main");
}

int get_local_var_offset()
{
  return b_get_local_var_offset();
}

int get_formal_param_offset(TYPETAG tag)
{
  return b_get_formal_param_offset(tag);
}

//encoding
void encode_expr(EXPR expr)
{
   int i;
      
  if (expr == NULL) {
    bug("Encoded expression is null in encode_expr");
  }
  
  //every thing depends on the type of expression (tag)
  switch (expr->tag) {
    //a few types do nothing
    case LFUN:
    case ERROR:
      break;
    //the rest need some work done
    case INTCONST:
      //push the value
      b_push_const_int(expr->u.intval);
      
      //convert up, if necessary
      if (ty_query(expr->type) == TYUNSIGNEDCHAR) {
	b_convert(TYSIGNEDLONGINT, TYUNSIGNEDCHAR);
      }
      break;
    case REALCONST:
      //push the value
      b_push_const_double(expr->u.realval);
      break;
    case STRCONST:
      //push the value
      b_push_const_string(expr->u.strval);
      break;
    case GID:
      //push the literal string associated with the id
      b_push_ext_addr(st_get_id_str(expr->u.gid));
      break;
    case LVAR: //100%
      //reset local address
      b_push_loc_addr(0);

      //offset for each link
      i = 0;
      for (i = 0; i < expr->u.lvar.link_count; i++) {
	b_offset(FUNC_LINK_OFFSET);
	b_deref(TYPTR);
      }

      //offset for lvar
      b_offset(expr->u.lvar.offset);

      //check for final reference var
      if (expr->u.lvar.is_ref == TRUE) {
	b_deref(TYPTR);
      }
      break;
    case NULLOP:
      //push the value (0) to indicate null
      b_push_const_int(0);
      break;
    //the rest are more complicated and require their own functions
    case UNOP:
      encode_unop_expr(expr->u.unop.op, expr);
      break;
    case BINOP:
      encode_binop_expr(expr->u.binop.op, expr);
      break;
    case FCALL:
      encode_fcall_expr(expr->u.fcall.function, expr->u.fcall.args);
      break;
    case ARRAY_ACCESS:
      encode_array_access_expr(expr->u.array_access.gid, expr->u.array_access.index_list);
      break;
  }
}

void encode_unop_expr(EXPR_UNOP op, EXPR expr)
{
  long low, high;
  TYPE baseT;
  TYPETAG tag, rval_tag;
  ST_ID id;
  TYPE type, base_type;
  BOOLEAN converted_to_int;
  
  //make sure operand is encoded first (could be long subtree)
  encode_expr(expr->u.unop.operand);

  //fill in helper vars
  converted_to_int = FALSE;
  tag = ty_query(expr->u.unop.operand->type);
  rval_tag = ty_query(expr->type);
  
  //specific actions, based on op
  switch (op) {
    //ops that do nothing
    case INDIR_OP:
    case UPLUS_OP:
      break;
    //ops that need work
    case CONVERT_OP:
      //some special cases first (subrange, pointer)
      if (tag == TYSUBRANGE) { //subrange
	base_type = ty_query_subrange(expr->u.unop.operand->type, &low, &high);
	b_convert(TYSUBRANGE, ty_query(base_type));
      } else if (tag == TYPTR) { //pointer (do nothing)
	//nothing
      } else { //otherwise just convert to rval type
	b_convert(tag, rval_tag);
      }
      break;
    case NEG_OP:
      b_negate(tag);
      break;
    case ORD_OP:
      if (tag == TYUNSIGNEDCHAR) {
	b_convert(tag, TYSIGNEDLONGINT);
      }
      break;
    case CHR_OP:
      if (tag == TYSIGNEDLONGINT) {
	b_convert(tag, TYUNSIGNEDCHAR);
      }
      break;
    case UN_SUCC_OP:
      //convert to int, if necessary
      if (tag != TYSIGNEDLONGINT) {
	b_convert(tag, TYSIGNEDLONGINT);
	converted_to_int = TRUE;
      }
      
      //push int (1) and specify add operation
      b_push_const_int(1);
      b_arith_rel_op(B_ADD, TYSIGNEDLONGINT);

      //if expression was converted earlier, convert back
      if (converted_to_int == TRUE) {
	b_convert(TYSIGNEDLONGINT, tag);
      }
      break;
    case UN_PRED_OP:
      //convert to int, if necessary
      if (tag != TYSIGNEDLONGINT) {
	b_convert(tag, TYSIGNEDLONGINT);
	converted_to_int = TRUE;
      }
      
      //push int (-1) and specify add operation
      b_push_const_int(-1);
      b_arith_rel_op(B_ADD, TYSIGNEDLONGINT);

      //if expression was converted earlier, convert back
      if (converted_to_int == TRUE) {
	b_convert(TYSIGNEDLONGINT, tag);
      }
      break;
    case NEW_OP:
      //start allocating for arg list
      b_alloc_arglist(4);

      //add size of pointer's type (the type of the type it's pointing to)
      b_push_const_int(get_type_size(ty_query_ptr(expr->u.unop.operand->type, &id, &type)));

      //load the argument
      b_load_arg(TYUNSIGNEDINT);
      
      //call malloc (to allocate memory for ptr type)
      b_funcall_by_name("malloc", TYPTR);

      //assign pointer to allocated memory
      b_assign(TYPTR);
      //pop stack
      b_pop();
      break;
    case DISPOSE_OP:
      //load the argument
      b_load_arg(TYPTR);

      //call free (to free memory for ptr type)
      b_funcall_by_name("free", TYVOID);
      break;
    case DEREF_OP:
      //just do a deref operation
      b_deref(tag);
      break;
    case SET_RETURN_OP:
      //set the return type to expression's operand type
      b_set_return(ty_query(expr->u.unop.operand->type));
      break;
  }
}

void encode_binop_expr(EXPR_BINOP op, EXPR expr)
{
  TYPETAG type_tag;
  TYPETAG left_type_tag, right_type_tag;
  
  //make sure operands are encoded first (could be long subtrees)
  encode_expr(expr->u.binop.left);
  encode_expr(expr->u.binop.right);
  
  //fill in helper vars
  type_tag = ty_query(expr->type);
  left_type_tag = ty_query(expr->u.binop.left->type);
  right_type_tag = ty_query(expr->u.binop.right->type);

  //specific actions, based on op
  switch (op) {
    //ops that do nothing
    case SYMDIFF_OP:
    case OR_OP:
    case XOR_OP:
    case AND_OP:
      break;
    //simple operations (just perform operation)
    case ADD_OP:
      b_arith_rel_op(B_ADD, type_tag);
      break;
    case SUB_OP:
      b_arith_rel_op(B_SUB, type_tag);
      break;
    case MUL_OP:
      b_arith_rel_op(B_MULT, type_tag);
      break;
    case DIV_OP:
      b_arith_rel_op(B_DIV, type_tag);
      break;
    case MOD_OP:
      b_arith_rel_op(B_MOD, type_tag);
      break;
    case REALDIV_OP:
      b_arith_rel_op(B_DIV, type_tag);
      break;
    case EQ_OP:
      b_arith_rel_op(B_EQ, type_tag);
      break;
    case LESS_OP:
      b_arith_rel_op(B_LT, type_tag);
      break;
    case LE_OP:
      b_arith_rel_op(B_LE, type_tag);
      break;
    case NE_OP:
      b_arith_rel_op(B_NE, type_tag);
    break;
    case GE_OP:
      b_arith_rel_op(B_GE, type_tag);
    break;
    case GREATER_OP:
      b_arith_rel_op(B_GT, type_tag);
    break;
    //assign needs some extra checks
    case ASSIGN_OP:      
      //if left expr is lvar, push it's address
      if (expr->u.binop.left->tag == LVAR) {
	b_push_loc_addr(expr->u.binop.left->u.lvar.offset);
      }
      
      //if left/right typetags don't match, convert left->right
      if (left_type_tag != right_type_tag) {
	b_convert(right_type_tag, left_type_tag);
      }
     
      //perform assign operation
      b_assign(left_type_tag);
      
      //pop the stack
      b_pop();
    break;
    
  }
}

void encode_fcall_expr(EXPR func, EXPR_LIST args)
{
  char *func_gname;
  TYPE func_ret_type;
  PARAM_LIST func_params;
  BOOLEAN check_args;
  
  int arg_list_size;
  EXPR_LIST t_arg;
  TYPETAG arg_tag;
  
  //fill in helper vars
  func_ret_type = ty_query_func(func->type, &func_params, &check_args);
  arg_list_size = 0;
  t_arg = args;

  //save function name
  if (func->tag == GID) { //if it's a GID
    func_gname = st_get_id_str(func->u.gid);
  }
  
  //figure out arg list size
  t_arg = args;
  while (t_arg != NULL) {
	
    //figure out size of arg
    if (ty_query(t_arg->expr->type) == TYDOUBLE || ty_query(t_arg->expr->type) == TYFLOAT) { //float/double -> add 8
      arg_list_size += 8;
    } else { //anything else -> add 4
      arg_list_size += 4;
    }

    //get next item
    t_arg = t_arg->next;
  }
  //now that we know arg list size, allocate
  b_alloc_arglist(arg_list_size);

  //encode arguments
  t_arg = args;
  while (t_arg != NULL) {

    //encode the arg (could be a long subtree)
    encode_expr(t_arg->expr);
    
    //find arg's typetag
    arg_tag = ty_query(t_arg->expr->type);

    //check the associated parameter too
    if (func_params != NULL) {
      if (func_params->is_ref == TRUE) { //pass-by-reference
	
	//make sure arg is lval
	if (is_lval(t_arg->expr) == FALSE) {
	  bug("Function argument expected to be lval in encode_fcall_expr");
	} //else
	
	//make sure types of arg and param are compatible
	if (ty_test_equality(t_arg->expr->type, func_params->type) == FALSE) {
	  error("Parameter types do not match");
	}
	
	//load the arg as a pointer
	b_load_arg(TYPTR);
      } else { //pass-by-value

	//if arg is lval, need to deref
	if (is_lval(t_arg->expr) == TRUE) {
	  b_deref(arg_tag);
	}

	//upcast type and load argument as new type
	if (arg_tag == TYSIGNEDCHAR || arg_tag == TYUNSIGNEDCHAR) { //char -> int
	  b_convert(arg_tag, TYSIGNEDLONGINT);
	  b_load_arg(TYSIGNEDLONGINT);
	} else if (arg_tag == TYFLOAT) { //float -> double
	  b_convert(arg_tag, TYDOUBLE);
	  b_load_arg(TYDOUBLE);
	} else { //anything else (no upcast)
	  b_load_arg(arg_tag);
	}
      }
    } else { //there is no param list      
	//if arg is lval, need to deref
	if (is_lval(t_arg->expr) == TRUE) {
	  b_deref(arg_tag);
	}

	//upcast type and load argument as new type
	if (arg_tag == TYSIGNEDCHAR || arg_tag == TYUNSIGNEDCHAR) { //char -> int
	  b_convert(arg_tag, TYSIGNEDLONGINT);
	  b_load_arg(TYSIGNEDLONGINT);
	} else if (arg_tag == TYFLOAT) { //float -> double
	  b_convert(arg_tag, TYDOUBLE);
	  b_load_arg(TYDOUBLE);
	} else { //anything else (no upcast)
	  b_load_arg(arg_tag);
	}
    }

    //get next arg
    t_arg = t_arg->next;

    //get next param (if exists)
    if (func_params != NULL) {
      func_params = func_params->next;
    }

  }
  
  //finally call the function by name
  b_funcall_by_name(func_gname, ty_query(func_ret_type));
}

//functions/procedures
void enter_func_body(char *global_func_name, TYPE type, int loc_var_offset)
{
  TYPE return_type;
  PARAM_LIST param;
  BOOLEAN check_args_t;
  TYPETAG return_tag;
  TYPETAG tag_t;
  int param_offset_t;

  //fill in helper vars
  return_type = ty_query_func(type, &param, &check_args_t);
  return_tag = ty_query(return_type);

  //enter function
  b_func_prologue(global_func_name);

  //store parameters
  while (param != NULL) {
    tag_t = ty_query(param->type);
    param_offset_t = b_store_formal_param(tag_t); //param_offset_t necessary later? AG
    //st lookup to make sure offset matches? AG

    //get next param
    param = param->next;
  }
  
  //if type isn't void (func call), allocate for return val
  if (return_tag != TYVOID) {
    b_alloc_return_value();
  }
  
  //printf("loc_var_offset: %d\n", loc_var_offset);
  
  //allocate specified local var space
  b_alloc_local_vars(loc_var_offset);
}

void exit_func_body(char *global_func_name, TYPE type)
{
  TYPE return_type;
  PARAM_LIST param;
  BOOLEAN check_args_t;
  TYPETAG return_tag;

  //fill in helper vars
  return_type = ty_query_func(type, &param, &check_args_t);
  return_tag = ty_query(return_type);

  //if non void return type (func call), prepare return type
  if (return_tag != TYVOID) {
    b_prepare_return(return_tag);
  }

  //pop function id off of stack (do earlier?) AG
  fi_top--;

  //exit function
  b_func_epilogue(global_func_name);

  //exit current block
  st_exit_block();

  //update bo stack
  bo_top--;
}

/* Project 3 */

//other
long get_array_shift_by(INDEX_LIST index, long elem_size)
{
  long low, high, range;
  long shift_by;
  
  ty_query_subrange(index->type, &low, &high);
  range = high - low + 1; //+1? AG
  
  if (index->next != NULL) {
    shift_by = range * get_array_shift_by(index->next, elem_size);
  } else {
    shift_by = range * elem_size;
  }
  return shift_by;
}

void encode_array_access_expr(EXPR expr, EXPR_LIST expr_list)
{
  long low, high;
  long elem_size, shift_by_size;
  TYPE array_type;
  INDEX_LIST index_t;
  EXPR_LIST index_expr_t;
  
  //fill in helper vars
  array_type = ty_query_array(expr->type, &index_t);
  elem_size = get_type_size(array_type);
  
  //encode the array id expression
  encode_expr(expr);
 
  //encode each index expression
  index_expr_t = expr_get_head(expr_list);
  index_t = index_get_head(index_t);
  while (index_expr_t != NULL && index_t != NULL) {
    
    //encode the index expression
    encode_expr(index_expr_t->expr);
   
    //figure out the range of the array indeces
    ty_query_subrange(index_t->type, &low, &high);
    
    if (index_t->next != NULL) { //first/middle index
      b_push_const_int(low);
      b_arith_rel_op(B_SUB, TYSIGNEDLONGINT);
      
      shift_by_size = get_array_shift_by(index_t->next, elem_size);
      b_ptr_arith_op(B_ADD, TYSIGNEDLONGINT, shift_by_size);
      
    } else if (index_t->next == NULL) { //last index (also if only 1 index)
      b_push_const_int(low);
      b_arith_rel_op(B_SUB,TYSIGNEDLONGINT);
      shift_by_size = elem_size;
      b_ptr_arith_op(B_ADD, TYSIGNEDLONGINT, shift_by_size);
    }
   
    index_expr_t = index_expr_t->next;
    index_t = index_t->next;
  }
}

//encode a for loop
void encode_dispatch(VAL_LIST vals, char *next_label)
{
  char *match_label;
  char *check_fail_lbl;
  
  match_label = new_symbol();

  //encode for each value
  vals = val_get_tail(vals);
  while (vals != NULL) {
    
    //for subranges:
    if (vals->lo != vals->hi) {
      //need to make 2 dispatches (for low/high)
      check_fail_lbl = new_symbol();
      b_dispatch(B_LT, TYSIGNEDLONGINT, vals->lo, check_fail_lbl, FALSE);
      b_dispatch(B_LE, TYSIGNEDLONGINT, vals->hi, match_label, TRUE);
      b_label(check_fail_lbl);
    } else { //not subrange
      //just dispatch
      b_dispatch(B_EQ, TYSIGNEDLONGINT, vals->hi, match_label, TRUE);
    }
    
    //get prev value
    vals = vals->prev;
  }

  //jump to "no match" label
  b_jump(next_label);

  //put down continue
  b_label(match_label);
}

char *encode_for_loop(EXPR identifier, EXPR from_limit, int direction, EXPR to_limit, char *exit_lbl)
{
  char *start_lbl;
  
  //encode to limit (top of stack)
  encode_expr(to_limit);

  //convert to value's type if necessary
  if (ty_query(to_limit->type) != TYSIGNEDLONGINT) {
    b_convert(ty_query(to_limit->type), TYSIGNEDLONGINT);
  }
  
  //need to make sure to_limit is an r-value
  if (is_lval(to_limit) == TRUE) {
    b_deref(ty_query(from_limit->type));
  }

  //duplicate to_limit on stack to keep static version throughout loop
  b_duplicate(TYSIGNEDLONGINT);	
  
  //set identifier's value to from_limit
  b_push_ext_addr(st_get_id_str(identifier->u.gid));
  encode_expr(from_limit);
  b_assign(ty_query(identifier->type));

  //create a new start label
  start_lbl = new_symbol();
  b_label(start_lbl);

  //convert from value's type if necessary
  if (ty_query(from_limit->type) != TYSIGNEDLONGINT) {
    b_convert(ty_query(from_limit->type), TYSIGNEDLONGINT);
  }

  //compare from/to values (based on direction)
  if (direction == 0) {
    b_arith_rel_op(B_LT, TYSIGNEDLONGINT);
  } else {
    b_arith_rel_op(B_GT, TYSIGNEDLONGINT);
  }

  //if we're done with loop, jump to exit
  b_cond_jump(TYSIGNEDLONGINT, B_NONZERO, exit_lbl);

  //give back the generated start label
  return start_lbl;
}

