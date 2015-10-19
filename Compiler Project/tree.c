#include <stdlib.h>
#include "string.h"
#include "tree.h"
#include "encode.h"

//id list
ID_LIST id_create_node(ST_ID id)
{
  ID_LIST new_node;

  new_node = (ID_LIST)malloc(sizeof(ID_NODE));
  new_node->prev = NULL;
  new_node->next = NULL;
  new_node->id = id;
  
  return new_node;
}

ID_LIST id_get_head(ID_LIST list)
{
  ID_LIST head = list;
  while (head != NULL && head->prev != NULL) {
    head = head->prev;
  }
  return head;
}

ID_LIST id_get_tail(ID_LIST list)
{
  ID_LIST tail = list;
  while (tail != NULL && tail->next != NULL) {
    tail = tail->prev;
  }
  return tail;
}

long id_count(ID_LIST list)
{
  ID_LIST next;
  long count;
  
  count = 0;
  
  next = id_get_head(list);
  while (next != NULL) {
    count++;
    next = next->next;
  }
  
  return count;
}

ID_LIST id_append_id(ID_LIST list, ST_ID id)
{
  ID_LIST new_node;
  
  new_node = id_create_node(id);
  if (list != NULL) {    
      new_node->prev = list;
      list->next = new_node;
  }
  
  return new_node;
}

//param list
PARAM_LIST param_create_node(ID_LIST id_list, TYPE type, BOOLEAN is_ref)
{
  PARAM_LIST new_node;
  ID_LIST id;
  PARAM_LIST last_param;
  
  last_param = NULL;
  id = id_get_head(id_list);
  while (id != NULL) {    
    new_node = (PARAM_LIST)malloc(sizeof(PARAM));
    new_node->prev = last_param;
    new_node->next = NULL;
    new_node->id = id->id;
    new_node->type = type;
    new_node->is_ref = is_ref;
    new_node->sc = NO_SC;
    new_node->err = FALSE;
    
    if (last_param != NULL) {
      last_param->next = new_node;
    }
    
    last_param = new_node;
    id = id->next;
  }
  return new_node;
}

PARAM_LIST param_get_head(PARAM_LIST list)
{
  PARAM_LIST head = list;

  while (head != NULL && head->prev != NULL) {
    head = head->prev;
  }
  return head;
}

PARAM_LIST param_append_param(PARAM_LIST list, PARAM_LIST param)
{  
  PARAM_LIST head;
  
  if (param == NULL) {
    return list;
  }
  
  head = param_get_head(param);
  if (list != NULL) {    
    head->prev = list;
    list->next = head;
  }

  return param;
}

//index list
INDEX_LIST index_create_node(TYPE type)
{
  if (type == NULL) {
    error("Empty subrange in array index");
    error("Illegal index type (ignored)");
    return NULL;    
  }
  
  INDEX_LIST new_node;

  new_node = (INDEX_LIST)malloc(sizeof(INDEX));
  new_node->prev = NULL;
  new_node->next = NULL;
  new_node->type = type;

  return new_node;
}

INDEX_LIST index_get_head(INDEX_LIST list)
{
  INDEX_LIST head = list;
  while (head != NULL && head->prev != NULL) {
    head = head->prev;
  }
  return head;
}

INDEX_LIST index_append_type(INDEX_LIST list, TYPE type)
{  
  if (type == NULL) {
    error("Empty subrange in array index");
    error("Illegal index type (ignored)");
    return list;
  }
  
  INDEX_LIST new_node;
  new_node = index_create_node(type);
    if (list != NULL) {    
      new_node->prev = list;
      list->next = new_node;
    }
  
    return new_node;
  
}

//create/install structures
BOOLEAN is_typetag_simple(TYPETAG tag)
{
  switch (tag) {
    case TYUNION:
    case TYENUM:
    case TYSTRUCT:
    case TYARRAY:
    case TYSET:
    case TYFUNC:
    case TYBITFIELD:
    case TYSUBRANGE:
    case TYERROR:
      return FALSE;
  }
  
  return TRUE;
}

BOOLEAN is_typtag_numerical(TYPETAG tag)
{
  switch (tag) {
    case TYSIGNEDLONGINT:
    case TYFLOAT:
    case TYDOUBLE:
      return TRUE;
  }
  
  return FALSE;
}

BOOLEAN is_typtag_numerical_or_char(TYPETAG tag)
{
  switch (tag) {
    case TYSIGNEDLONGINT:
    case TYFLOAT:
    case TYDOUBLE:
    case TYUNSIGNEDCHAR:
    case TYSIGNEDCHAR:
      return TRUE;
  }
  
  return FALSE;
}

void check_for_duplicate_params(PARAM_LIST list)
{
  PARAM_LIST param;
  ST_ID first_id;
  
  first_id = list->id;
  
  param = list->next;
  while (param != NULL) {
    if (first_id == param->id) {
      error("Duplicate parameter name: \"%s\"", st_get_id_str(first_id));
    }
    
    param = param->next;
  }
}

void make_type(ST_ID id, TYPE type)
{ 
  ST_DR data;
  BOOLEAN valid;
  
  data = stdr_alloc();
  data->tag = TYPENAME;
  data->u.typename.type = type;
  
  valid = st_install(id, data);
  if (valid != TRUE) {
    error("Duplicate definition of \"%s\" in block %d", st_get_id_str(id), st_get_cur_block());
  }
}

void make_var(ID_LIST list, TYPE type)
{        
  ST_DR data;
  BOOLEAN resolved;
  ST_ID id;
  ID_LIST node;
  TYPETAG tag;
  
  //make sure type exists
  if (type == NULL) {
    error("Variable(s) must be of data type"); 
    return;
  }

  tag = ty_query(type);
  //type can't be a pure function or error
  if (tag == TYFUNC || tag == TYERROR) {
      error("Variable(s) must be of data type");

      data = stdr_alloc();
      data->u.decl.type = type;
      data->u.decl.sc = NO_SC;
      data->tag = GDECL;

      if (tag == TYERROR) {
	data->u.decl.err = TRUE;
      }

      resolved = st_install(list->id, data);
      return;
  }
  
  node = id_get_tail(list);
  while (node != NULL) {
    id = node->id;

    data = stdr_alloc();
    data->tag = GDECL;
    data->u.decl.type = type;
    data->u.decl.sc = NO_SC;
    data->u.decl.v.offset = 0;
    data->u.decl.err = FALSE;
    
    resolved = st_install(id, data);
    if (resolved != TRUE) {
      error("Duplicate variable declaration: \"%s\"", st_get_id_str(node->id));
    } else {
      int alignment;
      int size;
      
      //allocate memory here
      alignment = get_type_alignment(type);
      size = get_type_size(type);
      
      b_global_decl(st_get_id_str(id), alignment, size);
      b_skip(size);
    }

    node = node->prev;
  }
}

TYPE make_array(INDEX_LIST index_list, TYPE type)
{
  INDEX_LIST head;
  TYPETAG tag;
  
  head = index_get_head(index_list);
  tag = TYERROR;
  
  //check for illegal index type error       	
  if (type == NULL){
    error("Data type expected for array elements");
    return ty_build_basic(TYERROR);	
  }
    
  tag = ty_query(type);
  
  //check for non-simple type error 
   if (tag != TYARRAY && is_typetag_simple(tag) == FALSE) { 
    error("Data type expected for array elements");
    return ty_build_basic(TYERROR);	
  }
  
  return ty_build_array(type, head);
}

TYPE make_func(PARAM_LIST list, TYPE return_type)
{
  TYPETAG tag, p_tag;
  PARAM_LIST param;
  BOOLEAN has_error;
  
  tag = ty_query(return_type);
  //check if type tag is simple
  if (is_typetag_simple(tag)) {
    has_error = FALSE;
  } else { //not simple, show error
    has_error = TRUE;
    error("Function return type must be simple type");
  }
  
  if (has_error == FALSE) {
    param = param_get_head(list);
    while (param != NULL) {
      p_tag = ty_query(param->type);
      
      if (is_typetag_simple(p_tag) != TRUE) {
	error("Parameter type must be a simple type");
      }
  
      check_for_duplicate_params(param);
      
      param = param->next;
    }
  }
  
  return ty_build_func(return_type, param_get_head(list), TRUE);
}

TYPE make_subrange(EXPR lo, EXPR hi)  
{
  long low, high;

  if (lo->tag != INTCONST) {
    error("Subrange lower index is not Integer");
    return NULL;
  }
  if (hi->tag != INTCONST) {
    error("Subrange upper index is not Integer");
    return NULL;
  }

  low = lo->u.intval;
  high = hi->u.intval;

  //make sure low < high
  if (low > high) {
    //error("Empty subrange in array index");
    return NULL;
  } else { //build subrange
    return ty_build_subrange(ty_build_basic(TYSIGNEDLONGINT), low, high);
  }
}

//utils
void resolve_unresolved_types()
{
  ST_DR data_rec;
  int block;
  TYPE unr_type, ptr, next;
  ST_ID id;
  MEMBER_LIST unr_member;
  BOOLEAN resolved;
  
  unr_type = ty_get_unresolved();
  while (unr_type != NULL) { 
    resolved = FALSE;
    
    ptr = ty_query_ptr(unr_type, &id, &next);
    
    data_rec = st_lookup(id, &block);    
    if (data_rec != NULL && data_rec->u.typename.type != NULL) {
      resolved = ty_resolve_ptr(unr_type, data_rec->u.typename.type);
    }
    
    if (resolved != TRUE) {
      error("Unresolved type name: \"%s\"",st_get_id_str(id));
    }
    
    unr_type = next;
  }
}

///////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
//############### DEVELOPMENT PROJECT 2 #########################\\
///////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

//expression lists

EXPR_LIST expr_get_head(EXPR_LIST list)
{
  EXPR_LIST head = list;
  while (head != NULL && head->prev != NULL) {
    head = head->prev;
  }
  return head;
}

EXPR_LIST expr_prepend(EXPR_LIST list, EXPR expr)
{
  EXPR_LIST new = malloc(sizeof(EXPR_LIST_NODE));
  new->expr = expr;
  new->next = list;
	if (list != NULL) {
		list->prev = new;
	}
  return new;
}

EXPR_LIST expr_append(EXPR_LIST list, EXPR expr)
{
  EXPR_LIST new = malloc(sizeof(EXPR_LIST_NODE));
  new->expr = expr;
	new->prev = list;
	if (list != NULL) {
		list->next = new;
	}
  return new;
}

void expr_free(EXPR expr)  
{
  //make sure expression needs freeing
  if (expr == NULL) {
    return;
  } //else
  
  if (expr->tag == UNOP) { //unary op
    //need to free operand
    expr_free(expr->u.unop.operand);
  } else if (expr->tag == BINOP) { //binary op
    //need to free both operands
    expr_free(expr->u.binop.left);
    expr_free(expr->u.binop.right);
  } else if(expr->tag == FCALL) { //function call
    //free function and args
    expr_free(expr->u.fcall.function);
    expr_list_free(expr->u.fcall.args);
  } //else if simple expression type, don't need anything special
  
  //finally free the expression itself
  free(expr);
}

void expr_list_free(EXPR_LIST list)  
{  
  //make sure expression list needs freeing
  if (list == NULL || list->expr == NULL) {
    return;
  }
  
  //free expression itself
  expr_free(list->expr);
  
  //free both previous and next (to make sure we get whole list)
  //if either ends up being null, it will just that direction
  expr_list_free(list->prev);
  expr_list_free(list->next);
  
  //free the list itself
  free(list);
}

//make expression node (by type)

////simple types
EXPR make_error_expr()
{
  EXPR expr;
  
  expr = malloc(sizeof(EXPR_NODE));
  expr->tag = ERROR;
  expr->type = ty_build_basic(TYERROR);

  return expr;
}

EXPR make_intconst_expr(long val, TYPE type)  
{
  EXPR expr;
  
  expr = malloc(sizeof(EXPR_NODE));
  expr->tag = INTCONST;
  expr->u.intval = val;
  expr->type = ty_build_basic(TYSIGNEDLONGINT);

  //add a convert node, if needed
  if (ty_query(type) != TYSIGNEDLONGINT) {
    expr = make_convert_expr(expr, type);
  }
 
  return expr;
}

EXPR make_realconst_expr(double val)  
{
  // create the node and allocates memory
  EXPR expr;
  
  expr = malloc(sizeof(EXPR_NODE));
  expr->tag = REALCONST;
  expr->u.realval = val;
  expr->type = ty_build_basic(TYDOUBLE);

  return expr;
}

EXPR make_sign_number_expr(EXPR_UNOP op, EXPR num)  
{
  if (num->tag != INTCONST && num->tag != REALCONST) {
    error("Failed to check sign, number is not INTCONST or REALCONST");
  }
  
  //flip sign, if necessary
  if (op == NEG_OP) {
    if (num->tag == INTCONST) {
      num->u.intval *= -1;
    } else if (num->tag == REALCONST) {
      num->u.realval *= -1;
    }
  }
  
  return num;
}

EXPR make_id_expr(ST_ID id)  
{
  ST_DR record;
  int block_num;
  EXPR expr;

  // create node and allocate memory
  expr = malloc(sizeof(EXPR_NODE));

  //lookup record for id
  record = st_lookup(id, &block_num);
  if (record == NULL) {
    error("Undeclared identifier \"%s\" in expression", st_get_id_str(id));
    return make_error_expr();
  } //else

  //check if id corresponds to a type
  if (record->tag == TYPENAME) {
    error("Identifier installed as a Typename");
    return make_error_expr();
  } //else

  //fill in expression's type
  expr->type = record->u.decl.type;
 
  //if this is a global declaration (variable)
  if (record->tag == GDECL) {
    expr->tag = GID;
    expr->u.gid = id;
  } else if (record->tag == LDECL) { //inside a function (100%)
    //printf("declaring ldecl\n");
    expr->tag = LVAR;
    expr->u.lvar.is_ref = record->u.decl.is_ref;
    expr->u.lvar.link_count = st_get_cur_block() - block_num;
    expr->u.lvar.offset = record->u.decl.v.offset;
  } else if (record->tag == FDECL)  { //defining a function
    if (block_num <= 1) {
      expr->tag = GID;
      expr->u.gid = id;
    } else {
      expr->tag = LFUN;
      expr->u.lfun.global_name = record->u.decl.v.global_func_name;
      expr->u.lfun.link_count = st_get_cur_block() - block_num;
    }
  }

  return expr;
}

EXPR make_strconst_expr(char *str)  
{
  EXPR expr;
  
  expr = malloc(sizeof(EXPR_NODE));
  expr->tag = STRCONST;
  expr->u.strval = str;
  expr->type = ty_build_ptr(NULL, ty_build_basic(TYUNSIGNEDCHAR));

  return expr;
}

EXPR make_null_expr(EXPR_NULLOP op)  
{
  EXPR expr;
  
  expr = malloc(sizeof(EXPR_NODE));
  expr->tag = NULLOP;
  expr->u.nullop.op = op;

  //expr's type is either void or char (based on op)
  if (op == NIL_OP) {
    expr->type = ty_build_basic(TYVOID);
  } else {
    expr->type = ty_build_basic(TYSIGNEDCHAR);
  }

  return expr;
}

////more complicated types
EXPR make_convert_expr(EXPR sub_expr, TYPE type)
{
  EXPR expr;
  
  expr = make_un_expr(CONVERT_OP, sub_expr);
  expr->type = type;

  return expr;
}

EXPR make_un_expr(EXPR_UNOP op, EXPR sub_expr)  
{
  ST_ID id;
  TYPE next;
  long low, high;
  TYPETAG sub_expr_tag;
  
  //if expression is an error, just return it
  if (sub_expr->tag == ERROR) {
    return sub_expr;
  } //else

  //otherwise create a new expression
  EXPR expr;
  
  expr = malloc(sizeof(EXPR_NODE));
  expr->tag = UNOP;	
  expr->u.unop.op = op;
  expr->type = sub_expr->type;
  expr->u.unop.operand = sub_expr;

  //try fold expression
  expr = expr_fold_unop(expr);
  //check if folded expr is simple enough to return
  if (expr->tag != UNOP) {
    return expr;
  }

  //if a deref op, just return expr
  if (op == DEREF_OP) {
    return expr; 
  }
    
  //check addressof or new error
  if (op == ADDRESS_OP || op == NEW_OP) {
    if (is_lval(sub_expr) == FALSE) {
      error("L value expected for unary operator");
      return make_error_expr();
    }
  } else if (is_lval(sub_expr)) {
    expr->u.unop.operand = make_un_expr(DEREF_OP, sub_expr);
  }
  
  //check sub expression's type tag
  sub_expr_tag = ty_query(sub_expr->type);

  //see if type conversion is necessary (for float, subrange)
  if (op != CONVERT_OP) {    
    //convert float to double
    if (sub_expr_tag == TYFLOAT) {
      expr->u.unop.operand = make_convert_expr(sub_expr, ty_build_basic(TYDOUBLE));
    }
    
    if (sub_expr_tag == TYSUBRANGE) {
      next = ty_query_subrange(sub_expr->type, &low, &high);
      expr->u.unop.operand = make_convert_expr(sub_expr, next);
    }
  }

  //check sub expression's type tag again (incase we changed it)
  sub_expr_tag = ty_query(sub_expr->type);
  
  //do something based on operation
  switch (op) {
    case CONVERT_OP:
    case DEREF_OP:
    case UN_EOF_OP:
    case UN_EOLN_OP:
    case NEW_OP:
    case ADDRESS_OP:
    case SET_RETURN_OP:
      break;
    case UN_SUCC_OP:
    case UN_PRED_OP: //type must be [int, char]
      if (sub_expr_tag != TYSIGNEDLONGINT && sub_expr_tag != TYUNSIGNEDCHAR) {
	error("Nonordinal type argument to Succ or Pred");
	return make_error_expr();
      } //else
      break;
    case CHR_OP: //type must be [int]
      if (sub_expr_tag != TYSIGNEDLONGINT) {
	error("Illegal type argument to Chr");
	return make_error_expr();
      } //else
      
      //upcast to int
      expr->type = ty_build_basic(TYUNSIGNEDCHAR);
      break;
    case ORD_OP: //type must be [int, char]
      if (sub_expr_tag != TYSIGNEDLONGINT && sub_expr_tag != TYUNSIGNEDCHAR && sub_expr_tag != TYSIGNEDCHAR) {
	error("Illegal type argument to Ord");
	return make_error_expr();
      } //else
      
      //upcast to int
      expr->type = ty_build_basic(TYSIGNEDLONGINT);
      break;
    case INDIR_OP:
      //get underlying type
      expr->type = ty_query_ptr(sub_expr->type, &id, &next);
      break; 
    case UPLUS_OP: //type must be [int, float, double]
      if (sub_expr_tag != TYSIGNEDLONGINT && sub_expr_tag != TYFLOAT && sub_expr_tag != TYDOUBLE) {
	error("Illegal type argument to unary plus");
	return make_error_expr();
      } //else
      break;
    case NEG_OP: //type must be [int, float, double]
      if (sub_expr_tag != TYSIGNEDLONGINT && sub_expr_tag != TYFLOAT && sub_expr_tag != TYDOUBLE) {
	error("Illegal type argument to unary minus");
	return make_error_expr();
      } //else
      break;
    case DISPOSE_OP:
      b_alloc_arglist(4);
      break;
  }

  return expr;
}

EXPR make_bin_expr(EXPR_BINOP op, EXPR left, EXPR right)  
{
  EXPR expr_node;
  //for querying subrange
  TYPE next;
  long low, high;
  
  //make sure neither expr is an error
  if (left->tag == ERROR || right->tag == ERROR) {
    return make_error_expr();
  } //else
  
  expr_node = malloc(sizeof(EXPR_NODE));
  expr_node->tag = BINOP;
  expr_node->type = right->type;
  expr_node->u.binop.op = op;
  expr_node->u.binop.left = left;
  expr_node->u.binop.right = right;

  //try fold expression
  expr_node = expr_fold_binop(expr_node);
  //check if folded expr is simple enough to return
  if (expr_node->tag != BINOP) {
    return expr_node;
  }

  //checks if a unary conversion is necessary
  TYPETAG sub_tag_l = ty_query(left->type);
  TYPETAG sub_tag_r = ty_query(right->type);

  //check if lval is required (for assign ops)
  if (op == ASSIGN_OP) {
    
    //make sure left expr is an lval
    if (is_lval(left) != TRUE) {
      error("Assignment requires l-value on the left");
      return make_error_expr();	
    } //else
    
    //if right expr is lval, need to deref
    if (is_lval(right))  {
      right = expr_node->u.binop.right = make_un_expr(DEREF_OP, right); 
    }
    
    //make sure right sub expr's type isn't void
    if (sub_tag_r == TYVOID)  {
      error("Cannot convert between nondata types");
      return make_error_expr();
    } //else
    
    //see if right sub expr needs a convert node
    if (sub_tag_r == TYFLOAT) { //float
      expr_node->u.binop.right = make_convert_expr(right, ty_build_basic(TYDOUBLE));
    } else if (sub_tag_r == TYSUBRANGE) { //subrange
      next = ty_query_subrange(right->type, &low, &high);
      expr_node->u.binop.right = make_convert_expr(right, next);
    }
    
    //pass this on to a more specific "assign" function
    return check_assign(expr_node);
  } //else
    
  //make sure both expr are rvals
  if (is_lval(left)) { //left
    expr_node->u.binop.left = make_un_expr(DEREF_OP, left);
  }
  if (is_lval(right)) { //right
    expr_node->u.binop.right = make_un_expr(DEREF_OP, right);
  }

  //see if left sub expr needs a convert node
  if (sub_tag_l == TYFLOAT || sub_tag_l == TYSUBRANGE) {
    
    if (sub_tag_l == TYFLOAT) { //float
      expr_node->u.binop.left = make_convert_expr(left, ty_build_basic(TYDOUBLE));
    } else { //subrange
      next = ty_query_subrange(left->type, &low, &high);
      expr_node->u.binop.left = make_convert_expr(left, next);
    }
    
    //update tag
    sub_tag_l = ty_query(expr_node->u.binop.left->type);
  }

  //see if right sub expr needs a convert node
  if (sub_tag_r == TYFLOAT || sub_tag_r == TYSUBRANGE) {
    
    if (sub_tag_r == TYFLOAT) { //float
      expr_node->u.binop.right = make_convert_expr(right, ty_build_basic(TYDOUBLE));
    } else { //subrange
      next = ty_query_subrange(right->type, &low, &high);
      expr_node->u.binop.right = make_convert_expr(right, next);
    }
    
    //update tag
    sub_tag_r = ty_query(expr_node->u.binop.right->type);
  }

  //see if mixed types need to be resolved (int and double)
  if (sub_tag_l == TYDOUBLE || sub_tag_r == TYDOUBLE) { //if left or right expr is double
    //left expr is int?
    if (sub_tag_l == TYSIGNEDLONGINT) {
      expr_node->u.binop.left = make_convert_expr(expr_node->u.binop.left, ty_build_basic(TYDOUBLE));
      
      //update tag
      sub_tag_l = ty_query(expr_node->u.binop.left->type);
    }
    //right expr is int?
    if (sub_tag_r == TYSIGNEDLONGINT) {
      expr_node->u.binop.right = make_convert_expr(expr_node->u.binop.right, ty_build_basic(TYDOUBLE));
      
      //update tag
      sub_tag_r = ty_query(expr_node->u.binop.right->type);
    }
  }
  
  //do special action for specific ops
  switch (op) {
   //assignment
    case ASSIGN_OP:
       //should have already taken care of this (called check_assign earlier)
      break;
    //basic math operations (mixed types)
    case ADD_OP:
    case SUB_OP:
    case MUL_OP:
    case DIV_OP:
      //make sure both expr are numerical
      if (is_typtag_numerical(sub_tag_r) == FALSE || is_typtag_numerical(sub_tag_l) == FALSE) {
      	error("Nonnumerical type argument(s) to arithmetic operation");
	return make_error_expr();
      } //else
      
      //at least one is a double?
      if (sub_tag_l == TYDOUBLE || sub_tag_r == TYDOUBLE) {
	//if left is int, convert to double
	if (sub_tag_l == TYSIGNEDLONGINT) {
	  expr_node->u.binop.left = upcast_int_expr(expr_node->u.binop.left);
	}
	//if right is int, convert to double
	if (sub_tag_r == TYSIGNEDLONGINT) {
	  expr_node->u.binop.right = upcast_int_expr(expr_node->u.binop.right);
	}
	
	expr_node->type = ty_build_basic(TYDOUBLE);
      } else { //they're both ints
	expr_node->type = ty_build_basic(TYSIGNEDLONGINT);
      }
      break;
    //basic math operations (ints)
    case MOD_OP:
    case REALDIV_OP: //need to check for doubles?
      //make sure both expr are ints
      if (sub_tag_r != TYSIGNEDLONGINT || sub_tag_l != TYSIGNEDLONGINT) {
	error("Noninteger type argument(s) to integer arithmetic operation");
	return make_error_expr();
      } //else
      
      expr_node->type = ty_build_basic(TYSIGNEDLONGINT);
      break;
    //comparison
    case EQ_OP:
    case LESS_OP:
    case LE_OP:
    case NE_OP:
    case GE_OP:
    case GREATER_OP:
      //make sure both expr have same type
      if (sub_tag_r != sub_tag_l) {
	error("Incompatible type arguments to comparison operator");
	return make_error_expr();
      } //else
      
      //make sure both expr are numerical or char types
      if (is_typtag_numerical_or_char(sub_tag_r) == FALSE && is_typtag_numerical_or_char(sub_tag_l) == FALSE) {
      	error("Illegal type arguments to comparison operator");
	return make_error_expr();
      } //else
      
      //promote char types to int
      //left
      if (sub_tag_l == TYUNSIGNEDCHAR || sub_tag_l == TYSIGNEDCHAR) {
	expr_node->u.binop.left = make_convert_expr(expr_node->u.binop.left, ty_build_basic(TYSIGNEDLONGINT));
      }
      //right
      if (sub_tag_r == TYUNSIGNEDCHAR || sub_tag_r == TYSIGNEDCHAR) {
	expr_node->u.binop.right = make_convert_expr(expr_node->u.binop.right, ty_build_basic(TYSIGNEDLONGINT));
      }
      
      expr_node->type = ty_build_basic(TYSIGNEDLONGINT);
      expr_node = make_convert_expr(expr_node, ty_build_basic(TYSIGNEDCHAR));
      break;
  }

  return expr_node;
}

EXPR make_fcall_expr(EXPR func, EXPR_LIST args)
{
  EXPR expr;
  TYPE func_ret_type, next;
  PARAM_LIST func_params;
  BOOLEAN check_args;
  long low, high;
  EXPR_LIST args_t;
  TYPETAG sub_expr_tag;
  
  //make sure passed expr type is actually function
  if (ty_query(func->type) != TYFUNC) {
     bug("Expression sent to make_fcall_expr is NOT a function.");
  }
  
  //fill in some helper vars
  args_t = args;
  func_ret_type = ty_query_func(func->type, &func_params, &check_args);
  
  //if arguments provided, check them
  if (args != NULL) {

    //if check arguments required, match arguments to func param list, do conversions
    if (check_args == TRUE) {
      //process all the arguments
      while (args_t != NULL && func_params != NULL) {
	
	  //if current parameter is pass-by-reference
	  if (func_params->is_ref == TRUE) {
	    
	    //make sure passed arg is a rval
	    if (is_lval(args_t->expr) == FALSE) {
	      error("Reference parameter is not an l-value");
	      return make_error_expr();
	    } //else
	    
	    //make sure types match
	    if (ty_test_equality(args_t->expr->type, func_params->type) == FALSE) {
	      error("Reference argument has incompatible type");
	      return make_error_expr();
	    } //else
	  }
	  
	  //if current parameter is pass-by-value
	  if (func_params->is_ref == FALSE) {
	    
	    //if arg is lval, deref it
	    if (is_lval(args_t->expr) == TRUE) {
	      args_t->expr = make_un_expr(DEREF_OP, args_t->expr);
	    }

	    //get argument's type tag for quick access
	    sub_expr_tag = ty_query(args_t->expr->type);

	    //upcast float
	    if (sub_expr_tag == TYFLOAT) {
	      args_t->expr = make_convert_expr(args_t->expr, ty_build_basic(TYDOUBLE));
	    }
	    
	    //upcast subrange
	    if (sub_expr_tag == TYSUBRANGE) {
	      next = ty_query_subrange(args_t->expr->type, &low, &high);
	      args_t->expr = make_convert_expr(args_t->expr, next);
	    }
	  }

	  //get next items
	  args_t = args_t->next;
	  func_params = func_params->next;
      } //while

      //make sure num of parms and num args match
      if (args_t == NULL && func_params != NULL) {
	error("Wrong number of arguments to procedure or function call");
	return make_error_expr();
      } //else
      
      //make sure num of parms and num args match
      if (func_params == NULL && args_t != NULL) {
	error("Wrong number of arguments to procedure or function call");
	return make_error_expr();
      } //else
    } //if check_args
    
    //if check arguments not required, make rvals and do necessary conversion
    if (check_args == FALSE) {
      //process all the arguments
      while (args_t != NULL) {
	//if this argument is lval, deref it
	if (is_lval(args_t->expr) == TRUE) {
	  args_t->expr = make_un_expr(DEREF_OP, args_t->expr);
	}
	
	//get argument's type tag for quick access
	sub_expr_tag = ty_query(args_t->expr->type);

	//upcast float
	if (sub_expr_tag == TYFLOAT) {
	  args_t->expr = make_convert_expr(args_t->expr, ty_build_basic(TYDOUBLE));
	}
	
	//upcast subrange
	if (sub_expr_tag == TYSUBRANGE) {
	  next = ty_query_subrange(args_t->expr->type, &low, &high);
	  args_t->expr = make_convert_expr(args_t->expr, next);
	}
	
	//get next argument
	args_t = args_t->next;
      }
    } //if check_args != TRUE

  } //if args != null
  
  //now we're ready to create the new expr
  expr = malloc(sizeof(EXPR_NODE));
  expr->tag = FCALL;
  expr->type = func_ret_type;
  expr->u.fcall.args = args;
  expr->u.fcall.function = func;

  return expr;
}

//defining functions
char *get_global_func_name(ST_ID id)
{
  return st_get_id_str(id);
}

void build_func_decl(ST_ID id, TYPE type, DIRECTIVE directive)
{
  PARAM_LIST func_params;
  BOOLEAN check_args;
  TYPE return_type;
  ST_DR dr;
    
  dr = stdr_alloc();	  
  dr->tag = GDECL;
  dr->u.decl.is_ref = FALSE;
  dr->u.decl.v.global_func_name = get_global_func_name(id);

  //only 2 directives possible (external & forward)
  if (directive == DIR_EXTERNAL) {  
    dr->u.decl.sc = EXTERN_SC;
    return_type = ty_query_func(type, &func_params, &check_args);
    dr->u.decl.type = ty_build_func(return_type, func_params, FALSE);
  } else if (directive == DIR_FORWARD) { //not sure what to do with forward yet, just set basic attributes?
    dr->u.decl.sc = NO_SC;
    dr->u.decl.type = type;
  } else {
    error("Invalid directive: \"%s\"", directive);
  }
    
  //make sure we installed function id
  if (st_install(id, dr) == FALSE) {
    error("Duplicate forward or external function declaration");
  }
}

int process_var_decl(ID_LIST ids, TYPE type, int cur_offset)
{
  ST_DR stdr;
  TYPETAG tag;
  int total_size = 0;

  //make sure type exists
  if (type == NULL) {
    error("Variable(s) must be of data type"); 
    return;
  }
  
  //fill in helper vars
  tag = ty_query(type);

  //make sure type isn't func or error
  if (tag == TYFUNC || tag == TYERROR) {
      error("Variable(s) must be of data type");
      return;
  }
  //printf("entered process_var_decl\n");
  //printf("base_offset_stack[bo_top]: %d\n",base_offset_stack[bo_top]);
  //align, if necessary
  if (tag != TYSIGNEDCHAR && tag != TYUNSIGNEDCHAR) { //anything but char
    //printf("tag isn't char, decrement bo_stack by %d\n", get_type_alignment(ty_build_basic(tag)));
    base_offset_stack[bo_top] -= get_type_alignment(ty_build_basic(tag));
    //printf("base_offset_stack[bo_top]: %d\n",base_offset_stack[bo_top]);
  }
	
  //go through all var ids
  while (ids != NULL) {
    //decrement by type size
    //printf("processing id, decrementing bo_stack by %d\n", get_type_size(type));
    base_offset_stack[bo_top] -= get_type_size(type);
    total_size += get_type_size(type);
    //printf("base_offset_stack[bo_top]: %d\n",base_offset_stack[bo_top]);
    
    //create new stdr
    stdr = stdr_alloc();	  
    stdr->tag = LDECL;
    stdr->u.decl.type = type;
    stdr->u.decl.v.offset = base_offset_stack[bo_top];

    //install variable name
    if (st_install(ids->id, stdr) != TRUE) {
      error("Duplicate variable declaration: \"%s\"", st_get_id_str(ids->id));
    } else {
      //encode variable
      //how to encode local var? AG
      //declare_variable(list->id, type);
    }

    //get next variable
    ids = ids->next;
  }

  //return new offset
  return total_size;//base_offset_stack[bo_top];
}

void install_local_params(PARAM_LIST param)
{  
  ST_DR stdr;
  long low, high;

  while (param != NULL) {
    //create new record
    stdr = stdr_alloc();
    stdr->tag = PDECL;
    stdr->u.decl.type = param->type;
    stdr->u.decl.sc = param->sc;
    stdr->u.decl.is_ref = param->is_ref;
    stdr->u.decl.err = param->err;
    
    //set offset (based on is_ref or type)
    if (param->is_ref == TRUE) {
      stdr->u.decl.v.offset = get_formal_param_offset(TYPTR);
    } else {
      if (ty_query(param->type) != TYSUBRANGE) {
	stdr->u.decl.v.offset = get_formal_param_offset(ty_query(param->type));
      } else {
	stdr->u.decl.v.offset = get_formal_param_offset(ty_query(ty_query_subrange(param->type, &low, &high)));
      }
    }

    //install parameter
    st_install(param->id, stdr);

    //get next item
    param = param->next;
  }
}

int enter_function(ST_ID id, TYPE type, char *global_func_name)
{
  int block;
  ST_DR stdr;
  PARAM_LIST param1, param2;
  BOOLEAN c_args1, c_args2;
  TYPE type1, type2;
  TYPETAG type_tag1;

  //fill in helper vars
  type1 = ty_query_func(type, &param1, &c_args1);
  type_tag1 = ty_query(type1);
  stdr = st_lookup(id, &block);

  //if record not found, install function
  if (stdr == NULL) {
    //create record
    stdr = stdr_alloc();
    stdr->tag = FDECL;
    stdr->u.decl.type = type;
    stdr->u.decl.sc = NO_SC;
    stdr->u.decl.is_ref = FALSE;
    stdr->u.decl.v.global_func_name = global_func_name;

    //install it
    st_install(id, stdr);
  } else { //already installed
    
    //make sure tag/storage class are correct
    if(stdr->tag != GDECL || stdr->u.decl.sc != NO_SC) {
      error("Duplicate function declaration");
      return;
    } //else
    
    //find function's return type and make sure it matches
    type2 = ty_query_func(stdr->u.decl.type, &param2, &c_args2);
    if (ty_test_equality(type1, type2) != TRUE) {
      error("Incorrect return type");
      return;
    } //else
    
    //update data record
    stdr->tag = FDECL;
    stdr->u.decl.v.global_func_name = global_func_name;
  }

  //push function's id onto stack
  fi_top++;
  func_id_stack[fi_top] = id;

  //enter a new block
  st_enter_block();

  //increment stack pointer
  bo_top++;

  //initi parameter offset
  b_init_formal_param_offset();

  //check block number to see if function is local (block 1 means global)
  if (st_get_cur_block() > 2) {
    b_store_formal_param(TYPTR);
  }

  //install params
  install_local_params(param1);

  //find initial offset
  base_offset_stack[bo_top] = get_local_var_offset();
  
  //printf("bo_top: %d\n", bo_top);
  //printf("base_offset_stack[bo_top]: %d\n",base_offset_stack[bo_top]);
  
  //if return type not void, make room
  if (type_tag1 != TYVOID) {
    base_offset_stack[bo_top] -= 8;
  }
  
  //return new offset
  return base_offset_stack[bo_top];
}

//utilities
EXPR check_assign_or_proc_call(EXPR left, ST_ID id, EXPR right)
{
  char *id_str;
  int block;
  PARAM_LIST params;
  BOOLEAN check_args;
  TYPETAG func_type;
  
  id_str = NULL;

  //if right isn't null, either function return var or assignment
  if (right != NULL) {
    
      //if id is the current function's id, this is being used as a return var
      if (id == func_id_stack[fi_top] && fi_top >= 0) {

      //find function's return type
      func_type = ty_query(ty_query_func(left->type, &params, &check_args));

      //make sure return type isn't void
      if (func_type == TYVOID) {
	error("Cannot set the return value of a procedure");
	return make_error_expr();
      } //else

      return make_un_expr(SET_RETURN_OP, right);
    } //else
      
    //otherwise it's an assignment operation, so make one
    return make_bin_expr(ASSIGN_OP, left, right);
  } //else

  //now we can assume right expr is null
  if (left == NULL) {
    bug("NULL left expression given to check_assign_or_proc_call");
  }

  //if left tag is a unop, just return it back
  if (left->tag == UNOP) { 
    return left;
  } //else

  //if left expr is a function expression, just return it back
  if (left->tag == FCALL) {
    
    //make sure func type is void (procedure)
    if (ty_query(left->type) != TYVOID) { 
      error("Procedure call to non-void function");
      return make_error_expr();
    } //else
    
    return left;
  } //else

  //if left expr refers to a global id, make a fcall expression
  if ((left->tag == GID) || (left->tag == LFUN)) {
    
    //make sure expression's type is func
    if (ty_query(left->type) != TYFUNC) {
      error("Expected procedure name, saw data");
      return make_error_expr();
    } //else
    
    return make_fcall_expr(left, NULL);   
  }
  
  //if not function call, error
  if (left->tag != FCALL) {
    error("Procedure call expected");
  }
  
  //if we get this far, something went wrong, return error expr
  return make_error_expr();
}

EXPR check_assign(EXPR assign)
{
  EXPR left, right;
  TYPETAG left_tag, right_tag;
  
  //fill in vars for quick access
  left = assign->u.binop.left;
  right = assign->u.binop.right;
  
  left_tag = ty_query(left->type);
  right_tag = ty_query(right->type);

  //if either expr is an error, return error expr
  if (right_tag == TYERROR || left_tag == TYERROR) {
    return make_error_expr();
  } //else
  
  //printf("%d,%d\n", left_tag, right_tag);
  //if both tags are the same, don't do anything
  if (left_tag == right_tag) {
    return assign;
  } //else
  
  //if left expr is double, check for correct right expr type
  if (left_tag == TYDOUBLE) {
    
    //make sure right expr is compatible type
    if (right_tag != TYFLOAT && right_tag != TYSIGNEDLONGINT) {
      error("Illegal conversion");
      return make_error_expr();
    } //else
    
    //convert up if necessary
    if (right_tag == TYFLOAT || right_tag == TYSIGNEDLONGINT) {
      
      if (assign->u.binop.right->tag == INTCONST) { //int const (promote to double)
	assign->u.binop.right = make_realconst_expr(assign->u.binop.right->u.intval);
      } else { //otherwise (convert to double)
	assign->u.binop.right = make_convert_expr(assign->u.binop.right, ty_build_basic(TYDOUBLE));
      }
    }
    
    return assign;
  } //else
  
  //if left expr is float, check for correct right expr type
  if (left_tag == TYFLOAT) {
    
    //make sure right expr is compatible type
    if (right_tag != TYDOUBLE && right_tag != TYSIGNEDLONGINT) {
      error("Illegal conversion");
      return make_error_expr();
    } //else
    
    //convert to float
    assign->u.binop.right = make_convert_expr(assign->u.binop.right, ty_build_basic(TYFLOAT));

    return assign;
  } //else
  
  //convert between strings and char
  if (left_tag == TYUNSIGNEDCHAR && right->tag == STRCONST) {
    
    //make sure right expr only has one character in string
    if (strlen(right->u.strval) != 1) {
      error("Illegal conversion");
      return make_error_expr();
    } //else
    
    //upcast that one char to int
    assign->u.binop.right = make_intconst_expr(right->u.strval[0], ty_build_basic(TYSIGNEDLONGINT));

    return assign;
  } //else
  
  //if we get this far, the types aren't compatible
  error("Illegal conversion");
  return make_error_expr();
}

EXPR upcast_int_expr(EXPR expr)
{ 
  //original is int
  if (expr->tag == INTCONST) {
    expr = make_realconst_expr(expr->u.intval);
  }
  
  //original is a unary conversion with underlying int type
  if (expr->tag == UNOP && expr->u.unop.op == CONVERT_OP && expr->u.unop.operand->tag == INTCONST) {
    expr = make_realconst_expr(expr->u.unop.operand->u.intval);
  }

  return expr;
}

BOOLEAN is_lval(EXPR expr)
{
  //make sure expr is not null
  if (expr == NULL) {
    bug("Empty expression sent to is_lval.");
  }

  //get typetag from type
  TYPETAG expr_type_tag = ty_query(expr->type);

  //if LVAR, it's lval
  if (expr->tag == LVAR) {
    return TRUE;
  } //else
  
  //if tag is GID and typetag is a data type (not TYFUNC or TYERROR), expr is an lval 
  if ((expr->tag == GID ||  expr->tag == ARRAY_ACCESS) && expr_type_tag != TYFUNC && expr_type_tag != TYERROR) { //also check for array access later AG
    return TRUE;
  } //else
  
  //if type is UNOP, check operator
  if (expr->tag == UNOP) {
      //if operator is the indirection operator (^), expr is an lval 
      if (expr->u.unop.op == INDIR_OP) {
	return TRUE;
      } //else
  }

  //anything else is an rval
  return FALSE;
}

EXPR expr_fold_unop(EXPR expr)
{
  EXPR_UNOP op;
  TYPETAG expr_tag;
  EXPR sub_expr;
  TYPETAG sub_expr_tag;

  //fill in helper vars
  op = expr->u.unop.op;
  expr_tag = ty_query(expr->type);
  sub_expr = expr->u.unop.operand;
  sub_expr_tag = ty_query(sub_expr->type);

  //fold based on operation
  switch (op) {
    //ops with no folding
    case DEREF_OP:
    case UN_EOF_OP:
    case UN_EOLN_OP:
    case INDIR_OP:
    case NEW_OP:
    case DISPOSE_OP:
    case ADDRESS_OP:
    case SET_RETURN_OP:
    case CONVERT_OP:
      break;
    case NEG_OP: //negate and upcast, based on type
    case UPLUS_OP: //upcast,  on type
      if (sub_expr->tag == INTCONST) {
	expr->tag = INTCONST;
	if (op == NEG_OP) { //negate
	  expr->u.intval = -1 * sub_expr->u.intval;
	} else { //just set value
	  expr->u.intval = sub_expr->u.intval;
	}
	expr->type = ty_build_basic(TYSIGNEDLONGINT);
      } else if (sub_expr->tag == REALCONST) {
	expr->tag = REALCONST;
	if (op == NEG_OP) { //negate
	  expr->u.realval = -1 * sub_expr->u.realval;
	} else { //just set value
	  expr->u.realval = sub_expr->u.realval;
	}
	expr->type = ty_build_basic(TYDOUBLE);
      }
      break;
    case ORD_OP: //upcast to signed longint, based on type
      if (sub_expr->tag == STRCONST) {  
	if (strlen(sub_expr->u.strval) == 1) {
	  expr->tag = INTCONST;
	  expr->u.intval = sub_expr->u.strval[0];
	  expr->type = ty_build_basic(TYSIGNEDLONGINT);
	}
      } else if (sub_expr->tag == INTCONST) {  
	expr->tag = INTCONST;
	expr->u.intval = sub_expr->u.intval;
	expr->type = ty_build_basic(TYSIGNEDLONGINT);
      }
      break;
    case CHR_OP: //cast to unsigned char, based on type
      if (sub_expr->tag == INTCONST && sub_expr_tag == TYSIGNEDLONGINT) {
	expr->tag = INTCONST;
	expr->u.intval = sub_expr->u.intval;
	expr->type = ty_build_basic(TYUNSIGNEDCHAR);
      }
      break;
    case UN_PRED_OP: //decrement value and (maybe) make str const, based on type
    case UN_SUCC_OP: //increment value and (maybe) make str const, based on type
      if (sub_expr_tag == TYSIGNEDLONGINT || sub_expr_tag == TYUNSIGNEDCHAR) {
	if (sub_expr->tag == INTCONST) {
	  expr->tag = INTCONST;
	  expr->type = sub_expr->type;
	  if (op == UN_SUCC_OP) { //increment
	    sub_expr->u.intval++;
	  } else { //decrement
	    sub_expr->u.intval--;
	  }
	  expr->u.intval = sub_expr->u.intval;
	} else if (sub_expr->tag == STRCONST && strlen(sub_expr->u.strval) == 1) {
	  char *str = malloc(sizeof(char));
	  if (op == UN_SUCC_OP) { //increment
	    sub_expr->u.strval[0]++;
	  } else { //decrement
	    sub_expr->u.strval[0]--;
	  }
	  str = sub_expr->u.strval;
	  expr = make_strconst_expr(str);
	}
      }
      break;
  }

  return expr;
}

EXPR expr_fold_binop(EXPR expr)
{
  EXPR_BINOP op;
  double real_val;
  long int_val;
  TYPETAG tag;

  //fill in helper vars
  op = expr->u.binop.op;
  tag = ty_query(expr->type);

  //fold based on operation
  switch (op) {
    //ops with no folding
    case ASSIGN_OP:
      break;
    case ADD_OP: //if operands constants, add and create constant, based on type
    case SUB_OP: //if operands constants, subtract and create constant, based on type
    case MUL_OP: //if operands constants, multiply and create constant, based on type
    case DIV_OP: //if operands constants, divide and create constant, based on type
    case MOD_OP: //if operands constants, modulo and create constant, based on type
    case REALDIV_OP: //if operands constants, divide and create constant, based on type
      //make sure types are int or real
      if ((expr->u.binop.left->tag == INTCONST || expr->u.binop.left->tag == REALCONST) 
	&& (expr->u.binop.right->tag == INTCONST || expr->u.binop.right->tag == REALCONST)) {

	if (tag == TYDOUBLE || tag == TYFLOAT) { //real value
	  
	  //figure out new real value
	  //get left value
	  if (expr->u.binop.left->tag == REALCONST) {
	    real_val = expr->u.binop.left->u.realval;
	  } else {
	    real_val = expr->u.binop.left->u.intval;
	  }
	  
	  //augment real_val based on right value
	  if (expr->u.binop.right->tag == REALCONST) {
	    if (op == ADD_OP) { //add
	      real_val += expr->u.binop.right->u.realval;
	    } else if (op == SUB_OP) { //subtract
	      real_val -= expr->u.binop.right->u.realval;
	    } else if (op == MUL_OP) { //multiply
	      real_val *= expr->u.binop.right->u.realval;
	    } else if (op == DIV_OP) { //divide
	      real_val /= expr->u.binop.right->u.realval;
	    }
	  } else {
	    if (op == ADD_OP) { //add
	      real_val += expr->u.binop.right->u.intval;
	    } else if (op == SUB_OP) { //subtract
	      real_val -= expr->u.binop.right->u.intval;
	    } else if (op == MUL_OP) { //multiply
	      real_val *= expr->u.binop.right->u.intval;
	    } else if (op == DIV_OP) { //divide
	      real_val /= expr->u.binop.right->u.intval;
	    }
	  }
	  
	  //create folded expression
	  expr = make_realconst_expr(real_val);
	} else { //integer value
	  
	  //figure out new int value
	  //get left value
	  int_val = expr->u.binop.left->u.intval;

	  //augment int_val based on right value
	  if (op == ADD_OP) { //add
	    int_val += expr->u.binop.right->u.intval;
	  } else if (op == SUB_OP) { //subtract
	    int_val -= expr->u.binop.right->u.intval;
	  } else if (op == MUL_OP) { //multiply
	    int_val *= expr->u.binop.right->u.intval;
	  } else if (op == DIV_OP) { //divide
	    int_val /= expr->u.binop.right->u.intval;
	  } else if (op == MOD_OP) { //modulo
	    int_val %= expr->u.binop.left->u.intval;
	  } else if (op == REALDIV_OP) {
	    int_val /= expr->u.binop.left->u.intval;
	  }
	  
	  //create folded expression
	  expr = make_intconst_expr(int_val, ty_build_basic(TYSIGNEDLONGINT));
	}
      }
      break; 
    case EQ_OP: //if operands constants, check equality and create constant, based on type
    case LESS_OP: //if operands constants, check equality and create constant, based on type
    case LE_OP: //if operands constants, check equality and create constant, based on type
    case NE_OP: //if operands constants, check equality and create constant, based on type
    case GE_OP: //if operands constants, check equality and create constant, based on type
    case GREATER_OP: //if operands constants, check equality and create constant, based on type
      //if both left/right are integer constants
      if (expr->u.binop.left->tag == INTCONST && expr->u.binop.right->tag == INTCONST) {
	
	//figure out "BOOLEAN" value (1 or 0) based one equality
	if (op == EQ_OP) { //==
	  int_val = expr->u.binop.left->u.intval == expr->u.binop.left->u.intval;
	} else if (op == LESS_OP) { //<
	  int_val = expr->u.binop.left->u.intval < expr->u.binop.left->u.intval;
	} else if (op == LE_OP) { //<=
	  int_val = expr->u.binop.left->u.intval <= expr->u.binop.left->u.intval;
	} else if (op == NE_OP) { //!=
	  int_val = expr->u.binop.left->u.intval != expr->u.binop.left->u.intval;
	} else if (op == GE_OP) { //>=
	  int_val = expr->u.binop.left->u.intval >= expr->u.binop.left->u.intval;
	} else if (op == GREATER_OP) { //>
	  int_val = expr->u.binop.left->u.intval > expr->u.binop.left->u.intval;
	}

	//create expr for result and a convert to char
	expr = make_intconst_expr(int_val, ty_build_basic(TYSIGNEDLONGINT));
	expr = make_convert_expr(expr, ty_build_basic(TYSIGNEDCHAR));
	
	//if left/right are strings of length 1
      } else if (expr->u.binop.left->tag == STRCONST && expr->u.binop.right->tag == STRCONST 
	&& strlen(expr->u.binop.left->u.strval) == 1 
	&& strlen(expr->u.binop.right->u.strval) == 1) {

	//figure out "BOOLEAN" value (1 or 0) based one equality
	if (op == EQ_OP) { //==
	  int_val = expr->u.binop.left->u.strval[0] == expr->u.binop.left->u.strval[0];
	} else if (op == LESS_OP) { //<
	  int_val = expr->u.binop.left->u.strval[0] < expr->u.binop.left->u.strval[0];
	} else if (op == LE_OP) { //<=
	  int_val = expr->u.binop.left->u.strval[0] <= expr->u.binop.left->u.strval[0];
	} else if (op == NE_OP) { //!=
	  int_val = expr->u.binop.left->u.strval[0] != expr->u.binop.left->u.strval[0];
	} else if (op == GE_OP) { //>=
	  int_val = expr->u.binop.left->u.strval[0] >= expr->u.binop.left->u.strval[0];
	} else if (op == GREATER_OP) { //>
	  int_val = expr->u.binop.left->u.strval[0] > expr->u.binop.left->u.strval[0];
	}

	//create expr for result and a convert to char
	expr = make_intconst_expr(int_val, ty_build_basic(TYSIGNEDLONGINT));
	expr = make_convert_expr(expr, ty_build_basic(TYSIGNEDCHAR));
      }
      break;
  }

  return expr;
}

///////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
//############### DEVELOPMENT PROJECT 3 #########################\\
///////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

EXPR make_array_access_expr(EXPR expr, EXPR_LIST expr_list)
{
  TYPE array_type;
  TYPETAG type_tag_t;
  INDEX_LIST indices;
  EXPR new_expr;
  EXPR_LIST expr_t;
  
  //make sure new expression is of type array
  if (ty_query(expr->type) != TYARRAY) {
    error("Nonarray in array access expression");	
    return make_error_expr();
  } //else

  //fill in helper vars
  array_type = ty_query_array(expr->type, &indices);
  
  //check given expressions against array's indeces
  expr_t = expr_get_head(expr_list);
  indices = index_get_head(indices);
  while (expr_t != NULL && indices != NULL) {
    //deref expression if it's l-val
    if (is_lval(expr_t->expr) == TRUE) {
      expr_t->expr = make_un_expr(DEREF_OP, expr_t->expr);
    }
    
    //make sure expression is of ordinal type (longint)
    type_tag_t = ty_query(expr_t->expr->type);
    if (type_tag_t != TYUNSIGNEDLONGINT && type_tag_t != TYSIGNEDLONGINT) {
      error("Incompatible index type in array access");
      return make_error_expr();
    } //else

    //get next expression/index
    expr_t = expr_t->next;
    indices = indices->next;
  }
  
  //make sure number of expressions and indeces match
  if ((expr_t == NULL && indices != NULL) || (expr_t != NULL && indices == NULL)) {
    error("Wrong number of indices in array access");
    return make_error_expr();
  } //else

  //generate new expression node
  new_expr = malloc(sizeof(EXPR_NODE));
  new_expr->tag = ARRAY_ACCESS;
  new_expr->type = array_type;
  new_expr->u.array_access.index_list = expr_list;
  new_expr->u.array_access.gid = expr;

  return new_expr;
}

//check for error conditions in for loop
BOOLEAN check_for_loop_errors(EXPR identifier, EXPR from_limit, EXPR to_limit)
{
  TYPETAG id_type_tag;
  BOOLEAN t1, t2, t3;
  
  //make sure id is an lval
  if (is_lval(identifier) == FALSE) {
    error("For-loop control variable not an l-val");
    return FALSE;
  } //else

  //make sure id's type is an ordinal type
  id_type_tag = ty_query(identifier->type);
  if(id_type_tag != TYSIGNEDLONGINT && id_type_tag != TYUNSIGNEDCHAR && id_type_tag != TYSIGNEDCHAR) {
    error("For-loop control variable not of ordinal type");
    return FALSE;
  } //else

  //make sure types of id, from_limit, and to_limit match
  t1 = ty_test_equality(identifier->type, from_limit->type);
  t2 = ty_test_equality(identifier->type, to_limit->type);
  t3 = ty_test_equality(to_limit->type, from_limit->type);
  if(t1 == FALSE || t2 == FALSE || t3 == FALSE) {
    error("Type mismatch in for-loop control");
    return FALSE;
  } //else

  //otherwise no errors
  return TRUE;
}

VAL_LIST val_get_tail(VAL_LIST val)
{
  VAL_LIST tail;
  
  tail = val;
  while (tail != NULL && tail->next != NULL) {
    tail = tail->next;
  }
  
  return tail;
}

VAL_LIST new_case_value(TYPETAG type, long lo, long hi)
{
  VAL_LIST val_list;
  
  //make sure type is ordinal
  if (type != TYSIGNEDLONGINT && type != TYUNSIGNEDCHAR && type != TYSIGNEDCHAR) {
    error("Case expression is not of ordinal type");
  }

  val_list = malloc(sizeof(VAL_LIST_REC));
  val_list->lo = lo;
  val_list->hi = hi;
  val_list->type = type;
  
  return val_list;
}

BOOLEAN check_case_values(TYPETAG type, VAL_LIST vals, VAL_LIST prev_vals)
{
    VAL_LIST vals_t;
    VAL_LIST prev_vals_t;

    //if this is the first element, just accept
    if (prev_vals == NULL) {
      prev_vals = vals;
      return TRUE;
    } //else

    //check each item in vals list
    vals_t = vals;
    while (vals_t != NULL) {
      //make sure types of  match
      if (type != vals_t->type) {
	//error("Case constant type does not match case expression type");
	return FALSE;
      }

      //make sure values have appropriate range (warn)
      if (vals_t->lo > vals_t->hi) {
	warning("Empty range in case constant (ignored)");
      }

      //check vals against previous ones
      prev_vals_t = prev_vals;
      while (prev_vals_t != NULL) {
	  
	//make sure values don't overlap
	if ((vals_t->lo >= prev_vals_t->lo && vals_t->lo <= prev_vals_t->hi) 
	    || (vals_t->hi >= prev_vals_t->lo && vals_t->hi <= prev_vals_t->hi)) {
	  error("Overlapping constants in case statement");
	  return FALSE;
	}
	
	if ((prev_vals_t->lo >= vals_t->lo && prev_vals_t->lo <= vals_t->hi)
	    || (prev_vals_t->hi >= vals_t->lo && prev_vals_t->hi <= vals_t->hi)) {
	  error("Overlapping constants in case statement");
	  return FALSE;
	}
	
	//add value to previous list (if we're at the end)
	if (prev_vals_t->next == NULL) {
	  prev_vals_t->next = new_case_value(vals_t->type, vals_t->lo, vals_t->hi);
	  prev_vals_t->next->prev = prev_vals_t;
	  prev_vals_t = NULL;
	} else {
	  prev_vals_t = prev_vals_t->next;
	}
      }
      
      //move onto the next value
      vals_t = vals_t->next;
  }

  //if we get this far, we're good
  return TRUE;
}

BOOLEAN get_case_value(EXPR expr, long *val, TYPETAG *type)
{
  //if expression is a simple int, just give value/type
  if (expr->tag == INTCONST) {
    *type = ty_query(expr->type);
    *val = expr->u.intval;
    return TRUE;
  } else if(expr->tag == STRCONST) { //if string constant

    //first try to make it a int (if strlength is one)
    if (strlen(expr->u.strval) == 1) {
      *type = TYSIGNEDCHAR;
      *val = expr->u.strval[0];
      expr = make_intconst_expr(expr->u.strval[0], ty_build_basic(TYSIGNEDLONGINT));
      return TRUE;
    }
  }
  
  //if we get this far, non ordinal type, error
  error("Case constant has non-ordinal type");
  return FALSE;
}

BOOLEAN check_case_const(VAL_LIST vals, TYPETAG tag)
{
  VAL_LIST vals_t;
  
  //check each value
  vals_t = vals;
  while (vals_t != NULL) {
    //make sure value matches expression type tag
    if (vals_t->type != tag) {
      error("Case constant type does not match case expression type");
      return FALSE;
    }
    
    //get next value
    vals_t = vals_t->next;
  }
  
  //if we got this far, we're good
  return TRUE;
}