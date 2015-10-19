/****************************************************************************/
/* 
	definitions to support types.c
*/

#ifndef TYPES_H
#define TYPES_H

#include "defs.h"

/* "users" should not need to know details of type representation */
typedef struct type_rec *TYPE;

/* returned by ty_query */
typedef enum {
    TYVOID, TYFLOAT, TYDOUBLE, TYLONGDOUBLE, TYSIGNEDLONGINT,
    TYSIGNEDSHORTINT, TYSIGNEDINT, TYUNSIGNEDLONGINT,
    TYUNSIGNEDSHORTINT, TYUNSIGNEDINT, TYUNSIGNEDCHAR,
    TYSIGNEDCHAR, TYSTRUCT, TYUNION, TYENUM, TYARRAY, TYSET,
    TYFUNC, TYPTR, TYBITFIELD, TYSUBRANGE, TYERROR
} TYPETAG;

/* compact representation of storage class specifiers */
typedef enum {
    STATIC_SC, EXTERN_SC, AUTO_SC, TYPEDEF_SC, REGISTER_SC, NO_SC
} STORAGE_CLASS;

/*
Parameter list structure IS manipulated directly by "users". types.c
makes no assumption about the parameter list except all PARAMs should
be linked together by the "next" field, and the last PARAM in that linked
list should have a NULL "next" field.  In particular, the order of the
PARAMS is flexible and "prev" field can be used for any purpose (types.c
doesn't use it).
*/
typedef struct param {
    ST_ID id;
    TYPE type;
    STORAGE_CLASS sc;
    BOOLEAN err;
    BOOLEAN is_ref;
    struct param *next, *prev;
} PARAM, *PARAM_LIST;

/*
Member list structure IS manipulated directly by "users". types.c
makes no assumption about the member list except all MEMBERS should
be linked together by the "next" field, and the last MEMBER in that linked
list should have a NULL "next" field.  In particular, the order of the
MEMBERS is flexible and "prev" field can be used for any purpose (types.c
doesn't use it).
*/
typedef struct member {
    ST_ID id;
    TYPE type;
    STORAGE_CLASS sc;
    BOOLEAN err;
    struct member *next, *prev;
} MEMBER, *MEMBER_LIST;

/*
Index list structure IS manipulated directly by "users".  types.c
makes no assumption about the index list except all INDEXes should
be linked together by the "next" field, and the last INDEX in that linked
list should have a NULL "next" field.  In particular, the order of the
INDEXes is flexible and "prev" field can be used for any purpose (types.c
doesn't use it).
*/
typedef struct index {
    TYPE type;
    struct index *next, *prev;
} INDEX, *INDEX_LIST;



/* Routines to manipulate "TYPE" */

void ty_types_init(void);
    /*
    This routine initializes the types module data structures.
    */


TYPETAG ty_query(TYPE type);
    /*
    This routine returns the actual type or tag value of the "TYPE".
    */


BOOLEAN ty_test_equality(TYPE type1, TYPE type2);
    /*
    This routine takes 2 parameters both of type "TYPE"
    and returns TRUE if the 2 TYPES are equal (compatible)
    and FALSE otherwise.
    */

TYPE ty_strip_modifier(TYPE typein);
    /*
    This routine takes 1 parameter of type "TYPE". If the 
    tag of the type is a modifier (ex. ptr,array,func,set,subrange)
    the routine will strip off the modifier and return
    the remainder of the type. If the type sent in is not 
    a modifier, it returns NULL.
    */

TYPE ty_build_basic(TYPETAG tag);
    /*
    This routine take 1 parameter, a type tag of a basic type
    It builds a "TYPE" out of the basic tag passed in.
    */

TYPE ty_build_ptr(ST_ID id, TYPE object);
    /*
    This routine takes 2 parameters, exactly one of which is nonNULL: if
    used in the context of a pointer to an identifier, then the first
    parameter is nonNULL and is the enrollment papers for the unresolved
    identifier to be pointed to; if used in the context of a pointer to
    a procedure/function, then the second parameter is nonNULL and is the
    TYPE of the function to be pointed to.  In either case, ty_build_ptr
    constructs and returns a new pointer TYPE.  In the first case, the
    object type of the pointer is unresolved (object type is NULL),
    the identifier is included with the new pointer, and the pointer node
    is placed on a linked list of unresolved pointer types.  Each of these
    should eventually be resolved to point to actual TYPEs by calling
    ty_resolve_ptr().  In the second case, the object type becomes the
    given procedure/function type, and the pointer node is not placed
    on the unresolved list.
    */

TYPE ty_build_array(TYPE object, INDEX_LIST indices);
    /*
    This routine takes 2 parameters: a "TYPE", and a list of indices
    of the array.  It constructs a new TYPE which it returns which will
    contain the array indices and be "array of object".
    */

TYPE ty_build_set(TYPE object);
    /*
    This routine takes 1 parameter: the "TYPE" of the elements of a set.
    It returns a new TYPE which will be "set of object".
    */

TYPE ty_build_struct(MEMBER_LIST members);
    /*
    This routine takes a member list
    and returns a TYPE for the structure containing
    its member list.
    */

TYPE ty_build_union(MEMBER_LIST members);
    /*
    This routine takes a member list and returns a TYPE for the
    union containing its member list.
    */

TYPE ty_build_enum(unsigned int num_values);
    /*
    This routine takes one argument, the number of values of an
    enumerated type, and returns the TYPE enumeration.
    */

TYPE ty_build_func(TYPE object, PARAM_LIST params, BOOLEAN check_args);
    /*
    This routine takes 3 parameters: a previously constructed
    object of type "TYPE", a parameter list for the function, and a
    BOOLEAN flag whether to check that actual arguments correspond to
    formal arguments in a function call.  The routine builds a "TYPE"
    for the function and stores the functions parameters and flag.
    It returns the newly constructed type.
    */

TYPE ty_build_subrange(TYPE object, long low, long high);
    /*
    This routine takes 3 parameters: a costructed object of type "TYPE"
    (the base type of the subrange to be constructed--an ordinal type),
    the ordinal value of the lower limit, and the ordinal value of the
    upper limit of the subrange.  The routine builds and returns
    a constructed subrange type from the values given.
    */

TYPE ty_query_func(TYPE type, PARAM_LIST *params, BOOLEAN *check_args);
    /*
    This routine takes one input parameter which must be a function TYPE.
    The routine returns through the second parameter the parameter list
    itself, and through the third parameter the argument check flag.
    The function return value is the TYPE of the return value of the
    input function type.
    */

TYPE ty_query_array(TYPE type, INDEX_LIST *indices);
    /*
    This routine takes 1 input parameter which must be an array TYPE.
    It returns through the second parameter the indices of the
    array.  Through the function return value it returns the type of
    the array elements.
    */

TYPE ty_query_set(TYPE type);
    /*
    This routine takes 1 parameter which must be a set TYPE.
    It returns the element type of the set.
    */

TYPE ty_query_ptr(TYPE type, ST_ID *id, TYPE *next);
    /*
    This routine takes 1 input parameter "type" which must be a pointer
    type node.  It returns the TYPE pointed to, if there is one.  If not,
    the object type is unresolved, and NULL is returned.  In the case of
    an unresolved pointer type, the output parameter id is set to the
    original enrollment papers passed to ty_build_ptr() when the pointer
    was first created, and the output parameter next is set to the next
    node in the linked list of current unresolved pointer types.  If the
    pointer is resolved, both id and next parameters are set to NULL.
    */

TYPE ty_query_subrange(TYPE type, long *low, long *high);
    /*
    This routine takes 1 input parameter which must be a subrange
    type.  It returns through the parameter list the ordinal values
    of the lower and upper limits of the subrange.  It returns as
    the function return value the base type of the subrange.
    */

unsigned int ty_query_enum(TYPE type);
    /*
    This routine takes 1 parameter, which must be an enumerated type.
    It returns the number of values in the enumeration.
    */

TYPE ty_assign_members(TYPE type, MEMBER_LIST members);
    /*
    This routine takes 2 parameters: a previously constructed
    "TYPE" and a struct or union member list. If the input type
    is of type struct, union or domain, the routine assigns the member
    list to the input type. If the type is not one of these types
    it is an error.
    */

MEMBER_LIST ty_retrieve_members(TYPE type);
    /*
    This routine takes 1 parameter which must be a struct or union TYPE.
    It returns the member list of the given struct/union.
    */

TYPE ty_get_unresolved(void);
    /*
    This routine returns the global linked list of currently unresolved
    pointer type nodes.  The nodes are simply linked via their next fields,
    which can be obtained by calling ty_query_ptr().  The final next field
    is NULL.  As a side effect, the global list is reinitiallized to be
    empty.
    */

BOOLEAN ty_resolve_ptr(TYPE ptr_type, TYPE object_type);
    /*
    This routine takes 2 parameters: a pointer-to-unresolved type (ptr_type)
    and the new resolved type (object_type).  Changes the object type
    of the pointer to the new type.  Returns TRUE if successful, and
    FALSE if there is a problem (nonpointer, unresolved object_type, etc.)
    */

void ty_print_type(TYPE typein);
    /*
    This function takes 1 parameter, a TYPE, and just prints out in
    English the meaning of the "C" TYPE.
    */

void ty_print_class(STORAGE_CLASS sc);
    /*
    This function takes 1 parameter, a storage class, and just prints out in
    English the meaning of the "C" storage class.
    */

void ty_print_typetag(TYPETAG tag);
    /*
    This function takes 1 parameter, a TYPE tag, and just prints out
    in English the meaning of the TYPE tag.
    */

void ty_print_paramlist(PARAM_LIST params);
    /*
    This function takes 1 parameter, a pointer to a function TYPE prototype
    parameter list, and prints out in English the meaning of each parameter
    of that list.
    */

void ty_print_memlist(MEMBER_LIST params);
    /*
    This function takes 1 parameter, a pointer to a struct/union TYPE member
    list, and prints out in English the meaning of each member of that list.
    */

void ty_print_indexlist(INDEX_LIST indices);
    /*
    This function takes 1 parameter, a pointer to a list of array indices,
    and prints out in English the meaning of each index of that list.
    */

#endif
