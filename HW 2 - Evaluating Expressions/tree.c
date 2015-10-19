#include "tree.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

TN make_binop_node(binop_type op_type, TN left, TN right)
{
	TN new_node;

	new_node = (TN)malloc(sizeof(TREE_NODE));
	assert(new_node != NULL);

	new_node->type = TN_BINOP;
	new_node->info.binop.op = op_type;
	new_node->info.binop.left = left;
	new_node->info.binop.right = right;

	new_node->cached_val_state = TN_UNEVALUATED;

	return new_node;
}

TN make_unop_node(unop_type op_type, TN right)
{
	TN new_node;

	new_node = (TN)malloc(sizeof(TREE_NODE));
	assert(new_node != NULL);

	new_node->type = TN_UNOP;
	new_node->info.unop.op = op_type;
	new_node->info.unop.right = right;

	new_node->cached_val_state = TN_UNEVALUATED;

	return new_node;
}

TN make_const_node(int val)
{
	TN new_node;

	new_node = (TN)malloc(sizeof(TREE_NODE));
	assert(new_node != NULL);

	new_node->type = TN_CONST;
	new_node->info.const_val = val;

	new_node->cached_val_state = TN_UNEVALUATED;

	return new_node;
}

TN make_var_node(char name)
{
	TN new_node;

	new_node = (TN)malloc(sizeof(TREE_NODE));
	assert(new_node != NULL);

	new_node->type = TN_VAR;
	new_node->info.var_name = name;

	new_node->cached_val_state = TN_UNEVALUATED;

	return new_node;
}
