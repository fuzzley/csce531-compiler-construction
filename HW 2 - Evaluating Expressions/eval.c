#include "eval.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define EXPRS_ARR_RESIZE_BY 300

static int val_tab[26];

static TN *exprs_arr = NULL;
static int exprs_len = 0;

void exprs_arr_resize(int size)
{
	int i;

	TN *temp = exprs_arr;
	int temp_size = exprs_len;

	exprs_len = size;
	exprs_arr = (TN *)malloc(exprs_len * sizeof(TN));
	for (i = 0; i < exprs_len; i++) {
		exprs_arr[i] = NULL;
	}

	// This only occurs on the initial sizing, with empty dictionary
	if (temp == NULL) {
		return;
	}

	for (i = 0; i < temp_size; i++) {
		exprs_arr[i] = temp[i];
	}
	free(temp);
}

void save_subtree(TN sub_tree)
{
	if (n_expressions + 1 > exprs_len) {
		exprs_arr_resize(exprs_len + EXPRS_ARR_RESIZE_BY);	
	}

	exprs_arr[n_expressions] = sub_tree;
	n_expressions++;
}

TN get_subtree(int index)
{
	return exprs_arr[index];
}

int get_n_expressions()
{
	return n_expressions;
}

//variables
int get_var_val(char name)
{
	return val_tab[name - 'A'];
}

void set_var_val(char name, int val)
{
	val_tab[name - 'A'] = val;
}

//tree node eval
int eval_tn_const(TN tn)
{
	return tn->info.const_val;
}

int eval_tn_var(TN tn)
{
	return get_var_val(tn->info.var_name);
}

int eval_tn_unop(TN tn)
{
	tn_val_state state = tn->cached_val_state;

	//check for cached val
	if (state == TN_EVALUATED) {
		return tn->cached_val;	
	} else if (state == TN_EVALUATING) { //check for infinite eval loop
		fprintf(stderr, "%s\n", "Found circular reference while evaluating expression.");
		exit(1);
	}
	//set state to evaluating
	tn->cached_val_state = TN_EVALUATING;

	//evaluate
	int val = eval_tree(tn->info.unop.right);
	if (tn->info.unop.op == U_MINUS) {
		val *= -1;
	}

	//change state to original one
	tn->cached_val_state = state;

	return val;
}

int eval_tn_binop(TN tn)
{
	tn_val_state state = tn->cached_val_state;

	//check for cached val
	if (state == TN_EVALUATED) {
		return tn->cached_val;	
	} else if (state == TN_EVALUATING) { //check for infinite eval loop
		fprintf(stderr, "%s\n", "Found circular reference while evaluating expression.");
		exit(1);
	}
	//set state to evaluating
	tn->cached_val_state = TN_EVALUATING;

	int val = -1;

	int left_val = eval_tree(tn->info.binop.left);
	int right_val = eval_tree(tn->info.binop.right);

	//evaluate
	if (tn->info.binop.op == BI_PLUS) {
		val = left_val + right_val;
	} else if (tn->info.binop.op == BI_MINUS) {
		val = left_val - right_val;
	} else if (tn->info.binop.op == BI_MULTIPLY) {
		val = left_val * right_val;
	} else if (tn->info.binop.op == BI_DIVISION) {
		val = left_val / right_val;
	} else if (tn->info.binop.op == BI_MODULO) {
		val = left_val % right_val;
	}

	//change state to original state
	tn->cached_val_state = state;

	return val;
}

//tree
int eval_tree(TN tree)
{
	tn_type type = tree->type;
	int val = -1;

	if (type == TN_CONST) {
		val = eval_tn_const(tree);
	} else if (type == TN_VAR) {
		val = eval_tn_var(tree);
	} else if (type == TN_UNOP) {
		val = eval_tn_unop(tree);		
	} else if (type == TN_BINOP) {
		val = eval_tn_binop(tree);
	}

	return val;
}

void refresh_tree_cache_val(TN tn, bool recursive)
{
	tn->cached_val_state = TN_UNEVALUATED;
	tn->cached_val = eval_tree(tn);
	tn->cached_val_state = TN_EVALUATED;

	if (recursive == true) {
		tn_type type = tn->type;
		if (type == TN_UNOP) {
			refresh_tree_cache_val(tn->info.unop.right, recursive);		
		} else if (type == TN_BINOP) {
			refresh_tree_cache_val(tn->info.binop.left, recursive);
			refresh_tree_cache_val(tn->info.binop.right, recursive);
		}
	}
}

void refresh_all_tree_cache_val(bool recursive)
{	
	int i;
	for (i = 0; i < n_expressions; i++) {
		refresh_tree_cache_val(exprs_arr[i], recursive);
	}
}
