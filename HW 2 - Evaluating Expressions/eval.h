#include "tree.h"

static int n_expressions = 0;
int get_n_expressions();

int eval_tree(TN);
void save_subtree(TN);
TN get_subtree(int);

void refresh_tree_cache_val(TN, bool);
void refresh_all_tree_cache_val(bool);

int get_var_val(char);
void set_var_val(char, int);
