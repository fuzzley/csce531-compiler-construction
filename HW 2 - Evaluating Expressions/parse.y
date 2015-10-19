/* Simple Expression Evaluator */

/* parse.y
 *
 * Grammer rules for bison.
 * Includes the lexical analyzer routine yylex().
 */


%{
#include "eval.h"
#include <ctype.h>
#include <stdio.h>
#define YYDEBUG 1
%}

%union {
	int y_const;
	TN y_tree;
	char y_var;
}

%token CONST VAR

%type <y_tree> expr term factor
%type <y_const> assign line eval CONST
%type <y_var> VAR
	
%%

session
    : { print_welcome(); }
      eval
    ;

eval
    : eval line
    | /* empty */
    ;

line
    : assign '\n'		{ printf("%d\n%d: ", $1, (get_n_expressions() + 1)); }
    ;

assign
    : VAR '=' expr		{
					int val = eval_tree($3);
					set_var_val($1, val);

					save_subtree($3);
					refresh_all_tree_cache_val(false);

					$$ = val;
				}
    | expr			{				
					save_subtree($1);
					refresh_tree_cache_val($1, false);

					$$ = eval_tree($1);
				}
    ;

expr
    : expr '+' term		{ $$ = make_binop_node(BI_PLUS, $1, $3); }
    | expr '-' term		{ $$ = make_binop_node(BI_MINUS, $1, $3); }
    | term
    | '-' term			{ $$ = make_unop_node(U_MINUS, $2); }
    | '+' term			{ $$ = make_unop_node(U_PLUS, $2); }
    ;

term
    : term '*' factor		{ $$ = make_binop_node(BI_MULTIPLY, $1, $3); }
    | term '/' factor		{ $$ = make_binop_node(BI_DIVISION, $1, $3); }
    | term '%' factor		{ $$ = make_binop_node(BI_MODULO, $1, $3); }
    | factor
    ;

factor
    : '(' expr ')'		{ $$ = $2; }
    | CONST			{ $$ = make_const_node($1); }
    | VAR			{ $$ = make_var_node($1); }
    | '#' factor		{
					int val = eval_tree($2);
					int n_expr = get_n_expressions();
					if (val >= 1 && val <= n_expr) {
						$$ = get_subtree(val - 1);
					} else {
						fprintf(stderr, "#%d is must be between #1 and #%d.\n", val, n_expr);
						exit(1);
					}
				}
    ;

%%

yyerror(char *s)
{
    fprintf(stderr, "%s\n", s);
}

print_welcome()
{
    printf("Welcome to the Simple Expression Evaluator.\n");
    printf("Enter one expression per line, end with ^D\n\n1: ");
}
