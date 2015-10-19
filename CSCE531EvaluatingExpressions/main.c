/* Simple Expression Evaluator */

/* main.c */

int yyparse();

int main()
{
	extern int yydebug;
	yydebug = 0;         /* change to 1 to see debugging info */
	yyparse();
	printf("%s", "\n");
	return 0;
}
