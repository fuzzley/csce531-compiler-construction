digit		[0-9]
int_const	[+-]?{digit}+
exponent	[Ee][+-]?{int_const}
real_const	{int_const}"."{int_const}{exponent}?
alpha		[A-Za-z_]
alpha_num	{alpha}|{digit}
str_const	\"(\\.|[^"])*\"
identifier	{alpha}{alpha_num}*
define		"#define"
%{
	#include "dictionary.h"

	boolean in_define_statement = FALSE;
	char *in_define_key = NULL;
%}
%%
\r\n		{
	putchar('\n');
}
\n		{
	if (in_define_statement == TRUE) {
		in_define_statement = FALSE;
	} else {
		REJECT;	
	}
}
{define}	{
	debug("found #define\n");
	in_define_statement = TRUE;
}
{identifier} {
	if (in_define_statement == TRUE) {
		if (in_define_key == NULL) {
			in_define_key = str_dupl(yytext);
			debug1("found #define key %s\n", in_define_key);
		}
		else {
			char *key = str_dupl(in_define_key);
			char *value = str_dupl(yytext);

			//in_define_statement = FALSE;
			free((void *)in_define_key);
			in_define_key = NULL;
			
			debug2("found #define id w/ key %s, value %s\n", key, value);

			process_define_identifier(key, value);
		}
	} else {
		char *key = str_dupl(yytext);
		if (output_substitution(stdout, key) != TRUE) {
			REJECT;
		}
	}
}
{int_const} {
	if (in_define_statement == TRUE) {
		char *key = str_dupl(in_define_key);
		long value = atol(yytext);

		//in_define_statement = FALSE;
		free((void *)in_define_key);
		in_define_key = NULL;
		
		debug2("found #define int w/ key %s, value %ld\n", key, value);

		process_define_int_const(key, value);
	} else {
		REJECT;
	}
}
{str_const} {
	if (in_define_statement == TRUE) {
		char *key = str_dupl(in_define_key);
		char *value = str_dupl(yytext);

		//in_define_statement = FALSE;
		free((void *)in_define_key);
		in_define_key = NULL;

		debug2("found #define str w/ key %s, value %s\n", key, value);

		process_define_str_const(key, value);
	} else {
		REJECT;
	}
}
%%
int yywrap()
{
	return 1; //just stop (0 to keep going)
}
int main(int argc, char **args)
{
	if (argc > 1) {
		FILE * fin = fopen(args[1], "r");
		if (fin != NULL) {
			yyin = fin;
		}
	}
	init_dict();
	yylex();
	if (DEBUG) {
		print_contents();
	}
	return 0;
}
