%{
#include "tree.h"
#include "y.tab.h"

#define DEBUG 0

#define debug(s)      if (DEBUG) fprintf(stderr,s)
#define debug1(s,t)   if (DEBUG) fprintf(stderr,s,t)
#define debug2(s,t,u) if (DEBUG) fprintf(stderr,s,t,u)

char * str_dupl(const char *s);
%}
digit		[0-9]
int_const	{digit}+
alpha		[A-Za-z]
%%
{alpha}		{
  yylval.y_var = toupper(yytext[0]);   /* case insensitive */
  return VAR;
}
{int_const}	{
  yylval.y_const = atoi(yytext);
  return CONST;
}
[ \t]+		/* empty */
(.|\n)		{
  return (char)yytext[0];
}
%%
char * str_dupl(const char *s) /* make a duplicate of s */
{
	char *p;
	p = (char *)malloc(strlen(s) + 1); /* +1 for '\0' */
	if (p != NULL)
		strcpy(p, s);
	return p;
}
