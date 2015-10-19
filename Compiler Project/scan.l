%{
/*
 * lex/flex input for "Pascal" lexical scanner
 *
 */

#include <ctype.h>
#include <string.h>

#include <stdbool.h>
#include "types.h"
#include "symtab.h"
#include "tree.h"
#include "encode.h"
#include "message.h"
/* defined in defs.h */
#include BACKEND_HEADER_FILE
#include "y.tab.h"

#undef yywrap

void comment(int);
void count();
char * convert_id(const char *), * convert_str_const(const char *);


YYSTYPE yylval;

%}

D			[0-9]
L			[a-zA-Z_]
E			[Ee][+-]?{D}+

%%

[ \t\f\v\r\b\n]+		{ count(); }
"{" 				{ comment(1); }
"(*"				{ comment(2); }
[Aa][Bb][Ss]			{ count(); return p_ABS; }
[Aa][Nn][Dd]			{ count(); return LEX_AND; }
[Aa][Rr][Cc][Tt][Aa][Nn]	{ count(); return p_ARCTAN; }
[Aa][Rr][Rr][Aa][Yy]		{ count(); return LEX_ARRAY; }
[Bb][Ee][Gg][Ii][Nn]		{ count(); return LEX_BEGIN; }
[Bb][Rr][Ee][Aa][Kk]		{ count(); return BREAK; }
[Cc][Aa][Ss][Ee]		{ count(); return LEX_CASE; }
[Cc][Hh][Rr]			{ count(); return p_CHR; }
[Cc][Oo][Nn][Ss][Tt]		{ count(); return LEX_CONST; }
[Cc][Oo][Nn][Tt][Ii][Nn][Uu][Ee] { count(); return CONTINUE; }
[Cc][Oo][Ss]			{ count(); return p_COS; }
[Dd][Ii][Ss][Pp][Oo][Ss][Ee]	{ count(); return p_DISPOSE; }
[Dd][Ii][Vv]			{ count(); return LEX_DIV; }
[Dd][Oo]			{ count(); return LEX_DO; }
[Dd][Oo][Ww][Nn][Tt][Oo]	{ count(); return LEX_DOWNTO; }
[Ee][Ll][Ss][Ee]		{ count(); return LEX_ELSE; }
[Ee][Nn][Dd]			{ count(); return LEX_END; }
[Ee][Oo][Ff]			{ count(); return p_EOF; }
[Ee][Oo][Ll][Nn]		{ count(); return p_EOLN; }
[Ee][Xx][Pp]			{ count(); return p_EXP; }
[Ee][Xx][Tt][Ee][Rr][Nn][Aa][Ll]	{ count(); return LEX_EXTERNAL; }
[Ff][Aa][Ll][Ss][Ee]		{ count(); return p_FALSE; }
[Ff][Ii][Ll][Ee]		{ count(); return LEX_FILE; }
[Ff][Oo][Rr]			{ count(); return LEX_FOR; }
[Ff][Oo][Rr][Ww][Aa][Rr][Dd]	{ count(); return LEX_FORWARD; }
[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]	{ count(); return LEX_FUNCTION; }
[Gg][Ee][Tt]			{ count(); return p_GET; }
[Gg][Oo][Tt][Oo]		{ count(); return LEX_GOTO; }
[Ii][Ff]			{ count(); return LEX_IF; }
[Ii][Nn]			{ count(); return LEX_IN; }
[Ii][Nn][Pp][Uu][Tt]		{ count(); return p_INPUT; }
[Ii][Ss]			{ count(); return LEX_IS; }
[Ll][Aa][Bb][Ee][Ll]		{ count(); return LEX_LABEL; }
[Ll][Nn]			{ count(); return p_LN; }
[Mm][Aa][Xx][Ii][Nn][Tt]	{ count(); return p_MAXINT; }
[Mm][Oo][Dd]			{ count(); return LEX_MOD; }
[Nn][Ee][Ww]			{ count(); return p_NEW; }
[Nn][Ii][Ll]			{ count(); return LEX_NIL; }
[Nn][Oo][Tt]			{ count(); return LEX_NOT; }
[Oo][Dd][Dd]			{ count(); return p_ODD; }
[Oo][Ff]			{ count(); return LEX_OF; }
[Oo][Rr]			{ count(); return LEX_OR; }
[Oo][Rr][Dd]			{ count(); return p_ORD; }
[Oo][Uu][Tt][Pp][Uu][Tt]	{ count(); return p_OUTPUT; }
[Pp][Aa][Cc][Kk]		{ count(); return p_PACK; }
[Pp][Aa][Cc][Kk][Ee][Dd]	{ count(); return LEX_PACKED; }
[Pp][Aa][Gg][Ee]		{ count(); return p_PAGE; }
[Pp][Rr][Ee][Dd]		{ count(); return p_PRED; }
[Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee]	{ count(); return LEX_PROCEDURE; }
[Pp][Rr][Oo][Gg][Rr][Aa][Mm]	{ count(); return LEX_PROGRAM; }
[Pp][Uu][Tt]			{ count(); return p_PUT; }
[Rr][Ee][Aa][Dd]		{ count(); return p_READ; }
[Rr][Ee][Aa][Dd][Ll][Nn]	{ count(); return p_READLN; }
[Rr][Ee][Cc][Oo][Rr][Dd]	{ count(); return LEX_RECORD; }
[Rr][Ee][Pp][Ee][Aa][Tt]	{ count(); return LEX_REPEAT; }
[Rr][Ee][Ss][Ee][Tt]		{ count(); return p_RESET; }
[Rr][Ee][Ww][Rr][Ii][Tt][Ee]	{ count(); return p_REWRITE; }
[Rr][Oo][Uu][Nn][Dd]		{ count(); return p_ROUND; }
[Ss][Ee][Tt]			{ count(); return LEX_SET; }
[Ss][Ii][Nn]			{ count(); return p_SIN; }
[Ss][Qq][Rr]			{ count(); return p_SQR; }
[Ss][Qq][Rr][Tt]		{ count(); return p_SQRT; }
[Ss][Uu][Cc][Cc]		{ count(); return p_SUCC; }
[Tt][Hh][Ee][Nn]		{ count(); return LEX_THEN; }
[Tt][Oo]			{ count(); return LEX_TO; }
[Tt][Rr][Uu][Ee]		{ count(); return p_TRUE; }
[Tt][Rr][Uu][Nn][Cc]		{ count(); return p_TRUNC; }
[Tt][Yy][Pp][Ee]		{ count(); return LEX_TYPE; }
[Uu][Nn][Pp][Aa][Cc][Kk]	{ count(); return p_UNPACK; }
[Uu][Nn][Tt][Ii][Ll]		{ count(); return LEX_UNTIL; }
[Uu][Ss][Ee][Ss]		{ count(); return LEX_USES; }
[Vv][Aa][Rr]			{ count(); return LEX_VAR; }
[Ww][Hh][Ii][Ll][Ee]		{ count(); return LEX_WHILE; }
[Ww][Ii][Tt][Hh]		{ count(); return LEX_WITH; }
[Ww][Rr][Ii][Tt][Ee]		{ count(); return p_WRITE; }
[Ww][Rr][Ii][Tt][Ee][Ll][Nn]	{ count(); return p_WRITELN; }
[Xx][Oo][Rr]			{ count(); return LEX_XOR; }
{L}({L}|{D})*			{
				    count();
				    yylval.y_string = convert_id(yytext);
				    return LEX_ID;
				}
{D}+				{
				    count();
				    yylval.y_int = strtol(yytext,0,10);
				    return LEX_INTCONST;
				}
{D}+"."{D}+({E})?      		{
				    count();
				    yylval.y_real = strtod(yytext,0);
				    return LEX_REALCONST;
				}
"'"([^'\r\n\f\v\b]|"''")*"'"	{
				    count();
				    yylval.y_string= convert_str_const(yytext);
				    return LEX_STRCONST;
				}
"(."				{ count(); return '['; }
".)"				{ count(); return ']'; }
".."				{ count(); return LEX_RANGE; }
"..."				{ count(); return LEX_ELLIPSIS; }
":="				{ count(); return LEX_ASSIGN; }
"<="				{ count(); return LEX_LE; }
">="				{ count(); return LEX_GE; }
"<>"				{ count(); return LEX_NE; }
"><"				{ count(); return LEX_SYMDIFF; }
"=>"				{ count(); return LEX_RENAME; }
"**"				{ count(); return LEX_POWER; }
"+>"				{ count(); return LEX_CEIL_PLUS; }
"->"				{ count(); return LEX_CEIL_MINUS; }
"*>"				{ count(); return LEX_CEIL_MULT; }
"/>"				{ count(); return LEX_CEIL_DIV; }
"+<"				{ count(); return LEX_FLOOR_PLUS; }
"-<"				{ count(); return LEX_FLOOR_MINUS; }
"*<"				{ count(); return LEX_FLOOR_MULT; }
"/<"				{ count(); return LEX_FLOOR_DIV; }
.				{ count(); return yytext[0]; }

%%


int yywrap()
{
    return 1;
}

static int column = 0;

void comment(int comment_type)
{
    char c, c1;
    int is_star = 0;

    column += comment_type==1 ? 1 : 2;  /* for '{' (type 1) or '(*' (type 2) */

    while (1)
    {
	c = input();

	if (c == 0)
	    error("open comment at EOF");

	if (c == '\n')
	{
	    column = 0;
	    b_lineno_comment(++yylineno);
	}
	else if (c == '\t')
	    column += 8 - (column % 8);
	else
	    column++;

	if (comment_type == 1 && c == '}')
	    return;

	if (comment_type == 2 && is_star && c == ')')
	    return;

	if (comment_type == 2)
	    is_star = (c == '*');
    }
}

void count()
{
    int i;

    for (i = 0; yytext[i] != '\0'; i++)
    {
	if (yytext[i] == '\n')
	{
	    b_lineno_comment(++yylineno);
	    column = 0;
	}
	else if (yytext[i] == '\t')
	    column += 8 - (column % 8);
	else
	    column++;
    }
}


int digit(char c)
{
    if (isdigit(c))
	return c - '0';
    if (isxdigit(c))
	return toupper(c) - 'A' + 10;
    bug("Non-hex digit found: %c", c);
}


/* Determines if the given character can legally follow a backslash
   in a Pascal string constant */
BOOLEAN is_escchar(char c)
{
    switch (c)
    {
    case '\\': case 'a' : case 'b' : case 'f' : case 'n' :
    case 'r' : case 't' : case 'v' : case 'x' :
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
	return TRUE;
    default:
	return FALSE;
    }

    bug("is_escchar: failed to return inside switch");
}


char * convert_id(const char * s)
{
    char *str = st_save_string(s);
    char *start = str;
    if (!*str)
	bug("Empty identifier");
    *str = toupper(*str);
    for (str++; *str; str++)
	*str = tolower(*str);
    return start;
}

char * convert_str_const(const char * str)
{
    /* We convert the string to C/assembler source for the sake of gcc.
       This requires converting '' into ' and " into \" */
    int len = 0;
    BOOLEAN is_escaped = FALSE, is_quote = FALSE;
    const char *p;
    char *buf, *q;

    /* First, find the length of the converted string, and detect errors */
    if (*str++ != '\'')
	bug("Malformed string constant: no initial single quote");

    for (p = str+1; *p != '\0'; p++,len++)
    {
	if (*p == '\\' && !is_escaped)
	    is_escaped = TRUE;
	else if (is_escaped)
	{
	    if (is_escchar(*p))
		is_escaped = FALSE;
	    else
	    {
		error("Illegal escape sequence in string constant");
		len = 0;
		break;
	    }
	}
	else if (*p == '"')
	    len++;
	else if (*p == '\'')
	{
	    if (is_quote)
		len--;
	    is_quote = !is_quote;
	}
    }

    /* The final single quote should turn is_quote on. */
    if (!is_quote)
	bug("convert_str_const: malformed string");

    /* Allocate new memory for the converted string, sans quotes. */

    q = buf = (char *)malloc(sizeof(char) * (len + 1));

    if (!buf)
	fatal("convert_str_const: out of memory");

    /* str is pointing to the first char after the initial quote. */
    /* Loop invariant: p points to the next free space in buf. */
    while (0 < len)
	if (*str == '\'') {
	    if (*(str+1) == '\'') {
		*q++ = '\'';
		str += 2;
		len--;
	    }
	    else
		break;
	}
	else if (*str == '"') {
	    *q++ = '\\';
	    *q++ = '"';
	    str++;
	    len -= 2;
	}
	else
	{
	    *q++ = *str++;
	    len--;
	}

    if (len != 0)
	bug("convert_str_const: string length calculated incorrectly");

    *q = '\0';

    return buf;
}
