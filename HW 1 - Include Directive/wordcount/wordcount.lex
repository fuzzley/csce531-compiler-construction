/* A simple line, word, and character counter for standard input */

    int numChars = 0, numWords = 0, numLines = 0;

%%

\n		{ ++numLines; ++numChars; }

[ \t]		{ ++numChars; }

[^ \t\n]+	{ ++numWords; numChars += yyleng; }

%%

main()
{
    yylex();
    printf( "%d lines, %d words, %d characters\n",
	    numLines, numWords, numChars );
}
