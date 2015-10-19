tokens : lex.yy.c
	gcc lex.yy.c -o tokens -lfl

lex.yy.c : tokens.lex
	flex tokens.lex

clean :
	rm tokens lex.yy.c
