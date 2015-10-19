wordcount : lex.yy.c
	gcc lex.yy.c -o wordcount -lfl

lex.yy.c : wordcount.lex
	flex wordcount.lex

clean :
	rm wordcount lex.yy.c
