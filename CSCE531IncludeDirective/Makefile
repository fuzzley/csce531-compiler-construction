preproc:
	flex -o src/preproc.lex.linux.c src/preproc.lex && gcc -o babycpp src/preproc.lex.linux.c src/dictionary.c -lc

flex:
	flex -o src/preproc.lex.linux.c src/preproc.lex

c_all:
	gcc -o babycpp src/preproc.lex.linux.c src/dictionary.c -lc

c_dictionary:
	gcc -c src/dictionary.c -lc

clean:
	rm *.o --f && rm babycpp --f
