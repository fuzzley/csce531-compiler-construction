#
#	makefile for the Simple Expression Evaluator
#
#

CC = gcc

CFLAGS = -g

LIBS = -lfl

YACC = bison

LEX = flex

SOURCE = parse.y main.c scan.c tree.c eval.c

OBJECTS = parse.o main.o scan.o tree.o eval.o

see: $(OBJECTS)
	$(CC) $(CFLAGS) $(OBJECTS) $(LIBS) -o see

y.tab.h parse.c: parse.y
	$(YACC) -y -t -v -d parse.y
	mv y.tab.c parse.c

scan.c: scan.l
	$(LEX) -o scan.c scan.l

clean:
	-rm *.o parse.c scan.c y.output see
