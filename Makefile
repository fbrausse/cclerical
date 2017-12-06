
LEX = flex
YACC = bison
CFLAGS = -Wall

.PHONY: clean

test: test.o clerical.tab.o clerical.lex.o

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

test.o clerical.lex.o: clerical.tab.h

clerical.tab.c clerical.tab.h: clerical.y
	$(YACC) $<

clerical.lex.c: clerical.l
	$(LEX) -o $@ $<

clean:
	$(RM) test test.o clerical.tab.* clerical.lex.*
