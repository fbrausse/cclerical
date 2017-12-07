
LEX = flex
YACC = bison
CFLAGS = -Wall

.PHONY: clean

test: test.o clerical.tab.o clerical.lex.o clerical.o

%.c: %.y
%.c: %.l

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

test.o clerical.lex.o: clerical.tab.h clerical.lex.h

clerical.tab.c clerical.tab.h: clerical.y
	$(YACC) $(YFLAGS) $<

clerical.lex.c clerical.lex.h: clerical.l
	$(LEX) $(LEXFLAGS) -o clerical.lex.c --header-file=clerical.lex.h $<

clean:
	$(RM) test test.o clerical.tab.* clerical.lex.* clerical.o
