
LEX = flex
YACC = bison
CFLAGS = -Wall -O0 -g

.PHONY: clean

test: test.o cclerical.tab.o cclerical.lex.o cclerical.o

# cancelling rules for cclerical.c
%.c: %.y
%.c: %.l

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

%.lex.o: override CFLAGS += -Wno-unused-function

test.o cclerical.lex.o: cclerical.tab.h cclerical.lex.h

cclerical.tab.c cclerical.tab.h: cclerical.y
	$(YACC) $(YFLAGS) $<

cclerical.lex.c cclerical.lex.h: cclerical.l
	$(LEX) $(LEXFLAGS) -o cclerical.lex.c --header-file=cclerical.lex.h $<

clean:
	$(RM) test test.o cclerical.tab.* cclerical.lex.* cclerical.o
