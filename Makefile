
LEXFLAGS = --batch
YFLAGS = -v
CFLAGS = -Wall -Wextra -O0 -g -Wpedantic
CCL_VERSION = 0.1

OBJS = \
	ccl.o \
	ccl-ssa.o \
	ccl-ast-irram.o \
	cclerical.tab.o \
	cclerical.lex.o \
	cclerical.o \

.PHONY: clean examples

ccl: $(OBJS)

examples: ccl
	$(MAKE) -C examples

# cancelling rules for cclerical.c
%.c: %.y
%.c: %.l

%.o: override CFLAGS += -std=c11
%.o: %.c $(wildcard *.h)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

# flex uses isatty(3p), fileno(3p)
%.lex.o: override CPPFLAGS += -D_POSIX_C_SOURCE=1
%.lex.o: override CFLAGS += -Wno-unused-function -Wno-sign-compare

ccl.o cclerical.tab.o cclerical.lex.o: cclerical.tab.h cclerical.lex.h
$(OBJS): CPPFLAGS += '-DCCL_VERSION_STR="$(CCL_VERSION)"'

cclerical.tab.c cclerical.tab.h: cclerical.y
	$(YACC) $(YFLAGS) -b cclerical $<

cclerical.lex.c cclerical.lex.h: cclerical.l
	$(LEX) $(LEXFLAGS) -o cclerical.lex.c --header-file=cclerical.lex.h $<

clean:
	$(RM) ccl $(OBJS) cclerical.tab.h cclerical.lex.h cclerical.output
	$(MAKE) -C examples clean
