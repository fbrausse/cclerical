
IRRAM ?= ~/iRRAM/installed

CXXFLAGS = -I $(IRRAM)/include
LDFLAGS  = -L $(IRRAM)/lib -Xlinker -rpath -Xlinker $(IRRAM)/lib
LDLIBS   = -liRRAM -lmpfr -lgmp

EXAMPLES = \
	abs \
	if \
	if2 \
	logmap \
	sqrt \

.PHONY: all clean

all: $(EXAMPLES)

../ccl:
	$(MAKE) -C .. ccl

%.cc: %.t17 ../ccl
	../ccl -b iRRAM -o $@ $<

%: %.cc ../std-irram.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $^ $(LDFLAGS) $(LDLIBS) -o $@

clean:
	$(RM) $(EXAMPLES)
