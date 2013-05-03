include makelist

CXX        = g++
CPPFLAGS   = -Wall -DNDEBUG -DSWIG_SHARED_OBJ
LDFLAGS    = -L../lib
FC         = f95
FLDFLAGS   = -lgfortranbegin -lgfortran

%.o: %.f
	$(FC) $(FFLAGS) $(TARGET_ARCH) -g -c -o $@ $<

all: test2

clean:
	$(RM) test2 test2.o test2.exe sel1 sel2 phreeqc.log phreeqc.out phreeqc.err error.inp

test2: test2.o ../lib/libiphreeqc.a
	$(CXX) $(LDFLAGS) -o test2 test2.o -liphreeqc $(FLDFLAGS)

test2.o: test2.f
