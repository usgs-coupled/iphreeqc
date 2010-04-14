
FFLAGS     = -O3 -I../include
LDFLAGS    = -L../lib
FLDFLAGS   = -lgfortranbegin -lgfortran

%.o: %.f
	$(FC) $(FFLAGS) $(TARGET_ARCH) -c -o $@ $<

all: kinn

clean:
	$(RM) kinn.o kinn.exe kinn phreeqc.log phreeqc.out phreeqc.err selected.out

kinn: kinn.o ../lib/libiphreeqc.a 
	$(CXX) $(LDFLAGS) -o kinn kinn.o -liphreeqc $(FLDFLAGS)

kinn.o: kinn.f ../include/IPhreeqc.f.inc
