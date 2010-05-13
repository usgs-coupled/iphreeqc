CC = gcc
CPPFLAGS = -I../../include 
CXX = g++
DEFS = -DNDEBUG=1 -DFC_FUNC\(name,NAME\)=name\ \#\#\ _ -DFC_FUNC_\(name,NAME\)=name\ \#\#\ _ -DFC_MAIN=MAIN__ 
F77 = gfortran
FC = gfortran
CFLAGS = -g -O2
FFLAGS = -I../../include -g -O2
FCLIBS =  -L/usr/lib/gcc/i586-manbo-linux-gnu/4.4.1 -L/usr/lib/gcc/i586-manbo-linux-gnu/4.4.1/../../.. -lgfortranbegin -lgfortran -lm
LIBTOOL = /bin/sh ../../_build/libtool
LDFLAGS = 
IPHREEQC_LA = ../../_build/src/libiphreeqc.la

all: AccumulateLine F90AccumulateLine CreateIPhreeqc F90CreateIPhreeqc

AccumulateLine: AccumulateLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

F90AccumulateLine: F90AccumulateLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA) $(FCLIBS)

CreateIPhreeqc: CreateIPhreeqc.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

F90CreateIPhreeqc: F90CreateIPhreeqc.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA) $(FCLIBS)

.SUFFIXES: .c .cxx .f .f90 .lo


.c.lo:
	$(LIBTOOL) --mode=compile $(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

.cxx.lo:
	$(LIBTOOL) --mode=compile $(CXX) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

.f.lo:
	$(LIBTOOL) --mode=compile $(F77) $(FFLAGS) -c -o $@ $<

.f90.lo:
	$(LIBTOOL) --mode=compile $(FC) $(FFLAGS) -c -o $@ $<


clean:
	$(LIBTOOL) --mode=clean rm -f AccumulateLine AccumulateLine.lo F90AccumulateLine F90AccumulateLine.lo
