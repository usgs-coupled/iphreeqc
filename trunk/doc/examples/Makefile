CC = gcc
CPPFLAGS = -I../../src -I../../src/phreeqcpp
CXX = g++
DEFS = -DNDEBUG=1 -DFC_FUNC\(name,NAME\)=name\ \#\#\ _ -DFC_FUNC_\(name,NAME\)=name\ \#\#\ _ -DFC_MAIN=MAIN__ 
F77 = gfortran
FC = gfortran
CFLAGS = -g -O2 -Wall
FFLAGS = -I../../src -g -O2 -Wall
FCLIBS =  -L/usr/lib/gcc/i586-manbo-linux-gnu/4.4.1 -L/usr/lib/gcc/i586-manbo-linux-gnu/4.4.1/../../.. -lgfortranbegin -lgfortran -lm
LIBTOOL = /bin/sh ../../_build/libtool
LDFLAGS = 
IPHREEQC_LA = ../../_build/src/libiphreeqc.la

TARGETS = \
	AccumulateLine \
	CreateIPhreeqc \
	GetComponent \
	GetDumpString \
	GetErrorString \
	GetLogString \
	GetOutputString \
	GetSelectedOutputString \
	GetSelectedOutputValue \
	GetSelectedOutputValue2 \
	IPhreeqc


F90_TARGETS = \
	F90CreateIPhreeqc \
	F90GetComponent \
	F90GetDumpStringLine \
	F90GetErrorStringLine \
	F90GetLogStringLine \
	F90GetOutputStringLine \
	F90ClearAccumulatedLines \
	F90GetSelectedOutputStringLine \
	F90GetSelectedOutputValue


all: $(TARGETS) $(F90_TARGETS)

AccumulateLine: AccumulateLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

CreateIPhreeqc: CreateIPhreeqc.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

GetComponent: GetComponent.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

GetDumpString: GetDumpString.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

GetErrorString: GetErrorString.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

GetLogString: GetLogString.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

GetOutputString: GetOutputString.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

GetSelectedOutputString: GetSelectedOutputString.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

GetSelectedOutputValue: GetSelectedOutputValue.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

GetSelectedOutputValue2: GetSelectedOutputValue2.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

IPhreeqc: IPhreeqc.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)


F90AccumulateLine: F90AccumulateLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA) $(FCLIBS)

F90CreateIPhreeqc: F90CreateIPhreeqc.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA) $(FCLIBS)

F90GetComponent: F90GetComponent.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA) $(FCLIBS)

F90GetDumpStringLine: F90GetDumpStringLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA) $(FCLIBS)

F90GetErrorStringLine: F90GetErrorStringLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA) $(FCLIBS)

F90GetLogStringLine: F90GetLogStringLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA) $(FCLIBS)

F90GetOutputStringLine: F90GetOutputStringLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA) $(FCLIBS)

F90ClearAccumulatedLines: F90ClearAccumulatedLines.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA) $(FCLIBS)

F90GetSelectedOutputStringLine: F90GetSelectedOutputStringLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA) $(FCLIBS)

F90GetSelectedOutputValue: F90GetSelectedOutputValue.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA) $(FCLIBS)

.SUFFIXES: .c .cxx .f .f90 .lo


.c.lo:
	$(LIBTOOL) --mode=compile $(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

.cxx.lo:
	$(LIBTOOL) --mode=compile $(CXX) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

.cpp.lo:
	$(LIBTOOL) --mode=compile $(CXX) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

.f.lo:
	$(LIBTOOL) --mode=compile $(F77) $(FFLAGS) -c -o $@ $<

.f90.lo:
	$(LIBTOOL) --mode=compile $(FC) $(FFLAGS) -c -o $@ $<

LO_FILES = \
	AccumulateLine.lo \
	CreateIPhreeqc.lo \
	DestroyIPhreeqc.lo \
	GetComponent.lo \
	GetDumpString.lo \
	GetErrorString.lo \
	GetLogString.lo \
	GetOutputString.lo \
	GetSelectedOutputString.lo \
	GetSelectedOutputValue.lo \
	GetSelectedOutputValue2.lo \
	IPhreeqc.lo


F90_LO_FILES = \
	F90AccumulateLine.lo \
	F90ClearAccumulatedLines.lo \
	F90CreateIPhreeqc.lo \
	F90DestroyIPhreeqc.lo \
	F90GetComponent.lo \
	F90GetDumpStringLine.lo \
	F90GetErrorStringLine.lo \
	F90GetLogStringLine.lo \
	F90GetOutputStringLine.lo \
	F90GetSelectedOutputStringLine.lo \
	F90GetSelectedOutputValue.lo


clean:
	$(LIBTOOL) --mode=clean rm -f *~ $(TARGETS) $(F90_TARGETS) $(LO_FILES) $(F90_LO_FILES)
