CC = gcc
CPPFLAGS = -I../../src -I../../src/phreeqcpp -I../../src/phreeqcpp/common -I../../src/phreeqcpp/PhreeqcKeywords
CXX = g++
DEFS = -DNDEBUG=1 -DFC_FUNC\(name,NAME\)=name\ \#\#\ _ -DFC_FUNC_\(name,NAME\)=name\ \#\#\ _ -DFC_MAIN=MAIN__ 
F77 = gfortran
FC = gfortran
CFLAGS = -g -O2 -Wall
FFLAGS = -I../../src -g -O2 -Wall
#FCLIBS =  -L/usr/lib/gcc/i586-manbo-linux-gnu/4.4.1 -L/usr/lib/gcc/i586-manbo-linux-gnu/4.4.1/../../.. -lgfortranbegin -lgfortran -lm
FCLIBS = -L/usr/lib/gcc/x86_64-linux-gnu/4.8 -L/usr/lib/gcc/x86_64-linux-gnu/4.8/../../../x86_64-linux-gnu -L/usr/lib/gcc/x86_64-linux-gnu/4.8/../../../../lib -L/lib/x86_64-linux-gnu -L/lib/../lib -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib -L/usr/lib/gcc/x86_64-linux-gnu/4.8/../../.. -lgfortran -lm -lquadmath
#LIBTOOL = /bin/sh ../../_build/libtool
LIBTOOL = /usr/bin/libtool
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
	GetVersionString \
	Multi \
	SetBasicCallback \
	SetCurrentSelectedOutputUserNumber \
	IPhreeqc


F90_TARGETS = \
	F90CreateIPhreeqc \
	F90GetComponent \
	F90GetDumpStringLine \
	F90GetErrorStringLine \
	F90GetLogStringLine \
	F90GetOutputStringLine \
	F90GetVersionString \
	F90ClearAccumulatedLines \
	F90GetSelectedOutputStringLine \
	F90GetSelectedOutputValue \
	F90SetBasicFortranCallback


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

GetVersionString: GetVersionString.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

Multi: Multi.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

SetBasicCallback: SetBasicCallback.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

SetCurrentSelectedOutputUserNumber: SetCurrentSelectedOutputUserNumber.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)

IPhreeqc: IPhreeqc.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< $(IPHREEQC_LA)


F90AccumulateLine: F90AccumulateLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA) $(FCLIBS)

F90CreateIPhreeqc: F90CreateIPhreeqc.lo $(IPHREEQC_LA) 
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA) $(FCLIBS)

F90GetComponent: F90GetComponent.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA) $(FCLIBS)

F90GetDumpStringLine: F90GetDumpStringLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA) $(FCLIBS)

F90GetErrorStringLine: F90GetErrorStringLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA) $(FCLIBS)

F90GetLogStringLine: F90GetLogStringLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA) $(FCLIBS)

F90GetOutputStringLine: F90GetOutputStringLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA) $(FCLIBS)

F90ClearAccumulatedLines: F90ClearAccumulatedLines.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA) $(FCLIBS)

F90GetSelectedOutputStringLine: F90GetSelectedOutputStringLine.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA) $(FCLIBS)

F90GetSelectedOutputValue: F90GetSelectedOutputValue.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA) $(FCLIBS)

F90GetVersionString: F90GetVersionString.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA) $(FCLIBS)

F90SetBasicFortranCallback: F90SetBasicFortranCallback.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(CXX) $(LDFLAGS) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA) $(FCLIBS)

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


F90CreateIPhreeqc.lo: IPhreeqc_interface.lo
F90ClearAccumulatedLines.lo: IPhreeqc_interface.lo
F90CreateIPhreeqc.lo: IPhreeqc_interface.lo
F90DestroyIPhreeqc.lo: IPhreeqc_interface.lo
F90GetComponent.lo: IPhreeqc_interface.lo
F90GetDumpStringLine.lo: IPhreeqc_interface.lo
F90GetErrorStringLine.lo: IPhreeqc_interface.lo
F90GetLogStringLine.lo: IPhreeqc_interface.lo
F90GetOutputStringLine.lo: IPhreeqc_interface.lo
F90GetSelectedOutputStringLine.lo: IPhreeqc_interface.lo
F90GetSelectedOutputValue.lo: IPhreeqc_interface.lo
F90GetVersionString.lo: IPhreeqc_interface.lo
F90SetBasicFortranCallback.lo: IPhreeqc_interface.lo


IPhreeqc_interface.lo: ../../src/IPhreeqc_interface.F90
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
	GetVersionString.lo \
	IPhreeqc_interface.lo \
	Multi.lo \
	SetBasicCallback.lo \
	SetCurrentSelectedOutputUserNumber.lo \
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
	F90GetSelectedOutputValue.lo \
	F90GetVersionString.lo \
	F90SetBasicFortranCallback.lo

MOD_FILES = \
	callback.mod \
	iphreeqc.mod \
	mydata.mod

clean:
	$(LIBTOOL) --mode=clean rm -f *~ $(TARGETS) $(F90_TARGETS) $(LO_FILES) $(F90_LO_FILES) $(MOD_FILES)
