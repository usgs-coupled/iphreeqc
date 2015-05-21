SHELL=/bin/bash
HOSTNAME ?= $(shell hostname)

ifeq (${HOSTNAME}, ubuntu)
  LDFLAGS     = -L../../../_build/INSTALL/lib
  IPHREEQC_LA = ../../../_build/INSTALL/lib/libiphreeqc.la
  LIBTOOL     = /usr/bin/libtool
  FC          = gfortran
  FFLAGS      =
  MOD_FILES   = callback.mod iphreeqc.mod mydata.mod subs.mod
endif

all: advect advect-lt

advect: advect.o
	gfortran -o advect advect.o IPhreeqc_interface.o $(LDFLAGS) -liphreeqc -lstdc++

#advect.o: advect.F90 IPhreeqc_interface.o
#	gfortran -c $<
advect.o: advect.lo

#IPhreeqc_interface.o: ../../../src/IPhreeqc_interface.F90
#	gfortran -c $<
IPhreeqc_interface.o: IPhreeqc_interface.lo

advect-lt: advect.lo $(IPHREEQC_LA)
	$(LIBTOOL) --mode=link $(FC) -o $@ $< IPhreeqc_interface.lo $(IPHREEQC_LA)

advect.lo: advect.F90 IPhreeqc_interface.lo
	$(LIBTOOL) --mode=compile $(FC) $(FFLAGS) -c -o $@ $<

IPhreeqc_interface.lo: ../../../src/IPhreeqc_interface.F90
	$(LIBTOOL) --mode=compile $(FC) $(FFLAGS) -c -o $@ $<

clean:
	${LIBTOOL} --mode=clean rm -f advect advect-lt advect.lo IPhreeqc_interface.lo ${MOD_FILES}
