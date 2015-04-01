EXTRA_DIST = test_c.vcproj test_cxx.vcproj test_f90.vfproj test_f.vfproj main77.f main.f90

AM_CPPFLAGS = -I$(top_srcdir)/src -I$(top_srcdir)/src/phreeqcpp -I$(top_srcdir)/src/phreeqcpp/PhreeqcKeywords
AM_FCFLAGS = -I$(top_srcdir)/src
AM_FFLAGS = -I$(top_srcdir)/src

TESTS = test_c test_cxx
check_PROGRAMS = test_c test_cxx

test_c_SOURCES = test_c.c
test_c_LDADD = $(top_builddir)/src/libiphreeqc.la
test_c_LINK = $(CXXLINK)

test_cxx_SOURCES = test_cxx.cxx
test_cxx_LDADD = $(top_builddir)/src/libiphreeqc.la

CLEANFILES =\
	XYZ\
	phreeqc.0.log\
	phreeqc.0.err

if BUILD_FORTRAN
##TESTS += test_f test_f90
##check_PROGRAMS += test_f test_f90

TESTS += test_f90
check_PROGRAMS += test_f90

##test_f_SOURCES = test_f.f main_fortran.cxx
##test_f_LDADD = $(top_builddir)/src/libiphreeqc.la $(FCLIBS)

test_f90_SOURCES = test_f90.F90 main_fortran.cxx IPhreeqc_interface.F90
test_f90_LDADD = $(top_builddir)/src/libiphreeqc.la $(FCLIBS)

mostlyclean-local: clean-modules
.PHONY: clean-modules
clean-modules:
	-test -z $(FC_MODEXT) || $(RM) *.$(FC_MODEXT)

test_f90.$(OBJEXT) : test_f90.F90 IPhreeqc_interface.$(OBJEXT)

IPhreeqc_interface.$(OBJEXT) : $(srcdir)/IPhreeqc_interface.F90

$(srcdir)/IPhreeqc_interface.F90:
	cd $(srcdir) && $(LN_S) ../src/IPhreeqc_interface.F90 .

endif
