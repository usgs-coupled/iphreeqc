EXTRA_DIST = test_c.vcproj test_cxx.vcproj test_f90.vfproj test_f.vfproj main77.f main.f90

AM_CPPFLAGS = -I$(top_srcdir)/src -I$(top_srcdir)/src/phreeqcpp
AM_FCFLAGS = -I$(top_srcdir)/src
AM_FFLAGS = -I$(top_srcdir)/src

TESTS = test_c test_cxx
check_PROGRAMS = test_c test_cxx

test_c_SOURCES = test_c.c
test_c_LDADD = $(top_builddir)/src/libiphreeqc.la
test_c_LINK = $(CXXLINK)

test_cxx_SOURCES = test_cxx.cxx
test_cxx_LDADD = $(top_builddir)/src/libiphreeqc.la

if BUILD_FORTRAN
TESTS += test_f test_f90
check_PROGRAMS += test_f test_f90

test_f_SOURCES = test_f.f main_fortran.cxx
test_f_LDADD = $(top_builddir)/src/libiphreeqc.la $(FCLIBS)

test_f90_SOURCES = test_f90.f90 main_fortran.cxx
test_f90_LDADD = $(top_builddir)/src/libiphreeqc.la $(FCLIBS)
endif
