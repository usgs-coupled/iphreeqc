#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.64])
AC_INIT([IPhreeqc], [2.17.0], [charlton@usgs.gov])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([-Wall -Werror foreign subdir-objects])
AC_CONFIG_SRCDIR([src/IPhreeqc.cpp])

AC_DEFINE(SWIG_SHARED_OBJ)
AC_DEFINE(PHREEQC_CPP)
AC_DEFINE(PHREEQC_CLASS)
AC_DEFINE(USE_PHRQ_ALLOC)
AC_DEFINE(NDEBUG)

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX

# Check if the fortran interface should be included
AC_MSG_CHECKING([if Fortran interface included])
AC_ARG_ENABLE([fortran],
     [AC_HELP_STRING([--enable-fortran],
                     [include the Fortran interface @<:@default=yes@:>@])],
     [IPQ_FORTRAN=$enableval],
     [IPQ_FORTRAN=yes])

if test "X$IPQ_FORTRAN" = "Xyes"; then
  AC_MSG_RESULT(yes)
  AC_PROG_FC
  AC_FC_LIBRARY_LDFLAGS
  AC_FC_WRAPPERS 
  AC_FC_MAIN
  AC_SUBST(F77, $FC)
  AC_SUBST(FFLAGS, $FCFLAGS) 
else
  AC_MSG_RESULT(no)
fi
AM_CONDITIONAL([BUILD_FORTRAN], [test "X$IPQ_FORTRAN" = "Xyes"])

AC_PROG_LIBTOOL

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.


AC_CONFIG_FILES([
  Makefile
  src/Makefile
  tests/Makefile
  tests/phreeqc.dat
  tests/ex1
  examples/ex1
  examples/main_fortran.cxx
  examples/Makefile
  examples/phreeqc.dat
  examples/test_c.c
  examples/test_cxx.cxx
  examples/test_f.f
  examples/test_f90.f90
])
AC_OUTPUT
