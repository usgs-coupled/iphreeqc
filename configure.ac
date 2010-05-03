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

# libtool versioning
# 1. Start with version information of ‘0:0:0’ for each libtool library. 
# 2. Update the version information only immediately before a public release of your software. More frequent updates are unnecessary,
#    and only guarantee that the current interface number gets larger faster. 
# 3. If the library source code has changed at all since the last update, then increment revision (‘c:r:a’ becomes ‘c:r+1:a’). 
# 4. If any interfaces have been added, removed, or changed since the last update, increment current, and set revision to 0. 
# 5. If any interfaces have been added since the last public release, then increment age. 
# 6. If any interfaces have been removed since the last public release, then set age to 0.
IPQ_CURRENT=0
IPQ_REVISION=0
IPQ_AGE=0
IPQ_VERSION=$IPQ_CURRENT:$IPQ_REVISION:$IPQ_AGE
AC_SUBST(IPQ_VERSION)

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
