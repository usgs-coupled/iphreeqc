#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.64])
AC_INIT([IPhreeqc], [2.17], [charlton@usgs.gov])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_CONFIG_SRCDIR([src/IPhreeqc.cpp])

# Checks for programs.
AC_PROG_CXX

# Check if the fortran interface should be included
AC_MSG_CHECKING([if Fortran interface included])
AC_ARG_ENABLE([fortran],
     [AC_HELP_STRING([--enable-fortran],
                     [include the Fortran interface @<:@default=yes@:>@])],
     [IPQ_FORTRAN=$enableval],
     [IPQ_FORTRAN=yes])

if test "X$IPQ_FORTRAN" = "Xyes"; then
 echo "yes"
 AC_PROG_FC
 AC_FC_LIBRARY_LDFLAGS
 AC_FC_WRAPPERS
 AC_FC_MAIN
 AC_SUBST(F77, $FC)
 AC_SUBST(FFLAGS, $FCFLAGS) 
else
 echo "no"
fi
AM_CONDITIONAL([BUILD_FORTRAN], [test "X$IPQ_FORTRAN" = "Xyes"])

# Disable shared library building by default (use --enable-shared to enable)
AC_DISABLE_SHARED
AC_PROG_LIBTOOL

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile src/Makefile tests/Makefile tests/phreeqc.dat tests/ex1])
AC_OUTPUT

