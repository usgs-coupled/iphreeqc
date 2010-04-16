# Prelude.
AC_INIT([IPhreeqc], [1.0], [charlton@usgs.gov])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

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
 AC_PROG_F77
 AC_F77_LIBRARY_LDFLAGS
 AC_F77_WRAPPERS
else
 echo "no"
fi
AM_CONDITIONAL([BUILD_FORTRAN], [test "X$IPQ_FORTRAN" = "Xyes"])

# Disable shared library building by default (use --enable-shared to enable)
AC_DISABLE_SHARED
AC_PROG_LIBTOOL

# Output files.
AC_CONFIG_FILES([Makefile src/Makefile test/Makefile testcpp/Makefile])
AC_OUTPUT

