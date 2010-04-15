# Prelude.
AC_INIT([IPhreeqc], [1.0], [charlton@usgs.gov])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Checks for programs.
AC_PROG_CXX
AC_PROG_F77
AC_F77_LIBRARY_LDFLAGS

# Disable shared library building by default (use --enable-shared to enable)
AC_DISABLE_SHARED
AC_PROG_LIBTOOL

# Output files.
AC_CONFIG_FILES([Makefile src/Makefile test/Makefile testcpp/Makefile])
AC_OUTPUT
