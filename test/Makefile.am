EXTRA_DIST = phreeqc.dat llnl.dat
AM_FCFLAGS = -I$(top_srcdir)/include
AM_FFLAGS = -I$(top_srcdir)/include

noinst_PROGRAMS = kinn
kinn_SOURCES = kinn.f main.cxx
kinn_LDADD = $(top_builddir)/src/libiphreeqc.la $(FCLIBS)
