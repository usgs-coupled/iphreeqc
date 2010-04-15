EXTRA_DIST = phreeqc.dat llnl.dat
AM_FFLAGS = -I$(top_srcdir)/include
noinst_PROGRAMS = kinn
kinn_SOURCES = kinn.f dummy.cxx
kinn_LDADD = ../src/libiphreeqc.la $(FLIBS)
