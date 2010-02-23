SRC=src/phreeqcpp/phreeqc
DEST=RPhreeqc/phreeqc/src
MODPATH=src
IPATH=include
CP=cp -f

PSRC = \
	$(DEST)/advection.cpp \
	$(DEST)/basic.cpp \
	$(DEST)/basicsubs.cpp \
	$(DEST)/cl1.cpp \
	$(DEST)/cvdense.cpp \
	$(DEST)/cvode.cpp \
	$(DEST)/dense.cpp \
	$(DEST)/dw.cpp \
	$(DEST)/input.cpp \
	$(DEST)/integrate.cpp \
	$(DEST)/inverse.cpp \
	$(DEST)/isotopes.cpp \
	$(DEST)/kinetics.cpp \
	$(DEST)/mainsubs.cpp \
	$(DEST)/model.cpp \
	$(DEST)/nvector.cpp \
	$(DEST)/nvector_serial.cpp \
	$(DEST)/p2clib.cpp \
	$(DEST)/parse.cpp \
	$(DEST)/phqalloc.cpp \
	$(DEST)/pitzer.cpp \
	$(DEST)/pitzer_structures.cpp \
	$(DEST)/prep.cpp \
	$(DEST)/print.cpp \
	$(DEST)/read.cpp \
	$(DEST)/readtr.cpp \
	$(DEST)/smalldense.cpp \
	$(DEST)/spread.cpp \
	$(DEST)/step.cpp \
	$(DEST)/structures.cpp \
	$(DEST)/sundialsmath.cpp \
	$(DEST)/tally.cpp \
	$(DEST)/tidy.cpp \
	$(DEST)/transport.cpp \
	$(DEST)/utilities.cpp


MODSRC = \
	$(DEST)/IPhreeqc.cpp \
	$(DEST)/SelectedOutput.cpp \
	$(DEST)/Var.cpp \
	$(DEST)/global.cpp \
	$(DEST)/module_files.cpp \
	$(DEST)/module_output.cpp


HSRC = \
	$(DEST)/IPhreeqc.h \
	$(DEST)/Var.h \
	$(DEST)/CVar.hxx \
	$(DEST)/Debug.h \
	$(DEST)/module_files.h \
	$(DEST)/ErrorReporter.hxx \
	$(DEST)/phreeqcns.hxx \
	$(DEST)/PhreeqcParser.hxx \
	$(DEST)/SelectedOutput.hxx \
	$(DEST)/cvdense.h \
	$(DEST)/cvode.h \
	$(DEST)/dense.h \
	$(DEST)/global.h \
	$(DEST)/input.h \
	$(DEST)/kinetics.h \
	$(DEST)/nvector.h \
	$(DEST)/nvector_serial.h \
	$(DEST)/output.h \
	$(DEST)/p2c.h \
	$(DEST)/phqalloc.h \
	$(DEST)/phrqproto.h \
	$(DEST)/phrqtype.h \
	$(DEST)/pitzer.h \
	$(DEST)/smalldense.h \
	$(DEST)/sundialsmath.h \
	$(DEST)/sundialstypes.h \
	$(DEST)/phreeqc_files.inl \
	$(DEST)/output.inl

#
# phreeqc files
#
all: $(PSRC) $(MODSRC) $(HSRC)

$(DEST)/advection.cpp : $(SRC)/advection.c
	$(CP) $< $@

$(DEST)/utilities.cpp : $(SRC)/utilities.c
	$(CP) $< $@

$(DEST)/basic.cpp : $(SRC)/basic.c
	$(CP) $< $@

$(DEST)/basicsubs.cpp : $(SRC)/basicsubs.c
	$(CP) $< $@

$(DEST)/cl1.cpp : $(SRC)/cl1.c
	$(CP) $< $@

$(DEST)/cvdense.cpp : $(SRC)/cvdense.c
	$(CP) $< $@

$(DEST)/cvode.cpp : $(SRC)/cvode.c
	$(CP) $< $@

$(DEST)/dense.cpp : $(SRC)/dense.c
	$(CP) $< $@

$(DEST)/dw.cpp : $(SRC)/dw.c
	$(CP) $< $@

$(DEST)/input.cpp : $(SRC)/input.c
	$(CP) $< $@

$(DEST)/integrate.cpp : $(SRC)/integrate.c
	$(CP) $< $@

$(DEST)/inverse.cpp : $(SRC)/inverse.c
	$(CP) $< $@

$(DEST)/isotopes.cpp : $(SRC)/isotopes.c
	$(CP) $< $@

$(DEST)/kinetics.cpp : $(SRC)/kinetics.c
	$(CP) $< $@

$(DEST)/mainsubs.cpp : $(SRC)/mainsubs.c
	$(CP) $< $@

$(DEST)/model.cpp : $(SRC)/model.c
	$(CP) $< $@

$(DEST)/nvector.cpp : $(SRC)/nvector.c
	$(CP) $< $@

$(DEST)/nvector_serial.cpp : $(SRC)/nvector_serial.c
	$(CP) $< $@

$(DEST)/p2clib.cpp : $(SRC)/p2clib.c
	$(CP) $< $@

$(DEST)/parse.cpp : $(SRC)/parse.c
	$(CP) $< $@

$(DEST)/phqalloc.cpp : $(SRC)/phqalloc.c
	$(CP) $< $@

$(DEST)/pitzer.cpp : $(SRC)/pitzer.c
	$(CP) $< $@

$(DEST)/pitzer_structures.cpp : $(SRC)/pitzer_structures.c
	$(CP) $< $@

$(DEST)/prep.cpp : $(SRC)/prep.c
	$(CP) $< $@

$(DEST)/print.cpp : $(SRC)/print.c
	$(CP) $< $@

$(DEST)/read.cpp : $(SRC)/read.c
	$(CP) $< $@

$(DEST)/readtr.cpp : $(SRC)/readtr.c
	$(CP) $< $@

$(DEST)/smalldense.cpp : $(SRC)/smalldense.c
	$(CP) $< $@

$(DEST)/spread.cpp : $(SRC)/spread.c
	$(CP) $< $@

$(DEST)/step.cpp : $(SRC)/step.c
	$(CP) $< $@

$(DEST)/structures.cpp : $(SRC)/structures.c
	$(CP) $< $@

$(DEST)/sundialsmath.cpp : $(SRC)/sundialsmath.c
	$(CP) $< $@

$(DEST)/tally.cpp : $(SRC)/tally.c
	$(CP) $< $@

$(DEST)/tidy.cpp : $(SRC)/tidy.c
	$(CP) $< $@

$(DEST)/transport.cpp : $(SRC)/transport.c
	$(CP) $< $@


#
# module files
#
$(DEST)/IPhreeqc.cpp : $(MODPATH)/IPhreeqc.cpp
	$(CP) $< $@

$(DEST)/SelectedOutput.cpp : $(MODPATH)/SelectedOutput.cpp
	$(CP) $< $@

$(DEST)/Var.cpp : $(MODPATH)/Var.c
	$(CP) $< $@

##$(DEST)/fwrap.cpp : $(MODPATH)/fwrap.c
##	$(CP) $< $@

$(DEST)/global.cpp : $(MODPATH)/global.c
	$(CP) $< $@

$(DEST)/module_files.cpp : $(MODPATH)/module_files.c
	$(CP) $< $@

$(DEST)/module_output.cpp : $(MODPATH)/module_output.c
	$(CP) $< $@


#
# header files
#
$(DEST)/Var.h : $(IPATH)/Var.h
	$(CP) $< $@

$(DEST)/IPhreeqc.h : $(IPATH)/IPhreeqc.h
	$(CP) $< $@

$(DEST)/CVar.hxx : $(MODPATH)/CVar.hxx
	$(CP) $< $@

$(DEST)/Debug.h : $(MODPATH)/Debug.h
	$(CP) $< $@

##$(DEST)/fwrap.h : $(MODPATH)/fwrap.h
##	$(CP) $< $@

$(DEST)/module_files.h : $(MODPATH)/module_files.h
	$(CP) $< $@

$(DEST)/ErrorReporter.hxx : $(MODPATH)/ErrorReporter.hxx
	$(CP) $< $@

$(DEST)/phreeqcns.hxx : $(MODPATH)/phreeqcns.hxx
	$(CP) $< $@

$(DEST)/PhreeqcParser.hxx : $(MODPATH)/PhreeqcParser.hxx
	$(CP) $< $@

$(DEST)/SelectedOutput.hxx : $(MODPATH)/SelectedOutput.hxx
	$(CP) $< $@

$(DEST)/cvdense.h : $(SRC)/cvdense.h
	$(CP) $< $@

$(DEST)/cvode.h : $(SRC)/cvode.h
	$(CP) $< $@

$(DEST)/dense.h : $(SRC)/dense.h
	$(CP) $< $@

$(DEST)/global.h : $(SRC)/global.h
	$(CP) $< $@

$(DEST)/input.h : $(SRC)/input.h
	$(CP) $< $@

$(DEST)/kinetics.h : $(SRC)/kinetics.h
	$(CP) $< $@

$(DEST)/nvector.h : $(SRC)/nvector.h
	$(CP) $< $@

$(DEST)/nvector_serial.h : $(SRC)/nvector_serial.h
	$(CP) $< $@

$(DEST)/output.h : $(SRC)/output.h
	$(CP) $< $@

$(DEST)/p2c.h : $(SRC)/p2c.h
	$(CP) $< $@

$(DEST)/phqalloc.h : $(SRC)/phqalloc.h
	$(CP) $< $@

$(DEST)/phrqproto.h : $(SRC)/phrqproto.h
	$(CP) $< $@

$(DEST)/phrqtype.h : $(SRC)/phrqtype.h
	$(CP) $< $@

$(DEST)/pitzer.h : $(SRC)/pitzer.h
	$(CP) $< $@

$(DEST)/smalldense.h : $(SRC)/smalldense.h
	$(CP) $< $@

$(DEST)/sundialsmath.h : $(SRC)/sundialsmath.h
	$(CP) $< $@

$(DEST)/sundialstypes.h: $(SRC)/sundialstypes.h
	$(CP) $< $@

$(DEST)/phreeqc_files.inl : $(SRC)/phreeqc_files.c
	$(CP) $< $@

$(DEST)/output.inl : $(SRC)/output.c
	$(CP) $< $@
