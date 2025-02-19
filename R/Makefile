# phreeqc                   TOPDIR
# ├── data                  DATADIR
# ├── inst                  INST
# ├── man                   MAN
# ├── R
# └── src                   IDEST
#     └── phreeqcpp         DEST
#
# see https://r-pkgs.org/
#

CP=cp -f
TOPDIR=phreeqc
IDEST=phreeqc/src
DEST=phreeqc/src/phreeqcpp
ISRC=../src
SRC=../src/phreeqcpp
INST=phreeqc/inst
DBDIR=../database
DATADIR=phreeqc/data
EXDIR=../phreeqc3-examples
MAN=phreeqc/man

# ?= set only if not already set
RELEASE_DATE ?= $(shell date --rfc-3339=date)
VERSION      ?= $(shell curl -sS https://raw.githubusercontent.com/usgs-coupled/phreeqc-version/main/next_ver.sh | sh)

DBS = \
	Amm.ascii \
	ColdChem.ascii \
	core10.ascii \
	ex15.ascii \
	frezchem.ascii \
	iso.ascii \
	Kinec_v3.ascii \
	Kinec.v2.ascii \
	llnl.ascii \
	minteq.ascii \
	minteq.v4.ascii \
	phreeqc_rates.ascii \
	PHREEQC_ThermoddemV1.10_15Dec2020.ascii \
	phreeqc.ascii \
	pitzer.ascii \
	sit.ascii \
	stimela.ascii \
	Tipping_Hurley.ascii \
	wateq4f.ascii

EXS = \
	$(EXDIR)/ex1 \
	$(EXDIR)/ex2 \
	$(EXDIR)/ex3 \
	$(EXDIR)/ex4 \
	$(EXDIR)/ex5 \
	$(EXDIR)/ex6 \
	$(EXDIR)/ex7 \
	$(EXDIR)/ex8 \
	$(EXDIR)/ex9 \
	$(EXDIR)/ex10 \
	$(EXDIR)/ex11 \
	$(EXDIR)/ex12 \
	$(EXDIR)/ex13a \
	$(EXDIR)/ex13b \
	$(EXDIR)/ex13c \
	$(EXDIR)/ex14 \
	$(EXDIR)/ex15 \
	$(EXDIR)/ex16 \
	$(EXDIR)/ex17 \
	$(EXDIR)/ex18 \
	$(EXDIR)/ex19 \
	$(EXDIR)/ex20a \
	$(EXDIR)/ex20b \
	ex21.ascii \
	$(EXDIR)/ex22

RDFILES = \
	$(MAN)/phrAccumulateLine.Rd \
	$(MAN)/phrClearAccumulatedLines.Rd \
	$(MAN)/phreeqc-package.Rd \
	$(MAN)/phrGetAccumulatedLines.Rd \
	$(MAN)/phrGetComponentList.Rd \
	$(MAN)/phrGetDumpFileName.Rd \
	$(MAN)/phrGetDumpString.Rd \
	$(MAN)/phrGetErrorFileName.Rd \
	$(MAN)/phrGetErrorFileOn.Rd \
	$(MAN)/phrGetErrorString.Rd \
	$(MAN)/phrGetLogFileName.Rd \
	$(MAN)/phrGetLogString.Rd \
	$(MAN)/phrGetOutputFileName.Rd \
	$(MAN)/phrGetOutputString.Rd \
	$(MAN)/phrGetSelectedOutputFileName.Rd \
	$(MAN)/phrGetSelectedOutput.Rd \
	$(MAN)/phrGetSelectedOutputString.Rd \
	$(MAN)/phrGetWarningString.Rd \
	$(MAN)/phrLoadDatabase.Rd \
	$(MAN)/phrLoadDatabaseString.Rd \
	$(MAN)/phrRunAccumulated.Rd \
	$(MAN)/phrRunFile.Rd \
	$(MAN)/phrRunString.Rd \
	$(MAN)/phrSetDumpFileName.Rd \
	$(MAN)/phrSetDumpFileOn.Rd \
	$(MAN)/phrSetDumpStringOn.Rd \
	$(MAN)/phrSetErrorFileName.Rd \
	$(MAN)/phrSetErrorFileOn.Rd \
	$(MAN)/phrSetErrorStringOn.Rd \
	$(MAN)/phrSetLogFileName.Rd \
	$(MAN)/phrSetLogFileOn.Rd \
	$(MAN)/phrSetLogStringOn.Rd \
	$(MAN)/phrSetOutputFileName.Rd \
	$(MAN)/phrSetOutputFileOn.Rd \
	$(MAN)/phrSetOutputStringOn.Rd \
	$(MAN)/phrSetSelectedOutputFileName.Rd \
	$(MAN)/phrSetSelectedOutputFileOn.Rd \
	$(MAN)/phrSetSelectedOutputStringOn.Rd


PSRC = \
	$(DEST)/advection.cpp \
	$(DEST)/basicsubs.cpp \
	$(DEST)/ChartHandler.cpp \
	$(DEST)/ChartHandler.h \
	$(DEST)/ChartObject.cpp \
	$(DEST)/ChartObject.h \
	$(DEST)/cl1.cpp \
	$(DEST)/CurveObject.cpp \
	$(DEST)/CurveObject.h \
	$(DEST)/cvdense.cpp \
	$(DEST)/cvdense.h \
	$(DEST)/cvode.cpp \
	$(DEST)/cvode.h \
	$(DEST)/cxxKinetics.cpp \
	$(DEST)/cxxKinetics.h \
	$(DEST)/cxxMix.cpp \
	$(DEST)/cxxMix.h \
	$(DEST)/dense.cpp \
	$(DEST)/dense.h \
	$(DEST)/Dictionary.cpp \
	$(DEST)/Dictionary.h \
	$(DEST)/dumper.cpp \
	$(DEST)/dumper.h \
	$(DEST)/Exchange.cpp \
	$(DEST)/Exchange.h \
	$(DEST)/ExchComp.cpp \
	$(DEST)/ExchComp.h \
	$(DEST)/GasComp.cpp \
	$(DEST)/GasComp.h \
	$(DEST)/gases.cpp \
	$(DEST)/GasPhase.cpp \
	$(DEST)/GasPhase.h \
	$(DEST)/global_structures.h \
	$(DEST)/input.cpp \
	$(DEST)/integrate.cpp \
	$(DEST)/inverse.cpp \
	$(DEST)/ISolution.cpp \
	$(DEST)/ISolution.h \
	$(DEST)/ISolutionComp.cpp \
	$(DEST)/ISolutionComp.h \
	$(DEST)/isotopes.cpp \
	$(DEST)/Keywords.cpp \
	$(DEST)/Keywords.h \
	$(DEST)/kinetics.cpp \
	$(DEST)/KineticsComp.cpp \
	$(DEST)/KineticsComp.h \
	$(DEST)/mainsubs.cpp \
	$(DEST)/model.cpp \
	$(DEST)/NA.h \
	$(DEST)/NameDouble.cpp \
	$(DEST)/NameDouble.h \
	$(DEST)/NumKeyword.cpp \
	$(DEST)/NumKeyword.h \
	$(DEST)/nvector.cpp \
	$(DEST)/nvector.h \
	$(DEST)/nvector_serial.cpp \
	$(DEST)/nvector_serial.h \
	$(DEST)/parse.cpp \
	$(DEST)/Parser.cpp \
	$(DEST)/Parser.h \
	$(DEST)/PBasic.cpp \
	$(DEST)/PBasic.h \
	$(DEST)/phqalloc.cpp \
	$(DEST)/phqalloc.h \
	$(DEST)/Phreeqc.cpp \
	$(DEST)/Phreeqc.h \
	$(DEST)/PHRQ_base.cpp \
	$(DEST)/PHRQ_base.h \
	$(DEST)/PHRQ_exports.h \
	$(DEST)/PHRQ_io.cpp \
	$(DEST)/PHRQ_io.h \
	$(DEST)/PHRQ_io_output.cpp \
	$(DEST)/phrqtype.h \
	$(DEST)/pitzer.cpp \
	$(DEST)/pitzer_structures.cpp \
	$(DEST)/PPassemblage.cpp \
	$(DEST)/PPassemblage.h \
	$(DEST)/PPassemblageComp.cpp \
	$(DEST)/PPassemblageComp.h \
	$(DEST)/prep.cpp \
	$(DEST)/Pressure.cpp \
	$(DEST)/Pressure.h \
	$(DEST)/print.cpp \
	$(DEST)/Reaction.cpp \
	$(DEST)/Reaction.h \
	$(DEST)/read.cpp \
	$(DEST)/ReadClass.cpp \
	$(DEST)/readtr.cpp \
	$(DEST)/runner.cpp \
	$(DEST)/runner.h \
	$(DEST)/SelectedOutput.cpp \
	$(DEST)/SelectedOutput.h \
	$(DEST)/Serializer.cpp \
	$(DEST)/Serializer.h \
	$(DEST)/sit.cpp \
	$(DEST)/smalldense.cpp \
	$(DEST)/smalldense.h \
	$(DEST)/Solution.cpp \
	$(DEST)/Solution.h \
	$(DEST)/SolutionIsotope.cpp \
	$(DEST)/SolutionIsotope.h \
	$(DEST)/spread.cpp \
	$(DEST)/SS.cpp \
	$(DEST)/SS.h \
	$(DEST)/SSassemblage.cpp \
	$(DEST)/SSassemblage.h \
	$(DEST)/SScomp.cpp \
	$(DEST)/SScomp.h \
	$(DEST)/step.cpp \
	$(DEST)/StorageBin.cpp \
	$(DEST)/StorageBin.h \
	$(DEST)/StorageBinList.cpp \
	$(DEST)/StorageBinList.h \
	$(DEST)/structures.cpp \
	$(DEST)/sundialsmath.cpp \
	$(DEST)/sundialsmath.h \
	$(DEST)/sundialstypes.h \
	$(DEST)/Surface.cpp \
	$(DEST)/Surface.h \
	$(DEST)/SurfaceCharge.cpp \
	$(DEST)/SurfaceCharge.h \
	$(DEST)/SurfaceComp.cpp \
	$(DEST)/SurfaceComp.h \
	$(DEST)/System.cpp \
	$(DEST)/System.h \
	$(DEST)/tally.cpp \
	$(DEST)/Temperature.cpp \
	$(DEST)/Temperature.h \
	$(DEST)/tidy.cpp \
	$(DEST)/transport.cpp \
	$(DEST)/Use.cpp \
	$(DEST)/Use.h \
	$(DEST)/UserPunch.cpp \
	$(DEST)/UserPunch.h \
	$(DEST)/utilities.cpp \
	$(DEST)/Utils.cpp \
	$(DEST)/Utils.h

XSRC = \
	$(IDEST)/CSelectedOutput.cpp \
	$(IDEST)/CSelectedOutputhxx.h \
	$(IDEST)/CVarhxx.h \
	$(IDEST)/Debug.h \
	$(IDEST)/ErrorReporterhxx.h \
	$(IDEST)/IPhreeqcCallbacks.h \
	$(IDEST)/IPhreeqc.cpp \
	$(IDEST)/IPhreeqc.h \
	$(IDEST)/IPhreeqchpp.h \
	$(IDEST)/IPhreeqcLib.cpp \
	$(IDEST)/R.cpp \
	$(IDEST)/thread.h \
	$(IDEST)/Var.c \
	$(IDEST)/Var.h \
	$(IDEST)/Version.h \
	$(IDEST)/Makevars

DATA = \
	$(DATADIR)/databases.rda \
	$(DATADIR)/examples.rda

all: $(PSRC) $(XSRC) $(DATA) $(MAN)/phreeqc-package.Rd

source: $(TOPDIR)/R/phreeqc.R $(TOPDIR)/DESCRIPTION $(TOPDIR)/NAMESPACE $(PSRC) $(XSRC) $(DATA)

$(DATADIR)/databases.rda : $(DBS) build-databases.R
	rm -f .RData
	R --no-save --no-restore CMD BATCH build-databases.R
	rm -f .RData

ex15.ascii : $(EXDIR)/ex15.dat
	perl -pe 's/[^[:ascii:]]/?/g' $< > $@

ex21.ascii : $(EXDIR)/ex21
	perl -pe 's/[^[:ascii:]]/?/g' $< > $@

%.ascii : $(DBDIR)/%.dat
	perl -pe 's/[^[:ascii:]]/?/g' $< > $@

$(DATADIR)/examples.rda : $(EXS) build-examples.R
	rm -f .RData
	R --no-save --no-restore CMD BATCH build-examples.R
	rm -f .RData

document : $(MAN)/phreeqc-package.Rd

$(MAN)/phreeqc-package.Rd : $(TOPDIR)/R/phreeqc.R $(TOPDIR)/DESCRIPTION $(TOPDIR)/NAMESPACE $(PSRC) $(XSRC) $(DATA)
	rm -f $(RDFILES)
	Rscript -e 'devtools::document("phreeqc")'

build : phreeqc_$(VERSION).tar.gz

phreeqc_$(VERSION).tar.gz : $(MAN)/phreeqc-package.Rd
	Rscript -e 'devtools::build("phreeqc")'

##$(PSRC): $(DEST)

##$(ISRC): $(IDEST)

##$(DEST) : $(IDEST)

$(DEST)/advection.cpp : $(SRC)/advection.cpp
	$(CP) $< $@

$(DEST)/basicsubs.cpp : $(SRC)/basicsubs.cpp
	$(CP) $< $@

$(DEST)/ChartHandler.cpp : $(SRC)/ChartHandler.cpp
	$(CP) $< $@

$(DEST)/ChartHandler.h : $(SRC)/ChartHandler.h
	$(CP) $< $@

$(DEST)/ChartObject.cpp : $(SRC)/ChartObject.cpp
	$(CP) $< $@

$(DEST)/ChartObject.h : $(SRC)/ChartObject.h
	$(CP) $< $@

$(DEST)/cl1.cpp : $(SRC)/cl1.cpp
	$(CP) $< $@

##$(DEST)/cl1mp.cpp : $(SRC)/cl1mp.cpp
##	$(CP) $< $@

##$(DEST)/class_main.cpp : $(SRC)/class_main.cpp
##	$(CP) $< $@

$(DEST)/CurveObject.cpp : $(SRC)/CurveObject.cpp
	$(CP) $< $@

$(DEST)/CurveObject.h : $(SRC)/CurveObject.h
	$(CP) $< $@

$(DEST)/cvdense.cpp : $(SRC)/cvdense.cpp
	$(CP) $< $@

$(DEST)/cvdense.h : $(SRC)/cvdense.h
	$(CP) $< $@

$(DEST)/cvode.cpp : $(SRC)/cvode.cpp
	$(CP) $< $@

$(DEST)/cvode.h : $(SRC)/cvode.h
	$(CP) $< $@

$(DEST)/cxxKinetics.cpp : $(SRC)/cxxKinetics.cxx
	$(CP) $< $@

$(DEST)/cxxKinetics.h : $(SRC)/cxxKinetics.h
	$(CP) $< $@

$(DEST)/cxxMix.cpp : $(SRC)/cxxMix.cxx
	$(CP) $< $@

$(DEST)/cxxMix.h : $(SRC)/cxxMix.h
	$(CP) $< $@

$(DEST)/dense.cpp : $(SRC)/dense.cpp
	$(CP) $< $@

$(DEST)/dense.h : $(SRC)/dense.h
	$(CP) $< $@

$(DEST)/Dictionary.cpp : $(SRC)/Dictionary.cpp
	$(CP) $< $@

$(DEST)/Dictionary.h : $(SRC)/Dictionary.h
	$(CP) $< $@

$(DEST)/dumper.cpp : $(SRC)/dumper.cpp
	$(CP) $< $@

$(DEST)/dumper.h : $(SRC)/dumper.h
	$(CP) $< $@

$(DEST)/Exchange.cpp : $(SRC)/Exchange.cxx
	$(CP) $< $@

$(DEST)/Exchange.h : $(SRC)/Exchange.h
	$(CP) $< $@

$(DEST)/ExchComp.cpp : $(SRC)/ExchComp.cxx
	$(CP) $< $@

$(DEST)/ExchComp.h : $(SRC)/ExchComp.h
	$(CP) $< $@

$(DEST)/GasComp.cpp : $(SRC)/GasComp.cxx
	$(CP) $< $@

$(DEST)/GasComp.h : $(SRC)/GasComp.h
	$(CP) $< $@

$(DEST)/gases.cpp : $(SRC)/gases.cpp
	$(CP) $< $@

$(DEST)/GasPhase.cpp : $(SRC)/GasPhase.cxx
	$(CP) $< $@

$(DEST)/GasPhase.h : $(SRC)/GasPhase.h
	$(CP) $< $@

$(DEST)/global_structures.h : $(SRC)/global_structures.h
	$(CP) $< $@

$(DEST)/input.cpp : $(SRC)/input.cpp
	$(CP) $< $@

$(DEST)/integrate.cpp : $(SRC)/integrate.cpp
	$(CP) $< $@

$(DEST)/inverse.cpp : $(SRC)/inverse.cpp
	$(CP) $< $@

$(DEST)/ISolution.cpp : $(SRC)/ISolution.cxx
	$(CP) $< $@

$(DEST)/ISolution.h : $(SRC)/ISolution.h
	$(CP) $< $@

$(DEST)/ISolutionComp.cpp : $(SRC)/ISolutionComp.cxx
	$(CP) $< $@

$(DEST)/ISolutionComp.h : $(SRC)/ISolutionComp.h
	$(CP) $< $@

$(DEST)/isotopes.cpp : $(SRC)/isotopes.cpp
	$(CP) $< $@

$(DEST)/Keywords.cpp : $(SRC)/PhreeqcKeywords/Keywords.cpp
	$(CP) $< $@

$(DEST)/Keywords.h : $(SRC)/PhreeqcKeywords/Keywords.h
	$(CP) $< $@

$(DEST)/kinetics.cpp : $(SRC)/kinetics.cpp
	$(CP) $< $@

$(DEST)/KineticsComp.cpp : $(SRC)/KineticsComp.cxx
	$(CP) $< $@

$(DEST)/KineticsComp.h : $(SRC)/KineticsComp.h
	$(CP) $< $@

$(DEST)/mainsubs.cpp : $(SRC)/mainsubs.cpp
	$(CP) $< $@

$(DEST)/model.cpp : $(SRC)/model.cpp
	$(CP) $< $@

$(DEST)/NA.h : $(SRC)/NA.h
	$(CP) $< $@

$(DEST)/NameDouble.cpp : $(SRC)/NameDouble.cxx
	$(CP) $< $@

$(DEST)/NameDouble.h : $(SRC)/NameDouble.h
	$(CP) $< $@

$(DEST)/NumKeyword.cpp : $(SRC)/NumKeyword.cxx
	$(CP) $< $@

$(DEST)/NumKeyword.h : $(SRC)/NumKeyword.h
	$(CP) $< $@

$(DEST)/nvector.cpp : $(SRC)/nvector.cpp
	$(CP) $< $@

$(DEST)/nvector.h : $(SRC)/nvector.h
	$(CP) $< $@

$(DEST)/nvector_serial.cpp : $(SRC)/nvector_serial.cpp
	$(CP) $< $@

$(DEST)/nvector_serial.h : $(SRC)/nvector_serial.h
	$(CP) $< $@

$(DEST)/parse.cpp : $(SRC)/parse.cpp
	$(CP) $< $@

$(DEST)/Parser.cpp : $(SRC)/common/Parser.cxx
	$(CP) $< $@

$(DEST)/Parser.h : $(SRC)/common/Parser.h
	$(CP) $< $@

$(DEST)/PBasic.cpp : $(SRC)/PBasic.cpp
	$(CP) $< $@

$(DEST)/PBasic.h : $(SRC)/PBasic.h
	$(CP) $< $@

$(DEST)/phqalloc.cpp : $(SRC)/phqalloc.cpp
	$(CP) $< $@

$(DEST)/phqalloc.h : $(SRC)/phqalloc.h
	$(CP) $< $@

$(DEST)/Phreeqc.cpp : $(SRC)/Phreeqc.cpp
	$(CP) $< $@

$(DEST)/Phreeqc.h : $(SRC)/Phreeqc.h
	$(CP) $< $@

$(DEST)/PHRQ_base.cpp : $(SRC)/common/PHRQ_base.cxx
	$(CP) $< $@

$(DEST)/PHRQ_base.h : $(SRC)/common/PHRQ_base.h
	$(CP) $< $@

$(DEST)/PHRQ_exports.h : $(SRC)/common/PHRQ_exports.h
	$(CP) $< $@

$(DEST)/PHRQ_io.cpp : $(SRC)/common/PHRQ_io.cpp
	$(CP) $< $@

$(DEST)/PHRQ_io.h : $(SRC)/common/PHRQ_io.h
	$(CP) $< $@

$(DEST)/PHRQ_io_output.cpp : $(SRC)/PHRQ_io_output.cpp
	$(CP) $< $@

$(DEST)/phrqtype.h : $(SRC)/common/phrqtype.h
	$(CP) $< $@

$(DEST)/pitzer.cpp : $(SRC)/pitzer.cpp
	$(CP) $< $@

$(DEST)/pitzer_structures.cpp : $(SRC)/pitzer_structures.cpp
	$(CP) $< $@

$(DEST)/PPassemblage.cpp : $(SRC)/PPassemblage.cxx
	$(CP) $< $@

$(DEST)/PPassemblage.h : $(SRC)/PPassemblage.h
	$(CP) $< $@

$(DEST)/PPassemblageComp.cpp : $(SRC)/PPassemblageComp.cxx
	$(CP) $< $@

$(DEST)/PPassemblageComp.h : $(SRC)/PPassemblageComp.h
	$(CP) $< $@

$(DEST)/prep.cpp : $(SRC)/prep.cpp
	$(CP) $< $@

$(DEST)/Pressure.cpp : $(SRC)/Pressure.cxx
	$(CP) $< $@

$(DEST)/Pressure.h : $(SRC)/Pressure.h
	$(CP) $< $@

$(DEST)/print.cpp : $(SRC)/print.cpp
	$(CP) $< $@

$(DEST)/Reaction.cpp : $(SRC)/Reaction.cxx
	$(CP) $< $@

$(DEST)/Reaction.h : $(SRC)/Reaction.h
	$(CP) $< $@

$(DEST)/read.cpp : $(SRC)/read.cpp
	$(CP) $< $@

$(DEST)/ReadClass.cpp : $(SRC)/ReadClass.cxx
	$(CP) $< $@

$(DEST)/readtr.cpp : $(SRC)/readtr.cpp
	$(CP) $< $@

$(DEST)/runner.cpp : $(SRC)/runner.cpp
	$(CP) $< $@

$(DEST)/runner.h : $(SRC)/runner.h
	$(CP) $< $@

$(DEST)/SelectedOutput.cpp : $(SRC)/SelectedOutput.cpp
	$(CP) $< $@

$(DEST)/SelectedOutput.h : $(SRC)/SelectedOutput.h
	$(CP) $< $@

$(DEST)/Serializer.cpp : $(SRC)/Serializer.cxx
	$(CP) $< $@

$(DEST)/Serializer.h : $(SRC)/Serializer.h
	$(CP) $< $@

$(DEST)/sit.cpp : $(SRC)/sit.cpp
	$(CP) $< $@

$(DEST)/smalldense.cpp : $(SRC)/smalldense.cpp
	$(CP) $< $@

$(DEST)/smalldense.h : $(SRC)/smalldense.h
	$(CP) $< $@

$(DEST)/Solution.cpp : $(SRC)/Solution.cxx
	$(CP) $< $@

$(DEST)/Solution.h : $(SRC)/Solution.h
	$(CP) $< $@

$(DEST)/SolutionIsotope.cpp : $(SRC)/SolutionIsotope.cxx
	$(CP) $< $@

$(DEST)/SolutionIsotope.h : $(SRC)/SolutionIsotope.h
	$(CP) $< $@

$(DEST)/spread.cpp : $(SRC)/spread.cpp
	$(CP) $< $@

$(DEST)/SS.cpp : $(SRC)/SS.cxx
	$(CP) $< $@

$(DEST)/SS.h : $(SRC)/SS.h
	$(CP) $< $@

$(DEST)/SSassemblage.cpp : $(SRC)/SSassemblage.cxx
	$(CP) $< $@

$(DEST)/SSassemblage.h : $(SRC)/SSassemblage.h
	$(CP) $< $@

$(DEST)/SScomp.cpp : $(SRC)/SScomp.cxx
	$(CP) $< $@

$(DEST)/SScomp.h : $(SRC)/SScomp.h
	$(CP) $< $@

$(DEST)/step.cpp : $(SRC)/step.cpp
	$(CP) $< $@

$(DEST)/StorageBin.cpp : $(SRC)/StorageBin.cxx
	$(CP) $< $@

$(DEST)/StorageBin.h : $(SRC)/StorageBin.h
	$(CP) $< $@

$(DEST)/StorageBinList.cpp : $(SRC)/StorageBinList.cpp
	$(CP) $< $@

$(DEST)/StorageBinList.h : $(SRC)/StorageBinList.h
	$(CP) $< $@

$(DEST)/structures.cpp : $(SRC)/structures.cpp
	$(CP) $< $@

$(DEST)/sundialsmath.cpp : $(SRC)/sundialsmath.cpp
	$(CP) $< $@

$(DEST)/sundialsmath.h : $(SRC)/sundialsmath.h
	$(CP) $< $@

$(DEST)/sundialstypes.h : $(SRC)/sundialstypes.h
	$(CP) $< $@

$(DEST)/Surface.cpp : $(SRC)/Surface.cxx
	$(CP) $< $@

$(DEST)/Surface.h : $(SRC)/Surface.h
	$(CP) $< $@

$(DEST)/SurfaceCharge.cpp : $(SRC)/SurfaceCharge.cxx
	$(CP) $< $@

$(DEST)/SurfaceCharge.h : $(SRC)/SurfaceCharge.h
	$(CP) $< $@

$(DEST)/SurfaceComp.cpp : $(SRC)/SurfaceComp.cxx
	$(CP) $< $@

$(DEST)/SurfaceComp.h : $(SRC)/SurfaceComp.h
	$(CP) $< $@

$(DEST)/System.cpp : $(SRC)/System.cxx
	$(CP) $< $@

$(DEST)/System.h : $(SRC)/System.h
	$(CP) $< $@

$(DEST)/tally.cpp : $(SRC)/tally.cpp
	$(CP) $< $@

$(DEST)/Temperature.cpp : $(SRC)/Temperature.cxx
	$(CP) $< $@

$(DEST)/Temperature.h : $(SRC)/Temperature.h
	$(CP) $< $@

$(DEST)/tidy.cpp : $(SRC)/tidy.cpp
	$(CP) $< $@

$(DEST)/transport.cpp : $(SRC)/transport.cpp
	$(CP) $< $@

$(DEST)/Use.cpp : $(SRC)/Use.cpp
	$(CP) $< $@

$(DEST)/Use.h : $(SRC)/Use.h
	$(CP) $< $@

$(DEST)/UserPunch.cpp : $(SRC)/UserPunch.cpp
	$(CP) $< $@

$(DEST)/UserPunch.h : $(SRC)/UserPunch.h
	$(CP) $< $@

$(DEST)/utilities.cpp : $(SRC)/utilities.cpp
	$(CP) $< $@

$(DEST)/Utils.cpp : $(SRC)/common/Utils.cxx
	$(CP) $< $@

$(DEST)/Utils.h : $(SRC)/common/Utils.h
	$(CP) $< $@

###############################################


$(IDEST)/CSelectedOutput.cpp : $(ISRC)/CSelectedOutput.cpp
	sed \
	 -e 's/#include "CSelectedOutput.hxx"/#include "CSelectedOutputhxx.h"/g' \
	 -e 's/\.lVal/.u.lVal/g' \
	 -e 's/->lVal/->u.lVal/g' \
	 -e 's/\.dVal/.u.dVal/g' \
	 -e 's/->dVal/->u.dVal/g' \
	 -e 's/\.sVal/.u.sVal/g' \
	 -e 's/->sVal/->u.sVal/g' \
	 -e 's/\.vresult/.u.vresult/g' \
	 -e 's/->vresult/->u.vresult/g' \
	< $< > $@

$(IDEST)/CSelectedOutputhxx.h : $(ISRC)/CSelectedOutput.hxx
	sed 's/#include "CVar.hxx"/#include "CVarhxx.h"/g' $< > $@

$(IDEST)/CVarhxx.h : $(ISRC)/CVar.hxx
	sed \
	 -e 's/\.lVal/.u.lVal/g' \
	 -e 's/->lVal/->u.lVal/g' \
	 -e 's/\.dVal/.u.dVal/g' \
	 -e 's/->dVal/->u.dVal/g' \
	 -e 's/\.sVal/.u.sVal/g' \
	 -e 's/->sVal/->u.sVal/g' \
	 -e 's/\.vresult/.u.vresult/g' \
	 -e 's/->vresult/->u.vresult/g' \
	< $< > $@

$(IDEST)/Debug.h : $(ISRC)/Debug.h
	$(CP) $< $@

$(IDEST)/ErrorReporterhxx.h : $(ISRC)/ErrorReporter.hxx
	$(CP) $< $@

$(IDEST)/IPhreeqcCallbacks.h : ../src/IPhreeqcCallbacks.h
	$(CP) $< $@

$(IDEST)/IPhreeqc.cpp : $(ISRC)/IPhreeqc.cpp
	sed \
	 -e 's/#include "IPhreeqc.hpp"/#include "IPhreeqchpp.h"/g' \
	 -e 's/#include "ErrorReporter.hxx"/#include "ErrorReporterhxx.h"/g' \
	 -e 's/#include "CSelectedOutput.hxx"/#include "CSelectedOutputhxx.h"/g' \
	 -e 's/\.lVal/.u.lVal/g' \
	 -e 's/->lVal/->u.lVal/g' \
	 -e 's/\.dVal/.u.dVal/g' \
	 -e 's/->dVal/->u.dVal/g' \
	 -e 's/\.sVal/.u.sVal/g' \
	 -e 's/->sVal/->u.sVal/g' \
	 -e 's/\.vresult/.u.vresult/g' \
	 -e 's/->vresult/->u.vresult/g' \
	< $< > $@

$(IDEST)/IPhreeqc.h : $(ISRC)/IPhreeqc.h
	$(CP) $< $@

$(IDEST)/IPhreeqchpp.h : $(ISRC)/IPhreeqc.hpp
	$(CP) $< $@

$(IDEST)/IPhreeqcLib.cpp : $(ISRC)/IPhreeqcLib.cpp
	sed 's/#include "IPhreeqc.hpp"/#include "IPhreeqchpp.h"/g' $< > $@

$(IDEST)/thread.h : $(ISRC)/thread.h
	$(CP) $< $@

$(IDEST)/Var.c : $(ISRC)/Var.c
	sed \
	 -e 's/\.lVal/.u.lVal/g' \
	 -e 's/->lVal/->u.lVal/g' \
	 -e 's/\.dVal/.u.dVal/g' \
	 -e 's/->dVal/->u.dVal/g' \
	 -e 's/\.sVal/.u.sVal/g' \
	 -e 's/->sVal/->u.sVal/g' \
	 -e 's/\.vresult/.u.vresult/g' \
	 -e 's/->vresult/->u.vresult/g' \
	< $< > $@

$(IDEST)/Var.h : $(ISRC)/Var.h
	$(CP) $< $@

$(IDEST)/Version.h : $(ISRC)/Version.h
	$(CP) $< $@

########################################

$(INST)/phreeqc.dat : ../database/phreeqc.dat
	$(CP) $< $@

$(INST)/ex1 : ../phreeqc3-examples/ex1
	$(CP) $< $@

$(INST)/ex2 : ../phreeqc3-examples/ex2
	$(CP) $< $@

########################################

check : all
	R CMD check --as-cran phreeqc

check_built : phreeqc_$(VERSION).tar.gz
	Rscript -e "devtools::check_built(path=\"phreeqc_$(VERSION).tar.gz\")"

valgrind : install
	echo R -d "valgrind --tool=memcheck --leak-check=full" --vanilla < valgrind.R 2>&1 | tee valgrind.full.out

install : all
	echo R CMD INSTALL --build phreeqc

$(TOPDIR)/NAMESPACE : NAMESPACE
	$(CP) $< $@

$(TOPDIR)/R/phreeqc.R : phreeqc.R.in phreeqc.R-$(VERSION)_$(RELEASE_DATE).TIMESTAMP
	rm phreeqc.R-*.TIMESTAMP
	sed \
	 -e 's/@VERSION@/$(VERSION)/g' \
	 -e 's/@RELEASE_DATE@/$(RELEASE_DATE)/g' \
	< phreeqc.R.in > $(TOPDIR)/R/phreeqc.R
	touch phreeqc.R-$(VERSION)_$(RELEASE_DATE).TIMESTAMP

phreeqc.R-$(VERSION)_$(RELEASE_DATE).TIMESTAMP :
	touch phreeqc.R-$(VERSION)_$(RELEASE_DATE).TIMESTAMP

$(TOPDIR)/DESCRIPTION : DESCRIPTION.in DESCRIPTION-$(VERSION).TIMESTAMP
	rm DESCRIPTION-*.TIMESTAMP
	sed \
	 -e 's/@VERSION@/$(VERSION)/g' \
	< DESCRIPTION.in > $(TOPDIR)/DESCRIPTION
	touch DESCRIPTION-$(VERSION).TIMESTAMP

DESCRIPTION-$(VERSION).TIMESTAMP :
	touch DESCRIPTION-$(VERSION).TIMESTAMP

# build : all
# 	R CMD build --resave-data phreeqc

uninstall :
	R CMD REMOVE phreeqc

$(IDEST)/Makevars : Makevars
	$(CP) $< $@

$(IDEST)/R.cpp : R.cpp
	sed \
	 -e 's/#include "IPhreeqc.hpp"/#include "IPhreeqchpp.h"/g' \
	< $< > $@

$(IDEST):
	mkdir $(IDEST)

$(DEST):
	mkdir $(DEST)

# Force directory creation every time make is executed
# see https://www.cmcrossroads.com/article/making-directories-gnu-make

$(shell mkdir -p $(TOPDIR)/R)

$(shell mkdir -p $(TOPDIR)/src/phreeqcpp)

$(shell mkdir -p $(TOPDIR)/data)

clean :
	rm -f $(PSRC) $(XSRC) $(DEST)/*.o $(IDEST)/*.o $(DEST)/*.so
