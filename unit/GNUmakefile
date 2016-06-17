CPPFLAGS            = 
##CPPUNIT             = /z/parkplace/home/charlton
CXX                 = g++
CXXCOMPILE          = $(CXX) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CXXFLAGS)
CXXFLAGS            = -g -O2 -Wall -fno-var-tracking-assignments
##CXXFLAGS            = -fprofile-arcs -ftest-coverage
CXXLINK             = $(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@
DEFS                = -DNDEBUG -DSWIG_SHARED_OBJ -DPHREEQC_CPP -DPHREEQC_CLASS -DUSE_PHRQ_ALLOC -DCPPUNIT
DEPDIR              = .deps
INCLUDES            = -I../src -I../src/phreeqcpp -I../src/phreeqcpp/common -I../src/phreeqcpp/PhreeqcKeywords
## -I$(CPPUNIT)/include
##LDFLAGS             = -L$(CPPUNIT)/lib 
LIBS                = -lcppunit -ldl -lm
MKDIR_P             = /bin/mkdir -p
MV                  = mv -f
RANLIB              = ranlib
RM                  = rm -f
TARGET              = unit
VPATH               = .:../src:../src/phreeqcpp:../src/phreeqcpp/common:../src/phreeqcpp/PhreeqcKeywords

%.o: %.c
	test -z "$(DEPDIR)" || $(MKDIR_P) "$(DEPDIR)"
	depbase=`echo $@ | sed 's|[^/]*$$|$(DEPDIR)/&|;s|\.o$$||'`;\
	$(CXXCOMPILE) -MT $@ -MD -MP -MF $$depbase.Tpo -c -o $@ $< &&\
	$(MV) $$depbase.Tpo $$depbase.Po

%.o: %.cpp
	test -z "$(DEPDIR)" || $(MKDIR_P) "$(DEPDIR)"
	depbase=`echo $@ | sed 's|[^/]*$$|$(DEPDIR)/&|;s|\.o$$||'`;\
	$(CXXCOMPILE) -MT $@ -MD -MP -MF $$depbase.Tpo -c -o $@ $< &&\
	$(MV) $$depbase.Tpo $$depbase.Po

%.o: %.cxx
	test -z "$(DEPDIR)" || $(MKDIR_P) "$(DEPDIR)"
	depbase=`echo $@ | sed 's|[^/]*$$|$(DEPDIR)/&|;s|\.o$$||'`;\
	$(CXXCOMPILE) -MT $@ -MD -MP -MF $$depbase.Tpo -c -o $@ $< &&\
	$(MV) $$depbase.Tpo $$depbase.Po


# test objs
TOBJS =	\
	FileTest.o \
	TestCVar.o \
	TestIPhreeqc.o \
	TestIPhreeqcLib.o \
	TestSelectedOutput.o \
	TestVar.o \
	unit.o

# iphreeqc objs
SOBJS =	\
	IPhreeqc.o \
	IPhreeqcLib.o \
	CSelectedOutput.o \
	Var.o

# phreeqcpp objs
PPOBJS = \
	cxxKinetics.o \
	cxxMix.o \
	Dictionary.o \
	dumper.o \
	Exchange.o \
	ExchComp.o \
	GasComp.o \
	GasPhase.o \
	ISolutionComp.o \
	ISolution.o \
	Keywords.o \
	KineticsComp.o \
	NameDouble.o \
	NumKeyword.o \
	Parser.o \
	PBasic.o \
	Phreeqc.o \
	PHRQ_base.o \
	PHRQ_io.o \
	PPassemblageComp.o \
	PPassemblage.o \
	Pressure.o \
	Reaction.o \
	ReadClass.o \
	runner.o \
	SelectedOutput.o \
	Serializer.o \
	Solution.o \
	SolutionIsotope.o \
	SSassemblage.o \
	SScomp.o \
	SS.o \
	StorageBin.o \
	StorageBinList.o \
	SurfaceCharge.o \
	SurfaceComp.o \
	Surface.o \
	System.o \
	Temperature.o \
	Use.o \
	UserPunch.o \
	Utils.o

# phreeqc objs
POBJS =	\
	advection.o \
	basicsubs.o \
	cl1.o \
	cvdense.o \
	cvode.o \
	dense.o \
	gases.o \
	input.o \
	integrate.o \
	inverse.o \
	isotopes.o \
	kinetics.o \
	mainsubs.o \
	model.o \
	nvector.o \
	nvector_serial.o \
	parse.o \
	phqalloc.o \
	PHRQ_io_output.o \
	pitzer.o \
	pitzer_structures.o \
	prep.o \
	print.o \
	read.o \
	readtr.o \
	sit.o \
	smalldense.o \
	spread.o \
	step.o \
	structures.o \
	sundialsmath.o \
	tally.o \
	tidy.o \
	transport.o \
	utilities.o

all: $(TARGET)

ifeq ($(HOSTNAME),parkplace.cr.usgs.gov)
export LD_LIBRARY_PATH=/home/charlton/x64/lib
endif

ifeq ($(HOSTNAME),lobo02)
export LD_LIBRARY_PATH=/z/parkplace/home/charlton/x64/lib
endif

check: $(TARGET)
	./$(TARGET) 

valgrind: $(TARGET)
	valgrind --leak-check=full --log-file=$(TARGET).valgrind.out  ./$(TARGET)

$(TARGET): $(TOBJS) $(POBJS) $(PPOBJS) $(SOBJS)
	$(CXXLINK) $(TOBJS) $(POBJS) $(PPOBJS) $(SOBJS) $(LIBS)

clean:
	$(RM) $(TOBJS) $(POBJS) $(PPOBJS) $(SOBJS) unit unit.exe *~

-include ./$(DEPDIR)/advection.Po
-include ./$(DEPDIR)/basicsubs.Po
-include ./$(DEPDIR)/cl1.Po
-include ./$(DEPDIR)/CSelectedOutput.Po
-include ./$(DEPDIR)/cvdense.Po
-include ./$(DEPDIR)/cvode.Po
-include ./$(DEPDIR)/cxxKinetics.Po
-include ./$(DEPDIR)/cxxMix.Po
-include ./$(DEPDIR)/dense.Po
-include ./$(DEPDIR)/Dictionary.Po
-include ./$(DEPDIR)/dumper.Po
-include ./$(DEPDIR)/Exchange.Po
-include ./$(DEPDIR)/ExchComp.Po
-include ./$(DEPDIR)/files
-include ./$(DEPDIR)/GasComp.Po
-include ./$(DEPDIR)/gases.Po
-include ./$(DEPDIR)/GasPhase.Po
-include ./$(DEPDIR)/input.Po
-include ./$(DEPDIR)/integrate.Po
-include ./$(DEPDIR)/inverse.Po
-include ./$(DEPDIR)/IPhreeqcLib.Po
-include ./$(DEPDIR)/IPhreeqc.Po
-include ./$(DEPDIR)/ISolutionComp.Po
-include ./$(DEPDIR)/ISolution.Po
-include ./$(DEPDIR)/isotopes.Po
-include ./$(DEPDIR)/Keywords.Po
-include ./$(DEPDIR)/KineticsComp.Po
-include ./$(DEPDIR)/kinetics.Po
-include ./$(DEPDIR)/mainsubs.Po
-include ./$(DEPDIR)/model.Po
-include ./$(DEPDIR)/NameDouble.Po
-include ./$(DEPDIR)/NumKeyword.Po
-include ./$(DEPDIR)/nvector.Po
-include ./$(DEPDIR)/nvector_serial.Po
-include ./$(DEPDIR)/parse.Po
-include ./$(DEPDIR)/Parser.Po
-include ./$(DEPDIR)/PBasic.Po
-include ./$(DEPDIR)/phqalloc.Po
-include ./$(DEPDIR)/Phreeqc.Po
-include ./$(DEPDIR)/PHRQ_base.Po
-include ./$(DEPDIR)/PHRQ_io_output.Po
-include ./$(DEPDIR)/PHRQ_io.Po
-include ./$(DEPDIR)/pitzer.Po
-include ./$(DEPDIR)/pitzer_structures.Po
-include ./$(DEPDIR)/PPassemblageComp.Po
-include ./$(DEPDIR)/PPassemblage.Po
-include ./$(DEPDIR)/prep.Po
-include ./$(DEPDIR)/Pressure.Po
-include ./$(DEPDIR)/print.Po
-include ./$(DEPDIR)/Reaction.Po
-include ./$(DEPDIR)/ReadClass.Po
-include ./$(DEPDIR)/read.Po
-include ./$(DEPDIR)/readtr.Po
-include ./$(DEPDIR)/runner.Po
-include ./$(DEPDIR)/SelectedOutput.Po
-include ./$(DEPDIR)/Serializer.Po
-include ./$(DEPDIR)/sit.Po
-include ./$(DEPDIR)/smalldense.Po
-include ./$(DEPDIR)/SolutionIsotopeList.Po
-include ./$(DEPDIR)/SolutionIsotope.Po
-include ./$(DEPDIR)/Solution.Po
-include ./$(DEPDIR)/spread.Po
-include ./$(DEPDIR)/SSassemblage.Po
-include ./$(DEPDIR)/SScomp.Po
-include ./$(DEPDIR)/SS.Po
-include ./$(DEPDIR)/step.Po
-include ./$(DEPDIR)/StorageBinList.Po
-include ./$(DEPDIR)/StorageBin.Po
-include ./$(DEPDIR)/structures.Po
-include ./$(DEPDIR)/sundialsmath.Po
-include ./$(DEPDIR)/SurfaceCharge.Po
-include ./$(DEPDIR)/SurfaceComp.Po
-include ./$(DEPDIR)/Surface.Po
-include ./$(DEPDIR)/System.Po
-include ./$(DEPDIR)/tally.Po
-include ./$(DEPDIR)/Temperature.Po
-include ./$(DEPDIR)/TestCVar.Po
-include ./$(DEPDIR)/TestIPhreeqcLib.Po
-include ./$(DEPDIR)/TestIPhreeqc.Po
-include ./$(DEPDIR)/TestSelectedOutput.Po
-include ./$(DEPDIR)/TestVar.Po
-include ./$(DEPDIR)/tidy.Po
-include ./$(DEPDIR)/transport.Po
-include ./$(DEPDIR)/unit.Po
-include ./$(DEPDIR)/UserPunch.Po
-include ./$(DEPDIR)/utilities.Po
-include ./$(DEPDIR)/Utils.Po
-include ./$(DEPDIR)/Var.Po
