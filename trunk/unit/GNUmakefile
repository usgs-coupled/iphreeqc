CPPFLAGS            = 
CPPUNIT             = /z/parkplace/home/charlton
CXX                 = g++
CXXCOMPILE          = $(CXX) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CXXFLAGS)
CXXFLAGS            = -g -O2 -Wall
CXXLINK             = $(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@
DEFS                = -DNDEBUG -DSWIG_SHARED_OBJ -DPHREEQC_CPP -DPHREEQC_CLASS -DUSE_PHRQ_ALLOC -DCPPUNIT
DEPDIR              = .deps
INCLUDES            = -I../include -I../src -I../src/phreeqcpp -I../src/phreeqcpp/phreeqc -I$(CPPUNIT)/include
LDFLAGS             = -L$(CPPUNIT)/lib 
LIBS                = -lcppunit -ldl -lm
MKDIR_P             = /bin/mkdir -p
MV                  = mv -f
RANLIB              = ranlib
RM                  = rm -f
TARGET              = unit
VPATH               = .:../src:../src/phreeqcpp:../src/phreeqcpp/phreeqc

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
	SelectedOutput.o \
	Var.o

# phreeqcpp objs
PPOBJS = \
	cxxKinetics.o \
	cxxMix.o \
	dumper.o \
	Exchange.o \
	ExchComp.o \
	GasPhase.o \
	ISolutionComp.o \
	ISolution.o \
	KineticsComp.o \
	NameDouble.o \
	NumKeyword.o \
	Parser.o \
	Phreeqc.o \
	PPassemblageComp.o \
	PPassemblage.o \
	Reaction.o \
	ReadClass.o \
	runner.o \
	Solution.o \
	SolutionIsotope.o \
	SolutionIsotopeList.o \
	SSassemblage.o \
	SSassemblageSS.o \
	StorageBin.o \
	StorageBinList.o \
	SurfaceCharge.o \
	SurfaceComp.o \
	Surface.o \
	System.o \
	Temperature.o \
	Utils.o

# phreeqc objs
POBJS =	\
	advection.o \
	basic.o \
	basicsubs.o \
	cl1.o \
	cvdense.o \
	cvode.o \
	dense.o \
	dw.o \
	input.o \
	integrate.o \
	inverse.o \
	isotopes.o \
	kinetics.o \
	mainsubs.o \
	model.o \
	nvector.o \
	nvector_serial.o \
	output.o \
	p2clib.o \
	parse.o \
	phqalloc.o \
	phreeqc_files.o \
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

$(TARGET): $(TOBJS) $(POBJS) $(PPOBJS) $(SOBJS)
	$(CXXLINK) $(TOBJS) $(POBJS) $(PPOBJS) $(SOBJS) $(LIBS)

clean:
	$(RM) $(TOBJS) $(POBJS) $(PPOBJS) $(SOBJS) unit unit.exe *~

-include ./$(DEPDIR)/advection.Po
-include ./$(DEPDIR)/basic.Po
-include ./$(DEPDIR)/basicsubs.Po
-include ./$(DEPDIR)/cl1.Po
-include ./$(DEPDIR)/cvdense.Po
-include ./$(DEPDIR)/cvode.Po
-include ./$(DEPDIR)/cxxKinetics.Po
-include ./$(DEPDIR)/cxxMix.Po
-include ./$(DEPDIR)/dense.Po
-include ./$(DEPDIR)/dumper.Po
-include ./$(DEPDIR)/dw.Po
-include ./$(DEPDIR)/Exchange.Po
-include ./$(DEPDIR)/ExchComp.Po
-include ./$(DEPDIR)/GasPhase.Po
-include ./$(DEPDIR)/input.Po
-include ./$(DEPDIR)/integrate.Po
-include ./$(DEPDIR)/inverse.Po
-include ./$(DEPDIR)/IPhreeqc.Po
-include ./$(DEPDIR)/IPhreeqcLib.Po
-include ./$(DEPDIR)/ISolution.Po
-include ./$(DEPDIR)/ISolutionComp.Po
-include ./$(DEPDIR)/isotopes.Po
-include ./$(DEPDIR)/kinetics.Po
-include ./$(DEPDIR)/KineticsComp.Po
-include ./$(DEPDIR)/mainsubs.Po
-include ./$(DEPDIR)/model.Po
-include ./$(DEPDIR)/NameDouble.Po
-include ./$(DEPDIR)/NumKeyword.Po
-include ./$(DEPDIR)/nvector.Po
-include ./$(DEPDIR)/nvector_serial.Po
-include ./$(DEPDIR)/output.Po
-include ./$(DEPDIR)/p2clib.Po
-include ./$(DEPDIR)/parse.Po
-include ./$(DEPDIR)/Parser.Po
-include ./$(DEPDIR)/phqalloc.Po
-include ./$(DEPDIR)/Phreeqc.Po
-include ./$(DEPDIR)/phreeqc_files.Po
-include ./$(DEPDIR)/pitzer.Po
-include ./$(DEPDIR)/pitzer_structures.Po
-include ./$(DEPDIR)/PPassemblage.Po
-include ./$(DEPDIR)/PPassemblageComp.Po
-include ./$(DEPDIR)/prep.Po
-include ./$(DEPDIR)/print.Po
-include ./$(DEPDIR)/Reaction.Po
-include ./$(DEPDIR)/read.Po
-include ./$(DEPDIR)/ReadClass.Po
-include ./$(DEPDIR)/readtr.Po
-include ./$(DEPDIR)/runner.Po
-include ./$(DEPDIR)/SelectedOutput.Po
-include ./$(DEPDIR)/sit.Po
-include ./$(DEPDIR)/smalldense.Po
-include ./$(DEPDIR)/Solution.Po
-include ./$(DEPDIR)/SolutionIsotope.Po
-include ./$(DEPDIR)/SolutionIsotopeList.Po
-include ./$(DEPDIR)/spread.Po
-include ./$(DEPDIR)/SSassemblage.Po
-include ./$(DEPDIR)/SSassemblageSS.Po
-include ./$(DEPDIR)/step.Po
-include ./$(DEPDIR)/StorageBin.Po
-include ./$(DEPDIR)/StorageBinList.Po
-include ./$(DEPDIR)/structures.Po
-include ./$(DEPDIR)/sundialsmath.Po
-include ./$(DEPDIR)/Surface.Po
-include ./$(DEPDIR)/SurfaceCharge.Po
-include ./$(DEPDIR)/SurfaceComp.Po
-include ./$(DEPDIR)/System.Po
-include ./$(DEPDIR)/tally.Po
-include ./$(DEPDIR)/Temperature.Po
-include ./$(DEPDIR)/TestCVar.Po
-include ./$(DEPDIR)/TestIPhreeqc.Po
-include ./$(DEPDIR)/TestIPhreeqcLib.Po
-include ./$(DEPDIR)/TestSelectedOutput.Po
-include ./$(DEPDIR)/TestVar.Po
-include ./$(DEPDIR)/tidy.Po
-include ./$(DEPDIR)/transport.Po
-include ./$(DEPDIR)/unit.Po
-include ./$(DEPDIR)/utilities.Po
-include ./$(DEPDIR)/Utils.Po
-include ./$(DEPDIR)/Var.Po
