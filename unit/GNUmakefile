CPPUNIT             = /z/parkplace/home/charlton
CC                  = g++
DEFINES             = -DNDEBUG -DSWIG_SHARED_OBJ -DPHREEQC_CPP -DPHREEQC_CLASS -DUSE_PHRQ_ALLOC -DCPPUNIT
CFLAGS              = -Wall -O3
CPPFLAGS            = $(DEFINES) -I../include -I../src -I../src/phreeqcpp -I../src/phreeqcpp/phreeqc -I$(CPPUNIT)/include
TARGET_ARCH         =
CXX                 = g++
CXXFLAGS            = -Wall
RANLIB              = ranlib
RM                  = rm -f
AR                  = ar ruv
TARGET              = unit
LDFLAGS             = -L$(CPPUNIT)/lib -ldl

VPATH=.:../src:../src/phreeqcpp:../src/phreeqcpp/phreeqc

%.o: %.f
	$(FC) $(FFLAGS) $(TARGET_ARCH) -c -o $@ $<

%.o: %.F
	$(FC) $(FFLAGS) $(TARGET_ARCH) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c -o $@ $<

%.o: %.cxx
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c -o $@ $<

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c -o $@ $<



TOBJS =	\
		TestCVar.o \
		TestIPhreeqc.o \
		TestIPhreeqcLib.o \
		TestSelectedOutput.o \
		TestVar.o \
		unit.o


SOBJS =	\
		IPhreeqc.o \
		IPhreeqcLib.o \
		SelectedOutput.o \
		Var.o

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
	$(CXX) $(LDFLAGS) -o unit $(TOBJS) $(POBJS) $(PPOBJS) $(SOBJS) -lcppunit

clean:
	$(RM) $(TOBJS) $(POBJS) $(PPOBJS) $(SOBJS) unit unit.exe *~


# TOBJS
TestCVar.o: TestCVar.cpp TestCVar.h ../src/CVar.hxx ../src/Debug.h \
  ../src/../include/Var.h
TestSelectedOutput.o: TestSelectedOutput.cpp TestSelectedOutput.h \
  ../src/SelectedOutput.hxx ../src/CVar.hxx ../src/Debug.h \
  ../src/../include/Var.h ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/phqalloc.h \
  ../src/phreeqcpp/phreeqc/input.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
TestVar.o: TestVar.cpp TestVar.h ../include/Var.h
unit.o: unit.cpp TestVar.h ../include/Var.h TestCVar.h ../src/CVar.hxx \
  ../src/Debug.h ../src/../include/Var.h TestSelectedOutput.h \
  ../src/SelectedOutput.hxx ../src/CVar.hxx

# POBJS
advection.o: ../src/phreeqcpp/phreeqc/advection.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
basic.o: ../src/phreeqcpp/phreeqc/basic.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h ../src/phreeqcpp/phreeqc/p2c.h
basicsubs.o: ../src/phreeqcpp/phreeqc/basicsubs.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
cl1.o: ../src/phreeqcpp/phreeqc/cl1.c ../src/phreeqcpp/phreeqc/phqalloc.h \
  ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/phrqtype.h
cvdense.o: ../src/phreeqcpp/phreeqc/cvdense.c \
  ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
  ../src/phreeqcpp/phreeqc/sundialstypes.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
  ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/phreeqc/smalldense.h \
  ../src/phreeqcpp/phreeqc/sundialsmath.h \
  ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/phqalloc.h
cvode.o: ../src/phreeqcpp/phreeqc/cvode.c \
  ../src/phreeqcpp/phreeqc/cvode.h \
  ../src/phreeqcpp/phreeqc/sundialstypes.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
  ../src/phreeqcpp/phreeqc/sundialsmath.h \
  ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/kinetics.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h
dense.o: ../src/phreeqcpp/phreeqc/dense.c \
  ../src/phreeqcpp/phreeqc/sundialstypes.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/sundialsmath.h \
  ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/phreeqc/smalldense.h \
  ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/phqalloc.h
dw.o: ../src/phreeqcpp/phreeqc/dw.c ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/pitzer.h
input.o: ../src/phreeqcpp/phreeqc/input.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/input.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h
integrate.o: ../src/phreeqcpp/phreeqc/integrate.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
inverse.o: ../src/phreeqcpp/phreeqc/inverse.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
isotopes.o: ../src/phreeqcpp/phreeqc/isotopes.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
kinetics.o: ../src/phreeqcpp/phreeqc/kinetics.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h \
  ../src/phreeqcpp/phreeqc/sundialstypes.h \
  ../src/phreeqcpp/phreeqc/cvode.h ../src/phreeqcpp/phreeqc/nvector.h \
  ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/dense.h \
  ../src/phreeqcpp/phreeqc/smalldense.h \
  ../src/phreeqcpp/phreeqc/nvector_serial.h \
  ../src/phreeqcpp/phreeqc/kinetics.h
mainsubs.o: ../src/phreeqcpp/phreeqc/mainsubs.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h ../src/phreeqcpp/phreeqc/input.h
model.o: ../src/phreeqcpp/phreeqc/model.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
nvector.o: ../src/phreeqcpp/phreeqc/nvector.c \
  ../src/phreeqcpp/phreeqc/nvector.h \
  ../src/phreeqcpp/phreeqc/sundialstypes.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/output.h
nvector_serial.o: ../src/phreeqcpp/phreeqc/nvector_serial.c \
  ../src/phreeqcpp/phreeqc/nvector_serial.h \
  ../src/phreeqcpp/phreeqc/nvector.h \
  ../src/phreeqcpp/phreeqc/sundialstypes.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/sundialsmath.h \
  ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/phqalloc.h
p2clib.o: ../src/phreeqcpp/phreeqc/p2clib.c \
  ../src/phreeqcpp/phreeqc/p2c.h ../src/phreeqcpp/phreeqc/output.h
parse.o: ../src/phreeqcpp/phreeqc/parse.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
phqalloc.o: ../src/phreeqcpp/phreeqc/phqalloc.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/output.h
pitzer.o: ../src/phreeqcpp/phreeqc/pitzer.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h ../src/phreeqcpp/phreeqc/pitzer.h
pitzer_structures.o: ../src/phreeqcpp/phreeqc/pitzer_structures.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h ../src/phreeqcpp/phreeqc/pitzer.h
prep.o: ../src/phreeqcpp/phreeqc/prep.c ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/phqalloc.h \
  ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/phrqproto.h
print.o: ../src/phreeqcpp/phreeqc/print.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h ../src/phreeqcpp/phreeqc/pitzer.h
read.o: ../src/phreeqcpp/phreeqc/read.c ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/phqalloc.h \
  ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/phrqproto.h
readtr.o: ../src/phreeqcpp/phreeqc/readtr.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
smalldense.o: ../src/phreeqcpp/phreeqc/smalldense.c \
  ../src/phreeqcpp/phreeqc/smalldense.h \
  ../src/phreeqcpp/phreeqc/sundialstypes.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/sundialsmath.h \
  ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/phqalloc.h
spread.o: ../src/phreeqcpp/phreeqc/spread.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
step.o: ../src/phreeqcpp/phreeqc/step.c ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/phqalloc.h \
  ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/phrqproto.h
structures.o: ../src/phreeqcpp/phreeqc/structures.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
sundialsmath.o: ../src/phreeqcpp/phreeqc/sundialsmath.c \
  ../src/phreeqcpp/phreeqc/sundialsmath.h \
  ../src/phreeqcpp/phreeqc/sundialstypes.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/output.h
tally.o: ../src/phreeqcpp/phreeqc/tally.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
tidy.o: ../src/phreeqcpp/phreeqc/tidy.c ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/phqalloc.h \
  ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/phrqproto.h
transport.o: ../src/phreeqcpp/phreeqc/transport.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
utilities.o: ../src/phreeqcpp/phreeqc/utilities.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h

# SOBJS
SelectedOutput.o: ../src/SelectedOutput.cpp ../src/SelectedOutput.hxx \
 ../src/CVar.hxx ../src/Debug.h ../include/Var.h
Var.o: ../src/Var.c ../src/../include/Var.h
fwrap.o: ../src/fwrap.cpp ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/../include/Var.h
global.o: ../src/global.c ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/phqalloc.h
module_files.o: ../src/module_files.cpp ../src/module_files.h \
  ../src/phreeqcpp/phreeqc/phreeqc_files.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h ../src/phreeqcpp/phreeqc/input.h
module_output.o: ../src/module_output.cpp ../src/module_files.h \
  ../src/phreeqcpp/phreeqc/output.c ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h

# PPOBJS
SolutionIsotope.o: ../src/phreeqcpp/SolutionIsotope.cxx \
 ../src/phreeqcpp/Utils.h ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
PPassemblageComp.o: ../src/phreeqcpp/PPassemblageComp.cxx \
 ../src/phreeqcpp/Utils.h ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/PPassemblageComp.h ../src/phreeqcpp/NameDouble.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Dictionary.h ../src/phreeqcpp/Solution.h \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
KineticsComp.o: ../src/phreeqcpp/KineticsComp.cxx \
 ../src/phreeqcpp/Utils.h ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/KineticsComp.h ../src/phreeqcpp/NameDouble.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Dictionary.h ../src/phreeqcpp/Solution.h \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
Surface.o: ../src/phreeqcpp/Surface.cxx ../src/phreeqcpp/Phreeqc_class.h \
 ../src/phreeqcpp/Utils.h ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h ../src/phreeqcpp/Surface.h \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/SurfaceComp.h \
 ../src/phreeqcpp/NameDouble.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/SurfaceCharge.h ../src/phreeqcpp/cxxMix.h \
 ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/phrqproto.h
Parser.o: ../src/phreeqcpp/Parser.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Phreeqc_class.h
cxxKinetics.o: ../src/phreeqcpp/cxxKinetics.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/cxxKinetics.h ../src/phreeqcpp/NumKeyword.h \
 ../src/phreeqcpp/KineticsComp.h ../src/phreeqcpp/NameDouble.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/cxxMix.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
StorageBin.o: ../src/phreeqcpp/StorageBin.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/NameDouble.h ../src/phreeqcpp/Phreeqc_class.h \
 ../src/phreeqcpp/Parser.h ../src/phreeqcpp/StorageBin.h \
 ../src/phreeqcpp/System.h ../src/phreeqcpp/SSassemblage.h \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/Solution.h \
 ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/Exchange.h \
 ../src/phreeqcpp/ExchComp.h ../src/phreeqcpp/GasPhase.h \
 ../src/phreeqcpp/cxxKinetics.h ../src/phreeqcpp/KineticsComp.h \
 ../src/phreeqcpp/PPassemblage.h ../src/phreeqcpp/PPassemblageComp.h \
 ../src/phreeqcpp/SSassemblageSS.h ../src/phreeqcpp/Surface.h \
 ../src/phreeqcpp/SurfaceComp.h ../src/phreeqcpp/SurfaceCharge.h \
 ../src/phreeqcpp/cxxMix.h ../src/phreeqcpp/Reaction.h \
 ../src/phreeqcpp/Temperature.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
Dictionary.o: ../src/phreeqcpp/Dictionary.cxx \
 ../src/phreeqcpp/Dictionary.h ../src/phreeqcpp/Solution.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/NumKeyword.h \
 ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/NameDouble.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h ../src/phreeqcpp/phreeqc/output.h
SurfaceComp.o: ../src/phreeqcpp/SurfaceComp.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/SurfaceComp.h ../src/phreeqcpp/Phreeqc_class.h \
 ../src/phreeqcpp/NameDouble.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Dictionary.h ../src/phreeqcpp/Solution.h \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
NameDouble.o: ../src/phreeqcpp/NameDouble.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/NameDouble.h ../src/phreeqcpp/Phreeqc_class.h \
 ../src/phreeqcpp/Parser.h ../src/phreeqcpp/Dictionary.h \
 ../src/phreeqcpp/Solution.h ../src/phreeqcpp/NumKeyword.h \
 ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
Exchange.o: ../src/phreeqcpp/Exchange.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h ../src/phreeqcpp/cxxMix.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/NumKeyword.h \
 ../src/phreeqcpp/Exchange.h ../src/phreeqcpp/ExchComp.h \
 ../src/phreeqcpp/NameDouble.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/phrqproto.h
System.o: ../src/phreeqcpp/System.cxx ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h ../src/phreeqcpp/System.h \
 ../src/phreeqcpp/NameDouble.h ../src/phreeqcpp/Phreeqc_class.h \
 ../src/phreeqcpp/Parser.h ../src/phreeqcpp/SSassemblage.h \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/Solution.h \
 ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/Exchange.h \
 ../src/phreeqcpp/ExchComp.h ../src/phreeqcpp/GasPhase.h \
 ../src/phreeqcpp/cxxKinetics.h ../src/phreeqcpp/KineticsComp.h \
 ../src/phreeqcpp/PPassemblage.h ../src/phreeqcpp/PPassemblageComp.h \
 ../src/phreeqcpp/SSassemblageSS.h ../src/phreeqcpp/Surface.h \
 ../src/phreeqcpp/SurfaceComp.h ../src/phreeqcpp/SurfaceCharge.h \
 ../src/phreeqcpp/cxxMix.h ../src/phreeqcpp/Reaction.h \
 ../src/phreeqcpp/Temperature.h
ISolution.o: ../src/phreeqcpp/ISolution.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/ISolution.h ../src/phreeqcpp/ISolutionComp.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/NumKeyword.h \
 ../src/phreeqcpp/Solution.h ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/NameDouble.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
class_main.o: ../src/phreeqcpp/class_main.cpp ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
runner.o: ../src/phreeqcpp/runner.cpp ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Phreeqc_class.h
Temperature.o: ../src/phreeqcpp/Temperature.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Parser.h ../src/phreeqcpp/Phreeqc_class.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/Temperature.h ../src/phreeqcpp/NumKeyword.h \
 ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/phrqproto.h
ReadClass.o: ../src/phreeqcpp/ReadClass.cxx ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Solution.h ../src/phreeqcpp/NumKeyword.h \
 ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/NameDouble.h \
 ../src/phreeqcpp/Exchange.h ../src/phreeqcpp/ExchComp.h \
 ../src/phreeqcpp/Surface.h ../src/phreeqcpp/SurfaceComp.h \
 ../src/phreeqcpp/SurfaceCharge.h ../src/phreeqcpp/PPassemblage.h \
 ../src/phreeqcpp/PPassemblageComp.h ../src/phreeqcpp/cxxKinetics.h \
 ../src/phreeqcpp/KineticsComp.h ../src/phreeqcpp/SSassemblage.h \
 ../src/phreeqcpp/GasPhase.h ../src/phreeqcpp/Reaction.h \
 ../src/phreeqcpp/cxxMix.h ../src/phreeqcpp/Temperature.h \
 ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/phrqproto.h
SurfaceCharge.o: ../src/phreeqcpp/SurfaceCharge.cxx \
 ../src/phreeqcpp/Utils.h ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/SurfaceCharge.h ../src/phreeqcpp/NameDouble.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Dictionary.h ../src/phreeqcpp/Solution.h \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
NumKeyword.o: ../src/phreeqcpp/NumKeyword.cxx \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Phreeqc_class.h
ISolutionComp.o: ../src/phreeqcpp/ISolutionComp.cxx \
 ../src/phreeqcpp/Utils.h ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/ISolutionComp.h ../src/phreeqcpp/Phreeqc_class.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h ../src/phreeqcpp/phreeqc/phqalloc.h
StorageBinList.o: ../src/phreeqcpp/StorageBinList.cpp \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Phreeqc_class.h
SolutionIsotopeList.o: ../src/phreeqcpp/SolutionIsotopeList.cxx \
 ../src/phreeqcpp/Utils.h ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
cxxMix.o: ../src/phreeqcpp/cxxMix.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Parser.h ../src/phreeqcpp/Phreeqc_class.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h ../src/phreeqcpp/cxxMix.h \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
Utils.o: ../src/phreeqcpp/Utils.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Parser.h ../src/phreeqcpp/Phreeqc_class.h \
 ../src/phreeqcpp/phreeqc/output.h
SAXPhreeqc.o: ../src/phreeqcpp/SAXPhreeqc.cxx \
 ../src/phreeqcpp/SAXPhreeqc.h ../src/phreeqcpp/SaxPhreeqcHandlers.h \
 ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h ../src/phreeqcpp/phreeqc/phrqproto.h \
 ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h
ExchComp.o: ../src/phreeqcpp/ExchComp.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h ../src/phreeqcpp/ExchComp.h \
 ../src/phreeqcpp/NameDouble.h ../src/phreeqcpp/Phreeqc_class.h \
 ../src/phreeqcpp/Parser.h ../src/phreeqcpp/Dictionary.h \
 ../src/phreeqcpp/Solution.h ../src/phreeqcpp/NumKeyword.h \
 ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
SSassemblage.o: ../src/phreeqcpp/SSassemblage.cxx \
 ../src/phreeqcpp/Utils.h ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/SSassemblage.h ../src/phreeqcpp/Phreeqc_class.h \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/NameDouble.h \
 ../src/phreeqcpp/Parser.h ../src/phreeqcpp/SSassemblageSS.h \
 ../src/phreeqcpp/cxxMix.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
Solution.o: ../src/phreeqcpp/Solution.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h ../src/phreeqcpp/Solution.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/NumKeyword.h \
 ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/NameDouble.h ../src/phreeqcpp/cxxMix.h \
 ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/phrqproto.h
SSassemblageSS.o: ../src/phreeqcpp/SSassemblageSS.cxx \
 ../src/phreeqcpp/Utils.h ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/SSassemblageSS.h ../src/phreeqcpp/NameDouble.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Dictionary.h ../src/phreeqcpp/Solution.h \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/SolutionIsotopeList.h \
 ../src/phreeqcpp/SolutionIsotope.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
GasPhase.o: ../src/phreeqcpp/GasPhase.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h ../src/phreeqcpp/GasPhase.h \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/NameDouble.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/cxxMix.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
PPassemblage.o: ../src/phreeqcpp/PPassemblage.cxx \
 ../src/phreeqcpp/Utils.h ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/PPassemblage.h ../src/phreeqcpp/NumKeyword.h \
 ../src/phreeqcpp/PPassemblageComp.h ../src/phreeqcpp/NameDouble.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/cxxMix.h ../src/phreeqcpp/phreeqc/phqalloc.h \
 ../src/phreeqcpp/phreeqc/phrqproto.h
Reaction.o: ../src/phreeqcpp/Reaction.cxx ../src/phreeqcpp/Utils.h \
 ../src/phreeqcpp/Phreeqc.h ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h ../src/phreeqcpp/Reaction.h \
 ../src/phreeqcpp/NumKeyword.h ../src/phreeqcpp/NameDouble.h \
 ../src/phreeqcpp/Phreeqc_class.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/phrqproto.h
Phreeqc.o: ../src/phreeqcpp/Phreeqc.cpp ../src/phreeqcpp/Phreeqc.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/nvector.h \
 ../src/phreeqcpp/phreeqc/sundialstypes.h \
 ../src/phreeqcpp/phreeqc/cvdense.h ../src/phreeqcpp/phreeqc/cvode.h \
 ../src/phreeqcpp/phreeqc/nvector.h ../src/phreeqcpp/phreeqc/dense.h \
 ../src/phreeqcpp/phreeqc/smalldense.h \
 ../src/phreeqcpp/phreeqc/nvector_serial.h \
 ../src/phreeqcpp/phreeqc/dense.h ../src/phreeqcpp/runner.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/phreeqc/p2c.h \
 ../src/phreeqcpp/phreeqc/global_structures.h \
 ../src/phreeqcpp/phreeqc/basic.h \
 ../src/phreeqcpp/phreeqc/pitzer_structures.h \
 ../src/phreeqcpp/phreeqc/pitzer.h ../src/phreeqcpp/phreeqc/input.h \
 ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/global.h \
 ../src/phreeqcpp/phreeqc/global_structures.h
dumper.o: ../src/phreeqcpp/dumper.cpp ../src/phreeqcpp/dumper.h \
 ../src/phreeqcpp/StorageBinList.h ../src/phreeqcpp/Parser.h \
 ../src/phreeqcpp/Phreeqc_class.h

