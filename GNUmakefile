CC                  = g++
DEFINES             = -DNDEBUG -DSWIG_SHARED_OBJ -DPHREEQC_CPP -DPHREEQC_CLASS -DUSE_PHRQ_ALLOC
CFLAGS              = -Wall
CPPFLAGS            = -O3 $(DEFINES) -Iinclude -Isrc -Isrc/phreeqcpp -Isrc/phreeqcpp/phreeqc
TARGET_ARCH         =
FC                  = g95
FFLAGS              = -fno-second-underscore
CXX                 = g++
CXXFLAGS            = -Wall
RANLIB              = ranlib
RM                  = rm -f
MKDIR               = mkdir -p
AR                  = ar ruv
TARGET              = lib/libiphreeqc.a

VPATH=src:src/phreeqcpp:src/phreeqcpp/phreeqc

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
	

OBJS = \
		advection.o \
		basic.o \
		basicsubs.o \
		cl1.o \
		cvdense.o \
		cvode.o \
		cxxKinetics.o \
		cxxMix.o \
		dense.o \
		dumper.o \
		dw.o \
		Exchange.o \
		ExchComp.o \
		fwrap.o \
		fwrap2.o \
		fwrap3.o \
		GasPhase.o \
		input.o \
		integrate.o \
		inverse.o \
		IPhreeqc2.o \
		IPhreeqcF.o \
		IPhreeqcLib.o \
		ISolution.o \
		ISolutionComp.o \
		isotopes.o \
		kinetics.o \
		KineticsComp.o \
		mainsubs.o \
		model.o \
		NameDouble.o \
		NumKeyword.o \
		nvector.o \
		nvector_serial.o \
		output.o \
		p2clib.o \
		parse.o \
		Parser.o \
		phqalloc.o \
		Phreeqc.o \
		phreeqc_files.o \
		pitzer.o \
		pitzer_structures.o \
		pp_sys.o \
		PPassemblage.o \
		PPassemblageComp.o \
		prep.o \
		print.o \
		Reaction.o \
		read.o \
		ReadClass.o \
		readtr.o \
		runner.o \
		SelectedOutput.o \
		sit.o \
		smalldense.o \
		Solution.o \
		SolutionIsotope.o \
		SolutionIsotopeList.o \
		spread.o \
		SSassemblage.o \
		SSassemblageSS.o \
		step.o \
		StorageBin.o \
		StorageBinList.o \
		structures.o \
		sundialsmath.o \
		Surface.o \
		SurfaceCharge.o \
		SurfaceComp.o \
		System.o \
		tally.o \
		Temperature.o \
		tidy.o \
		transport.o \
		utilities.o \
		Utils.o \
		Var.o \

all: $(TARGET)

$(TARGET): $(OBJS)
	$(MKDIR) lib
	$(AR)  $(TARGET) $(OBJS)
	$(RANLIB) $(TARGET)	

clean:
	$(RM) $(OBJS)

R:
	$(MAKE) -f Makefile


# phreeqc (POBJS)


###########################################

# Fortran
IPhreeqcF.o: src/IPhreeqcF.F


sit.o: src/phreeqcpp/phreeqc/sit.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
isotopes.o: src/phreeqcpp/phreeqc/isotopes.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
inverse.o: src/phreeqcpp/phreeqc/inverse.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
read.o: src/phreeqcpp/phreeqc/read.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
advection.o: src/phreeqcpp/phreeqc/advection.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
pitzer.o: src/phreeqcpp/phreeqc/pitzer.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
spread.o: src/phreeqcpp/phreeqc/spread.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
nvector_serial.o: src/phreeqcpp/phreeqc/nvector_serial.c \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialsmath.h src/phreeqcpp/phreeqc/output.h
cl1.o: src/phreeqcpp/phreeqc/cl1.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h
basic.o: src/phreeqcpp/phreeqc/basic.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h src/phreeqcpp/phreeqc/p2c.h
mainsubs.o: src/phreeqcpp/phreeqc/mainsubs.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h src/phreeqcpp/phreeqc/input.h
cvode.o: src/phreeqcpp/phreeqc/cvode.c src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialsmath.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h
phqalloc.o: src/phreeqcpp/phreeqc/phqalloc.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/output.h
basicsubs.o: src/phreeqcpp/phreeqc/basicsubs.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
integrate.o: src/phreeqcpp/phreeqc/integrate.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
transport.o: src/phreeqcpp/phreeqc/transport.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
output.o: src/phreeqcpp/phreeqc/output.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/output.h src/phreeqcpp/phreeqc/phrqproto.h \
 src/phreeqcpp/phreeqc/phqalloc.h
phreeqc_files.o: src/phreeqcpp/phreeqc/phreeqc_files.c \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h src/phreeqcpp/phreeqc/input.h
tidy.o: src/phreeqcpp/phreeqc/tidy.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
utilities.o: src/phreeqcpp/phreeqc/utilities.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
p2clib.o: src/phreeqcpp/phreeqc/p2clib.c src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/output.h
step.o: src/phreeqcpp/phreeqc/step.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
cl1mp.o: src/phreeqcpp/phreeqc/cl1mp.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h
nvector.o: src/phreeqcpp/phreeqc/nvector.c \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/output.h
pitzer_structures.o: src/phreeqcpp/phreeqc/pitzer_structures.c \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
tally.o: src/phreeqcpp/phreeqc/tally.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
dense.o: src/phreeqcpp/phreeqc/dense.c \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialsmath.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h src/phreeqcpp/phreeqc/output.h
input.o: src/phreeqcpp/phreeqc/input.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h src/phreeqcpp/phreeqc/phqalloc.h
model.o: src/phreeqcpp/phreeqc/model.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
prep.o: src/phreeqcpp/phreeqc/prep.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
kinetics.o: src/phreeqcpp/phreeqc/kinetics.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
dw.o: src/phreeqcpp/phreeqc/dw.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phrqproto.h src/phreeqcpp/phreeqc/output.h
parse.o: src/phreeqcpp/phreeqc/parse.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
readtr.o: src/phreeqcpp/phreeqc/readtr.c \
 src/phreeqcpp/phreeqc/../StorageBin.h src/phreeqcpp/phreeqc/../System.h \
 src/phreeqcpp/phreeqc/../NameDouble.h \
 src/phreeqcpp/phreeqc/../Phreeqc_class.h \
 src/phreeqcpp/phreeqc/../Parser.h \
 src/phreeqcpp/phreeqc/../SSassemblageSS.h src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
structures.o: src/phreeqcpp/phreeqc/structures.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
main.o: src/phreeqcpp/phreeqc/main.c src/phreeqcpp/phreeqc/global.h \
 src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/output.h src/phreeqcpp/phreeqc/phrqproto.h \
 src/phreeqcpp/phreeqc/input.h
cvdense.o: src/phreeqcpp/phreeqc/cvdense.c \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h src/phreeqcpp/phreeqc/sundialsmath.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/nvector_serial.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/runner.h \
 src/phreeqcpp/StorageBinList.h src/phreeqcpp/dumper.h \
 src/phreeqcpp/phreeqc/p2c.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/basic.h src/phreeqcpp/phreeqc/pitzer_structures.h \
 src/phreeqcpp/phreeqc/pitzer.h src/phreeqcpp/phreeqc/input.h \
 src/phreeqcpp/phreeqc/output.h src/phreeqcpp/phreeqc/global.h \
 src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h
smalldense.o: src/phreeqcpp/phreeqc/smalldense.c \
 src/phreeqcpp/phreeqc/smalldense.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialsmath.h \
 src/phreeqcpp/phreeqc/output.h
sundialsmath.o: src/phreeqcpp/phreeqc/sundialsmath.c \
 src/phreeqcpp/phreeqc/sundialsmath.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/output.h
print.o: src/phreeqcpp/phreeqc/print.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h
SolutionIsotope.o: src/phreeqcpp/SolutionIsotope.cxx \
 src/phreeqcpp/Utils.h src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/SolutionIsotope.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/Phreeqc_class.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
PPassemblageComp.o: src/phreeqcpp/PPassemblageComp.cxx \
 src/phreeqcpp/Utils.h src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/PPassemblageComp.h src/phreeqcpp/NameDouble.h \
 src/phreeqcpp/Phreeqc_class.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/Dictionary.h src/phreeqcpp/Solution.h \
 src/phreeqcpp/NumKeyword.h src/phreeqcpp/SolutionIsotopeList.h \
 src/phreeqcpp/SolutionIsotope.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
KineticsComp.o: src/phreeqcpp/KineticsComp.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/KineticsComp.h src/phreeqcpp/NameDouble.h \
 src/phreeqcpp/Phreeqc_class.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/Dictionary.h src/phreeqcpp/Solution.h \
 src/phreeqcpp/NumKeyword.h src/phreeqcpp/SolutionIsotopeList.h \
 src/phreeqcpp/SolutionIsotope.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
Surface.o: src/phreeqcpp/Surface.cxx src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/Utils.h src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/Surface.h src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/SurfaceComp.h src/phreeqcpp/NameDouble.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/SurfaceCharge.h \
 src/phreeqcpp/cxxMix.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
Parser.o: src/phreeqcpp/Parser.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/Phreeqc_class.h
cxxKinetics.o: src/phreeqcpp/cxxKinetics.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/cxxKinetics.h src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/KineticsComp.h src/phreeqcpp/NameDouble.h \
 src/phreeqcpp/Phreeqc_class.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/cxxMix.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
StorageBin.o: src/phreeqcpp/StorageBin.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/NameDouble.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/StorageBin.h src/phreeqcpp/System.h \
 src/phreeqcpp/SSassemblage.h src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/Solution.h src/phreeqcpp/SolutionIsotopeList.h \
 src/phreeqcpp/SolutionIsotope.h src/phreeqcpp/Exchange.h \
 src/phreeqcpp/ExchComp.h src/phreeqcpp/GasPhase.h \
 src/phreeqcpp/cxxKinetics.h src/phreeqcpp/KineticsComp.h \
 src/phreeqcpp/PPassemblage.h src/phreeqcpp/PPassemblageComp.h \
 src/phreeqcpp/SSassemblageSS.h src/phreeqcpp/Surface.h \
 src/phreeqcpp/SurfaceComp.h src/phreeqcpp/SurfaceCharge.h \
 src/phreeqcpp/cxxMix.h src/phreeqcpp/Reaction.h \
 src/phreeqcpp/Temperature.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
Dictionary.o: src/phreeqcpp/Dictionary.cxx src/phreeqcpp/Dictionary.h \
 src/phreeqcpp/Solution.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/NumKeyword.h src/phreeqcpp/SolutionIsotopeList.h \
 src/phreeqcpp/SolutionIsotope.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/NameDouble.h src/phreeqcpp/phreeqc/global.h \
 src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/phrqproto.h \
 src/phreeqcpp/phreeqc/output.h
SurfaceComp.o: src/phreeqcpp/SurfaceComp.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/SurfaceComp.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/NameDouble.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/Dictionary.h src/phreeqcpp/Solution.h \
 src/phreeqcpp/NumKeyword.h src/phreeqcpp/SolutionIsotopeList.h \
 src/phreeqcpp/SolutionIsotope.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
NameDouble.o: src/phreeqcpp/NameDouble.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/NameDouble.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/Dictionary.h \
 src/phreeqcpp/Solution.h src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/SolutionIsotopeList.h src/phreeqcpp/SolutionIsotope.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/phrqproto.h
Exchange.o: src/phreeqcpp/Exchange.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/cxxMix.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/NumKeyword.h src/phreeqcpp/Exchange.h \
 src/phreeqcpp/ExchComp.h src/phreeqcpp/NameDouble.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
System.o: src/phreeqcpp/System.cxx src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/System.h src/phreeqcpp/NameDouble.h \
 src/phreeqcpp/Phreeqc_class.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/SSassemblage.h src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/Solution.h src/phreeqcpp/SolutionIsotopeList.h \
 src/phreeqcpp/SolutionIsotope.h src/phreeqcpp/Exchange.h \
 src/phreeqcpp/ExchComp.h src/phreeqcpp/GasPhase.h \
 src/phreeqcpp/cxxKinetics.h src/phreeqcpp/KineticsComp.h \
 src/phreeqcpp/PPassemblage.h src/phreeqcpp/PPassemblageComp.h \
 src/phreeqcpp/SSassemblageSS.h src/phreeqcpp/Surface.h \
 src/phreeqcpp/SurfaceComp.h src/phreeqcpp/SurfaceCharge.h \
 src/phreeqcpp/cxxMix.h src/phreeqcpp/Reaction.h \
 src/phreeqcpp/Temperature.h
ISolution.o: src/phreeqcpp/ISolution.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/ISolution.h src/phreeqcpp/ISolutionComp.h \
 src/phreeqcpp/Phreeqc_class.h src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/Solution.h src/phreeqcpp/SolutionIsotopeList.h \
 src/phreeqcpp/SolutionIsotope.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/NameDouble.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
class_main.o: src/phreeqcpp/class_main.cpp src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phrqproto.h
runner.o: src/phreeqcpp/runner.cpp src/phreeqcpp/runner.h \
 src/phreeqcpp/StorageBinList.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/Phreeqc_class.h
Temperature.o: src/phreeqcpp/Temperature.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/Temperature.h src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/phrqproto.h
ReadClass.o: src/phreeqcpp/ReadClass.cxx src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/Phreeqc_class.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/Solution.h src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/SolutionIsotopeList.h src/phreeqcpp/SolutionIsotope.h \
 src/phreeqcpp/NameDouble.h src/phreeqcpp/Exchange.h \
 src/phreeqcpp/ExchComp.h src/phreeqcpp/Surface.h \
 src/phreeqcpp/SurfaceComp.h src/phreeqcpp/SurfaceCharge.h \
 src/phreeqcpp/PPassemblage.h src/phreeqcpp/PPassemblageComp.h \
 src/phreeqcpp/cxxKinetics.h src/phreeqcpp/KineticsComp.h \
 src/phreeqcpp/SSassemblage.h src/phreeqcpp/GasPhase.h \
 src/phreeqcpp/Reaction.h src/phreeqcpp/cxxMix.h \
 src/phreeqcpp/Temperature.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
SurfaceCharge.o: src/phreeqcpp/SurfaceCharge.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/SurfaceCharge.h src/phreeqcpp/NameDouble.h \
 src/phreeqcpp/Phreeqc_class.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/Dictionary.h src/phreeqcpp/Solution.h \
 src/phreeqcpp/NumKeyword.h src/phreeqcpp/SolutionIsotopeList.h \
 src/phreeqcpp/SolutionIsotope.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
NumKeyword.o: src/phreeqcpp/NumKeyword.cxx src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/Phreeqc_class.h
ISolutionComp.o: src/phreeqcpp/ISolutionComp.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/ISolutionComp.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/phreeqc/phrqproto.h src/phreeqcpp/phreeqc/phqalloc.h
StorageBinList.o: src/phreeqcpp/StorageBinList.cpp \
 src/phreeqcpp/StorageBinList.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/Phreeqc_class.h
SolutionIsotopeList.o: src/phreeqcpp/SolutionIsotopeList.cxx \
 src/phreeqcpp/Utils.h src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/SolutionIsotopeList.h src/phreeqcpp/SolutionIsotope.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/phrqproto.h
cxxMix.o: src/phreeqcpp/cxxMix.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/cxxMix.h src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/phrqproto.h
Utils.o: src/phreeqcpp/Utils.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/phreeqc/output.h
SAXPhreeqc.o: src/phreeqcpp/SAXPhreeqc.cxx src/phreeqcpp/SAXPhreeqc.h \
 src/phreeqcpp/SaxPhreeqcHandlers.h src/phreeqcpp/phreeqc/global.h \
 src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/phrqproto.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/output.h
ExchComp.o: src/phreeqcpp/ExchComp.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/ExchComp.h src/phreeqcpp/NameDouble.h \
 src/phreeqcpp/Phreeqc_class.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/Dictionary.h src/phreeqcpp/Solution.h \
 src/phreeqcpp/NumKeyword.h src/phreeqcpp/SolutionIsotopeList.h \
 src/phreeqcpp/SolutionIsotope.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
SSassemblage.o: src/phreeqcpp/SSassemblage.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/SSassemblage.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/NumKeyword.h src/phreeqcpp/NameDouble.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/SSassemblageSS.h \
 src/phreeqcpp/cxxMix.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
Solution.o: src/phreeqcpp/Solution.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/Solution.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/NumKeyword.h src/phreeqcpp/SolutionIsotopeList.h \
 src/phreeqcpp/SolutionIsotope.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/NameDouble.h src/phreeqcpp/cxxMix.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/phrqproto.h
SSassemblageSS.o: src/phreeqcpp/SSassemblageSS.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/SSassemblageSS.h src/phreeqcpp/NameDouble.h \
 src/phreeqcpp/Phreeqc_class.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/Dictionary.h src/phreeqcpp/Solution.h \
 src/phreeqcpp/NumKeyword.h src/phreeqcpp/SolutionIsotopeList.h \
 src/phreeqcpp/SolutionIsotope.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
GasPhase.o: src/phreeqcpp/GasPhase.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/GasPhase.h src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/NameDouble.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/cxxMix.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/phrqproto.h
PPassemblage.o: src/phreeqcpp/PPassemblage.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/PPassemblage.h src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/PPassemblageComp.h src/phreeqcpp/NameDouble.h \
 src/phreeqcpp/Phreeqc_class.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/cxxMix.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
Reaction.o: src/phreeqcpp/Reaction.cxx src/phreeqcpp/Utils.h \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/Reaction.h src/phreeqcpp/NumKeyword.h \
 src/phreeqcpp/NameDouble.h src/phreeqcpp/Phreeqc_class.h \
 src/phreeqcpp/Parser.h src/phreeqcpp/phreeqc/phqalloc.h \
 src/phreeqcpp/phreeqc/phrqproto.h
Phreeqc.o: src/phreeqcpp/Phreeqc.cpp src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h
dumper.o: src/phreeqcpp/dumper.cpp src/phreeqcpp/dumper.h \
 src/phreeqcpp/StorageBinList.h src/phreeqcpp/Parser.h \
 src/phreeqcpp/Phreeqc_class.h
IPhreeqc.o: src/IPhreeqc.cpp \
 include/IPhreeqcCallbacks.h src/CVar.hxx src/Debug.h include/Var.h \
 include/IPhreeqc.hpp src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 include/IPhreeqcCallbacks.h src/SelectedOutput.hxx src/CVar.hxx \
 src/ErrorReporter.hxx
fwrap2.o: src/fwrap2.cpp include/Var.h src/fwrap.h
fwrap3.o: src/fwrap3.cpp include/Var.h src/fwrap.h
module_output.o: src/module_output.cpp src/module_files.h include/IPhreeqc.hpp \
 src/phreeqcpp/Phreeqc.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/cvdense.h src/phreeqcpp/phreeqc/cvode.h \
 src/phreeqcpp/phreeqc/nvector.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 include/IPhreeqcCallbacks.h include/Var.h src/SelectedOutput.hxx \
 src/CVar.hxx src/Debug.h src/phreeqcpp/phreeqc/output.c \
 src/phreeqcpp/phreeqc/output.h src/phreeqcpp/phreeqc/phrqproto.h \
 src/phreeqcpp/phreeqc/phqalloc.h
module_files.o: src/module_files.cpp src/module_files.h \
 src/phreeqcpp/phreeqc/phreeqc_files.c src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/sundialstypes.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/p2c.h \
 src/phreeqcpp/phreeqc/global_structures.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/pitzer_structures.h src/phreeqcpp/phreeqc/pitzer.h \
 src/phreeqcpp/phreeqc/input.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/global.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phqalloc.h src/phreeqcpp/phreeqc/output.h \
 src/phreeqcpp/phreeqc/phrqproto.h src/phreeqcpp/phreeqc/input.h \
 include/IPhreeqc.hpp include/IPhreeqcCallbacks.h include/Var.h \
 src/SelectedOutput.hxx src/CVar.hxx src/Debug.h
SelectedOutput.o: src/SelectedOutput.cpp src/phreeqcpp/phreeqc/phrqtype.h \
 src/phreeqcpp/phreeqc/p2c.h src/phreeqcpp/phreeqc/global_structures.h \
 src/phreeqcpp/phreeqc/phrqtype.h src/phreeqcpp/phreeqc/basic.h \
 src/phreeqcpp/phreeqc/basic.h src/phreeqcpp/Phreeqc.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/sundialstypes.h src/phreeqcpp/phreeqc/cvdense.h \
 src/phreeqcpp/phreeqc/cvode.h src/phreeqcpp/phreeqc/nvector.h \
 src/phreeqcpp/phreeqc/dense.h src/phreeqcpp/phreeqc/smalldense.h \
 src/phreeqcpp/phreeqc/nvector_serial.h src/phreeqcpp/phreeqc/dense.h \
 src/phreeqcpp/runner.h src/phreeqcpp/StorageBinList.h \
 src/phreeqcpp/dumper.h src/phreeqcpp/phreeqc/pitzer_structures.h \
 src/phreeqcpp/phreeqc/pitzer.h src/phreeqcpp/phreeqc/input.h \
 src/phreeqcpp/phreeqc/output.h src/phreeqcpp/phreeqc/global.h \
 src/phreeqcpp/phreeqc/global_structures.h src/SelectedOutput.hxx \
 src/CVar.hxx src/Debug.h include/Var.h
pp_sys.o: src/pp_sys.cpp
fwrap.o: src/fwrap.cpp src/phreeqcpp/phreeqc/phrqtype.h \
 include/IPhreeqcCallbacks.h src/CVar.hxx src/Debug.h include/Var.h \
 src/fwrap.h
Var.o: src/Var.c include/Var.h
