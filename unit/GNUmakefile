CPPUNIT             = /z/parkplace/home/charlton
CC                  = g++
CFLAGS              = -Wall -O3
CPPFLAGS            = -DSWIG_SHARED_OBJ -I$(CPPUNIT)/include -I../include -I../src/phreeqcpp -I../src/phreeqcpp/phreeqc
TARGET_ARCH         =
FC                  = f95
FFLAGS              = -fno-second-underscore
CXX                 = g++
CXXFLAGS            = -Wall -DNDEBUG
RANLIB              = ranlib
RM                  = rm -f
AR                  = ar ruv
TARGET              = unit
LDFLAGS             = -L../lib -L$(CPPUNIT)/lib -ldl

VPATH=.:../src:../src/phreeqcpp/phreeqc

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
		TestInterface.o \
		TestSelectedOutput.o \
		TestVar.o \
		unit.o


SOBJS =	\
		IPhreeqc.o \
		SelectedOutput.o \
		Var.o \
		fwrap.o \
		global.o \
		module_files.o \
		module_output.o


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
		p2clib.o \
		parse.o \
		phqalloc.o \
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

$(TARGET): $(TOBJS) $(POBJS) $(SOBJS)
	$(CXX) $(LDFLAGS) -o unit $(TOBJS) $(POBJS) $(SOBJS) -lcppunit

clean:
	$(RM) $(TOBJS) $(POBJS) $(SOBJS) unit unit.exe *~


# TOBJS
TestCVar.o: TestCVar.cpp TestCVar.h ../src/CVar.hxx ../src/Debug.h \
  ../src/../include/Var.h
TestInterface.o: TestInterface.cpp TestInterface.h ../include/IPhreeqc.h \
  ../include/Var.h ../src/CVar.hxx ../src/Debug.h ../src/../include/Var.h \
  ../src/phreeqcns.hxx ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/phqalloc.h \
  ../src/phreeqcpp/phreeqc/input.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
TestSelectedOutput.o: TestSelectedOutput.cpp TestSelectedOutput.h \
  ../src/SelectedOutput.hxx ../src/CVar.hxx ../src/Debug.h \
  ../src/../include/Var.h ../src/phreeqcns.hxx \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/input.h \
  ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/phrqproto.h
TestVar.o: TestVar.cpp TestVar.h ../include/Var.h
unit.o: unit.cpp TestVar.h ../include/Var.h TestCVar.h ../src/CVar.hxx \
  ../src/Debug.h ../src/../include/Var.h TestSelectedOutput.h \
  ../src/SelectedOutput.hxx ../src/CVar.hxx TestInterface.h

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
IPhreeqc.o: ../src/IPhreeqc.cpp ../src/phreeqcns.hxx \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/input.h \
  ../src/phreeqcpp/phreeqc/output.h ../src/phreeqcpp/phreeqc/phrqproto.h \
  ../src/ErrorReporter.hxx ../src/SelectedOutput.hxx \
  ../src/CVar.hxx ../src/Debug.h ../src/../include/Var.h \
  ../src/../include/IPhreeqc.h ../src/../include/Var.h \
  ../src/module_files.h
SelectedOutput.o: ../src/SelectedOutput.cpp ../src/SelectedOutput.hxx \
  ../src/CVar.hxx ../src/Debug.h ../src/../include/Var.h \
  ../src/phreeqcns.hxx ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/phqalloc.h \
  ../src/phreeqcpp/phreeqc/input.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h
Var.o: ../src/Var.c ../src/../include/Var.h
fwrap.o: ../src/fwrap.c ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/../include/IPhreeqc.h \
  ../src/../include/Var.h
global.o: ../src/global.c ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/phqalloc.h
module_files.o: ../src/module_files.c ../src/module_files.h \
  ../src/phreeqcpp/phreeqc/phreeqc_files.c \
  ../src/phreeqcpp/phreeqc/global.h ../src/phreeqcpp/phreeqc/phrqtype.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h ../src/phreeqcpp/phreeqc/input.h
module_output.o: ../src/module_output.c ../src/module_files.h \
  ../src/phreeqcpp/phreeqc/output.c ../src/phreeqcpp/phreeqc/global.h \
  ../src/phreeqcpp/phreeqc/phrqtype.h ../src/phreeqcpp/phreeqc/output.h \
  ../src/phreeqcpp/phreeqc/phrqproto.h \
  ../src/phreeqcpp/phreeqc/phqalloc.h
