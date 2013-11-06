TARGET = IPhreeqc.chm

DOXYGEN = doxygen

HEADERS = \
	../src/IPhreeqc.h \
	../src/Var.h \
	../src/IPhreeqc.hpp

EXAMPLES = \
	examples \
	examples/AccumulateLine.c \
	examples/CreateIPhreeqc.c \
	examples/F90ClearAccumulatedLines.f90 \
	examples/F90CreateIPhreeqc.f90 \
	examples/F90DestroyIPhreeqc.f90 \
	examples/F90GetComponent.f90 \
	examples/F90GetDumpStringLine.f90 \
	examples/F90GetErrorStringLine.f90 \
	examples/F90GetLogStringLine.f90 \
	examples/F90GetOutputStringLine.f90 \
	examples/F90GetSelectedOutputStringLine.f90 \
	examples/F90GetSelectedOutputValue.f90 \
	examples/F90GetVersionString.f90 \
	examples/F90SetBasicFortranCallback.f90 \
	examples/GetComponent.c \
	examples/GetDumpString.c \
	examples/GetErrorString.c \
	examples/GetLogString.c \
	examples/GetOutputString.c \
	examples/GetSelectedOutputString.c \
	examples/GetSelectedOutputValue.c \
	examples/GetSelectedOutputValue2.c \
	examples/GetVersionString.c \
	examples/ic \
	examples/IPhreeqc.cpp \
	examples/Multi.cpp \
	examples/phreeqc.dat \
	examples/SetBasicCallback.c


all: $(TARGET)

IPhreeqc.chm: IPhreeqc.hhp TOC.hhc html/index.html
	@hhc IPhreeqc.hhp || exit 0

view: $(TARGET)
	cygstart $(TARGET)

html/index.html: Doxyfile $(HEADERS) $(EXAMPLES)
	$(DOXYGEN)

clean:
	rm -rf html $(TARGET)
