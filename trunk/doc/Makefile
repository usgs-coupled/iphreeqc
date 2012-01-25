TARGET = IPhreeqc.chm

DOXYGEN = doxygen

HEADERS = \
	../include/IPhreeqc.h \
	../include/Var.h \
	../include/IPhreeqc.hpp

EXAMPLES = \
	examples/AccumulateLine.c \
	examples/CreateIPhreeqc.c \
	examples/GetComponent.c \
	examples/GetDumpString.c \
	examples/GetSelectedOutputValue.c \
	examples/F90ClearAccumulatedLines.f90 \
	examples/F90CreateIPhreeqc.f90 \
	examples/F90DestroyIPhreeqc.f90 \
	examples/F90GetComponent.f90 \
	examples/F90GetSelectedOutputValue.f90 \
	examples/F90GetDumpLine.f90 \
	examples/F90GetDumpStringLine.f90


all: $(TARGET)

IPhreeqc.chm: IPhreeqc.hhp TOC.hhc html/index.html
	@hhc IPhreeqc.hhp || exit 0

view: $(TARGET)
	cygstart $(TARGET)

html/index.html: Doxyfile $(HEADERS) $(EXAMPLES)
	$(DOXYGEN)

clean:
	rm -rf html $(TARGET)
