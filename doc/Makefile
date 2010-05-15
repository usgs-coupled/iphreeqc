TARGET = IPhreeqc.chm

HEADERS = \
	../include/IPhreeqc.h \
	../include/Var.h \
	../include/IPhreeqc.hpp

EXAMPLES = \
	examples/AccumulateLine.c \
	examples/CreateIPhreeqc.c \
	examples/GetDumpString.c \
	examples/F90GetDumpLine.f90
	

all: $(TARGET)

IPhreeqc.chm: IPhreeqc.hhp TOC.hhc html/index.html
	@hhc IPhreeqc.hhp || exit 0

view: $(TARGET)
	cygstart $(TARGET)

html/index.html: Doxyfile $(HEADERS) $(EXAMPLES)
	doxygen

clean:
	rm -rf html $(TARGET)
