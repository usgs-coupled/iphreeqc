TARGET = IPhreeqc.chm

all: $(TARGET)

IPhreeqc.chm: IPhreeqc.hhp TOC.hhc html/index.html
	@hhc IPhreeqc.hhp || exit 0

view: $(TARGET)
	cygstart $(TARGET)

html/index.html : Doxyfile ../include/IPhreeqc.h ../include/Var.h ../include/IPhreeqc.hpp
	doxygen

clean:
	rm -rf html $(TARGET)
