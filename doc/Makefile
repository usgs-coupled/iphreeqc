DOCS =	\
		html/IPhreeqc_8h-source.html \
		html/IPhreeqc_8h.html \
		html/Var_8h-source.html \
		html/Var_8h.html \
		html/annotated.html \
		html/doxygen.css \
		html/doxygen.png \
		html/files.html \
		html/functions.html \
		html/globals.html \
		html/index.html \
		html/structVAR.html

##TARGET = $(DOCS)
TARGET = html/index.html

all:$(TARGET)

$(TARGET): Doxyfile ../include/IPhreeqc.h ../include/Var.h
	doxygen

clean:
	rm -f $(DOCS)
