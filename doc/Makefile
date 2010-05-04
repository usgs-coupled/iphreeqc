TARGET = html/index.html

all:$(TARGET)

$(TARGET): Doxyfile ../include/IPhreeqc.h ../include/Var.h ../include/IPhreeqc.hpp
	doxygen

clean:
	rm -rf html
