# Compiler flags...
CPP_COMPILER = g++
C_COMPILER = gcc

# Include paths...
DebugDll_Include_Path=-I"src" -I"src/phreeqcpp" 
DebugDll_Include_Path=-I"src" -I"src/phreeqcpp" 
Debug_Include_Path=-I"src" -I"src/phreeqcpp" 
Debug_Include_Path=-I"src" -I"src/phreeqcpp" 
ReleaseDll_Include_Path=-I"src" -I"src/phreeqcpp" 
ReleaseDll_Include_Path=-I"src" -I"src/phreeqcpp" 
Release_Include_Path=-I"src" -I"src/phreeqcpp" 
Release_Include_Path=-I"src" -I"src/phreeqcpp" 

# Library paths...
DebugDll_Library_Path=
DebugDll_Library_Path=
Debug_Library_Path=
Debug_Library_Path=
ReleaseDll_Library_Path=
ReleaseDll_Library_Path=
Release_Library_Path=
Release_Library_Path=

# Additional libraries...
DebugDll_Libraries=
DebugDll_Libraries=
Debug_Libraries=
Debug_Libraries=
ReleaseDll_Libraries=
ReleaseDll_Libraries=
Release_Libraries=
Release_Libraries=

# Preprocessor definitions...
DebugDll_Preprocessor_Definitions=-D _DEBUG -D GCC_BUILD -D _LIB 
DebugDll_Preprocessor_Definitions=-D _DEBUG -D GCC_BUILD -D _LIB 
Debug_Preprocessor_Definitions=-D _DEBUG -D GCC_BUILD -D _LIB 
Debug_Preprocessor_Definitions=-D _DEBUG -D GCC_BUILD -D _LIB 
ReleaseDll_Preprocessor_Definitions=-D NDEBUG -D GCC_BUILD -D _LIB 
ReleaseDll_Preprocessor_Definitions=-D NDEBUG -D GCC_BUILD -D _LIB 
Release_Preprocessor_Definitions=-D NDEBUG -D GCC_BUILD -D _LIB 
Release_Preprocessor_Definitions=-D NDEBUG -D GCC_BUILD -D _LIB 

# Implicitly linked object files...
DebugDll_Implicitly_Linked_Objects=
DebugDll_Implicitly_Linked_Objects=
Debug_Implicitly_Linked_Objects=
Debug_Implicitly_Linked_Objects=
ReleaseDll_Implicitly_Linked_Objects=
ReleaseDll_Implicitly_Linked_Objects=
Release_Implicitly_Linked_Objects=
Release_Implicitly_Linked_Objects=

# Compiler flags...
DebugDll_Compiler_Flags=-O0 -g 
DebugDll_Compiler_Flags=-O0 -g 
Debug_Compiler_Flags=-O0 
Debug_Compiler_Flags=-O0 
ReleaseDll_Compiler_Flags=-O2 
ReleaseDll_Compiler_Flags=-O2 
Release_Compiler_Flags=-O2 
Release_Compiler_Flags=-O2 

# Builds all configurations for this project...
.PHONY: build_all_configurations
build_all_configurations: DebugDll DebugDll Debug Debug ReleaseDll ReleaseDll Release Release 

# Builds the DebugDll configuration...
.PHONY: DebugDll
DebugDll: create_folders gccDebugDll/src/fwrap.o gccDebugDll/src/fwrap2.o gccDebugDll/src/fwrap3.o gccDebugDll/src/fwrap4.o gccDebugDll/src/fwrap5.o gccDebugDll/src/fwrap6.o gccDebugDll/src/fwrap7.o gccDebugDll/src/fwrap8.o gccDebugDll/src/IPhreeqc.o gccDebugDll/src/IPhreeqcLib.o gccDebugDll/src/Var.o gccDebugDll/src/CSelectedOutput.o gccDebugDll/src/phreeqcpp/SelectedOutput.o gccDebugDll/src/phreeqcpp/UserPunch.o gccDebugDll/src/phreeqcpp/cxxKinetics.o gccDebugDll/src/phreeqcpp/cxxMix.o gccDebugDll/src/phreeqcpp/dumper.o gccDebugDll/src/phreeqcpp/Exchange.o gccDebugDll/src/phreeqcpp/ExchComp.o gccDebugDll/src/phreeqcpp/GasComp.o gccDebugDll/src/phreeqcpp/GasPhase.o gccDebugDll/src/phreeqcpp/ISolution.o gccDebugDll/src/phreeqcpp/ISolutionComp.o gccDebugDll/src/phreeqcpp/Keywords.o gccDebugDll/src/phreeqcpp/KineticsComp.o gccDebugDll/src/phreeqcpp/NameDouble.o gccDebugDll/src/phreeqcpp/NumKeyword.o gccDebugDll/src/phreeqcpp/Parser.o gccDebugDll/src/phreeqcpp/PBasic.o gccDebugDll/src/phreeqcpp/Phreeqc.o gccDebugDll/src/phreeqcpp/PHRQ_base.o gccDebugDll/src/phreeqcpp/PHRQ_io.o gccDebugDll/src/phreeqcpp/PPassemblage.o gccDebugDll/src/phreeqcpp/PPassemblageComp.o gccDebugDll/src/phreeqcpp/Pressure.o gccDebugDll/src/phreeqcpp/Reaction.o gccDebugDll/src/phreeqcpp/ReadClass.o gccDebugDll/src/phreeqcpp/runner.o gccDebugDll/src/phreeqcpp/Solution.o gccDebugDll/src/phreeqcpp/SolutionIsotope.o gccDebugDll/src/phreeqcpp/SS.o gccDebugDll/src/phreeqcpp/SSassemblage.o gccDebugDll/src/phreeqcpp/SScomp.o gccDebugDll/src/phreeqcpp/StorageBin.o gccDebugDll/src/phreeqcpp/StorageBinList.o gccDebugDll/src/phreeqcpp/Surface.o gccDebugDll/src/phreeqcpp/SurfaceCharge.o gccDebugDll/src/phreeqcpp/SurfaceComp.o gccDebugDll/src/phreeqcpp/System.o gccDebugDll/src/phreeqcpp/Temperature.o gccDebugDll/src/phreeqcpp/Use.o gccDebugDll/src/phreeqcpp/Utils.o gccDebugDll/src/phreeqcpp/advection.o gccDebugDll/src/phreeqcpp/basicsubs.o gccDebugDll/src/phreeqcpp/cl1.o gccDebugDll/src/phreeqcpp/cvdense.o gccDebugDll/src/phreeqcpp/cvode.o gccDebugDll/src/phreeqcpp/dense.o gccDebugDll/src/phreeqcpp/dw.o gccDebugDll/src/phreeqcpp/gases.o gccDebugDll/src/phreeqcpp/input.o gccDebugDll/src/phreeqcpp/integrate.o gccDebugDll/src/phreeqcpp/inverse.o gccDebugDll/src/phreeqcpp/isotopes.o gccDebugDll/src/phreeqcpp/kinetics.o gccDebugDll/src/phreeqcpp/mainsubs.o gccDebugDll/src/phreeqcpp/model.o gccDebugDll/src/phreeqcpp/nvector.o gccDebugDll/src/phreeqcpp/nvector_serial.o gccDebugDll/src/phreeqcpp/parse.o gccDebugDll/src/phreeqcpp/phqalloc.o gccDebugDll/src/phreeqcpp/PHRQ_io_output.o gccDebugDll/src/phreeqcpp/pitzer.o gccDebugDll/src/phreeqcpp/pitzer_structures.o gccDebugDll/src/phreeqcpp/prep.o gccDebugDll/src/phreeqcpp/print.o gccDebugDll/src/phreeqcpp/read.o gccDebugDll/src/phreeqcpp/readtr.o gccDebugDll/src/phreeqcpp/sit.o gccDebugDll/src/phreeqcpp/smalldense.o gccDebugDll/src/phreeqcpp/spread.o gccDebugDll/src/phreeqcpp/step.o gccDebugDll/src/phreeqcpp/structures.o gccDebugDll/src/phreeqcpp/sundialsmath.o gccDebugDll/src/phreeqcpp/tally.o gccDebugDll/src/phreeqcpp/tidy.o gccDebugDll/src/phreeqcpp/transport.o gccDebugDll/src/phreeqcpp/utilities.o 
	ar rcs ../../gccDebugDll/libIPhreeqc.a gccDebugDll/src/fwrap.o gccDebugDll/src/fwrap2.o gccDebugDll/src/fwrap3.o gccDebugDll/src/fwrap4.o gccDebugDll/src/fwrap5.o gccDebugDll/src/fwrap6.o gccDebugDll/src/fwrap7.o gccDebugDll/src/fwrap8.o gccDebugDll/src/IPhreeqc.o gccDebugDll/src/IPhreeqcLib.o gccDebugDll/src/Var.o gccDebugDll/src/CSelectedOutput.o gccDebugDll/src/phreeqcpp/SelectedOutput.o gccDebugDll/src/phreeqcpp/UserPunch.o gccDebugDll/src/phreeqcpp/cxxKinetics.o gccDebugDll/src/phreeqcpp/cxxMix.o gccDebugDll/src/phreeqcpp/dumper.o gccDebugDll/src/phreeqcpp/Exchange.o gccDebugDll/src/phreeqcpp/ExchComp.o gccDebugDll/src/phreeqcpp/GasComp.o gccDebugDll/src/phreeqcpp/GasPhase.o gccDebugDll/src/phreeqcpp/ISolution.o gccDebugDll/src/phreeqcpp/ISolutionComp.o gccDebugDll/src/phreeqcpp/Keywords.o gccDebugDll/src/phreeqcpp/KineticsComp.o gccDebugDll/src/phreeqcpp/NameDouble.o gccDebugDll/src/phreeqcpp/NumKeyword.o gccDebugDll/src/phreeqcpp/Parser.o gccDebugDll/src/phreeqcpp/PBasic.o gccDebugDll/src/phreeqcpp/Phreeqc.o gccDebugDll/src/phreeqcpp/PHRQ_base.o gccDebugDll/src/phreeqcpp/PHRQ_io.o gccDebugDll/src/phreeqcpp/PPassemblage.o gccDebugDll/src/phreeqcpp/PPassemblageComp.o gccDebugDll/src/phreeqcpp/Pressure.o gccDebugDll/src/phreeqcpp/Reaction.o gccDebugDll/src/phreeqcpp/ReadClass.o gccDebugDll/src/phreeqcpp/runner.o gccDebugDll/src/phreeqcpp/Solution.o gccDebugDll/src/phreeqcpp/SolutionIsotope.o gccDebugDll/src/phreeqcpp/SS.o gccDebugDll/src/phreeqcpp/SSassemblage.o gccDebugDll/src/phreeqcpp/SScomp.o gccDebugDll/src/phreeqcpp/StorageBin.o gccDebugDll/src/phreeqcpp/StorageBinList.o gccDebugDll/src/phreeqcpp/Surface.o gccDebugDll/src/phreeqcpp/SurfaceCharge.o gccDebugDll/src/phreeqcpp/SurfaceComp.o gccDebugDll/src/phreeqcpp/System.o gccDebugDll/src/phreeqcpp/Temperature.o gccDebugDll/src/phreeqcpp/Use.o gccDebugDll/src/phreeqcpp/Utils.o gccDebugDll/src/phreeqcpp/advection.o gccDebugDll/src/phreeqcpp/basicsubs.o gccDebugDll/src/phreeqcpp/cl1.o gccDebugDll/src/phreeqcpp/cvdense.o gccDebugDll/src/phreeqcpp/cvode.o gccDebugDll/src/phreeqcpp/dense.o gccDebugDll/src/phreeqcpp/dw.o gccDebugDll/src/phreeqcpp/gases.o gccDebugDll/src/phreeqcpp/input.o gccDebugDll/src/phreeqcpp/integrate.o gccDebugDll/src/phreeqcpp/inverse.o gccDebugDll/src/phreeqcpp/isotopes.o gccDebugDll/src/phreeqcpp/kinetics.o gccDebugDll/src/phreeqcpp/mainsubs.o gccDebugDll/src/phreeqcpp/model.o gccDebugDll/src/phreeqcpp/nvector.o gccDebugDll/src/phreeqcpp/nvector_serial.o gccDebugDll/src/phreeqcpp/parse.o gccDebugDll/src/phreeqcpp/phqalloc.o gccDebugDll/src/phreeqcpp/PHRQ_io_output.o gccDebugDll/src/phreeqcpp/pitzer.o gccDebugDll/src/phreeqcpp/pitzer_structures.o gccDebugDll/src/phreeqcpp/prep.o gccDebugDll/src/phreeqcpp/print.o gccDebugDll/src/phreeqcpp/read.o gccDebugDll/src/phreeqcpp/readtr.o gccDebugDll/src/phreeqcpp/sit.o gccDebugDll/src/phreeqcpp/smalldense.o gccDebugDll/src/phreeqcpp/spread.o gccDebugDll/src/phreeqcpp/step.o gccDebugDll/src/phreeqcpp/structures.o gccDebugDll/src/phreeqcpp/sundialsmath.o gccDebugDll/src/phreeqcpp/tally.o gccDebugDll/src/phreeqcpp/tidy.o gccDebugDll/src/phreeqcpp/transport.o gccDebugDll/src/phreeqcpp/utilities.o  $(DebugDll_Implicitly_Linked_Objects)

# Compiles file src/fwrap.cpp for the DebugDll configuration...
-include gccDebugDll/src/fwrap.d
gccDebugDll/src/fwrap.o: src/fwrap.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/fwrap.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap.cpp $(DebugDll_Include_Path) > gccDebugDll/src/fwrap.d

# Compiles file src/fwrap2.cpp for the DebugDll configuration...
-include gccDebugDll/src/fwrap2.d
gccDebugDll/src/fwrap2.o: src/fwrap2.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap2.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/fwrap2.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap2.cpp $(DebugDll_Include_Path) > gccDebugDll/src/fwrap2.d

# Compiles file src/fwrap3.cpp for the DebugDll configuration...
-include gccDebugDll/src/fwrap3.d
gccDebugDll/src/fwrap3.o: src/fwrap3.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap3.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/fwrap3.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap3.cpp $(DebugDll_Include_Path) > gccDebugDll/src/fwrap3.d

# Compiles file src/fwrap4.cpp for the DebugDll configuration...
-include gccDebugDll/src/fwrap4.d
gccDebugDll/src/fwrap4.o: src/fwrap4.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap4.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/fwrap4.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap4.cpp $(DebugDll_Include_Path) > gccDebugDll/src/fwrap4.d

# Compiles file src/fwrap5.cpp for the DebugDll configuration...
-include gccDebugDll/src/fwrap5.d
gccDebugDll/src/fwrap5.o: src/fwrap5.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap5.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/fwrap5.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap5.cpp $(DebugDll_Include_Path) > gccDebugDll/src/fwrap5.d

# Compiles file src/fwrap6.cpp for the DebugDll configuration...
-include gccDebugDll/src/fwrap6.d
gccDebugDll/src/fwrap6.o: src/fwrap6.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap6.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/fwrap6.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap6.cpp $(DebugDll_Include_Path) > gccDebugDll/src/fwrap6.d

# Compiles file src/fwrap7.cpp for the DebugDll configuration...
-include gccDebugDll/src/fwrap7.d
gccDebugDll/src/fwrap7.o: src/fwrap7.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap7.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/fwrap7.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap7.cpp $(DebugDll_Include_Path) > gccDebugDll/src/fwrap7.d

# Compiles file src/fwrap8.cpp for the DebugDll configuration...
-include gccDebugDll/src/fwrap8.d
gccDebugDll/src/fwrap8.o: src/fwrap8.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap8.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/fwrap8.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap8.cpp $(DebugDll_Include_Path) > gccDebugDll/src/fwrap8.d

# Compiles file src/IPhreeqc.cpp for the DebugDll configuration...
-include gccDebugDll/src/IPhreeqc.d
gccDebugDll/src/IPhreeqc.o: src/IPhreeqc.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/IPhreeqc.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/IPhreeqc.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/IPhreeqc.cpp $(DebugDll_Include_Path) > gccDebugDll/src/IPhreeqc.d

# Compiles file src/IPhreeqcLib.cpp for the DebugDll configuration...
-include gccDebugDll/src/IPhreeqcLib.d
gccDebugDll/src/IPhreeqcLib.o: src/IPhreeqcLib.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/IPhreeqcLib.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/IPhreeqcLib.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/IPhreeqcLib.cpp $(DebugDll_Include_Path) > gccDebugDll/src/IPhreeqcLib.d

# Compiles file src/Var.c for the DebugDll configuration...
-include gccDebugDll/src/Var.d
gccDebugDll/src/Var.o: src/Var.c
	$(C_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/Var.c $(DebugDll_Include_Path) -o gccDebugDll/src/Var.o
	$(C_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/Var.c $(DebugDll_Include_Path) > gccDebugDll/src/Var.d

# Compiles file src/CSelectedOutput.cpp for the DebugDll configuration...
-include gccDebugDll/src/CSelectedOutput.d
gccDebugDll/src/CSelectedOutput.o: src/CSelectedOutput.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/CSelectedOutput.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/CSelectedOutput.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/CSelectedOutput.cpp $(DebugDll_Include_Path) > gccDebugDll/src/CSelectedOutput.d

# Compiles file src/phreeqcpp/SelectedOutput.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/SelectedOutput.d
gccDebugDll/src/phreeqcpp/SelectedOutput.o: src/phreeqcpp/SelectedOutput.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SelectedOutput.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/SelectedOutput.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SelectedOutput.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/SelectedOutput.d

# Compiles file src/phreeqcpp/UserPunch.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/UserPunch.d
gccDebugDll/src/phreeqcpp/UserPunch.o: src/phreeqcpp/UserPunch.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/UserPunch.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/UserPunch.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/UserPunch.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/UserPunch.d

# Compiles file src/phreeqcpp/cxxKinetics.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/cxxKinetics.d
gccDebugDll/src/phreeqcpp/cxxKinetics.o: src/phreeqcpp/cxxKinetics.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/cxxKinetics.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/cxxKinetics.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/cxxKinetics.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/cxxKinetics.d

# Compiles file src/phreeqcpp/cxxMix.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/cxxMix.d
gccDebugDll/src/phreeqcpp/cxxMix.o: src/phreeqcpp/cxxMix.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/cxxMix.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/cxxMix.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/cxxMix.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/cxxMix.d

# Compiles file src/phreeqcpp/dumper.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/dumper.d
gccDebugDll/src/phreeqcpp/dumper.o: src/phreeqcpp/dumper.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/dumper.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/dumper.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/dumper.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/dumper.d

# Compiles file src/phreeqcpp/Exchange.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/Exchange.d
gccDebugDll/src/phreeqcpp/Exchange.o: src/phreeqcpp/Exchange.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Exchange.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/Exchange.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Exchange.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/Exchange.d

# Compiles file src/phreeqcpp/ExchComp.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/ExchComp.d
gccDebugDll/src/phreeqcpp/ExchComp.o: src/phreeqcpp/ExchComp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/ExchComp.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/ExchComp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/ExchComp.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/ExchComp.d

# Compiles file src/phreeqcpp/GasComp.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/GasComp.d
gccDebugDll/src/phreeqcpp/GasComp.o: src/phreeqcpp/GasComp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/GasComp.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/GasComp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/GasComp.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/GasComp.d

# Compiles file src/phreeqcpp/GasPhase.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/GasPhase.d
gccDebugDll/src/phreeqcpp/GasPhase.o: src/phreeqcpp/GasPhase.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/GasPhase.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/GasPhase.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/GasPhase.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/GasPhase.d

# Compiles file src/phreeqcpp/ISolution.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/ISolution.d
gccDebugDll/src/phreeqcpp/ISolution.o: src/phreeqcpp/ISolution.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/ISolution.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/ISolution.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/ISolution.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/ISolution.d

# Compiles file src/phreeqcpp/ISolutionComp.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/ISolutionComp.d
gccDebugDll/src/phreeqcpp/ISolutionComp.o: src/phreeqcpp/ISolutionComp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/ISolutionComp.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/ISolutionComp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/ISolutionComp.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/ISolutionComp.d

# Compiles file src/phreeqcpp/Keywords.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/Keywords.d
gccDebugDll/src/phreeqcpp/Keywords.o: src/phreeqcpp/Keywords.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Keywords.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/Keywords.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Keywords.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/Keywords.d

# Compiles file src/phreeqcpp/KineticsComp.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/KineticsComp.d
gccDebugDll/src/phreeqcpp/KineticsComp.o: src/phreeqcpp/KineticsComp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/KineticsComp.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/KineticsComp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/KineticsComp.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/KineticsComp.d

# Compiles file src/phreeqcpp/NameDouble.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/NameDouble.d
gccDebugDll/src/phreeqcpp/NameDouble.o: src/phreeqcpp/NameDouble.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/NameDouble.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/NameDouble.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/NameDouble.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/NameDouble.d

# Compiles file src/phreeqcpp/NumKeyword.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/NumKeyword.d
gccDebugDll/src/phreeqcpp/NumKeyword.o: src/phreeqcpp/NumKeyword.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/NumKeyword.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/NumKeyword.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/NumKeyword.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/NumKeyword.d

# Compiles file src/phreeqcpp/Parser.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/Parser.d
gccDebugDll/src/phreeqcpp/Parser.o: src/phreeqcpp/Parser.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Parser.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/Parser.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Parser.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/Parser.d

# Compiles file src/phreeqcpp/PBasic.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/PBasic.d
gccDebugDll/src/phreeqcpp/PBasic.o: src/phreeqcpp/PBasic.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/PBasic.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/PBasic.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/PBasic.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/PBasic.d

# Compiles file src/phreeqcpp/Phreeqc.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/Phreeqc.d
gccDebugDll/src/phreeqcpp/Phreeqc.o: src/phreeqcpp/Phreeqc.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Phreeqc.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/Phreeqc.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Phreeqc.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/Phreeqc.d

# Compiles file src/phreeqcpp/PHRQ_base.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/PHRQ_base.d
gccDebugDll/src/phreeqcpp/PHRQ_base.o: src/phreeqcpp/PHRQ_base.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/PHRQ_base.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/PHRQ_base.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/PHRQ_base.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/PHRQ_base.d

# Compiles file src/phreeqcpp/PHRQ_io.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/PHRQ_io.d
gccDebugDll/src/phreeqcpp/PHRQ_io.o: src/phreeqcpp/PHRQ_io.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/PHRQ_io.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/PHRQ_io.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/PHRQ_io.d

# Compiles file src/phreeqcpp/PPassemblage.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/PPassemblage.d
gccDebugDll/src/phreeqcpp/PPassemblage.o: src/phreeqcpp/PPassemblage.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/PPassemblage.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/PPassemblage.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/PPassemblage.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/PPassemblage.d

# Compiles file src/phreeqcpp/PPassemblageComp.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/PPassemblageComp.d
gccDebugDll/src/phreeqcpp/PPassemblageComp.o: src/phreeqcpp/PPassemblageComp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/PPassemblageComp.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/PPassemblageComp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/PPassemblageComp.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/PPassemblageComp.d

# Compiles file src/phreeqcpp/Pressure.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/Pressure.d
gccDebugDll/src/phreeqcpp/Pressure.o: src/phreeqcpp/Pressure.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Pressure.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/Pressure.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Pressure.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/Pressure.d

# Compiles file src/phreeqcpp/Reaction.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/Reaction.d
gccDebugDll/src/phreeqcpp/Reaction.o: src/phreeqcpp/Reaction.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Reaction.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/Reaction.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Reaction.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/Reaction.d

# Compiles file src/phreeqcpp/ReadClass.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/ReadClass.d
gccDebugDll/src/phreeqcpp/ReadClass.o: src/phreeqcpp/ReadClass.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/ReadClass.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/ReadClass.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/ReadClass.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/ReadClass.d

# Compiles file src/phreeqcpp/runner.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/runner.d
gccDebugDll/src/phreeqcpp/runner.o: src/phreeqcpp/runner.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/runner.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/runner.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/runner.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/runner.d

# Compiles file src/phreeqcpp/Solution.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/Solution.d
gccDebugDll/src/phreeqcpp/Solution.o: src/phreeqcpp/Solution.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Solution.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/Solution.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Solution.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/Solution.d

# Compiles file src/phreeqcpp/SolutionIsotope.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/SolutionIsotope.d
gccDebugDll/src/phreeqcpp/SolutionIsotope.o: src/phreeqcpp/SolutionIsotope.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SolutionIsotope.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/SolutionIsotope.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SolutionIsotope.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/SolutionIsotope.d

# Compiles file src/phreeqcpp/SS.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/SS.d
gccDebugDll/src/phreeqcpp/SS.o: src/phreeqcpp/SS.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SS.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/SS.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SS.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/SS.d

# Compiles file src/phreeqcpp/SSassemblage.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/SSassemblage.d
gccDebugDll/src/phreeqcpp/SSassemblage.o: src/phreeqcpp/SSassemblage.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SSassemblage.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/SSassemblage.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SSassemblage.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/SSassemblage.d

# Compiles file src/phreeqcpp/SScomp.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/SScomp.d
gccDebugDll/src/phreeqcpp/SScomp.o: src/phreeqcpp/SScomp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SScomp.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/SScomp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SScomp.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/SScomp.d

# Compiles file src/phreeqcpp/StorageBin.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/StorageBin.d
gccDebugDll/src/phreeqcpp/StorageBin.o: src/phreeqcpp/StorageBin.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/StorageBin.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/StorageBin.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/StorageBin.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/StorageBin.d

# Compiles file src/phreeqcpp/StorageBinList.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/StorageBinList.d
gccDebugDll/src/phreeqcpp/StorageBinList.o: src/phreeqcpp/StorageBinList.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/StorageBinList.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/StorageBinList.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/StorageBinList.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/StorageBinList.d

# Compiles file src/phreeqcpp/Surface.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/Surface.d
gccDebugDll/src/phreeqcpp/Surface.o: src/phreeqcpp/Surface.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Surface.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/Surface.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Surface.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/Surface.d

# Compiles file src/phreeqcpp/SurfaceCharge.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/SurfaceCharge.d
gccDebugDll/src/phreeqcpp/SurfaceCharge.o: src/phreeqcpp/SurfaceCharge.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SurfaceCharge.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/SurfaceCharge.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SurfaceCharge.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/SurfaceCharge.d

# Compiles file src/phreeqcpp/SurfaceComp.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/SurfaceComp.d
gccDebugDll/src/phreeqcpp/SurfaceComp.o: src/phreeqcpp/SurfaceComp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SurfaceComp.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/SurfaceComp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SurfaceComp.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/SurfaceComp.d

# Compiles file src/phreeqcpp/System.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/System.d
gccDebugDll/src/phreeqcpp/System.o: src/phreeqcpp/System.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/System.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/System.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/System.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/System.d

# Compiles file src/phreeqcpp/Temperature.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/Temperature.d
gccDebugDll/src/phreeqcpp/Temperature.o: src/phreeqcpp/Temperature.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Temperature.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/Temperature.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Temperature.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/Temperature.d

# Compiles file src/phreeqcpp/Use.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/Use.d
gccDebugDll/src/phreeqcpp/Use.o: src/phreeqcpp/Use.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Use.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/Use.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Use.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/Use.d

# Compiles file src/phreeqcpp/Utils.cxx for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/Utils.d
gccDebugDll/src/phreeqcpp/Utils.o: src/phreeqcpp/Utils.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Utils.cxx $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/Utils.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Utils.cxx $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/Utils.d

# Compiles file src/phreeqcpp/advection.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/advection.d
gccDebugDll/src/phreeqcpp/advection.o: src/phreeqcpp/advection.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/advection.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/advection.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/advection.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/advection.d

# Compiles file src/phreeqcpp/basicsubs.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/basicsubs.d
gccDebugDll/src/phreeqcpp/basicsubs.o: src/phreeqcpp/basicsubs.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/basicsubs.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/basicsubs.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/basicsubs.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/basicsubs.d

# Compiles file src/phreeqcpp/cl1.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/cl1.d
gccDebugDll/src/phreeqcpp/cl1.o: src/phreeqcpp/cl1.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/cl1.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/cl1.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/cl1.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/cl1.d

# Compiles file src/phreeqcpp/cvdense.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/cvdense.d
gccDebugDll/src/phreeqcpp/cvdense.o: src/phreeqcpp/cvdense.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/cvdense.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/cvdense.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/cvdense.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/cvdense.d

# Compiles file src/phreeqcpp/cvode.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/cvode.d
gccDebugDll/src/phreeqcpp/cvode.o: src/phreeqcpp/cvode.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/cvode.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/cvode.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/cvode.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/cvode.d

# Compiles file src/phreeqcpp/dense.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/dense.d
gccDebugDll/src/phreeqcpp/dense.o: src/phreeqcpp/dense.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/dense.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/dense.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/dense.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/dense.d

# Compiles file src/phreeqcpp/dw.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/dw.d
gccDebugDll/src/phreeqcpp/dw.o: src/phreeqcpp/dw.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/dw.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/dw.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/dw.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/dw.d

# Compiles file src/phreeqcpp/gases.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/gases.d
gccDebugDll/src/phreeqcpp/gases.o: src/phreeqcpp/gases.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/gases.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/gases.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/gases.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/gases.d

# Compiles file src/phreeqcpp/input.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/input.d
gccDebugDll/src/phreeqcpp/input.o: src/phreeqcpp/input.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/input.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/input.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/input.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/input.d

# Compiles file src/phreeqcpp/integrate.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/integrate.d
gccDebugDll/src/phreeqcpp/integrate.o: src/phreeqcpp/integrate.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/integrate.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/integrate.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/integrate.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/integrate.d

# Compiles file src/phreeqcpp/inverse.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/inverse.d
gccDebugDll/src/phreeqcpp/inverse.o: src/phreeqcpp/inverse.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/inverse.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/inverse.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/inverse.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/inverse.d

# Compiles file src/phreeqcpp/isotopes.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/isotopes.d
gccDebugDll/src/phreeqcpp/isotopes.o: src/phreeqcpp/isotopes.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/isotopes.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/isotopes.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/isotopes.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/isotopes.d

# Compiles file src/phreeqcpp/kinetics.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/kinetics.d
gccDebugDll/src/phreeqcpp/kinetics.o: src/phreeqcpp/kinetics.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/kinetics.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/kinetics.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/kinetics.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/kinetics.d

# Compiles file src/phreeqcpp/mainsubs.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/mainsubs.d
gccDebugDll/src/phreeqcpp/mainsubs.o: src/phreeqcpp/mainsubs.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/mainsubs.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/mainsubs.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/mainsubs.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/mainsubs.d

# Compiles file src/phreeqcpp/model.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/model.d
gccDebugDll/src/phreeqcpp/model.o: src/phreeqcpp/model.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/model.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/model.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/model.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/model.d

# Compiles file src/phreeqcpp/nvector.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/nvector.d
gccDebugDll/src/phreeqcpp/nvector.o: src/phreeqcpp/nvector.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/nvector.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/nvector.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/nvector.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/nvector.d

# Compiles file src/phreeqcpp/nvector_serial.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/nvector_serial.d
gccDebugDll/src/phreeqcpp/nvector_serial.o: src/phreeqcpp/nvector_serial.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/nvector_serial.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/nvector_serial.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/nvector_serial.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/nvector_serial.d

# Compiles file src/phreeqcpp/parse.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/parse.d
gccDebugDll/src/phreeqcpp/parse.o: src/phreeqcpp/parse.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/parse.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/parse.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/parse.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/parse.d

# Compiles file src/phreeqcpp/phqalloc.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/phqalloc.d
gccDebugDll/src/phreeqcpp/phqalloc.o: src/phreeqcpp/phqalloc.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/phqalloc.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/phqalloc.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/phqalloc.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/phqalloc.d

# Compiles file src/phreeqcpp/PHRQ_io_output.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/PHRQ_io_output.d
gccDebugDll/src/phreeqcpp/PHRQ_io_output.o: src/phreeqcpp/PHRQ_io_output.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/PHRQ_io_output.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/PHRQ_io_output.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io_output.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/PHRQ_io_output.d

# Compiles file src/phreeqcpp/pitzer.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/pitzer.d
gccDebugDll/src/phreeqcpp/pitzer.o: src/phreeqcpp/pitzer.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/pitzer.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/pitzer.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/pitzer.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/pitzer.d

# Compiles file src/phreeqcpp/pitzer_structures.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/pitzer_structures.d
gccDebugDll/src/phreeqcpp/pitzer_structures.o: src/phreeqcpp/pitzer_structures.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/pitzer_structures.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/pitzer_structures.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/pitzer_structures.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/pitzer_structures.d

# Compiles file src/phreeqcpp/prep.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/prep.d
gccDebugDll/src/phreeqcpp/prep.o: src/phreeqcpp/prep.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/prep.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/prep.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/prep.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/prep.d

# Compiles file src/phreeqcpp/print.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/print.d
gccDebugDll/src/phreeqcpp/print.o: src/phreeqcpp/print.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/print.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/print.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/print.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/print.d

# Compiles file src/phreeqcpp/read.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/read.d
gccDebugDll/src/phreeqcpp/read.o: src/phreeqcpp/read.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/read.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/read.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/read.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/read.d

# Compiles file src/phreeqcpp/readtr.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/readtr.d
gccDebugDll/src/phreeqcpp/readtr.o: src/phreeqcpp/readtr.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/readtr.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/readtr.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/readtr.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/readtr.d

# Compiles file src/phreeqcpp/sit.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/sit.d
gccDebugDll/src/phreeqcpp/sit.o: src/phreeqcpp/sit.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/sit.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/sit.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/sit.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/sit.d

# Compiles file src/phreeqcpp/smalldense.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/smalldense.d
gccDebugDll/src/phreeqcpp/smalldense.o: src/phreeqcpp/smalldense.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/smalldense.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/smalldense.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/smalldense.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/smalldense.d

# Compiles file src/phreeqcpp/spread.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/spread.d
gccDebugDll/src/phreeqcpp/spread.o: src/phreeqcpp/spread.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/spread.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/spread.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/spread.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/spread.d

# Compiles file src/phreeqcpp/step.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/step.d
gccDebugDll/src/phreeqcpp/step.o: src/phreeqcpp/step.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/step.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/step.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/step.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/step.d

# Compiles file src/phreeqcpp/structures.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/structures.d
gccDebugDll/src/phreeqcpp/structures.o: src/phreeqcpp/structures.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/structures.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/structures.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/structures.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/structures.d

# Compiles file src/phreeqcpp/sundialsmath.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/sundialsmath.d
gccDebugDll/src/phreeqcpp/sundialsmath.o: src/phreeqcpp/sundialsmath.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/sundialsmath.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/sundialsmath.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/sundialsmath.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/sundialsmath.d

# Compiles file src/phreeqcpp/tally.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/tally.d
gccDebugDll/src/phreeqcpp/tally.o: src/phreeqcpp/tally.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/tally.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/tally.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/tally.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/tally.d

# Compiles file src/phreeqcpp/tidy.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/tidy.d
gccDebugDll/src/phreeqcpp/tidy.o: src/phreeqcpp/tidy.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/tidy.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/tidy.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/tidy.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/tidy.d

# Compiles file src/phreeqcpp/transport.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/transport.d
gccDebugDll/src/phreeqcpp/transport.o: src/phreeqcpp/transport.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/transport.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/transport.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/transport.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/transport.d

# Compiles file src/phreeqcpp/utilities.cpp for the DebugDll configuration...
-include gccDebugDll/src/phreeqcpp/utilities.d
gccDebugDll/src/phreeqcpp/utilities.o: src/phreeqcpp/utilities.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/utilities.cpp $(DebugDll_Include_Path) -o gccDebugDll/src/phreeqcpp/utilities.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/utilities.cpp $(DebugDll_Include_Path) > gccDebugDll/src/phreeqcpp/utilities.d

# Builds the DebugDll configuration...
.PHONY: DebugDll
DebugDll: create_folders x64/gccDebugDll/src/fwrap.o x64/gccDebugDll/src/fwrap2.o x64/gccDebugDll/src/fwrap3.o x64/gccDebugDll/src/fwrap4.o x64/gccDebugDll/src/fwrap5.o x64/gccDebugDll/src/fwrap6.o x64/gccDebugDll/src/fwrap7.o x64/gccDebugDll/src/fwrap8.o x64/gccDebugDll/src/IPhreeqc.o x64/gccDebugDll/src/IPhreeqcLib.o x64/gccDebugDll/src/Var.o x64/gccDebugDll/src/CSelectedOutput.o x64/gccDebugDll/src/phreeqcpp/SelectedOutput.o x64/gccDebugDll/src/phreeqcpp/UserPunch.o x64/gccDebugDll/src/phreeqcpp/cxxKinetics.o x64/gccDebugDll/src/phreeqcpp/cxxMix.o x64/gccDebugDll/src/phreeqcpp/dumper.o x64/gccDebugDll/src/phreeqcpp/Exchange.o x64/gccDebugDll/src/phreeqcpp/ExchComp.o x64/gccDebugDll/src/phreeqcpp/GasComp.o x64/gccDebugDll/src/phreeqcpp/GasPhase.o x64/gccDebugDll/src/phreeqcpp/ISolution.o x64/gccDebugDll/src/phreeqcpp/ISolutionComp.o x64/gccDebugDll/src/phreeqcpp/Keywords.o x64/gccDebugDll/src/phreeqcpp/KineticsComp.o x64/gccDebugDll/src/phreeqcpp/NameDouble.o x64/gccDebugDll/src/phreeqcpp/NumKeyword.o x64/gccDebugDll/src/phreeqcpp/Parser.o x64/gccDebugDll/src/phreeqcpp/PBasic.o x64/gccDebugDll/src/phreeqcpp/Phreeqc.o x64/gccDebugDll/src/phreeqcpp/PHRQ_base.o x64/gccDebugDll/src/phreeqcpp/PHRQ_io.o x64/gccDebugDll/src/phreeqcpp/PPassemblage.o x64/gccDebugDll/src/phreeqcpp/PPassemblageComp.o x64/gccDebugDll/src/phreeqcpp/Pressure.o x64/gccDebugDll/src/phreeqcpp/Reaction.o x64/gccDebugDll/src/phreeqcpp/ReadClass.o x64/gccDebugDll/src/phreeqcpp/runner.o x64/gccDebugDll/src/phreeqcpp/Solution.o x64/gccDebugDll/src/phreeqcpp/SolutionIsotope.o x64/gccDebugDll/src/phreeqcpp/SS.o x64/gccDebugDll/src/phreeqcpp/SSassemblage.o x64/gccDebugDll/src/phreeqcpp/SScomp.o x64/gccDebugDll/src/phreeqcpp/StorageBin.o x64/gccDebugDll/src/phreeqcpp/StorageBinList.o x64/gccDebugDll/src/phreeqcpp/Surface.o x64/gccDebugDll/src/phreeqcpp/SurfaceCharge.o x64/gccDebugDll/src/phreeqcpp/SurfaceComp.o x64/gccDebugDll/src/phreeqcpp/System.o x64/gccDebugDll/src/phreeqcpp/Temperature.o x64/gccDebugDll/src/phreeqcpp/Use.o x64/gccDebugDll/src/phreeqcpp/Utils.o x64/gccDebugDll/src/phreeqcpp/advection.o x64/gccDebugDll/src/phreeqcpp/basicsubs.o x64/gccDebugDll/src/phreeqcpp/cl1.o x64/gccDebugDll/src/phreeqcpp/cvdense.o x64/gccDebugDll/src/phreeqcpp/cvode.o x64/gccDebugDll/src/phreeqcpp/dense.o x64/gccDebugDll/src/phreeqcpp/dw.o x64/gccDebugDll/src/phreeqcpp/gases.o x64/gccDebugDll/src/phreeqcpp/input.o x64/gccDebugDll/src/phreeqcpp/integrate.o x64/gccDebugDll/src/phreeqcpp/inverse.o x64/gccDebugDll/src/phreeqcpp/isotopes.o x64/gccDebugDll/src/phreeqcpp/kinetics.o x64/gccDebugDll/src/phreeqcpp/mainsubs.o x64/gccDebugDll/src/phreeqcpp/model.o x64/gccDebugDll/src/phreeqcpp/nvector.o x64/gccDebugDll/src/phreeqcpp/nvector_serial.o x64/gccDebugDll/src/phreeqcpp/parse.o x64/gccDebugDll/src/phreeqcpp/phqalloc.o x64/gccDebugDll/src/phreeqcpp/PHRQ_io_output.o x64/gccDebugDll/src/phreeqcpp/pitzer.o x64/gccDebugDll/src/phreeqcpp/pitzer_structures.o x64/gccDebugDll/src/phreeqcpp/prep.o x64/gccDebugDll/src/phreeqcpp/print.o x64/gccDebugDll/src/phreeqcpp/read.o x64/gccDebugDll/src/phreeqcpp/readtr.o x64/gccDebugDll/src/phreeqcpp/sit.o x64/gccDebugDll/src/phreeqcpp/smalldense.o x64/gccDebugDll/src/phreeqcpp/spread.o x64/gccDebugDll/src/phreeqcpp/step.o x64/gccDebugDll/src/phreeqcpp/structures.o x64/gccDebugDll/src/phreeqcpp/sundialsmath.o x64/gccDebugDll/src/phreeqcpp/tally.o x64/gccDebugDll/src/phreeqcpp/tidy.o x64/gccDebugDll/src/phreeqcpp/transport.o x64/gccDebugDll/src/phreeqcpp/utilities.o 
	ar rcs ../../x64/gccDebugDll/libIPhreeqc.a x64/gccDebugDll/src/fwrap.o x64/gccDebugDll/src/fwrap2.o x64/gccDebugDll/src/fwrap3.o x64/gccDebugDll/src/fwrap4.o x64/gccDebugDll/src/fwrap5.o x64/gccDebugDll/src/fwrap6.o x64/gccDebugDll/src/fwrap7.o x64/gccDebugDll/src/fwrap8.o x64/gccDebugDll/src/IPhreeqc.o x64/gccDebugDll/src/IPhreeqcLib.o x64/gccDebugDll/src/Var.o x64/gccDebugDll/src/CSelectedOutput.o x64/gccDebugDll/src/phreeqcpp/SelectedOutput.o x64/gccDebugDll/src/phreeqcpp/UserPunch.o x64/gccDebugDll/src/phreeqcpp/cxxKinetics.o x64/gccDebugDll/src/phreeqcpp/cxxMix.o x64/gccDebugDll/src/phreeqcpp/dumper.o x64/gccDebugDll/src/phreeqcpp/Exchange.o x64/gccDebugDll/src/phreeqcpp/ExchComp.o x64/gccDebugDll/src/phreeqcpp/GasComp.o x64/gccDebugDll/src/phreeqcpp/GasPhase.o x64/gccDebugDll/src/phreeqcpp/ISolution.o x64/gccDebugDll/src/phreeqcpp/ISolutionComp.o x64/gccDebugDll/src/phreeqcpp/Keywords.o x64/gccDebugDll/src/phreeqcpp/KineticsComp.o x64/gccDebugDll/src/phreeqcpp/NameDouble.o x64/gccDebugDll/src/phreeqcpp/NumKeyword.o x64/gccDebugDll/src/phreeqcpp/Parser.o x64/gccDebugDll/src/phreeqcpp/PBasic.o x64/gccDebugDll/src/phreeqcpp/Phreeqc.o x64/gccDebugDll/src/phreeqcpp/PHRQ_base.o x64/gccDebugDll/src/phreeqcpp/PHRQ_io.o x64/gccDebugDll/src/phreeqcpp/PPassemblage.o x64/gccDebugDll/src/phreeqcpp/PPassemblageComp.o x64/gccDebugDll/src/phreeqcpp/Pressure.o x64/gccDebugDll/src/phreeqcpp/Reaction.o x64/gccDebugDll/src/phreeqcpp/ReadClass.o x64/gccDebugDll/src/phreeqcpp/runner.o x64/gccDebugDll/src/phreeqcpp/Solution.o x64/gccDebugDll/src/phreeqcpp/SolutionIsotope.o x64/gccDebugDll/src/phreeqcpp/SS.o x64/gccDebugDll/src/phreeqcpp/SSassemblage.o x64/gccDebugDll/src/phreeqcpp/SScomp.o x64/gccDebugDll/src/phreeqcpp/StorageBin.o x64/gccDebugDll/src/phreeqcpp/StorageBinList.o x64/gccDebugDll/src/phreeqcpp/Surface.o x64/gccDebugDll/src/phreeqcpp/SurfaceCharge.o x64/gccDebugDll/src/phreeqcpp/SurfaceComp.o x64/gccDebugDll/src/phreeqcpp/System.o x64/gccDebugDll/src/phreeqcpp/Temperature.o x64/gccDebugDll/src/phreeqcpp/Use.o x64/gccDebugDll/src/phreeqcpp/Utils.o x64/gccDebugDll/src/phreeqcpp/advection.o x64/gccDebugDll/src/phreeqcpp/basicsubs.o x64/gccDebugDll/src/phreeqcpp/cl1.o x64/gccDebugDll/src/phreeqcpp/cvdense.o x64/gccDebugDll/src/phreeqcpp/cvode.o x64/gccDebugDll/src/phreeqcpp/dense.o x64/gccDebugDll/src/phreeqcpp/dw.o x64/gccDebugDll/src/phreeqcpp/gases.o x64/gccDebugDll/src/phreeqcpp/input.o x64/gccDebugDll/src/phreeqcpp/integrate.o x64/gccDebugDll/src/phreeqcpp/inverse.o x64/gccDebugDll/src/phreeqcpp/isotopes.o x64/gccDebugDll/src/phreeqcpp/kinetics.o x64/gccDebugDll/src/phreeqcpp/mainsubs.o x64/gccDebugDll/src/phreeqcpp/model.o x64/gccDebugDll/src/phreeqcpp/nvector.o x64/gccDebugDll/src/phreeqcpp/nvector_serial.o x64/gccDebugDll/src/phreeqcpp/parse.o x64/gccDebugDll/src/phreeqcpp/phqalloc.o x64/gccDebugDll/src/phreeqcpp/PHRQ_io_output.o x64/gccDebugDll/src/phreeqcpp/pitzer.o x64/gccDebugDll/src/phreeqcpp/pitzer_structures.o x64/gccDebugDll/src/phreeqcpp/prep.o x64/gccDebugDll/src/phreeqcpp/print.o x64/gccDebugDll/src/phreeqcpp/read.o x64/gccDebugDll/src/phreeqcpp/readtr.o x64/gccDebugDll/src/phreeqcpp/sit.o x64/gccDebugDll/src/phreeqcpp/smalldense.o x64/gccDebugDll/src/phreeqcpp/spread.o x64/gccDebugDll/src/phreeqcpp/step.o x64/gccDebugDll/src/phreeqcpp/structures.o x64/gccDebugDll/src/phreeqcpp/sundialsmath.o x64/gccDebugDll/src/phreeqcpp/tally.o x64/gccDebugDll/src/phreeqcpp/tidy.o x64/gccDebugDll/src/phreeqcpp/transport.o x64/gccDebugDll/src/phreeqcpp/utilities.o  $(DebugDll_Implicitly_Linked_Objects)

# Compiles file src/fwrap.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/fwrap.d
x64/gccDebugDll/src/fwrap.o: src/fwrap.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/fwrap.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/fwrap.d

# Compiles file src/fwrap2.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/fwrap2.d
x64/gccDebugDll/src/fwrap2.o: src/fwrap2.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap2.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/fwrap2.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap2.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/fwrap2.d

# Compiles file src/fwrap3.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/fwrap3.d
x64/gccDebugDll/src/fwrap3.o: src/fwrap3.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap3.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/fwrap3.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap3.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/fwrap3.d

# Compiles file src/fwrap4.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/fwrap4.d
x64/gccDebugDll/src/fwrap4.o: src/fwrap4.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap4.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/fwrap4.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap4.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/fwrap4.d

# Compiles file src/fwrap5.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/fwrap5.d
x64/gccDebugDll/src/fwrap5.o: src/fwrap5.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap5.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/fwrap5.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap5.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/fwrap5.d

# Compiles file src/fwrap6.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/fwrap6.d
x64/gccDebugDll/src/fwrap6.o: src/fwrap6.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap6.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/fwrap6.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap6.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/fwrap6.d

# Compiles file src/fwrap7.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/fwrap7.d
x64/gccDebugDll/src/fwrap7.o: src/fwrap7.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap7.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/fwrap7.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap7.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/fwrap7.d

# Compiles file src/fwrap8.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/fwrap8.d
x64/gccDebugDll/src/fwrap8.o: src/fwrap8.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/fwrap8.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/fwrap8.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/fwrap8.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/fwrap8.d

# Compiles file src/IPhreeqc.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/IPhreeqc.d
x64/gccDebugDll/src/IPhreeqc.o: src/IPhreeqc.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/IPhreeqc.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/IPhreeqc.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/IPhreeqc.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/IPhreeqc.d

# Compiles file src/IPhreeqcLib.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/IPhreeqcLib.d
x64/gccDebugDll/src/IPhreeqcLib.o: src/IPhreeqcLib.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/IPhreeqcLib.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/IPhreeqcLib.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/IPhreeqcLib.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/IPhreeqcLib.d

# Compiles file src/Var.c for the DebugDll configuration...
-include x64/gccDebugDll/src/Var.d
x64/gccDebugDll/src/Var.o: src/Var.c
	$(C_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/Var.c $(DebugDll_Include_Path) -o x64/gccDebugDll/src/Var.o
	$(C_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/Var.c $(DebugDll_Include_Path) > x64/gccDebugDll/src/Var.d

# Compiles file src/CSelectedOutput.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/CSelectedOutput.d
x64/gccDebugDll/src/CSelectedOutput.o: src/CSelectedOutput.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/CSelectedOutput.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/CSelectedOutput.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/CSelectedOutput.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/CSelectedOutput.d

# Compiles file src/phreeqcpp/SelectedOutput.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/SelectedOutput.d
x64/gccDebugDll/src/phreeqcpp/SelectedOutput.o: src/phreeqcpp/SelectedOutput.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SelectedOutput.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/SelectedOutput.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SelectedOutput.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/SelectedOutput.d

# Compiles file src/phreeqcpp/UserPunch.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/UserPunch.d
x64/gccDebugDll/src/phreeqcpp/UserPunch.o: src/phreeqcpp/UserPunch.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/UserPunch.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/UserPunch.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/UserPunch.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/UserPunch.d

# Compiles file src/phreeqcpp/cxxKinetics.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/cxxKinetics.d
x64/gccDebugDll/src/phreeqcpp/cxxKinetics.o: src/phreeqcpp/cxxKinetics.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/cxxKinetics.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/cxxKinetics.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/cxxKinetics.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/cxxKinetics.d

# Compiles file src/phreeqcpp/cxxMix.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/cxxMix.d
x64/gccDebugDll/src/phreeqcpp/cxxMix.o: src/phreeqcpp/cxxMix.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/cxxMix.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/cxxMix.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/cxxMix.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/cxxMix.d

# Compiles file src/phreeqcpp/dumper.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/dumper.d
x64/gccDebugDll/src/phreeqcpp/dumper.o: src/phreeqcpp/dumper.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/dumper.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/dumper.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/dumper.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/dumper.d

# Compiles file src/phreeqcpp/Exchange.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/Exchange.d
x64/gccDebugDll/src/phreeqcpp/Exchange.o: src/phreeqcpp/Exchange.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Exchange.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/Exchange.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Exchange.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/Exchange.d

# Compiles file src/phreeqcpp/ExchComp.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/ExchComp.d
x64/gccDebugDll/src/phreeqcpp/ExchComp.o: src/phreeqcpp/ExchComp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/ExchComp.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/ExchComp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/ExchComp.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/ExchComp.d

# Compiles file src/phreeqcpp/GasComp.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/GasComp.d
x64/gccDebugDll/src/phreeqcpp/GasComp.o: src/phreeqcpp/GasComp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/GasComp.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/GasComp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/GasComp.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/GasComp.d

# Compiles file src/phreeqcpp/GasPhase.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/GasPhase.d
x64/gccDebugDll/src/phreeqcpp/GasPhase.o: src/phreeqcpp/GasPhase.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/GasPhase.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/GasPhase.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/GasPhase.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/GasPhase.d

# Compiles file src/phreeqcpp/ISolution.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/ISolution.d
x64/gccDebugDll/src/phreeqcpp/ISolution.o: src/phreeqcpp/ISolution.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/ISolution.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/ISolution.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/ISolution.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/ISolution.d

# Compiles file src/phreeqcpp/ISolutionComp.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/ISolutionComp.d
x64/gccDebugDll/src/phreeqcpp/ISolutionComp.o: src/phreeqcpp/ISolutionComp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/ISolutionComp.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/ISolutionComp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/ISolutionComp.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/ISolutionComp.d

# Compiles file src/phreeqcpp/Keywords.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/Keywords.d
x64/gccDebugDll/src/phreeqcpp/Keywords.o: src/phreeqcpp/Keywords.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Keywords.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/Keywords.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Keywords.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/Keywords.d

# Compiles file src/phreeqcpp/KineticsComp.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/KineticsComp.d
x64/gccDebugDll/src/phreeqcpp/KineticsComp.o: src/phreeqcpp/KineticsComp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/KineticsComp.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/KineticsComp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/KineticsComp.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/KineticsComp.d

# Compiles file src/phreeqcpp/NameDouble.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/NameDouble.d
x64/gccDebugDll/src/phreeqcpp/NameDouble.o: src/phreeqcpp/NameDouble.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/NameDouble.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/NameDouble.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/NameDouble.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/NameDouble.d

# Compiles file src/phreeqcpp/NumKeyword.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/NumKeyword.d
x64/gccDebugDll/src/phreeqcpp/NumKeyword.o: src/phreeqcpp/NumKeyword.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/NumKeyword.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/NumKeyword.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/NumKeyword.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/NumKeyword.d

# Compiles file src/phreeqcpp/Parser.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/Parser.d
x64/gccDebugDll/src/phreeqcpp/Parser.o: src/phreeqcpp/Parser.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Parser.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/Parser.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Parser.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/Parser.d

# Compiles file src/phreeqcpp/PBasic.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/PBasic.d
x64/gccDebugDll/src/phreeqcpp/PBasic.o: src/phreeqcpp/PBasic.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/PBasic.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/PBasic.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/PBasic.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/PBasic.d

# Compiles file src/phreeqcpp/Phreeqc.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/Phreeqc.d
x64/gccDebugDll/src/phreeqcpp/Phreeqc.o: src/phreeqcpp/Phreeqc.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Phreeqc.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/Phreeqc.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Phreeqc.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/Phreeqc.d

# Compiles file src/phreeqcpp/PHRQ_base.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/PHRQ_base.d
x64/gccDebugDll/src/phreeqcpp/PHRQ_base.o: src/phreeqcpp/PHRQ_base.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/PHRQ_base.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/PHRQ_base.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/PHRQ_base.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/PHRQ_base.d

# Compiles file src/phreeqcpp/PHRQ_io.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/PHRQ_io.d
x64/gccDebugDll/src/phreeqcpp/PHRQ_io.o: src/phreeqcpp/PHRQ_io.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/PHRQ_io.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/PHRQ_io.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/PHRQ_io.d

# Compiles file src/phreeqcpp/PPassemblage.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/PPassemblage.d
x64/gccDebugDll/src/phreeqcpp/PPassemblage.o: src/phreeqcpp/PPassemblage.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/PPassemblage.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/PPassemblage.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/PPassemblage.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/PPassemblage.d

# Compiles file src/phreeqcpp/PPassemblageComp.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/PPassemblageComp.d
x64/gccDebugDll/src/phreeqcpp/PPassemblageComp.o: src/phreeqcpp/PPassemblageComp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/PPassemblageComp.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/PPassemblageComp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/PPassemblageComp.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/PPassemblageComp.d

# Compiles file src/phreeqcpp/Pressure.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/Pressure.d
x64/gccDebugDll/src/phreeqcpp/Pressure.o: src/phreeqcpp/Pressure.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Pressure.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/Pressure.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Pressure.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/Pressure.d

# Compiles file src/phreeqcpp/Reaction.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/Reaction.d
x64/gccDebugDll/src/phreeqcpp/Reaction.o: src/phreeqcpp/Reaction.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Reaction.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/Reaction.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Reaction.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/Reaction.d

# Compiles file src/phreeqcpp/ReadClass.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/ReadClass.d
x64/gccDebugDll/src/phreeqcpp/ReadClass.o: src/phreeqcpp/ReadClass.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/ReadClass.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/ReadClass.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/ReadClass.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/ReadClass.d

# Compiles file src/phreeqcpp/runner.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/runner.d
x64/gccDebugDll/src/phreeqcpp/runner.o: src/phreeqcpp/runner.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/runner.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/runner.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/runner.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/runner.d

# Compiles file src/phreeqcpp/Solution.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/Solution.d
x64/gccDebugDll/src/phreeqcpp/Solution.o: src/phreeqcpp/Solution.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Solution.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/Solution.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Solution.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/Solution.d

# Compiles file src/phreeqcpp/SolutionIsotope.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/SolutionIsotope.d
x64/gccDebugDll/src/phreeqcpp/SolutionIsotope.o: src/phreeqcpp/SolutionIsotope.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SolutionIsotope.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/SolutionIsotope.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SolutionIsotope.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/SolutionIsotope.d

# Compiles file src/phreeqcpp/SS.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/SS.d
x64/gccDebugDll/src/phreeqcpp/SS.o: src/phreeqcpp/SS.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SS.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/SS.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SS.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/SS.d

# Compiles file src/phreeqcpp/SSassemblage.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/SSassemblage.d
x64/gccDebugDll/src/phreeqcpp/SSassemblage.o: src/phreeqcpp/SSassemblage.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SSassemblage.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/SSassemblage.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SSassemblage.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/SSassemblage.d

# Compiles file src/phreeqcpp/SScomp.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/SScomp.d
x64/gccDebugDll/src/phreeqcpp/SScomp.o: src/phreeqcpp/SScomp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SScomp.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/SScomp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SScomp.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/SScomp.d

# Compiles file src/phreeqcpp/StorageBin.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/StorageBin.d
x64/gccDebugDll/src/phreeqcpp/StorageBin.o: src/phreeqcpp/StorageBin.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/StorageBin.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/StorageBin.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/StorageBin.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/StorageBin.d

# Compiles file src/phreeqcpp/StorageBinList.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/StorageBinList.d
x64/gccDebugDll/src/phreeqcpp/StorageBinList.o: src/phreeqcpp/StorageBinList.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/StorageBinList.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/StorageBinList.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/StorageBinList.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/StorageBinList.d

# Compiles file src/phreeqcpp/Surface.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/Surface.d
x64/gccDebugDll/src/phreeqcpp/Surface.o: src/phreeqcpp/Surface.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Surface.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/Surface.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Surface.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/Surface.d

# Compiles file src/phreeqcpp/SurfaceCharge.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/SurfaceCharge.d
x64/gccDebugDll/src/phreeqcpp/SurfaceCharge.o: src/phreeqcpp/SurfaceCharge.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SurfaceCharge.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/SurfaceCharge.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SurfaceCharge.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/SurfaceCharge.d

# Compiles file src/phreeqcpp/SurfaceComp.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/SurfaceComp.d
x64/gccDebugDll/src/phreeqcpp/SurfaceComp.o: src/phreeqcpp/SurfaceComp.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/SurfaceComp.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/SurfaceComp.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/SurfaceComp.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/SurfaceComp.d

# Compiles file src/phreeqcpp/System.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/System.d
x64/gccDebugDll/src/phreeqcpp/System.o: src/phreeqcpp/System.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/System.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/System.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/System.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/System.d

# Compiles file src/phreeqcpp/Temperature.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/Temperature.d
x64/gccDebugDll/src/phreeqcpp/Temperature.o: src/phreeqcpp/Temperature.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Temperature.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/Temperature.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Temperature.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/Temperature.d

# Compiles file src/phreeqcpp/Use.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/Use.d
x64/gccDebugDll/src/phreeqcpp/Use.o: src/phreeqcpp/Use.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Use.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/Use.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Use.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/Use.d

# Compiles file src/phreeqcpp/Utils.cxx for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/Utils.d
x64/gccDebugDll/src/phreeqcpp/Utils.o: src/phreeqcpp/Utils.cxx
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/Utils.cxx $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/Utils.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/Utils.cxx $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/Utils.d

# Compiles file src/phreeqcpp/advection.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/advection.d
x64/gccDebugDll/src/phreeqcpp/advection.o: src/phreeqcpp/advection.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/advection.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/advection.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/advection.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/advection.d

# Compiles file src/phreeqcpp/basicsubs.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/basicsubs.d
x64/gccDebugDll/src/phreeqcpp/basicsubs.o: src/phreeqcpp/basicsubs.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/basicsubs.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/basicsubs.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/basicsubs.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/basicsubs.d

# Compiles file src/phreeqcpp/cl1.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/cl1.d
x64/gccDebugDll/src/phreeqcpp/cl1.o: src/phreeqcpp/cl1.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/cl1.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/cl1.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/cl1.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/cl1.d

# Compiles file src/phreeqcpp/cvdense.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/cvdense.d
x64/gccDebugDll/src/phreeqcpp/cvdense.o: src/phreeqcpp/cvdense.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/cvdense.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/cvdense.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/cvdense.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/cvdense.d

# Compiles file src/phreeqcpp/cvode.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/cvode.d
x64/gccDebugDll/src/phreeqcpp/cvode.o: src/phreeqcpp/cvode.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/cvode.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/cvode.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/cvode.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/cvode.d

# Compiles file src/phreeqcpp/dense.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/dense.d
x64/gccDebugDll/src/phreeqcpp/dense.o: src/phreeqcpp/dense.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/dense.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/dense.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/dense.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/dense.d

# Compiles file src/phreeqcpp/dw.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/dw.d
x64/gccDebugDll/src/phreeqcpp/dw.o: src/phreeqcpp/dw.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/dw.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/dw.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/dw.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/dw.d

# Compiles file src/phreeqcpp/gases.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/gases.d
x64/gccDebugDll/src/phreeqcpp/gases.o: src/phreeqcpp/gases.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/gases.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/gases.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/gases.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/gases.d

# Compiles file src/phreeqcpp/input.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/input.d
x64/gccDebugDll/src/phreeqcpp/input.o: src/phreeqcpp/input.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/input.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/input.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/input.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/input.d

# Compiles file src/phreeqcpp/integrate.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/integrate.d
x64/gccDebugDll/src/phreeqcpp/integrate.o: src/phreeqcpp/integrate.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/integrate.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/integrate.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/integrate.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/integrate.d

# Compiles file src/phreeqcpp/inverse.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/inverse.d
x64/gccDebugDll/src/phreeqcpp/inverse.o: src/phreeqcpp/inverse.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/inverse.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/inverse.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/inverse.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/inverse.d

# Compiles file src/phreeqcpp/isotopes.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/isotopes.d
x64/gccDebugDll/src/phreeqcpp/isotopes.o: src/phreeqcpp/isotopes.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/isotopes.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/isotopes.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/isotopes.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/isotopes.d

# Compiles file src/phreeqcpp/kinetics.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/kinetics.d
x64/gccDebugDll/src/phreeqcpp/kinetics.o: src/phreeqcpp/kinetics.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/kinetics.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/kinetics.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/kinetics.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/kinetics.d

# Compiles file src/phreeqcpp/mainsubs.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/mainsubs.d
x64/gccDebugDll/src/phreeqcpp/mainsubs.o: src/phreeqcpp/mainsubs.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/mainsubs.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/mainsubs.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/mainsubs.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/mainsubs.d

# Compiles file src/phreeqcpp/model.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/model.d
x64/gccDebugDll/src/phreeqcpp/model.o: src/phreeqcpp/model.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/model.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/model.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/model.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/model.d

# Compiles file src/phreeqcpp/nvector.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/nvector.d
x64/gccDebugDll/src/phreeqcpp/nvector.o: src/phreeqcpp/nvector.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/nvector.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/nvector.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/nvector.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/nvector.d

# Compiles file src/phreeqcpp/nvector_serial.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/nvector_serial.d
x64/gccDebugDll/src/phreeqcpp/nvector_serial.o: src/phreeqcpp/nvector_serial.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/nvector_serial.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/nvector_serial.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/nvector_serial.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/nvector_serial.d

# Compiles file src/phreeqcpp/parse.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/parse.d
x64/gccDebugDll/src/phreeqcpp/parse.o: src/phreeqcpp/parse.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/parse.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/parse.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/parse.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/parse.d

# Compiles file src/phreeqcpp/phqalloc.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/phqalloc.d
x64/gccDebugDll/src/phreeqcpp/phqalloc.o: src/phreeqcpp/phqalloc.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/phqalloc.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/phqalloc.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/phqalloc.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/phqalloc.d

# Compiles file src/phreeqcpp/PHRQ_io_output.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/PHRQ_io_output.d
x64/gccDebugDll/src/phreeqcpp/PHRQ_io_output.o: src/phreeqcpp/PHRQ_io_output.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/PHRQ_io_output.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/PHRQ_io_output.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io_output.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/PHRQ_io_output.d

# Compiles file src/phreeqcpp/pitzer.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/pitzer.d
x64/gccDebugDll/src/phreeqcpp/pitzer.o: src/phreeqcpp/pitzer.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/pitzer.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/pitzer.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/pitzer.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/pitzer.d

# Compiles file src/phreeqcpp/pitzer_structures.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/pitzer_structures.d
x64/gccDebugDll/src/phreeqcpp/pitzer_structures.o: src/phreeqcpp/pitzer_structures.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/pitzer_structures.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/pitzer_structures.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/pitzer_structures.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/pitzer_structures.d

# Compiles file src/phreeqcpp/prep.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/prep.d
x64/gccDebugDll/src/phreeqcpp/prep.o: src/phreeqcpp/prep.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/prep.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/prep.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/prep.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/prep.d

# Compiles file src/phreeqcpp/print.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/print.d
x64/gccDebugDll/src/phreeqcpp/print.o: src/phreeqcpp/print.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/print.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/print.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/print.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/print.d

# Compiles file src/phreeqcpp/read.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/read.d
x64/gccDebugDll/src/phreeqcpp/read.o: src/phreeqcpp/read.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/read.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/read.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/read.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/read.d

# Compiles file src/phreeqcpp/readtr.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/readtr.d
x64/gccDebugDll/src/phreeqcpp/readtr.o: src/phreeqcpp/readtr.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/readtr.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/readtr.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/readtr.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/readtr.d

# Compiles file src/phreeqcpp/sit.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/sit.d
x64/gccDebugDll/src/phreeqcpp/sit.o: src/phreeqcpp/sit.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/sit.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/sit.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/sit.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/sit.d

# Compiles file src/phreeqcpp/smalldense.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/smalldense.d
x64/gccDebugDll/src/phreeqcpp/smalldense.o: src/phreeqcpp/smalldense.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/smalldense.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/smalldense.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/smalldense.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/smalldense.d

# Compiles file src/phreeqcpp/spread.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/spread.d
x64/gccDebugDll/src/phreeqcpp/spread.o: src/phreeqcpp/spread.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/spread.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/spread.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/spread.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/spread.d

# Compiles file src/phreeqcpp/step.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/step.d
x64/gccDebugDll/src/phreeqcpp/step.o: src/phreeqcpp/step.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/step.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/step.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/step.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/step.d

# Compiles file src/phreeqcpp/structures.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/structures.d
x64/gccDebugDll/src/phreeqcpp/structures.o: src/phreeqcpp/structures.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/structures.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/structures.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/structures.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/structures.d

# Compiles file src/phreeqcpp/sundialsmath.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/sundialsmath.d
x64/gccDebugDll/src/phreeqcpp/sundialsmath.o: src/phreeqcpp/sundialsmath.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/sundialsmath.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/sundialsmath.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/sundialsmath.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/sundialsmath.d

# Compiles file src/phreeqcpp/tally.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/tally.d
x64/gccDebugDll/src/phreeqcpp/tally.o: src/phreeqcpp/tally.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/tally.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/tally.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/tally.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/tally.d

# Compiles file src/phreeqcpp/tidy.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/tidy.d
x64/gccDebugDll/src/phreeqcpp/tidy.o: src/phreeqcpp/tidy.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/tidy.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/tidy.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/tidy.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/tidy.d

# Compiles file src/phreeqcpp/transport.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/transport.d
x64/gccDebugDll/src/phreeqcpp/transport.o: src/phreeqcpp/transport.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/transport.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/transport.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/transport.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/transport.d

# Compiles file src/phreeqcpp/utilities.cpp for the DebugDll configuration...
-include x64/gccDebugDll/src/phreeqcpp/utilities.d
x64/gccDebugDll/src/phreeqcpp/utilities.o: src/phreeqcpp/utilities.cpp
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -c src/phreeqcpp/utilities.cpp $(DebugDll_Include_Path) -o x64/gccDebugDll/src/phreeqcpp/utilities.o
	$(CPP_COMPILER) $(DebugDll_Preprocessor_Definitions) $(DebugDll_Compiler_Flags) -MM src/phreeqcpp/utilities.cpp $(DebugDll_Include_Path) > x64/gccDebugDll/src/phreeqcpp/utilities.d

# Builds the Debug configuration...
.PHONY: Debug
Debug: create_folders gccDebug/src/fwrap.o gccDebug/src/fwrap2.o gccDebug/src/fwrap3.o gccDebug/src/fwrap4.o gccDebug/src/fwrap5.o gccDebug/src/fwrap6.o gccDebug/src/fwrap7.o gccDebug/src/fwrap8.o gccDebug/src/IPhreeqc.o gccDebug/src/IPhreeqcLib.o gccDebug/src/Var.o gccDebug/src/CSelectedOutput.o gccDebug/src/phreeqcpp/SelectedOutput.o gccDebug/src/phreeqcpp/UserPunch.o gccDebug/src/phreeqcpp/cxxKinetics.o gccDebug/src/phreeqcpp/cxxMix.o gccDebug/src/phreeqcpp/dumper.o gccDebug/src/phreeqcpp/Exchange.o gccDebug/src/phreeqcpp/ExchComp.o gccDebug/src/phreeqcpp/GasComp.o gccDebug/src/phreeqcpp/GasPhase.o gccDebug/src/phreeqcpp/ISolution.o gccDebug/src/phreeqcpp/ISolutionComp.o gccDebug/src/phreeqcpp/Keywords.o gccDebug/src/phreeqcpp/KineticsComp.o gccDebug/src/phreeqcpp/NameDouble.o gccDebug/src/phreeqcpp/NumKeyword.o gccDebug/src/phreeqcpp/Parser.o gccDebug/src/phreeqcpp/PBasic.o gccDebug/src/phreeqcpp/Phreeqc.o gccDebug/src/phreeqcpp/PHRQ_base.o gccDebug/src/phreeqcpp/PHRQ_io.o gccDebug/src/phreeqcpp/PPassemblage.o gccDebug/src/phreeqcpp/PPassemblageComp.o gccDebug/src/phreeqcpp/Pressure.o gccDebug/src/phreeqcpp/Reaction.o gccDebug/src/phreeqcpp/ReadClass.o gccDebug/src/phreeqcpp/runner.o gccDebug/src/phreeqcpp/Solution.o gccDebug/src/phreeqcpp/SolutionIsotope.o gccDebug/src/phreeqcpp/SS.o gccDebug/src/phreeqcpp/SSassemblage.o gccDebug/src/phreeqcpp/SScomp.o gccDebug/src/phreeqcpp/StorageBin.o gccDebug/src/phreeqcpp/StorageBinList.o gccDebug/src/phreeqcpp/Surface.o gccDebug/src/phreeqcpp/SurfaceCharge.o gccDebug/src/phreeqcpp/SurfaceComp.o gccDebug/src/phreeqcpp/System.o gccDebug/src/phreeqcpp/Temperature.o gccDebug/src/phreeqcpp/Use.o gccDebug/src/phreeqcpp/Utils.o gccDebug/src/phreeqcpp/advection.o gccDebug/src/phreeqcpp/basicsubs.o gccDebug/src/phreeqcpp/cl1.o gccDebug/src/phreeqcpp/cvdense.o gccDebug/src/phreeqcpp/cvode.o gccDebug/src/phreeqcpp/dense.o gccDebug/src/phreeqcpp/dw.o gccDebug/src/phreeqcpp/gases.o gccDebug/src/phreeqcpp/input.o gccDebug/src/phreeqcpp/integrate.o gccDebug/src/phreeqcpp/inverse.o gccDebug/src/phreeqcpp/isotopes.o gccDebug/src/phreeqcpp/kinetics.o gccDebug/src/phreeqcpp/mainsubs.o gccDebug/src/phreeqcpp/model.o gccDebug/src/phreeqcpp/nvector.o gccDebug/src/phreeqcpp/nvector_serial.o gccDebug/src/phreeqcpp/parse.o gccDebug/src/phreeqcpp/phqalloc.o gccDebug/src/phreeqcpp/PHRQ_io_output.o gccDebug/src/phreeqcpp/pitzer.o gccDebug/src/phreeqcpp/pitzer_structures.o gccDebug/src/phreeqcpp/prep.o gccDebug/src/phreeqcpp/print.o gccDebug/src/phreeqcpp/read.o gccDebug/src/phreeqcpp/readtr.o gccDebug/src/phreeqcpp/sit.o gccDebug/src/phreeqcpp/smalldense.o gccDebug/src/phreeqcpp/spread.o gccDebug/src/phreeqcpp/step.o gccDebug/src/phreeqcpp/structures.o gccDebug/src/phreeqcpp/sundialsmath.o gccDebug/src/phreeqcpp/tally.o gccDebug/src/phreeqcpp/tidy.o gccDebug/src/phreeqcpp/transport.o gccDebug/src/phreeqcpp/utilities.o 
	ar rcs ../../gccDebug/libIPhreeqc.a gccDebug/src/fwrap.o gccDebug/src/fwrap2.o gccDebug/src/fwrap3.o gccDebug/src/fwrap4.o gccDebug/src/fwrap5.o gccDebug/src/fwrap6.o gccDebug/src/fwrap7.o gccDebug/src/fwrap8.o gccDebug/src/IPhreeqc.o gccDebug/src/IPhreeqcLib.o gccDebug/src/Var.o gccDebug/src/CSelectedOutput.o gccDebug/src/phreeqcpp/SelectedOutput.o gccDebug/src/phreeqcpp/UserPunch.o gccDebug/src/phreeqcpp/cxxKinetics.o gccDebug/src/phreeqcpp/cxxMix.o gccDebug/src/phreeqcpp/dumper.o gccDebug/src/phreeqcpp/Exchange.o gccDebug/src/phreeqcpp/ExchComp.o gccDebug/src/phreeqcpp/GasComp.o gccDebug/src/phreeqcpp/GasPhase.o gccDebug/src/phreeqcpp/ISolution.o gccDebug/src/phreeqcpp/ISolutionComp.o gccDebug/src/phreeqcpp/Keywords.o gccDebug/src/phreeqcpp/KineticsComp.o gccDebug/src/phreeqcpp/NameDouble.o gccDebug/src/phreeqcpp/NumKeyword.o gccDebug/src/phreeqcpp/Parser.o gccDebug/src/phreeqcpp/PBasic.o gccDebug/src/phreeqcpp/Phreeqc.o gccDebug/src/phreeqcpp/PHRQ_base.o gccDebug/src/phreeqcpp/PHRQ_io.o gccDebug/src/phreeqcpp/PPassemblage.o gccDebug/src/phreeqcpp/PPassemblageComp.o gccDebug/src/phreeqcpp/Pressure.o gccDebug/src/phreeqcpp/Reaction.o gccDebug/src/phreeqcpp/ReadClass.o gccDebug/src/phreeqcpp/runner.o gccDebug/src/phreeqcpp/Solution.o gccDebug/src/phreeqcpp/SolutionIsotope.o gccDebug/src/phreeqcpp/SS.o gccDebug/src/phreeqcpp/SSassemblage.o gccDebug/src/phreeqcpp/SScomp.o gccDebug/src/phreeqcpp/StorageBin.o gccDebug/src/phreeqcpp/StorageBinList.o gccDebug/src/phreeqcpp/Surface.o gccDebug/src/phreeqcpp/SurfaceCharge.o gccDebug/src/phreeqcpp/SurfaceComp.o gccDebug/src/phreeqcpp/System.o gccDebug/src/phreeqcpp/Temperature.o gccDebug/src/phreeqcpp/Use.o gccDebug/src/phreeqcpp/Utils.o gccDebug/src/phreeqcpp/advection.o gccDebug/src/phreeqcpp/basicsubs.o gccDebug/src/phreeqcpp/cl1.o gccDebug/src/phreeqcpp/cvdense.o gccDebug/src/phreeqcpp/cvode.o gccDebug/src/phreeqcpp/dense.o gccDebug/src/phreeqcpp/dw.o gccDebug/src/phreeqcpp/gases.o gccDebug/src/phreeqcpp/input.o gccDebug/src/phreeqcpp/integrate.o gccDebug/src/phreeqcpp/inverse.o gccDebug/src/phreeqcpp/isotopes.o gccDebug/src/phreeqcpp/kinetics.o gccDebug/src/phreeqcpp/mainsubs.o gccDebug/src/phreeqcpp/model.o gccDebug/src/phreeqcpp/nvector.o gccDebug/src/phreeqcpp/nvector_serial.o gccDebug/src/phreeqcpp/parse.o gccDebug/src/phreeqcpp/phqalloc.o gccDebug/src/phreeqcpp/PHRQ_io_output.o gccDebug/src/phreeqcpp/pitzer.o gccDebug/src/phreeqcpp/pitzer_structures.o gccDebug/src/phreeqcpp/prep.o gccDebug/src/phreeqcpp/print.o gccDebug/src/phreeqcpp/read.o gccDebug/src/phreeqcpp/readtr.o gccDebug/src/phreeqcpp/sit.o gccDebug/src/phreeqcpp/smalldense.o gccDebug/src/phreeqcpp/spread.o gccDebug/src/phreeqcpp/step.o gccDebug/src/phreeqcpp/structures.o gccDebug/src/phreeqcpp/sundialsmath.o gccDebug/src/phreeqcpp/tally.o gccDebug/src/phreeqcpp/tidy.o gccDebug/src/phreeqcpp/transport.o gccDebug/src/phreeqcpp/utilities.o  $(Debug_Implicitly_Linked_Objects)

# Compiles file src/fwrap.cpp for the Debug configuration...
-include gccDebug/src/fwrap.d
gccDebug/src/fwrap.o: src/fwrap.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap.cpp $(Debug_Include_Path) -o gccDebug/src/fwrap.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap.cpp $(Debug_Include_Path) > gccDebug/src/fwrap.d

# Compiles file src/fwrap2.cpp for the Debug configuration...
-include gccDebug/src/fwrap2.d
gccDebug/src/fwrap2.o: src/fwrap2.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap2.cpp $(Debug_Include_Path) -o gccDebug/src/fwrap2.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap2.cpp $(Debug_Include_Path) > gccDebug/src/fwrap2.d

# Compiles file src/fwrap3.cpp for the Debug configuration...
-include gccDebug/src/fwrap3.d
gccDebug/src/fwrap3.o: src/fwrap3.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap3.cpp $(Debug_Include_Path) -o gccDebug/src/fwrap3.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap3.cpp $(Debug_Include_Path) > gccDebug/src/fwrap3.d

# Compiles file src/fwrap4.cpp for the Debug configuration...
-include gccDebug/src/fwrap4.d
gccDebug/src/fwrap4.o: src/fwrap4.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap4.cpp $(Debug_Include_Path) -o gccDebug/src/fwrap4.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap4.cpp $(Debug_Include_Path) > gccDebug/src/fwrap4.d

# Compiles file src/fwrap5.cpp for the Debug configuration...
-include gccDebug/src/fwrap5.d
gccDebug/src/fwrap5.o: src/fwrap5.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap5.cpp $(Debug_Include_Path) -o gccDebug/src/fwrap5.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap5.cpp $(Debug_Include_Path) > gccDebug/src/fwrap5.d

# Compiles file src/fwrap6.cpp for the Debug configuration...
-include gccDebug/src/fwrap6.d
gccDebug/src/fwrap6.o: src/fwrap6.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap6.cpp $(Debug_Include_Path) -o gccDebug/src/fwrap6.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap6.cpp $(Debug_Include_Path) > gccDebug/src/fwrap6.d

# Compiles file src/fwrap7.cpp for the Debug configuration...
-include gccDebug/src/fwrap7.d
gccDebug/src/fwrap7.o: src/fwrap7.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap7.cpp $(Debug_Include_Path) -o gccDebug/src/fwrap7.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap7.cpp $(Debug_Include_Path) > gccDebug/src/fwrap7.d

# Compiles file src/fwrap8.cpp for the Debug configuration...
-include gccDebug/src/fwrap8.d
gccDebug/src/fwrap8.o: src/fwrap8.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap8.cpp $(Debug_Include_Path) -o gccDebug/src/fwrap8.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap8.cpp $(Debug_Include_Path) > gccDebug/src/fwrap8.d

# Compiles file src/IPhreeqc.cpp for the Debug configuration...
-include gccDebug/src/IPhreeqc.d
gccDebug/src/IPhreeqc.o: src/IPhreeqc.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/IPhreeqc.cpp $(Debug_Include_Path) -o gccDebug/src/IPhreeqc.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/IPhreeqc.cpp $(Debug_Include_Path) > gccDebug/src/IPhreeqc.d

# Compiles file src/IPhreeqcLib.cpp for the Debug configuration...
-include gccDebug/src/IPhreeqcLib.d
gccDebug/src/IPhreeqcLib.o: src/IPhreeqcLib.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/IPhreeqcLib.cpp $(Debug_Include_Path) -o gccDebug/src/IPhreeqcLib.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/IPhreeqcLib.cpp $(Debug_Include_Path) > gccDebug/src/IPhreeqcLib.d

# Compiles file src/Var.c for the Debug configuration...
-include gccDebug/src/Var.d
gccDebug/src/Var.o: src/Var.c
	$(C_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/Var.c $(Debug_Include_Path) -o gccDebug/src/Var.o
	$(C_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/Var.c $(Debug_Include_Path) > gccDebug/src/Var.d

# Compiles file src/CSelectedOutput.cpp for the Debug configuration...
-include gccDebug/src/CSelectedOutput.d
gccDebug/src/CSelectedOutput.o: src/CSelectedOutput.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/CSelectedOutput.cpp $(Debug_Include_Path) -o gccDebug/src/CSelectedOutput.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/CSelectedOutput.cpp $(Debug_Include_Path) > gccDebug/src/CSelectedOutput.d

# Compiles file src/phreeqcpp/SelectedOutput.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/SelectedOutput.d
gccDebug/src/phreeqcpp/SelectedOutput.o: src/phreeqcpp/SelectedOutput.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SelectedOutput.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/SelectedOutput.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SelectedOutput.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/SelectedOutput.d

# Compiles file src/phreeqcpp/UserPunch.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/UserPunch.d
gccDebug/src/phreeqcpp/UserPunch.o: src/phreeqcpp/UserPunch.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/UserPunch.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/UserPunch.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/UserPunch.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/UserPunch.d

# Compiles file src/phreeqcpp/cxxKinetics.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/cxxKinetics.d
gccDebug/src/phreeqcpp/cxxKinetics.o: src/phreeqcpp/cxxKinetics.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/cxxKinetics.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/cxxKinetics.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/cxxKinetics.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/cxxKinetics.d

# Compiles file src/phreeqcpp/cxxMix.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/cxxMix.d
gccDebug/src/phreeqcpp/cxxMix.o: src/phreeqcpp/cxxMix.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/cxxMix.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/cxxMix.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/cxxMix.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/cxxMix.d

# Compiles file src/phreeqcpp/dumper.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/dumper.d
gccDebug/src/phreeqcpp/dumper.o: src/phreeqcpp/dumper.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/dumper.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/dumper.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/dumper.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/dumper.d

# Compiles file src/phreeqcpp/Exchange.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/Exchange.d
gccDebug/src/phreeqcpp/Exchange.o: src/phreeqcpp/Exchange.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Exchange.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/Exchange.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Exchange.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/Exchange.d

# Compiles file src/phreeqcpp/ExchComp.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/ExchComp.d
gccDebug/src/phreeqcpp/ExchComp.o: src/phreeqcpp/ExchComp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/ExchComp.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/ExchComp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/ExchComp.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/ExchComp.d

# Compiles file src/phreeqcpp/GasComp.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/GasComp.d
gccDebug/src/phreeqcpp/GasComp.o: src/phreeqcpp/GasComp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/GasComp.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/GasComp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/GasComp.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/GasComp.d

# Compiles file src/phreeqcpp/GasPhase.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/GasPhase.d
gccDebug/src/phreeqcpp/GasPhase.o: src/phreeqcpp/GasPhase.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/GasPhase.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/GasPhase.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/GasPhase.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/GasPhase.d

# Compiles file src/phreeqcpp/ISolution.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/ISolution.d
gccDebug/src/phreeqcpp/ISolution.o: src/phreeqcpp/ISolution.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/ISolution.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/ISolution.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/ISolution.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/ISolution.d

# Compiles file src/phreeqcpp/ISolutionComp.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/ISolutionComp.d
gccDebug/src/phreeqcpp/ISolutionComp.o: src/phreeqcpp/ISolutionComp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/ISolutionComp.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/ISolutionComp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/ISolutionComp.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/ISolutionComp.d

# Compiles file src/phreeqcpp/Keywords.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/Keywords.d
gccDebug/src/phreeqcpp/Keywords.o: src/phreeqcpp/Keywords.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Keywords.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/Keywords.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Keywords.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/Keywords.d

# Compiles file src/phreeqcpp/KineticsComp.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/KineticsComp.d
gccDebug/src/phreeqcpp/KineticsComp.o: src/phreeqcpp/KineticsComp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/KineticsComp.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/KineticsComp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/KineticsComp.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/KineticsComp.d

# Compiles file src/phreeqcpp/NameDouble.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/NameDouble.d
gccDebug/src/phreeqcpp/NameDouble.o: src/phreeqcpp/NameDouble.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/NameDouble.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/NameDouble.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/NameDouble.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/NameDouble.d

# Compiles file src/phreeqcpp/NumKeyword.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/NumKeyword.d
gccDebug/src/phreeqcpp/NumKeyword.o: src/phreeqcpp/NumKeyword.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/NumKeyword.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/NumKeyword.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/NumKeyword.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/NumKeyword.d

# Compiles file src/phreeqcpp/Parser.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/Parser.d
gccDebug/src/phreeqcpp/Parser.o: src/phreeqcpp/Parser.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Parser.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/Parser.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Parser.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/Parser.d

# Compiles file src/phreeqcpp/PBasic.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/PBasic.d
gccDebug/src/phreeqcpp/PBasic.o: src/phreeqcpp/PBasic.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/PBasic.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/PBasic.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/PBasic.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/PBasic.d

# Compiles file src/phreeqcpp/Phreeqc.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/Phreeqc.d
gccDebug/src/phreeqcpp/Phreeqc.o: src/phreeqcpp/Phreeqc.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Phreeqc.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/Phreeqc.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Phreeqc.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/Phreeqc.d

# Compiles file src/phreeqcpp/PHRQ_base.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/PHRQ_base.d
gccDebug/src/phreeqcpp/PHRQ_base.o: src/phreeqcpp/PHRQ_base.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/PHRQ_base.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/PHRQ_base.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/PHRQ_base.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/PHRQ_base.d

# Compiles file src/phreeqcpp/PHRQ_io.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/PHRQ_io.d
gccDebug/src/phreeqcpp/PHRQ_io.o: src/phreeqcpp/PHRQ_io.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/PHRQ_io.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/PHRQ_io.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/PHRQ_io.d

# Compiles file src/phreeqcpp/PPassemblage.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/PPassemblage.d
gccDebug/src/phreeqcpp/PPassemblage.o: src/phreeqcpp/PPassemblage.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/PPassemblage.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/PPassemblage.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/PPassemblage.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/PPassemblage.d

# Compiles file src/phreeqcpp/PPassemblageComp.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/PPassemblageComp.d
gccDebug/src/phreeqcpp/PPassemblageComp.o: src/phreeqcpp/PPassemblageComp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/PPassemblageComp.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/PPassemblageComp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/PPassemblageComp.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/PPassemblageComp.d

# Compiles file src/phreeqcpp/Pressure.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/Pressure.d
gccDebug/src/phreeqcpp/Pressure.o: src/phreeqcpp/Pressure.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Pressure.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/Pressure.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Pressure.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/Pressure.d

# Compiles file src/phreeqcpp/Reaction.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/Reaction.d
gccDebug/src/phreeqcpp/Reaction.o: src/phreeqcpp/Reaction.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Reaction.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/Reaction.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Reaction.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/Reaction.d

# Compiles file src/phreeqcpp/ReadClass.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/ReadClass.d
gccDebug/src/phreeqcpp/ReadClass.o: src/phreeqcpp/ReadClass.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/ReadClass.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/ReadClass.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/ReadClass.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/ReadClass.d

# Compiles file src/phreeqcpp/runner.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/runner.d
gccDebug/src/phreeqcpp/runner.o: src/phreeqcpp/runner.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/runner.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/runner.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/runner.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/runner.d

# Compiles file src/phreeqcpp/Solution.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/Solution.d
gccDebug/src/phreeqcpp/Solution.o: src/phreeqcpp/Solution.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Solution.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/Solution.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Solution.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/Solution.d

# Compiles file src/phreeqcpp/SolutionIsotope.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/SolutionIsotope.d
gccDebug/src/phreeqcpp/SolutionIsotope.o: src/phreeqcpp/SolutionIsotope.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SolutionIsotope.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/SolutionIsotope.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SolutionIsotope.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/SolutionIsotope.d

# Compiles file src/phreeqcpp/SS.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/SS.d
gccDebug/src/phreeqcpp/SS.o: src/phreeqcpp/SS.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SS.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/SS.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SS.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/SS.d

# Compiles file src/phreeqcpp/SSassemblage.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/SSassemblage.d
gccDebug/src/phreeqcpp/SSassemblage.o: src/phreeqcpp/SSassemblage.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SSassemblage.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/SSassemblage.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SSassemblage.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/SSassemblage.d

# Compiles file src/phreeqcpp/SScomp.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/SScomp.d
gccDebug/src/phreeqcpp/SScomp.o: src/phreeqcpp/SScomp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SScomp.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/SScomp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SScomp.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/SScomp.d

# Compiles file src/phreeqcpp/StorageBin.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/StorageBin.d
gccDebug/src/phreeqcpp/StorageBin.o: src/phreeqcpp/StorageBin.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/StorageBin.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/StorageBin.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/StorageBin.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/StorageBin.d

# Compiles file src/phreeqcpp/StorageBinList.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/StorageBinList.d
gccDebug/src/phreeqcpp/StorageBinList.o: src/phreeqcpp/StorageBinList.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/StorageBinList.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/StorageBinList.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/StorageBinList.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/StorageBinList.d

# Compiles file src/phreeqcpp/Surface.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/Surface.d
gccDebug/src/phreeqcpp/Surface.o: src/phreeqcpp/Surface.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Surface.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/Surface.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Surface.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/Surface.d

# Compiles file src/phreeqcpp/SurfaceCharge.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/SurfaceCharge.d
gccDebug/src/phreeqcpp/SurfaceCharge.o: src/phreeqcpp/SurfaceCharge.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SurfaceCharge.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/SurfaceCharge.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SurfaceCharge.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/SurfaceCharge.d

# Compiles file src/phreeqcpp/SurfaceComp.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/SurfaceComp.d
gccDebug/src/phreeqcpp/SurfaceComp.o: src/phreeqcpp/SurfaceComp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SurfaceComp.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/SurfaceComp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SurfaceComp.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/SurfaceComp.d

# Compiles file src/phreeqcpp/System.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/System.d
gccDebug/src/phreeqcpp/System.o: src/phreeqcpp/System.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/System.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/System.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/System.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/System.d

# Compiles file src/phreeqcpp/Temperature.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/Temperature.d
gccDebug/src/phreeqcpp/Temperature.o: src/phreeqcpp/Temperature.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Temperature.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/Temperature.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Temperature.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/Temperature.d

# Compiles file src/phreeqcpp/Use.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/Use.d
gccDebug/src/phreeqcpp/Use.o: src/phreeqcpp/Use.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Use.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/Use.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Use.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/Use.d

# Compiles file src/phreeqcpp/Utils.cxx for the Debug configuration...
-include gccDebug/src/phreeqcpp/Utils.d
gccDebug/src/phreeqcpp/Utils.o: src/phreeqcpp/Utils.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Utils.cxx $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/Utils.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Utils.cxx $(Debug_Include_Path) > gccDebug/src/phreeqcpp/Utils.d

# Compiles file src/phreeqcpp/advection.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/advection.d
gccDebug/src/phreeqcpp/advection.o: src/phreeqcpp/advection.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/advection.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/advection.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/advection.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/advection.d

# Compiles file src/phreeqcpp/basicsubs.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/basicsubs.d
gccDebug/src/phreeqcpp/basicsubs.o: src/phreeqcpp/basicsubs.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/basicsubs.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/basicsubs.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/basicsubs.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/basicsubs.d

# Compiles file src/phreeqcpp/cl1.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/cl1.d
gccDebug/src/phreeqcpp/cl1.o: src/phreeqcpp/cl1.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/cl1.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/cl1.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/cl1.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/cl1.d

# Compiles file src/phreeqcpp/cvdense.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/cvdense.d
gccDebug/src/phreeqcpp/cvdense.o: src/phreeqcpp/cvdense.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/cvdense.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/cvdense.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/cvdense.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/cvdense.d

# Compiles file src/phreeqcpp/cvode.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/cvode.d
gccDebug/src/phreeqcpp/cvode.o: src/phreeqcpp/cvode.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/cvode.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/cvode.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/cvode.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/cvode.d

# Compiles file src/phreeqcpp/dense.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/dense.d
gccDebug/src/phreeqcpp/dense.o: src/phreeqcpp/dense.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/dense.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/dense.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/dense.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/dense.d

# Compiles file src/phreeqcpp/dw.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/dw.d
gccDebug/src/phreeqcpp/dw.o: src/phreeqcpp/dw.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/dw.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/dw.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/dw.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/dw.d

# Compiles file src/phreeqcpp/gases.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/gases.d
gccDebug/src/phreeqcpp/gases.o: src/phreeqcpp/gases.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/gases.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/gases.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/gases.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/gases.d

# Compiles file src/phreeqcpp/input.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/input.d
gccDebug/src/phreeqcpp/input.o: src/phreeqcpp/input.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/input.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/input.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/input.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/input.d

# Compiles file src/phreeqcpp/integrate.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/integrate.d
gccDebug/src/phreeqcpp/integrate.o: src/phreeqcpp/integrate.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/integrate.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/integrate.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/integrate.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/integrate.d

# Compiles file src/phreeqcpp/inverse.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/inverse.d
gccDebug/src/phreeqcpp/inverse.o: src/phreeqcpp/inverse.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/inverse.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/inverse.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/inverse.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/inverse.d

# Compiles file src/phreeqcpp/isotopes.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/isotopes.d
gccDebug/src/phreeqcpp/isotopes.o: src/phreeqcpp/isotopes.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/isotopes.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/isotopes.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/isotopes.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/isotopes.d

# Compiles file src/phreeqcpp/kinetics.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/kinetics.d
gccDebug/src/phreeqcpp/kinetics.o: src/phreeqcpp/kinetics.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/kinetics.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/kinetics.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/kinetics.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/kinetics.d

# Compiles file src/phreeqcpp/mainsubs.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/mainsubs.d
gccDebug/src/phreeqcpp/mainsubs.o: src/phreeqcpp/mainsubs.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/mainsubs.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/mainsubs.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/mainsubs.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/mainsubs.d

# Compiles file src/phreeqcpp/model.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/model.d
gccDebug/src/phreeqcpp/model.o: src/phreeqcpp/model.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/model.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/model.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/model.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/model.d

# Compiles file src/phreeqcpp/nvector.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/nvector.d
gccDebug/src/phreeqcpp/nvector.o: src/phreeqcpp/nvector.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/nvector.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/nvector.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/nvector.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/nvector.d

# Compiles file src/phreeqcpp/nvector_serial.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/nvector_serial.d
gccDebug/src/phreeqcpp/nvector_serial.o: src/phreeqcpp/nvector_serial.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/nvector_serial.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/nvector_serial.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/nvector_serial.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/nvector_serial.d

# Compiles file src/phreeqcpp/parse.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/parse.d
gccDebug/src/phreeqcpp/parse.o: src/phreeqcpp/parse.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/parse.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/parse.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/parse.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/parse.d

# Compiles file src/phreeqcpp/phqalloc.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/phqalloc.d
gccDebug/src/phreeqcpp/phqalloc.o: src/phreeqcpp/phqalloc.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/phqalloc.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/phqalloc.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/phqalloc.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/phqalloc.d

# Compiles file src/phreeqcpp/PHRQ_io_output.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/PHRQ_io_output.d
gccDebug/src/phreeqcpp/PHRQ_io_output.o: src/phreeqcpp/PHRQ_io_output.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/PHRQ_io_output.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/PHRQ_io_output.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io_output.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/PHRQ_io_output.d

# Compiles file src/phreeqcpp/pitzer.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/pitzer.d
gccDebug/src/phreeqcpp/pitzer.o: src/phreeqcpp/pitzer.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/pitzer.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/pitzer.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/pitzer.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/pitzer.d

# Compiles file src/phreeqcpp/pitzer_structures.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/pitzer_structures.d
gccDebug/src/phreeqcpp/pitzer_structures.o: src/phreeqcpp/pitzer_structures.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/pitzer_structures.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/pitzer_structures.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/pitzer_structures.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/pitzer_structures.d

# Compiles file src/phreeqcpp/prep.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/prep.d
gccDebug/src/phreeqcpp/prep.o: src/phreeqcpp/prep.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/prep.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/prep.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/prep.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/prep.d

# Compiles file src/phreeqcpp/print.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/print.d
gccDebug/src/phreeqcpp/print.o: src/phreeqcpp/print.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/print.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/print.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/print.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/print.d

# Compiles file src/phreeqcpp/read.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/read.d
gccDebug/src/phreeqcpp/read.o: src/phreeqcpp/read.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/read.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/read.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/read.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/read.d

# Compiles file src/phreeqcpp/readtr.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/readtr.d
gccDebug/src/phreeqcpp/readtr.o: src/phreeqcpp/readtr.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/readtr.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/readtr.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/readtr.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/readtr.d

# Compiles file src/phreeqcpp/sit.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/sit.d
gccDebug/src/phreeqcpp/sit.o: src/phreeqcpp/sit.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/sit.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/sit.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/sit.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/sit.d

# Compiles file src/phreeqcpp/smalldense.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/smalldense.d
gccDebug/src/phreeqcpp/smalldense.o: src/phreeqcpp/smalldense.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/smalldense.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/smalldense.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/smalldense.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/smalldense.d

# Compiles file src/phreeqcpp/spread.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/spread.d
gccDebug/src/phreeqcpp/spread.o: src/phreeqcpp/spread.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/spread.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/spread.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/spread.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/spread.d

# Compiles file src/phreeqcpp/step.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/step.d
gccDebug/src/phreeqcpp/step.o: src/phreeqcpp/step.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/step.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/step.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/step.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/step.d

# Compiles file src/phreeqcpp/structures.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/structures.d
gccDebug/src/phreeqcpp/structures.o: src/phreeqcpp/structures.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/structures.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/structures.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/structures.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/structures.d

# Compiles file src/phreeqcpp/sundialsmath.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/sundialsmath.d
gccDebug/src/phreeqcpp/sundialsmath.o: src/phreeqcpp/sundialsmath.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/sundialsmath.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/sundialsmath.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/sundialsmath.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/sundialsmath.d

# Compiles file src/phreeqcpp/tally.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/tally.d
gccDebug/src/phreeqcpp/tally.o: src/phreeqcpp/tally.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/tally.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/tally.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/tally.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/tally.d

# Compiles file src/phreeqcpp/tidy.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/tidy.d
gccDebug/src/phreeqcpp/tidy.o: src/phreeqcpp/tidy.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/tidy.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/tidy.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/tidy.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/tidy.d

# Compiles file src/phreeqcpp/transport.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/transport.d
gccDebug/src/phreeqcpp/transport.o: src/phreeqcpp/transport.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/transport.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/transport.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/transport.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/transport.d

# Compiles file src/phreeqcpp/utilities.cpp for the Debug configuration...
-include gccDebug/src/phreeqcpp/utilities.d
gccDebug/src/phreeqcpp/utilities.o: src/phreeqcpp/utilities.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/utilities.cpp $(Debug_Include_Path) -o gccDebug/src/phreeqcpp/utilities.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/utilities.cpp $(Debug_Include_Path) > gccDebug/src/phreeqcpp/utilities.d

# Builds the Debug configuration...
.PHONY: Debug
Debug: create_folders x64/gccDebug/src/fwrap.o x64/gccDebug/src/fwrap2.o x64/gccDebug/src/fwrap3.o x64/gccDebug/src/fwrap4.o x64/gccDebug/src/fwrap5.o x64/gccDebug/src/fwrap6.o x64/gccDebug/src/fwrap7.o x64/gccDebug/src/fwrap8.o x64/gccDebug/src/IPhreeqc.o x64/gccDebug/src/IPhreeqcLib.o x64/gccDebug/src/Var.o x64/gccDebug/src/CSelectedOutput.o x64/gccDebug/src/phreeqcpp/SelectedOutput.o x64/gccDebug/src/phreeqcpp/UserPunch.o x64/gccDebug/src/phreeqcpp/cxxKinetics.o x64/gccDebug/src/phreeqcpp/cxxMix.o x64/gccDebug/src/phreeqcpp/dumper.o x64/gccDebug/src/phreeqcpp/Exchange.o x64/gccDebug/src/phreeqcpp/ExchComp.o x64/gccDebug/src/phreeqcpp/GasComp.o x64/gccDebug/src/phreeqcpp/GasPhase.o x64/gccDebug/src/phreeqcpp/ISolution.o x64/gccDebug/src/phreeqcpp/ISolutionComp.o x64/gccDebug/src/phreeqcpp/Keywords.o x64/gccDebug/src/phreeqcpp/KineticsComp.o x64/gccDebug/src/phreeqcpp/NameDouble.o x64/gccDebug/src/phreeqcpp/NumKeyword.o x64/gccDebug/src/phreeqcpp/Parser.o x64/gccDebug/src/phreeqcpp/PBasic.o x64/gccDebug/src/phreeqcpp/Phreeqc.o x64/gccDebug/src/phreeqcpp/PHRQ_base.o x64/gccDebug/src/phreeqcpp/PHRQ_io.o x64/gccDebug/src/phreeqcpp/PPassemblage.o x64/gccDebug/src/phreeqcpp/PPassemblageComp.o x64/gccDebug/src/phreeqcpp/Pressure.o x64/gccDebug/src/phreeqcpp/Reaction.o x64/gccDebug/src/phreeqcpp/ReadClass.o x64/gccDebug/src/phreeqcpp/runner.o x64/gccDebug/src/phreeqcpp/Solution.o x64/gccDebug/src/phreeqcpp/SolutionIsotope.o x64/gccDebug/src/phreeqcpp/SS.o x64/gccDebug/src/phreeqcpp/SSassemblage.o x64/gccDebug/src/phreeqcpp/SScomp.o x64/gccDebug/src/phreeqcpp/StorageBin.o x64/gccDebug/src/phreeqcpp/StorageBinList.o x64/gccDebug/src/phreeqcpp/Surface.o x64/gccDebug/src/phreeqcpp/SurfaceCharge.o x64/gccDebug/src/phreeqcpp/SurfaceComp.o x64/gccDebug/src/phreeqcpp/System.o x64/gccDebug/src/phreeqcpp/Temperature.o x64/gccDebug/src/phreeqcpp/Use.o x64/gccDebug/src/phreeqcpp/Utils.o x64/gccDebug/src/phreeqcpp/advection.o x64/gccDebug/src/phreeqcpp/basicsubs.o x64/gccDebug/src/phreeqcpp/cl1.o x64/gccDebug/src/phreeqcpp/cvdense.o x64/gccDebug/src/phreeqcpp/cvode.o x64/gccDebug/src/phreeqcpp/dense.o x64/gccDebug/src/phreeqcpp/dw.o x64/gccDebug/src/phreeqcpp/gases.o x64/gccDebug/src/phreeqcpp/input.o x64/gccDebug/src/phreeqcpp/integrate.o x64/gccDebug/src/phreeqcpp/inverse.o x64/gccDebug/src/phreeqcpp/isotopes.o x64/gccDebug/src/phreeqcpp/kinetics.o x64/gccDebug/src/phreeqcpp/mainsubs.o x64/gccDebug/src/phreeqcpp/model.o x64/gccDebug/src/phreeqcpp/nvector.o x64/gccDebug/src/phreeqcpp/nvector_serial.o x64/gccDebug/src/phreeqcpp/parse.o x64/gccDebug/src/phreeqcpp/phqalloc.o x64/gccDebug/src/phreeqcpp/PHRQ_io_output.o x64/gccDebug/src/phreeqcpp/pitzer.o x64/gccDebug/src/phreeqcpp/pitzer_structures.o x64/gccDebug/src/phreeqcpp/prep.o x64/gccDebug/src/phreeqcpp/print.o x64/gccDebug/src/phreeqcpp/read.o x64/gccDebug/src/phreeqcpp/readtr.o x64/gccDebug/src/phreeqcpp/sit.o x64/gccDebug/src/phreeqcpp/smalldense.o x64/gccDebug/src/phreeqcpp/spread.o x64/gccDebug/src/phreeqcpp/step.o x64/gccDebug/src/phreeqcpp/structures.o x64/gccDebug/src/phreeqcpp/sundialsmath.o x64/gccDebug/src/phreeqcpp/tally.o x64/gccDebug/src/phreeqcpp/tidy.o x64/gccDebug/src/phreeqcpp/transport.o x64/gccDebug/src/phreeqcpp/utilities.o 
	ar rcs ../../x64/gccDebug/libIPhreeqc.a x64/gccDebug/src/fwrap.o x64/gccDebug/src/fwrap2.o x64/gccDebug/src/fwrap3.o x64/gccDebug/src/fwrap4.o x64/gccDebug/src/fwrap5.o x64/gccDebug/src/fwrap6.o x64/gccDebug/src/fwrap7.o x64/gccDebug/src/fwrap8.o x64/gccDebug/src/IPhreeqc.o x64/gccDebug/src/IPhreeqcLib.o x64/gccDebug/src/Var.o x64/gccDebug/src/CSelectedOutput.o x64/gccDebug/src/phreeqcpp/SelectedOutput.o x64/gccDebug/src/phreeqcpp/UserPunch.o x64/gccDebug/src/phreeqcpp/cxxKinetics.o x64/gccDebug/src/phreeqcpp/cxxMix.o x64/gccDebug/src/phreeqcpp/dumper.o x64/gccDebug/src/phreeqcpp/Exchange.o x64/gccDebug/src/phreeqcpp/ExchComp.o x64/gccDebug/src/phreeqcpp/GasComp.o x64/gccDebug/src/phreeqcpp/GasPhase.o x64/gccDebug/src/phreeqcpp/ISolution.o x64/gccDebug/src/phreeqcpp/ISolutionComp.o x64/gccDebug/src/phreeqcpp/Keywords.o x64/gccDebug/src/phreeqcpp/KineticsComp.o x64/gccDebug/src/phreeqcpp/NameDouble.o x64/gccDebug/src/phreeqcpp/NumKeyword.o x64/gccDebug/src/phreeqcpp/Parser.o x64/gccDebug/src/phreeqcpp/PBasic.o x64/gccDebug/src/phreeqcpp/Phreeqc.o x64/gccDebug/src/phreeqcpp/PHRQ_base.o x64/gccDebug/src/phreeqcpp/PHRQ_io.o x64/gccDebug/src/phreeqcpp/PPassemblage.o x64/gccDebug/src/phreeqcpp/PPassemblageComp.o x64/gccDebug/src/phreeqcpp/Pressure.o x64/gccDebug/src/phreeqcpp/Reaction.o x64/gccDebug/src/phreeqcpp/ReadClass.o x64/gccDebug/src/phreeqcpp/runner.o x64/gccDebug/src/phreeqcpp/Solution.o x64/gccDebug/src/phreeqcpp/SolutionIsotope.o x64/gccDebug/src/phreeqcpp/SS.o x64/gccDebug/src/phreeqcpp/SSassemblage.o x64/gccDebug/src/phreeqcpp/SScomp.o x64/gccDebug/src/phreeqcpp/StorageBin.o x64/gccDebug/src/phreeqcpp/StorageBinList.o x64/gccDebug/src/phreeqcpp/Surface.o x64/gccDebug/src/phreeqcpp/SurfaceCharge.o x64/gccDebug/src/phreeqcpp/SurfaceComp.o x64/gccDebug/src/phreeqcpp/System.o x64/gccDebug/src/phreeqcpp/Temperature.o x64/gccDebug/src/phreeqcpp/Use.o x64/gccDebug/src/phreeqcpp/Utils.o x64/gccDebug/src/phreeqcpp/advection.o x64/gccDebug/src/phreeqcpp/basicsubs.o x64/gccDebug/src/phreeqcpp/cl1.o x64/gccDebug/src/phreeqcpp/cvdense.o x64/gccDebug/src/phreeqcpp/cvode.o x64/gccDebug/src/phreeqcpp/dense.o x64/gccDebug/src/phreeqcpp/dw.o x64/gccDebug/src/phreeqcpp/gases.o x64/gccDebug/src/phreeqcpp/input.o x64/gccDebug/src/phreeqcpp/integrate.o x64/gccDebug/src/phreeqcpp/inverse.o x64/gccDebug/src/phreeqcpp/isotopes.o x64/gccDebug/src/phreeqcpp/kinetics.o x64/gccDebug/src/phreeqcpp/mainsubs.o x64/gccDebug/src/phreeqcpp/model.o x64/gccDebug/src/phreeqcpp/nvector.o x64/gccDebug/src/phreeqcpp/nvector_serial.o x64/gccDebug/src/phreeqcpp/parse.o x64/gccDebug/src/phreeqcpp/phqalloc.o x64/gccDebug/src/phreeqcpp/PHRQ_io_output.o x64/gccDebug/src/phreeqcpp/pitzer.o x64/gccDebug/src/phreeqcpp/pitzer_structures.o x64/gccDebug/src/phreeqcpp/prep.o x64/gccDebug/src/phreeqcpp/print.o x64/gccDebug/src/phreeqcpp/read.o x64/gccDebug/src/phreeqcpp/readtr.o x64/gccDebug/src/phreeqcpp/sit.o x64/gccDebug/src/phreeqcpp/smalldense.o x64/gccDebug/src/phreeqcpp/spread.o x64/gccDebug/src/phreeqcpp/step.o x64/gccDebug/src/phreeqcpp/structures.o x64/gccDebug/src/phreeqcpp/sundialsmath.o x64/gccDebug/src/phreeqcpp/tally.o x64/gccDebug/src/phreeqcpp/tidy.o x64/gccDebug/src/phreeqcpp/transport.o x64/gccDebug/src/phreeqcpp/utilities.o  $(Debug_Implicitly_Linked_Objects)

# Compiles file src/fwrap.cpp for the Debug configuration...
-include x64/gccDebug/src/fwrap.d
x64/gccDebug/src/fwrap.o: src/fwrap.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap.cpp $(Debug_Include_Path) -o x64/gccDebug/src/fwrap.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap.cpp $(Debug_Include_Path) > x64/gccDebug/src/fwrap.d

# Compiles file src/fwrap2.cpp for the Debug configuration...
-include x64/gccDebug/src/fwrap2.d
x64/gccDebug/src/fwrap2.o: src/fwrap2.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap2.cpp $(Debug_Include_Path) -o x64/gccDebug/src/fwrap2.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap2.cpp $(Debug_Include_Path) > x64/gccDebug/src/fwrap2.d

# Compiles file src/fwrap3.cpp for the Debug configuration...
-include x64/gccDebug/src/fwrap3.d
x64/gccDebug/src/fwrap3.o: src/fwrap3.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap3.cpp $(Debug_Include_Path) -o x64/gccDebug/src/fwrap3.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap3.cpp $(Debug_Include_Path) > x64/gccDebug/src/fwrap3.d

# Compiles file src/fwrap4.cpp for the Debug configuration...
-include x64/gccDebug/src/fwrap4.d
x64/gccDebug/src/fwrap4.o: src/fwrap4.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap4.cpp $(Debug_Include_Path) -o x64/gccDebug/src/fwrap4.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap4.cpp $(Debug_Include_Path) > x64/gccDebug/src/fwrap4.d

# Compiles file src/fwrap5.cpp for the Debug configuration...
-include x64/gccDebug/src/fwrap5.d
x64/gccDebug/src/fwrap5.o: src/fwrap5.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap5.cpp $(Debug_Include_Path) -o x64/gccDebug/src/fwrap5.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap5.cpp $(Debug_Include_Path) > x64/gccDebug/src/fwrap5.d

# Compiles file src/fwrap6.cpp for the Debug configuration...
-include x64/gccDebug/src/fwrap6.d
x64/gccDebug/src/fwrap6.o: src/fwrap6.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap6.cpp $(Debug_Include_Path) -o x64/gccDebug/src/fwrap6.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap6.cpp $(Debug_Include_Path) > x64/gccDebug/src/fwrap6.d

# Compiles file src/fwrap7.cpp for the Debug configuration...
-include x64/gccDebug/src/fwrap7.d
x64/gccDebug/src/fwrap7.o: src/fwrap7.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap7.cpp $(Debug_Include_Path) -o x64/gccDebug/src/fwrap7.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap7.cpp $(Debug_Include_Path) > x64/gccDebug/src/fwrap7.d

# Compiles file src/fwrap8.cpp for the Debug configuration...
-include x64/gccDebug/src/fwrap8.d
x64/gccDebug/src/fwrap8.o: src/fwrap8.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/fwrap8.cpp $(Debug_Include_Path) -o x64/gccDebug/src/fwrap8.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/fwrap8.cpp $(Debug_Include_Path) > x64/gccDebug/src/fwrap8.d

# Compiles file src/IPhreeqc.cpp for the Debug configuration...
-include x64/gccDebug/src/IPhreeqc.d
x64/gccDebug/src/IPhreeqc.o: src/IPhreeqc.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/IPhreeqc.cpp $(Debug_Include_Path) -o x64/gccDebug/src/IPhreeqc.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/IPhreeqc.cpp $(Debug_Include_Path) > x64/gccDebug/src/IPhreeqc.d

# Compiles file src/IPhreeqcLib.cpp for the Debug configuration...
-include x64/gccDebug/src/IPhreeqcLib.d
x64/gccDebug/src/IPhreeqcLib.o: src/IPhreeqcLib.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/IPhreeqcLib.cpp $(Debug_Include_Path) -o x64/gccDebug/src/IPhreeqcLib.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/IPhreeqcLib.cpp $(Debug_Include_Path) > x64/gccDebug/src/IPhreeqcLib.d

# Compiles file src/Var.c for the Debug configuration...
-include x64/gccDebug/src/Var.d
x64/gccDebug/src/Var.o: src/Var.c
	$(C_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/Var.c $(Debug_Include_Path) -o x64/gccDebug/src/Var.o
	$(C_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/Var.c $(Debug_Include_Path) > x64/gccDebug/src/Var.d

# Compiles file src/CSelectedOutput.cpp for the Debug configuration...
-include x64/gccDebug/src/CSelectedOutput.d
x64/gccDebug/src/CSelectedOutput.o: src/CSelectedOutput.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/CSelectedOutput.cpp $(Debug_Include_Path) -o x64/gccDebug/src/CSelectedOutput.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/CSelectedOutput.cpp $(Debug_Include_Path) > x64/gccDebug/src/CSelectedOutput.d

# Compiles file src/phreeqcpp/SelectedOutput.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/SelectedOutput.d
x64/gccDebug/src/phreeqcpp/SelectedOutput.o: src/phreeqcpp/SelectedOutput.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SelectedOutput.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/SelectedOutput.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SelectedOutput.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/SelectedOutput.d

# Compiles file src/phreeqcpp/UserPunch.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/UserPunch.d
x64/gccDebug/src/phreeqcpp/UserPunch.o: src/phreeqcpp/UserPunch.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/UserPunch.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/UserPunch.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/UserPunch.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/UserPunch.d

# Compiles file src/phreeqcpp/cxxKinetics.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/cxxKinetics.d
x64/gccDebug/src/phreeqcpp/cxxKinetics.o: src/phreeqcpp/cxxKinetics.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/cxxKinetics.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/cxxKinetics.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/cxxKinetics.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/cxxKinetics.d

# Compiles file src/phreeqcpp/cxxMix.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/cxxMix.d
x64/gccDebug/src/phreeqcpp/cxxMix.o: src/phreeqcpp/cxxMix.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/cxxMix.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/cxxMix.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/cxxMix.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/cxxMix.d

# Compiles file src/phreeqcpp/dumper.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/dumper.d
x64/gccDebug/src/phreeqcpp/dumper.o: src/phreeqcpp/dumper.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/dumper.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/dumper.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/dumper.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/dumper.d

# Compiles file src/phreeqcpp/Exchange.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/Exchange.d
x64/gccDebug/src/phreeqcpp/Exchange.o: src/phreeqcpp/Exchange.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Exchange.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/Exchange.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Exchange.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/Exchange.d

# Compiles file src/phreeqcpp/ExchComp.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/ExchComp.d
x64/gccDebug/src/phreeqcpp/ExchComp.o: src/phreeqcpp/ExchComp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/ExchComp.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/ExchComp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/ExchComp.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/ExchComp.d

# Compiles file src/phreeqcpp/GasComp.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/GasComp.d
x64/gccDebug/src/phreeqcpp/GasComp.o: src/phreeqcpp/GasComp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/GasComp.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/GasComp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/GasComp.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/GasComp.d

# Compiles file src/phreeqcpp/GasPhase.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/GasPhase.d
x64/gccDebug/src/phreeqcpp/GasPhase.o: src/phreeqcpp/GasPhase.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/GasPhase.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/GasPhase.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/GasPhase.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/GasPhase.d

# Compiles file src/phreeqcpp/ISolution.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/ISolution.d
x64/gccDebug/src/phreeqcpp/ISolution.o: src/phreeqcpp/ISolution.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/ISolution.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/ISolution.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/ISolution.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/ISolution.d

# Compiles file src/phreeqcpp/ISolutionComp.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/ISolutionComp.d
x64/gccDebug/src/phreeqcpp/ISolutionComp.o: src/phreeqcpp/ISolutionComp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/ISolutionComp.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/ISolutionComp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/ISolutionComp.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/ISolutionComp.d

# Compiles file src/phreeqcpp/Keywords.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/Keywords.d
x64/gccDebug/src/phreeqcpp/Keywords.o: src/phreeqcpp/Keywords.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Keywords.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/Keywords.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Keywords.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/Keywords.d

# Compiles file src/phreeqcpp/KineticsComp.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/KineticsComp.d
x64/gccDebug/src/phreeqcpp/KineticsComp.o: src/phreeqcpp/KineticsComp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/KineticsComp.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/KineticsComp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/KineticsComp.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/KineticsComp.d

# Compiles file src/phreeqcpp/NameDouble.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/NameDouble.d
x64/gccDebug/src/phreeqcpp/NameDouble.o: src/phreeqcpp/NameDouble.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/NameDouble.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/NameDouble.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/NameDouble.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/NameDouble.d

# Compiles file src/phreeqcpp/NumKeyword.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/NumKeyword.d
x64/gccDebug/src/phreeqcpp/NumKeyword.o: src/phreeqcpp/NumKeyword.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/NumKeyword.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/NumKeyword.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/NumKeyword.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/NumKeyword.d

# Compiles file src/phreeqcpp/Parser.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/Parser.d
x64/gccDebug/src/phreeqcpp/Parser.o: src/phreeqcpp/Parser.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Parser.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/Parser.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Parser.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/Parser.d

# Compiles file src/phreeqcpp/PBasic.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/PBasic.d
x64/gccDebug/src/phreeqcpp/PBasic.o: src/phreeqcpp/PBasic.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/PBasic.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/PBasic.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/PBasic.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/PBasic.d

# Compiles file src/phreeqcpp/Phreeqc.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/Phreeqc.d
x64/gccDebug/src/phreeqcpp/Phreeqc.o: src/phreeqcpp/Phreeqc.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Phreeqc.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/Phreeqc.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Phreeqc.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/Phreeqc.d

# Compiles file src/phreeqcpp/PHRQ_base.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/PHRQ_base.d
x64/gccDebug/src/phreeqcpp/PHRQ_base.o: src/phreeqcpp/PHRQ_base.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/PHRQ_base.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/PHRQ_base.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/PHRQ_base.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/PHRQ_base.d

# Compiles file src/phreeqcpp/PHRQ_io.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/PHRQ_io.d
x64/gccDebug/src/phreeqcpp/PHRQ_io.o: src/phreeqcpp/PHRQ_io.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/PHRQ_io.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/PHRQ_io.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/PHRQ_io.d

# Compiles file src/phreeqcpp/PPassemblage.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/PPassemblage.d
x64/gccDebug/src/phreeqcpp/PPassemblage.o: src/phreeqcpp/PPassemblage.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/PPassemblage.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/PPassemblage.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/PPassemblage.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/PPassemblage.d

# Compiles file src/phreeqcpp/PPassemblageComp.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/PPassemblageComp.d
x64/gccDebug/src/phreeqcpp/PPassemblageComp.o: src/phreeqcpp/PPassemblageComp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/PPassemblageComp.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/PPassemblageComp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/PPassemblageComp.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/PPassemblageComp.d

# Compiles file src/phreeqcpp/Pressure.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/Pressure.d
x64/gccDebug/src/phreeqcpp/Pressure.o: src/phreeqcpp/Pressure.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Pressure.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/Pressure.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Pressure.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/Pressure.d

# Compiles file src/phreeqcpp/Reaction.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/Reaction.d
x64/gccDebug/src/phreeqcpp/Reaction.o: src/phreeqcpp/Reaction.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Reaction.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/Reaction.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Reaction.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/Reaction.d

# Compiles file src/phreeqcpp/ReadClass.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/ReadClass.d
x64/gccDebug/src/phreeqcpp/ReadClass.o: src/phreeqcpp/ReadClass.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/ReadClass.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/ReadClass.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/ReadClass.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/ReadClass.d

# Compiles file src/phreeqcpp/runner.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/runner.d
x64/gccDebug/src/phreeqcpp/runner.o: src/phreeqcpp/runner.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/runner.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/runner.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/runner.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/runner.d

# Compiles file src/phreeqcpp/Solution.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/Solution.d
x64/gccDebug/src/phreeqcpp/Solution.o: src/phreeqcpp/Solution.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Solution.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/Solution.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Solution.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/Solution.d

# Compiles file src/phreeqcpp/SolutionIsotope.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/SolutionIsotope.d
x64/gccDebug/src/phreeqcpp/SolutionIsotope.o: src/phreeqcpp/SolutionIsotope.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SolutionIsotope.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/SolutionIsotope.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SolutionIsotope.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/SolutionIsotope.d

# Compiles file src/phreeqcpp/SS.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/SS.d
x64/gccDebug/src/phreeqcpp/SS.o: src/phreeqcpp/SS.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SS.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/SS.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SS.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/SS.d

# Compiles file src/phreeqcpp/SSassemblage.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/SSassemblage.d
x64/gccDebug/src/phreeqcpp/SSassemblage.o: src/phreeqcpp/SSassemblage.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SSassemblage.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/SSassemblage.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SSassemblage.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/SSassemblage.d

# Compiles file src/phreeqcpp/SScomp.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/SScomp.d
x64/gccDebug/src/phreeqcpp/SScomp.o: src/phreeqcpp/SScomp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SScomp.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/SScomp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SScomp.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/SScomp.d

# Compiles file src/phreeqcpp/StorageBin.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/StorageBin.d
x64/gccDebug/src/phreeqcpp/StorageBin.o: src/phreeqcpp/StorageBin.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/StorageBin.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/StorageBin.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/StorageBin.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/StorageBin.d

# Compiles file src/phreeqcpp/StorageBinList.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/StorageBinList.d
x64/gccDebug/src/phreeqcpp/StorageBinList.o: src/phreeqcpp/StorageBinList.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/StorageBinList.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/StorageBinList.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/StorageBinList.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/StorageBinList.d

# Compiles file src/phreeqcpp/Surface.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/Surface.d
x64/gccDebug/src/phreeqcpp/Surface.o: src/phreeqcpp/Surface.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Surface.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/Surface.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Surface.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/Surface.d

# Compiles file src/phreeqcpp/SurfaceCharge.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/SurfaceCharge.d
x64/gccDebug/src/phreeqcpp/SurfaceCharge.o: src/phreeqcpp/SurfaceCharge.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SurfaceCharge.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/SurfaceCharge.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SurfaceCharge.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/SurfaceCharge.d

# Compiles file src/phreeqcpp/SurfaceComp.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/SurfaceComp.d
x64/gccDebug/src/phreeqcpp/SurfaceComp.o: src/phreeqcpp/SurfaceComp.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/SurfaceComp.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/SurfaceComp.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/SurfaceComp.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/SurfaceComp.d

# Compiles file src/phreeqcpp/System.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/System.d
x64/gccDebug/src/phreeqcpp/System.o: src/phreeqcpp/System.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/System.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/System.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/System.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/System.d

# Compiles file src/phreeqcpp/Temperature.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/Temperature.d
x64/gccDebug/src/phreeqcpp/Temperature.o: src/phreeqcpp/Temperature.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Temperature.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/Temperature.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Temperature.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/Temperature.d

# Compiles file src/phreeqcpp/Use.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/Use.d
x64/gccDebug/src/phreeqcpp/Use.o: src/phreeqcpp/Use.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Use.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/Use.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Use.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/Use.d

# Compiles file src/phreeqcpp/Utils.cxx for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/Utils.d
x64/gccDebug/src/phreeqcpp/Utils.o: src/phreeqcpp/Utils.cxx
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/Utils.cxx $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/Utils.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/Utils.cxx $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/Utils.d

# Compiles file src/phreeqcpp/advection.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/advection.d
x64/gccDebug/src/phreeqcpp/advection.o: src/phreeqcpp/advection.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/advection.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/advection.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/advection.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/advection.d

# Compiles file src/phreeqcpp/basicsubs.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/basicsubs.d
x64/gccDebug/src/phreeqcpp/basicsubs.o: src/phreeqcpp/basicsubs.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/basicsubs.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/basicsubs.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/basicsubs.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/basicsubs.d

# Compiles file src/phreeqcpp/cl1.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/cl1.d
x64/gccDebug/src/phreeqcpp/cl1.o: src/phreeqcpp/cl1.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/cl1.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/cl1.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/cl1.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/cl1.d

# Compiles file src/phreeqcpp/cvdense.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/cvdense.d
x64/gccDebug/src/phreeqcpp/cvdense.o: src/phreeqcpp/cvdense.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/cvdense.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/cvdense.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/cvdense.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/cvdense.d

# Compiles file src/phreeqcpp/cvode.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/cvode.d
x64/gccDebug/src/phreeqcpp/cvode.o: src/phreeqcpp/cvode.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/cvode.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/cvode.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/cvode.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/cvode.d

# Compiles file src/phreeqcpp/dense.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/dense.d
x64/gccDebug/src/phreeqcpp/dense.o: src/phreeqcpp/dense.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/dense.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/dense.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/dense.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/dense.d

# Compiles file src/phreeqcpp/dw.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/dw.d
x64/gccDebug/src/phreeqcpp/dw.o: src/phreeqcpp/dw.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/dw.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/dw.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/dw.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/dw.d

# Compiles file src/phreeqcpp/gases.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/gases.d
x64/gccDebug/src/phreeqcpp/gases.o: src/phreeqcpp/gases.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/gases.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/gases.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/gases.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/gases.d

# Compiles file src/phreeqcpp/input.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/input.d
x64/gccDebug/src/phreeqcpp/input.o: src/phreeqcpp/input.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/input.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/input.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/input.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/input.d

# Compiles file src/phreeqcpp/integrate.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/integrate.d
x64/gccDebug/src/phreeqcpp/integrate.o: src/phreeqcpp/integrate.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/integrate.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/integrate.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/integrate.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/integrate.d

# Compiles file src/phreeqcpp/inverse.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/inverse.d
x64/gccDebug/src/phreeqcpp/inverse.o: src/phreeqcpp/inverse.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/inverse.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/inverse.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/inverse.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/inverse.d

# Compiles file src/phreeqcpp/isotopes.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/isotopes.d
x64/gccDebug/src/phreeqcpp/isotopes.o: src/phreeqcpp/isotopes.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/isotopes.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/isotopes.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/isotopes.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/isotopes.d

# Compiles file src/phreeqcpp/kinetics.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/kinetics.d
x64/gccDebug/src/phreeqcpp/kinetics.o: src/phreeqcpp/kinetics.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/kinetics.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/kinetics.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/kinetics.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/kinetics.d

# Compiles file src/phreeqcpp/mainsubs.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/mainsubs.d
x64/gccDebug/src/phreeqcpp/mainsubs.o: src/phreeqcpp/mainsubs.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/mainsubs.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/mainsubs.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/mainsubs.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/mainsubs.d

# Compiles file src/phreeqcpp/model.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/model.d
x64/gccDebug/src/phreeqcpp/model.o: src/phreeqcpp/model.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/model.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/model.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/model.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/model.d

# Compiles file src/phreeqcpp/nvector.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/nvector.d
x64/gccDebug/src/phreeqcpp/nvector.o: src/phreeqcpp/nvector.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/nvector.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/nvector.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/nvector.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/nvector.d

# Compiles file src/phreeqcpp/nvector_serial.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/nvector_serial.d
x64/gccDebug/src/phreeqcpp/nvector_serial.o: src/phreeqcpp/nvector_serial.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/nvector_serial.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/nvector_serial.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/nvector_serial.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/nvector_serial.d

# Compiles file src/phreeqcpp/parse.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/parse.d
x64/gccDebug/src/phreeqcpp/parse.o: src/phreeqcpp/parse.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/parse.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/parse.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/parse.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/parse.d

# Compiles file src/phreeqcpp/phqalloc.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/phqalloc.d
x64/gccDebug/src/phreeqcpp/phqalloc.o: src/phreeqcpp/phqalloc.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/phqalloc.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/phqalloc.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/phqalloc.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/phqalloc.d

# Compiles file src/phreeqcpp/PHRQ_io_output.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/PHRQ_io_output.d
x64/gccDebug/src/phreeqcpp/PHRQ_io_output.o: src/phreeqcpp/PHRQ_io_output.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/PHRQ_io_output.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/PHRQ_io_output.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io_output.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/PHRQ_io_output.d

# Compiles file src/phreeqcpp/pitzer.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/pitzer.d
x64/gccDebug/src/phreeqcpp/pitzer.o: src/phreeqcpp/pitzer.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/pitzer.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/pitzer.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/pitzer.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/pitzer.d

# Compiles file src/phreeqcpp/pitzer_structures.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/pitzer_structures.d
x64/gccDebug/src/phreeqcpp/pitzer_structures.o: src/phreeqcpp/pitzer_structures.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/pitzer_structures.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/pitzer_structures.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/pitzer_structures.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/pitzer_structures.d

# Compiles file src/phreeqcpp/prep.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/prep.d
x64/gccDebug/src/phreeqcpp/prep.o: src/phreeqcpp/prep.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/prep.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/prep.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/prep.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/prep.d

# Compiles file src/phreeqcpp/print.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/print.d
x64/gccDebug/src/phreeqcpp/print.o: src/phreeqcpp/print.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/print.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/print.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/print.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/print.d

# Compiles file src/phreeqcpp/read.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/read.d
x64/gccDebug/src/phreeqcpp/read.o: src/phreeqcpp/read.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/read.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/read.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/read.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/read.d

# Compiles file src/phreeqcpp/readtr.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/readtr.d
x64/gccDebug/src/phreeqcpp/readtr.o: src/phreeqcpp/readtr.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/readtr.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/readtr.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/readtr.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/readtr.d

# Compiles file src/phreeqcpp/sit.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/sit.d
x64/gccDebug/src/phreeqcpp/sit.o: src/phreeqcpp/sit.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/sit.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/sit.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/sit.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/sit.d

# Compiles file src/phreeqcpp/smalldense.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/smalldense.d
x64/gccDebug/src/phreeqcpp/smalldense.o: src/phreeqcpp/smalldense.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/smalldense.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/smalldense.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/smalldense.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/smalldense.d

# Compiles file src/phreeqcpp/spread.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/spread.d
x64/gccDebug/src/phreeqcpp/spread.o: src/phreeqcpp/spread.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/spread.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/spread.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/spread.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/spread.d

# Compiles file src/phreeqcpp/step.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/step.d
x64/gccDebug/src/phreeqcpp/step.o: src/phreeqcpp/step.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/step.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/step.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/step.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/step.d

# Compiles file src/phreeqcpp/structures.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/structures.d
x64/gccDebug/src/phreeqcpp/structures.o: src/phreeqcpp/structures.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/structures.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/structures.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/structures.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/structures.d

# Compiles file src/phreeqcpp/sundialsmath.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/sundialsmath.d
x64/gccDebug/src/phreeqcpp/sundialsmath.o: src/phreeqcpp/sundialsmath.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/sundialsmath.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/sundialsmath.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/sundialsmath.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/sundialsmath.d

# Compiles file src/phreeqcpp/tally.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/tally.d
x64/gccDebug/src/phreeqcpp/tally.o: src/phreeqcpp/tally.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/tally.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/tally.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/tally.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/tally.d

# Compiles file src/phreeqcpp/tidy.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/tidy.d
x64/gccDebug/src/phreeqcpp/tidy.o: src/phreeqcpp/tidy.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/tidy.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/tidy.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/tidy.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/tidy.d

# Compiles file src/phreeqcpp/transport.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/transport.d
x64/gccDebug/src/phreeqcpp/transport.o: src/phreeqcpp/transport.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/transport.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/transport.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/transport.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/transport.d

# Compiles file src/phreeqcpp/utilities.cpp for the Debug configuration...
-include x64/gccDebug/src/phreeqcpp/utilities.d
x64/gccDebug/src/phreeqcpp/utilities.o: src/phreeqcpp/utilities.cpp
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -c src/phreeqcpp/utilities.cpp $(Debug_Include_Path) -o x64/gccDebug/src/phreeqcpp/utilities.o
	$(CPP_COMPILER) $(Debug_Preprocessor_Definitions) $(Debug_Compiler_Flags) -MM src/phreeqcpp/utilities.cpp $(Debug_Include_Path) > x64/gccDebug/src/phreeqcpp/utilities.d

# Builds the ReleaseDll configuration...
.PHONY: ReleaseDll
ReleaseDll: create_folders gccReleaseDll/src/fwrap.o gccReleaseDll/src/fwrap2.o gccReleaseDll/src/fwrap3.o gccReleaseDll/src/fwrap4.o gccReleaseDll/src/fwrap5.o gccReleaseDll/src/fwrap6.o gccReleaseDll/src/fwrap7.o gccReleaseDll/src/fwrap8.o gccReleaseDll/src/IPhreeqc.o gccReleaseDll/src/IPhreeqcLib.o gccReleaseDll/src/Var.o gccReleaseDll/src/CSelectedOutput.o gccReleaseDll/src/phreeqcpp/SelectedOutput.o gccReleaseDll/src/phreeqcpp/UserPunch.o gccReleaseDll/src/phreeqcpp/cxxKinetics.o gccReleaseDll/src/phreeqcpp/cxxMix.o gccReleaseDll/src/phreeqcpp/dumper.o gccReleaseDll/src/phreeqcpp/Exchange.o gccReleaseDll/src/phreeqcpp/ExchComp.o gccReleaseDll/src/phreeqcpp/GasComp.o gccReleaseDll/src/phreeqcpp/GasPhase.o gccReleaseDll/src/phreeqcpp/ISolution.o gccReleaseDll/src/phreeqcpp/ISolutionComp.o gccReleaseDll/src/phreeqcpp/Keywords.o gccReleaseDll/src/phreeqcpp/KineticsComp.o gccReleaseDll/src/phreeqcpp/NameDouble.o gccReleaseDll/src/phreeqcpp/NumKeyword.o gccReleaseDll/src/phreeqcpp/Parser.o gccReleaseDll/src/phreeqcpp/PBasic.o gccReleaseDll/src/phreeqcpp/Phreeqc.o gccReleaseDll/src/phreeqcpp/PHRQ_base.o gccReleaseDll/src/phreeqcpp/PHRQ_io.o gccReleaseDll/src/phreeqcpp/PPassemblage.o gccReleaseDll/src/phreeqcpp/PPassemblageComp.o gccReleaseDll/src/phreeqcpp/Pressure.o gccReleaseDll/src/phreeqcpp/Reaction.o gccReleaseDll/src/phreeqcpp/ReadClass.o gccReleaseDll/src/phreeqcpp/runner.o gccReleaseDll/src/phreeqcpp/Solution.o gccReleaseDll/src/phreeqcpp/SolutionIsotope.o gccReleaseDll/src/phreeqcpp/SS.o gccReleaseDll/src/phreeqcpp/SSassemblage.o gccReleaseDll/src/phreeqcpp/SScomp.o gccReleaseDll/src/phreeqcpp/StorageBin.o gccReleaseDll/src/phreeqcpp/StorageBinList.o gccReleaseDll/src/phreeqcpp/Surface.o gccReleaseDll/src/phreeqcpp/SurfaceCharge.o gccReleaseDll/src/phreeqcpp/SurfaceComp.o gccReleaseDll/src/phreeqcpp/System.o gccReleaseDll/src/phreeqcpp/Temperature.o gccReleaseDll/src/phreeqcpp/Use.o gccReleaseDll/src/phreeqcpp/Utils.o gccReleaseDll/src/phreeqcpp/advection.o gccReleaseDll/src/phreeqcpp/basicsubs.o gccReleaseDll/src/phreeqcpp/cl1.o gccReleaseDll/src/phreeqcpp/cvdense.o gccReleaseDll/src/phreeqcpp/cvode.o gccReleaseDll/src/phreeqcpp/dense.o gccReleaseDll/src/phreeqcpp/dw.o gccReleaseDll/src/phreeqcpp/gases.o gccReleaseDll/src/phreeqcpp/input.o gccReleaseDll/src/phreeqcpp/integrate.o gccReleaseDll/src/phreeqcpp/inverse.o gccReleaseDll/src/phreeqcpp/isotopes.o gccReleaseDll/src/phreeqcpp/kinetics.o gccReleaseDll/src/phreeqcpp/mainsubs.o gccReleaseDll/src/phreeqcpp/model.o gccReleaseDll/src/phreeqcpp/nvector.o gccReleaseDll/src/phreeqcpp/nvector_serial.o gccReleaseDll/src/phreeqcpp/parse.o gccReleaseDll/src/phreeqcpp/phqalloc.o gccReleaseDll/src/phreeqcpp/PHRQ_io_output.o gccReleaseDll/src/phreeqcpp/pitzer.o gccReleaseDll/src/phreeqcpp/pitzer_structures.o gccReleaseDll/src/phreeqcpp/prep.o gccReleaseDll/src/phreeqcpp/print.o gccReleaseDll/src/phreeqcpp/read.o gccReleaseDll/src/phreeqcpp/readtr.o gccReleaseDll/src/phreeqcpp/sit.o gccReleaseDll/src/phreeqcpp/smalldense.o gccReleaseDll/src/phreeqcpp/spread.o gccReleaseDll/src/phreeqcpp/step.o gccReleaseDll/src/phreeqcpp/structures.o gccReleaseDll/src/phreeqcpp/sundialsmath.o gccReleaseDll/src/phreeqcpp/tally.o gccReleaseDll/src/phreeqcpp/tidy.o gccReleaseDll/src/phreeqcpp/transport.o gccReleaseDll/src/phreeqcpp/utilities.o 
	ar rcs ../../gccReleaseDll/libIPhreeqc.a gccReleaseDll/src/fwrap.o gccReleaseDll/src/fwrap2.o gccReleaseDll/src/fwrap3.o gccReleaseDll/src/fwrap4.o gccReleaseDll/src/fwrap5.o gccReleaseDll/src/fwrap6.o gccReleaseDll/src/fwrap7.o gccReleaseDll/src/fwrap8.o gccReleaseDll/src/IPhreeqc.o gccReleaseDll/src/IPhreeqcLib.o gccReleaseDll/src/Var.o gccReleaseDll/src/CSelectedOutput.o gccReleaseDll/src/phreeqcpp/SelectedOutput.o gccReleaseDll/src/phreeqcpp/UserPunch.o gccReleaseDll/src/phreeqcpp/cxxKinetics.o gccReleaseDll/src/phreeqcpp/cxxMix.o gccReleaseDll/src/phreeqcpp/dumper.o gccReleaseDll/src/phreeqcpp/Exchange.o gccReleaseDll/src/phreeqcpp/ExchComp.o gccReleaseDll/src/phreeqcpp/GasComp.o gccReleaseDll/src/phreeqcpp/GasPhase.o gccReleaseDll/src/phreeqcpp/ISolution.o gccReleaseDll/src/phreeqcpp/ISolutionComp.o gccReleaseDll/src/phreeqcpp/Keywords.o gccReleaseDll/src/phreeqcpp/KineticsComp.o gccReleaseDll/src/phreeqcpp/NameDouble.o gccReleaseDll/src/phreeqcpp/NumKeyword.o gccReleaseDll/src/phreeqcpp/Parser.o gccReleaseDll/src/phreeqcpp/PBasic.o gccReleaseDll/src/phreeqcpp/Phreeqc.o gccReleaseDll/src/phreeqcpp/PHRQ_base.o gccReleaseDll/src/phreeqcpp/PHRQ_io.o gccReleaseDll/src/phreeqcpp/PPassemblage.o gccReleaseDll/src/phreeqcpp/PPassemblageComp.o gccReleaseDll/src/phreeqcpp/Pressure.o gccReleaseDll/src/phreeqcpp/Reaction.o gccReleaseDll/src/phreeqcpp/ReadClass.o gccReleaseDll/src/phreeqcpp/runner.o gccReleaseDll/src/phreeqcpp/Solution.o gccReleaseDll/src/phreeqcpp/SolutionIsotope.o gccReleaseDll/src/phreeqcpp/SS.o gccReleaseDll/src/phreeqcpp/SSassemblage.o gccReleaseDll/src/phreeqcpp/SScomp.o gccReleaseDll/src/phreeqcpp/StorageBin.o gccReleaseDll/src/phreeqcpp/StorageBinList.o gccReleaseDll/src/phreeqcpp/Surface.o gccReleaseDll/src/phreeqcpp/SurfaceCharge.o gccReleaseDll/src/phreeqcpp/SurfaceComp.o gccReleaseDll/src/phreeqcpp/System.o gccReleaseDll/src/phreeqcpp/Temperature.o gccReleaseDll/src/phreeqcpp/Use.o gccReleaseDll/src/phreeqcpp/Utils.o gccReleaseDll/src/phreeqcpp/advection.o gccReleaseDll/src/phreeqcpp/basicsubs.o gccReleaseDll/src/phreeqcpp/cl1.o gccReleaseDll/src/phreeqcpp/cvdense.o gccReleaseDll/src/phreeqcpp/cvode.o gccReleaseDll/src/phreeqcpp/dense.o gccReleaseDll/src/phreeqcpp/dw.o gccReleaseDll/src/phreeqcpp/gases.o gccReleaseDll/src/phreeqcpp/input.o gccReleaseDll/src/phreeqcpp/integrate.o gccReleaseDll/src/phreeqcpp/inverse.o gccReleaseDll/src/phreeqcpp/isotopes.o gccReleaseDll/src/phreeqcpp/kinetics.o gccReleaseDll/src/phreeqcpp/mainsubs.o gccReleaseDll/src/phreeqcpp/model.o gccReleaseDll/src/phreeqcpp/nvector.o gccReleaseDll/src/phreeqcpp/nvector_serial.o gccReleaseDll/src/phreeqcpp/parse.o gccReleaseDll/src/phreeqcpp/phqalloc.o gccReleaseDll/src/phreeqcpp/PHRQ_io_output.o gccReleaseDll/src/phreeqcpp/pitzer.o gccReleaseDll/src/phreeqcpp/pitzer_structures.o gccReleaseDll/src/phreeqcpp/prep.o gccReleaseDll/src/phreeqcpp/print.o gccReleaseDll/src/phreeqcpp/read.o gccReleaseDll/src/phreeqcpp/readtr.o gccReleaseDll/src/phreeqcpp/sit.o gccReleaseDll/src/phreeqcpp/smalldense.o gccReleaseDll/src/phreeqcpp/spread.o gccReleaseDll/src/phreeqcpp/step.o gccReleaseDll/src/phreeqcpp/structures.o gccReleaseDll/src/phreeqcpp/sundialsmath.o gccReleaseDll/src/phreeqcpp/tally.o gccReleaseDll/src/phreeqcpp/tidy.o gccReleaseDll/src/phreeqcpp/transport.o gccReleaseDll/src/phreeqcpp/utilities.o  $(ReleaseDll_Implicitly_Linked_Objects)

# Compiles file src/fwrap.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/fwrap.d
gccReleaseDll/src/fwrap.o: src/fwrap.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/fwrap.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/fwrap.d

# Compiles file src/fwrap2.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/fwrap2.d
gccReleaseDll/src/fwrap2.o: src/fwrap2.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap2.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/fwrap2.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap2.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/fwrap2.d

# Compiles file src/fwrap3.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/fwrap3.d
gccReleaseDll/src/fwrap3.o: src/fwrap3.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap3.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/fwrap3.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap3.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/fwrap3.d

# Compiles file src/fwrap4.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/fwrap4.d
gccReleaseDll/src/fwrap4.o: src/fwrap4.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap4.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/fwrap4.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap4.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/fwrap4.d

# Compiles file src/fwrap5.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/fwrap5.d
gccReleaseDll/src/fwrap5.o: src/fwrap5.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap5.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/fwrap5.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap5.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/fwrap5.d

# Compiles file src/fwrap6.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/fwrap6.d
gccReleaseDll/src/fwrap6.o: src/fwrap6.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap6.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/fwrap6.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap6.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/fwrap6.d

# Compiles file src/fwrap7.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/fwrap7.d
gccReleaseDll/src/fwrap7.o: src/fwrap7.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap7.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/fwrap7.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap7.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/fwrap7.d

# Compiles file src/fwrap8.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/fwrap8.d
gccReleaseDll/src/fwrap8.o: src/fwrap8.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap8.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/fwrap8.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap8.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/fwrap8.d

# Compiles file src/IPhreeqc.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/IPhreeqc.d
gccReleaseDll/src/IPhreeqc.o: src/IPhreeqc.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/IPhreeqc.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/IPhreeqc.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/IPhreeqc.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/IPhreeqc.d

# Compiles file src/IPhreeqcLib.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/IPhreeqcLib.d
gccReleaseDll/src/IPhreeqcLib.o: src/IPhreeqcLib.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/IPhreeqcLib.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/IPhreeqcLib.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/IPhreeqcLib.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/IPhreeqcLib.d

# Compiles file src/Var.c for the ReleaseDll configuration...
-include gccReleaseDll/src/Var.d
gccReleaseDll/src/Var.o: src/Var.c
	$(C_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/Var.c $(ReleaseDll_Include_Path) -o gccReleaseDll/src/Var.o
	$(C_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/Var.c $(ReleaseDll_Include_Path) > gccReleaseDll/src/Var.d

# Compiles file src/CSelectedOutput.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/CSelectedOutput.d
gccReleaseDll/src/CSelectedOutput.o: src/CSelectedOutput.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/CSelectedOutput.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/CSelectedOutput.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/CSelectedOutput.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/CSelectedOutput.d

# Compiles file src/phreeqcpp/SelectedOutput.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/SelectedOutput.d
gccReleaseDll/src/phreeqcpp/SelectedOutput.o: src/phreeqcpp/SelectedOutput.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SelectedOutput.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/SelectedOutput.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SelectedOutput.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/SelectedOutput.d

# Compiles file src/phreeqcpp/UserPunch.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/UserPunch.d
gccReleaseDll/src/phreeqcpp/UserPunch.o: src/phreeqcpp/UserPunch.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/UserPunch.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/UserPunch.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/UserPunch.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/UserPunch.d

# Compiles file src/phreeqcpp/cxxKinetics.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/cxxKinetics.d
gccReleaseDll/src/phreeqcpp/cxxKinetics.o: src/phreeqcpp/cxxKinetics.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/cxxKinetics.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/cxxKinetics.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/cxxKinetics.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/cxxKinetics.d

# Compiles file src/phreeqcpp/cxxMix.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/cxxMix.d
gccReleaseDll/src/phreeqcpp/cxxMix.o: src/phreeqcpp/cxxMix.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/cxxMix.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/cxxMix.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/cxxMix.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/cxxMix.d

# Compiles file src/phreeqcpp/dumper.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/dumper.d
gccReleaseDll/src/phreeqcpp/dumper.o: src/phreeqcpp/dumper.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/dumper.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/dumper.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/dumper.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/dumper.d

# Compiles file src/phreeqcpp/Exchange.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/Exchange.d
gccReleaseDll/src/phreeqcpp/Exchange.o: src/phreeqcpp/Exchange.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Exchange.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/Exchange.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Exchange.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/Exchange.d

# Compiles file src/phreeqcpp/ExchComp.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/ExchComp.d
gccReleaseDll/src/phreeqcpp/ExchComp.o: src/phreeqcpp/ExchComp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/ExchComp.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/ExchComp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/ExchComp.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/ExchComp.d

# Compiles file src/phreeqcpp/GasComp.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/GasComp.d
gccReleaseDll/src/phreeqcpp/GasComp.o: src/phreeqcpp/GasComp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/GasComp.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/GasComp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/GasComp.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/GasComp.d

# Compiles file src/phreeqcpp/GasPhase.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/GasPhase.d
gccReleaseDll/src/phreeqcpp/GasPhase.o: src/phreeqcpp/GasPhase.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/GasPhase.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/GasPhase.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/GasPhase.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/GasPhase.d

# Compiles file src/phreeqcpp/ISolution.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/ISolution.d
gccReleaseDll/src/phreeqcpp/ISolution.o: src/phreeqcpp/ISolution.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/ISolution.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/ISolution.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/ISolution.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/ISolution.d

# Compiles file src/phreeqcpp/ISolutionComp.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/ISolutionComp.d
gccReleaseDll/src/phreeqcpp/ISolutionComp.o: src/phreeqcpp/ISolutionComp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/ISolutionComp.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/ISolutionComp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/ISolutionComp.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/ISolutionComp.d

# Compiles file src/phreeqcpp/Keywords.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/Keywords.d
gccReleaseDll/src/phreeqcpp/Keywords.o: src/phreeqcpp/Keywords.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Keywords.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/Keywords.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Keywords.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/Keywords.d

# Compiles file src/phreeqcpp/KineticsComp.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/KineticsComp.d
gccReleaseDll/src/phreeqcpp/KineticsComp.o: src/phreeqcpp/KineticsComp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/KineticsComp.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/KineticsComp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/KineticsComp.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/KineticsComp.d

# Compiles file src/phreeqcpp/NameDouble.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/NameDouble.d
gccReleaseDll/src/phreeqcpp/NameDouble.o: src/phreeqcpp/NameDouble.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/NameDouble.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/NameDouble.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/NameDouble.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/NameDouble.d

# Compiles file src/phreeqcpp/NumKeyword.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/NumKeyword.d
gccReleaseDll/src/phreeqcpp/NumKeyword.o: src/phreeqcpp/NumKeyword.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/NumKeyword.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/NumKeyword.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/NumKeyword.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/NumKeyword.d

# Compiles file src/phreeqcpp/Parser.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/Parser.d
gccReleaseDll/src/phreeqcpp/Parser.o: src/phreeqcpp/Parser.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Parser.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/Parser.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Parser.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/Parser.d

# Compiles file src/phreeqcpp/PBasic.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/PBasic.d
gccReleaseDll/src/phreeqcpp/PBasic.o: src/phreeqcpp/PBasic.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/PBasic.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/PBasic.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/PBasic.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/PBasic.d

# Compiles file src/phreeqcpp/Phreeqc.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/Phreeqc.d
gccReleaseDll/src/phreeqcpp/Phreeqc.o: src/phreeqcpp/Phreeqc.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Phreeqc.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/Phreeqc.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Phreeqc.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/Phreeqc.d

# Compiles file src/phreeqcpp/PHRQ_base.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/PHRQ_base.d
gccReleaseDll/src/phreeqcpp/PHRQ_base.o: src/phreeqcpp/PHRQ_base.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/PHRQ_base.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/PHRQ_base.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/PHRQ_base.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/PHRQ_base.d

# Compiles file src/phreeqcpp/PHRQ_io.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/PHRQ_io.d
gccReleaseDll/src/phreeqcpp/PHRQ_io.o: src/phreeqcpp/PHRQ_io.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/PHRQ_io.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/PHRQ_io.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/PHRQ_io.d

# Compiles file src/phreeqcpp/PPassemblage.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/PPassemblage.d
gccReleaseDll/src/phreeqcpp/PPassemblage.o: src/phreeqcpp/PPassemblage.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/PPassemblage.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/PPassemblage.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/PPassemblage.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/PPassemblage.d

# Compiles file src/phreeqcpp/PPassemblageComp.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/PPassemblageComp.d
gccReleaseDll/src/phreeqcpp/PPassemblageComp.o: src/phreeqcpp/PPassemblageComp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/PPassemblageComp.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/PPassemblageComp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/PPassemblageComp.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/PPassemblageComp.d

# Compiles file src/phreeqcpp/Pressure.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/Pressure.d
gccReleaseDll/src/phreeqcpp/Pressure.o: src/phreeqcpp/Pressure.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Pressure.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/Pressure.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Pressure.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/Pressure.d

# Compiles file src/phreeqcpp/Reaction.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/Reaction.d
gccReleaseDll/src/phreeqcpp/Reaction.o: src/phreeqcpp/Reaction.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Reaction.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/Reaction.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Reaction.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/Reaction.d

# Compiles file src/phreeqcpp/ReadClass.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/ReadClass.d
gccReleaseDll/src/phreeqcpp/ReadClass.o: src/phreeqcpp/ReadClass.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/ReadClass.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/ReadClass.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/ReadClass.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/ReadClass.d

# Compiles file src/phreeqcpp/runner.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/runner.d
gccReleaseDll/src/phreeqcpp/runner.o: src/phreeqcpp/runner.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/runner.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/runner.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/runner.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/runner.d

# Compiles file src/phreeqcpp/Solution.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/Solution.d
gccReleaseDll/src/phreeqcpp/Solution.o: src/phreeqcpp/Solution.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Solution.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/Solution.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Solution.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/Solution.d

# Compiles file src/phreeqcpp/SolutionIsotope.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/SolutionIsotope.d
gccReleaseDll/src/phreeqcpp/SolutionIsotope.o: src/phreeqcpp/SolutionIsotope.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SolutionIsotope.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/SolutionIsotope.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SolutionIsotope.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/SolutionIsotope.d

# Compiles file src/phreeqcpp/SS.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/SS.d
gccReleaseDll/src/phreeqcpp/SS.o: src/phreeqcpp/SS.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SS.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/SS.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SS.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/SS.d

# Compiles file src/phreeqcpp/SSassemblage.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/SSassemblage.d
gccReleaseDll/src/phreeqcpp/SSassemblage.o: src/phreeqcpp/SSassemblage.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SSassemblage.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/SSassemblage.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SSassemblage.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/SSassemblage.d

# Compiles file src/phreeqcpp/SScomp.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/SScomp.d
gccReleaseDll/src/phreeqcpp/SScomp.o: src/phreeqcpp/SScomp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SScomp.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/SScomp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SScomp.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/SScomp.d

# Compiles file src/phreeqcpp/StorageBin.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/StorageBin.d
gccReleaseDll/src/phreeqcpp/StorageBin.o: src/phreeqcpp/StorageBin.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/StorageBin.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/StorageBin.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/StorageBin.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/StorageBin.d

# Compiles file src/phreeqcpp/StorageBinList.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/StorageBinList.d
gccReleaseDll/src/phreeqcpp/StorageBinList.o: src/phreeqcpp/StorageBinList.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/StorageBinList.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/StorageBinList.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/StorageBinList.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/StorageBinList.d

# Compiles file src/phreeqcpp/Surface.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/Surface.d
gccReleaseDll/src/phreeqcpp/Surface.o: src/phreeqcpp/Surface.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Surface.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/Surface.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Surface.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/Surface.d

# Compiles file src/phreeqcpp/SurfaceCharge.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/SurfaceCharge.d
gccReleaseDll/src/phreeqcpp/SurfaceCharge.o: src/phreeqcpp/SurfaceCharge.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SurfaceCharge.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/SurfaceCharge.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SurfaceCharge.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/SurfaceCharge.d

# Compiles file src/phreeqcpp/SurfaceComp.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/SurfaceComp.d
gccReleaseDll/src/phreeqcpp/SurfaceComp.o: src/phreeqcpp/SurfaceComp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SurfaceComp.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/SurfaceComp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SurfaceComp.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/SurfaceComp.d

# Compiles file src/phreeqcpp/System.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/System.d
gccReleaseDll/src/phreeqcpp/System.o: src/phreeqcpp/System.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/System.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/System.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/System.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/System.d

# Compiles file src/phreeqcpp/Temperature.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/Temperature.d
gccReleaseDll/src/phreeqcpp/Temperature.o: src/phreeqcpp/Temperature.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Temperature.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/Temperature.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Temperature.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/Temperature.d

# Compiles file src/phreeqcpp/Use.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/Use.d
gccReleaseDll/src/phreeqcpp/Use.o: src/phreeqcpp/Use.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Use.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/Use.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Use.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/Use.d

# Compiles file src/phreeqcpp/Utils.cxx for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/Utils.d
gccReleaseDll/src/phreeqcpp/Utils.o: src/phreeqcpp/Utils.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Utils.cxx $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/Utils.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Utils.cxx $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/Utils.d

# Compiles file src/phreeqcpp/advection.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/advection.d
gccReleaseDll/src/phreeqcpp/advection.o: src/phreeqcpp/advection.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/advection.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/advection.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/advection.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/advection.d

# Compiles file src/phreeqcpp/basicsubs.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/basicsubs.d
gccReleaseDll/src/phreeqcpp/basicsubs.o: src/phreeqcpp/basicsubs.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/basicsubs.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/basicsubs.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/basicsubs.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/basicsubs.d

# Compiles file src/phreeqcpp/cl1.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/cl1.d
gccReleaseDll/src/phreeqcpp/cl1.o: src/phreeqcpp/cl1.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/cl1.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/cl1.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/cl1.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/cl1.d

# Compiles file src/phreeqcpp/cvdense.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/cvdense.d
gccReleaseDll/src/phreeqcpp/cvdense.o: src/phreeqcpp/cvdense.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/cvdense.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/cvdense.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/cvdense.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/cvdense.d

# Compiles file src/phreeqcpp/cvode.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/cvode.d
gccReleaseDll/src/phreeqcpp/cvode.o: src/phreeqcpp/cvode.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/cvode.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/cvode.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/cvode.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/cvode.d

# Compiles file src/phreeqcpp/dense.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/dense.d
gccReleaseDll/src/phreeqcpp/dense.o: src/phreeqcpp/dense.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/dense.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/dense.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/dense.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/dense.d

# Compiles file src/phreeqcpp/dw.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/dw.d
gccReleaseDll/src/phreeqcpp/dw.o: src/phreeqcpp/dw.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/dw.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/dw.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/dw.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/dw.d

# Compiles file src/phreeqcpp/gases.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/gases.d
gccReleaseDll/src/phreeqcpp/gases.o: src/phreeqcpp/gases.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/gases.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/gases.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/gases.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/gases.d

# Compiles file src/phreeqcpp/input.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/input.d
gccReleaseDll/src/phreeqcpp/input.o: src/phreeqcpp/input.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/input.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/input.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/input.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/input.d

# Compiles file src/phreeqcpp/integrate.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/integrate.d
gccReleaseDll/src/phreeqcpp/integrate.o: src/phreeqcpp/integrate.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/integrate.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/integrate.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/integrate.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/integrate.d

# Compiles file src/phreeqcpp/inverse.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/inverse.d
gccReleaseDll/src/phreeqcpp/inverse.o: src/phreeqcpp/inverse.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/inverse.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/inverse.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/inverse.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/inverse.d

# Compiles file src/phreeqcpp/isotopes.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/isotopes.d
gccReleaseDll/src/phreeqcpp/isotopes.o: src/phreeqcpp/isotopes.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/isotopes.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/isotopes.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/isotopes.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/isotopes.d

# Compiles file src/phreeqcpp/kinetics.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/kinetics.d
gccReleaseDll/src/phreeqcpp/kinetics.o: src/phreeqcpp/kinetics.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/kinetics.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/kinetics.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/kinetics.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/kinetics.d

# Compiles file src/phreeqcpp/mainsubs.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/mainsubs.d
gccReleaseDll/src/phreeqcpp/mainsubs.o: src/phreeqcpp/mainsubs.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/mainsubs.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/mainsubs.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/mainsubs.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/mainsubs.d

# Compiles file src/phreeqcpp/model.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/model.d
gccReleaseDll/src/phreeqcpp/model.o: src/phreeqcpp/model.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/model.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/model.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/model.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/model.d

# Compiles file src/phreeqcpp/nvector.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/nvector.d
gccReleaseDll/src/phreeqcpp/nvector.o: src/phreeqcpp/nvector.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/nvector.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/nvector.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/nvector.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/nvector.d

# Compiles file src/phreeqcpp/nvector_serial.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/nvector_serial.d
gccReleaseDll/src/phreeqcpp/nvector_serial.o: src/phreeqcpp/nvector_serial.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/nvector_serial.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/nvector_serial.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/nvector_serial.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/nvector_serial.d

# Compiles file src/phreeqcpp/parse.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/parse.d
gccReleaseDll/src/phreeqcpp/parse.o: src/phreeqcpp/parse.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/parse.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/parse.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/parse.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/parse.d

# Compiles file src/phreeqcpp/phqalloc.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/phqalloc.d
gccReleaseDll/src/phreeqcpp/phqalloc.o: src/phreeqcpp/phqalloc.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/phqalloc.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/phqalloc.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/phqalloc.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/phqalloc.d

# Compiles file src/phreeqcpp/PHRQ_io_output.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/PHRQ_io_output.d
gccReleaseDll/src/phreeqcpp/PHRQ_io_output.o: src/phreeqcpp/PHRQ_io_output.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/PHRQ_io_output.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/PHRQ_io_output.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io_output.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/PHRQ_io_output.d

# Compiles file src/phreeqcpp/pitzer.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/pitzer.d
gccReleaseDll/src/phreeqcpp/pitzer.o: src/phreeqcpp/pitzer.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/pitzer.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/pitzer.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/pitzer.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/pitzer.d

# Compiles file src/phreeqcpp/pitzer_structures.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/pitzer_structures.d
gccReleaseDll/src/phreeqcpp/pitzer_structures.o: src/phreeqcpp/pitzer_structures.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/pitzer_structures.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/pitzer_structures.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/pitzer_structures.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/pitzer_structures.d

# Compiles file src/phreeqcpp/prep.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/prep.d
gccReleaseDll/src/phreeqcpp/prep.o: src/phreeqcpp/prep.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/prep.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/prep.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/prep.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/prep.d

# Compiles file src/phreeqcpp/print.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/print.d
gccReleaseDll/src/phreeqcpp/print.o: src/phreeqcpp/print.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/print.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/print.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/print.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/print.d

# Compiles file src/phreeqcpp/read.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/read.d
gccReleaseDll/src/phreeqcpp/read.o: src/phreeqcpp/read.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/read.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/read.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/read.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/read.d

# Compiles file src/phreeqcpp/readtr.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/readtr.d
gccReleaseDll/src/phreeqcpp/readtr.o: src/phreeqcpp/readtr.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/readtr.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/readtr.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/readtr.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/readtr.d

# Compiles file src/phreeqcpp/sit.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/sit.d
gccReleaseDll/src/phreeqcpp/sit.o: src/phreeqcpp/sit.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/sit.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/sit.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/sit.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/sit.d

# Compiles file src/phreeqcpp/smalldense.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/smalldense.d
gccReleaseDll/src/phreeqcpp/smalldense.o: src/phreeqcpp/smalldense.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/smalldense.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/smalldense.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/smalldense.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/smalldense.d

# Compiles file src/phreeqcpp/spread.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/spread.d
gccReleaseDll/src/phreeqcpp/spread.o: src/phreeqcpp/spread.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/spread.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/spread.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/spread.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/spread.d

# Compiles file src/phreeqcpp/step.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/step.d
gccReleaseDll/src/phreeqcpp/step.o: src/phreeqcpp/step.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/step.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/step.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/step.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/step.d

# Compiles file src/phreeqcpp/structures.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/structures.d
gccReleaseDll/src/phreeqcpp/structures.o: src/phreeqcpp/structures.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/structures.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/structures.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/structures.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/structures.d

# Compiles file src/phreeqcpp/sundialsmath.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/sundialsmath.d
gccReleaseDll/src/phreeqcpp/sundialsmath.o: src/phreeqcpp/sundialsmath.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/sundialsmath.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/sundialsmath.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/sundialsmath.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/sundialsmath.d

# Compiles file src/phreeqcpp/tally.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/tally.d
gccReleaseDll/src/phreeqcpp/tally.o: src/phreeqcpp/tally.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/tally.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/tally.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/tally.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/tally.d

# Compiles file src/phreeqcpp/tidy.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/tidy.d
gccReleaseDll/src/phreeqcpp/tidy.o: src/phreeqcpp/tidy.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/tidy.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/tidy.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/tidy.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/tidy.d

# Compiles file src/phreeqcpp/transport.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/transport.d
gccReleaseDll/src/phreeqcpp/transport.o: src/phreeqcpp/transport.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/transport.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/transport.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/transport.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/transport.d

# Compiles file src/phreeqcpp/utilities.cpp for the ReleaseDll configuration...
-include gccReleaseDll/src/phreeqcpp/utilities.d
gccReleaseDll/src/phreeqcpp/utilities.o: src/phreeqcpp/utilities.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/utilities.cpp $(ReleaseDll_Include_Path) -o gccReleaseDll/src/phreeqcpp/utilities.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/utilities.cpp $(ReleaseDll_Include_Path) > gccReleaseDll/src/phreeqcpp/utilities.d

# Builds the ReleaseDll configuration...
.PHONY: ReleaseDll
ReleaseDll: create_folders x64/gccReleaseDll/src/fwrap.o x64/gccReleaseDll/src/fwrap2.o x64/gccReleaseDll/src/fwrap3.o x64/gccReleaseDll/src/fwrap4.o x64/gccReleaseDll/src/fwrap5.o x64/gccReleaseDll/src/fwrap6.o x64/gccReleaseDll/src/fwrap7.o x64/gccReleaseDll/src/fwrap8.o x64/gccReleaseDll/src/IPhreeqc.o x64/gccReleaseDll/src/IPhreeqcLib.o x64/gccReleaseDll/src/Var.o x64/gccReleaseDll/src/CSelectedOutput.o x64/gccReleaseDll/src/phreeqcpp/SelectedOutput.o x64/gccReleaseDll/src/phreeqcpp/UserPunch.o x64/gccReleaseDll/src/phreeqcpp/cxxKinetics.o x64/gccReleaseDll/src/phreeqcpp/cxxMix.o x64/gccReleaseDll/src/phreeqcpp/dumper.o x64/gccReleaseDll/src/phreeqcpp/Exchange.o x64/gccReleaseDll/src/phreeqcpp/ExchComp.o x64/gccReleaseDll/src/phreeqcpp/GasComp.o x64/gccReleaseDll/src/phreeqcpp/GasPhase.o x64/gccReleaseDll/src/phreeqcpp/ISolution.o x64/gccReleaseDll/src/phreeqcpp/ISolutionComp.o x64/gccReleaseDll/src/phreeqcpp/Keywords.o x64/gccReleaseDll/src/phreeqcpp/KineticsComp.o x64/gccReleaseDll/src/phreeqcpp/NameDouble.o x64/gccReleaseDll/src/phreeqcpp/NumKeyword.o x64/gccReleaseDll/src/phreeqcpp/Parser.o x64/gccReleaseDll/src/phreeqcpp/PBasic.o x64/gccReleaseDll/src/phreeqcpp/Phreeqc.o x64/gccReleaseDll/src/phreeqcpp/PHRQ_base.o x64/gccReleaseDll/src/phreeqcpp/PHRQ_io.o x64/gccReleaseDll/src/phreeqcpp/PPassemblage.o x64/gccReleaseDll/src/phreeqcpp/PPassemblageComp.o x64/gccReleaseDll/src/phreeqcpp/Pressure.o x64/gccReleaseDll/src/phreeqcpp/Reaction.o x64/gccReleaseDll/src/phreeqcpp/ReadClass.o x64/gccReleaseDll/src/phreeqcpp/runner.o x64/gccReleaseDll/src/phreeqcpp/Solution.o x64/gccReleaseDll/src/phreeqcpp/SolutionIsotope.o x64/gccReleaseDll/src/phreeqcpp/SS.o x64/gccReleaseDll/src/phreeqcpp/SSassemblage.o x64/gccReleaseDll/src/phreeqcpp/SScomp.o x64/gccReleaseDll/src/phreeqcpp/StorageBin.o x64/gccReleaseDll/src/phreeqcpp/StorageBinList.o x64/gccReleaseDll/src/phreeqcpp/Surface.o x64/gccReleaseDll/src/phreeqcpp/SurfaceCharge.o x64/gccReleaseDll/src/phreeqcpp/SurfaceComp.o x64/gccReleaseDll/src/phreeqcpp/System.o x64/gccReleaseDll/src/phreeqcpp/Temperature.o x64/gccReleaseDll/src/phreeqcpp/Use.o x64/gccReleaseDll/src/phreeqcpp/Utils.o x64/gccReleaseDll/src/phreeqcpp/advection.o x64/gccReleaseDll/src/phreeqcpp/basicsubs.o x64/gccReleaseDll/src/phreeqcpp/cl1.o x64/gccReleaseDll/src/phreeqcpp/cvdense.o x64/gccReleaseDll/src/phreeqcpp/cvode.o x64/gccReleaseDll/src/phreeqcpp/dense.o x64/gccReleaseDll/src/phreeqcpp/dw.o x64/gccReleaseDll/src/phreeqcpp/gases.o x64/gccReleaseDll/src/phreeqcpp/input.o x64/gccReleaseDll/src/phreeqcpp/integrate.o x64/gccReleaseDll/src/phreeqcpp/inverse.o x64/gccReleaseDll/src/phreeqcpp/isotopes.o x64/gccReleaseDll/src/phreeqcpp/kinetics.o x64/gccReleaseDll/src/phreeqcpp/mainsubs.o x64/gccReleaseDll/src/phreeqcpp/model.o x64/gccReleaseDll/src/phreeqcpp/nvector.o x64/gccReleaseDll/src/phreeqcpp/nvector_serial.o x64/gccReleaseDll/src/phreeqcpp/parse.o x64/gccReleaseDll/src/phreeqcpp/phqalloc.o x64/gccReleaseDll/src/phreeqcpp/PHRQ_io_output.o x64/gccReleaseDll/src/phreeqcpp/pitzer.o x64/gccReleaseDll/src/phreeqcpp/pitzer_structures.o x64/gccReleaseDll/src/phreeqcpp/prep.o x64/gccReleaseDll/src/phreeqcpp/print.o x64/gccReleaseDll/src/phreeqcpp/read.o x64/gccReleaseDll/src/phreeqcpp/readtr.o x64/gccReleaseDll/src/phreeqcpp/sit.o x64/gccReleaseDll/src/phreeqcpp/smalldense.o x64/gccReleaseDll/src/phreeqcpp/spread.o x64/gccReleaseDll/src/phreeqcpp/step.o x64/gccReleaseDll/src/phreeqcpp/structures.o x64/gccReleaseDll/src/phreeqcpp/sundialsmath.o x64/gccReleaseDll/src/phreeqcpp/tally.o x64/gccReleaseDll/src/phreeqcpp/tidy.o x64/gccReleaseDll/src/phreeqcpp/transport.o x64/gccReleaseDll/src/phreeqcpp/utilities.o 
	ar rcs ../../x64/gccReleaseDll/libIPhreeqc.a x64/gccReleaseDll/src/fwrap.o x64/gccReleaseDll/src/fwrap2.o x64/gccReleaseDll/src/fwrap3.o x64/gccReleaseDll/src/fwrap4.o x64/gccReleaseDll/src/fwrap5.o x64/gccReleaseDll/src/fwrap6.o x64/gccReleaseDll/src/fwrap7.o x64/gccReleaseDll/src/fwrap8.o x64/gccReleaseDll/src/IPhreeqc.o x64/gccReleaseDll/src/IPhreeqcLib.o x64/gccReleaseDll/src/Var.o x64/gccReleaseDll/src/CSelectedOutput.o x64/gccReleaseDll/src/phreeqcpp/SelectedOutput.o x64/gccReleaseDll/src/phreeqcpp/UserPunch.o x64/gccReleaseDll/src/phreeqcpp/cxxKinetics.o x64/gccReleaseDll/src/phreeqcpp/cxxMix.o x64/gccReleaseDll/src/phreeqcpp/dumper.o x64/gccReleaseDll/src/phreeqcpp/Exchange.o x64/gccReleaseDll/src/phreeqcpp/ExchComp.o x64/gccReleaseDll/src/phreeqcpp/GasComp.o x64/gccReleaseDll/src/phreeqcpp/GasPhase.o x64/gccReleaseDll/src/phreeqcpp/ISolution.o x64/gccReleaseDll/src/phreeqcpp/ISolutionComp.o x64/gccReleaseDll/src/phreeqcpp/Keywords.o x64/gccReleaseDll/src/phreeqcpp/KineticsComp.o x64/gccReleaseDll/src/phreeqcpp/NameDouble.o x64/gccReleaseDll/src/phreeqcpp/NumKeyword.o x64/gccReleaseDll/src/phreeqcpp/Parser.o x64/gccReleaseDll/src/phreeqcpp/PBasic.o x64/gccReleaseDll/src/phreeqcpp/Phreeqc.o x64/gccReleaseDll/src/phreeqcpp/PHRQ_base.o x64/gccReleaseDll/src/phreeqcpp/PHRQ_io.o x64/gccReleaseDll/src/phreeqcpp/PPassemblage.o x64/gccReleaseDll/src/phreeqcpp/PPassemblageComp.o x64/gccReleaseDll/src/phreeqcpp/Pressure.o x64/gccReleaseDll/src/phreeqcpp/Reaction.o x64/gccReleaseDll/src/phreeqcpp/ReadClass.o x64/gccReleaseDll/src/phreeqcpp/runner.o x64/gccReleaseDll/src/phreeqcpp/Solution.o x64/gccReleaseDll/src/phreeqcpp/SolutionIsotope.o x64/gccReleaseDll/src/phreeqcpp/SS.o x64/gccReleaseDll/src/phreeqcpp/SSassemblage.o x64/gccReleaseDll/src/phreeqcpp/SScomp.o x64/gccReleaseDll/src/phreeqcpp/StorageBin.o x64/gccReleaseDll/src/phreeqcpp/StorageBinList.o x64/gccReleaseDll/src/phreeqcpp/Surface.o x64/gccReleaseDll/src/phreeqcpp/SurfaceCharge.o x64/gccReleaseDll/src/phreeqcpp/SurfaceComp.o x64/gccReleaseDll/src/phreeqcpp/System.o x64/gccReleaseDll/src/phreeqcpp/Temperature.o x64/gccReleaseDll/src/phreeqcpp/Use.o x64/gccReleaseDll/src/phreeqcpp/Utils.o x64/gccReleaseDll/src/phreeqcpp/advection.o x64/gccReleaseDll/src/phreeqcpp/basicsubs.o x64/gccReleaseDll/src/phreeqcpp/cl1.o x64/gccReleaseDll/src/phreeqcpp/cvdense.o x64/gccReleaseDll/src/phreeqcpp/cvode.o x64/gccReleaseDll/src/phreeqcpp/dense.o x64/gccReleaseDll/src/phreeqcpp/dw.o x64/gccReleaseDll/src/phreeqcpp/gases.o x64/gccReleaseDll/src/phreeqcpp/input.o x64/gccReleaseDll/src/phreeqcpp/integrate.o x64/gccReleaseDll/src/phreeqcpp/inverse.o x64/gccReleaseDll/src/phreeqcpp/isotopes.o x64/gccReleaseDll/src/phreeqcpp/kinetics.o x64/gccReleaseDll/src/phreeqcpp/mainsubs.o x64/gccReleaseDll/src/phreeqcpp/model.o x64/gccReleaseDll/src/phreeqcpp/nvector.o x64/gccReleaseDll/src/phreeqcpp/nvector_serial.o x64/gccReleaseDll/src/phreeqcpp/parse.o x64/gccReleaseDll/src/phreeqcpp/phqalloc.o x64/gccReleaseDll/src/phreeqcpp/PHRQ_io_output.o x64/gccReleaseDll/src/phreeqcpp/pitzer.o x64/gccReleaseDll/src/phreeqcpp/pitzer_structures.o x64/gccReleaseDll/src/phreeqcpp/prep.o x64/gccReleaseDll/src/phreeqcpp/print.o x64/gccReleaseDll/src/phreeqcpp/read.o x64/gccReleaseDll/src/phreeqcpp/readtr.o x64/gccReleaseDll/src/phreeqcpp/sit.o x64/gccReleaseDll/src/phreeqcpp/smalldense.o x64/gccReleaseDll/src/phreeqcpp/spread.o x64/gccReleaseDll/src/phreeqcpp/step.o x64/gccReleaseDll/src/phreeqcpp/structures.o x64/gccReleaseDll/src/phreeqcpp/sundialsmath.o x64/gccReleaseDll/src/phreeqcpp/tally.o x64/gccReleaseDll/src/phreeqcpp/tidy.o x64/gccReleaseDll/src/phreeqcpp/transport.o x64/gccReleaseDll/src/phreeqcpp/utilities.o  $(ReleaseDll_Implicitly_Linked_Objects)

# Compiles file src/fwrap.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/fwrap.d
x64/gccReleaseDll/src/fwrap.o: src/fwrap.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/fwrap.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/fwrap.d

# Compiles file src/fwrap2.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/fwrap2.d
x64/gccReleaseDll/src/fwrap2.o: src/fwrap2.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap2.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/fwrap2.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap2.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/fwrap2.d

# Compiles file src/fwrap3.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/fwrap3.d
x64/gccReleaseDll/src/fwrap3.o: src/fwrap3.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap3.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/fwrap3.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap3.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/fwrap3.d

# Compiles file src/fwrap4.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/fwrap4.d
x64/gccReleaseDll/src/fwrap4.o: src/fwrap4.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap4.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/fwrap4.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap4.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/fwrap4.d

# Compiles file src/fwrap5.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/fwrap5.d
x64/gccReleaseDll/src/fwrap5.o: src/fwrap5.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap5.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/fwrap5.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap5.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/fwrap5.d

# Compiles file src/fwrap6.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/fwrap6.d
x64/gccReleaseDll/src/fwrap6.o: src/fwrap6.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap6.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/fwrap6.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap6.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/fwrap6.d

# Compiles file src/fwrap7.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/fwrap7.d
x64/gccReleaseDll/src/fwrap7.o: src/fwrap7.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap7.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/fwrap7.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap7.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/fwrap7.d

# Compiles file src/fwrap8.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/fwrap8.d
x64/gccReleaseDll/src/fwrap8.o: src/fwrap8.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/fwrap8.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/fwrap8.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/fwrap8.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/fwrap8.d

# Compiles file src/IPhreeqc.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/IPhreeqc.d
x64/gccReleaseDll/src/IPhreeqc.o: src/IPhreeqc.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/IPhreeqc.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/IPhreeqc.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/IPhreeqc.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/IPhreeqc.d

# Compiles file src/IPhreeqcLib.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/IPhreeqcLib.d
x64/gccReleaseDll/src/IPhreeqcLib.o: src/IPhreeqcLib.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/IPhreeqcLib.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/IPhreeqcLib.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/IPhreeqcLib.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/IPhreeqcLib.d

# Compiles file src/Var.c for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/Var.d
x64/gccReleaseDll/src/Var.o: src/Var.c
	$(C_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/Var.c $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/Var.o
	$(C_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/Var.c $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/Var.d

# Compiles file src/CSelectedOutput.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/CSelectedOutput.d
x64/gccReleaseDll/src/CSelectedOutput.o: src/CSelectedOutput.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/CSelectedOutput.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/CSelectedOutput.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/CSelectedOutput.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/CSelectedOutput.d

# Compiles file src/phreeqcpp/SelectedOutput.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/SelectedOutput.d
x64/gccReleaseDll/src/phreeqcpp/SelectedOutput.o: src/phreeqcpp/SelectedOutput.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SelectedOutput.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/SelectedOutput.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SelectedOutput.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/SelectedOutput.d

# Compiles file src/phreeqcpp/UserPunch.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/UserPunch.d
x64/gccReleaseDll/src/phreeqcpp/UserPunch.o: src/phreeqcpp/UserPunch.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/UserPunch.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/UserPunch.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/UserPunch.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/UserPunch.d

# Compiles file src/phreeqcpp/cxxKinetics.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/cxxKinetics.d
x64/gccReleaseDll/src/phreeqcpp/cxxKinetics.o: src/phreeqcpp/cxxKinetics.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/cxxKinetics.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/cxxKinetics.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/cxxKinetics.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/cxxKinetics.d

# Compiles file src/phreeqcpp/cxxMix.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/cxxMix.d
x64/gccReleaseDll/src/phreeqcpp/cxxMix.o: src/phreeqcpp/cxxMix.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/cxxMix.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/cxxMix.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/cxxMix.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/cxxMix.d

# Compiles file src/phreeqcpp/dumper.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/dumper.d
x64/gccReleaseDll/src/phreeqcpp/dumper.o: src/phreeqcpp/dumper.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/dumper.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/dumper.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/dumper.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/dumper.d

# Compiles file src/phreeqcpp/Exchange.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/Exchange.d
x64/gccReleaseDll/src/phreeqcpp/Exchange.o: src/phreeqcpp/Exchange.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Exchange.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/Exchange.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Exchange.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/Exchange.d

# Compiles file src/phreeqcpp/ExchComp.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/ExchComp.d
x64/gccReleaseDll/src/phreeqcpp/ExchComp.o: src/phreeqcpp/ExchComp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/ExchComp.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/ExchComp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/ExchComp.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/ExchComp.d

# Compiles file src/phreeqcpp/GasComp.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/GasComp.d
x64/gccReleaseDll/src/phreeqcpp/GasComp.o: src/phreeqcpp/GasComp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/GasComp.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/GasComp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/GasComp.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/GasComp.d

# Compiles file src/phreeqcpp/GasPhase.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/GasPhase.d
x64/gccReleaseDll/src/phreeqcpp/GasPhase.o: src/phreeqcpp/GasPhase.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/GasPhase.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/GasPhase.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/GasPhase.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/GasPhase.d

# Compiles file src/phreeqcpp/ISolution.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/ISolution.d
x64/gccReleaseDll/src/phreeqcpp/ISolution.o: src/phreeqcpp/ISolution.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/ISolution.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/ISolution.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/ISolution.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/ISolution.d

# Compiles file src/phreeqcpp/ISolutionComp.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/ISolutionComp.d
x64/gccReleaseDll/src/phreeqcpp/ISolutionComp.o: src/phreeqcpp/ISolutionComp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/ISolutionComp.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/ISolutionComp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/ISolutionComp.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/ISolutionComp.d

# Compiles file src/phreeqcpp/Keywords.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/Keywords.d
x64/gccReleaseDll/src/phreeqcpp/Keywords.o: src/phreeqcpp/Keywords.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Keywords.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/Keywords.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Keywords.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/Keywords.d

# Compiles file src/phreeqcpp/KineticsComp.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/KineticsComp.d
x64/gccReleaseDll/src/phreeqcpp/KineticsComp.o: src/phreeqcpp/KineticsComp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/KineticsComp.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/KineticsComp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/KineticsComp.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/KineticsComp.d

# Compiles file src/phreeqcpp/NameDouble.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/NameDouble.d
x64/gccReleaseDll/src/phreeqcpp/NameDouble.o: src/phreeqcpp/NameDouble.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/NameDouble.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/NameDouble.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/NameDouble.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/NameDouble.d

# Compiles file src/phreeqcpp/NumKeyword.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/NumKeyword.d
x64/gccReleaseDll/src/phreeqcpp/NumKeyword.o: src/phreeqcpp/NumKeyword.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/NumKeyword.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/NumKeyword.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/NumKeyword.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/NumKeyword.d

# Compiles file src/phreeqcpp/Parser.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/Parser.d
x64/gccReleaseDll/src/phreeqcpp/Parser.o: src/phreeqcpp/Parser.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Parser.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/Parser.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Parser.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/Parser.d

# Compiles file src/phreeqcpp/PBasic.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/PBasic.d
x64/gccReleaseDll/src/phreeqcpp/PBasic.o: src/phreeqcpp/PBasic.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/PBasic.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/PBasic.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/PBasic.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/PBasic.d

# Compiles file src/phreeqcpp/Phreeqc.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/Phreeqc.d
x64/gccReleaseDll/src/phreeqcpp/Phreeqc.o: src/phreeqcpp/Phreeqc.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Phreeqc.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/Phreeqc.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Phreeqc.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/Phreeqc.d

# Compiles file src/phreeqcpp/PHRQ_base.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/PHRQ_base.d
x64/gccReleaseDll/src/phreeqcpp/PHRQ_base.o: src/phreeqcpp/PHRQ_base.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/PHRQ_base.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/PHRQ_base.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/PHRQ_base.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/PHRQ_base.d

# Compiles file src/phreeqcpp/PHRQ_io.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/PHRQ_io.d
x64/gccReleaseDll/src/phreeqcpp/PHRQ_io.o: src/phreeqcpp/PHRQ_io.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/PHRQ_io.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/PHRQ_io.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/PHRQ_io.d

# Compiles file src/phreeqcpp/PPassemblage.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/PPassemblage.d
x64/gccReleaseDll/src/phreeqcpp/PPassemblage.o: src/phreeqcpp/PPassemblage.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/PPassemblage.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/PPassemblage.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/PPassemblage.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/PPassemblage.d

# Compiles file src/phreeqcpp/PPassemblageComp.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/PPassemblageComp.d
x64/gccReleaseDll/src/phreeqcpp/PPassemblageComp.o: src/phreeqcpp/PPassemblageComp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/PPassemblageComp.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/PPassemblageComp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/PPassemblageComp.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/PPassemblageComp.d

# Compiles file src/phreeqcpp/Pressure.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/Pressure.d
x64/gccReleaseDll/src/phreeqcpp/Pressure.o: src/phreeqcpp/Pressure.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Pressure.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/Pressure.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Pressure.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/Pressure.d

# Compiles file src/phreeqcpp/Reaction.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/Reaction.d
x64/gccReleaseDll/src/phreeqcpp/Reaction.o: src/phreeqcpp/Reaction.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Reaction.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/Reaction.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Reaction.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/Reaction.d

# Compiles file src/phreeqcpp/ReadClass.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/ReadClass.d
x64/gccReleaseDll/src/phreeqcpp/ReadClass.o: src/phreeqcpp/ReadClass.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/ReadClass.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/ReadClass.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/ReadClass.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/ReadClass.d

# Compiles file src/phreeqcpp/runner.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/runner.d
x64/gccReleaseDll/src/phreeqcpp/runner.o: src/phreeqcpp/runner.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/runner.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/runner.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/runner.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/runner.d

# Compiles file src/phreeqcpp/Solution.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/Solution.d
x64/gccReleaseDll/src/phreeqcpp/Solution.o: src/phreeqcpp/Solution.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Solution.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/Solution.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Solution.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/Solution.d

# Compiles file src/phreeqcpp/SolutionIsotope.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/SolutionIsotope.d
x64/gccReleaseDll/src/phreeqcpp/SolutionIsotope.o: src/phreeqcpp/SolutionIsotope.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SolutionIsotope.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/SolutionIsotope.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SolutionIsotope.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/SolutionIsotope.d

# Compiles file src/phreeqcpp/SS.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/SS.d
x64/gccReleaseDll/src/phreeqcpp/SS.o: src/phreeqcpp/SS.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SS.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/SS.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SS.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/SS.d

# Compiles file src/phreeqcpp/SSassemblage.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/SSassemblage.d
x64/gccReleaseDll/src/phreeqcpp/SSassemblage.o: src/phreeqcpp/SSassemblage.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SSassemblage.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/SSassemblage.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SSassemblage.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/SSassemblage.d

# Compiles file src/phreeqcpp/SScomp.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/SScomp.d
x64/gccReleaseDll/src/phreeqcpp/SScomp.o: src/phreeqcpp/SScomp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SScomp.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/SScomp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SScomp.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/SScomp.d

# Compiles file src/phreeqcpp/StorageBin.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/StorageBin.d
x64/gccReleaseDll/src/phreeqcpp/StorageBin.o: src/phreeqcpp/StorageBin.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/StorageBin.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/StorageBin.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/StorageBin.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/StorageBin.d

# Compiles file src/phreeqcpp/StorageBinList.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/StorageBinList.d
x64/gccReleaseDll/src/phreeqcpp/StorageBinList.o: src/phreeqcpp/StorageBinList.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/StorageBinList.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/StorageBinList.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/StorageBinList.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/StorageBinList.d

# Compiles file src/phreeqcpp/Surface.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/Surface.d
x64/gccReleaseDll/src/phreeqcpp/Surface.o: src/phreeqcpp/Surface.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Surface.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/Surface.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Surface.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/Surface.d

# Compiles file src/phreeqcpp/SurfaceCharge.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/SurfaceCharge.d
x64/gccReleaseDll/src/phreeqcpp/SurfaceCharge.o: src/phreeqcpp/SurfaceCharge.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SurfaceCharge.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/SurfaceCharge.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SurfaceCharge.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/SurfaceCharge.d

# Compiles file src/phreeqcpp/SurfaceComp.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/SurfaceComp.d
x64/gccReleaseDll/src/phreeqcpp/SurfaceComp.o: src/phreeqcpp/SurfaceComp.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/SurfaceComp.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/SurfaceComp.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/SurfaceComp.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/SurfaceComp.d

# Compiles file src/phreeqcpp/System.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/System.d
x64/gccReleaseDll/src/phreeqcpp/System.o: src/phreeqcpp/System.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/System.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/System.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/System.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/System.d

# Compiles file src/phreeqcpp/Temperature.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/Temperature.d
x64/gccReleaseDll/src/phreeqcpp/Temperature.o: src/phreeqcpp/Temperature.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Temperature.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/Temperature.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Temperature.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/Temperature.d

# Compiles file src/phreeqcpp/Use.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/Use.d
x64/gccReleaseDll/src/phreeqcpp/Use.o: src/phreeqcpp/Use.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Use.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/Use.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Use.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/Use.d

# Compiles file src/phreeqcpp/Utils.cxx for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/Utils.d
x64/gccReleaseDll/src/phreeqcpp/Utils.o: src/phreeqcpp/Utils.cxx
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/Utils.cxx $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/Utils.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/Utils.cxx $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/Utils.d

# Compiles file src/phreeqcpp/advection.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/advection.d
x64/gccReleaseDll/src/phreeqcpp/advection.o: src/phreeqcpp/advection.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/advection.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/advection.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/advection.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/advection.d

# Compiles file src/phreeqcpp/basicsubs.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/basicsubs.d
x64/gccReleaseDll/src/phreeqcpp/basicsubs.o: src/phreeqcpp/basicsubs.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/basicsubs.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/basicsubs.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/basicsubs.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/basicsubs.d

# Compiles file src/phreeqcpp/cl1.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/cl1.d
x64/gccReleaseDll/src/phreeqcpp/cl1.o: src/phreeqcpp/cl1.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/cl1.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/cl1.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/cl1.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/cl1.d

# Compiles file src/phreeqcpp/cvdense.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/cvdense.d
x64/gccReleaseDll/src/phreeqcpp/cvdense.o: src/phreeqcpp/cvdense.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/cvdense.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/cvdense.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/cvdense.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/cvdense.d

# Compiles file src/phreeqcpp/cvode.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/cvode.d
x64/gccReleaseDll/src/phreeqcpp/cvode.o: src/phreeqcpp/cvode.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/cvode.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/cvode.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/cvode.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/cvode.d

# Compiles file src/phreeqcpp/dense.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/dense.d
x64/gccReleaseDll/src/phreeqcpp/dense.o: src/phreeqcpp/dense.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/dense.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/dense.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/dense.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/dense.d

# Compiles file src/phreeqcpp/dw.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/dw.d
x64/gccReleaseDll/src/phreeqcpp/dw.o: src/phreeqcpp/dw.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/dw.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/dw.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/dw.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/dw.d

# Compiles file src/phreeqcpp/gases.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/gases.d
x64/gccReleaseDll/src/phreeqcpp/gases.o: src/phreeqcpp/gases.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/gases.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/gases.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/gases.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/gases.d

# Compiles file src/phreeqcpp/input.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/input.d
x64/gccReleaseDll/src/phreeqcpp/input.o: src/phreeqcpp/input.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/input.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/input.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/input.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/input.d

# Compiles file src/phreeqcpp/integrate.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/integrate.d
x64/gccReleaseDll/src/phreeqcpp/integrate.o: src/phreeqcpp/integrate.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/integrate.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/integrate.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/integrate.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/integrate.d

# Compiles file src/phreeqcpp/inverse.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/inverse.d
x64/gccReleaseDll/src/phreeqcpp/inverse.o: src/phreeqcpp/inverse.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/inverse.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/inverse.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/inverse.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/inverse.d

# Compiles file src/phreeqcpp/isotopes.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/isotopes.d
x64/gccReleaseDll/src/phreeqcpp/isotopes.o: src/phreeqcpp/isotopes.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/isotopes.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/isotopes.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/isotopes.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/isotopes.d

# Compiles file src/phreeqcpp/kinetics.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/kinetics.d
x64/gccReleaseDll/src/phreeqcpp/kinetics.o: src/phreeqcpp/kinetics.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/kinetics.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/kinetics.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/kinetics.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/kinetics.d

# Compiles file src/phreeqcpp/mainsubs.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/mainsubs.d
x64/gccReleaseDll/src/phreeqcpp/mainsubs.o: src/phreeqcpp/mainsubs.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/mainsubs.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/mainsubs.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/mainsubs.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/mainsubs.d

# Compiles file src/phreeqcpp/model.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/model.d
x64/gccReleaseDll/src/phreeqcpp/model.o: src/phreeqcpp/model.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/model.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/model.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/model.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/model.d

# Compiles file src/phreeqcpp/nvector.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/nvector.d
x64/gccReleaseDll/src/phreeqcpp/nvector.o: src/phreeqcpp/nvector.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/nvector.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/nvector.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/nvector.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/nvector.d

# Compiles file src/phreeqcpp/nvector_serial.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/nvector_serial.d
x64/gccReleaseDll/src/phreeqcpp/nvector_serial.o: src/phreeqcpp/nvector_serial.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/nvector_serial.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/nvector_serial.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/nvector_serial.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/nvector_serial.d

# Compiles file src/phreeqcpp/parse.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/parse.d
x64/gccReleaseDll/src/phreeqcpp/parse.o: src/phreeqcpp/parse.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/parse.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/parse.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/parse.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/parse.d

# Compiles file src/phreeqcpp/phqalloc.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/phqalloc.d
x64/gccReleaseDll/src/phreeqcpp/phqalloc.o: src/phreeqcpp/phqalloc.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/phqalloc.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/phqalloc.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/phqalloc.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/phqalloc.d

# Compiles file src/phreeqcpp/PHRQ_io_output.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/PHRQ_io_output.d
x64/gccReleaseDll/src/phreeqcpp/PHRQ_io_output.o: src/phreeqcpp/PHRQ_io_output.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/PHRQ_io_output.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/PHRQ_io_output.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io_output.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/PHRQ_io_output.d

# Compiles file src/phreeqcpp/pitzer.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/pitzer.d
x64/gccReleaseDll/src/phreeqcpp/pitzer.o: src/phreeqcpp/pitzer.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/pitzer.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/pitzer.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/pitzer.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/pitzer.d

# Compiles file src/phreeqcpp/pitzer_structures.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/pitzer_structures.d
x64/gccReleaseDll/src/phreeqcpp/pitzer_structures.o: src/phreeqcpp/pitzer_structures.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/pitzer_structures.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/pitzer_structures.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/pitzer_structures.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/pitzer_structures.d

# Compiles file src/phreeqcpp/prep.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/prep.d
x64/gccReleaseDll/src/phreeqcpp/prep.o: src/phreeqcpp/prep.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/prep.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/prep.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/prep.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/prep.d

# Compiles file src/phreeqcpp/print.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/print.d
x64/gccReleaseDll/src/phreeqcpp/print.o: src/phreeqcpp/print.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/print.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/print.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/print.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/print.d

# Compiles file src/phreeqcpp/read.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/read.d
x64/gccReleaseDll/src/phreeqcpp/read.o: src/phreeqcpp/read.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/read.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/read.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/read.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/read.d

# Compiles file src/phreeqcpp/readtr.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/readtr.d
x64/gccReleaseDll/src/phreeqcpp/readtr.o: src/phreeqcpp/readtr.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/readtr.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/readtr.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/readtr.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/readtr.d

# Compiles file src/phreeqcpp/sit.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/sit.d
x64/gccReleaseDll/src/phreeqcpp/sit.o: src/phreeqcpp/sit.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/sit.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/sit.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/sit.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/sit.d

# Compiles file src/phreeqcpp/smalldense.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/smalldense.d
x64/gccReleaseDll/src/phreeqcpp/smalldense.o: src/phreeqcpp/smalldense.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/smalldense.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/smalldense.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/smalldense.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/smalldense.d

# Compiles file src/phreeqcpp/spread.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/spread.d
x64/gccReleaseDll/src/phreeqcpp/spread.o: src/phreeqcpp/spread.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/spread.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/spread.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/spread.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/spread.d

# Compiles file src/phreeqcpp/step.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/step.d
x64/gccReleaseDll/src/phreeqcpp/step.o: src/phreeqcpp/step.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/step.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/step.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/step.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/step.d

# Compiles file src/phreeqcpp/structures.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/structures.d
x64/gccReleaseDll/src/phreeqcpp/structures.o: src/phreeqcpp/structures.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/structures.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/structures.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/structures.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/structures.d

# Compiles file src/phreeqcpp/sundialsmath.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/sundialsmath.d
x64/gccReleaseDll/src/phreeqcpp/sundialsmath.o: src/phreeqcpp/sundialsmath.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/sundialsmath.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/sundialsmath.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/sundialsmath.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/sundialsmath.d

# Compiles file src/phreeqcpp/tally.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/tally.d
x64/gccReleaseDll/src/phreeqcpp/tally.o: src/phreeqcpp/tally.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/tally.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/tally.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/tally.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/tally.d

# Compiles file src/phreeqcpp/tidy.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/tidy.d
x64/gccReleaseDll/src/phreeqcpp/tidy.o: src/phreeqcpp/tidy.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/tidy.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/tidy.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/tidy.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/tidy.d

# Compiles file src/phreeqcpp/transport.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/transport.d
x64/gccReleaseDll/src/phreeqcpp/transport.o: src/phreeqcpp/transport.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/transport.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/transport.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/transport.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/transport.d

# Compiles file src/phreeqcpp/utilities.cpp for the ReleaseDll configuration...
-include x64/gccReleaseDll/src/phreeqcpp/utilities.d
x64/gccReleaseDll/src/phreeqcpp/utilities.o: src/phreeqcpp/utilities.cpp
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -c src/phreeqcpp/utilities.cpp $(ReleaseDll_Include_Path) -o x64/gccReleaseDll/src/phreeqcpp/utilities.o
	$(CPP_COMPILER) $(ReleaseDll_Preprocessor_Definitions) $(ReleaseDll_Compiler_Flags) -MM src/phreeqcpp/utilities.cpp $(ReleaseDll_Include_Path) > x64/gccReleaseDll/src/phreeqcpp/utilities.d

# Builds the Release configuration...
.PHONY: Release
Release: create_folders gccRelease/src/fwrap.o gccRelease/src/fwrap2.o gccRelease/src/fwrap3.o gccRelease/src/fwrap4.o gccRelease/src/fwrap5.o gccRelease/src/fwrap6.o gccRelease/src/fwrap7.o gccRelease/src/fwrap8.o gccRelease/src/IPhreeqc.o gccRelease/src/IPhreeqcLib.o gccRelease/src/Var.o gccRelease/src/CSelectedOutput.o gccRelease/src/phreeqcpp/SelectedOutput.o gccRelease/src/phreeqcpp/UserPunch.o gccRelease/src/phreeqcpp/cxxKinetics.o gccRelease/src/phreeqcpp/cxxMix.o gccRelease/src/phreeqcpp/dumper.o gccRelease/src/phreeqcpp/Exchange.o gccRelease/src/phreeqcpp/ExchComp.o gccRelease/src/phreeqcpp/GasComp.o gccRelease/src/phreeqcpp/GasPhase.o gccRelease/src/phreeqcpp/ISolution.o gccRelease/src/phreeqcpp/ISolutionComp.o gccRelease/src/phreeqcpp/Keywords.o gccRelease/src/phreeqcpp/KineticsComp.o gccRelease/src/phreeqcpp/NameDouble.o gccRelease/src/phreeqcpp/NumKeyword.o gccRelease/src/phreeqcpp/Parser.o gccRelease/src/phreeqcpp/PBasic.o gccRelease/src/phreeqcpp/Phreeqc.o gccRelease/src/phreeqcpp/PHRQ_base.o gccRelease/src/phreeqcpp/PHRQ_io.o gccRelease/src/phreeqcpp/PPassemblage.o gccRelease/src/phreeqcpp/PPassemblageComp.o gccRelease/src/phreeqcpp/Pressure.o gccRelease/src/phreeqcpp/Reaction.o gccRelease/src/phreeqcpp/ReadClass.o gccRelease/src/phreeqcpp/runner.o gccRelease/src/phreeqcpp/Solution.o gccRelease/src/phreeqcpp/SolutionIsotope.o gccRelease/src/phreeqcpp/SS.o gccRelease/src/phreeqcpp/SSassemblage.o gccRelease/src/phreeqcpp/SScomp.o gccRelease/src/phreeqcpp/StorageBin.o gccRelease/src/phreeqcpp/StorageBinList.o gccRelease/src/phreeqcpp/Surface.o gccRelease/src/phreeqcpp/SurfaceCharge.o gccRelease/src/phreeqcpp/SurfaceComp.o gccRelease/src/phreeqcpp/System.o gccRelease/src/phreeqcpp/Temperature.o gccRelease/src/phreeqcpp/Use.o gccRelease/src/phreeqcpp/Utils.o gccRelease/src/phreeqcpp/advection.o gccRelease/src/phreeqcpp/basicsubs.o gccRelease/src/phreeqcpp/cl1.o gccRelease/src/phreeqcpp/cvdense.o gccRelease/src/phreeqcpp/cvode.o gccRelease/src/phreeqcpp/dense.o gccRelease/src/phreeqcpp/dw.o gccRelease/src/phreeqcpp/gases.o gccRelease/src/phreeqcpp/input.o gccRelease/src/phreeqcpp/integrate.o gccRelease/src/phreeqcpp/inverse.o gccRelease/src/phreeqcpp/isotopes.o gccRelease/src/phreeqcpp/kinetics.o gccRelease/src/phreeqcpp/mainsubs.o gccRelease/src/phreeqcpp/model.o gccRelease/src/phreeqcpp/nvector.o gccRelease/src/phreeqcpp/nvector_serial.o gccRelease/src/phreeqcpp/parse.o gccRelease/src/phreeqcpp/phqalloc.o gccRelease/src/phreeqcpp/PHRQ_io_output.o gccRelease/src/phreeqcpp/pitzer.o gccRelease/src/phreeqcpp/pitzer_structures.o gccRelease/src/phreeqcpp/prep.o gccRelease/src/phreeqcpp/print.o gccRelease/src/phreeqcpp/read.o gccRelease/src/phreeqcpp/readtr.o gccRelease/src/phreeqcpp/sit.o gccRelease/src/phreeqcpp/smalldense.o gccRelease/src/phreeqcpp/spread.o gccRelease/src/phreeqcpp/step.o gccRelease/src/phreeqcpp/structures.o gccRelease/src/phreeqcpp/sundialsmath.o gccRelease/src/phreeqcpp/tally.o gccRelease/src/phreeqcpp/tidy.o gccRelease/src/phreeqcpp/transport.o gccRelease/src/phreeqcpp/utilities.o 
	ar rcs ../../gccRelease/libIPhreeqc.a gccRelease/src/fwrap.o gccRelease/src/fwrap2.o gccRelease/src/fwrap3.o gccRelease/src/fwrap4.o gccRelease/src/fwrap5.o gccRelease/src/fwrap6.o gccRelease/src/fwrap7.o gccRelease/src/fwrap8.o gccRelease/src/IPhreeqc.o gccRelease/src/IPhreeqcLib.o gccRelease/src/Var.o gccRelease/src/CSelectedOutput.o gccRelease/src/phreeqcpp/SelectedOutput.o gccRelease/src/phreeqcpp/UserPunch.o gccRelease/src/phreeqcpp/cxxKinetics.o gccRelease/src/phreeqcpp/cxxMix.o gccRelease/src/phreeqcpp/dumper.o gccRelease/src/phreeqcpp/Exchange.o gccRelease/src/phreeqcpp/ExchComp.o gccRelease/src/phreeqcpp/GasComp.o gccRelease/src/phreeqcpp/GasPhase.o gccRelease/src/phreeqcpp/ISolution.o gccRelease/src/phreeqcpp/ISolutionComp.o gccRelease/src/phreeqcpp/Keywords.o gccRelease/src/phreeqcpp/KineticsComp.o gccRelease/src/phreeqcpp/NameDouble.o gccRelease/src/phreeqcpp/NumKeyword.o gccRelease/src/phreeqcpp/Parser.o gccRelease/src/phreeqcpp/PBasic.o gccRelease/src/phreeqcpp/Phreeqc.o gccRelease/src/phreeqcpp/PHRQ_base.o gccRelease/src/phreeqcpp/PHRQ_io.o gccRelease/src/phreeqcpp/PPassemblage.o gccRelease/src/phreeqcpp/PPassemblageComp.o gccRelease/src/phreeqcpp/Pressure.o gccRelease/src/phreeqcpp/Reaction.o gccRelease/src/phreeqcpp/ReadClass.o gccRelease/src/phreeqcpp/runner.o gccRelease/src/phreeqcpp/Solution.o gccRelease/src/phreeqcpp/SolutionIsotope.o gccRelease/src/phreeqcpp/SS.o gccRelease/src/phreeqcpp/SSassemblage.o gccRelease/src/phreeqcpp/SScomp.o gccRelease/src/phreeqcpp/StorageBin.o gccRelease/src/phreeqcpp/StorageBinList.o gccRelease/src/phreeqcpp/Surface.o gccRelease/src/phreeqcpp/SurfaceCharge.o gccRelease/src/phreeqcpp/SurfaceComp.o gccRelease/src/phreeqcpp/System.o gccRelease/src/phreeqcpp/Temperature.o gccRelease/src/phreeqcpp/Use.o gccRelease/src/phreeqcpp/Utils.o gccRelease/src/phreeqcpp/advection.o gccRelease/src/phreeqcpp/basicsubs.o gccRelease/src/phreeqcpp/cl1.o gccRelease/src/phreeqcpp/cvdense.o gccRelease/src/phreeqcpp/cvode.o gccRelease/src/phreeqcpp/dense.o gccRelease/src/phreeqcpp/dw.o gccRelease/src/phreeqcpp/gases.o gccRelease/src/phreeqcpp/input.o gccRelease/src/phreeqcpp/integrate.o gccRelease/src/phreeqcpp/inverse.o gccRelease/src/phreeqcpp/isotopes.o gccRelease/src/phreeqcpp/kinetics.o gccRelease/src/phreeqcpp/mainsubs.o gccRelease/src/phreeqcpp/model.o gccRelease/src/phreeqcpp/nvector.o gccRelease/src/phreeqcpp/nvector_serial.o gccRelease/src/phreeqcpp/parse.o gccRelease/src/phreeqcpp/phqalloc.o gccRelease/src/phreeqcpp/PHRQ_io_output.o gccRelease/src/phreeqcpp/pitzer.o gccRelease/src/phreeqcpp/pitzer_structures.o gccRelease/src/phreeqcpp/prep.o gccRelease/src/phreeqcpp/print.o gccRelease/src/phreeqcpp/read.o gccRelease/src/phreeqcpp/readtr.o gccRelease/src/phreeqcpp/sit.o gccRelease/src/phreeqcpp/smalldense.o gccRelease/src/phreeqcpp/spread.o gccRelease/src/phreeqcpp/step.o gccRelease/src/phreeqcpp/structures.o gccRelease/src/phreeqcpp/sundialsmath.o gccRelease/src/phreeqcpp/tally.o gccRelease/src/phreeqcpp/tidy.o gccRelease/src/phreeqcpp/transport.o gccRelease/src/phreeqcpp/utilities.o  $(Release_Implicitly_Linked_Objects)

# Compiles file src/fwrap.cpp for the Release configuration...
-include gccRelease/src/fwrap.d
gccRelease/src/fwrap.o: src/fwrap.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap.cpp $(Release_Include_Path) -o gccRelease/src/fwrap.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap.cpp $(Release_Include_Path) > gccRelease/src/fwrap.d

# Compiles file src/fwrap2.cpp for the Release configuration...
-include gccRelease/src/fwrap2.d
gccRelease/src/fwrap2.o: src/fwrap2.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap2.cpp $(Release_Include_Path) -o gccRelease/src/fwrap2.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap2.cpp $(Release_Include_Path) > gccRelease/src/fwrap2.d

# Compiles file src/fwrap3.cpp for the Release configuration...
-include gccRelease/src/fwrap3.d
gccRelease/src/fwrap3.o: src/fwrap3.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap3.cpp $(Release_Include_Path) -o gccRelease/src/fwrap3.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap3.cpp $(Release_Include_Path) > gccRelease/src/fwrap3.d

# Compiles file src/fwrap4.cpp for the Release configuration...
-include gccRelease/src/fwrap4.d
gccRelease/src/fwrap4.o: src/fwrap4.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap4.cpp $(Release_Include_Path) -o gccRelease/src/fwrap4.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap4.cpp $(Release_Include_Path) > gccRelease/src/fwrap4.d

# Compiles file src/fwrap5.cpp for the Release configuration...
-include gccRelease/src/fwrap5.d
gccRelease/src/fwrap5.o: src/fwrap5.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap5.cpp $(Release_Include_Path) -o gccRelease/src/fwrap5.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap5.cpp $(Release_Include_Path) > gccRelease/src/fwrap5.d

# Compiles file src/fwrap6.cpp for the Release configuration...
-include gccRelease/src/fwrap6.d
gccRelease/src/fwrap6.o: src/fwrap6.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap6.cpp $(Release_Include_Path) -o gccRelease/src/fwrap6.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap6.cpp $(Release_Include_Path) > gccRelease/src/fwrap6.d

# Compiles file src/fwrap7.cpp for the Release configuration...
-include gccRelease/src/fwrap7.d
gccRelease/src/fwrap7.o: src/fwrap7.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap7.cpp $(Release_Include_Path) -o gccRelease/src/fwrap7.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap7.cpp $(Release_Include_Path) > gccRelease/src/fwrap7.d

# Compiles file src/fwrap8.cpp for the Release configuration...
-include gccRelease/src/fwrap8.d
gccRelease/src/fwrap8.o: src/fwrap8.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap8.cpp $(Release_Include_Path) -o gccRelease/src/fwrap8.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap8.cpp $(Release_Include_Path) > gccRelease/src/fwrap8.d

# Compiles file src/IPhreeqc.cpp for the Release configuration...
-include gccRelease/src/IPhreeqc.d
gccRelease/src/IPhreeqc.o: src/IPhreeqc.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/IPhreeqc.cpp $(Release_Include_Path) -o gccRelease/src/IPhreeqc.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/IPhreeqc.cpp $(Release_Include_Path) > gccRelease/src/IPhreeqc.d

# Compiles file src/IPhreeqcLib.cpp for the Release configuration...
-include gccRelease/src/IPhreeqcLib.d
gccRelease/src/IPhreeqcLib.o: src/IPhreeqcLib.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/IPhreeqcLib.cpp $(Release_Include_Path) -o gccRelease/src/IPhreeqcLib.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/IPhreeqcLib.cpp $(Release_Include_Path) > gccRelease/src/IPhreeqcLib.d

# Compiles file src/Var.c for the Release configuration...
-include gccRelease/src/Var.d
gccRelease/src/Var.o: src/Var.c
	$(C_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/Var.c $(Release_Include_Path) -o gccRelease/src/Var.o
	$(C_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/Var.c $(Release_Include_Path) > gccRelease/src/Var.d

# Compiles file src/CSelectedOutput.cpp for the Release configuration...
-include gccRelease/src/CSelectedOutput.d
gccRelease/src/CSelectedOutput.o: src/CSelectedOutput.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/CSelectedOutput.cpp $(Release_Include_Path) -o gccRelease/src/CSelectedOutput.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/CSelectedOutput.cpp $(Release_Include_Path) > gccRelease/src/CSelectedOutput.d

# Compiles file src/phreeqcpp/SelectedOutput.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/SelectedOutput.d
gccRelease/src/phreeqcpp/SelectedOutput.o: src/phreeqcpp/SelectedOutput.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SelectedOutput.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/SelectedOutput.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SelectedOutput.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/SelectedOutput.d

# Compiles file src/phreeqcpp/UserPunch.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/UserPunch.d
gccRelease/src/phreeqcpp/UserPunch.o: src/phreeqcpp/UserPunch.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/UserPunch.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/UserPunch.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/UserPunch.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/UserPunch.d

# Compiles file src/phreeqcpp/cxxKinetics.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/cxxKinetics.d
gccRelease/src/phreeqcpp/cxxKinetics.o: src/phreeqcpp/cxxKinetics.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/cxxKinetics.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/cxxKinetics.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/cxxKinetics.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/cxxKinetics.d

# Compiles file src/phreeqcpp/cxxMix.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/cxxMix.d
gccRelease/src/phreeqcpp/cxxMix.o: src/phreeqcpp/cxxMix.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/cxxMix.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/cxxMix.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/cxxMix.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/cxxMix.d

# Compiles file src/phreeqcpp/dumper.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/dumper.d
gccRelease/src/phreeqcpp/dumper.o: src/phreeqcpp/dumper.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/dumper.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/dumper.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/dumper.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/dumper.d

# Compiles file src/phreeqcpp/Exchange.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/Exchange.d
gccRelease/src/phreeqcpp/Exchange.o: src/phreeqcpp/Exchange.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Exchange.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/Exchange.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Exchange.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/Exchange.d

# Compiles file src/phreeqcpp/ExchComp.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/ExchComp.d
gccRelease/src/phreeqcpp/ExchComp.o: src/phreeqcpp/ExchComp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/ExchComp.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/ExchComp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/ExchComp.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/ExchComp.d

# Compiles file src/phreeqcpp/GasComp.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/GasComp.d
gccRelease/src/phreeqcpp/GasComp.o: src/phreeqcpp/GasComp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/GasComp.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/GasComp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/GasComp.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/GasComp.d

# Compiles file src/phreeqcpp/GasPhase.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/GasPhase.d
gccRelease/src/phreeqcpp/GasPhase.o: src/phreeqcpp/GasPhase.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/GasPhase.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/GasPhase.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/GasPhase.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/GasPhase.d

# Compiles file src/phreeqcpp/ISolution.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/ISolution.d
gccRelease/src/phreeqcpp/ISolution.o: src/phreeqcpp/ISolution.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/ISolution.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/ISolution.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/ISolution.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/ISolution.d

# Compiles file src/phreeqcpp/ISolutionComp.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/ISolutionComp.d
gccRelease/src/phreeqcpp/ISolutionComp.o: src/phreeqcpp/ISolutionComp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/ISolutionComp.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/ISolutionComp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/ISolutionComp.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/ISolutionComp.d

# Compiles file src/phreeqcpp/Keywords.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/Keywords.d
gccRelease/src/phreeqcpp/Keywords.o: src/phreeqcpp/Keywords.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Keywords.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/Keywords.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Keywords.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/Keywords.d

# Compiles file src/phreeqcpp/KineticsComp.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/KineticsComp.d
gccRelease/src/phreeqcpp/KineticsComp.o: src/phreeqcpp/KineticsComp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/KineticsComp.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/KineticsComp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/KineticsComp.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/KineticsComp.d

# Compiles file src/phreeqcpp/NameDouble.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/NameDouble.d
gccRelease/src/phreeqcpp/NameDouble.o: src/phreeqcpp/NameDouble.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/NameDouble.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/NameDouble.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/NameDouble.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/NameDouble.d

# Compiles file src/phreeqcpp/NumKeyword.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/NumKeyword.d
gccRelease/src/phreeqcpp/NumKeyword.o: src/phreeqcpp/NumKeyword.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/NumKeyword.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/NumKeyword.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/NumKeyword.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/NumKeyword.d

# Compiles file src/phreeqcpp/Parser.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/Parser.d
gccRelease/src/phreeqcpp/Parser.o: src/phreeqcpp/Parser.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Parser.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/Parser.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Parser.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/Parser.d

# Compiles file src/phreeqcpp/PBasic.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/PBasic.d
gccRelease/src/phreeqcpp/PBasic.o: src/phreeqcpp/PBasic.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/PBasic.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/PBasic.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/PBasic.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/PBasic.d

# Compiles file src/phreeqcpp/Phreeqc.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/Phreeqc.d
gccRelease/src/phreeqcpp/Phreeqc.o: src/phreeqcpp/Phreeqc.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Phreeqc.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/Phreeqc.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Phreeqc.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/Phreeqc.d

# Compiles file src/phreeqcpp/PHRQ_base.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/PHRQ_base.d
gccRelease/src/phreeqcpp/PHRQ_base.o: src/phreeqcpp/PHRQ_base.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/PHRQ_base.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/PHRQ_base.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/PHRQ_base.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/PHRQ_base.d

# Compiles file src/phreeqcpp/PHRQ_io.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/PHRQ_io.d
gccRelease/src/phreeqcpp/PHRQ_io.o: src/phreeqcpp/PHRQ_io.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/PHRQ_io.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/PHRQ_io.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/PHRQ_io.d

# Compiles file src/phreeqcpp/PPassemblage.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/PPassemblage.d
gccRelease/src/phreeqcpp/PPassemblage.o: src/phreeqcpp/PPassemblage.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/PPassemblage.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/PPassemblage.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/PPassemblage.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/PPassemblage.d

# Compiles file src/phreeqcpp/PPassemblageComp.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/PPassemblageComp.d
gccRelease/src/phreeqcpp/PPassemblageComp.o: src/phreeqcpp/PPassemblageComp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/PPassemblageComp.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/PPassemblageComp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/PPassemblageComp.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/PPassemblageComp.d

# Compiles file src/phreeqcpp/Pressure.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/Pressure.d
gccRelease/src/phreeqcpp/Pressure.o: src/phreeqcpp/Pressure.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Pressure.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/Pressure.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Pressure.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/Pressure.d

# Compiles file src/phreeqcpp/Reaction.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/Reaction.d
gccRelease/src/phreeqcpp/Reaction.o: src/phreeqcpp/Reaction.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Reaction.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/Reaction.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Reaction.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/Reaction.d

# Compiles file src/phreeqcpp/ReadClass.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/ReadClass.d
gccRelease/src/phreeqcpp/ReadClass.o: src/phreeqcpp/ReadClass.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/ReadClass.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/ReadClass.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/ReadClass.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/ReadClass.d

# Compiles file src/phreeqcpp/runner.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/runner.d
gccRelease/src/phreeqcpp/runner.o: src/phreeqcpp/runner.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/runner.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/runner.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/runner.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/runner.d

# Compiles file src/phreeqcpp/Solution.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/Solution.d
gccRelease/src/phreeqcpp/Solution.o: src/phreeqcpp/Solution.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Solution.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/Solution.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Solution.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/Solution.d

# Compiles file src/phreeqcpp/SolutionIsotope.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/SolutionIsotope.d
gccRelease/src/phreeqcpp/SolutionIsotope.o: src/phreeqcpp/SolutionIsotope.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SolutionIsotope.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/SolutionIsotope.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SolutionIsotope.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/SolutionIsotope.d

# Compiles file src/phreeqcpp/SS.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/SS.d
gccRelease/src/phreeqcpp/SS.o: src/phreeqcpp/SS.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SS.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/SS.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SS.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/SS.d

# Compiles file src/phreeqcpp/SSassemblage.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/SSassemblage.d
gccRelease/src/phreeqcpp/SSassemblage.o: src/phreeqcpp/SSassemblage.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SSassemblage.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/SSassemblage.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SSassemblage.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/SSassemblage.d

# Compiles file src/phreeqcpp/SScomp.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/SScomp.d
gccRelease/src/phreeqcpp/SScomp.o: src/phreeqcpp/SScomp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SScomp.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/SScomp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SScomp.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/SScomp.d

# Compiles file src/phreeqcpp/StorageBin.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/StorageBin.d
gccRelease/src/phreeqcpp/StorageBin.o: src/phreeqcpp/StorageBin.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/StorageBin.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/StorageBin.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/StorageBin.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/StorageBin.d

# Compiles file src/phreeqcpp/StorageBinList.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/StorageBinList.d
gccRelease/src/phreeqcpp/StorageBinList.o: src/phreeqcpp/StorageBinList.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/StorageBinList.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/StorageBinList.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/StorageBinList.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/StorageBinList.d

# Compiles file src/phreeqcpp/Surface.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/Surface.d
gccRelease/src/phreeqcpp/Surface.o: src/phreeqcpp/Surface.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Surface.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/Surface.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Surface.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/Surface.d

# Compiles file src/phreeqcpp/SurfaceCharge.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/SurfaceCharge.d
gccRelease/src/phreeqcpp/SurfaceCharge.o: src/phreeqcpp/SurfaceCharge.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SurfaceCharge.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/SurfaceCharge.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SurfaceCharge.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/SurfaceCharge.d

# Compiles file src/phreeqcpp/SurfaceComp.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/SurfaceComp.d
gccRelease/src/phreeqcpp/SurfaceComp.o: src/phreeqcpp/SurfaceComp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SurfaceComp.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/SurfaceComp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SurfaceComp.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/SurfaceComp.d

# Compiles file src/phreeqcpp/System.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/System.d
gccRelease/src/phreeqcpp/System.o: src/phreeqcpp/System.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/System.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/System.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/System.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/System.d

# Compiles file src/phreeqcpp/Temperature.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/Temperature.d
gccRelease/src/phreeqcpp/Temperature.o: src/phreeqcpp/Temperature.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Temperature.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/Temperature.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Temperature.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/Temperature.d

# Compiles file src/phreeqcpp/Use.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/Use.d
gccRelease/src/phreeqcpp/Use.o: src/phreeqcpp/Use.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Use.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/Use.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Use.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/Use.d

# Compiles file src/phreeqcpp/Utils.cxx for the Release configuration...
-include gccRelease/src/phreeqcpp/Utils.d
gccRelease/src/phreeqcpp/Utils.o: src/phreeqcpp/Utils.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Utils.cxx $(Release_Include_Path) -o gccRelease/src/phreeqcpp/Utils.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Utils.cxx $(Release_Include_Path) > gccRelease/src/phreeqcpp/Utils.d

# Compiles file src/phreeqcpp/advection.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/advection.d
gccRelease/src/phreeqcpp/advection.o: src/phreeqcpp/advection.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/advection.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/advection.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/advection.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/advection.d

# Compiles file src/phreeqcpp/basicsubs.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/basicsubs.d
gccRelease/src/phreeqcpp/basicsubs.o: src/phreeqcpp/basicsubs.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/basicsubs.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/basicsubs.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/basicsubs.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/basicsubs.d

# Compiles file src/phreeqcpp/cl1.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/cl1.d
gccRelease/src/phreeqcpp/cl1.o: src/phreeqcpp/cl1.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/cl1.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/cl1.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/cl1.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/cl1.d

# Compiles file src/phreeqcpp/cvdense.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/cvdense.d
gccRelease/src/phreeqcpp/cvdense.o: src/phreeqcpp/cvdense.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/cvdense.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/cvdense.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/cvdense.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/cvdense.d

# Compiles file src/phreeqcpp/cvode.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/cvode.d
gccRelease/src/phreeqcpp/cvode.o: src/phreeqcpp/cvode.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/cvode.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/cvode.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/cvode.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/cvode.d

# Compiles file src/phreeqcpp/dense.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/dense.d
gccRelease/src/phreeqcpp/dense.o: src/phreeqcpp/dense.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/dense.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/dense.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/dense.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/dense.d

# Compiles file src/phreeqcpp/dw.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/dw.d
gccRelease/src/phreeqcpp/dw.o: src/phreeqcpp/dw.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/dw.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/dw.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/dw.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/dw.d

# Compiles file src/phreeqcpp/gases.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/gases.d
gccRelease/src/phreeqcpp/gases.o: src/phreeqcpp/gases.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/gases.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/gases.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/gases.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/gases.d

# Compiles file src/phreeqcpp/input.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/input.d
gccRelease/src/phreeqcpp/input.o: src/phreeqcpp/input.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/input.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/input.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/input.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/input.d

# Compiles file src/phreeqcpp/integrate.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/integrate.d
gccRelease/src/phreeqcpp/integrate.o: src/phreeqcpp/integrate.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/integrate.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/integrate.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/integrate.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/integrate.d

# Compiles file src/phreeqcpp/inverse.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/inverse.d
gccRelease/src/phreeqcpp/inverse.o: src/phreeqcpp/inverse.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/inverse.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/inverse.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/inverse.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/inverse.d

# Compiles file src/phreeqcpp/isotopes.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/isotopes.d
gccRelease/src/phreeqcpp/isotopes.o: src/phreeqcpp/isotopes.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/isotopes.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/isotopes.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/isotopes.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/isotopes.d

# Compiles file src/phreeqcpp/kinetics.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/kinetics.d
gccRelease/src/phreeqcpp/kinetics.o: src/phreeqcpp/kinetics.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/kinetics.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/kinetics.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/kinetics.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/kinetics.d

# Compiles file src/phreeqcpp/mainsubs.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/mainsubs.d
gccRelease/src/phreeqcpp/mainsubs.o: src/phreeqcpp/mainsubs.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/mainsubs.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/mainsubs.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/mainsubs.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/mainsubs.d

# Compiles file src/phreeqcpp/model.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/model.d
gccRelease/src/phreeqcpp/model.o: src/phreeqcpp/model.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/model.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/model.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/model.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/model.d

# Compiles file src/phreeqcpp/nvector.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/nvector.d
gccRelease/src/phreeqcpp/nvector.o: src/phreeqcpp/nvector.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/nvector.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/nvector.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/nvector.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/nvector.d

# Compiles file src/phreeqcpp/nvector_serial.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/nvector_serial.d
gccRelease/src/phreeqcpp/nvector_serial.o: src/phreeqcpp/nvector_serial.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/nvector_serial.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/nvector_serial.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/nvector_serial.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/nvector_serial.d

# Compiles file src/phreeqcpp/parse.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/parse.d
gccRelease/src/phreeqcpp/parse.o: src/phreeqcpp/parse.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/parse.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/parse.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/parse.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/parse.d

# Compiles file src/phreeqcpp/phqalloc.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/phqalloc.d
gccRelease/src/phreeqcpp/phqalloc.o: src/phreeqcpp/phqalloc.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/phqalloc.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/phqalloc.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/phqalloc.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/phqalloc.d

# Compiles file src/phreeqcpp/PHRQ_io_output.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/PHRQ_io_output.d
gccRelease/src/phreeqcpp/PHRQ_io_output.o: src/phreeqcpp/PHRQ_io_output.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/PHRQ_io_output.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/PHRQ_io_output.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io_output.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/PHRQ_io_output.d

# Compiles file src/phreeqcpp/pitzer.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/pitzer.d
gccRelease/src/phreeqcpp/pitzer.o: src/phreeqcpp/pitzer.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/pitzer.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/pitzer.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/pitzer.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/pitzer.d

# Compiles file src/phreeqcpp/pitzer_structures.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/pitzer_structures.d
gccRelease/src/phreeqcpp/pitzer_structures.o: src/phreeqcpp/pitzer_structures.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/pitzer_structures.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/pitzer_structures.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/pitzer_structures.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/pitzer_structures.d

# Compiles file src/phreeqcpp/prep.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/prep.d
gccRelease/src/phreeqcpp/prep.o: src/phreeqcpp/prep.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/prep.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/prep.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/prep.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/prep.d

# Compiles file src/phreeqcpp/print.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/print.d
gccRelease/src/phreeqcpp/print.o: src/phreeqcpp/print.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/print.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/print.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/print.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/print.d

# Compiles file src/phreeqcpp/read.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/read.d
gccRelease/src/phreeqcpp/read.o: src/phreeqcpp/read.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/read.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/read.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/read.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/read.d

# Compiles file src/phreeqcpp/readtr.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/readtr.d
gccRelease/src/phreeqcpp/readtr.o: src/phreeqcpp/readtr.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/readtr.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/readtr.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/readtr.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/readtr.d

# Compiles file src/phreeqcpp/sit.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/sit.d
gccRelease/src/phreeqcpp/sit.o: src/phreeqcpp/sit.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/sit.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/sit.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/sit.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/sit.d

# Compiles file src/phreeqcpp/smalldense.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/smalldense.d
gccRelease/src/phreeqcpp/smalldense.o: src/phreeqcpp/smalldense.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/smalldense.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/smalldense.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/smalldense.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/smalldense.d

# Compiles file src/phreeqcpp/spread.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/spread.d
gccRelease/src/phreeqcpp/spread.o: src/phreeqcpp/spread.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/spread.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/spread.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/spread.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/spread.d

# Compiles file src/phreeqcpp/step.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/step.d
gccRelease/src/phreeqcpp/step.o: src/phreeqcpp/step.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/step.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/step.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/step.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/step.d

# Compiles file src/phreeqcpp/structures.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/structures.d
gccRelease/src/phreeqcpp/structures.o: src/phreeqcpp/structures.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/structures.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/structures.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/structures.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/structures.d

# Compiles file src/phreeqcpp/sundialsmath.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/sundialsmath.d
gccRelease/src/phreeqcpp/sundialsmath.o: src/phreeqcpp/sundialsmath.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/sundialsmath.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/sundialsmath.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/sundialsmath.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/sundialsmath.d

# Compiles file src/phreeqcpp/tally.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/tally.d
gccRelease/src/phreeqcpp/tally.o: src/phreeqcpp/tally.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/tally.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/tally.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/tally.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/tally.d

# Compiles file src/phreeqcpp/tidy.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/tidy.d
gccRelease/src/phreeqcpp/tidy.o: src/phreeqcpp/tidy.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/tidy.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/tidy.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/tidy.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/tidy.d

# Compiles file src/phreeqcpp/transport.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/transport.d
gccRelease/src/phreeqcpp/transport.o: src/phreeqcpp/transport.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/transport.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/transport.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/transport.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/transport.d

# Compiles file src/phreeqcpp/utilities.cpp for the Release configuration...
-include gccRelease/src/phreeqcpp/utilities.d
gccRelease/src/phreeqcpp/utilities.o: src/phreeqcpp/utilities.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/utilities.cpp $(Release_Include_Path) -o gccRelease/src/phreeqcpp/utilities.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/utilities.cpp $(Release_Include_Path) > gccRelease/src/phreeqcpp/utilities.d

# Builds the Release configuration...
.PHONY: Release
Release: create_folders x64/gccRelease/src/fwrap.o x64/gccRelease/src/fwrap2.o x64/gccRelease/src/fwrap3.o x64/gccRelease/src/fwrap4.o x64/gccRelease/src/fwrap5.o x64/gccRelease/src/fwrap6.o x64/gccRelease/src/fwrap7.o x64/gccRelease/src/fwrap8.o x64/gccRelease/src/IPhreeqc.o x64/gccRelease/src/IPhreeqcLib.o x64/gccRelease/src/Var.o x64/gccRelease/src/CSelectedOutput.o x64/gccRelease/src/phreeqcpp/SelectedOutput.o x64/gccRelease/src/phreeqcpp/UserPunch.o x64/gccRelease/src/phreeqcpp/cxxKinetics.o x64/gccRelease/src/phreeqcpp/cxxMix.o x64/gccRelease/src/phreeqcpp/dumper.o x64/gccRelease/src/phreeqcpp/Exchange.o x64/gccRelease/src/phreeqcpp/ExchComp.o x64/gccRelease/src/phreeqcpp/GasComp.o x64/gccRelease/src/phreeqcpp/GasPhase.o x64/gccRelease/src/phreeqcpp/ISolution.o x64/gccRelease/src/phreeqcpp/ISolutionComp.o x64/gccRelease/src/phreeqcpp/Keywords.o x64/gccRelease/src/phreeqcpp/KineticsComp.o x64/gccRelease/src/phreeqcpp/NameDouble.o x64/gccRelease/src/phreeqcpp/NumKeyword.o x64/gccRelease/src/phreeqcpp/Parser.o x64/gccRelease/src/phreeqcpp/PBasic.o x64/gccRelease/src/phreeqcpp/Phreeqc.o x64/gccRelease/src/phreeqcpp/PHRQ_base.o x64/gccRelease/src/phreeqcpp/PHRQ_io.o x64/gccRelease/src/phreeqcpp/PPassemblage.o x64/gccRelease/src/phreeqcpp/PPassemblageComp.o x64/gccRelease/src/phreeqcpp/Pressure.o x64/gccRelease/src/phreeqcpp/Reaction.o x64/gccRelease/src/phreeqcpp/ReadClass.o x64/gccRelease/src/phreeqcpp/runner.o x64/gccRelease/src/phreeqcpp/Solution.o x64/gccRelease/src/phreeqcpp/SolutionIsotope.o x64/gccRelease/src/phreeqcpp/SS.o x64/gccRelease/src/phreeqcpp/SSassemblage.o x64/gccRelease/src/phreeqcpp/SScomp.o x64/gccRelease/src/phreeqcpp/StorageBin.o x64/gccRelease/src/phreeqcpp/StorageBinList.o x64/gccRelease/src/phreeqcpp/Surface.o x64/gccRelease/src/phreeqcpp/SurfaceCharge.o x64/gccRelease/src/phreeqcpp/SurfaceComp.o x64/gccRelease/src/phreeqcpp/System.o x64/gccRelease/src/phreeqcpp/Temperature.o x64/gccRelease/src/phreeqcpp/Use.o x64/gccRelease/src/phreeqcpp/Utils.o x64/gccRelease/src/phreeqcpp/advection.o x64/gccRelease/src/phreeqcpp/basicsubs.o x64/gccRelease/src/phreeqcpp/cl1.o x64/gccRelease/src/phreeqcpp/cvdense.o x64/gccRelease/src/phreeqcpp/cvode.o x64/gccRelease/src/phreeqcpp/dense.o x64/gccRelease/src/phreeqcpp/dw.o x64/gccRelease/src/phreeqcpp/gases.o x64/gccRelease/src/phreeqcpp/input.o x64/gccRelease/src/phreeqcpp/integrate.o x64/gccRelease/src/phreeqcpp/inverse.o x64/gccRelease/src/phreeqcpp/isotopes.o x64/gccRelease/src/phreeqcpp/kinetics.o x64/gccRelease/src/phreeqcpp/mainsubs.o x64/gccRelease/src/phreeqcpp/model.o x64/gccRelease/src/phreeqcpp/nvector.o x64/gccRelease/src/phreeqcpp/nvector_serial.o x64/gccRelease/src/phreeqcpp/parse.o x64/gccRelease/src/phreeqcpp/phqalloc.o x64/gccRelease/src/phreeqcpp/PHRQ_io_output.o x64/gccRelease/src/phreeqcpp/pitzer.o x64/gccRelease/src/phreeqcpp/pitzer_structures.o x64/gccRelease/src/phreeqcpp/prep.o x64/gccRelease/src/phreeqcpp/print.o x64/gccRelease/src/phreeqcpp/read.o x64/gccRelease/src/phreeqcpp/readtr.o x64/gccRelease/src/phreeqcpp/sit.o x64/gccRelease/src/phreeqcpp/smalldense.o x64/gccRelease/src/phreeqcpp/spread.o x64/gccRelease/src/phreeqcpp/step.o x64/gccRelease/src/phreeqcpp/structures.o x64/gccRelease/src/phreeqcpp/sundialsmath.o x64/gccRelease/src/phreeqcpp/tally.o x64/gccRelease/src/phreeqcpp/tidy.o x64/gccRelease/src/phreeqcpp/transport.o x64/gccRelease/src/phreeqcpp/utilities.o 
	ar rcs ../../x64/gccRelease/libIPhreeqc.a x64/gccRelease/src/fwrap.o x64/gccRelease/src/fwrap2.o x64/gccRelease/src/fwrap3.o x64/gccRelease/src/fwrap4.o x64/gccRelease/src/fwrap5.o x64/gccRelease/src/fwrap6.o x64/gccRelease/src/fwrap7.o x64/gccRelease/src/fwrap8.o x64/gccRelease/src/IPhreeqc.o x64/gccRelease/src/IPhreeqcLib.o x64/gccRelease/src/Var.o x64/gccRelease/src/CSelectedOutput.o x64/gccRelease/src/phreeqcpp/SelectedOutput.o x64/gccRelease/src/phreeqcpp/UserPunch.o x64/gccRelease/src/phreeqcpp/cxxKinetics.o x64/gccRelease/src/phreeqcpp/cxxMix.o x64/gccRelease/src/phreeqcpp/dumper.o x64/gccRelease/src/phreeqcpp/Exchange.o x64/gccRelease/src/phreeqcpp/ExchComp.o x64/gccRelease/src/phreeqcpp/GasComp.o x64/gccRelease/src/phreeqcpp/GasPhase.o x64/gccRelease/src/phreeqcpp/ISolution.o x64/gccRelease/src/phreeqcpp/ISolutionComp.o x64/gccRelease/src/phreeqcpp/Keywords.o x64/gccRelease/src/phreeqcpp/KineticsComp.o x64/gccRelease/src/phreeqcpp/NameDouble.o x64/gccRelease/src/phreeqcpp/NumKeyword.o x64/gccRelease/src/phreeqcpp/Parser.o x64/gccRelease/src/phreeqcpp/PBasic.o x64/gccRelease/src/phreeqcpp/Phreeqc.o x64/gccRelease/src/phreeqcpp/PHRQ_base.o x64/gccRelease/src/phreeqcpp/PHRQ_io.o x64/gccRelease/src/phreeqcpp/PPassemblage.o x64/gccRelease/src/phreeqcpp/PPassemblageComp.o x64/gccRelease/src/phreeqcpp/Pressure.o x64/gccRelease/src/phreeqcpp/Reaction.o x64/gccRelease/src/phreeqcpp/ReadClass.o x64/gccRelease/src/phreeqcpp/runner.o x64/gccRelease/src/phreeqcpp/Solution.o x64/gccRelease/src/phreeqcpp/SolutionIsotope.o x64/gccRelease/src/phreeqcpp/SS.o x64/gccRelease/src/phreeqcpp/SSassemblage.o x64/gccRelease/src/phreeqcpp/SScomp.o x64/gccRelease/src/phreeqcpp/StorageBin.o x64/gccRelease/src/phreeqcpp/StorageBinList.o x64/gccRelease/src/phreeqcpp/Surface.o x64/gccRelease/src/phreeqcpp/SurfaceCharge.o x64/gccRelease/src/phreeqcpp/SurfaceComp.o x64/gccRelease/src/phreeqcpp/System.o x64/gccRelease/src/phreeqcpp/Temperature.o x64/gccRelease/src/phreeqcpp/Use.o x64/gccRelease/src/phreeqcpp/Utils.o x64/gccRelease/src/phreeqcpp/advection.o x64/gccRelease/src/phreeqcpp/basicsubs.o x64/gccRelease/src/phreeqcpp/cl1.o x64/gccRelease/src/phreeqcpp/cvdense.o x64/gccRelease/src/phreeqcpp/cvode.o x64/gccRelease/src/phreeqcpp/dense.o x64/gccRelease/src/phreeqcpp/dw.o x64/gccRelease/src/phreeqcpp/gases.o x64/gccRelease/src/phreeqcpp/input.o x64/gccRelease/src/phreeqcpp/integrate.o x64/gccRelease/src/phreeqcpp/inverse.o x64/gccRelease/src/phreeqcpp/isotopes.o x64/gccRelease/src/phreeqcpp/kinetics.o x64/gccRelease/src/phreeqcpp/mainsubs.o x64/gccRelease/src/phreeqcpp/model.o x64/gccRelease/src/phreeqcpp/nvector.o x64/gccRelease/src/phreeqcpp/nvector_serial.o x64/gccRelease/src/phreeqcpp/parse.o x64/gccRelease/src/phreeqcpp/phqalloc.o x64/gccRelease/src/phreeqcpp/PHRQ_io_output.o x64/gccRelease/src/phreeqcpp/pitzer.o x64/gccRelease/src/phreeqcpp/pitzer_structures.o x64/gccRelease/src/phreeqcpp/prep.o x64/gccRelease/src/phreeqcpp/print.o x64/gccRelease/src/phreeqcpp/read.o x64/gccRelease/src/phreeqcpp/readtr.o x64/gccRelease/src/phreeqcpp/sit.o x64/gccRelease/src/phreeqcpp/smalldense.o x64/gccRelease/src/phreeqcpp/spread.o x64/gccRelease/src/phreeqcpp/step.o x64/gccRelease/src/phreeqcpp/structures.o x64/gccRelease/src/phreeqcpp/sundialsmath.o x64/gccRelease/src/phreeqcpp/tally.o x64/gccRelease/src/phreeqcpp/tidy.o x64/gccRelease/src/phreeqcpp/transport.o x64/gccRelease/src/phreeqcpp/utilities.o  $(Release_Implicitly_Linked_Objects)

# Compiles file src/fwrap.cpp for the Release configuration...
-include x64/gccRelease/src/fwrap.d
x64/gccRelease/src/fwrap.o: src/fwrap.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap.cpp $(Release_Include_Path) -o x64/gccRelease/src/fwrap.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap.cpp $(Release_Include_Path) > x64/gccRelease/src/fwrap.d

# Compiles file src/fwrap2.cpp for the Release configuration...
-include x64/gccRelease/src/fwrap2.d
x64/gccRelease/src/fwrap2.o: src/fwrap2.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap2.cpp $(Release_Include_Path) -o x64/gccRelease/src/fwrap2.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap2.cpp $(Release_Include_Path) > x64/gccRelease/src/fwrap2.d

# Compiles file src/fwrap3.cpp for the Release configuration...
-include x64/gccRelease/src/fwrap3.d
x64/gccRelease/src/fwrap3.o: src/fwrap3.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap3.cpp $(Release_Include_Path) -o x64/gccRelease/src/fwrap3.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap3.cpp $(Release_Include_Path) > x64/gccRelease/src/fwrap3.d

# Compiles file src/fwrap4.cpp for the Release configuration...
-include x64/gccRelease/src/fwrap4.d
x64/gccRelease/src/fwrap4.o: src/fwrap4.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap4.cpp $(Release_Include_Path) -o x64/gccRelease/src/fwrap4.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap4.cpp $(Release_Include_Path) > x64/gccRelease/src/fwrap4.d

# Compiles file src/fwrap5.cpp for the Release configuration...
-include x64/gccRelease/src/fwrap5.d
x64/gccRelease/src/fwrap5.o: src/fwrap5.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap5.cpp $(Release_Include_Path) -o x64/gccRelease/src/fwrap5.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap5.cpp $(Release_Include_Path) > x64/gccRelease/src/fwrap5.d

# Compiles file src/fwrap6.cpp for the Release configuration...
-include x64/gccRelease/src/fwrap6.d
x64/gccRelease/src/fwrap6.o: src/fwrap6.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap6.cpp $(Release_Include_Path) -o x64/gccRelease/src/fwrap6.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap6.cpp $(Release_Include_Path) > x64/gccRelease/src/fwrap6.d

# Compiles file src/fwrap7.cpp for the Release configuration...
-include x64/gccRelease/src/fwrap7.d
x64/gccRelease/src/fwrap7.o: src/fwrap7.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap7.cpp $(Release_Include_Path) -o x64/gccRelease/src/fwrap7.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap7.cpp $(Release_Include_Path) > x64/gccRelease/src/fwrap7.d

# Compiles file src/fwrap8.cpp for the Release configuration...
-include x64/gccRelease/src/fwrap8.d
x64/gccRelease/src/fwrap8.o: src/fwrap8.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/fwrap8.cpp $(Release_Include_Path) -o x64/gccRelease/src/fwrap8.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/fwrap8.cpp $(Release_Include_Path) > x64/gccRelease/src/fwrap8.d

# Compiles file src/IPhreeqc.cpp for the Release configuration...
-include x64/gccRelease/src/IPhreeqc.d
x64/gccRelease/src/IPhreeqc.o: src/IPhreeqc.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/IPhreeqc.cpp $(Release_Include_Path) -o x64/gccRelease/src/IPhreeqc.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/IPhreeqc.cpp $(Release_Include_Path) > x64/gccRelease/src/IPhreeqc.d

# Compiles file src/IPhreeqcLib.cpp for the Release configuration...
-include x64/gccRelease/src/IPhreeqcLib.d
x64/gccRelease/src/IPhreeqcLib.o: src/IPhreeqcLib.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/IPhreeqcLib.cpp $(Release_Include_Path) -o x64/gccRelease/src/IPhreeqcLib.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/IPhreeqcLib.cpp $(Release_Include_Path) > x64/gccRelease/src/IPhreeqcLib.d

# Compiles file src/Var.c for the Release configuration...
-include x64/gccRelease/src/Var.d
x64/gccRelease/src/Var.o: src/Var.c
	$(C_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/Var.c $(Release_Include_Path) -o x64/gccRelease/src/Var.o
	$(C_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/Var.c $(Release_Include_Path) > x64/gccRelease/src/Var.d

# Compiles file src/CSelectedOutput.cpp for the Release configuration...
-include x64/gccRelease/src/CSelectedOutput.d
x64/gccRelease/src/CSelectedOutput.o: src/CSelectedOutput.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/CSelectedOutput.cpp $(Release_Include_Path) -o x64/gccRelease/src/CSelectedOutput.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/CSelectedOutput.cpp $(Release_Include_Path) > x64/gccRelease/src/CSelectedOutput.d

# Compiles file src/phreeqcpp/SelectedOutput.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/SelectedOutput.d
x64/gccRelease/src/phreeqcpp/SelectedOutput.o: src/phreeqcpp/SelectedOutput.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SelectedOutput.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/SelectedOutput.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SelectedOutput.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/SelectedOutput.d

# Compiles file src/phreeqcpp/UserPunch.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/UserPunch.d
x64/gccRelease/src/phreeqcpp/UserPunch.o: src/phreeqcpp/UserPunch.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/UserPunch.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/UserPunch.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/UserPunch.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/UserPunch.d

# Compiles file src/phreeqcpp/cxxKinetics.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/cxxKinetics.d
x64/gccRelease/src/phreeqcpp/cxxKinetics.o: src/phreeqcpp/cxxKinetics.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/cxxKinetics.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/cxxKinetics.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/cxxKinetics.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/cxxKinetics.d

# Compiles file src/phreeqcpp/cxxMix.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/cxxMix.d
x64/gccRelease/src/phreeqcpp/cxxMix.o: src/phreeqcpp/cxxMix.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/cxxMix.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/cxxMix.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/cxxMix.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/cxxMix.d

# Compiles file src/phreeqcpp/dumper.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/dumper.d
x64/gccRelease/src/phreeqcpp/dumper.o: src/phreeqcpp/dumper.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/dumper.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/dumper.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/dumper.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/dumper.d

# Compiles file src/phreeqcpp/Exchange.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/Exchange.d
x64/gccRelease/src/phreeqcpp/Exchange.o: src/phreeqcpp/Exchange.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Exchange.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/Exchange.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Exchange.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/Exchange.d

# Compiles file src/phreeqcpp/ExchComp.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/ExchComp.d
x64/gccRelease/src/phreeqcpp/ExchComp.o: src/phreeqcpp/ExchComp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/ExchComp.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/ExchComp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/ExchComp.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/ExchComp.d

# Compiles file src/phreeqcpp/GasComp.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/GasComp.d
x64/gccRelease/src/phreeqcpp/GasComp.o: src/phreeqcpp/GasComp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/GasComp.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/GasComp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/GasComp.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/GasComp.d

# Compiles file src/phreeqcpp/GasPhase.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/GasPhase.d
x64/gccRelease/src/phreeqcpp/GasPhase.o: src/phreeqcpp/GasPhase.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/GasPhase.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/GasPhase.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/GasPhase.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/GasPhase.d

# Compiles file src/phreeqcpp/ISolution.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/ISolution.d
x64/gccRelease/src/phreeqcpp/ISolution.o: src/phreeqcpp/ISolution.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/ISolution.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/ISolution.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/ISolution.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/ISolution.d

# Compiles file src/phreeqcpp/ISolutionComp.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/ISolutionComp.d
x64/gccRelease/src/phreeqcpp/ISolutionComp.o: src/phreeqcpp/ISolutionComp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/ISolutionComp.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/ISolutionComp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/ISolutionComp.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/ISolutionComp.d

# Compiles file src/phreeqcpp/Keywords.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/Keywords.d
x64/gccRelease/src/phreeqcpp/Keywords.o: src/phreeqcpp/Keywords.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Keywords.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/Keywords.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Keywords.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/Keywords.d

# Compiles file src/phreeqcpp/KineticsComp.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/KineticsComp.d
x64/gccRelease/src/phreeqcpp/KineticsComp.o: src/phreeqcpp/KineticsComp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/KineticsComp.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/KineticsComp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/KineticsComp.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/KineticsComp.d

# Compiles file src/phreeqcpp/NameDouble.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/NameDouble.d
x64/gccRelease/src/phreeqcpp/NameDouble.o: src/phreeqcpp/NameDouble.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/NameDouble.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/NameDouble.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/NameDouble.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/NameDouble.d

# Compiles file src/phreeqcpp/NumKeyword.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/NumKeyword.d
x64/gccRelease/src/phreeqcpp/NumKeyword.o: src/phreeqcpp/NumKeyword.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/NumKeyword.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/NumKeyword.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/NumKeyword.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/NumKeyword.d

# Compiles file src/phreeqcpp/Parser.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/Parser.d
x64/gccRelease/src/phreeqcpp/Parser.o: src/phreeqcpp/Parser.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Parser.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/Parser.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Parser.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/Parser.d

# Compiles file src/phreeqcpp/PBasic.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/PBasic.d
x64/gccRelease/src/phreeqcpp/PBasic.o: src/phreeqcpp/PBasic.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/PBasic.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/PBasic.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/PBasic.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/PBasic.d

# Compiles file src/phreeqcpp/Phreeqc.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/Phreeqc.d
x64/gccRelease/src/phreeqcpp/Phreeqc.o: src/phreeqcpp/Phreeqc.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Phreeqc.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/Phreeqc.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Phreeqc.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/Phreeqc.d

# Compiles file src/phreeqcpp/PHRQ_base.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/PHRQ_base.d
x64/gccRelease/src/phreeqcpp/PHRQ_base.o: src/phreeqcpp/PHRQ_base.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/PHRQ_base.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/PHRQ_base.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/PHRQ_base.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/PHRQ_base.d

# Compiles file src/phreeqcpp/PHRQ_io.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/PHRQ_io.d
x64/gccRelease/src/phreeqcpp/PHRQ_io.o: src/phreeqcpp/PHRQ_io.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/PHRQ_io.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/PHRQ_io.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/PHRQ_io.d

# Compiles file src/phreeqcpp/PPassemblage.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/PPassemblage.d
x64/gccRelease/src/phreeqcpp/PPassemblage.o: src/phreeqcpp/PPassemblage.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/PPassemblage.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/PPassemblage.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/PPassemblage.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/PPassemblage.d

# Compiles file src/phreeqcpp/PPassemblageComp.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/PPassemblageComp.d
x64/gccRelease/src/phreeqcpp/PPassemblageComp.o: src/phreeqcpp/PPassemblageComp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/PPassemblageComp.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/PPassemblageComp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/PPassemblageComp.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/PPassemblageComp.d

# Compiles file src/phreeqcpp/Pressure.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/Pressure.d
x64/gccRelease/src/phreeqcpp/Pressure.o: src/phreeqcpp/Pressure.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Pressure.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/Pressure.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Pressure.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/Pressure.d

# Compiles file src/phreeqcpp/Reaction.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/Reaction.d
x64/gccRelease/src/phreeqcpp/Reaction.o: src/phreeqcpp/Reaction.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Reaction.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/Reaction.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Reaction.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/Reaction.d

# Compiles file src/phreeqcpp/ReadClass.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/ReadClass.d
x64/gccRelease/src/phreeqcpp/ReadClass.o: src/phreeqcpp/ReadClass.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/ReadClass.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/ReadClass.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/ReadClass.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/ReadClass.d

# Compiles file src/phreeqcpp/runner.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/runner.d
x64/gccRelease/src/phreeqcpp/runner.o: src/phreeqcpp/runner.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/runner.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/runner.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/runner.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/runner.d

# Compiles file src/phreeqcpp/Solution.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/Solution.d
x64/gccRelease/src/phreeqcpp/Solution.o: src/phreeqcpp/Solution.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Solution.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/Solution.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Solution.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/Solution.d

# Compiles file src/phreeqcpp/SolutionIsotope.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/SolutionIsotope.d
x64/gccRelease/src/phreeqcpp/SolutionIsotope.o: src/phreeqcpp/SolutionIsotope.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SolutionIsotope.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/SolutionIsotope.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SolutionIsotope.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/SolutionIsotope.d

# Compiles file src/phreeqcpp/SS.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/SS.d
x64/gccRelease/src/phreeqcpp/SS.o: src/phreeqcpp/SS.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SS.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/SS.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SS.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/SS.d

# Compiles file src/phreeqcpp/SSassemblage.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/SSassemblage.d
x64/gccRelease/src/phreeqcpp/SSassemblage.o: src/phreeqcpp/SSassemblage.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SSassemblage.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/SSassemblage.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SSassemblage.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/SSassemblage.d

# Compiles file src/phreeqcpp/SScomp.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/SScomp.d
x64/gccRelease/src/phreeqcpp/SScomp.o: src/phreeqcpp/SScomp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SScomp.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/SScomp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SScomp.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/SScomp.d

# Compiles file src/phreeqcpp/StorageBin.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/StorageBin.d
x64/gccRelease/src/phreeqcpp/StorageBin.o: src/phreeqcpp/StorageBin.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/StorageBin.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/StorageBin.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/StorageBin.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/StorageBin.d

# Compiles file src/phreeqcpp/StorageBinList.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/StorageBinList.d
x64/gccRelease/src/phreeqcpp/StorageBinList.o: src/phreeqcpp/StorageBinList.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/StorageBinList.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/StorageBinList.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/StorageBinList.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/StorageBinList.d

# Compiles file src/phreeqcpp/Surface.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/Surface.d
x64/gccRelease/src/phreeqcpp/Surface.o: src/phreeqcpp/Surface.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Surface.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/Surface.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Surface.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/Surface.d

# Compiles file src/phreeqcpp/SurfaceCharge.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/SurfaceCharge.d
x64/gccRelease/src/phreeqcpp/SurfaceCharge.o: src/phreeqcpp/SurfaceCharge.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SurfaceCharge.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/SurfaceCharge.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SurfaceCharge.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/SurfaceCharge.d

# Compiles file src/phreeqcpp/SurfaceComp.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/SurfaceComp.d
x64/gccRelease/src/phreeqcpp/SurfaceComp.o: src/phreeqcpp/SurfaceComp.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/SurfaceComp.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/SurfaceComp.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/SurfaceComp.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/SurfaceComp.d

# Compiles file src/phreeqcpp/System.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/System.d
x64/gccRelease/src/phreeqcpp/System.o: src/phreeqcpp/System.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/System.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/System.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/System.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/System.d

# Compiles file src/phreeqcpp/Temperature.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/Temperature.d
x64/gccRelease/src/phreeqcpp/Temperature.o: src/phreeqcpp/Temperature.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Temperature.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/Temperature.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Temperature.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/Temperature.d

# Compiles file src/phreeqcpp/Use.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/Use.d
x64/gccRelease/src/phreeqcpp/Use.o: src/phreeqcpp/Use.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Use.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/Use.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Use.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/Use.d

# Compiles file src/phreeqcpp/Utils.cxx for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/Utils.d
x64/gccRelease/src/phreeqcpp/Utils.o: src/phreeqcpp/Utils.cxx
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/Utils.cxx $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/Utils.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/Utils.cxx $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/Utils.d

# Compiles file src/phreeqcpp/advection.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/advection.d
x64/gccRelease/src/phreeqcpp/advection.o: src/phreeqcpp/advection.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/advection.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/advection.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/advection.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/advection.d

# Compiles file src/phreeqcpp/basicsubs.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/basicsubs.d
x64/gccRelease/src/phreeqcpp/basicsubs.o: src/phreeqcpp/basicsubs.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/basicsubs.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/basicsubs.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/basicsubs.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/basicsubs.d

# Compiles file src/phreeqcpp/cl1.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/cl1.d
x64/gccRelease/src/phreeqcpp/cl1.o: src/phreeqcpp/cl1.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/cl1.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/cl1.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/cl1.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/cl1.d

# Compiles file src/phreeqcpp/cvdense.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/cvdense.d
x64/gccRelease/src/phreeqcpp/cvdense.o: src/phreeqcpp/cvdense.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/cvdense.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/cvdense.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/cvdense.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/cvdense.d

# Compiles file src/phreeqcpp/cvode.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/cvode.d
x64/gccRelease/src/phreeqcpp/cvode.o: src/phreeqcpp/cvode.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/cvode.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/cvode.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/cvode.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/cvode.d

# Compiles file src/phreeqcpp/dense.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/dense.d
x64/gccRelease/src/phreeqcpp/dense.o: src/phreeqcpp/dense.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/dense.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/dense.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/dense.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/dense.d

# Compiles file src/phreeqcpp/dw.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/dw.d
x64/gccRelease/src/phreeqcpp/dw.o: src/phreeqcpp/dw.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/dw.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/dw.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/dw.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/dw.d

# Compiles file src/phreeqcpp/gases.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/gases.d
x64/gccRelease/src/phreeqcpp/gases.o: src/phreeqcpp/gases.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/gases.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/gases.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/gases.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/gases.d

# Compiles file src/phreeqcpp/input.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/input.d
x64/gccRelease/src/phreeqcpp/input.o: src/phreeqcpp/input.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/input.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/input.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/input.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/input.d

# Compiles file src/phreeqcpp/integrate.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/integrate.d
x64/gccRelease/src/phreeqcpp/integrate.o: src/phreeqcpp/integrate.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/integrate.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/integrate.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/integrate.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/integrate.d

# Compiles file src/phreeqcpp/inverse.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/inverse.d
x64/gccRelease/src/phreeqcpp/inverse.o: src/phreeqcpp/inverse.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/inverse.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/inverse.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/inverse.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/inverse.d

# Compiles file src/phreeqcpp/isotopes.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/isotopes.d
x64/gccRelease/src/phreeqcpp/isotopes.o: src/phreeqcpp/isotopes.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/isotopes.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/isotopes.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/isotopes.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/isotopes.d

# Compiles file src/phreeqcpp/kinetics.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/kinetics.d
x64/gccRelease/src/phreeqcpp/kinetics.o: src/phreeqcpp/kinetics.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/kinetics.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/kinetics.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/kinetics.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/kinetics.d

# Compiles file src/phreeqcpp/mainsubs.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/mainsubs.d
x64/gccRelease/src/phreeqcpp/mainsubs.o: src/phreeqcpp/mainsubs.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/mainsubs.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/mainsubs.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/mainsubs.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/mainsubs.d

# Compiles file src/phreeqcpp/model.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/model.d
x64/gccRelease/src/phreeqcpp/model.o: src/phreeqcpp/model.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/model.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/model.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/model.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/model.d

# Compiles file src/phreeqcpp/nvector.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/nvector.d
x64/gccRelease/src/phreeqcpp/nvector.o: src/phreeqcpp/nvector.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/nvector.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/nvector.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/nvector.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/nvector.d

# Compiles file src/phreeqcpp/nvector_serial.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/nvector_serial.d
x64/gccRelease/src/phreeqcpp/nvector_serial.o: src/phreeqcpp/nvector_serial.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/nvector_serial.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/nvector_serial.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/nvector_serial.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/nvector_serial.d

# Compiles file src/phreeqcpp/parse.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/parse.d
x64/gccRelease/src/phreeqcpp/parse.o: src/phreeqcpp/parse.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/parse.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/parse.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/parse.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/parse.d

# Compiles file src/phreeqcpp/phqalloc.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/phqalloc.d
x64/gccRelease/src/phreeqcpp/phqalloc.o: src/phreeqcpp/phqalloc.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/phqalloc.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/phqalloc.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/phqalloc.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/phqalloc.d

# Compiles file src/phreeqcpp/PHRQ_io_output.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/PHRQ_io_output.d
x64/gccRelease/src/phreeqcpp/PHRQ_io_output.o: src/phreeqcpp/PHRQ_io_output.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/PHRQ_io_output.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/PHRQ_io_output.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/PHRQ_io_output.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/PHRQ_io_output.d

# Compiles file src/phreeqcpp/pitzer.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/pitzer.d
x64/gccRelease/src/phreeqcpp/pitzer.o: src/phreeqcpp/pitzer.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/pitzer.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/pitzer.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/pitzer.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/pitzer.d

# Compiles file src/phreeqcpp/pitzer_structures.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/pitzer_structures.d
x64/gccRelease/src/phreeqcpp/pitzer_structures.o: src/phreeqcpp/pitzer_structures.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/pitzer_structures.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/pitzer_structures.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/pitzer_structures.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/pitzer_structures.d

# Compiles file src/phreeqcpp/prep.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/prep.d
x64/gccRelease/src/phreeqcpp/prep.o: src/phreeqcpp/prep.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/prep.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/prep.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/prep.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/prep.d

# Compiles file src/phreeqcpp/print.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/print.d
x64/gccRelease/src/phreeqcpp/print.o: src/phreeqcpp/print.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/print.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/print.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/print.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/print.d

# Compiles file src/phreeqcpp/read.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/read.d
x64/gccRelease/src/phreeqcpp/read.o: src/phreeqcpp/read.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/read.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/read.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/read.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/read.d

# Compiles file src/phreeqcpp/readtr.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/readtr.d
x64/gccRelease/src/phreeqcpp/readtr.o: src/phreeqcpp/readtr.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/readtr.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/readtr.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/readtr.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/readtr.d

# Compiles file src/phreeqcpp/sit.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/sit.d
x64/gccRelease/src/phreeqcpp/sit.o: src/phreeqcpp/sit.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/sit.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/sit.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/sit.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/sit.d

# Compiles file src/phreeqcpp/smalldense.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/smalldense.d
x64/gccRelease/src/phreeqcpp/smalldense.o: src/phreeqcpp/smalldense.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/smalldense.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/smalldense.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/smalldense.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/smalldense.d

# Compiles file src/phreeqcpp/spread.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/spread.d
x64/gccRelease/src/phreeqcpp/spread.o: src/phreeqcpp/spread.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/spread.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/spread.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/spread.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/spread.d

# Compiles file src/phreeqcpp/step.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/step.d
x64/gccRelease/src/phreeqcpp/step.o: src/phreeqcpp/step.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/step.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/step.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/step.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/step.d

# Compiles file src/phreeqcpp/structures.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/structures.d
x64/gccRelease/src/phreeqcpp/structures.o: src/phreeqcpp/structures.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/structures.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/structures.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/structures.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/structures.d

# Compiles file src/phreeqcpp/sundialsmath.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/sundialsmath.d
x64/gccRelease/src/phreeqcpp/sundialsmath.o: src/phreeqcpp/sundialsmath.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/sundialsmath.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/sundialsmath.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/sundialsmath.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/sundialsmath.d

# Compiles file src/phreeqcpp/tally.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/tally.d
x64/gccRelease/src/phreeqcpp/tally.o: src/phreeqcpp/tally.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/tally.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/tally.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/tally.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/tally.d

# Compiles file src/phreeqcpp/tidy.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/tidy.d
x64/gccRelease/src/phreeqcpp/tidy.o: src/phreeqcpp/tidy.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/tidy.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/tidy.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/tidy.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/tidy.d

# Compiles file src/phreeqcpp/transport.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/transport.d
x64/gccRelease/src/phreeqcpp/transport.o: src/phreeqcpp/transport.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/transport.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/transport.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/transport.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/transport.d

# Compiles file src/phreeqcpp/utilities.cpp for the Release configuration...
-include x64/gccRelease/src/phreeqcpp/utilities.d
x64/gccRelease/src/phreeqcpp/utilities.o: src/phreeqcpp/utilities.cpp
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -c src/phreeqcpp/utilities.cpp $(Release_Include_Path) -o x64/gccRelease/src/phreeqcpp/utilities.o
	$(CPP_COMPILER) $(Release_Preprocessor_Definitions) $(Release_Compiler_Flags) -MM src/phreeqcpp/utilities.cpp $(Release_Include_Path) > x64/gccRelease/src/phreeqcpp/utilities.d

# Creates the intermediate and output folders for each configuration...
.PHONY: create_folders
create_folders:
	mkdir -p gccDebugDll/src
	mkdir -p ../../gccDebugDll
	mkdir -p x64/gccDebugDll/src
	mkdir -p ../../x64/gccDebugDll
	mkdir -p gccDebug/src
	mkdir -p ../../gccDebug
	mkdir -p x64/gccDebug/src
	mkdir -p ../../x64/gccDebug
	mkdir -p gccReleaseDll/src
	mkdir -p ../../gccReleaseDll
	mkdir -p x64/gccReleaseDll/src
	mkdir -p ../../x64/gccReleaseDll
	mkdir -p gccRelease/src
	mkdir -p ../../gccRelease
	mkdir -p x64/gccRelease/src
	mkdir -p ../../x64/gccRelease

# Cleans intermediate and output files (objects, libraries, executables)...
.PHONY: clean
clean:
	rm -f gccDebugDll/*.o
	rm -f gccDebugDll/*.d
	rm -f ../../gccDebugDll/*.a
	rm -f ../../gccDebugDll/*.so
	rm -f ../../gccDebugDll/*.dll
	rm -f ../../gccDebugDll/*.exe
	rm -f x64/gccDebugDll/*.o
	rm -f x64/gccDebugDll/*.d
	rm -f ../../x64/gccDebugDll/*.a
	rm -f ../../x64/gccDebugDll/*.so
	rm -f ../../x64/gccDebugDll/*.dll
	rm -f ../../x64/gccDebugDll/*.exe
	rm -f gccDebug/*.o
	rm -f gccDebug/*.d
	rm -f ../../gccDebug/*.a
	rm -f ../../gccDebug/*.so
	rm -f ../../gccDebug/*.dll
	rm -f ../../gccDebug/*.exe
	rm -f x64/gccDebug/*.o
	rm -f x64/gccDebug/*.d
	rm -f ../../x64/gccDebug/*.a
	rm -f ../../x64/gccDebug/*.so
	rm -f ../../x64/gccDebug/*.dll
	rm -f ../../x64/gccDebug/*.exe
	rm -f gccReleaseDll/*.o
	rm -f gccReleaseDll/*.d
	rm -f ../../gccReleaseDll/*.a
	rm -f ../../gccReleaseDll/*.so
	rm -f ../../gccReleaseDll/*.dll
	rm -f ../../gccReleaseDll/*.exe
	rm -f x64/gccReleaseDll/*.o
	rm -f x64/gccReleaseDll/*.d
	rm -f ../../x64/gccReleaseDll/*.a
	rm -f ../../x64/gccReleaseDll/*.so
	rm -f ../../x64/gccReleaseDll/*.dll
	rm -f ../../x64/gccReleaseDll/*.exe
	rm -f gccRelease/*.o
	rm -f gccRelease/*.d
	rm -f ../../gccRelease/*.a
	rm -f ../../gccRelease/*.so
	rm -f ../../gccRelease/*.dll
	rm -f ../../gccRelease/*.exe
	rm -f x64/gccRelease/*.o
	rm -f x64/gccRelease/*.d
	rm -f ../../x64/gccRelease/*.a
	rm -f ../../x64/gccRelease/*.so
	rm -f ../../x64/gccRelease/*.dll
	rm -f ../../x64/gccRelease/*.exe

