# Microsoft Developer Studio Project File - Name="unit" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=unit - Win32 Profile
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "unit.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "unit.mak" CFG="unit - Win32 Profile"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "unit - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "unit - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "unit - Win32 Profile" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "unit - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GR /GX /O2 /I "$(DEV_CPPUNIT_INC)" /D "SWIG_SHARED_OBJ" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 cppunit.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386 /libpath:"$(DEV_CPPUNIT_LIB)"
# SUBTRACT LINK32 /profile

!ELSEIF  "$(CFG)" == "unit - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "$(DEV_CPPUNIT_INC)" /D "SWIG_SHARED_OBJ" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 cppunitd.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib Shlwapi.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept /libpath:"$(DEV_CPPUNIT_LIB)"

!ELSEIF  "$(CFG)" == "unit - Win32 Profile"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Profile"
# PROP BASE Intermediate_Dir "Profile"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Profile"
# PROP Intermediate_Dir "Profile"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "$(DEV_CPPUNIT_INC)" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "$(DEV_CPPUNIT_INC)" /D "SWIG_SHARED_OBJ" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 cppunitd.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept /libpath:"$(DEV_CPPUNIT_LIB)"
# ADD LINK32 cppunitd.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /profile /debug /machine:I386 /libpath:"$(DEV_CPPUNIT_LIB)"

!ENDIF 

# Begin Target

# Name "unit - Win32 Release"
# Name "unit - Win32 Debug"
# Name "unit - Win32 Profile"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Group "src"

# PROP Default_Filter ""
# Begin Group "phreeqcpp"

# PROP Default_Filter ""
# Begin Group "phreeqc"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\advection.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\basic.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\basicsubs.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\cl1.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\cl1mp.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\cvdense.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\cvode.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\dense.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\dw.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\input.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\integrate.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\inverse.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\isotopes.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\kinetics.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\main.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\mainsubs.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\model.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\nvector.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\nvector_serial.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\output.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\p2clib.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\parse.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\phqalloc.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\phreeqc_files.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\pitzer.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\pitzer_structures.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\prep.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\print.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\read.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\readtr.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\smalldense.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\spread.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\step.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\structures.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\sundialsmath.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\tally.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\tidy.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\transport.c
# End Source File
# Begin Source File

SOURCE=..\src\phreeqcpp\phreeqc\utilities.c
# End Source File
# End Group
# End Group
# Begin Source File

SOURCE=..\src\fwrap.c
# End Source File
# Begin Source File

SOURCE=..\src\global.c
# End Source File
# Begin Source File

SOURCE=..\src\IPhreeqc.cxx
# End Source File
# Begin Source File

SOURCE=..\src\module_files.c
# End Source File
# Begin Source File

SOURCE=..\src\module_output.c
# End Source File
# Begin Source File

SOURCE=..\src\Phreeqc.cxx
# End Source File
# Begin Source File

SOURCE=..\src\SelectedOutput.cxx
# End Source File
# Begin Source File

SOURCE=..\src\Var.c
# End Source File
# End Group
# Begin Source File

SOURCE=.\TestCVar.cpp
# End Source File
# Begin Source File

SOURCE=.\TestInterface.cpp
# End Source File
# Begin Source File

SOURCE=.\TestSelectedOutput.cpp
# End Source File
# Begin Source File

SOURCE=.\TestVar.cpp
# End Source File
# Begin Source File

SOURCE=.\unit.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\src\CVar.hxx
# End Source File
# Begin Source File

SOURCE=..\src\ErrorReporter.hxx
# End Source File
# Begin Source File

SOURCE=..\include\IPhreeqc.h
# End Source File
# Begin Source File

SOURCE=..\src\OutputFiles.hxx
# End Source File
# Begin Source File

SOURCE=..\src\Phreeqc.hxx
# End Source File
# Begin Source File

SOURCE=..\src\PhreeqcParser.hxx
# End Source File
# Begin Source File

SOURCE=..\src\SelectedOutput.hxx
# End Source File
# Begin Source File

SOURCE=.\TestCVar.h
# End Source File
# Begin Source File

SOURCE=.\TestInterface.h
# End Source File
# Begin Source File

SOURCE=.\TestSelectedOutput.h
# End Source File
# Begin Source File

SOURCE=.\TestVar.h
# End Source File
# Begin Source File

SOURCE=..\include\Var.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
