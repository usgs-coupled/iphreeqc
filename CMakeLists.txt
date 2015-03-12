cmake_minimum_required (VERSION 2.8.11)

##include(CheckLanguage)
##check_language(Fortran)
##if (CMAKE_Fortran_COMPILER)
##  enable_language(Fortran)
##endif()

if (POLICY CMP0054)
  cmake_policy(PUSH)
  cmake_policy(SET CMP0054 OLD)
endif()
project (IPhreeqc CXX C)
if (POLICY CMP0054)
  cmake_policy(POP)
endif()

### overide docdir on windows
##if(WIN32 AND NOT CMAKE_INSTALL_DOCDIR)
##  set(CMAKE_INSTALL_DOCDIR "" CACHE PATH "documentation root (doc)")
##  set(CMAKE_INSTALL_DOCDIR "doc")
##endif()

### Maybe this should be in subdir Tests
###
##if (NOT PHREEQCRM_BUILD_FORTRAN)
##  if (NOT CMAKE_Fortran_COMPILER OR CMAKE_Fortran_COMPILER STREQUAL "CMAKE_Fortran_COMPILER-NOTFOUND")
##    option (PHREEQCRM_BUILD_FORTRAN "Build FORTRAN support" OFF)
##  else()
##    option (PHREEQCRM_BUILD_FORTRAN "Build FORTRAN support" ON)
##  endif()
##else()
##  if (PHREEQCRM_BUILD_FORTRAN)
##    enable_language(Fortran)
##    ##include(FortranCInterface)
##    ##FortranCInterface_HEADER(FC.h MACRO_NAMESPACE "FC_")
##    ##FortranCInterface_VERIFY(CXX QUIET)
##  endif()  
##endif()

include(GNUInstallDirs)

# Disable deprecation warnings for standard C and STL functions in VS2005
# and later
IF(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400)
  ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE)
  ADD_DEFINITIONS(-D_SCL_SECURE_NO_WARNINGS)
ENDIF(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400) 
ADD_DEFINITIONS(-DSWIG_SHARED_OBJ)
ADD_DEFINITIONS(-DUSE_PHRQ_ALLOC)


SET(IPhreeqc_SOURCES
src/thread.h
src/CSelectedOutput.cpp
src/CSelectedOutput.hxx
src/CVar.hxx
src/Debug.h
src/ErrorReporter.hxx
src/IPhreeqc.cpp
src/IPhreeqc.h
src/IPhreeqc.hpp
src/IPhreeqcCallbacks.h
src/IPhreeqcLib.cpp
src/Var.c
src/Var.h
src/Version.h
src/phreeqcpp/advection.cpp
src/phreeqcpp/basicsubs.cpp
src/phreeqcpp/cl1.cpp
src/phreeqcpp/cvdense.cpp
src/phreeqcpp/cvdense.h
src/phreeqcpp/cvode.cpp
src/phreeqcpp/cvode.h
src/phreeqcpp/cxxKinetics.cxx
src/phreeqcpp/cxxKinetics.h
src/phreeqcpp/cxxMix.cxx
src/phreeqcpp/cxxMix.h
src/phreeqcpp/dense.cpp
src/phreeqcpp/dense.h
src/phreeqcpp/dumper.cpp
src/phreeqcpp/dumper.h
src/phreeqcpp/dw.cpp
src/phreeqcpp/gases.cpp
src/phreeqcpp/global_structures.h
src/phreeqcpp/input.cpp
src/phreeqcpp/integrate.cpp
src/phreeqcpp/inverse.cpp
src/phreeqcpp/isotopes.cpp
src/phreeqcpp/kinetics.cpp
src/phreeqcpp/mainsubs.cpp
src/phreeqcpp/model.cpp
src/phreeqcpp/nvector.cpp
src/phreeqcpp/nvector.h
src/phreeqcpp/nvector_serial.cpp
src/phreeqcpp/nvector_serial.h
src/phreeqcpp/parse.cpp
src/phreeqcpp/phqalloc.cpp
src/phreeqcpp/phqalloc.h
src/phreeqcpp/phrqtype.h
src/phreeqcpp/pitzer.cpp
src/phreeqcpp/pitzer_structures.cpp
src/phreeqcpp/prep.cpp
src/phreeqcpp/print.cpp
src/phreeqcpp/read.cpp
src/phreeqcpp/readtr.cpp
src/phreeqcpp/runner.cpp
src/phreeqcpp/runner.h
src/phreeqcpp/sit.cpp
src/phreeqcpp/smalldense.cpp
src/phreeqcpp/smalldense.h
src/phreeqcpp/spread.cpp
src/phreeqcpp/step.cpp
src/phreeqcpp/structures.cpp
src/phreeqcpp/sundialsmath.cpp
src/phreeqcpp/sundialsmath.h
src/phreeqcpp/sundialstypes.h
src/phreeqcpp/tally.cpp
src/phreeqcpp/tidy.cpp
src/phreeqcpp/transport.cpp
src/phreeqcpp/utilities.cpp
src/phreeqcpp/ChartHandler.cpp
src/phreeqcpp/ChartHandler.h
src/phreeqcpp/ChartObject.cpp
src/phreeqcpp/ChartObject.h
src/phreeqcpp/CurveObject.cpp
src/phreeqcpp/CurveObject.h
src/phreeqcpp/ExchComp.cxx
src/phreeqcpp/ExchComp.h
src/phreeqcpp/Exchange.cxx
src/phreeqcpp/Exchange.h
src/phreeqcpp/GasComp.cxx
src/phreeqcpp/GasComp.h
src/phreeqcpp/GasPhase.cxx
src/phreeqcpp/GasPhase.h
src/phreeqcpp/ISolution.cxx
src/phreeqcpp/ISolution.h
src/phreeqcpp/ISolutionComp.cxx
src/phreeqcpp/ISolutionComp.h
src/phreeqcpp/KineticsComp.cxx
src/phreeqcpp/KineticsComp.h
src/phreeqcpp/NA.h
src/phreeqcpp/NameDouble.cxx
src/phreeqcpp/NameDouble.h
src/phreeqcpp/NumKeyword.cxx
src/phreeqcpp/NumKeyword.h
src/phreeqcpp/PBasic.cpp
src/phreeqcpp/PBasic.h
src/phreeqcpp/PHRQ_base.cxx
src/phreeqcpp/PHRQ_base.h
src/phreeqcpp/PHRQ_io.cpp
src/phreeqcpp/PHRQ_io.h
src/phreeqcpp/PHRQ_io_output.cpp
src/phreeqcpp/PPassemblage.cxx
src/phreeqcpp/PPassemblage.h
src/phreeqcpp/PPassemblageComp.cxx
src/phreeqcpp/PPassemblageComp.h
src/phreeqcpp/Parser.cxx
src/phreeqcpp/Parser.h
src/phreeqcpp/Phreeqc.cpp
src/phreeqcpp/Phreeqc.h
src/phreeqcpp/PhreeqcKeywords/Keywords.cpp
src/phreeqcpp/PhreeqcKeywords/Keywords.h
src/phreeqcpp/Pressure.cxx
src/phreeqcpp/Pressure.h
src/phreeqcpp/Reaction.cxx
src/phreeqcpp/Reaction.h
src/phreeqcpp/ReadClass.cxx
src/phreeqcpp/SS.cxx
src/phreeqcpp/SS.h
src/phreeqcpp/SSassemblage.cxx
src/phreeqcpp/SSassemblage.h
src/phreeqcpp/SScomp.cxx
src/phreeqcpp/SScomp.h
src/phreeqcpp/SelectedOutput.cpp
src/phreeqcpp/SelectedOutput.h
src/phreeqcpp/Solution.cxx
src/phreeqcpp/Solution.h
src/phreeqcpp/SolutionIsotope.cxx
src/phreeqcpp/SolutionIsotope.h
src/phreeqcpp/StorageBin.cxx
src/phreeqcpp/StorageBin.h
src/phreeqcpp/StorageBinList.cpp
src/phreeqcpp/StorageBinList.h
src/phreeqcpp/Surface.cxx
src/phreeqcpp/Surface.h
src/phreeqcpp/SurfaceCharge.cxx
src/phreeqcpp/SurfaceCharge.h
src/phreeqcpp/SurfaceComp.cxx
src/phreeqcpp/SurfaceComp.h
src/phreeqcpp/System.cxx
src/phreeqcpp/System.h
src/phreeqcpp/Temperature.cxx
src/phreeqcpp/Temperature.h
src/phreeqcpp/Use.cpp
src/phreeqcpp/Use.h
src/phreeqcpp/UserPunch.cpp
src/phreeqcpp/UserPunch.h
src/phreeqcpp/Utils.cxx
src/phreeqcpp/Utils.h
)

# compile Var.c as c++
SET_SOURCE_FILES_PROPERTIES(src/Var.c PROPERTIES LANGUAGE CXX)

### maybe always include?
##if (PHREEQCRM_BUILD_FORTRAN)
##  SET(PhreeqcRM_SOURCES
##  ${PhreeqcRM_SOURCES}
##  RM_interface_F.cpp
##  )
##endif ()


if (IPHREEQC_BUILD_FORTRAN AND MSVC)
  SET(IPhreeqc_SOURCES
  ${IPhreeqc_SOURCES}
  src/fwrap.cpp
  src/fwrap1.cpp
  src/fwrap2.cpp
  src/fwrap3.cpp
  src/fwrap4.cpp
  src/fwrap5.cpp
  src/fwrap6.cpp      
  src/fwrap7.cpp
  src/fwrap8.cpp      
  )
endif ()

# setup include directories
include_directories("${PROJECT_SOURCE_DIR}")
include_directories("${PROJECT_SOURCE_DIR}/src")
include_directories("${PROJECT_SOURCE_DIR}/src/phreeqcpp")
include_directories("${PROJECT_SOURCE_DIR}/src/phreeqcpp/PhreeqcKeywords")


OPTION(BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
SET(LIB_TYPE STATIC)
if (BUILD_SHARED_LIBS)
  SET(LIB_TYPE SHARED)
  if (MSVC)
    OPTION(BUILD_CLR_LIBS "Build CLR Libraries" OFF)
  endif()
endif()
add_library(IPhreeqc ${LIB_TYPE} ${IPhreeqc_SOURCES})
set_target_properties(IPhreeqc PROPERTIES DEBUG_POSTFIX "d")

SET(IPhreeqc_Headers
${PROJECT_SOURCE_DIR}/src/IPhreeqc.h
${PROJECT_SOURCE_DIR}/src/IPhreeqc.hpp
${PROJECT_SOURCE_DIR}/src/IPhreeqcCallbacks.h
${PROJECT_SOURCE_DIR}/src/Var.h
)

##if (PHREEQCRM_BUILD_FORTRAN)
##  SET(PhreeqcRM_Headers
##  ${PhreeqcRM_Headers}
##  $(PROJECT_SOURCE_DIR)/RM_interface_F.h
##  $(PROJECT_SOURCE_DIR)/IPhreeqcPhast/IPhreeqc/IPhreeqc.f90.inc
##  $(PROJECT_SOURCE_DIR)/IPhreeqcPhast/IPhreeqc/IPhreeqc.f.inc
##  )
##endif ()

# Setup references for /CLR
if (MSVC AND BUILD_SHARED_LIBS AND BUILD_CLR_LIBS)
  set_target_properties(IPhreeqc PROPERTIES VS_DOTNET_REFERENCES "System;System.Drawing;System.Windows.Forms;${CMAKE_CURRENT_SOURCE_DIR}/src/phreeqcpp/ZedGraph.dll")  
  # Remove /EHsc from CMAKE_CXX_FLAGS
  STRING(REPLACE " /EHsc" " /EHa /clr" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
  # Remove /RTC1 from CMAKE_CXX_FLAGS_DEBUG
  STRING(REPLACE " /RTC1" "" CMAKE_CXX_FLAGS_DEBUG ${CMAKE_CXX_FLAGS_DEBUG})
  ADD_DEFINITIONS(-DMULTICHART)  
endif()

if (BUILD_CLR_LIBS AND MSVC AND MSVC_VERSION < 1600)
  message(FATAL_ERROR "CLR options must be set manually for Visual Studio versions prior to 2010")
endif()

if (MSVC10)
  # turn off warnings on VS2010
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4793 /wd4945")
endif()


# install
if (WIN32)
  if (BUILD_CLR_LIBS)
    if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
      SET(CMAKE_INSTALL_LIBDIR "clrx64")
    else()
      SET(CMAKE_INSTALL_LIBDIR "clr")
    endif()
  else()
    if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
      if (BUILD_SHARED_LIBS)
        SET(CMAKE_INSTALL_LIBDIR "dllx64")
      else()
        SET(CMAKE_INSTALL_LIBDIR "libx64")
      endif()
    else()
      if (BUILD_SHARED_LIBS)
        SET(CMAKE_INSTALL_LIBDIR "dll")
      else()
        SET(CMAKE_INSTALL_LIBDIR "lib")
      endif()
    endif()
  endif()
endif()

# install
install(TARGETS IPhreeqc DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES ${IPhreeqc_Headers} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
if (BUILD_CLR_LIBS)
  install(FILES "${PROJECT_SOURCE_DIR}/src/phreeqcpp/ZedGraph.dll" DESTINATION ${CMAKE_INSTALL_LIBDIR})
endif()

##IF(MSVC)
##  message("PROJECT_BINARY_DIR = ${PROJECT_BINARY_DIR}")
##  INSTALL(
##    DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
##    DESTINATION ${CMAKE_INSTALL_LIBDIR}
##    CONFIGURATIONS Debug
##    FILES_MATCHING
##    PATTERN "*.pdb"
##    )
##  INSTALL(
##    FILES
##    ${LIBRARY_OUTPUT_PATH}/Debug/${target_prefix}${target_name}.pdb
##    DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
##    DESTINATION ${CMAKE_INSTALL_LIBDIR}
##    CONFIGURATIONS Debug
##    )
##ENDIF(MSVC)

### tests
##enable_testing()
##
##add_subdirectory(Tests)
