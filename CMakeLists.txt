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


### Copy Release to openmp
##SET(CMAKE_CXX_FLAGS_OPENMP
##    ${CMAKE_CXX_FLAGS_RELEASE} CACHE STRING
##    "Flags used by the C++ compiler during openmp builds."
##    FORCE)
##SET(CMAKE_C_FLAGS_OPENMP
##    ${CMAKE_C_FLAGS_RELEASE} CACHE STRING
##    "Flags used by the C compiler during openmp builds."
##    FORCE)
##if (PHREEQCRM_BUILD_FORTRAN)
##  SET(CMAKE_Fortran_FLAGS_OPENMP
##      ${CMAKE_Fortran_FLAGS_RELEASE} CACHE STRING
##      "Flags used by the Fortran compiler during openmp builds."
##      FORCE)
##endif ()
##SET(CMAKE_EXE_LINKER_FLAGS_OPENMP
##    ${CMAKE_EXE_LINKER_FLAGS_RELEASE} CACHE STRING
##    "Flags used for linking binaries during openmp builds."
##    FORCE)
##SET(CMAKE_SHARED_LINKER_FLAGS_OPENMP
##    ${CMAKE_SHARED_LINKER_FLAGS_RELEASE} CACHE STRING
##    "Flags used by the shared libraries linker during openmp builds."
##    FORCE)
##MARK_AS_ADVANCED(
##    CMAKE_CXX_FLAGS_OPENMP
##    CMAKE_C_FLAGS_OPENMP
##    CMAKE_EXE_LINKER_FLAGS_OPENMP
##    CMAKE_SHARED_LINKER_FLAGS_OPENMP)
##if (PHREEQCRM_BUILD_FORTRAN)
##  MARK_AS_ADVANCED(CMAKE_Fortran_FLAGS_OPENMP)
##endif()

    
### Copy Debug to openmp_debug
##SET(CMAKE_CXX_FLAGS_OPENMP_DEBUG
##    ${CMAKE_CXX_FLAGS_DEBUG} CACHE STRING
##    "Flags used by the C++ compiler during debug openmp builds."
##    FORCE)
##SET(CMAKE_C_FLAGS_OPENMP_DEBUG
##    ${CMAKE_C_FLAGS_DEBUG} CACHE STRING
##    "Flags used by the C compiler during debug openmp builds."
##    FORCE)
##if (PHREEQCRM_BUILD_FORTRAN)
##  SET(CMAKE_Fortran_FLAGS_OPENMP_DEBUG
##      ${CMAKE_Fortran_FLAGS_DEBUG} CACHE STRING
##      "Flags used by the Fortran compiler during debug openmp builds."
##      FORCE)
##endif ()
##SET(CMAKE_EXE_LINKER_FLAGS_OPENMP_DEBUG
##    ${CMAKE_EXE_LINKER_FLAGS_DEBUG} CACHE STRING
##    "Flags used for linking binaries during debug openmp builds."
##    FORCE)
##SET(CMAKE_SHARED_LINKER_FLAGS_OPENMP_DEBUG
##    ${CMAKE_SHARED_LINKER_FLAGS_DEBUG} CACHE STRING
##    "Flags used by the shared libraries linker during debug openmp builds."
##    FORCE)
##MARK_AS_ADVANCED(
##    CMAKE_CXX_FLAGS_OPENMP_DEBUG
##    CMAKE_C_FLAGS_OPENMP_DEBUG
##    CMAKE_EXE_LINKER_FLAGS_OPENMP_DEBUG
##    CMAKE_SHARED_LINKER_FLAGS_OPENMP_DEBUG)
##if (PHREEQCRM_BUILD_FORTRAN)
##  MARK_AS_ADVANCED(CMAKE_Fortran_FLAGS_OPENMP_DEBUG)
##endif()


### Copy Release to mpi
##SET(CMAKE_CXX_FLAGS_MPI
##    ${CMAKE_CXX_FLAGS_RELEASE} CACHE STRING
##    "Flags used by the C++ compiler during mpi builds."
##    FORCE)
##SET(CMAKE_C_FLAGS_MPI
##    ${CMAKE_C_FLAGS_RELEASE} CACHE STRING
##    "Flags used by the C compiler during mpi builds."
##    FORCE)
##if (PHREEQCRM_BUILD_FORTRAN)
##  SET(CMAKE_Fortran_FLAGS_MPI
##      ${CMAKE_Fortran_FLAGS_RELEASE} CACHE STRING
##      "Flags used by the Fortran compiler during mpi builds."
##      FORCE)
##endif ()
##SET(CMAKE_EXE_LINKER_FLAGS_MPI
##    ${CMAKE_EXE_LINKER_FLAGS_RELEASE} CACHE STRING
##    "Flags used for linking binaries during mpi builds."
##    FORCE)
##SET(CMAKE_SHARED_LINKER_FLAGS_MPI
##    ${CMAKE_SHARED_LINKER_FLAGS_RELEASE} CACHE STRING
##    "Flags used by the shared libraries linker during mpi builds."
##    FORCE)
##MARK_AS_ADVANCED(
##    CMAKE_CXX_FLAGS_MPI
##    CMAKE_C_FLAGS_MPI
##    CMAKE_EXE_LINKER_FLAGS_MPI
##    CMAKE_SHARED_LINKER_FLAGS_MPI)
##if (PHREEQCRM_BUILD_FORTRAN)
##  MARK_AS_ADVANCED(CMAKE_Fortran_FLAGS_MPI)
##endif()


### Copy Debug to mpi_debug
##SET(CMAKE_CXX_FLAGS_MPI_DEBUG
##    ${CMAKE_CXX_FLAGS_DEBUG} CACHE STRING
##    "Flags used by the C++ compiler during debug mpi builds."
##    FORCE)
##SET(CMAKE_C_FLAGS_MPI_DEBUG
##    ${CMAKE_C_FLAGS_DEBUG} CACHE STRING
##    "Flags used by the C compiler during debug mpi builds."
##    FORCE)
##if (PHREEQCRM_BUILD_FORTRAN)
##  SET(CMAKE_Fortran_FLAGS_MPI_DEBUG
##      ${CMAKE_Fortran_FLAGS_DEBUG} CACHE STRING
##      "Flags used by the Fortran compiler during debug mpi builds."
##      FORCE)
##endif ()
##SET(CMAKE_EXE_LINKER_FLAGS_MPI_DEBUG
##    ${CMAKE_EXE_LINKER_FLAGS_DEBUG} CACHE STRING
##    "Flags used for linking binaries during debug mpi builds."
##    FORCE)
##SET(CMAKE_SHARED_LINKER_FLAGS_MPI_DEBUG
##    ${CMAKE_SHARED_LINKER_FLAGS_DEBUG} CACHE STRING
##    "Flags used by the shared libraries linker during debug mpi builds."
##    FORCE)
##MARK_AS_ADVANCED(
##    CMAKE_CXX_FLAGS_MPI_DEBUG
##    CMAKE_C_FLAGS_MPI_DEBUG
##    CMAKE_EXE_LINKER_FLAGS_MPI_DEBUG
##    CMAKE_SHARED_LINKER_FLAGS_MPI_DEBUG)
##if (PHREEQCRM_BUILD_FORTRAN)
##  MARK_AS_ADVANCED(CMAKE_Fortran_FLAGS_MPI_DEBUG)
##endif()


### check for openmp
##find_package(OpenMP)
##if (OPENMP_FOUND)
##  # openmp release
##  SET(CMAKE_C_FLAGS_OPENMP
##      "-DUSE_OPENMP ${CMAKE_C_FLAGS_OPENMP} ${OpenMP_C_FLAGS}" CACHE STRING
##      "Flags used by the compiler during openmp builds."
##      FORCE)
##  SET(CMAKE_CXX_FLAGS_OPENMP
##      "-DUSE_OPENMP ${CMAKE_CXX_FLAGS_OPENMP} ${OpenMP_CXX_FLAGS}" CACHE STRING
##      "Flags used by the compiler during openmp builds."
##      FORCE)
##  if (PHREEQCRM_BUILD_FORTRAN)
##    SET(CMAKE_Fortran_FLAGS_OPENMP
##        "-DUSE_OPENMP ${CMAKE_Fortran_FLAGS_OPENMP} ${OpenMP_Fortran_FLAGS}" CACHE STRING
##        "Flags used by the compiler during openmp builds."
##        FORCE)
##  endif ()
##
##  # openmp debug
##  SET(CMAKE_C_FLAGS_OPENMP_DEBUG
##      "-DUSE_OPENMP ${CMAKE_C_FLAGS_OPENMP_DEBUG} ${OpenMP_C_FLAGS}" CACHE STRING
##      "Flags used by the compiler during debug openmp builds."
##      FORCE)
##  SET(CMAKE_CXX_FLAGS_OPENMP_DEBUG
##      "-DUSE_OPENMP ${CMAKE_CXX_FLAGS_OPENMP_DEBUG} ${OpenMP_CXX_FLAGS}" CACHE STRING
##      "Flags used by the compiler during debug openmp builds."
##      FORCE)
##  if (PHREEQCRM_BUILD_FORTRAN)
##    SET(CMAKE_Fortran_FLAGS_OPENMP_DEBUG
##        "-DUSE_OPENMP ${CMAKE_Fortran_FLAGS_OPENMP_DEBUG} ${OpenMP_Fortran_FLAGS}" CACHE STRING
##        "Flags used by the compiler during debug openmp builds."
##        FORCE)
##  endif ()
##endif (OPENMP_FOUND)


### check for MPI
##option(PHREEQCRM_BUILD_MPI "Build MPI configuration" "OFF")
##if (PHREEQCRM_BUILD_MPI)
##  cmake_policy(SET CMP0054 OLD)
##  find_package(MPI)
##  cmake_policy(SET CMP0054 NEW)
##  
##  if (MPI_CXX_FOUND)
##    # mpi release
##    SET(CMAKE_C_FLAGS_MPI
##        "-DUSE_MPI ${CMAKE_C_FLAGS_MPI} ${MPI_C_COMPILE_FLAGS}" CACHE STRING
##        "Flags used by the compiler during mpi builds."
##        FORCE)
##    SET(CMAKE_CXX_FLAGS_MPI
##        "-DUSE_MPI ${CMAKE_CXX_FLAGS_MPI} ${MPI_CXX_COMPILE_FLAGS}" CACHE STRING
##        "Flags used by the compiler during mpi builds."
##        FORCE)
##    if (PHREEQCRM_BUILD_FORTRAN)
##      SET(CMAKE_Fortran_FLAGS_MPI
##          "-DUSE_MPI ${CMAKE_Fortran_FLAGS_MPI} ${MPI_Fortran_COMPILE_FLAGS}" CACHE STRING
##          "Flags used by the Fortran compiler during mpi builds."
##          FORCE)
##    endif ()
##    if (MPI_C_INCLUDE_PATH)
##      SET(CMAKE_C_FLAGS_MPI
##          "-I\"${MPI_C_INCLUDE_PATH}\" ${CMAKE_C_FLAGS_MPI}" CACHE STRING
##          "Flags used by the compiler during mpi builds."
##          FORCE)
##    endif (MPI_C_INCLUDE_PATH)
##    if (MPI_CXX_INCLUDE_PATH)
##      SET(CMAKE_CXX_FLAGS_MPI
##          "-I\"${MPI_CXX_INCLUDE_PATH}\" ${CMAKE_CXX_FLAGS_MPI}" CACHE STRING
##          "Flags used by the compiler during mpi builds."
##          FORCE)
##    endif (MPI_CXX_INCLUDE_PATH)
##    if (PHREEQCRM_BUILD_FORTRAN AND MPI_Fortran_INCLUDE_PATH)
##      if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
##        set(MS_MPI_ARCH_DIR amd64)
##      else()
##        set(MS_MPI_ARCH_DIR i386)
##      endif()
##      SET(CMAKE_Fortran_FLAGS_MPI
##          "-I\"${MPI_Fortran_INCLUDE_PATH}\" -I\"${MPI_Fortran_INCLUDE_PATH}/${MS_MPI_ARCH_DIR}\" ${CMAKE_Fortran_FLAGS_MPI}" CACHE STRING
##          "Flags used by the Fortran compiler during mpi builds."
##          FORCE)
##    endif ()    
##    
##    # mpi debug
##    SET(CMAKE_C_FLAGS_MPI_DEBUG
##        "-DUSE_MPI ${CMAKE_C_FLAGS_MPI_DEBUG} ${MPI_C_COMPILE_FLAGS}" CACHE STRING
##        "Flags used by the compiler during debug mpi builds."
##        FORCE)
##    SET(CMAKE_CXX_FLAGS_MPI_DEBUG
##        "-DUSE_MPI ${CMAKE_CXX_FLAGS_MPI_DEBUG} ${MPI_CXX_COMPILE_FLAGS}" CACHE STRING
##        "Flags used by the compiler during debug mpi builds."
##        FORCE)
##    if (PHREEQCRM_BUILD_FORTRAN)
##      SET(CMAKE_Fortran_FLAGS_MPI_DEBUG
##          "-DUSE_MPI ${CMAKE_Fortran_FLAGS_MPI_DEBUG} ${MPI_Fortran_COMPILE_FLAGS}" CACHE STRING
##          "Flags used by the Fortran compiler during debug mpi builds."
##          FORCE)
##    endif ()
##    if (MPI_C_INCLUDE_PATH)
##      SET(CMAKE_C_FLAGS_MPI_DEBUG
##          "-I\"${MPI_C_INCLUDE_PATH}\" ${CMAKE_C_FLAGS_MPI_DEBUG}" CACHE STRING
##          "Flags used by the compiler during debug mpi builds."
##          FORCE)
##    endif (MPI_C_INCLUDE_PATH)
##    if (MPI_CXX_INCLUDE_PATH)
##      SET(CMAKE_CXX_FLAGS_MPI_DEBUG
##          "-I\"${MPI_CXX_INCLUDE_PATH}\" ${CMAKE_CXX_FLAGS_MPI_DEBUG}" CACHE STRING
##          "Flags used by the compiler during debug mpi builds."
##          FORCE)
##    endif (MPI_CXX_INCLUDE_PATH)    
##    if (PHREEQCRM_BUILD_FORTRAN AND MPI_Fortran_INCLUDE_PATH)
##      if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
##        set(MS_MPI_ARCH_DIR amd64)
##      else()
##        set(MS_MPI_ARCH_DIR i386)
##      endif()
##      SET(CMAKE_Fortran_FLAGS_MPI_DEBUG
##          "-I\"${MPI_Fortran_INCLUDE_PATH}\" -I\"${MPI_Fortran_INCLUDE_PATH}/${MS_MPI_ARCH_DIR}\" ${CMAKE_Fortran_FLAGS_MPI_DEBUG}" CACHE STRING
##          "Flags used by the Fortran compiler during debug mpi builds."
##          FORCE)
##    endif ()    
##  endif (MPI_CXX_FOUND)
##endif (PHREEQCRM_BUILD_MPI)


##if (PHREEQCRM_BUILD_MPI)
##  if (CMAKE_CONFIGURATION_TYPES)
##    SET(CMAKE_CONFIGURATION_TYPES "openmp_debug;openmp;mpi;mpi_debug" CACHE STRING 
##    "Semicolon separated list of supported configuration types."
##    FORCE)
##  endif (CMAKE_CONFIGURATION_TYPES)
##else (PHREEQCRM_BUILD_MPI)
##  if (CMAKE_CONFIGURATION_TYPES)
##    SET(CMAKE_CONFIGURATION_TYPES "openmp_debug;openmp" CACHE STRING 
##    "Semicolon separated list of supported configuration types."
##    FORCE)
##  endif (CMAKE_CONFIGURATION_TYPES)
##endif (PHREEQCRM_BUILD_MPI)


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


OPTION (BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
SET(LIB_TYPE STATIC)
if (BUILD_SHARED_LIBS)
  SET(LIB_TYPE SHARED)
  OPTION (BUILD_CLR_LIBS "Build CLR Libraries" OFF)
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
  STRING(REPLACE " /EHsc" " /EHa /clr /wd4793 /wd4945" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
  # Remove /RTC1 from CMAKE_CXX_FLAGS_DEBUG
  STRING(REPLACE " /RTC1" "" CMAKE_CXX_FLAGS_DEBUG ${CMAKE_CXX_FLAGS_DEBUG})
  ADD_DEFINITIONS(-DMULTICHART)  
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

install(TARGETS IPhreeqc DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES ${IPhreeqc_Headers} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

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
