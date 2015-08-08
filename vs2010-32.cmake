set(CTEST_BUILD_NAME "VS2010-x64-fortran")
set(CTEST_SITE "IGSKAHHWWSCHARL")

set(CTEST_SOURCE_DIRECTORY "${CTEST_SCRIPT_DIRECTORY}")
set(CTEST_BINARY_DIRECTORY "${CTEST_SCRIPT_DIRECTORY}/_vs2010-32")
set(CTEST_CMAKE_GENERATOR "Visual Studio 10 2010")

# Build Fortran test (First build option can't have any beginning whitespace)
set(ADD_BUILD_OPTIONS "-DIPHREEQC_FORTRAN_TESTING:BOOL=ON")

# Build Shared Libraries
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_SHARED_LIBS:BOOL=OFF")

# Build the testing tree
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_TESTING:BOOL=ON")

# Enable Fortran module
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DIPHREEQC_ENABLE_MODULE:BOOL=ON")

# CMAKE_INSTALL_PREFIX
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCMAKE_INSTALL_PREFIX:PATH=${CTEST_SCRIPT_DIRECTORY}/_vs2010-64/INSTALL")

set(BUILD_OPTIONS -DCMAKE_INSTALL_PREFIX:PATH=${CTEST_SCRIPT_DIRECTORY}/vs2010
                  -DIPHREEQC_FORTRAN_TESTING:BOOL=ON
                  -DIPHREEQC_STATIC_RUNTIME:BOOL=ON)

CTEST_START("Experimental")
CTEST_CONFIGURE(BUILD "${CTEST_BINARY_DIRECTORY}"
                OPTIONS "${BUILD_OPTIONS}")
CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}")
CTEST_TEST(BUILD "${CTEST_BINARY_DIRECTORY}")
CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}" TARGET INSTALL)
###CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}" TARGET PACKAGE)

##set (EXECUTE_COMMAND C:\\Program Files\\7-Zip\\7z a ..\\..\\${CPACK_PACKAGE_NAME}-$ENV{CPACK_PACKAGE_VERSION}-${CPACK_SYSTEM_NAME}.7z)
##set (EXECUTE_COMMAND C:\\Program Files\\7-Zip\\7z a ..\\..\\${CPACK_PACKAGE_NAME}-$ENV{CPACK_PACKAGE_VERSION}-${CPACK_SYSTEM_NAME}.7z)
##execute_process(COMMAND ${EXECUTE_COMMAND}
##                WORKING_DIRECTORY ${CTEST_SCRIPT_DIRECTORY}/_vs2010-64/INSTALL)                
###CTEST_SUBMIT()