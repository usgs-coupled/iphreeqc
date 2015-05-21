# project
project(example_advect_f90 Fortran)

# files
SET(FORTRAN_Advect_Files
advect.F90
ic
phreeqc.dat
)

# src
SET(FORTRAN_Advect_SRC
advect.f90
../../../src/IPhreeqc_interface.F90
)

# executable
add_executable(example_advect_f90 ${FORTRAN_Advect_SRC})

# library dependencies
SET(EXTRA_LIBS ${EXTRA_LIBS} IPhreeqc)

# link 
target_link_libraries(example_advect_f90 ${EXTRA_LIBS})

# install directory
SET(FORTRAN_Advect_Dir ${EXAMPLES_DIR}/fortran/advect)

# install
install(FILES ${FORTRAN_Advect_Files} DESTINATION ${FORTRAN_Advect_Dir})
