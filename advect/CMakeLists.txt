SET(FORTRAN_Advect_Files
advect.f90
ic
phreeqc.dat
)

SET(FORTRAN_Advect_Dir ${EXAMPLES_DIR}/fortran/advect)

install(FILES ${FORTRAN_Advect_Files} DESTINATION ${FORTRAN_Advect_Dir})
