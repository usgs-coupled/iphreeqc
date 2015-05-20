SET(C_Advect_Files
advect.c
ic
phreeqc.dat
)

SET(C_Advect_Dir ${EXAMPLES_DIR}/cpp/advect)

install(FILES ${C_Advect_Files} DESTINATION ${C_Advect_Dir})



