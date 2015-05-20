SET(CPP_Advect_Files
advect.cpp
ic
phreeqc.dat
)

SET(CPP_Advect_Dir ${EXAMPLES_DIR}/cpp/advect)

install(FILES ${CPP_Advect_Files} DESTINATION ${CPP_Advect_Dir})
