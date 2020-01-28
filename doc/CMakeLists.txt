SET(IPhreeqc_Docs
IPhreeqc.pdf
NOTICE
Phreeqc_2_1999_manual.pdf
Phreeqc_3_2013_manual.pdf
README
RELEASE
)
install(FILES ${IPhreeqc_Docs} DESTINATION ${CMAKE_INSTALL_DOCDIR})
install(DIRECTORY html/ DESTINATION ${CMAKE_INSTALL_DOCDIR}/html)
