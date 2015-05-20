SET(IPhreeqc_Docs
IPhreeqc.pdf
)
install(FILES ${IPhreeqc_Docs} DESTINATION ${CMAKE_INSTALL_DOCDIR})
install(DIRECTORY html/ DESTINATION ${CMAKE_INSTALL_DOCDIR}/html)
