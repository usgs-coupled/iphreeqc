# files
SET(Ruby_Files
example.rb
${CMAKE_SOURCE_DIR}/database/phreeqc.dat
)

# install directory
SET(Ruby_Dir ${EXAMPLES_DIR}/swig/ruby)

# install
install(FILES ${Ruby_Files} DESTINATION ${Ruby_Dir})

