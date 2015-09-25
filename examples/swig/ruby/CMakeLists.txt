# files
SET(Ruby_Files
example.rb
advect.rb
${CMAKE_SOURCE_DIR}/database/phreeqc.dat
${CMAKE_SOURCE_DIR}/examples/cpp/advect/ic
)

# install directory
SET(Ruby_Dir ${EXAMPLES_DIR}/swig/ruby)

# install
install(FILES ${Ruby_Files} DESTINATION ${Ruby_Dir})

