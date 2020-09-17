# configure_file(ex2 ex2 COPYONLY)
# configure_file(main.cpp main.cpp COPYONLY)
# configure_file(phreeqc.dat phreeqc.dat COPYONLY)
# configure_file(README.txt README.txt COPYONLY)

configure_file(CMakeLists.txt.in CMakeLists.txt)

# 
SET(example_FILES
  ${IPhreeqc_BINARY_DIR}/examples/using-cmake/CMakeLists.txt
  ex2
  main.cpp
  phreeqc.dat
  README.txt
  )

# install directory
SET(example_DIR ${EXAMPLES_DIR}/using-cmake)

# install
install(FILES ${example_FILES} DESTINATION ${example_DIR})


