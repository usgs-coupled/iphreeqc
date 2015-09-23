# files
SET(Java_Files
callback.java
example.java
${CMAKE_SOURCE_DIR}/database/phreeqc.dat
${CMAKE_SOURCE_DIR}/examples/cpp/advect/ic
)

# install directory
SET(Java_Dir ${EXAMPLES_DIR}/swig/java)

# install
install(FILES ${Java_Files} DESTINATION ${Java_Dir})
install(DIRECTORY "${CMAKE_BINARY_DIR}/swig/" DESTINATION ${Java_Dir} FILES_MATCHING PATTERN "*.java")

