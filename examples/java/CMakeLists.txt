# files
SET(Java_Files
main.java
phreeqc.dat
)

# install directory
SET(Java_Dir ${EXAMPLES_DIR}/java)

# install
install(FILES ${Java_Files} DESTINATION ${Java_Dir})
install(DIRECTORY "${CMAKE_BINARY_DIR}/swig/" DESTINATION ${Java_Dir} FILES_MATCHING PATTERN "*.java")

