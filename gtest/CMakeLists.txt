project(unit)

include(GoogleTest)

add_executable(TestCVar TestCVar.cpp)
target_link_libraries(TestCVar IPhreeqc gtest gtest_main)
gtest_discover_tests(TestCVar)

add_executable(TestVar TestVar.cpp)
target_link_libraries(TestVar IPhreeqc gtest gtest_main)
gtest_discover_tests(TestVar)

add_executable(TestIPhreeqcLib TestIPhreeqcLib.cpp FileTest.cpp)
target_link_libraries(TestIPhreeqcLib IPhreeqc gtest gtest_main)
gtest_discover_tests(TestIPhreeqcLib)

add_executable(TestIPhreeqc TestIPhreeqc.cpp FileTest.cpp)
target_link_libraries(TestIPhreeqc IPhreeqc gtest gtest_main)
gtest_discover_tests(TestIPhreeqc)

add_executable(TestSelectedOutput TestSelectedOutput.cpp)
target_link_libraries(TestSelectedOutput IPhreeqc gtest gtest_main)
gtest_discover_tests(TestSelectedOutput)

if (MSVC AND BUILD_SHARED_LIBS)
  # copy dlls
  add_custom_command(TARGET TestCVar POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:IPhreeqc> $<TARGET_FILE_DIR:TestCVar>
    )
  add_custom_command(TARGET TestCVar POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:gtest> $<TARGET_FILE_DIR:TestCVar>
    )
  add_custom_command(TARGET TestCVar POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:gtest_main> $<TARGET_FILE_DIR:TestCVar>
    )
endif()


# databases
configure_file(
  ../unit/phreeqc.dat.old
  phreeqc.dat.old
  COPYONLY
  )

configure_file(
  ../database/phreeqc.dat
  phreeqc.dat
  COPYONLY
  )

configure_file(
  ../database/pitzer.dat
  pitzer.dat
  COPYONLY
  )

configure_file(
  ../database/wateq4f.dat
  wateq4f.dat
  COPYONLY
  )

configure_file(
  ../database/Amm.dat
  Amm.dat
  COPYONLY
  )

configure_file(
  ../database/frezchem.dat
  frezchem.dat
  COPYONLY
  )

configure_file(
  ../database/iso.dat
  iso.dat
  COPYONLY
  )

configure_file(
  ../unit/llnl.dat.old
  llnl.dat.old
  COPYONLY
  )

configure_file(
  ../database/llnl.dat
  llnl.dat
  COPYONLY
  )

configure_file(
  ../database/minteq.dat
  minteq.dat
  COPYONLY
  )

configure_file(
  ../database/minteq.v4.dat
  minteq.v4.dat
  COPYONLY
  )

configure_file(
  ../database/sit.dat
  sit.dat
  COPYONLY
  )

configure_file(
  ../database/ColdChem.dat
  ColdChem.dat
  COPYONLY
  )

configure_file(
  ../database/core10.dat
  core10.dat
  COPYONLY
  )

configure_file(
  ../database/Tipping_Hurley.dat
  Tipping_Hurley.dat
  COPYONLY
  )

configure_file(
  missing_e.dat
  missing_e.dat
  COPYONLY
  )

# misc files 

configure_file(
  conv_fail.in
  conv_fail.in
  COPYONLY
  )

configure_file(
  dump
  dump
  COPYONLY
  )

configure_file(
  multi_punch
  multi_punch
  COPYONLY
  )

configure_file(
  multi_punch_no_set
  multi_punch_no_set
  COPYONLY
  )

configure_file(
  kinn20140218
  kinn20140218
  COPYONLY
  )
