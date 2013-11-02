PROGRAM example
  INCLUDE "IPhreeqc.f90.inc"
  CHARACTER(LEN=80) :: version

  WRITE(*,*) "Version:"
  CALL GetVersionString(version)
  WRITE(*,*) TRIM(version)
END PROGRAM example
