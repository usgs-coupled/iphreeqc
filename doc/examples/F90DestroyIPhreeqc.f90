PROGRAM example
  INCLUDE "IPhreeqc.f90.inc"
  INTEGER(KIND=4) :: id
  
  id = CreateIPhreeqc()
  IF (id.LT.0) THEN
     STOP
  ENDIF
  
  IF (DestroyIPhreeqc(id).NE.IPQ_OK) THEN
     CALL OutputError(id)
     STOP
  ENDIF
  
END PROGRAM example
