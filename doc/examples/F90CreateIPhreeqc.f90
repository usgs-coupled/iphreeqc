PROGRAM example
  INCLUDE "IPhreeqc.f90.inc"
  INTEGER(KIND=4) :: id
  
  id = CreateIPhreeqc()
  IF (id.LT.0) THEN
     STOP
  ENDIF
  
  IF (LoadDatabase(id, "phreeqc.dat").NE.0) THEN
     CALL OutputError(id)
     STOP
  ENDIF
  
  IF (RunFile(id, "ex2").NE.0) THEN
     CALL OutputError(id)
     STOP
  ENDIF

  IF (DestroyIPhreeqc(id).NE.IPQ_OK) THEN
     CALL OutputError(id)
     STOP
  ENDIF
  
END PROGRAM example
