PROGRAM example
  INCLUDE "IPhreeqc.f90.inc"
  INTEGER(KIND=4) :: id
  INTEGER(KIND=4) :: i
  CHARACTER(LEN=40) :: comp
  
  id = CreateIPhreeqc()
  IF (id.LT.0) THEN
     STOP
  ENDIF
  
  IF (LoadDatabase(id, "phreeqc.dat").NE.0) THEN
     CALL OutputErrorString(id)
     STOP
  ENDIF
  
  IF (RunFile(id, "ex2").NE.0) THEN
     CALL OutputErrorString(id)
     STOP
  ENDIF
  
  DO i=1,GetComponentCount(id)
     CALL GetComponent(id, i, comp)
     WRITE(*,*) "comp #", i, "= ", comp
  ENDDO
  
  IF (DestroyIPhreeqc(id).NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  ENDIF
END PROGRAM example
