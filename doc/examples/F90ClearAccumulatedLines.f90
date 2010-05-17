PROGRAM example
  INCLUDE "IPhreeqc.f90.inc"
  INTEGER(KIND=4) :: id
  INTEGER(KIND=4) :: i
  
  id = CreateIPhreeqc()
  IF (id.LT.0) THEN
     STOP
  ENDIF
  
  IF (LoadDatabase(id, "../../database/wateq4f.dat").NE.0) THEN
     CALL OutputError(id)
     STOP
  ENDIF
  
  IF (AccumulateLine(id, "SOLUTION 1").NE.IPQ_OK) THEN
     CALL OutputError(id)
     STOP
  ENDIF
  
  IF (AccumulateLine(id, "pH -2").NE.IPQ_OK) THEN
     CALL OutputError(id)
     STOP
  ENDIF
  
  IF (AccumulateLine(id, "END").NE.IPQ_OK) THEN
     CALL OutputError(id)
     STOP
  ENDIF
  
  IF (RunAccumulated(id).NE.0) THEN
     CALL OutputLines(id)
     CALL OutputError(id)
     IF (AccumulateLine(id, "SOLUTION 1").NE.IPQ_OK) THEN
        CALL OutputError(id)
        STOP
     ENDIF
     
     IF (AccumulateLine(id, "pH 2").NE.IPQ_OK) THEN
        CALL OutputError(id)
        STOP
     ENDIF
     
     IF (AccumulateLine(id, "END").NE.IPQ_OK) THEN
        CALL OutputError(id)
        STOP
     ENDIF
     
     IF (RunAccumulated(id).NE.0) THEN
        STOP
     ENDIF
  ENDIF
  
  IF (DestroyIPhreeqc(id).NE.IPQ_OK) THEN
     CALL OutputError(id)
     STOP
  ENDIF
  WRITE(*,*) "Ok"
END PROGRAM example
