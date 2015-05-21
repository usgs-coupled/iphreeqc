PROGRAM example
  USE IPhreeqc
  INTEGER(KIND=4) :: id
  
  id = CreateIPhreeqc()
  IF (id.LT.0) THEN
     STOP
  END IF
  
  IF (LoadDatabase(id, "../../database/wateq4f.dat").NE.0) THEN
     CALL OutputErrorString(id)
     STOP
  END IF
  
  IF (AccumulateLine(id, "SOLUTION 1").NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF
  
  IF (AccumulateLine(id, "pH -2").NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF
  
  IF (AccumulateLine(id, "END").NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF
  
  IF (RunAccumulated(id).NE.0) THEN
     CALL OutputAccumulatedLines(id)
     CALL OutputErrorString(id)
     IF (AccumulateLine(id, "SOLUTION 1").NE.IPQ_OK) THEN
        CALL OutputErrorString(id)
        STOP
     END IF
     
     IF (AccumulateLine(id, "pH 2").NE.IPQ_OK) THEN
        CALL OutputErrorString(id)
        STOP
     END IF
     
     IF (AccumulateLine(id, "END").NE.IPQ_OK) THEN
        CALL OutputErrorString(id)
        STOP
     END IF
     
     IF (RunAccumulated(id).NE.0) THEN
        STOP
     END IF
  END IF
  
  IF (DestroyIPhreeqc(id).NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF
  WRITE(*,*) "Ok"
END PROGRAM example
