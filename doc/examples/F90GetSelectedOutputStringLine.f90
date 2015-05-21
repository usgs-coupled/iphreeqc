PROGRAM example
  USE IPhreeqc
  INTEGER(KIND=4)   :: id
  INTEGER(KIND=4)   :: i
  CHARACTER(LEN=400):: line
  
  id = CreateIPhreeqc()
  IF (id.LT.0) THEN
     STOP
  END IF
  
  IF (SetSelectedOutputStringOn(id, .TRUE.).NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF
  
  IF (LoadDatabase(id, "phreeqc.dat").NE.0) THEN
     CALL OutputErrorString(id)
     STOP
  END IF
  
  IF (RunFile(id, "ex2").NE.0) THEN
     CALL OutputErrorString(id)
     STOP
  END IF
  
  WRITE(*,*) "selected-output:"
  DO i=1,GetSelectedOutputStringLineCount(id)
     CALL GetSelectedOutputStringLine(id, i, line)
     WRITE(*,*) TRIM(line)
  END DO

  IF (DestroyIPhreeqc(id).NE.IPQ_OK) THEN
     STOP
  END IF
END PROGRAM example
