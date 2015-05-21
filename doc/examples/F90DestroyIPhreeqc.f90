PROGRAM example
  USE IPhreeqc
  INTEGER(KIND=4) :: id
  
  id = CreateIPhreeqc()
  IF (id.LT.0) THEN
     STOP
  END IF
  
  IF (DestroyIPhreeqc(id).NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF
END PROGRAM example
