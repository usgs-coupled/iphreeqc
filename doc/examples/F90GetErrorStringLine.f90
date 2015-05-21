PROGRAM example
  USE IPhreeqc
  INTEGER(KIND=4) :: id
  INTEGER(KIND=4) :: i
  CHARACTER(LEN=80) :: line

  id = CreateIPhreeqc()
  IF (id.LT.0) THEN
     STOP
  END IF

  IF (SetErrorStringOn(id, .TRUE.).NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF

  IF (AccumulateLine(id, "SOLUTION 1 Pure water").NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF

  IF (AccumulateLine(id, "EQUILIBRIUM_PHASES 1").NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF

  IF (AccumulateLine(id, "    Calcite 0 10").NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF

  IF (AccumulateLine(id, "SAVE solution 1").NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF

  IF (AccumulateLine(id, "SAVE equilibrium_phases 1").NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF

  WRITE(*,*) "Input:"
  CALL OutputAccumulatedLines(id)

  IF (RunAccumulated(id).NE.0) THEN
     WRITE(*,*) "Error:"
     DO i=1,GetErrorStringLineCount(id)
        CALL GetErrorStringLine(id, i, line)
        WRITE(*,*) TRIM(line)
     END DO
     STOP
  END IF

  IF (DestroyIPhreeqc(id).NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF
END PROGRAM example
