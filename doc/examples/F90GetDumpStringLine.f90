PROGRAM example
  USE IPhreeqc
  INTEGER(KIND=4) :: id
  INTEGER(KIND=4) :: i
  CHARACTER(LEN=80) :: line

  id = CreateIPhreeqc()
  IF (id.LT.0) THEN
     STOP
  END IF

  IF (SetDumpStringOn(id, .TRUE.).NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF

  IF (LoadDatabase(id, "phreeqc.dat").NE.0) THEN
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

  IF (AccumulateLine(id, "DUMP").NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF

  IF (AccumulateLine(id, "    -solution 1").NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF

  IF (AccumulateLine(id, "    -equilibrium_phases 1").NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF

  WRITE(*,*) "Input:"
  CALL OutputAccumulatedLines(id)

  IF (RunAccumulated(id).NE.0) THEN
     CALL OutputErrorString(id)
     STOP
  END IF

  WRITE(*,*) "Dump:"
  DO i=1,GetDumpStringLineCount(id)
     CALL GetDumpStringLine(id, i, line)
     WRITE(*,*) TRIM(line)
  END DO

  IF (DestroyIPhreeqc(id).NE.IPQ_OK) THEN
     CALL OutputErrorString(id)
     STOP
  END IF
END PROGRAM example
