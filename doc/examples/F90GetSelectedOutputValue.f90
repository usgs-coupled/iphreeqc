PROGRAM example
  USE IPhreeqc
  INTEGER(KIND=4)   :: id
  INTEGER(KIND=4)   :: i
  INTEGER(KIND=4)   :: j
  INTEGER(KIND=4)   :: vt
  REAL(KIND=8)      :: dv
  CHARACTER(LEN=40) :: sv
  INTEGER(KIND=4)   :: sl
  
  id = CreateIPhreeqc()
  IF (id.LT.0) THEN
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
  DO i=0,GetSelectedOutputRowCount(id)
     DO j=1,GetSelectedOutputColumnCount(id)
        IF (GetSelectedOutputValue(id, i, j, vt, dv, sv, sl).EQ.IPQ_OK) THEN          
           IF (vt.EQ.TT_DOUBLE) THEN
              WRITE(*,"(g12.6,A1)",ADVANCE="NO") dv, " "
           ELSE IF (vt.EQ.TT_STRING) THEN
              IF (sl.EQ.0) THEN
                 WRITE(*,"(A12,A1)",ADVANCE="NO") sv, " "
              ELSE
                 WRITE(*,"(A12,A1)",ADVANCE="NO") "#ERR", " "
              END IF
           END IF
        END IF
     END DO
     WRITE(*,*)
  END DO

  IF (DestroyIPhreeqc(id).NE.IPQ_OK) THEN
     STOP
  END IF
END PROGRAM example
