      SUBROUTINE F_MAIN

      IMPLICIT NONE
      INCLUDE 'IPhreeqc.f.inc'
      INTEGER id
      
      id = CreateIPhreeqc()
      IF (id.LT.0) THEN
        STOP
      ENDIF
      
      IF (LoadDatabase(id, "phreeqc.dat").NE.0) THEN
        CALL OutputError(id)
        STOP
      ENDIF
      
      IF (RunFile(id, "ex1").NE.0) THEN
        CALL OutputError(id)
        STOP
      ENDIF
      
      IF (DestroyIPhreeqc(id).NE.0) THEN
        CALL OutputError(id)
        STOP
      ENDIF
      
      END SUBROUTINE F_MAIN
