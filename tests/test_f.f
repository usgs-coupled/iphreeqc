      FUNCTION F_MAIN()

      IMPLICIT NONE
      INCLUDE 'IPhreeqc.f.inc'
      INTEGER(KIND=4) id
      INTEGER(KIND=4) F_MAIN
      
      id = CreateIPhreeqc()
      IF (id.LT.0) THEN
        F_MAIN = 1
        RETURN
      ENDIF
      
      IF (GetOutputOn(id)) THEN
        F_MAIN = 2
        RETURN
      ENDIF

      IF (SetOutputOn(id,.TRUE.).NE.IPQ_OK) THEN
        F_MAIN = 2
        RETURN
      ENDIF

      IF (.NOT.GetOutputOn(id)) THEN
        F_MAIN = 2
        RETURN
      ENDIF

      IF (LoadDatabase(id, "phreeqc.dat").NE.0) THEN
        CALL OutputError(id)
        F_MAIN = 3
        RETURN
      ENDIF
      
      IF (RunFile(id, "ex1").NE.0) THEN
        CALL OutputError(id)
        F_MAIN = 3
        RETURN
      ENDIF
      
      IF (DestroyIPhreeqc(id).NE.0) THEN
        CALL OutputError(id)
        F_MAIN = 5
        RETURN
      ENDIF
      
      F_MAIN = 0
      RETURN
      
      END FUNCTION F_MAIN


      FUNCTION TestGetSet(id,getFunc,setFunc)
      
      IMPLICIT NONE
      INCLUDE 'IPhreeqc.f90.inc'
      INTEGER(KIND=4) id
      INTEGER(KIND=4) TESTGETSET

      LOGICAL(KIND=4) getFunc
      INTEGER(KIND=4) setFunc
      
      IF (getFunc(id)) THEN
         TestGetSet = 2
         RETURN
      ENDIF
      
      IF (setFunc(id,.TRUE.).NE.IPQ_OK) THEN
         TestGetSet = 2
         RETURN
      ENDIF
      
      IF (.NOT.getFunc(id)) THEN
         TestGetSet = 2
         RETURN
      ENDIF
      
      IF (setFunc(id,.FALSE.).NE.IPQ_OK) THEN
         TestGetSet = 2
         RETURN
      ENDIF
      
      TestGetSet = 0  
      RETURN
      
      END FUNCTION TestGetSet
