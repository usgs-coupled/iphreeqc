      FUNCTION F_MAIN()

      IMPLICIT NONE
      INCLUDE 'IPhreeqc.f.inc'
      INTEGER(KIND=4) id

      INTEGER(KIND=4) EXIT_SUCCESS
      PARAMETER (EXIT_SUCCESS=0)

      INTEGER(KIND=4) EXIT_FAILURE
      PARAMETER (EXIT_FAILURE=1)

      INTEGER(KIND=4) F_MAIN
      
      id = CreateIPhreeqc()
      IF (id.LT.0) THEN
         F_MAIN = EXIT_FAILURE
         RETURN
      ENDIF
      
      IF (GetOutputOn(id)) THEN
         F_MAIN = EXIT_FAILURE
         RETURN
      ENDIF
      
      IF (SetOutputOn(id,.TRUE.).NE.IPQ_OK) THEN
         F_MAIN = EXIT_FAILURE
         RETURN
      ENDIF
      
      IF (.NOT.GetOutputOn(id)) THEN
         F_MAIN = EXIT_FAILURE
         RETURN
      ENDIF
      
      IF (LoadDatabase(id, "phreeqc.dat").NE.0) THEN
         CALL OutputError(id)
         F_MAIN = EXIT_FAILURE
         RETURN
      ENDIF
      
      IF (RunFile(id, "ex1").NE.0) THEN
         CALL OutputError(id)
         F_MAIN = EXIT_FAILURE
         RETURN
      ENDIF
      
      IF (DestroyIPhreeqc(id).NE.0) THEN
         CALL OutputError(id)
         F_MAIN = EXIT_FAILURE
         RETURN
      ENDIF
      
      F_MAIN = EXIT_SUCCESS
      RETURN
      
      END FUNCTION F_MAIN
      
      
      FUNCTION TestGetSet(id,getFunc,setFunc)
      
      IMPLICIT NONE
      INCLUDE 'IPhreeqc.f90.inc'
      INTEGER(KIND=4) id
      INTEGER(KIND=4) TESTGETSET
      
      LOGICAL(KIND=4) getFunc
      INTEGER(KIND=4) setFunc
      
      INTEGER(KIND=4) EXIT_SUCCESS
      PARAMETER (EXIT_SUCCESS=0)
      
      INTEGER(KIND=4) EXIT_FAILURE
      PARAMETER (EXIT_FAILURE=1)
      
      
      IF (getFunc(id)) THEN
         TestGetSet = EXIT_FAILURE
         RETURN
      ENDIF
      
      IF (setFunc(id,.TRUE.).NE.IPQ_OK) THEN
         TestGetSet = EXIT_FAILURE
         RETURN
      ENDIF
      
      IF (.NOT.getFunc(id)) THEN
         TestGetSet = EXIT_FAILURE
         RETURN
      ENDIF
      
      IF (setFunc(id,.FALSE.).NE.IPQ_OK) THEN
         TestGetSet = EXIT_FAILURE
         RETURN
      ENDIF
      
      TestGetSet = EXIT_SUCCESS
      RETURN
      
      END FUNCTION TestGetSet
