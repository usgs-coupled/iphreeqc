FUNCTION F_MAIN()
  
  IMPLICIT NONE
  INCLUDE 'IPhreeqc.f90.inc'
  INTEGER(KIND=4) id
  INTEGER(KIND=4) F_MAIN
  INTEGER(KIND=4) TestGetSet
  
  id = CreateIPhreeqc()
  IF (id.LT.0) THEN
     F_MAIN = 1
     RETURN
  ENDIF
  
  ! Dump
  IF (TestGetSet(id,GetDumpOn,SetDumpOn).NE.0) THEN
     F_MAIN = 2
     RETURN
  ENDIF
  
  ! Dump string
  IF (TestGetSet(id,GetDumpStringOn,SetDumpStringOn).NE.0) THEN
     F_MAIN = 2
     RETURN
  ENDIF
  
  ! Error
  IF (TestGetSet(id,GetErrorOn,SetErrorOn).NE.0) THEN
     F_MAIN = 2
     RETURN
  ENDIF
  
  ! Log
  IF (TestGetSet(id,GetLogOn,SetLogOn).NE.0) THEN
     F_MAIN = 2
     RETURN
  ENDIF
  
  ! Output
  IF (TestGetSet(id,GetOutputOn,SetOutputOn).NE.0) THEN
     F_MAIN = 2
     RETURN
  ENDIF
  
  ! Selected output
  IF (TestGetSet(id,GetSelectedOutputOn,SetSelectedOutputOn).NE.0) THEN
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
     F_MAIN = 4
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
  INTERFACE
     FUNCTION getFunc(id)
       INTEGER(KIND=4) id
       LOGICAL(KIND=4) getFunc
     END FUNCTION getFunc
  END INTERFACE
  INTERFACE
     FUNCTION setFunc(id,flag)
       INTEGER(KIND=4) id
       LOGICAL(KIND=4) flag
       INTEGER(KIND=4) setFunc
     END FUNCTION setFunc
  END INTERFACE
  
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
