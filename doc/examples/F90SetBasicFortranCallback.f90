MODULE MyData
  DOUBLE PRECISION year
END MODULE MyData

MODULE Callback
CONTAINS
  REAL(kind=C_DOUBLE) FUNCTION MyCallback(x1, x2, str, l) BIND(C, name='MyCallback')
    USE MyData, ONLY : year
    USE ISO_C_BINDING
    !       
    !   Use of a callback is optional.
    !   
    !   The callback provides a way to obtain data from a Basic program
    !   through the variables x1, x2, and str, and send data to a 
    !   Basic program through the return value of the callback.
    !   
    !   The callback function is called whenever CALLBACK(x1, x2, str$)  
    !   is used in a Basic program (usually USER_PUNCH). See file "ic".
    !   
    REAL(kind=C_DOUBLE),    INTENT(in)        :: x1, x2
    CHARACTER(kind=C_CHAR), INTENT(in)        :: str(*)
    INTEGER(kind=C_INT),    INTENT(in), value :: l
    character(len=l) fstr
    MyCallback = -1.0
    ! An example of a datum available in main program
    do i = 1, l
       fstr(i:i) = str(i)
    enddo
    IF (TRIM(fstr) .EQ. "Year") THEN
       WRITE (*,"(/a,i2,a,f8.2)") "Callback for cell ", INT(x1), ": pH ", x2
       MyCallback = year
    ENDIF
    RETURN
  END FUNCTION MyCallback
END MODULE Callback

PROGRAM Advect
  USE Callback
  USE MyData
  USE IPhreeqc
  INTEGER :: Id
  
  !Create module, load database, define initial conditions and selected output
  year = 2012.
  Id = CreateIPhreeqc()
  IF (LoadDatabase(Id, "phreeqc.dat") .NE. 0) THEN
     CALL OutputErrorString(Id)
     STOP
  ENDIF
  IF (SetSelectedOutputFileOn(id, .TRUE.) .NE. 0) THEN
     CALL OutputErrorString(Id)
     STOP
  ENDIF
  IF (SetBasicFortranCallback(id, MyCallback) .NE. 0) THEN
     CALL OutputErrorString(Id)
     STOP
  ENDIF
  IF (RunFile(Id, "ic") .NE. 0) THEN
     CALL OutputErrorString(Id)
     STOP
  ENDIF
  !Destroy module 
  IF (DestroyIPhreeqc(Id) .NE. 0) CALL OutputErrorString(Id)
END PROGRAM Advect
