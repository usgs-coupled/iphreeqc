      PROGRAM DRIVER

      IMPLICIT NONE
      INCLUDE '..\include\IPhreeqc.f90.inc'
      INTEGER iresult
      INTEGER rows, cols
      CHARACTER(30) comp
      INTEGER n
      INTEGER i
      INTEGER id
      
      id = CreateIPhreeqc()
      IF (id.LT.0) THEN
        CALL OutputError(id)
        STOP
      ENDIF

      iresult = LoadDatabase
     &(id, 'wateq4f.dat')
      IF (iresult.NE.0) THEN
        CALL OutputError(id)
        STOP 'Failed to load database'
      ENDIF

!!      iresult= Runfile('CO2test',
!!     & .true.,.true.,.true.,.true.)
      iresult = SetOutputOn(id, .TRUE.)
      iresult = SetErrorOn(id, .TRUE.)
      iresult = SetLogOn(id, .TRUE.)
      iresult = SetSelectedOutputOn(id, .TRUE.)
      iresult= Runfile(id, 'tester')

      IF (iresult.NE.0) THEN
        CALL OutputError(id)
        STOP 'Failed to run'
      ENDIF

      rows = GetSelectedOutputRowCount(id)
      cols = GetSelectedOutputColumnCount(id)

      PRINT*,'Rows= ',rows
      PRINT*,'Cols= ',cols

      STOP 'Ok'
      
	END