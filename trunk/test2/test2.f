      PROGRAM DRIVER

      IMPLICIT NONE
      INCLUDE '../include/IPhreeqc.f.inc'
      INTEGER iresult
      INTEGER rows, cols

      iresult = LoadDatabase
     &('wateq4f.dat')
      IF (iresult.NE.VR_OK) THEN
        CALL OutputLastError
        STOP 'Failed to load database'
      ENDIF

!!      iresult= Runfile('CO2test',
!!     & .true.,.true.,.true.,.true.)
      iresult= Runfile('tester',
     & .true.,.true.,.true.,.true.)

      IF (iresult.NE.VR_OK) THEN
        CALL OutputLastError
        STOP 'Failed to run'
      ENDIF

      rows = GetSelectedOutputRowCount()
      cols = GetSelectedOutputColumnCount()

      PRINT*,'Rows= ',rows
      PRINT*,'Cols= ',cols

      STOP 'Ok'
      
	END