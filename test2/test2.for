      PROGRAM DRIVER

      IMPLICIT NONE
      INCLUDE '..\include\IPhreeqc.f90.inc'
      INTEGER iresult
      INTEGER rows, cols
      CHARACTER(30) comp
      INTEGER n
      INTEGER i

      iresult = LoadDatabase
     &('wateq4f.dat')
      IF (iresult.NE.VR_OK) THEN
        CALL OutputLastError
        STOP 'Failed to load database'
      ENDIF

!!      iresult= Runfile('CO2test',
!!     & .true.,.true.,.true.,.true.)
      CALL SetOutputOn(.TRUE.)
      CALL SetErrorOn(.TRUE.)
      CALL SetLogOn(.TRUE.)
      CALL SetSelectedOutputOn(.TRUE.)
      iresult= Runfile('tester')

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