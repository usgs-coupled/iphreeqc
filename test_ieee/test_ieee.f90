!  test_ieee.f90 
!
!  FUNCTIONS:
!  test_ieee - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: test_ieee
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program test_ieee

    implicit none
    INCLUDE "IPhreeqc.f90.inc"

    ! Variables
    INTEGER(KIND=4)   id
    INTEGER(KIND=4)   nerr
    INTEGER(KIND=4)   r
    INTEGER(KIND=4)   c
    INTEGER(KIND=4)   t
    REAL(KIND=8)      d
    CHARACTER(LEN=80) s

    ! Body of test_ieee
    id = CreateIPhreeqc()

    nerr = SetSelectedOutputFileOn(id,.TRUE.)
    nerr = SetOutputFileOn(id,.TRUE.)
    nerr = SetDumpFileOn(id,.TRUE.)
    nerr = SetErrorFileOn(id,.TRUE.)
    nerr = SetLogFileOn(id,.TRUE.)

    nerr = LoadDatabase(id,'wateq4f.dat')
    IF (nerr.NE.0) THEN
       CALL OutputErrorString(id)
       STOP
    ENDIF

    nerr = RunFile(id,'IEEE')
    IF (nerr.NE.0) THEN
       CALL OutputErrorString(id)
       STOP
    ENDIF

    DO r=0,GetSelectedOutputRowCount(id)
       DO c=1,GetSelectedOutputColumnCount(id)
          IF (GetSelectedOutputValue(id,r,c,t,d,s).NE.IPQ_OK) THEN
             CALL OutputErrorString(id)
             STOP
          END IF
       END DO
    END DO

    nerr = DestroyIPhreeqc(id)
    IF (nerr.NE.0) THEN
       CALL OutputErrorString(id)
       STOP
    ENDIF

    PRINT *, 'ok'

    end program test_ieee

