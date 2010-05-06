!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      PROGRAM DRIVER

      IMPLICIT NONE
      INCLUDE 'IPhreeqc.f.inc'
      INTEGER iresult
      INTEGER i, j, k, rows, cols
      INTEGER vtype
      CHARACTER(30) svalue
      CHARACTER(30) comp
      INTEGER n
      INTEGER len
      INTEGER id
      REAL*8 dvalue
      
      id = CreateIPhreeqc()
      IF (id.LT.0) THEN
        CALL OutputError(id)
        STOP
      ENDIF

      iresult = LoadDatabase(id, 'llnl.dat')

      IF (iresult.NE.0) THEN
        CALL OutputError(id)
        STOP
      ENDIF
      
      iresult = GetWarningLineCount(id)      

!!!!	DO 45 k=1,10

      CALL SOLUTION(id, 1.0, 1.0, 1.0)
      CALL EQUILIBRIUM_PHASES(id, 'calcite', 0.0, 0.010)
      CALL USER_PUNCH(id, 'Ca', 10)
!!!!      CALL OutputLines
      iresult = SetOutputOn(id, .FALSE.)
      iresult = SetErrorOn(id, .FALSE.)
      iresult = SetLogOn(id, .FALSE.)
      iresult = SetSelectedOutputOn(id, .TRUE.)
      iresult = SetDumpOn(id, .FALSE.)
      iresult = RunAccumulated(id)
      IF (iresult.NE.0) THEN
        CALL OutputError(id)
        STOP
      ENDIF

      rows = GetSelectedOutputRowCount(id)
      cols = GetSelectedOutputColumnCount(id)

!!!      PRINT 10, 'Rows = ', rows
!!!      PRINT 10, 'Cols = ', cols
!!!10    FORMAT(A,I3)

!
!     output simulated selected output
!
      ! headings
      DO 20 j=1,cols
        iresult = GetSelectedOutputValue(id, 0, j, vtype, 
     &                                   dvalue, svalue)
        len = INDEX(svalue, ' ')
        PRINT 50, svalue(1:len-1), ACHAR(9)
20    CONTINUE
      PRINT *

      ! values
      DO 40 i=1,rows
        DO 30 j=1,cols
          iresult = GetSelectedOutputValue(id, i, j, vtype, 
     &                                     dvalue, svalue)
          IF (iresult.EQ.IPQ_OK) THEN
            IF (vtype.eq.TT_EMPTY) THEN
              PRINT 50, ' ', ACHAR(9)
            ELSEIF(vtype.eq.TT_DOUBLE) THEN
              PRINT 60, dvalue, ACHAR(9)
            ELSEIF(vtype.eq.TT_STRING) THEN
              len = INDEX(svalue, ' ')
              PRINT 50, svalue(1:len-1), ACHAR(9)
            ENDIF
          ELSE
            IF (iresult.eq.IPQ_INVALIDROW) THEN
              PRINT 50, 'INVROW', ACHAR(9)
            ELSEIF (iresult.eq.IPQ_INVALIDCOL) THEN
              PRINT 50, 'INVCOL', ACHAR(9)
            ELSE
              PRINT 50, 'ERROR', ACHAR(9)
            ENDIF
          ENDIF
30      CONTINUE
        PRINT *
40    CONTINUE
!!!!45	CONTINUE

      !! test ListComponents
      n = GetComponentCount(id)
      DO i = 1, n
        CALL GetComponent(id, i, comp)
        WRITE (*, *) trim(comp)
      END DO
      
      n = DestroyIPhreeqc(id)

50    FORMAT(A15,A,$)
60    FORMAT(1PG15.7E2,A,$)
      END PROGRAM DRIVER
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE SOLUTION(id, C, Ca, Na)
      INCLUDE 'IPhreeqc.f90.inc'
      INTEGER id
      INTEGER err
      REAL C, Ca, Na
      CHARACTER(80) line
        WRITE (line,100) 'SOLUTION 1'
        err = AccumulateLine(id, line)
        WRITE (line,110) 'C  ', C
        err = AccumulateLine(id, line)
        WRITE (line,110) 'Ca  ', Ca
        err = AccumulateLine(id, line)
        WRITE (line,110) 'Na  ', Na
        err = AccumulateLine(id, line)
100     FORMAT(A)
110     FORMAT(TR4,A,F8.4)
      END SUBROUTINE SOLUTION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE EQUILIBRIUM_PHASES(id, phase, si, amount)
      INCLUDE 'IPhreeqc.f90.inc'
      INTEGER id
      INTEGER err
      REAL si, amount
      CHARACTER*(*) phase
      CHARACTER(80) line
        WRITE (line,'(A)') 'EQUILIBRIUM_PHASES'
        err = AccumulateLine(id, line)
        WRITE (line,'(TR4, A, F8.4, F8.4)') phase, si, amount
        err = AccumulateLine(id, line)		
      END SUBROUTINE EQUILIBRIUM_PHASES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE USER_PUNCH(id, element, max)
      INCLUDE 'IPhreeqc.f90.inc'
      INTEGER id
      INTEGER err
      CHARACTER*(*) element
      INTEGER max
      INTEGER i
      CHARACTER(800) line
      CHARACTER(80) form
      CHARACTER(30) heading(20)
        WRITE (line,200) 'USER_PUNCH'
        err = AccumulateLine(id, line)
        DO i = 1,max
          WRITE (heading(i), 210)
     &                            i, '.name ',
     &                            i, '.type ',
     &                            i, '.moles '
        END DO
        WRITE (line, *) '-head ', (heading(i), i=1,max)
        err = AccumulateLine(id, line)
        WRITE (line, 200) '-start'
        err = AccumulateLine(id, line)
        WRITE (line, 220) '10 n = sys("'
     &                     , element, '", count, names$, types$, moles)'
        err = AccumulateLine(id, line)
        WRITE (line, 230) '20 n = ', max
        err = AccumulateLine(id, line)
        WRITE (line, 240) '30 if count < ', max
     &                            , ' then n = count'
        err = AccumulateLine(id, line)
        WRITE (line, 200) '40 for i = 1 to count'
        err = AccumulateLine(id, line)
        WRITE (line, 200) '50 PUNCH names$(i), types$(i), moles(i)'
        err = AccumulateLine(id, line)
        WRITE (line, 200) '60 next i'
        err = AccumulateLine(id, line)
        WRITE (line, 200) '70 list'
        err = AccumulateLine(id, line)
        WRITE (line, 200) '-end'
        err = AccumulateLine(id, line)
        WRITE (line, 200) 'SELECTED_OUTPUT'
        err = AccumulateLine(id, line)
!!!        WRITE (line, 200), '-file srctest.txt'
!!!        CALL AccumulateLine(line)
200     FORMAT(A)
210     FORMAT(3(I2,A))
220     FORMAT(A,A,A)
230     FORMAT(A,I2)
240     FORMAT(A,I2,A)
      END SUBROUTINE USER_PUNCH
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE PHASES_FIX_PH(id)
      INCLUDE 'IPhreeqc.f90.inc'
      INTEGER id
      INTEGER err
      CHARACTER(80) line
      INTEGER i
        WRITE (line,500) 'PHASES'
        err = AccumulateLine(id, line)
        WRITE (line,510) 'Fix_H+'
        err = AccumulateLine(id, line)
        WRITE (line,510) 'H+ = H+'
        err = AccumulateLine(id, line)
        WRITE (line,510) 'log_k 0.0'
        err = AccumulateLine(id, line)
500     FORMAT(A)
510     FORMAT(TR4,A)
      END SUBROUTINE PHASES_FIX_PH

