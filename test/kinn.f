!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      PROGRAM DRIVER

      IMPLICIT NONE
      INCLUDE 'IPhreeqc.f.inc'
      INTEGER iresult
      INTEGER i, j, k, rows, cols
      INTEGER vtype
      CHARACTER(30) svalue
      INTEGER len
      REAL*8 dvalue

      iresult = LoadDatabase('llnl.dat')

      IF (iresult.NE.VR_OK) THEN
        CALL OutputLastError
        STOP
      ENDIF

!!!!	DO 45 k=1,10

      CALL SOLUTION(1.0, 1.0, 1.0)
      CALL EQUILIBRIUM_PHASES('calcite', 0.0, 0.010)
      CALL USER_PUNCH('Ca', 10)
!!!!      CALL OutputLines
      iresult = Run(.FALSE., .FALSE., .FALSE., .TRUE.)
      IF (iresult.NE.VR_OK) THEN
        CALL OutputLastError
        STOP
      ENDIF

      rows = GetSelectedOutputRowCount()
      cols = GetSelectedOutputColumnCount()

!!!!      PRINT 10, 'Rows = ', rows
!!!!      PRINT 10, 'Cols = ', cols
10    FORMAT(A,I3)

!
!     output simulated selected output
!
      ! headings
      DO 20 j=1,cols
        iresult = GetSelectedOutputValue(0, j, vtype, dvalue, svalue)
        len = INDEX(svalue, ' ')
        PRINT 50, svalue(1:len-1), ACHAR(9)
20    CONTINUE
      PRINT *

      ! values
      DO 40 i=1,rows
        DO 30 j=1,cols
          iresult = GetSelectedOutputValue(i, j, vtype, dvalue, svalue)
          IF (iresult.EQ.VR_OK) THEN
            IF (vtype.eq.TT_EMPTY) THEN
              PRINT 50, ' ', ACHAR(9)
            ELSEIF(vtype.eq.TT_DOUBLE) THEN
              PRINT 60, dvalue, ACHAR(9)
            ELSEIF(vtype.eq.TT_STRING) THEN
              len = INDEX(svalue, ' ')
              PRINT 50, svalue(1:len-1), ACHAR(9)
            ENDIF
          ELSE
            IF (iresult.eq.VR_INVALIDROW) THEN
              PRINT 50, 'INVROW', ACHAR(9)
            ELSEIF (iresult.eq.VR_INVALIDCOL) THEN
              PRINT 50, 'INVCOL', ACHAR(9)
            ELSE
              PRINT 50, 'ERROR', ACHAR(9)
            ENDIF
          ENDIF
30      CONTINUE
        PRINT *
40    CONTINUE
!!!!45	CONTINUE

50    FORMAT(A15,A,$)
60    FORMAT(1PG15.7E2,A,$)
      END PROGRAM DRIVER
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE SOLUTION(C, Ca, Na)
      REAL C, Ca, Na
      CHARACTER(80) line
        WRITE (line,100),'SOLUTION 1'
        CALL AccumulateLine(line)
        WRITE (line,110),'C  ', C
        CALL AccumulateLine(line)
        WRITE (line,110),'Ca  ', Ca
        CALL AccumulateLine(line)
        WRITE (line,110),'Na  ', Na
        CALL AccumulateLine(line)
100     FORMAT(A)
110     FORMAT(TR4,A,F8.4)
      END SUBROUTINE SOLUTION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE EQUILIBRIUM_PHASES(phase, si, amount)
      REAL si, amount
      CHARACTER*(*) phase
      CHARACTER(80) line
        WRITE (line,'(A)'),'EQUILIBRIUM_PHASES'
        CALL AccumulateLine(line)
        WRITE (line,'(TR4, A, F8.4, F8.4)'), phase, si, amount
        CALL AccumulateLine(line)		
      END SUBROUTINE EQUILIBRIUM_PHASES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE USER_PUNCH(element, max)
      CHARACTER*(*) element
      INTEGER max
      INTEGER i
      CHARACTER(800) line
      CHARACTER(80) form
      CHARACTER(30) heading(20)
        WRITE (line,200),'USER_PUNCH'
        CALL AccumulateLine(line)
        DO i = 1,max
          WRITE (heading(i), 210),
     &                            i, '.name ',
     &                            i, '.type ',
     &                            i, '.moles '
        END DO
        WRITE (line, *), '-head ', (heading(i), i=1,max)
        CALL AccumulateLine(line)
        WRITE (line, 200), '-start'
        CALL AccumulateLine(line)
        WRITE (line, 220), '10 n = sys("'
     &                     , element, '", count, names$, types$, moles)'
        CALL AccumulateLine(line)
        WRITE (line, 230), '20 n = ', max
        CALL AccumulateLine(line)
        WRITE (line, 240), '30 if count < ', max
     &                            , ' then n = count'
        CALL AccumulateLine(line)
        WRITE (line, 200), '40 for i = 1 to count'
        CALL AccumulateLine(line)
        WRITE (line, 200), '50 PUNCH names$(i), types$(i), moles(i)'
        CALL AccumulateLine(line)
        WRITE (line, 200), '60 next i'
        CALL AccumulateLine(line)
        WRITE (line, 200), '70 list'
        CALL AccumulateLine(line)
        WRITE (line, 200), '-end'
        CALL AccumulateLine(line)
        WRITE (line, 200), 'SELECTED_OUTPUT'
        CALL AccumulateLine(line)
!!!        WRITE (line, 200), '-file srctest.txt'
!!!        CALL AccumulateLine(line)
200     FORMAT(A)
210     FORMAT(3(I2,A))
220     FORMAT(A,A,A)
230     FORMAT(A,I2)
240     FORMAT(A,I2,A)
      END SUBROUTINE USER_PUNCH
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE PHASES_FIX_PH
      CHARACTER(80) line
      INTEGER i
        WRITE (line,500),'PHASES'
        CALL AccumulateLine(line)
        WRITE (line,510),'Fix_H+'
        CALL AccumulateLine(line)
        WRITE (line,510),'H+ = H+'
        CALL AccumulateLine(line)
        WRITE (line,510),'log_k 0.0'
        CALL AccumulateLine(line)
500     FORMAT(A)
510     FORMAT(TR4,A)
      END SUBROUTINE PHASES_FIX_PH

