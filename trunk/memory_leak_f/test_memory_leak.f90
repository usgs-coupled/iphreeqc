

!**************************************************************************************************


PROGRAM test_memory_leak

USE init

INCLUDE "IPhreeqc.f90.inc"

REAL*8  :: xmin, xmax, ymin, ymax, dx, dy, x, y
INTEGER :: nstep, i, ii, nlines, nt = 0, ios
INTEGER :: id1, accumstat=0, nerr=0, nrow=0, ncol=0

INTEGER(4) :: v
REAL(8) :: d, c
CHARACTER(80) :: s, species, mainspecies

OPEN(UNIT=13, file='test_memory_leak.log', STATUS='UNKNOWN', IOSTAT = ios)
IF (ios.NE.0) STOP 'Could not open log file'

id1 = new_phreeqc_id()
!!nerr = DestroyIPhreeqc(id1)
!!STOP

nerr = SetSelectedOutputFileOn(id1,.TRUE.)
nerr = SetOutputFileOn(id1,.TRUE.)
nerr = SetDumpFileOn(id1,.TRUE.)
nerr = SetErrorFileOn(id1,.TRUE.)
nerr = SetLogFileOn(id1,.TRUE.)

nerr = LoadDatabase(id1,'wateq4f.dat')
IF (nerr.GE.1) THEN
   PRINT *, nerr,' errors reading the database.'
   STOP
ENDIF

CALL readinput(nlines)

LINE(:) = LINEORIG(:)

xmin = 2
xmax = 7

ymin = -80
ymax = 0

!dx = 0.01d0
!nstep = (xmax-xmin)/dx + 1

nstep = 20

dx = (xmax-xmin)/FLOAT(nstep)
dy = (ymax-ymin)/FLOAT(nstep)

mainspecies ="Fe"

DO y = ymin, ymax, dy
    DO x = xmin, xmax, dx

      nt = nt + 1

      WRITE (cx,'(ES20.12)') x
      WRITE (cy,'(ES20.12)') y

      DO i = 1, nlines
         IF (INDEX(line(i),'<x_axis>')>0) THEN
           CALL replacestring(line(i),'<x_axis>',ADJUSTL(cx))
         ENDIF
         IF (INDEX(line(i),'<y_axis>')>0) THEN
           CALL replacestring(line(i),'<y_axis>',ADJUSTL(cy))
         ENDIF
         IF (INDEX(line(i),'<mainspecies>')>0) THEN
           CALL replacestring(line(i),'<mainspecies>',mainspecies)
         ENDIF
         accumstat = AccumulateLine(id1,line(i))
!         print '(i0,t5,A)', i, trim(line(i))
         IF (accumstat.NE.IPQ_OK) THEN
            PRINT *,'Error accumulating line ',i,"."
            STOP
         ENDIF  
      ENDDO
      
      nerr = RunAccumulated(id1) 
       
      IF (nerr.NE.0) THEN
            PRINT *, nerr,' errors when running.'
            CALL OutputErrorString(id1)           
            STOP
      ENDIF
      
      nrow = GetSelectedOutputRowCount(id1)
      ncol = GetSelectedOutputColumnCount(id1)
      
      do i = 1, 2 ! ncol
          if (mod(i,2).EQ.1) then
              nerr = GetSelectedOutputValue(id1,nrow,i,v,c,species)
          else
              nerr = GetSelectedOutputValue(id1,nrow,i,v,d,s) + nerr
              IF (nerr.EQ.0) THEN
                 WRITE (13,'(I0,t15,F10.5,t30,F10.5,t45,A,t60,F11.3)') nt, x, y, TRIM(species), LOG10(d)
                 WRITE (*,'(I0,t15,F10.5,t30,F10.5,t45,A,t60,F11.3)') nt, x, y, TRIM(species), LOG10(d)
              ELSE
                STOP 'bad data.'
              ENDIF
          endif
      enddo
            
      line(:) = lineorig(:)
      line(:) = lineorig(:)
      
   ENDDO
ENDDO 

nerr = DestroyIPhreeqc(id1)

CLOSE (UNIT = 13)

END PROGRAM test_memory_leak

