MODULE init

IMPLICIT NONE

PRIVATE
PUBLIC :: line, lineorig, new_phreeqc_id, readinput, replacestring, cx, cy

INTEGER(KIND=4) :: ID_IPHREEQC(2),thisid1, thisid2
CHARACTER(LEN=160), DIMENSION(120) :: line = ""
CHARACTER(LEN=160), DIMENSION(120) :: lineorig = ""
CHARACTER(LEN=32) :: cx, cy


contains

!**************************************************************************************************

SUBROUTINE readinput(n)

INTEGER, INTENT(OUT) :: n
INTEGER :: ios

OPEN (UNIT = 11, FILE = 'test_memory_leak.pqi', STATUS = 'OLD', IOSTAT = ios)

IF (ios.NE.0)  STOP 'Could not open input file'

n = 1

DO

  READ (UNIT = 11, FMT = '(A)', IOSTAT=ios) lineorig(n)
  IF (ios.EQ.-1) EXIT
  IF (ios.NE.0)  STOP 'File error.'
  n = n + 1

ENDDO

n = n - 1

CLOSE (UNIT = 11)

IF (n.GT.0) then
  line(1:n) = lineorig(1:n)
ELSE
  STOP 'no chemical input.'
ENDIF

END SUBROUTINE readinput

!**************************************************************************************************

FUNCTION new_phreeqc_id()

INCLUDE 'IPhreeqc.f90.inc'

INTEGER(KIND=4) :: new_phreeqc_id

new_phreeqc_id = CreateIPhreeqc()

END FUNCTION new_phreeqc_id

!**************************************************************************************************

 SUBROUTINE replacestring(string, targetstring, newstring)
 
! replace all occurrences of targetstring with newstring
! trailing blanks (as per TRIM) in targetstring and newstring are removed
! ie cannot be used to replace one blank with two blanks
 
 IMPLICIT NONE
!
! Dummy arguments
!
 CHARACTER(*) ::  string, targetstring, newstring
 INTENT (IN) newstring, targetstring
 INTENT (INOUT) string
!
! Local variables
!
 INTEGER :: len_string, len_target, len_newstring, index_target
 
 ! first check for trivial cases of 'no change'
 IF (TRIM(targetstring).EQ.'' .OR. newstring.EQ.targetstring) RETURN
 
 index_target = 1
 
 len_string = LEN_TRIM(string)
 len_target = LEN_TRIM(targetstring)
 len_newstring = LEN_TRIM(newstring)

 DO WHILE (index_target.GT.0 .AND. index_target.LE.len_string) 
    len_string = LEN_TRIM(string)
    IF (len_target.GT.0) THEN
      index_target = INDEX(string,TRIM(targetstring))
    ELSE
      index_target = INDEX(string,TRIM(targetstring))
    ENDIF
    IF (len_newstring.GT.0) THEN
        IF (index_target.GT.1) THEN
           string = string(1:index_target-1)//TRIM(newstring)//string(index_target+len_target:)
        ELSEIF (index_target.EQ.1) THEN
           string = TRIM(newstring)//string(1+len_target:)
        ENDIF
    ELSE
       IF (index_target.GT.1) THEN
         string = string(1:index_target-1)//string(index_target+len_target:)
       ELSEIF (index_target.EQ.1) then
         string = string(1+len_target:)
       ENDIF
    ENDIF 
 
 ENDDO

 END SUBROUTINE replacestring


END MODULE init
