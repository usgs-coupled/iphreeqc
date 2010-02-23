          ! GetSelectedOutputValue TYPES
          INTEGER :: TT_EMPTY
          PARAMETER (TT_EMPTY=0)
          
          INTEGER :: TT_ERROR
          PARAMETER (TT_ERROR=1)
          
          INTEGER :: TT_DOUBLE
          PARAMETER (TT_DOUBLE=3)
          
          INTEGER :: TT_STRING
          PARAMETER (TT_STRING=4)
          
          
          ! ERROR (TT_ERROR) TYPES
          INTEGER :: VR_OK
          PARAMETER (VR_OK=0)
          
          INTEGER :: VR_OUTOFMEMORY
          PARAMETER (VR_OUTOFMEMORY=1)
          
          INTEGER :: VR_BADVARTYPE
          PARAMETER (VR_BADVARTYPE=2)
          
          INTEGER :: VR_INVALIDARG
          PARAMETER (VR_INVALIDARG=3)
          
          INTEGER :: VR_INVALIDROW
          PARAMETER (VR_INVALIDROW=4)
          
          INTEGER :: VR_INVALIDCOL
          PARAMETER (VR_INVALIDCOL=5)
          
          
          ! function prototypes
          
          INTEGER AccumulateLine
          INTEGER GetSelectedOutputColumnCount
          INTEGER GetSelectedOutputRowCount
          INTEGER GetSelectedOutputValue
          INTEGER LoadDatabase
          INTEGER Run
          INTEGER RunFile                      !! (FILENAME,OUTPUT_ON,ERROR_ON,LOG_ON,SELECTED_ON)


!!REM          INTERFACE
!!REM            FUNCTION LoadDatabase(FILENAME)
!!REM              CHARACTER(LEN=*) :: FILENAME
!!REM              INTEGER(KIND=4)  :: LoadDatabase
!!REM            END FUNCTION LoadDatabase
!!REM          END INTERFACE
!!REM
!!REM
!!REM          INTERFACE 
!!REM            SUBROUTINE OutputLastError
!!REM            END SUBROUTINE OutputLastError
!!REM          END INTERFACE 
!!REM
!!REM
!!REM          INTERFACE
!!REM            FUNCTION AccumulateLine(LINE)
!!REM              CHARACTER(LEN=*) :: LINE
!!REM              INTEGER(KIND=4)  :: AccumulateLine
!!REM            END FUNCTION AccumulateLine
!!REM          END INTERFACE
!!REM
!!REM
!!REM          INTERFACE
!!REM            FUNCTION Run(OUTPUT_ON, ERROR_ON, LOG_ON, SELECTED_ON)
!!REM              LOGICAL(KIND=4) :: OUTPUT_ON
!!REM              LOGICAL(KIND=4) :: ERROR_ON
!!REM              LOGICAL(KIND=4) :: LOG_ON
!!REM              LOGICAL(KIND=4) :: SELECTED_ON
!!REM              INTEGER(KIND=4) :: Run
!!REM            END FUNCTION Run
!!REM          END INTERFACE
!!REM
!!REM
!!REM          INTERFACE 
!!REM            SUBROUTINE OutputLines
!!REM            END SUBROUTINE OutputLines
!!REM          END INTERFACE 
!!REM
!!REM
!!REM          INTERFACE
!!REM            FUNCTION GetSelectedOutputRowCount
!!REM              INTEGER(KIND=4) :: GetSelectedOutputRowCount
!!REM            END FUNCTION GetSelectedOutputRowCount
!!REM          END INTERFACE
!!REM
!!REM
!!REM          INTERFACE
!!REM            FUNCTION GetSelectedOutputColumnCount
!!REM              INTEGER(KIND=4) :: GetSelectedOutputColumnCount
!!REM            END FUNCTION GetSelectedOutputColumnCount
!!REM          END INTERFACE
!!REM
!!REM
!!REM          INTERFACE
!!REM            FUNCTION GetSelectedOutputValue(ROW,COL,VTYPE,DVALUE,SVALUE)
!!REM              INTEGER(KIND=4)  :: ROW
!!REM              INTEGER(KIND=4)  :: COL
!!REM              INTEGER(KIND=4)  :: VTYPE
!!REM              REAL(KIND=8)     :: DVALUE
!!REM              CHARACTER(LEN=*) :: SVALUE
!!REM              INTEGER(KIND=4)  :: GetSelectedOutputValue
!!REM            END FUNCTION GetSelectedOutputValue
!!REM          END INTERFACE
