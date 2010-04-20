       ! GetSelectedOutputValue TYPES
       INTEGER,PARAMETER :: TT_EMPTY  = 0
       INTEGER,PARAMETER :: TT_ERROR  = 1
       INTEGER,PARAMETER :: TT_DOUBLE = 3
       INTEGER,PARAMETER :: TT_STRING = 4


       ! ERROR RETURN TYPES
       INTEGER,PARAMETER :: IPQ_OK           =  0
       INTEGER,PARAMETER :: IPQ_OUTOFMEMORY  = -1
       INTEGER,PARAMETER :: IPQ_BADVARTYPE   = -2
       INTEGER,PARAMETER :: IPQ_INVALIDARG   = -3
       INTEGER,PARAMETER :: IPQ_INVALIDROW   = -4
       INTEGER,PARAMETER :: IPQ_INVALIDCOL   = -5
       INTEGER,PARAMETER :: IPQ_BADINSTANCE  = -6


       INTERFACE
        FUNCTION CreateIPhreeqc()
         INTEGER(KIND=4) :: CreateIPhreeqc
        END FUNCTION CreateIPhreeqc
       END INTERFACE


       INTERFACE
        FUNCTION DestroyIPhreeqc(ID)
         INTEGER(KIND=4), INTENT(IN)  :: ID
         INTEGER(KIND=4)              :: DestroyIPhreeqc
        END FUNCTION DestroyIPhreeqc
       END INTERFACE


       INTERFACE
        FUNCTION LoadDatabase(ID,FILENAME)
         INTEGER(KIND=4),  INTENT(IN) :: ID
         CHARACTER(LEN=*), INTENT(IN) :: FILENAME
         INTEGER(KIND=4)              :: LoadDatabase
        END FUNCTION LoadDatabase
       END INTERFACE


       INTERFACE
        FUNCTION LoadDatabaseString(ID,INPUT)
         INTEGER(KIND=4)  :: ID
         CHARACTER(LEN=*) :: INPUT
         INTEGER(KIND=4)  :: LoadDatabaseString
        END FUNCTION LoadDatabaseString
       END INTERFACE


       INTERFACE 
        SUBROUTINE OutputLastError(ID)
         INTEGER(KIND=4)  :: ID
        END SUBROUTINE OutputLastError
       END INTERFACE 


       INTERFACE
        FUNCTION AccumulateLine(ID, LINE)
         INTEGER(KIND=4)  :: ID
         CHARACTER(LEN=*) :: LINE
         INTEGER(KIND=4)  :: AccumulateLine
        END FUNCTION AccumulateLine
       END INTERFACE


       INTERFACE
        FUNCTION RunAccumulated(ID)
         INTEGER(KIND=4) :: ID
         INTEGER(KIND=4) :: RunAccumulated
        END FUNCTION RunAccumulated
       END INTERFACE


       INTERFACE
        FUNCTION RunFile(ID,FNAME)
         INTEGER(KIND=4)  :: ID
         CHARACTER(LEN=*) :: FNAME
         INTEGER(KIND=4)  :: RunFile
        END FUNCTION RunFile
       END INTERFACE


       INTERFACE
        FUNCTION RunString(ID,INPUT)
         INTEGER(KIND=4)  :: ID
         CHARACTER(LEN=*) :: INPUT
         INTEGER(KIND=4)  :: RunString
        END FUNCTION RunString
       END INTERFACE


       INTERFACE
        FUNCTION SetSelectedOutputOn(ID,SEL_ON)
         INTEGER(KIND=4)  :: ID
         LOGICAL(KIND=4)  :: SEL_ON
         INTEGER(KIND=4)  :: SetSelectedOutputOn
        END FUNCTION SetSelectedOutputOn
       END INTERFACE


       INTERFACE
        FUNCTION SetOutputOn(ID,OUT_ON)
         INTEGER(KIND=4)  :: ID
         LOGICAL(KIND=4)  :: OUT_ON
         INTEGER(KIND=4)  :: SetOutputOn
        END FUNCTION SetOutputOn
       END INTERFACE


       INTERFACE
        FUNCTION SetErrorOn(ID,ERR_ON)
         INTEGER(KIND=4)  :: ID
         LOGICAL(KIND=4)  :: ERR_ON
         INTEGER(KIND=4)  :: SetErrorOn
        END FUNCTION SetErrorOn
       END INTERFACE


       INTERFACE
        FUNCTION SetLogOn(ID,LOG_ON)
         INTEGER(KIND=4)  :: ID
         LOGICAL(KIND=4)  :: LOG_ON
         INTEGER(KIND=4)  :: SetLogOn
        END FUNCTION SetLogOn
       END INTERFACE


       INTERFACE
        FUNCTION SetDumpOn(ID,DUMP_ON)
         INTEGER(KIND=4)  :: ID
         LOGICAL(KIND=4)  :: DUMP_ON
         INTEGER(KIND=4)  :: SetDumpOn
        END FUNCTION SetDumpOn
       END INTERFACE


       INTERFACE
        FUNCTION SetDumpStringOn(ID,DUMP_STRING_ON)
         INTEGER(KIND=4)  :: ID
         LOGICAL(KIND=4)  :: DUMP_STRING_ON
         INTEGER(KIND=4)  :: SetDumpStringOn
        END FUNCTION SetDumpStringOn
       END INTERFACE


       INTERFACE
        SUBROUTINE OutputLines(ID)
         INTEGER(KIND=4)  :: ID
        END SUBROUTINE OutputLines
       END INTERFACE 


       INTERFACE
        FUNCTION GetSelectedOutputRowCount(ID)
         INTEGER(KIND=4) :: ID
         INTEGER(KIND=4) :: GetSelectedOutputRowCount
        END FUNCTION GetSelectedOutputRowCount
       END INTERFACE


       INTERFACE
        FUNCTION GetSelectedOutputColumnCount(ID)
         INTEGER(KIND=4) :: ID
         INTEGER(KIND=4) :: GetSelectedOutputColumnCount
        END FUNCTION GetSelectedOutputColumnCount
       END INTERFACE


       INTERFACE
        FUNCTION GetSelectedOutputValue(ID,ROW,COL,VTYPE,DVALUE,SVALUE)
         INTEGER(KIND=4)  :: ID
         INTEGER(KIND=4)  :: ROW
         INTEGER(KIND=4)  :: COL
         INTEGER(KIND=4)  :: VTYPE
         REAL(KIND=8)     :: DVALUE
         CHARACTER(LEN=*) :: SVALUE
         INTEGER(KIND=4)  :: GetSelectedOutputValue
        END FUNCTION GetSelectedOutputValue
       END INTERFACE


       INTERFACE
        FUNCTION GetDumpLineCount(ID)
         INTEGER(KIND=4)  :: ID
         INTEGER(KIND=4)  :: GetDumpLineCount
        END FUNCTION GetDumpLineCount
       END INTERFACE


       INTERFACE
        SUBROUTINE GetDumpLine(ID,N,LINE)
         INTEGER(KIND=4)  :: ID
         INTEGER(KIND=4)  :: N
         CHARACTER(LEN=*) :: LINE
        END SUBROUTINE
       END INTERFACE


       INTERFACE
        FUNCTION GetErrorLineCount(ID)
         INTEGER(KIND=4)  :: ID
         INTEGER(KIND=4)  :: GetErrorLineCount
        END FUNCTION GetErrorLineCount
       END INTERFACE


       INTERFACE
        SUBROUTINE GetErrorLine(ID,N,LINE)
         INTEGER(KIND=4)  :: ID
         INTEGER(KIND=4)  :: N
         CHARACTER(LEN=*) :: LINE
        END SUBROUTINE
       END INTERFACE


       INTERFACE
        FUNCTION GetWarningLineCount(ID)
         INTEGER(KIND=4)  :: ID
         INTEGER(KIND=4)  :: GetWarningLineCount
        END FUNCTION GetWarningLineCount
       END INTERFACE


       INTERFACE
        SUBROUTINE GetWarningLine(ID,N,LINE)
         INTEGER(KIND=4)  :: ID
         INTEGER(KIND=4)  :: N
         CHARACTER(LEN=*) :: LINE
        END SUBROUTINE
       END INTERFACE


       INTERFACE
        FUNCTION GetComponentCount(ID)
         INTEGER(KIND=4)  :: ID
         INTEGER(KIND=4)  :: GetComponentCount
        END FUNCTION GetComponentCount
       END INTERFACE


       INTERFACE
        FUNCTION GetComponent(ID,N,COMP)
         INTEGER(KIND=4)  :: ID
         INTEGER(KIND=4)  :: N
         CHARACTER(LEN=*) :: COMP
         INTEGER(KIND=4)  :: GetComponent
        END FUNCTION GetComponent
       END INTERFACE
