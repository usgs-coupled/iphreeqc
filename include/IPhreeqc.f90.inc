          ! GetSelectedOutputValue TYPES
          INTEGER,PARAMETER :: TT_EMPTY  = 0
          INTEGER,PARAMETER :: TT_ERROR  = 1
          INTEGER,PARAMETER :: TT_DOUBLE = 3
          INTEGER,PARAMETER :: TT_STRING = 4


          ! ERROR (TT_ERROR) TYPES
          INTEGER,PARAMETER :: VR_OK           = 0
          INTEGER,PARAMETER :: VR_OUTOFMEMORY  = 1
          INTEGER,PARAMETER :: VR_BADVARTYPE   = 2
          INTEGER,PARAMETER :: VR_INVALIDARG   = 3
          INTEGER,PARAMETER :: VR_INVALIDROW   = 4
          INTEGER,PARAMETER :: VR_INVALIDCOL   = 5


          INTERFACE
            FUNCTION LoadDatabase(FILENAME)
              CHARACTER(LEN=*) :: FILENAME
              INTEGER(KIND=4)  :: LoadDatabase
            END FUNCTION LoadDatabase
          END INTERFACE


          INTERFACE
            FUNCTION LoadDatabaseString(INPUT)
              CHARACTER(LEN=*) :: INPUT
              INTEGER(KIND=4)  :: LoadDatabaseString
            END FUNCTION LoadDatabaseString
          END INTERFACE


          INTERFACE 
            SUBROUTINE OutputLastError
            END SUBROUTINE OutputLastError
          END INTERFACE 


          INTERFACE
            FUNCTION AccumulateLine(LINE)
              CHARACTER(LEN=*) :: LINE
              INTEGER(KIND=4)  :: AccumulateLine
            END FUNCTION AccumulateLine
          END INTERFACE


          INTERFACE
            FUNCTION RunAccumulated()
              INTEGER(KIND=4) :: RunAccumulated
            END FUNCTION RunAccumulated
          END INTERFACE


          INTERFACE
            FUNCTION RunFile(FNAME)
              CHARACTER(LEN=*) :: FNAME
              INTEGER(KIND=4)  :: RunFile
            END FUNCTION RunFile
          END INTERFACE


          INTERFACE
            FUNCTION RunString(INPUT)
              CHARACTER(LEN=*) :: INPUT
              INTEGER(KIND=4)  :: RunString
            END FUNCTION RunString
          END INTERFACE


          INTERFACE
            SUBROUTINE SetSelectedOutputOn(SEL_ON)
              LOGICAL(KIND=4)  :: SEL_ON
            END SUBROUTINE SetSelectedOutputOn
          END INTERFACE


          INTERFACE
            SUBROUTINE SetOutputOn(OUT_ON)
              LOGICAL(KIND=4)  :: OUT_ON
            END SUBROUTINE SetOutputOn
          END INTERFACE


          INTERFACE
            SUBROUTINE SetErrorOn(ERR_ON)
              LOGICAL(KIND=4)  :: ERR_ON
            END SUBROUTINE SetErrorOn
          END INTERFACE


          INTERFACE
            SUBROUTINE SetLogOn(LOG_ON)
              LOGICAL(KIND=4)  :: LOG_ON
            END SUBROUTINE SetLogOn
          END INTERFACE


          INTERFACE
            SUBROUTINE SetDumpOn(DUMP_ON)
              LOGICAL(KIND=4)  :: DUMP_ON
            END SUBROUTINE SetDumpOn
          END INTERFACE


          INTERFACE
            SUBROUTINE SetDumpStringOn(DUMP_STRING_ON)
              LOGICAL(KIND=4)  :: DUMP_STRING_ON
            END SUBROUTINE SetDumpStringOn
          END INTERFACE


          INTERFACE
            SUBROUTINE OutputLines
            END SUBROUTINE OutputLines
          END INTERFACE 


          INTERFACE
            FUNCTION GetSelectedOutputRowCount()
              INTEGER(KIND=4) :: GetSelectedOutputRowCount
            END FUNCTION GetSelectedOutputRowCount
          END INTERFACE


          INTERFACE
            FUNCTION GetSelectedOutputColumnCount()
              INTEGER(KIND=4) :: GetSelectedOutputColumnCount
            END FUNCTION GetSelectedOutputColumnCount
          END INTERFACE


          INTERFACE
            FUNCTION GetSelectedOutputValue(ROW,COL,VTYPE,DVALUE,SVALUE)
              INTEGER(KIND=4)  :: ROW
              INTEGER(KIND=4)  :: COL
              INTEGER(KIND=4)  :: VTYPE
              REAL(KIND=8)     :: DVALUE
              CHARACTER(LEN=*) :: SVALUE
              INTEGER(KIND=4)  :: GetSelectedOutputValue
            END FUNCTION GetSelectedOutputValue
          END INTERFACE


          INTERFACE
            FUNCTION GetDumpLineCount()
              INTEGER(KIND=4)  :: GetDumpLineCount
            END FUNCTION GetDumpLineCount
          END INTERFACE


          INTERFACE
            SUBROUTINE GetDumpLine(N,LINE)
              INTEGER(KIND=4)  :: N
              CHARACTER(LEN=*) :: LINE
            END SUBROUTINE
          END INTERFACE


          INTERFACE
            FUNCTION GetErrorLineCount()
              INTEGER(KIND=4)  :: GetErrorLineCount
            END FUNCTION GetErrorLineCount
          END INTERFACE


          INTERFACE
            SUBROUTINE GetErrorLine(N,LINE)
              INTEGER(KIND=4)  :: N
              CHARACTER(LEN=*) :: LINE
            END SUBROUTINE
          END INTERFACE


          INTERFACE
            FUNCTION GetComponentCount()
              INTEGER(KIND=4)  :: GetComponentCount
            END FUNCTION GetComponentCount
          END INTERFACE


          INTERFACE
            SUBROUTINE GetComponent(N,COMP)
              INTEGER(KIND=4)  :: N
              CHARACTER(LEN=*) :: COMP
            END SUBROUTINE
          END INTERFACE
