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
            FUNCTION Run(OUTPUT_ON, ERROR_ON, LOG_ON, SELECTED_ON)
              LOGICAL(KIND=4) :: OUTPUT_ON
              LOGICAL(KIND=4) :: ERROR_ON
              LOGICAL(KIND=4) :: LOG_ON
              LOGICAL(KIND=4) :: SELECTED_ON
              INTEGER(KIND=4) :: Run
            END FUNCTION Run
          END INTERFACE


          INTERFACE
            FUNCTION RunFile(FNAME,OUT_ON,ERR_ON,LOG_ON,SEL_ON)
              CHARACTER(LEN=*) :: FNAME
              LOGICAL(KIND=4)  :: OUT_ON
              LOGICAL(KIND=4)  :: ERR_ON
              LOGICAL(KIND=4)  :: LOG_ON
              LOGICAL(KIND=4)  :: SEL_ON
              INTEGER(KIND=4)  :: RunFile
            END FUNCTION RunFile
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
