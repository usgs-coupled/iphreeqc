      !
      ! GetSelectedOutputValue TYPES
      !
      INTEGER :: TT_EMPTY
      PARAMETER (TT_EMPTY=0)
          
      INTEGER :: TT_ERROR
      PARAMETER (TT_ERROR=1)
          
      INTEGER :: TT_DOUBLE
      PARAMETER (TT_DOUBLE=3)
          
      INTEGER :: TT_STRING
      PARAMETER (TT_STRING=4)
          
      !
      ! IPQ_RESULT TYPES
      !
          
      INTEGER :: IPQ_OK
      PARAMETER (IPQ_OK=0)
          
      INTEGER :: IPQ_OUTOFMEMORY
      PARAMETER (IPQ_OUTOFMEMORY=-1)
         
      INTEGER :: IPQ_BADVARTYPE
      PARAMETER (IPQ_BADVARTYPE=-2)
          
      INTEGER :: IPQ_INVALIDARG
      PARAMETER (IPQ_INVALIDARG=-3)
          
      INTEGER :: IPQ_INVALIDROW
      PARAMETER (IPQ_INVALIDROW=-4)
          
      INTEGER :: IPQ_INVALIDCOL
      PARAMETER (IPQ_INVALIDCOL=-5)
          
      INTEGER :: IPQ_BADINSTANCE
      PARAMETER (IPQ_BADINSTANCE=-6)
          
      !
      ! function prototypes
      !
          
      INTEGER AccumulateLine
      INTEGER CreateIPhreeqc
      INTEGER DestroyIPhreeqc
      INTEGER GetComponent
      INTEGER GetComponentCount
      INTEGER GetDumpLine
      INTEGER GetDumpLineCount
      INTEGER GetErrorLine
      INTEGER GetErrorLineCount
      INTEGER GetSelectedOutputColumnCount
      INTEGER GetSelectedOutputRowCount
      INTEGER GetSelectedOutputValue
      INTEGER GetWarningLine
      INTEGER GetWarningLineCount
      INTEGER LoadDatabase
      INTEGER LoadDatabaseString
!!      INTEGER OutputLastError
      INTEGER OutputLastWarning
      INTEGER OutputLines
      INTEGER RunAccumulated
      INTEGER RunFile
      INTEGER RunString
      INTEGER SetDumpOn
      INTEGER SetDumpStringOn
      INTEGER SetErrorOn
      INTEGER SetLogOn
      INTEGER SetOutputOn
      INTEGER SetSelectedOutputOn
      INTEGER UnLoadDatabase
