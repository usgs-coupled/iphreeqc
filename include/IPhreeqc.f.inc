C
C     GetSelectedOutputValue TYPES
C      
      INTEGER :: TT_EMPTY
      PARAMETER (TT_EMPTY=0)
          
      INTEGER :: TT_ERROR
      PARAMETER (TT_ERROR=1)
          
      INTEGER :: TT_DOUBLE
      PARAMETER (TT_DOUBLE=3)
          
      INTEGER :: TT_STRING
      PARAMETER (TT_STRING=4)
          
C
C     IPQ_RESULT TYPES
C      
          
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
          
C
C     function prototypes
C      
          
      INTEGER(KIND=4) AccumulateLine
      INTEGER(KIND=4) CreateIPhreeqc
      INTEGER(KIND=4) DestroyIPhreeqc
      INTEGER(KIND=4) GetComponentCount
      INTEGER(KIND=4) GetDumpLine
      INTEGER(KIND=4) GetDumpLineCount
      LOGICAL(KIND=4) GetDumpOn
      LOGICAL(KIND=4) GetDumpStringOn
      INTEGER(KIND=4) GetErrorLine
      INTEGER(KIND=4) GetErrorLineCount
      LOGICAL(KIND=4) GetErrorOn
      LOGICAL(KIND=4) GetLogOn
      LOGICAL(KIND=4) GetOutputOn
      INTEGER(KIND=4) GetSelectedOutputColumnCount
      LOGICAL(KIND=4) GetSelectedOutputOn
      INTEGER(KIND=4) GetSelectedOutputRowCount
      INTEGER(KIND=4) GetSelectedOutputValue
      INTEGER(KIND=4) GetWarningLine
      INTEGER(KIND=4) GetWarningLineCount
      INTEGER(KIND=4) LoadDatabase
      INTEGER(KIND=4) LoadDatabaseString
      INTEGER(KIND=4) OutputLastError
      INTEGER(KIND=4) OutputLastWarning
      INTEGER(KIND=4) OutputLines
      INTEGER(KIND=4) RunAccumulated
      INTEGER(KIND=4) RunFile
      INTEGER(KIND=4) RunString
      INTEGER(KIND=4) SetDumpOn
      INTEGER(KIND=4) SetDumpStringOn
      INTEGER(KIND=4) SetErrorOn
      INTEGER(KIND=4) SetLogOn
      INTEGER(KIND=4) SetOutputOn
      INTEGER(KIND=4) SetSelectedOutputOn
      INTEGER(KIND=4) UnLoadDatabase
