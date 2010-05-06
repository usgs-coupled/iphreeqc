#ifndef INC_IPHREEQC_H
#define INC_IPHREEQC_H

#include "Var.h"

#if defined(_WINDLL)
#define DLL_EXPORT __declspec(dllexport)
#else
#define DLL_EXPORT
#endif

/*! \brief Enumeration used to return error codes.
*/
typedef enum {
	IPQ_OK            =  0,  /*!< Success */
	IPQ_OUTOFMEMORY   = -1,  /*!< Failure, Out of memory */
	IPQ_BADVARTYPE    = -2,  /*!< Failure, Invalid VAR type */
	IPQ_INVALIDARG    = -3,  /*!< Failure, Invalid argument */
	IPQ_INVALIDROW    = -4,  /*!< Failure, Invalid row */
	IPQ_INVALIDCOL    = -5,  /*!< Failure, Invalid column */
	IPQ_BADINSTANCE   = -6,  /*!< Failure, Invalid instance id */
} IPQ_RESULT;


#if defined(__cplusplus)
extern "C" {
#endif

/**
 *  Accumlulate line(s) for input to phreeqc.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param line          The line(s) to add for input to phreeqc.
 *  @retval IPQ_OK Success
 *  @retval IPQ_OUTOFMEMORY Out of memory
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION AccumulateLine(LINE)
 *    INTEGER(KIND=4),   INTENT(IN)  :: ID
 *    CHARACTER(LEN=*),  INTENT(IN)  :: LINE
 *    INTEGER(KIND=4)                :: AccumulateLine
 *  END FUNCTION AccumulateLine
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT IPQ_RESULT AccumulateLine(int id, const char *line);


/**
 *  Appends the given error message and increments the error count.
 *  Internally used to create an error condition.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param error_msg     The error message to display.
 *  @returns             The current error count if successful; otherwise a negative value indicates an error occured (see \ref IPQ_RESULT).
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  Not implemented.
 *  @endhtmlonly
 */
	DLL_EXPORT int AddError(int id, const char* error_msg);


/**
 *  Create a new IPhreeqc instance.
 *  @return      A non-negative value if successful; otherwise a negative value indicates an error occured (see \ref IPQ_RESULT).
 *  @see         DestroyIPhreeqc
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION CreateIPhreeqc()
 *    INTEGER(KIND=4)  :: CreateIPhreeqc
 *  END FUNCTION CreateIPhreeqc
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int CreateIPhreeqc(void);


/**
 *  Release an IPhreeqc instance from memory.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @retval IPQ_OK Success
 *  @retval IPQ_BADINSTANCE The given id is invalid.
 *  @see                 CreateIPhreeqc
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION DestroyIPhreeqc(ID)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    INTEGER(KIND=4)               :: DestroyIPhreeqc
 *  END FUNCTION DestroyIPhreeqc
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT IPQ_RESULT DestroyIPhreeqc(int id);


/**
 *  Retrieves the given component.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param n             The zero-based index of the component to retrieve.
 *  @return              A null terminated string containing the given component.
 *                       Returns an empty string if n is out of range.
 *  @see                 GetComponentCount
 *  @par Fortran90 Interface:
 *  (Note: N is one-based for the fortran interface)
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE GetComponent(ID,N,COMP)
 *    INTEGER(KIND=4),   INTENT(IN)   :: ID
 *    INTEGER(KIND=4),   INTENT(IN)   :: N
 *    CHARACTER(LEN=*),  INTENT(OUT)  :: COMP
 *  END SUBROUTINE GetComponent
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT const char* GetComponent(int id, int n);


/**
 *  Retrieves the number of components in the current simulation.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @return              The current count of components.
 *                       A negative value indicates an error occured (see \ref IPQ_RESULT).
 *  @see                 GetComponent
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetComponentCount(ID)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    INTEGER(KIND=4)               :: GetComponentCount
 *  END FUNCTION GetComponentCount
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetComponentCount(int id);


/**
 *  Retrieves the given dump line.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param n             The zero-based index of the line to retrieve.
 *  @return              A null terminated string containing the given line.
 *                       Returns an empty string if n is out of range.
 *  @see                 GetDumpLineCount, SetDumpStringOn
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  (Note: N is one-based for the fortran interface.)
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE GetDumpLine(ID,N,LINE)
 *    INTEGER(KIND=4),   INTENT(IN)   :: ID
 *    INTEGER(KIND=4),   INTENT(IN)   :: N
 *    CHARACTER(LEN=*),  INTENT(OUT)  :: LINE
 *  END SUBROUTINE GetComponent
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT const char* GetDumpLine(int id, int n);


/**
 *  Retrieves the number of lines in the current dump string buffer.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @return              The number of lines.
 *  @see                 GetDumpLineCount, SetDumpStringOn
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetDumpLineCount(ID)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    INTEGER(KIND=4)               :: GetDumpLineCount
 *  END FUNCTION GetDumpLineCount
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetDumpLineCount(int id);


/**
 *  Retrieves the current value of the dump flag.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @see                 SetDumpOn
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetDumpOn(DUMP_ON)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    LOGICAL(KIND=4)               :: GetDumpOn
 *  END FUNCTION SetDumpOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetDumpOn(int id);


/**
 *  Retrieves the string buffer containing <b>DUMP</b> output.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @pre                 \ref SetDumpStringOn must have been set to true (non-zero) in order to recieve <b>DUMP</b> output.
 *  @see                 SetDumpStringOn
 *  @par Fortran90 Interface:
 *  Not implemented. (see \ref GetDumpLineCount, \ref GetDumpLine)
 */
	DLL_EXPORT const char* GetDumpString(int id);


/**
 *  Retrieves the current value of the dump string flag.
 *  @param id                  The instance id returned from \ref CreateIPhreeqc.
 *  @see SetDumpStringOn, GetDumpString
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetDumpStringOn(DUMP_STRING_ON)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    LOGICAL(KIND=4)               :: GetDumpStringOn
 *  END FUNCTION GetDumpStringOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetDumpStringOn(int id);


/**
 *  Retrieves the given error line.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param n             The zero-based index of the line to retrieve.
 *  @see                 GetErrorLineCount
 *  @par Fortran90 Interface:
 *  (Note: N is one-based for the fortran interface.)
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE GetErrorLine
 *    INTEGER(KIND=4),   INTENT(IN)   :: ID
 *    INTEGER(KIND=4),   INTENT(IN)   :: N
 *    CHARACTER(LEN=*),  INTENT(OUT)  :: LINE
 *  END SUBROUTINE GetErrorLine
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT const char* GetErrorLine(int id, int n);


/**
 *  Retrieves the number of lines in the current error string buffer.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @see                 GetErrorLine
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetErrorLineCount
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    INTEGER(KIND=4)               :: GetErrorLineCount
 *  END FUNCTION GetErrorLineCount
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetErrorLineCount(int id);


/**
 *  Retrieves the current value of the error flag.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @see                 SetErrorOn
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetErrorOn(ERROR_ON)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    LOGICAL(KIND=4)               :: GetErrorOn
 *  END FUNCTION GetErrorOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetErrorOn(int id);


/**
 *  Retrieves the error messages from the last call to \ref RunAccumulated, \ref RunFile, \ref LoadDatabase, or \ref LoadDatabaseString.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @par Fortran90 Interface:
 *  Not implemented. (see \ref GetErrorLineCount, \ref GetErrorLine, \ref OutputError)
 */
	DLL_EXPORT const char* GetErrorString(int id);


/**
 *  Retrieves the warning messages from the last call to \ref RunAccumulated, \ref RunFile, \ref LoadDatabase, or \ref LoadDatabaseString.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @par Fortran90 Interface:
 *  Not implemented. (see \ref GetWarningLineCount, \ref GetWarningLine, \ref OutputWarning)
 */
	DLL_EXPORT const char* GetWarningString(int id);


/**
 *  Retrieves the current value of the log flag.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @see SetLogOn
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetLogOn(ID)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    LOGICAL(KIND=4)               :: GetLogOn
 *  END FUNCTION GetLogOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetLogOn(int id);


/**
 *  Retrieves the current value of the output flag.
 *  @param id                 The instance id returned from \ref CreateIPhreeqc.
 *  @see SetOutputOn
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetOutputOn(ID)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *  END FUNCTION GetOutputOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetOutputOn(int id);


/**
 *  Returns the number of columns currently contained within selected_output.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @see                 GetSelectedOutputRowCount, GetSelectedOutputValue
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetSelectedOutputColumnCount
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    INTEGER(KIND=4)               :: GetSelectedOutputColumnCount
 *  END FUNCTION GetSelectedOutputColumnCount
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetSelectedOutputColumnCount(int id);


/**
 *  Retrieves the selected_output flag.
 *  @param id                    The instance id returned from \ref CreateIPhreeqc.
 *  @see                         SetSelectedOutputOn
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetSelectedOutputOn(SELECTED_ON)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    LOGICAL(KIND=4)               :: GetSelectedOutputOn
 *  END FUNCTION GetSelectedOutputOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetSelectedOutputOn(int id);


/**
 *  Returns the number of rows currently contained within selected_output.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @see GetSelectedOutputColumnCount, GetSelectedOutputValue
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetSelectedOutputRowCount
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    INTEGER(KIND=4)               :: GetSelectedOutputRowCount
 *  END FUNCTION GetSelectedOutputRowCount
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetSelectedOutputRowCount(int id);


/**
 *  Returns the \c VAR associated with the specified row and column.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param row  The row index.
 *  @param col  The column index.
 *  @param pVAR Pointer to the \c VAR to recieve the requested data.
 *  @retval IPQ_OK Success
 *  @retval IPQ_INVALIDROW The given row is out of range.
 *  @retval IPQ_INVALIDCOL The given column is out of range.
 *  @retval IPQ_OUTOFMEMORY Memory could not be allocated.
 *  @retval IPQ_BADINSTANCE The given id is invalid.
 *  @remarks
 *  Row 0 contains the column headings to the selected_ouput.
 *  @par Examples:
 *  The headings will include a suffix and/or prefix in order to differentiate the
 *  columns.
 *  @htmlonly
<p>
<table border=1>

<TR VALIGN="top">
<TH width=65%>
Input
</TH>
<TH width=35%>
Headings
</TH>
</TR>

<TR VALIGN="top">
<TD width=65%>
<CODE><PRE>
  SELECTED_OUTPUT
    -reset false
    -totals Ca Na
</PRE></CODE>
</TD>
<TD width=35%>
<CODE><PRE>
  Ca(mol/kgw)  Na(mol/kgw)
</PRE></CODE>
</TD>
</TR>

<TR VALIGN="top">
<TD width=65%>
<CODE><PRE>
  SELECTED_OUTPUT
    -reset false
    -molalities Fe+2 Hfo_sOZn+
</PRE></CODE>
</TD>
<TD width=35%>
<CODE><PRE>
  m_Fe+2(mol/kgw)  m_Hfo_sOZn+(mol/kgw)
</PRE></CODE>
</TD>
</TR>

<TR VALIGN="top">
<TD width=65%>
<CODE><PRE>
  SELECTED_OUTPUT
    -reset false
    -activities H+ Ca+2
</PRE></CODE>
</TD>
<TD width=35%>
<CODE><PRE>
  la_H+  la_Ca+2
</PRE></CODE>
</TD>
</TR>

<TR VALIGN="top">
<TD width=65%>
<CODE><PRE>
  SELECTED_OUTPUT
    -reset false
    -equilibrium_phases Calcite Dolomite
</PRE></CODE>
</TD>
<TD width=35%>
<CODE><PRE>
  Calcite  d_Calcite  Dolomite  d_Dolomite
</PRE></CODE>
</TD>
</TR>

<TR VALIGN="top">
<TD width=65%>
<CODE><PRE>
  SELECTED_OUTPUT
    -reset false
    -saturation_indices CO2(g) Siderite
</PRE></CODE>
</TD>
<TD width=35%>
<CODE><PRE>
  si_CO2(g)  si_Siderite
</PRE></CODE>
</TD>
</TR>

<TR VALIGN="top">
<TD width=65%>
<CODE><PRE>
  SELECTED_OUTPUT
    -reset false
    -gases CO2(g) N2(g)
</PRE></CODE>
</TD>
<TD width=35%>
<CODE><PRE>
  pressure "total mol" volume g_CO2(g) g_N2(g)
</PRE></CODE>
</TD>
</TR>

<TR VALIGN="top">
<TD width=65%>
<CODE><PRE>
  SELECTED_OUTPUT
    -reset false
    -kinetic_reactants CH2O Pyrite
</PRE></CODE>
</TD>
<TD width=35%>
<CODE><PRE>
  k_CH2O dk_CH2O k_Pyrite dk_Pyrite
</PRE></CODE>
</TD>
</TR>

<TR VALIGN="top">
<TD width=65%>
<CODE><PRE>
  SELECTED_OUTPUT
    -reset false
    -solid_solutions CaSO4 SrSO4
</PRE></CODE>
</TD>
<TD width=35%>
<CODE><PRE>
  s_CaSO4 s_SrSO4
</PRE></CODE>
</TD>
</TR>

</table>
 *  @endhtmlonly
 *  @par Fortran90 Interface:
 *  ROW is 1-based for the fortran interface except that the column headings are stored in ROW=0.
 *  COL is 1-based for the fortran interface.
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetSelectedOutputValue(ID,ROW,COL,VTYPE,DVALUE,SVALUE)
 *    INTEGER(KIND=4),   INTENT(IN)   :: ID
 *    INTEGER(KIND=4),   INTENT(IN)   :: ROW
 *    INTEGER(KIND=4),   INTENT(IN)   :: COL
 *    INTEGER(KIND=4),   INTENT(OUT)  :: VTYPE
 *    REAL*8,            INTENT(OUT)  :: DVALUE
 *    CHARACTER(LEN=*),  INTENT(OUT)  :: SVALUE
 *    INTEGER(KIND=4)                 :: GetSelectedOutputValue
 *  END FUNCTION GetSelectedOutputValue
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT IPQ_RESULT GetSelectedOutputValue(int id, int row, int col, VAR* pVAR);


/**
 *  Retrieves the current value of the dump flag.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param n             The zero-based index of the line to retrieve.
 *  @see                 GetWarningLineCount, OutputWarning
 *  @par Fortran90 Interface:
 *  (Note: N is one-based for the fortran interface.)
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE GetWarningLine(ID,N,LINE)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    LOGICAL(KIND=4),  INTENT(IN)  :: DUMP_ON
 *  END SUBROUTINE GetWarningLine
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT const char* GetWarningLine(int id, int n);


/**
 *  Retrieves the number of lines in the current warning string buffer.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetWarningLineCount(ID)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    INTEGER(KIND=4)               :: GetWarningLineCount
 *  END FUNCTION GetWarningLineCount
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetWarningLineCount(int id);


/**
 *  Load the specified database file into phreeqc.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param filename The name of the phreeqc database to load.
 *         The full path will be required if the file is not
 *         in the current working directory.
 *  @return The number of errors encountered.
 *  @remarks
 *  Any previous database definitions are cleared.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION LoadDatabase(FILENAME)
 *    INTEGER(KIND=4),   INTENT(IN)  :: ID
 *    CHARACTER(LEN=*),  INTENT(IN)  :: FILENAME
 *    INTEGER(KIND=4)                :: LoadDatabase
 *  END FUNCTION LoadDatabase
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int LoadDatabase(int id, const char* filename);


/**
 *  Load the specified string as a database into phreeqc.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param input         String containing data to be used as the phreeqc database.
 *  @return              The number of errors encountered.
 *  @remarks
 *  Any previous database definitions are cleared.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION LoadDatabaseString(INPUT)
 *    INTEGER(KIND=4),   INTENT(IN)  :: ID
 *    CHARACTER(LEN=*),  INTENT(IN)  :: INPUT
 *    INTEGER(KIND=4)                :: LoadDatabaseString
 *  END FUNCTION LoadDatabaseString
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int LoadDatabaseString(int id, const char* input);


/**
 *  Output the error messages normally stored in the phreeqc.err file to stdout.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE OutputError
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *  END SUBROUTINE OutputError
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT void OutputError(int id);


/**
 *  Output the warning messages to stdout.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE OutputWarning
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *  END SUBROUTINE OutputWarning
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT void OutputWarning(int id);


/**
 *  Output the accumulated input to stdout.  This is the input that will be
 *  used for the next call to RunAccumulated.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE OutputLines
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *  END SUBROUTINE OutputLines
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT void OutputLines(int id);


/**
 *  Runs the accumulated input sent to AccumulateLine.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @return              The number of errors encountered.
 *  @remarks
 *  The accumulated input is cleared upon a successful run (no errors).
 *  @pre \ref LoadDatabase/\ref LoadDatabaseString must have been called and returned 0 (zero) errors.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION RunAccumulated()
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    INTEGER(KIND=4)               :: RunAccumulated
 *  END FUNCTION RunAccumulated
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int RunAccumulated(int id);


/**
 *  Runs the specified phreeqc input file.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param filename      The name of the phreeqc input file to run.
 *  @return              The number of errors encountered during the run.
 *  @pre                 \ref LoadDatabase/\ref LoadDatabaseString must have been called and returned 0 (zero) errors.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION RunFile(FILENAME)
 *    INTEGER(KIND=4),   INTENT(IN)  :: ID
 *    CHARACTER(LEN=*),  INTENT(IN)  :: FILENAME
 *    INTEGER(KIND=4)                :: RunFile
 *  END FUNCTION RunFile
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int RunFile(int id, const char* filename);


/**
 *  Runs the specified string as input to phreeqc.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param input         String containing phreeqc input.
 *  @return              The number of errors encountered during the run.
 *  @pre                 \ref LoadDatabase/\ref LoadDatabaseString must have been called and returned 0 (zero) errors.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION RunString(INPUT)
 *    INTEGER(KIND=4),  INTENT(IN)   :: ID
 *    CHARACTER(LEN=*),  INTENT(IN)  :: INPUT
 *    INTEGER(KIND=4)                :: RunString
 *  END FUNCTION RunString
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int RunString(int id, const char* input);


/**
 *  Sets the dump flag on or off.  This flag controls whether or not phreeqc writes to the dump file.
 *  The default after calling \ref CreateIPhreeqc is off.
 *  @param id                   The instance id returned from \ref CreateIPhreeqc.
 *  @param dump_on              If non-zero turns on output to the <B>DUMP</B> (<B>dump.out</B> if unspecified) file.
 *  @retval IPQ_OK              Success
 *  @retval IPQ_BADINSTANCE     The given id is invalid.
 *  @see
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION SetDumpOn(ID,DUMP_ON)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    LOGICAL(KIND=4),  INTENT(IN)  :: DUMP_ON
 *  END FUNCTION SetDumpOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT IPQ_RESULT SetDumpOn(int id, int dump_on);


/**
 *  Sets the dump string flag on or off.  This flag controls whether or not the data normally sent
 *  to the dump file is stored in a buffer for later retrieval.  The default after calling
 *  \ref CreateIPhreeqc is off.
 *  @param id                The instance id returned from \ref CreateIPhreeqc.
 *  @param dump_string_on    If non-zero captures the output defined by the <B>DUMP</B> keyword into a string buffer.
 *  @retval IPQ_OK Success
 *  @retval IPQ_BADINSTANCE The given id is invalid.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION SetDumpStringOn(ID,DUMP_STRING_ON)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    LOGICAL(KIND=4),  INTENT(IN)  :: DUMP_STRING_ON
 *  END FUNCTION SetDumpStringOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT IPQ_RESULT SetDumpStringOn(int id, int dump_string_on);


/**
 *  Sets the error flag on or off.  This flag controls whether or not
 *  error messages are written to the phreeqc.err file.  The default after calling
 *  \ref CreateIPhreeqc is off.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param error_on      If non-zero turns on output to the <B>phreeqc.err</B> file.
 *  @retval IPQ_OK Success
 *  @retval IPQ_BADINSTANCE The given id is invalid.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION SetErrorOn(ERROR_ON)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    LOGICAL(KIND=4),  INTENT(IN)  :: ERROR_ON
 *  END FUNCTION SetErrorOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT IPQ_RESULT SetErrorOn(int id, int error_on);


/**
 *  Sets the log flag on or off.  This flag controls whether or not phreeqc
 *  writes log messages to the phreeqc.log file.  The default after calling
 *  \ref CreateIPhreeqc is off.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param log_on        If non-zero turns on output to the <B>phreeqc.log</B> file.
 *  @retval              IPQ_OK Success
 *  @retval              IPQ_BADINSTANCE The given id is invalid.
 *  @see                 GetLogOn
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION SetLogOn(ID,LOG_ON)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    LOGICAL(KIND=4),  INTENT(IN)  :: LOG_ON
 *    INTEGER(KIND=4)               :: SetLogOn
 *  END FUNCTION SetLogOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT IPQ_RESULT SetLogOn(int id, int log_on);


/**
 *  Sets the output flag on or off.  This flag controls whether or not phreeqc
 *  writes output to phreeqc.out.  This output is the output normally generated
 *  when phreeqc is run.  The default after calling \ref CreateIPhreeqc is off.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param output_on     If non-zero turns on output to the <B>phreeqc.out</B> file.
 *  @retval IPQ_OK Success
 *  @retval IPQ_BADINSTANCE The given id is invalid.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION SetOutputOn(ID,OUTPUT_ON)
 *    INTEGER(KIND=4),  INTENT(IN)  :: ID
 *    LOGICAL(KIND=4),  INTENT(IN)  :: OUTPUT_ON
 *    INTEGER(KIND=4)               :: SetOutputOn
 *  END FUNCTION SetOutputOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT IPQ_RESULT SetOutputOn(int id, int output_on);


/**
 *  Sets the selected output flag on or off.  This flag controls whether or not phreeqc writes output to
 *  the selected output file. The default after calling \ref CreateIPhreeqc is off.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @param sel_on        If non-zero turns on output to the <B>SELECTED_OUTPUT</B> (<B>selected.out</B> if unspecified) file.
 *  @retval IPQ_OK Success
 *  @retval IPQ_BADINSTANCE The given id is invalid.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION SetSelectedOutputOn(ID,SEL_ON)
 *    INTEGER(KIND=4),  INTENT(IN) :: ID
 *    LOGICAL(KIND=4),  INTENT(IN) :: SEL_ON
 *    INTEGER(KIND=4)              :: SetSelectedOutputOn
 *  END FUNCTION SetSelectedOutputOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT IPQ_RESULT SetSelectedOutputOn(int id, int sel_on);



/**
 *  Unloads any database currently loaded into phreeqc.  In addition, all
 *  previous phreeqc definitions (ie SOLUTIONS, EXCHANGERS, etc) are cleared from memory.
 *  @param id            The instance id returned from \ref CreateIPhreeqc.
 *  @remarks
 *  Use of the method is not normally necessary.  It is called automatically
 *  before each call to \ref LoadDatabase or \ref LoadDatabaseString.
 */
	DLL_EXPORT IPQ_RESULT UnLoadDatabase(int id);


// TODO int RunWithCallback(PFN_PRERUN_CALLBACK pfn_pre, PFN_POSTRUN_CALLBACK pfn_post, void *cookie, int output_on, int error_on, int log_on, int selected_output_on);


// TODO int CatchErrors(PFN_CATCH_CALLBACK pfn, void *cookie);


#if defined(__cplusplus)
}
#endif

#endif // INC_IPHREEQC_H
