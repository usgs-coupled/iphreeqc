#ifndef INC_IPHREEQC_H
#define INC_IPHREEQC_H

#include <map>
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


class IPhreeqc;

#if defined(__cplusplus)
extern "C" {
#endif

/**
 *  Appends the given error message and increments the error count.
 *  Internally used to create an error condition.
 *  @internal
 */
	DLL_EXPORT int AddError(int id, const char* error_msg);

/**
 *  Create a new IPhreeqc instance.
 *  @return A IPhreeqc instance id.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION CreateIPhreeqc()
 *    INTEGER(KIND=4) :: CreateIPhreeqc
 *  END FUNCTION CreateIPhreeqc
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int CreateIPhreeqc(void);

/**
 *  Release an IPhreeqc instance from memory.
 *  @param id Id returned from CreateIPhreeqc
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION DestroyIPhreeqc(ID)
 *    INTEGER(KIND=4) :: ID
 *    INTEGER(KIND=4) :: DestroyIPhreeqc
 *  END FUNCTION DestroyIPhreeqc
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT IPQ_RESULT DestroyIPhreeqc(int id);

/**
 *  Load the specified database file into phreeqc.
 *  @param id Id returned from CreateIPhreeqc
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
 *    CHARACTER(LEN=*), INTENT(IN) :: FILENAME
 *    INTEGER :: LoadDatabase
 *  END FUNCTION LoadDatabase
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int LoadDatabase(int id, const char* filename);

/**
 *  Load the specified string as a database into phreeqc.
 *  @param input String containing data to be used as the phreeqc database.
 *  @return The number of errors encountered.
 *  @remarks
 *  Any previous database definitions are cleared.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION LoadDatabaseString(INPUT)
 *    CHARACTER(LEN=*), INTENT(IN) :: INPUT
 *    INTEGER :: LoadDatabaseString
 *  END FUNCTION LoadDatabaseString
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int LoadDatabaseString(int id, const char* input);

/**
 *  Unload any database currently loaded into phreeqc.
 *  @remarks
 *  Any previous database definitions are cleared.
 */
	DLL_EXPORT int UnLoadDatabase(int id);

/**
 *  Output the error messages normally stored in the phreeqc.err file to stdout.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE OutputLastError
 *  END SUBROUTINE OutputLastError
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT void OutputLastError(int id);

	DLL_EXPORT void OutputLastWarning(int id);

	DLL_EXPORT const char* GetLastErrorString(int id);

	DLL_EXPORT const char* GetLastWarningString(int id);

	DLL_EXPORT const char* GetDumpString(int id);

	DLL_EXPORT int GetDumpLineCount(int id);

	DLL_EXPORT const char* GetDumpLine(int id, int n);

/**
 *  Accumlulate lines for input to phreeqc.
 *  @param line The line(s) to add for input to phreeqc.
 *  @retval VR_OK Success
 *  @retval VR_OUTOFMEMORY Out of memory
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION AccumulateLine(LINE)
 *    CHARACTER(LEN=*), INTENT(IN) :: LINE
 *    INTEGER :: AccumulateLine
 *  END FUNCTION AccumulateLine
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT IPQ_RESULT AccumulateLine(int id, const char *line);

/**
 *  Sets the selected_output flag on or off
 *  @param selected_output_on If non-zero turns on output to the <B>SELECTED_OUTPUT</B> (<B>selected.out</B> if unspecified) file.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE SetSelectedOutputOn(SELECTED_ON)
 *    LOGICAL, INTENT(IN) :: SELECTED_ON
 *  END SUBROUTINE SetSelectedOutputOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetSelectedOutputOn(int id);
	DLL_EXPORT IPQ_RESULT SetSelectedOutputOn(int id, int value);

/**
 *  Sets the output flag on or off
 *  @param output_on          If non-zero turns on output to the <B>phreeqc.out</B> file.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE SetOutputOn(OUTPUT_ON)
 *    LOGICAL, INTENT(IN) :: OUTPUT_ON
 *  END SUBROUTINE SetOutputOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetOutputOn(int id);
	DLL_EXPORT IPQ_RESULT SetOutputOn(int id, int value);

/**
 *  Sets the error flag on or off
 *  @param error_on           If non-zero turns on output to the <B>phreeqc.err</B> file.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE SetErrorOn(ERROR_ON)
 *    LOGICAL, INTENT(IN) :: ERROR_ON
 *  END SUBROUTINE SetOutputOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetErrorOn(int id);
	DLL_EXPORT IPQ_RESULT SetErrorOn(int id, int value);

/**
 *  Sets the log flag on or off
 *  @param log_on             If non-zero turns on output to the <B>phreeqc.log</B> file.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE SetLogOn(LOG_ON)
 *    LOGICAL, INTENT(IN) :: LOG_ON
 *  END SUBROUTINE SetLogOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetLogOn(int id);
	DLL_EXPORT IPQ_RESULT SetLogOn(int id, int value);

/**
 *  Sets the dump flag on or off
 *  @param dump_on             If non-zero turns on output to the <B>DUMP</B> (<B>dump.out</B> if unspecified) file.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE SetDumpOn(DUMP_ON)
 *    LOGICAL, INTENT(IN) :: DUMP_ON
 *  END SUBROUTINE SetDumpOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetDumpOn(int id);
	DLL_EXPORT IPQ_RESULT SetDumpOn(int id, int value);

/**
 *  Sets the dump string flag on or off
 *  @param dump_string_on      If non-zero captures as a string the output defined by the <B>DUMP</B> keyword.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE SetDumpStringOn(DUMP_STRING_ON)
 *    LOGICAL, INTENT(IN) :: DUMP_STRING_ON
 *  END SUBROUTINE SetDumpStringOn
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetDumpStringOn(int id);
	DLL_EXPORT IPQ_RESULT SetDumpStringOn(int id, int value);

/**
 *  Runs the accumulated input sent to AccumulateLine.
 *  @return The number of errors encountered.
 *  @remarks
 *  The accumulated input is cleared upon completion.
 *  @pre LoadDatabase/LoadDatabaseString must have been called and returned 0 (zero) errors.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION RunAccumulated()
 *    INTEGER :: RunAccumulated
 *  END FUNCTION RunAccumulated
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int RunAccumulated(int id);

/**
 *  Runs the specified phreeqc input file.
 *  @param filename           The name of the phreeqc input file to run.
 *  @return The number of errors encountered during the run.
 *  @pre LoadDatabase/LoadDatabaseString must have been called and returned 0 (zero) errors.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION RunFile(FILENAME)
 *    CHARACTER(LEN=*)    :: FILENAME
 *    INTEGER :: RunFile
 *  END FUNCTION RunFile
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int RunFile(int id, const char* filename);

/**
 *  Runs the specified string as input to phreeqc.
 *  @param input              String containing phreeqc input.
 *  @return The number of errors encountered during the run.
 *  @pre LoadDatabase/LoadDatabaseString must have been called and returned 0 (zero) errors.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION RunString(INPUT)
 *    CHARACTER(LEN=*)    :: INPUT
 *    INTEGER :: RunString
 *  END FUNCTION RunString
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int RunString(int id, const char* input);

/**
 *  Returns the number of rows currently contained within selected_output.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetSelectedOutputRowCount
 *    INTEGER :: GetSelectedOutputRowCount
 *  END FUNCTION GetSelectedOutputRowCount
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetSelectedOutputRowCount(int id);

/**
 *  Returns the number of columns currently contained within selected_output.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetSelectedOutputColumnCount
 *    INTEGER :: GetSelectedOutputColumnCount
 *  END FUNCTION GetSelectedOutputColumnCount
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetSelectedOutputColumnCount(int id);

/**
 *  Returns the \c VAR associated with the specified row and column.
 *  @param row  The row index.
 *  @param col  The column index.
 *  @param pVAR Pointer to the \c VAR to recieve the requested data.
 *  @retval VR_OK Success
 *  @retval VR_INVALIDROW The given row is out of range.
 *  @retval VR_INVALIDCOL The given column is out of range.
 *  @retval VR_OUTOFMEMORY Memory could not be allocated.
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
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetSelectedOutputValue(ROW,COL,VTYPE,DVALUE,SVALUE)
 *    INTEGER,          INTENT(IN)  :: ROW
 *    INTEGER,          INTENT(IN)  :: COL
 *    INTEGER,          INTENT(OUT) :: VTYPE
 *    REAL*8,           INTENT(OUT) :: DVALUE
 *    CHARACTER(LEN=*), INTENT(OUT) :: SVALUE
 *    INTEGER :: GetSelectedOutputValue
 *  END FUNCTION GetSelectedOutputValue
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT IPQ_RESULT GetSelectedOutputValue(int id, int row, int col, VAR* pVAR);

/**
 *  TODO
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetComponentCount
 *    INTEGER :: GetComponentCount
 *  END FUNCTION GetComponentCount
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetComponentCount(int id);

/**
 *  TODO
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetComponent
 *    INTEGER :: GetComponent
 *  END FUNCTION GetComponent
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT const char* GetComponent(int id, int n);

/**
 *  Send the accumulated input to stdout.
 *  This is the input that will be used for the next call to RunAccumulated.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE OutputLines
 *  END SUBROUTINE OutputLines
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT void OutputLines(int id);

// TODO int RunWithCallback(PFN_PRERUN_CALLBACK pfn_pre, PFN_POSTRUN_CALLBACK pfn_post, void *cookie, int output_on, int error_on, int log_on, int selected_output_on);

// TODO int CatchErrors(PFN_CATCH_CALLBACK pfn, void *cookie);

/**
 *  TODO
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetErrorLineCount
 *    INTEGER :: GetErrorLineCount
 *  END FUNCTION GetErrorLineCount
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetErrorLineCount(int id);

/**
 *  TODO
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE GetErrorLine
 *    INTEGER,          INTENT(IN)  :: N
 *    CHARACTER(LEN=*), INTENT(OUT) :: LINE
 *  END SUBROUTINE GetErrorLine
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT const char* GetErrorLine(int id, int n);

/**
 *  TODO
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION GetErrorLineCount
 *    INTEGER :: GetErrorLineCount
 *  END FUNCTION GetErrorLineCount
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT int GetWarningLineCount(int id);

/**
 *  TODO
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  SUBROUTINE GetErrorLine
 *    INTEGER,          INTENT(IN)  :: N
 *    CHARACTER(LEN=*), INTENT(OUT) :: LINE
 *  END SUBROUTINE GetErrorLine
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
	DLL_EXPORT const char* GetWarningLine(int id, int n);



#if defined(__cplusplus)
}
#endif

#endif // INC_IPHREEQC_H