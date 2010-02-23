#ifndef __IPHREEQC_H
#define __IPHREEQC_H

#include "Var.h"

#if defined(__cplusplus)
extern "C" {
#endif
/**
 *  Load the specified database file into phreeqc.
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
int     LoadDatabase(const char *filename);

/**
 *  Load the specified string as a database into phreeqc.
 *  @param input String containing data to be used as the phreeqc database.
 *  @return The number of errors encountered.
 *  @remarks
 *  Any previous database definitions are cleared.
 */
int     LoadDatabaseString(const char *input);

/**
 *  Unload any database currently loaded into phreeqc.
 *  @remarks
 *  Any previous database definitions are cleared.
 */
void    UnLoadDatabase(void);

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
void    OutputLastError(void);

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
VRESULT AccumulateLine(const char *line);

/**
 *  Runs the accumulated input sent to AccumulateLine.
 *  @param output_on          If non-zero turns on output to the <B>phreeqc.out</B> file.
 *  @param error_on           If non-zero turns on output to the <B>phreeqc.err</B> file.
 *  @param log_on             If non-zero turns on output to the <B>phreeqc.log</B> file.
 *  @param selected_output_on If non-zero turns on output to the <B>SELECTED_OUTPUT</B> (<B>selected.out</B> if unspecified) file.
 *  @return The number of errors encountered.
 *  @remarks
 *  The accumulated input is cleared upon completion.
 *  @pre LoadDatabase/LoadDatabaseString must have been called and returned 0 (zero) errors.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION Run(OUTPUT_ON, ERROR_ON, LOG_ON, SELECTED_ON)
 *    LOGICAL, INTENT(IN) :: OUTPUT_ON
 *    LOGICAL, INTENT(IN) :: ERROR_ON
 *    LOGICAL, INTENT(IN) :: LOG_ON
 *    LOGICAL, INTENT(IN) :: SELECTED_ON
 *    INTEGER :: Run
 *  END FUNCTION Run
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
int     Run(int output_on, int error_on, int log_on, int selected_output_on);

/**
 *  Runs the specified phreeqc input file.
 *  @param filename           The name of the phreeqc input file to run.
 *  @param output_on          If non-zero turns on output to the <B>phreeqc.out</B> file.
 *  @param error_on           If non-zero turns on output to the <B>phreeqc.err</B> file.
 *  @param log_on             If non-zero turns on output to the <B>phreeqc.log</B> file.
 *  @param selected_output_on If non-zero turns on output to the <B>SELECTED_OUTPUT</B> (<B>selected.out</B> if unspecified) file.
 *  @return The number of errors encountered during the run.
 *  @pre LoadDatabase/LoadDatabaseString must have been called and returned 0 (zero) errors.
 *  @par Fortran90 Interface:
 *  @htmlonly
 *  <CODE>
 *  <PRE>
 *  FUNCTION RunFile(FILENAME,OUTPUT_ON,ERROR_ON,LOG_ON,SELECTED_ON)
 *    CHARACTER(LEN=*)    :: FILENAME
 *    LOGICAL, INTENT(IN) :: OUTPUT_ON
 *    LOGICAL, INTENT(IN) :: ERROR_ON
 *    LOGICAL, INTENT(IN) :: LOG_ON
 *    LOGICAL, INTENT(IN) :: SELECTED_ON
 *    INTEGER :: RunFile
 *  END FUNCTION RunFile
 *  </PRE>
 *  </CODE>
 *  @endhtmlonly
 */
int     RunFile(const char *filename, int output_on, int error_on, int log_on, int selected_output_on);

/**
 *  Runs the specified phreeqc input file.
 *  @param input              String containing phreeqc input.
 *  @param output_on          If non-zero turns on output to the <B>phreeqc.out</B> file.
 *  @param error_on           If non-zero turns on output to the <B>phreeqc.err</B> file.
 *  @param log_on             If non-zero turns on output to the <B>phreeqc.log</B> file.
 *  @param selected_output_on If non-zero turns on output to the <B>SELECTED_OUTPUT</B> (<B>selected.out</B> if unspecified) file.
 *  @return The number of errors encountered during the run.
 *  @pre LoadDatabase/LoadDatabaseString must have been called and returned 0 (zero) errors.
 */
int     RunString(const char *input, int output_on, int error_on, int log_on, int selected_output_on);


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
int     GetSelectedOutputRowCount(void);

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
int     GetSelectedOutputColumnCount(void);

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
VRESULT GetSelectedOutputValue(int row, int col, VAR* pVAR);

/**
 *  Appends the given error message and increments the error count.
 *  Internally used to create an error condition.
 *  @internal
 */
size_t  AddError(const char* error_msg);

/**
 *  TODO
 *  @internal
 */
/*
void ClearErrors(void);
*/


/** 
 *  Send the accumulated input to stdout. 
 *  This is the input that will be used for the next call to Run.
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
void    OutputLines(void);


typedef int (*PFN_PRERUN_CALLBACK)(void *cookie);
typedef int (*PFN_POSTRUN_CALLBACK)(void *cookie);
typedef int (*PFN_CATCH_CALLBACK)(void *cookie);

int RunWithCallback(PFN_PRERUN_CALLBACK pfn_pre, PFN_POSTRUN_CALLBACK pfn_post, void *cookie, int output_on, int error_on, int log_on, int selected_output_on);

int CatchErrors(PFN_CATCH_CALLBACK pfn, void *cookie);

const char* GetLastErrorString(void);

#if defined(WIN32)
void DebugOutputLines(void);
#endif

#if defined(__cplusplus)
}
#endif

#endif  /* __IPHREEQC_H */
