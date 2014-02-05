#include <R.h>
#include <Rdefines.h>
#include <sstream>
#include <stdlib.h>
#include "Var.h"
#include "IPhreeqc.hpp"

static IPhreeqc s_iphreeqc;


extern "C" {

SEXP
accumLine(SEXP line)
{
  const char* str_in;

  // check args
  if (!isString(line) || length(line) != 1) {
    error("AccumulateLine:line is not a single string\n");
  }

  str_in = CHAR(STRING_ELT(line, 0));

  if (s_iphreeqc.AccumulateLine(str_in) != VR_OK) {
    std::ostringstream oss;
    oss << s_iphreeqc.GetErrorString();
    error(oss.str().c_str());
  }

  return(R_NilValue);
}

SEXP
clearAccum(void)
{
  s_iphreeqc.ClearAccumulatedLines();
  return(R_NilValue);
}

SEXP
getErrorFileOn(void)
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(LGLSXP, 1));
  if (s_iphreeqc.GetErrorFileOn()) {
    LOGICAL(ans)[0] = TRUE;
  }
  else {
    LOGICAL(ans)[0] = FALSE;
  }
  UNPROTECT(1);
  return(ans);
}

SEXP
setDumpFileOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!isLogical(value) || length(value) != 1) {
    error("SetDumpFileOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  s_iphreeqc.SetDumpFileOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setErrorFileOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!isLogical(value) || length(value) != 1) {
    error("SetErrorFileOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  s_iphreeqc.SetErrorFileOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setLogFileOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!isLogical(value) || length(value) != 1) {
    error("SetLogFileOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  s_iphreeqc.SetLogFileOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setOutputFileOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!isLogical(value) || length(value) != 1) {
    error("SetOutputFileOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  s_iphreeqc.SetOutputFileOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setSelectedOutputFileOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!isLogical(value) || length(value) != 1) {
    error("SetSelectedOutputFileOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  s_iphreeqc.SetSelectedOutputFileOn(LOGICAL(value)[0]);
  return(ans);
}

//{{
SEXP
setDumpStringOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!isLogical(value) || length(value) != 1) {
    error("SetDumpStringOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  s_iphreeqc.SetDumpStringOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setErrorStringOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!isLogical(value) || length(value) != 1) {
    error("SetErrorStringOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  s_iphreeqc.SetErrorStringOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setLogStringOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!isLogical(value) || length(value) != 1) {
    error("SetLogStringOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  s_iphreeqc.SetLogStringOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setOutputStringOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!isLogical(value) || length(value) != 1) {
    error("SetOutputStringOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  s_iphreeqc.SetOutputStringOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP  
setSelectedOutputStringOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!isLogical(value) || length(value) != 1) {
    error("SetSelectedOutputStringOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  s_iphreeqc.SetSelectedOutputStringOn(LOGICAL(value)[0]);
  return(ans);
}

//{{
SEXP
setDumpFileName(SEXP filename)
{
  const char* name;
  SEXP ans = R_NilValue;
  // check args
  if (!isString(filename) || length(filename) != 1) {
    error("SetDumpFileName:filename is not a single string\n");
  }

  name = CHAR(STRING_ELT(filename, 0));
  s_iphreeqc.SetDumpFileName(name);
  return(ans);
}

SEXP
setErrorFileName(SEXP filename)
{
  const char* name;
  SEXP ans = R_NilValue;
  // check args
  if (!isString(filename) || length(filename) != 1) {
    error("SetErrorFileName:filename is not a single string\n");
  }

  name = CHAR(STRING_ELT(filename, 0));
  s_iphreeqc.SetErrorFileName(name);
  return(ans);
}

SEXP
setLogFileName(SEXP filename)
{
  const char* name;
  SEXP ans = R_NilValue;
  // check args
  if (!isString(filename) || length(filename) != 1) {
    error("SetLogFileName:filename is not a single string\n");
  }

  name = CHAR(STRING_ELT(filename, 0));
  s_iphreeqc.SetLogFileName(name);
  return(ans);
}

SEXP
setOutputFileName(SEXP filename)
{
  const char* name;
  SEXP ans = R_NilValue;
  // check args
  if (!isString(filename) || length(filename) != 1) {
    error("SetOutputFileName:filename is not a single string\n");
  }

  name = CHAR(STRING_ELT(filename, 0));
  s_iphreeqc.SetOutputFileName(name);
  return(ans);
}

SEXP
setSelectedOutputFileName(SEXP filename)
{
  const char* name;
  SEXP ans = R_NilValue;
  // check args
  if (!isString(filename) || length(filename) != 1) {
    error("SetSelectedOutputFileName:filename is not a single string\n");
  }

  name = CHAR(STRING_ELT(filename, 0));
  s_iphreeqc.SetSelectedOutputFileName(name);
  return(ans);
}
//}}

////{{
SEXP
getDumpString(void)
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(s_iphreeqc.GetDumpString()));
  UNPROTECT(1);
  return ans;
}

SEXP
getLogString(void)
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(s_iphreeqc.GetLogString()));
  UNPROTECT(1);
  return ans;
}

SEXP
getOutputString(void)
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(s_iphreeqc.GetOutputString()));
  UNPROTECT(1);
  return ans;
}

SEXP
getSelectedOutputString(void)
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(s_iphreeqc.GetSelectedOutputString()));
  UNPROTECT(1);
  return ans;
}

SEXP
getWarningString(void)
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(s_iphreeqc.GetWarningString()));
  UNPROTECT(1);
  return ans;
}
////}}

//{{
SEXP
getDumpFileName(void)
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(s_iphreeqc.GetDumpFileName()));
  UNPROTECT(1);
  return ans;
}

SEXP
getErrorFileName(void)
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(s_iphreeqc.GetErrorFileName()));
  UNPROTECT(1);
  return ans;
}

SEXP
getLogFileName(void)
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(s_iphreeqc.GetLogFileName()));
  UNPROTECT(1);
  return ans;
}

SEXP
getOutputFileName(void)
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(s_iphreeqc.GetOutputFileName()));
  UNPROTECT(1);
  return ans;
}

SEXP
getSelectedOutputFileName(void)
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(s_iphreeqc.GetSelectedOutputFileName()));
  UNPROTECT(1);
  return ans;
}
//}}

SEXP
listComps(void)
{
  SEXP ans = R_NilValue;

  std::list< std::string > lc = s_iphreeqc.ListComponents();
  if (lc.size() > 0) {
    PROTECT(ans = allocVector(STRSXP, lc.size()));
    std::list< std::string >::iterator li = lc.begin();
    for (int i = 0; li != lc.end(); ++i, ++li) {
      SET_STRING_ELT(ans, i, mkChar((*li).c_str()));
    }
    UNPROTECT(1);
    return(ans);
  }

  return(R_NilValue);
}

SEXP
loadDB(SEXP filename)
{
  const char* name;

  // check args
  if (!isString(filename) || length(filename) != 1) {
    error("filename is not a single string\n");
  }

  name = CHAR(STRING_ELT(filename, 0));

  if (s_iphreeqc.LoadDatabase(name) != VR_OK) {
    std::ostringstream oss;
    oss << s_iphreeqc.GetErrorString();
    error(oss.str().c_str());
  }

  return(R_NilValue);
}

SEXP
loadDBStr(SEXP input)
{
  const char* string;

  // check args
  if (!isString(input) || length(input) != 1) {
    error("input is not a single string\n");
  }

  string = CHAR(STRING_ELT(input, 0));

  if (s_iphreeqc.LoadDatabaseString(string) != VR_OK) {
    std::ostringstream oss;
    oss << s_iphreeqc.GetErrorString();
    error(oss.str().c_str());
  }

  return(R_NilValue);
}

SEXP
getAccumLines(void)
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(s_iphreeqc.GetAccumulatedLines().c_str()));
  UNPROTECT(1);
  return ans;
}

SEXP
phreeqcDat(void)
{
  extern const char PHREEQC_DAT[];
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(PHREEQC_DAT));
  UNPROTECT(1);
  return ans;
}

SEXP
runAccum(void)
{
  if (s_iphreeqc.RunAccumulated() != VR_OK) {
    std::ostringstream oss;
    oss << s_iphreeqc.GetErrorString();
    error(oss.str().c_str());
  }
  return(R_NilValue);
}

SEXP
runFile(SEXP filename)
{
  const char* name;

  // check args
  if (!isString(filename) || length(filename) != 1) {
    error("RunFile: filename is not a single string\n");
  }

  name = CHAR(STRING_ELT(filename, 0));
  if (s_iphreeqc.RunFile(name) != VR_OK) {
    std::ostringstream oss;
    oss << s_iphreeqc.GetErrorString();
    error(oss.str().c_str());
  }

  return(R_NilValue);
}

SEXP
runString(SEXP input)
{
  const char* in;

  // check args
  if (!isString(input) || length(input) != 1) {
    error("RunString: input is not a single string\n");
  }

  in = CHAR(STRING_ELT(input, 0));
  if (s_iphreeqc.RunString(in) != VR_OK) {
    std::ostringstream oss;
    oss << s_iphreeqc.GetErrorString();
    error(oss.str().c_str());
  }

  return(R_NilValue);
}



/* SEXP */
/* getColumnCount() */
/* { */
/*   SEXP cols = R_NilValue; */
/*   PROTECT(cols = allocVector(INTSXP, 1)); */
/*   INTEGER(cols)[0] = GetSelectedOutputColumnCount(); */
/*   UNPROTECT(1); */
/*   return cols; */
/* } */

/* SEXP */
/* getRowCount() */
/* { */
/*   SEXP rows = R_NilValue; */
/*   PROTECT(rows = allocVector(INTSXP, 1)); */
/*   INTEGER(rows)[0] = GetSelectedOutputRowCount(); */
/*   UNPROTECT(1); */
/*   return rows; */
/* } */

SEXP
getCol(int ncol)
{
  int r;
  int cols;
  int rows;
  VAR vn;
  VAR vv;

  SEXP ans = R_NilValue;

  cols = s_iphreeqc.GetSelectedOutputColumnCount();
  rows = s_iphreeqc.GetSelectedOutputRowCount();
  if (cols == 0 || rows == 0) {
    //error("getColumn: no data\n");
    return ans;
  }

  VarInit(&vn);
  s_iphreeqc.GetSelectedOutputValue(0, ncol, &vn);

  VarInit(&vv);
  s_iphreeqc.GetSelectedOutputValue(1, ncol, &vv);

  switch (vv.type) {
  case TT_LONG:
    PROTECT(ans = allocVector(INTSXP, rows-1));
    for (r = 1; r < rows; ++r) {
      VarInit(&vv);
      s_iphreeqc.GetSelectedOutputValue(r, ncol, &vv);
      if (vv.lVal == -99) {
	INTEGER(ans)[r-1] = NA_INTEGER;
      }
      else {
	INTEGER(ans)[r-1] = vv.lVal;
      }
      VarClear(&vv);
    }
    UNPROTECT(1);
    break;
  case TT_DOUBLE:
    PROTECT(ans = allocVector(REALSXP, rows-1));
    for (r = 1; r < rows; ++r) {
      VarInit(&vv);
      s_iphreeqc.GetSelectedOutputValue(r, ncol, &vv);
      if (vv.dVal == -999.999) {
	REAL(ans)[r-1] = NA_REAL;
      }
      else {
	REAL(ans)[r-1] = vv.dVal;
      }
      VarClear(&vv);
    }
    UNPROTECT(1);
    break;
  case TT_STRING:
    PROTECT(ans = allocVector(STRSXP, rows-1));
    for (r = 1; r < rows; ++r) {
      VarInit(&vv);
      s_iphreeqc.GetSelectedOutputValue(r, ncol, &vv);
      SET_STRING_ELT(ans, r-1, mkChar(vv.sVal));
      VarClear(&vv);
    }
    UNPROTECT(1);
    break;
  case TT_EMPTY:
    break;
  case TT_ERROR:
    break;
  }
  return ans;
}


/* SEXP */
/* getColumn(SEXP column) */
/* { */
/*   int ncol; */

/*   PROTECT(column = AS_INTEGER(column)); */
/*   ncol = INTEGER_POINTER(column)[0]; */
/*   UNPROTECT(1); */

/*   return getCol(ncol); */
/* } */

#define CONVERT_TO_DATA_FRAME

SEXP
getSelOut(void)
{
  int r;
  int c;
  int cols;
  int rows;
  VAR vn;

  SEXP list;
  SEXP attr;
  SEXP col;

#if defined(CONVERT_TO_DATA_FRAME)
  SEXP klass;
  SEXP row_names;
#endif

  list = R_NilValue;

  cols = s_iphreeqc.GetSelectedOutputColumnCount();
  rows = s_iphreeqc.GetSelectedOutputRowCount();
  if (cols == 0 || rows == 0) {
    return list;
  }


  PROTECT(list = allocVector(VECSXP, cols));
  PROTECT(attr = allocVector(STRSXP, cols));
  for (c = 0; c < cols; ++c) {

    VarInit(&vn);
    s_iphreeqc.GetSelectedOutputValue(0, c, &vn);

    PROTECT(col = getCol(c));

    SET_VECTOR_ELT(list, c, col);
    SET_STRING_ELT(attr, c, mkChar(vn.sVal));

    VarClear(&vn);
  }

  setAttrib(list, R_NamesSymbol, attr);


#if defined(CONVERT_TO_DATA_FRAME)
  /* Turn the data "list" into a "data.frame" */
  /* see model.c */

  PROTECT(klass = mkString("data.frame"));
  setAttrib(list, R_ClassSymbol, klass);
  UNPROTECT(1);

  PROTECT(row_names = allocVector(INTSXP, rows-1));
  for (r = 0; r < rows-1; ++r) INTEGER(row_names)[r] = r+1;
  setAttrib(list, R_RowNamesSymbol, row_names);
  UNPROTECT(1);
#endif

  UNPROTECT(2+cols);
  return list;
}

SEXP
getErrStr()
{
  SEXP ans = R_NilValue;
  PROTECT(ans = allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, mkChar(s_iphreeqc.GetErrorString()));
  UNPROTECT(1);
  return ans;
}

} // extern "C"
