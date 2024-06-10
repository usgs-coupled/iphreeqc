
#include <sstream>
#include <stdlib.h>
#include "Var.h"
#include "IPhreeqc.hpp"
#include <R.h>
#include <Rdefines.h>

#define CONVERT_TO_DATA_FRAME

class R {
public:
  static IPhreeqc& singleton() {
    static IPhreeqc instance;
    return instance;
  }
  static std::string& err_str() {
    static std::string instance;
    return instance;
  }
};


extern "C" {


SEXP
accumLine(SEXP line)
{
  const char* str_in;

  // check args
  if (!Rf_isString(line) || Rf_length(line) != 1 || STRING_ELT(line, 0) == NA_STRING) {
    Rf_error("AccumulateLine:line is not a single string\n");
  }

  if (STRING_ELT(line, 0) != NA_STRING) {
    str_in = CHAR(STRING_ELT(line, 0));
    if (R::singleton().AccumulateLine(str_in) != VR_OK) {
      Rf_error("%s", R::singleton().GetErrorString());
    }
  }

  return(R_NilValue);
}

SEXP
accumLineLst(SEXP line)
{
  const char* str_in;

  // check args
  if (!Rf_isString(line)) {
    Rf_error("a character vector argument expected");
  }

  int n = Rf_length(line);
  //std::ostringstream oss;

  for (int i = 0; i < n; ++i) {
    if (STRING_ELT(line, i) != NA_STRING) {
      str_in = CHAR(STRING_ELT(line, 0));
      if (R::singleton().AccumulateLine(str_in) != VR_OK) {
        Rf_error("%s", R::singleton().GetErrorString());
      }
    }
  }

  return(R_NilValue);
}

SEXP
clearAccum(void)
{
  R::singleton().ClearAccumulatedLines();
  return(R_NilValue);
}

SEXP
getAccumLines(void)
{
  SEXP ans = R_NilValue;
  const char* cstr = R::singleton().GetAccumulatedLines().c_str();
  if (cstr && cstr[0]) {
    std::string str(cstr);
    std::istringstream iss(str);
    std::string line;
    std::vector< std::string > lines;
    while (std::getline(iss, line))
    {
      lines.push_back(line);
    }
    Rf_protect(ans = Rf_allocVector(STRSXP, lines.size()));
    for (size_t i = 0; i < lines.size(); ++i)
    {
      SET_STRING_ELT(ans, i, Rf_mkChar(lines[i].c_str()));
    }
    UNPROTECT(1);
  }
  return ans;
}

SEXP
getCol(int ncol)
{
  VAR vv;
  char buffer[80];
  SEXP ans = R_NilValue;

  int cols = R::singleton().GetSelectedOutputColumnCount();
  int rows = R::singleton().GetSelectedOutputRowCount();
  if (cols == 0 || rows == 0) {
    return ans;
  }

  // count each type
  int nd, nl, ns;
  nd = nl = ns = 0;
  for (int r = 1; r < rows && ns == 0; ++r) {
    VarInit(&vv);
    // may want to implement (int) GetSelectedOutputType(r,c)
    R::singleton().GetSelectedOutputValue(r, ncol, &vv);
    switch (vv.type) {
    case TT_DOUBLE:  ++nd; break;
    case TT_LONG:    ++nl; break;
    case TT_STRING:  ++ns; break;
    default: break;
    }
    VarClear(&vv);
  }


  if (ns) {
    // all strings
    Rf_protect(ans = Rf_allocVector(STRSXP, rows-1));
    for (int r = 1; r < rows; ++r) {
      VarInit(&vv);
      R::singleton().GetSelectedOutputValue(r, ncol, &vv);
      switch (vv.type) {
      case TT_EMPTY:
        SET_STRING_ELT(ans, r-1, Rf_mkChar(""));
        break;
      case TT_ERROR:
        switch (vv.u.vresult) {
        case VR_OK:          SET_STRING_ELT(ans, r-1, Rf_mkChar("VR_OK"));          break;
        case VR_OUTOFMEMORY: SET_STRING_ELT(ans, r-1, Rf_mkChar("VR_OUTOFMEMORY")); break;
        case VR_BADVARTYPE:  SET_STRING_ELT(ans, r-1, Rf_mkChar("VR_BADVARTYPE"));  break;
        case VR_INVALIDARG:  SET_STRING_ELT(ans, r-1, Rf_mkChar("VR_INVALIDARG"));  break;
        case VR_INVALIDROW:  SET_STRING_ELT(ans, r-1, Rf_mkChar("VR_INVALIDROW"));  break;
        case VR_INVALIDCOL:  SET_STRING_ELT(ans, r-1, Rf_mkChar("VR_INVALIDCOL"));  break;
        }
        break;
      case TT_LONG:
        if (vv.u.lVal == -99) {
          snprintf(buffer, sizeof(buffer), "NA");
        } else {
          snprintf(buffer, sizeof(buffer), "%ld", vv.u.lVal);
        }
        SET_STRING_ELT(ans, r-1, Rf_mkChar(buffer));
        break;
      case TT_DOUBLE:
        if (vv.u.dVal == -999.999 || vv.u.dVal == -99.) {
          snprintf(buffer, sizeof(buffer), "NA");
        } else {
          snprintf(buffer, sizeof(buffer), "%g", vv.u.dVal);
        }
        SET_STRING_ELT(ans, r-1, Rf_mkChar(buffer));
        break;
      case TT_STRING:
        SET_STRING_ELT(ans, r-1, Rf_mkChar(vv.u.sVal));
        break;
      }
      VarClear(&vv);
    }
    UNPROTECT(1);
  } // if (ns)
  else if (nd) {
    // all reals
    Rf_protect(ans = Rf_allocVector(REALSXP, rows-1));
    for (int r = 1; r < rows; ++r) {
      VarInit(&vv);
      R::singleton().GetSelectedOutputValue(r, ncol, &vv);
      switch (vv.type) {
      case TT_EMPTY:
        REAL(ans)[r-1] = NA_REAL;
        break;
      case TT_ERROR:
        REAL(ans)[r-1] = NA_REAL;
        break;
      case TT_LONG:
        if (vv.u.lVal == -99) {
          REAL(ans)[r-1] = NA_REAL;
        } else {
          REAL(ans)[r-1] = (double)vv.u.lVal;
        }
        break;
      case TT_DOUBLE:
        if (vv.u.dVal == -999.999 || vv.u.dVal == -99. || vv.u.dVal == 1e-99) {
          REAL(ans)[r-1] = NA_REAL;
        } else {
          REAL(ans)[r-1] = (double)vv.u.dVal;
        }
        break;
      default:
        break;
      }
      VarClear(&vv);
    }
    UNPROTECT(1);
  } // if (nd)
  else if (nl) {
    // all ints
    Rf_protect(ans = Rf_allocVector(INTSXP, rows-1));
    for (int r = 1; r < rows; ++r) {
      VarInit(&vv);
      R::singleton().GetSelectedOutputValue(r, ncol, &vv);
      switch (vv.type) {
      case TT_EMPTY:
        INTEGER(ans)[r-1] = NA_INTEGER;
        break;
      case TT_ERROR:
        INTEGER(ans)[r-1] = NA_INTEGER;
        break;
      case TT_LONG:
        if (vv.u.lVal == -99) {
          INTEGER(ans)[r-1] = NA_INTEGER;
        } else {
          INTEGER(ans)[r-1] = vv.u.lVal;
        }
        break;
      default:
        break;
      }
      VarClear(&vv);
    }
    UNPROTECT(1);
  } // if (nl)
  else {
    // all NA
    Rf_protect(ans = Rf_allocVector(INTSXP, rows-1));
    for (int r = 1; r < rows; ++r) {
      INTEGER(ans)[r-1] = NA_INTEGER;
    } // for
    UNPROTECT(1);
  }
  return ans;
}

SEXP
getDumpFileName(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, Rf_mkChar(R::singleton().GetDumpFileName()));
  UNPROTECT(1);
  return ans;
}

SEXP
getDumpStrings(void)
{
  SEXP ans = R_NilValue;
  const char* cstr = R::singleton().GetDumpString();
  if (cstr && cstr[0]) {
    std::string str(cstr);
    std::istringstream iss(str);
    std::string line;
    std::vector< std::string > lines;
    while (std::getline(iss, line))
    {
      lines.push_back(line);
    }
    Rf_protect(ans = Rf_allocVector(STRSXP, lines.size()));
    for (size_t i = 0; i < lines.size(); ++i)
    {
      SET_STRING_ELT(ans, i, Rf_mkChar(lines[i].c_str()));
    }
    UNPROTECT(1);
  }
  return ans;
}

SEXP
getErrorFileName(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, Rf_mkChar(R::singleton().GetErrorFileName()));
  UNPROTECT(1);
  return ans;
}

SEXP
getDumpFileOn(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(LGLSXP, 1));
  if (R::singleton().GetDumpFileOn()) {
    LOGICAL(ans)[0] = TRUE;
  }
  else {
    LOGICAL(ans)[0] = FALSE;
  }
  UNPROTECT(1);
  return(ans);
}

SEXP
getDumpStringOn(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(LGLSXP, 1));
  if (R::singleton().GetDumpStringOn()) {
    LOGICAL(ans)[0] = TRUE;
  }
  else {
    LOGICAL(ans)[0] = FALSE;
  }
  UNPROTECT(1);
  return(ans);
}

SEXP
getErrorFileOn(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(LGLSXP, 1));
  if (R::singleton().GetErrorFileOn()) {
    LOGICAL(ans)[0] = TRUE;
  }
  else {
    LOGICAL(ans)[0] = FALSE;
  }
  UNPROTECT(1);
  return(ans);
}

SEXP
getErrorStringOn(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(LGLSXP, 1));
  if (R::singleton().GetErrorStringOn()) {
    LOGICAL(ans)[0] = TRUE;
  }
  else {
    LOGICAL(ans)[0] = FALSE;
  }
  UNPROTECT(1);
  return(ans);
}

SEXP
getLogFileOn(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(LGLSXP, 1));
  if (R::singleton().GetLogFileOn()) {
    LOGICAL(ans)[0] = TRUE;
  }
  else {
    LOGICAL(ans)[0] = FALSE;
  }
  UNPROTECT(1);
  return(ans);
}

SEXP
getLogStringOn(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(LGLSXP, 1));
  if (R::singleton().GetLogStringOn()) {
    LOGICAL(ans)[0] = TRUE;
  }
  else {
    LOGICAL(ans)[0] = FALSE;
  }
  UNPROTECT(1);
  return(ans);
}

SEXP
getOutputStringOn(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(LGLSXP, 1));
  if (R::singleton().GetOutputStringOn()) {
    LOGICAL(ans)[0] = TRUE;
  }
  else {
    LOGICAL(ans)[0] = FALSE;
  }
  UNPROTECT(1);
  return(ans);
}

SEXP
getErrorStrings(void)
{
  SEXP ans = R_NilValue;
  const char* cstr = R::singleton().GetErrorString();
  if (cstr && cstr[0]) {
    std::string str(cstr);
    std::istringstream iss(str);
    std::string line;
    std::vector< std::string > lines;
    while (std::getline(iss, line))
    {
      lines.push_back(line);
    }
    Rf_protect(ans = Rf_allocVector(STRSXP, lines.size()));
    for (size_t i = 0; i < lines.size(); ++i)
    {
      SET_STRING_ELT(ans, i, Rf_mkChar(lines[i].c_str()));
    }
    UNPROTECT(1);
  }
  return ans;
}

SEXP
getLogFileName(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, Rf_mkChar(R::singleton().GetLogFileName()));
  UNPROTECT(1);
  return ans;
}

SEXP
getLogStrings(void)
{
  SEXP ans = R_NilValue;
  const char* cstr = R::singleton().GetLogString();
  if (cstr && cstr[0]) {
    std::string str(cstr);
    std::istringstream iss(str);
    std::string line;
    std::vector< std::string > lines;
    while (std::getline(iss, line))
    {
      lines.push_back(line);
    }
    Rf_protect(ans = Rf_allocVector(STRSXP, lines.size()));
    for (size_t i = 0; i < lines.size(); ++i)
    {
      SET_STRING_ELT(ans, i, Rf_mkChar(lines[i].c_str()));
    }
    UNPROTECT(1);
  }
  return ans;
}

SEXP
getOutputFileName(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, Rf_mkChar(R::singleton().GetOutputFileName()));
  UNPROTECT(1);
  return ans;
}

SEXP
getOutputFileOn(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(LGLSXP, 1));
  if (R::singleton().GetOutputFileOn()) {
    LOGICAL(ans)[0] = 1;
  }
  else {
    LOGICAL(ans)[0] = 0;
  }
  UNPROTECT(1);
  return ans;
}

SEXP
getOutputStrings(void)
{
  SEXP ans = R_NilValue;
  const char* cstr = R::singleton().GetOutputString();
  if (cstr && cstr[0]) {
    std::string str(cstr);
    std::istringstream iss(str);
    std::string line;
    std::vector< std::string > lines;
    while (std::getline(iss, line))
    {
      lines.push_back(line);
    }
    Rf_protect(ans = Rf_allocVector(STRSXP, lines.size()));
    for (size_t i = 0; i < lines.size(); ++i)
    {
      SET_STRING_ELT(ans, i, Rf_mkChar(lines[i].c_str()));
    }
    UNPROTECT(1);
  }
  return ans;
}

SEXP
getSelectedOutputFileName(SEXP nuser)
{
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isInteger(nuser) || Rf_length(nuser) != 1) {
    Rf_error("GetSelectedOutputFileName:nuser must be a single integer\n");
  }
  int save = R::singleton().GetCurrentSelectedOutputUserNumber();
  R::singleton().SetCurrentSelectedOutputUserNumber(INTEGER(nuser)[0]);
  Rf_protect(ans = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, Rf_mkChar(R::singleton().GetSelectedOutputFileName()));
  UNPROTECT(1);
  R::singleton().SetCurrentSelectedOutputUserNumber(save);
  return ans;
}

SEXP
getSelectedOutputStrings(void)
{
  SEXP ans = R_NilValue;
  const char* cstr = R::singleton().GetSelectedOutputString();
  if (cstr && cstr[0]) {
    std::string str(cstr);
    std::istringstream iss(str);
    std::string line;
    std::vector< std::string > lines;
    while (std::getline(iss, line))
    {
      lines.push_back(line);
    }
    Rf_protect(ans = Rf_allocVector(STRSXP, lines.size()));
    for (size_t i = 0; i < lines.size(); ++i)
    {
      SET_STRING_ELT(ans, i, Rf_mkChar(lines[i].c_str()));
    }
    UNPROTECT(1);
  }
  return ans;
}

SEXP
getSelectedOutputStringsLst(void)
{
  SEXP list;
  SEXP attr;

  list = R_NilValue;

  if (int n = R::singleton().GetSelectedOutputCount()) {
    SEXP so;
    char buffer[80];

    Rf_protect(list = Rf_allocVector(VECSXP, n));
    Rf_protect(attr = Rf_allocVector(STRSXP, n));

    int save = R::singleton().GetCurrentSelectedOutputUserNumber();
    for (int i = 0; i < n; ++i) {
      int d = R::singleton().GetNthSelectedOutputUserNumber(i);
      ::snprintf(buffer, sizeof(buffer), "n%d", d);
      SET_STRING_ELT(attr, i, Rf_mkChar(buffer));
      R::singleton().SetCurrentSelectedOutputUserNumber(d);
      Rf_protect(so = getSelectedOutputStrings());
      SET_VECTOR_ELT(list, i, so);
      UNPROTECT(1);
    }
    R::singleton().SetCurrentSelectedOutputUserNumber(save);
    Rf_setAttrib(list, R_NamesSymbol, attr);

    UNPROTECT(2);
  }
  return list;
}

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

  SEXP klass;
  SEXP row_names;

  list = R_NilValue;

  cols = R::singleton().GetSelectedOutputColumnCount();
  rows = R::singleton().GetSelectedOutputRowCount();
  if (cols == 0 || rows == 0) {
    return list;
  }

  Rf_protect(list = Rf_allocVector(VECSXP, cols));
  Rf_protect(attr = Rf_allocVector(STRSXP, cols));
  for (c = 0; c < cols; ++c) {

    VarInit(&vn);
    R::singleton().GetSelectedOutputValue(0, c, &vn);

    Rf_protect(col = getCol(c));

    SET_VECTOR_ELT(list, c, col);
    SET_STRING_ELT(attr, c, Rf_mkChar(vn.u.sVal));

    UNPROTECT(1);
    VarClear(&vn);
  }

  Rf_setAttrib(list, R_NamesSymbol, attr);

  // Turn the data "list" into a "data.frame"
  // see model.c

  Rf_protect(klass = Rf_mkString("data.frame"));
  Rf_setAttrib(list, R_ClassSymbol, klass);
  UNPROTECT(1);

  Rf_protect(row_names = Rf_allocVector(INTSXP, rows-1));
  for (r = 0; r < rows-1; ++r) INTEGER(row_names)[r] = r+1;
  Rf_setAttrib(list, R_RowNamesSymbol, row_names);
  UNPROTECT(1);

  UNPROTECT(2);
  return list;
}

SEXP
getSelOutLst(void)
{
  SEXP list;
  SEXP attr;

  list = R_NilValue;

  if (int n = R::singleton().GetSelectedOutputCount()) {
    SEXP so;
    char buffer[80];

    Rf_protect(list = Rf_allocVector(VECSXP, n));
    Rf_protect(attr = Rf_allocVector(STRSXP, n));

    int save = R::singleton().GetCurrentSelectedOutputUserNumber();
    for (int i = 0; i < n; ++i) {
      int d = R::singleton().GetNthSelectedOutputUserNumber(i);
      ::snprintf(buffer, sizeof(buffer), "n%d", d);
      SET_STRING_ELT(attr, i, Rf_mkChar(buffer));
      R::singleton().SetCurrentSelectedOutputUserNumber(d);
      Rf_protect(so = getSelOut());
      SET_VECTOR_ELT(list, i, so);
      UNPROTECT(1);
    }
    R::singleton().SetCurrentSelectedOutputUserNumber(save);
    Rf_setAttrib(list, R_NamesSymbol, attr);

    UNPROTECT(2);
  }
  return list;
}

SEXP
getVersionString(void)
{
  SEXP ans = R_NilValue;
  Rf_protect(ans = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(ans, 0, Rf_mkChar(R::singleton().GetVersionString()));
  UNPROTECT(1);
  return ans;
}

SEXP
getWarningStrings(void)
{
  SEXP ans = R_NilValue;
  const char* cstr = R::singleton().GetWarningString();
  if (cstr && cstr[0]) {
    std::string str(cstr);
    std::istringstream iss(str);
    std::string line;
    std::vector< std::string > lines;
    while (std::getline(iss, line))
    {
      lines.push_back(line);
    }
    Rf_protect(ans = Rf_allocVector(STRSXP, lines.size()));
    for (size_t i = 0; i < lines.size(); ++i)
    {
      SET_STRING_ELT(ans, i, Rf_mkChar(lines[i].c_str()));
    }
    UNPROTECT(1);
  }
  return ans;
}

SEXP
listComps(void)
{
  SEXP ans = R_NilValue;

  std::list< std::string > lc = R::singleton().ListComponents();
  if (lc.size() > 0) {
    Rf_protect(ans = Rf_allocVector(STRSXP, lc.size()));
    std::list< std::string >::iterator li = lc.begin();
    for (int i = 0; li != lc.end(); ++i, ++li) {
      SET_STRING_ELT(ans, i, Rf_mkChar((*li).c_str()));
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
  if (!Rf_isString(filename) || Rf_length(filename) != 1) {
    Rf_error("'filename' is not a single string");
  }

  name = CHAR(STRING_ELT(filename, 0));

  if (R::singleton().LoadDatabase(name) != VR_OK) {
    Rf_error("%s", R::singleton().GetErrorString());
  }

  return(R_NilValue);
}

SEXP
loadDBLst(SEXP input)
{
  // check args
  if (!Rf_isString(input)) {
    Rf_error("a character vector argument expected");
  }

  int n = Rf_length(input);
  std::ostringstream *poss = new std::ostringstream();

  for (int i = 0; i < n; ++i) {
    if (STRING_ELT(input, i) != NA_STRING) {
      (*poss) << CHAR(STRING_ELT(input, i)) << "\n";
    }
  }

  if (R::singleton().LoadDatabaseString((*poss).str().c_str()) != VR_OK) {
    // all dtors must be called before error
    delete poss;
    Rf_error("%s", R::singleton().GetErrorString());
  }

  delete poss;
  return(R_NilValue);
}

SEXP
loadDBStr(SEXP input)
{
  const char* string;

  // check args
  if (!Rf_isString(input) || Rf_length(input) != 1) {
    Rf_error("'input' is not a single string");
  }

  string = CHAR(STRING_ELT(input, 0));

  if (R::singleton().LoadDatabaseString(string) != VR_OK) {
    Rf_error("%s", R::singleton().GetErrorString());
  }

  return(R_NilValue);
}

SEXP
runAccum(void)
{
  if (R::singleton().RunAccumulated() != VR_OK) {
    Rf_error("%s", R::singleton().GetErrorString());
  }
  return(R_NilValue);
}

SEXP
runFile(SEXP filename)
{
  const char* name;

  // check args
  if (!Rf_isString(filename) || Rf_length(filename) != 1 || STRING_ELT(filename, 0) == NA_STRING) {
    Rf_error("'filename' must be a single character string");
  }

  name = CHAR(STRING_ELT(filename, 0));
  if (R::singleton().RunFile(name) != VR_OK) {
    Rf_error("%s", R::singleton().GetErrorString());
  }

  return(R_NilValue);
}

SEXP
runString(SEXP input)
{
  const char* in;

  // check args
  if (!Rf_isString(input)) {
    Rf_error("a character vector argument expected");
  }

  in = CHAR(STRING_ELT(input, 0));
  if (R::singleton().RunString(in) != VR_OK) {
    Rf_error("%s", R::singleton().GetErrorString());
  }

  return(R_NilValue);
}

SEXP
runStringLst(SEXP input)
{
  // check args
  if (!Rf_isString(input)) {
    Rf_error("a character vector argument expected");
  }

  int n = Rf_length(input);
  std::ostringstream *poss = new std::ostringstream();

  for (int i = 0; i < n; ++i) {
    if (STRING_ELT(input, i) != NA_STRING) {
      (*poss) << CHAR(STRING_ELT(input, i)) << "\n";
    }
  }

  if (R::singleton().RunString((*poss).str().c_str()) != VR_OK) {
    delete poss;
    Rf_error("%s", R::singleton().GetErrorString());
  }

  delete poss;
  return(R_NilValue);
}

SEXP
setDumpFileName(SEXP filename)
{
  const char* name;
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isString(filename) || Rf_length(filename) != 1) {
    Rf_error("SetDumpFileName:filename is not a single string\n");
  }

  name = CHAR(STRING_ELT(filename, 0));
  R::singleton().SetDumpFileName(name);
  return(ans);
}

SEXP
setDumpFileOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isLogical(value) || Rf_length(value) != 1 || LOGICAL(value)[0] == NA_LOGICAL) {
    R::singleton().AddError("SetDumpFileOn: value must either be \"TRUE\" or \"FALSE\"");
    Rf_error("value must either be \"TRUE\" or \"FALSE\"\n");
  }
  R::singleton().SetDumpFileOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setDumpStringOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isLogical(value) || Rf_length(value) != 1) {
    Rf_error("SetDumpStringOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  R::singleton().SetDumpStringOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setErrorFileName(SEXP filename)
{
  const char* name;
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isString(filename) || Rf_length(filename) != 1) {
    Rf_error("SetErrorFileName:filename is not a single string\n");
  }

  name = CHAR(STRING_ELT(filename, 0));
  R::singleton().SetErrorFileName(name);
  return(ans);
}

SEXP
setErrorFileOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isLogical(value) || Rf_length(value) != 1 || LOGICAL(value)[0] == NA_LOGICAL) {
    R::singleton().AddError("SetErrorFileOn: value must either be \"TRUE\" or \"FALSE\"");
    Rf_error("value must either be \"TRUE\" or \"FALSE\"\n");
  }
  R::singleton().SetErrorFileOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setErrorStringOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isLogical(value) || Rf_length(value) != 1) {
    Rf_error("SetErrorStringOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  R::singleton().SetErrorStringOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setLogFileName(SEXP filename)
{
  const char* name;
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isString(filename) || Rf_length(filename) != 1) {
    Rf_error("SetLogFileName:filename is not a single string\n");
  }

  name = CHAR(STRING_ELT(filename, 0));
  R::singleton().SetLogFileName(name);
  return(ans);
}

SEXP
setLogFileOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isLogical(value) || Rf_length(value) != 1) {
    R::singleton().AddError("SetLogFileOn: value must either be \"TRUE\" or \"FALSE\"");
    Rf_error("value must either be \"TRUE\" or \"FALSE\"");
  }
  R::singleton().SetLogFileOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setLogStringOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isLogical(value) || Rf_length(value) != 1) {
    Rf_error("SetLogStringOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  R::singleton().SetLogStringOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setOutputFileName(SEXP filename)
{
  const char* name;
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isString(filename) || Rf_length(filename) != 1) {
    Rf_error("SetOutputFileName:filename is not a single string\n");
  }

  name = CHAR(STRING_ELT(filename, 0));
  R::singleton().SetOutputFileName(name);
  return(ans);
}

SEXP
setOutputFileOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isLogical(value) || Rf_length(value) != 1) {
    Rf_error("value must either be \"TRUE\" or \"FALSE\"\n");
  }
  R::singleton().SetOutputFileOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setOutputStringOn(SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isLogical(value) || Rf_length(value) != 1) {
    Rf_error("SetOutputStringOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  R::singleton().SetOutputStringOn(LOGICAL(value)[0]);
  return(ans);
}

SEXP
setSelectedOutputFileName(SEXP nuser, SEXP filename)
{
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isInteger(nuser) || Rf_length(nuser) != 1) {
    Rf_error("SetSelectedOutputFileName:nuser must be a single integer\n");
  }
  if (!Rf_isString(filename) || Rf_length(filename) != 1) {
    Rf_error("SetSelectedOutputFileName:filename is not a single string\n");
  }
  int save = R::singleton().GetCurrentSelectedOutputUserNumber();
  const char* name = CHAR(STRING_ELT(filename, 0));
  R::singleton().SetCurrentSelectedOutputUserNumber(INTEGER(nuser)[0]);
  R::singleton().SetSelectedOutputFileName(name);
  R::singleton().SetCurrentSelectedOutputUserNumber(save);
  return(ans);
}

SEXP
setSelectedOutputFileOn(SEXP nuser, SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isInteger(nuser) || Rf_length(nuser) != 1) {
    Rf_error("nuser must be a single integer\n");
  }
  if (!Rf_isLogical(value) || Rf_length(value) != 1) {
    Rf_error("value must either be \"TRUE\" or \"FALSE\"\n");
  }
  int save = R::singleton().GetCurrentSelectedOutputUserNumber();
  R::singleton().SetCurrentSelectedOutputUserNumber(INTEGER(nuser)[0]);
  R::singleton().SetSelectedOutputFileOn(LOGICAL(value)[0]);
  R::singleton().SetCurrentSelectedOutputUserNumber(save);
  return(ans);
}

SEXP
setSelectedOutputStringOn(SEXP nuser, SEXP value)
{
  SEXP ans = R_NilValue;
  // check args
  if (!Rf_isInteger(nuser) || Rf_length(nuser) != 1) {
    Rf_error("SetSelectedOutputStringOn:nuser must be a single integer\n");
  }
  if (!Rf_isLogical(value) || Rf_length(value) != 1) {
    Rf_error("SetSelectedOutputStringOn:value must either be \"TRUE\" or \"FALSE\"\n");
  }
  int save = R::singleton().GetCurrentSelectedOutputUserNumber();
  R::singleton().SetCurrentSelectedOutputUserNumber(INTEGER(nuser)[0]);
  R::singleton().SetSelectedOutputStringOn(LOGICAL(value)[0]);
  R::singleton().SetCurrentSelectedOutputUserNumber(save);
  return(ans);
}


#include <R_ext/Rdynload.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

const static R_CallMethodDef R_CallDef[] = {
  CALLDEF(accumLineLst, 1),
  CALLDEF(clearAccum, 0),
  CALLDEF(getAccumLines, 0),
  CALLDEF(listComps, 0),
  CALLDEF(getDumpFileName, 0),
  CALLDEF(getDumpStrings, 0),
  CALLDEF(getErrorFileName, 0),
  CALLDEF(getDumpFileOn, 0),
  CALLDEF(getDumpStringOn, 0),
  CALLDEF(getErrorFileOn, 0),
  CALLDEF(getErrorStringOn, 0),
  CALLDEF(getLogFileOn, 0),
  CALLDEF(getLogStringOn, 0),
  CALLDEF(getOutputFileOn, 0),
  CALLDEF(getOutputStringOn, 0),
  CALLDEF(getErrorStrings, 0),
  CALLDEF(getLogFileName, 0),
  CALLDEF(getLogStrings, 0),
  CALLDEF(getOutputFileName, 0),
  CALLDEF(getOutputStrings, 0),
  CALLDEF(getSelOutLst, 0),
  CALLDEF(getWarningStrings, 0),
  CALLDEF(loadDB, 1),
  CALLDEF(loadDBLst, 1),
  CALLDEF(runAccum, 0),
  CALLDEF(runFile, 1),
  CALLDEF(runStringLst, 1),
  CALLDEF(setDumpFileName, 1),
  CALLDEF(setDumpFileOn, 1),
  CALLDEF(setDumpStringOn, 1),
  CALLDEF(setErrorFileName, 1),
  CALLDEF(setErrorFileOn, 1),
  CALLDEF(setErrorStringOn, 1),
  CALLDEF(setLogFileName, 1),
  CALLDEF(setLogFileOn, 1),
  CALLDEF(setLogStringOn, 1),
  CALLDEF(setOutputFileName, 1),
  CALLDEF(setOutputFileOn, 1),
  CALLDEF(setOutputStringOn, 1),
  CALLDEF(getSelectedOutputFileName, 1),
  CALLDEF(setSelectedOutputFileName, 2),
  CALLDEF(setSelectedOutputFileOn, 2),
  {NULL, NULL, 0}
};

void R_init_phreeqc(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

} // extern "C"
