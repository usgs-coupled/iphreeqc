# Package Functions

phrAccumulateLine =
function(line)
{
  invisible(.Call("accumLineLst", as.character(line), PACKAGE=.packageName))
}

phrClearAccumulatedLines =
function()
{
  invisible(.Call("clearAccum", PACKAGE=.packageName))
}

phrGetSelectedOutput =
function(allow_ = TRUE)
{
  sel_outs <- .Call("getSelOutLst", PACKAGE=.packageName)
  if (!is.null(sel_outs)) {
    for (t in names(sel_outs)) {
      if (!is.null(sel_outs[[t]])) {
        names(sel_outs[[t]]) <- make.names(names(sel_outs[[t]]), unique = TRUE, allow_ = allow_)
      }
    }
  }
  return(sel_outs)
}

phrGetErrorString =
function()
{
  return(.Call("getErrStr", PACKAGE=.packageName))
}

phrGetErrorFileOn =
function()
{
  return(.Call("getErrorFileOn", PACKAGE=.packageName))
}

phrSetDumpFileOn =
function(value)
{
  invisible(.Call("setDumpFileOn", as.logical(value), PACKAGE=.packageName))
}

phrSetErrorFileOn =
function(value)
{
  invisible(.Call("setErrorFileOn", as.logical(value), PACKAGE=.packageName))
}

phrSetLogFileOn =
function(value)
{
  invisible(.Call("setLogFileOn", as.logical(value), PACKAGE=.packageName))
}

phrSetOutputFileOn =
function(value)
{
  invisible(.Call("setOutputFileOn", as.logical(value), PACKAGE=.packageName))
}

phrSetSelectedOutputFileOn =
function(value)
{
  invisible(.Call("setSelectedOutputFileOn", as.logical(value), PACKAGE=.packageName))
}

phrSetDumpStringOn =
function(value)
{
  invisible(.Call("setDumpStringOn", as.logical(value), PACKAGE=.packageName))
}

phrSetErrorStringOn =
function(value)
{
  invisible(.Call("setErrorStringOn", as.logical(value), PACKAGE=.packageName))
}

phrSetLogStringOn =
function(value)
{
  invisible(.Call("setLogStringOn", as.logical(value), PACKAGE=.packageName))
}

phrSetOutputStringOn =
function(value)
{
  invisible(.Call("setOutputStringOn", as.logical(value), PACKAGE=.packageName))
}

phrSetSelectedOutputStringOn =
function(value)
{
  invisible(.Call("setSelectedOutputStringOn", as.logical(value), PACKAGE=.packageName))
}


phrSetDumpFileName =
function(filename)
{
  invisible(.Call("setDumpFileName", as.character(filename), PACKAGE=.packageName))
}

phrSetErrorFileName =
function(filename)
{
  invisible(.Call("setErrorFileName", as.character(filename), PACKAGE=.packageName))
}

phrSetLogFileName =
function(filename)
{
  invisible(.Call("setLogFileName", as.character(filename), PACKAGE=.packageName))
}

phrSetOutputFileName =
function(filename)
{
  invisible(.Call("setOutputFileName", as.character(filename), PACKAGE=.packageName))
}

phrSetSelectedOutputFileName =
function(filename)
{
  invisible(.Call("setSelectedOutputFileName", as.character(filename), PACKAGE=.packageName))
}


phrGetDumpFileName =
function()
{
  return(.Call("getDumpFileName", PACKAGE=.packageName))
}

phrGetErrorFileName =
function()
{
  return(.Call("getErrorFileName", PACKAGE=.packageName))
}

phrGetLogFileName =
function()
{
  return(.Call("getLogFileName", PACKAGE=.packageName))
}

phrGetOutputFileName =
function()
{
  return(.Call("getOutputFileName", PACKAGE=.packageName))
}

phrGetSelectedOutputFileName =
function()
{
  return(.Call("getSelectedOutputFileName", PACKAGE=.packageName))
}



phrGetDumpString =
function()
{
  return(.Call("getDumpString", PACKAGE=.packageName))
}

phrGetLogString =
function()
{
  return(.Call("getLogString", PACKAGE=.packageName))
}

phrGetOutputString =
function()
{
  return(.Call("getOutputString", PACKAGE=.packageName))
}

phrGetSelectedOutputString =
function()
{
  return(.Call("getSelectedOutputString", PACKAGE=.packageName))
}

phrGetWarningString =
function()
{
  return(.Call("getWarningString", PACKAGE=.packageName))
}


phrGetComponentList =
function()
{
  return(.Call("listComps", PACKAGE=.packageName))
}

phrLoadDatabase =
function(filename)
{
  invisible(.Call("loadDB", as.character(filename), PACKAGE=.packageName))
}

phrLoadDatabaseString =
function(input)
{
  invisible(.Call("loadDBStr", as.character(input), PACKAGE=.packageName))
}

phrLoadDatabaseList =
function(input)
{
  invisible(.Call("loadDBLst", as.character(input), PACKAGE=.packageName))
}

phrGetAccumulatedLines =
function()
{
  return(.Call("getAccumLines", PACKAGE=.packageName))
}

phrGetAccumulatedLines =
function()
{
  return(.Call("getAccumLines", PACKAGE=.packageName))
}

##phrPHREEQC_DAT =
##function()
##{
##  return(.Call("phreeqcDat", PACKAGE=.packageName))
##}

phrRunAccumulated =
function()
{
  invisible(.Call("runAccum", PACKAGE=.packageName))
}

phrRunFile =
function(filename)
{
  invisible(.Call("runFile", as.character(filename), PACKAGE=.packageName))
}

phrRunString =
function(input)
{
  invisible(.Call("runStringLst", as.character(input), PACKAGE=.packageName))
}
