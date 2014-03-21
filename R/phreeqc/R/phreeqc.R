##' R interface to the phreeqc geochemical modeling program.
##' 
##' Provides an interface to PHREEQC (Version 3)--A Computer Program for
##' Speciation, Batch-Reaction, One-Dimensional Transport, and Inverse
##' Geochemical Calculations.
##' 
##' \tabular{ll}{ Package: \tab phreeqc\cr Type: \tab Package\cr Version: \tab
##' 1.0\cr Date: \tab 2014-02-05\cr License: \tab Unlimited\cr } ~~ An overview
##' of how to use the package, including the most important functions ~~
##' 
##' @name phreeqc-package
##' @aliases phreeqc-package phreeqc
##' @docType package
##' @author David L. Parkhurst \email{dlpark@@usgs.gov}\cr C.A.J. Appelo
##' \email{appt@@hydrochemistry.eu}\cr Maintainer: Scott R. Charlton
##' \email{charlton@@usgs.gov}
##' @references \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @keywords package
##' @examples
##'
##' #########################################################################
##' # Run ex2 and plot results
##' #########################################################################
##' 
##' # load the phreeqc.dat database
##' phrLoadDatabaseString(phreeqc.dat.list)
##' 
##' # run example 2
##' phrRunString(ex2.list)
##' 
##' # retrieve selected_output as a list of data.frame
##' so <- phrGetSelectedOutput()
##' 
##' # plot the results
##' attach(so$n1)
##' title  <- "Gypsum-Anhydrite Stability"
##' xlabel <- "Temperature, in degrees celcius"
##' ylabel <- "Saturation index"
##' plot(temp.C., si_gypsum, main=title, xlab=xlabel, ylab=ylabel, col="darkred", xlim=c(25, 75), ylim=c(-0.4, 0.0))
##' points(temp.C., si_anhydrite, col="darkgreen")
##' legend("bottomright", c("Gypsum", "Anhydrite"), col = c("darkred", "darkgreen"), pch = c(1, 1))
##'
##' 
##' #########################################################################
##' # Load data from CSV and calculate CO2
##' #########################################################################
##'
##' 
##' # append solution input to given vector
##' fSoln <- function(vec, id, t, pH, alk) {
##'     if (is.na(alk)) alk = 0.0
##'     return(
##'         c(
##'             vec,
##'             paste("SOLUTION    ", id            ),
##'             paste("  temp      ", t             ),
##'             paste("  pH        ", pH            ),
##'             paste("  Alkalinity", alk, "ueq/kgw")
##'             )
##'         )
##' }
##' 
##' # append selected_output definition to given vector
##' fSelOut <- function(vec) {
##'     return (
##'         c(
##'             vec,
##'             "SELECTED_OUTPUT              ",
##'             "  -reset false               ",
##'             "  -solution true             ",
##'             "  -saturation_indices CO2(g) ",
##'             "USER_PUNCH                   ",
##'             "  headings CO2_umol_l        ",
##'             "  10 PUNCH MOL('CO2')*1e6    "
##'             )
##'         )
##' }
##'
##' # Note: CO2.df was created using the following:
##' # CO2.df <- read.csv("SE_to_export_03142014.csv")
##'
##' # pull out pertinent data out of df and into solns
##' solns <- data.frame("soln" = CO2.df[,1], "Temp" = CO2.df[,2], "pH" = CO2.df[,3], "alkalinity" = CO2.df[,21], "syringe_CO2" = CO2.df[,7])
##' 
##' # create input (as a character vector)
##' input <- vector()
##' for (i in 1:length(CO2.df[,1])) {
##'     input <- fSoln(input, solns[i, 1], solns[i, 2], solns[i, 3], solns[i, 4])
##' }
##' 
##' # add selected_output definition
##' input <- fSelOut(input)
##' 
##' # load database
##' phrLoadDatabaseString(phreeqc.dat.list)
##' 
##' # run
##' phrRunString(input)
##' 
##' # get results
##' selout <- phrGetSelectedOutput()
##' 
##' # merge the dataframes
##' results <- merge(solns, selout$n1)
##' 
NULL

# Package Functions

##' Accumlulate line(s) for input to phreeqc.
##' 
##' Appends a line of text to the input buffer in order to be run using
##' \code{\link{phrRunAccumulated}}.
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrAccumulateLine(line)
##' @param line The line(s) to add for input to phreeqc.
##' @return This function returns NULL.
##' @note %% ~~further notes~~
##' @seealso \code{\link{phrClearAccumulatedLines}},
##' \code{\link{phrGetAccumulatedLines}}, \code{\link{phrRunAccumulated}}
##' @references \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' # this example loads the phreeqc.dat database, accumulates input, and
##' # runs it
##' phrLoadDatabaseString(phreeqc.dat.list)
##' phrAccumulateLine("TITLE Example 2.--Temperature dependence of solubility")
##' phrAccumulateLine("                  of gypsum and anhydrite")
##' phrAccumulateLine("SOLUTION 1 Pure water")
##' phrAccumulateLine("        pH      7.0")
##' phrAccumulateLine("        temp    25.0")
##' phrAccumulateLine("EQUILIBRIUM_PHASES 1")
##' phrAccumulateLine("        Gypsum          0.0     1.0")
##' phrAccumulateLine("        Anhydrite       0.0     1.0")
##' phrAccumulateLine("REACTION_TEMPERATURE 1")
##' phrAccumulateLine("        25.0 75.0 in 51 steps")
##' phrAccumulateLine("SELECTED_OUTPUT")
##' phrAccumulateLine("        -file   ex2.sel")
##' phrAccumulateLine("        -temperature")
##' phrAccumulateLine("        -si     anhydrite  gypsum")
##' phrAccumulateLine("END")
##' phrSetOutputFileOn(TRUE)
##' if (is.null(phrRunAccumulated())) {
##'   cat(paste("see ", phrGetOutputFileName(), ".\n", sep=""))
##' }
##' 
phrAccumulateLine =
function(line)
{
  invisible(.Call("accumLineLst", as.character(line), PACKAGE=.packageName))
}



##' Clears the accumulated input buffer.
##' 
##' Clears the accumulated input buffer. The input buffer is accumulated from
##' calls to the \code{\link{phrAccumulateLine}} method.
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrClearAccumulatedLines()
##' @return This function returns NULL.
##' @note %% ~~further notes~~
##' @seealso \code{\link{phrAccumulateLine}},
##' \code{\link{phrGetAccumulatedLines}}, \code{\link{phrRunAccumulated}}
##' @references \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' # This example loads some keyword input, clears the input, and displays the results.
##' phrAccumulateLine("SOLUTION 1")
##' phrAccumulateLine("END")
##' cat(paste("The accumulated input is:\n", phrGetAccumulatedLines(), sep=""))
##' phrClearAccumulatedLines()
##' cat(paste("The accumulated input now is:\n", phrGetAccumulatedLines(), sep=""))
##' 
phrClearAccumulatedLines =
function()
{
  invisible(.Call("clearAccum", PACKAGE=.packageName))
}


##' Retrieves a list containing the current list of components.
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetComponentList()
##' @return A list containing the names of the components defined in the
##' current system.
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' # This example runs the ex2 input file and echos the list of components.
##' phrLoadDatabaseString(phreeqc.dat.list)
##' phrRunString(ex2.list)
##' cat("components:\n")
##' for (c in phrGetComponentList()) {
##'   cat(c, "\n")
##' }
##' 
phrGetComponentList =
function()
{
  return(.Call("listComps", PACKAGE=.packageName))
}


##' Returns the contents of the selected output as a list of data frames.
##' 
##' phrGetSelectedOutput returns a named list containing the resultant
##' selected output blocks.  The names of each data frame are creating by
##' concatenating the letter 'n' and the user number of the selected output
##' block.
##'
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetSelectedOutput()
##' @return Returns a named list of data frames containing the selected_output
##' from the previous run.
##' @seealso \code{\link{phrGetSelectedOutputFileOn}} 
##' @references \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' 
##'  # Load database and run ex2
##'  phrLoadDatabaseString(phreeqc.dat.list)
##'  phrRunString(ex2.list)
##'
##'  # display a summary of the results
##'  df <- phrGetSelectedOutput()
##'  summary(df$n1)
##' 
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



##' Return error string messages.
##' 
##' Returns a string containing any error messages that were generated
##' during the last invocation of the following methods:
##' \code{\link{phrAccumulateLine}}, \code{\link{phrLoadDatabase}},
##' \code{\link{phrLoadDatabaseString}}, \code{\link{phrRunAccumulatedeadString}},
##' \code{\link{phrRunFile}}, \code{\link{phrRunString}}
##' 
##' This routine is rarely needed when running interactively since the error
##' string is displayed when it occurs.
##' 
##' @usage phrGetErrorString()
##' @return This function returns the errors that occured during the previous
##' phrReadDB, phrRun, phrRunFile call.
##' @seealso \code{\link{phrReadString}}, \code{\link{phrRun}},
##' \code{\link{phrRunFile}}, \code{\link{phrGetSelectedOutput}}
##' @references Literature references and web URLs can go here.
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' # loaddatabase should fail
##' n <- try(phrLoadDatabase("missing.dat"), silent = TRUE)
##' # if n is non-NULL display error string
##' if (!is.null(n)) phrGetErrorString()
##' 
phrGetErrorString =
function()
{
  return(.Call("getErrStr", PACKAGE=.packageName))
}



##' Retrieves the current value of the error file switch.
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetErrorFileOn()
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function () 
##' {
##'   }
##' 
phrGetErrorFileOn =
function()
{
  return(.Call("getErrorFileOn", PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetDumpFileOn(value)
##' @param value %% ~~Describe \code{value} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (value) 
##' {
##'   }
##' 
phrSetDumpFileOn =
function(value)
{
  invisible(.Call("setDumpFileOn", as.logical(value), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetErrorFileOn(value)
##' @param value %% ~~Describe \code{value} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (value) 
##' {
##'   }
##' 
phrSetErrorFileOn =
function(value)
{
  invisible(.Call("setErrorFileOn", as.logical(value), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetLogFileOn(value)
##' @param value %% ~~Describe \code{value} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (value) 
##' {
##'   }
##' 
phrSetLogFileOn =
function(value)
{
  invisible(.Call("setLogFileOn", as.logical(value), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetOutputFileOn(value)
##' @param value %% ~~Describe \code{value} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (value) 
##' {
##'   }
##' 
phrSetOutputFileOn =
function(value)
{
  invisible(.Call("setOutputFileOn", as.logical(value), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetSelectedOutputFileOn(value)
##' @param value %% ~~Describe \code{value} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (value) 
##' {
##'   }
##' 
phrSetSelectedOutputFileOn =
function(value)
{
  invisible(.Call("setSelectedOutputFileOn", as.logical(value), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetDumpStringOn(value)
##' @param value %% ~~Describe \code{value} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (value) 
##' {
##'   }
##' 
phrSetDumpStringOn =
function(value)
{
  invisible(.Call("setDumpStringOn", as.logical(value), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetErrorStringOn(value)
##' @param value %% ~~Describe \code{value} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (value) 
##' {
##'   }
##' 
phrSetErrorStringOn =
function(value)
{
  invisible(.Call("setErrorStringOn", as.logical(value), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetLogStringOn(value)
##' @param value %% ~~Describe \code{value} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (value) 
##' {
##'   }
##' 
phrSetLogStringOn =
function(value)
{
  invisible(.Call("setLogStringOn", as.logical(value), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetOutputStringOn(value)
##' @param value %% ~~Describe \code{value} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (value) 
##' {
##'   }
##' 
phrSetOutputStringOn =
function(value)
{
  invisible(.Call("setOutputStringOn", as.logical(value), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetSelectedOutputStringOn(value)
##' @param value %% ~~Describe \code{value} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (value) 
##' {
##'   }
##' 
phrSetSelectedOutputStringOn =
function(value)
{
  invisible(.Call("setSelectedOutputStringOn", as.logical(value), PACKAGE=.packageName))
}




##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetDumpFileName(filename)
##' @param filename %% ~~Describe \code{filename} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (filename) 
##' {
##'   }
##' 
phrSetDumpFileName =
function(filename)
{
  invisible(.Call("setDumpFileName", as.character(filename), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetErrorFileName(filename)
##' @param filename %% ~~Describe \code{filename} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (filename) 
##' {
##'   }
##' 
phrSetErrorFileName =
function(filename)
{
  invisible(.Call("setErrorFileName", as.character(filename), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetLogFileName(filename)
##' @param filename %% ~~Describe \code{filename} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (filename) 
##' {
##'   }
##' 
phrSetLogFileName =
function(filename)
{
  invisible(.Call("setLogFileName", as.character(filename), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetOutputFileName(filename)
##' @param filename %% ~~Describe \code{filename} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (filename) 
##' {
##'   }
##' 
phrSetOutputFileName =
function(filename)
{
  invisible(.Call("setOutputFileName", as.character(filename), PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrSetSelectedOutputFileName(filename)
##' @param filename %% ~~Describe \code{filename} here~~
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function (filename) 
##' {
##'   }
##' 
phrSetSelectedOutputFileName =
function(filename)
{
  invisible(.Call("setSelectedOutputFileName", as.character(filename), PACKAGE=.packageName))
}




##' Retrieves the name of the dump file.
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetDumpFileName()
##' @return The name of the dump file as a string.
##' @note %% ~~further notes~~
##' @seealso \code{\link{phrGetDumpFileOn}}, \code{\link{phrGetDumpString}},
##' \code{\link{phrGetDumpStringOn}}, \code{\link{phrSetDumpFileName}},
##' \code{\link{phrSetDumpFileOn}}, \code{\link{phrSetDumpStringOn}}
##' @references \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' phrLoadDatabaseString(phreeqc.dat.list)
##' phrSetDumpFileOn(TRUE)
##' 
##' input <-              "SOLUTION 1 Pure water     \n"
##' input <- paste(input, "EQUILIBRIUM_PHASES 1      \n")
##' input <- paste(input, "    Calcite 0 10          \n")
##' input <- paste(input, "SAVE solution 1           \n")
##' input <- paste(input, "SAVE equilibrium_phases 1 \n")
##' input <- paste(input, "DUMP                      \n")
##' input <- paste(input, "    -solution 1           \n")
##' input <- paste(input, "    -equilibrium_phases  1\n")
##' 
##' if (!is.null(phrRunString(input))) {
##'     cat(phrGetErrorString())
##' }
##' cat(paste("see ", phrGetDumpFileName(), "."))
##' 
##' 
phrGetDumpFileName =
function()
{
  return(.Call("getDumpFileName", PACKAGE=.packageName))
}



##' Retrieves the name of the error file.
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetErrorFileName()
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function () 
##' {
##'   }
##' 
phrGetErrorFileName =
function()
{
  return(.Call("getErrorFileName", PACKAGE=.packageName))
}



##' Retrieves the name of the log file.
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetLogFileName()
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function () 
##' {
##'   }
##' 
phrGetLogFileName =
function()
{
  return(.Call("getLogFileName", PACKAGE=.packageName))
}



##' Retrieves the name of the output file.
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetOutputFileName()
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function () 
##' {
##'   }
##' 
phrGetOutputFileName =
function()
{
  return(.Call("getOutputFileName", PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetSelectedOutputFileName()
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function () 
##' {
##'   }
##' 
phrGetSelectedOutputFileName =
function()
{
  return(.Call("getSelectedOutputFileName", PACKAGE=.packageName))
}





##' Retrieves the string buffer containing DUMP output.
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetDumpString()
##' @return The dump output as a string.
##' @note %% ~~further notes~~
##' @seealso \code{\link{phrGetDumpFileName}}, \code{\link{phrGetDumpFileOn}},
##' \code{\link{phrGetDumpStringOn}}, \code{\link{phrSetDumpFileName}},
##' \code{\link{phrSetDumpFileOn}}, \code{\link{phrSetDumpStringOn}}
##' @references \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' phrLoadDatabaseString(phreeqc.dat.list)
##' phrSetDumpStringOn(TRUE)
##' 
##' input <-              "SOLUTION 1 Pure water     \n"
##' input <- paste(input, "EQUILIBRIUM_PHASES 1      \n")
##' input <- paste(input, "    Calcite 0 10          \n")
##' input <- paste(input, "SAVE solution 1           \n")
##' input <- paste(input, "SAVE equilibrium_phases 1 \n")
##' input <- paste(input, "DUMP                      \n")
##' input <- paste(input, "    -solution 1           \n")
##' input <- paste(input, "    -equilibrium_phases  1\n")
##' 
##' if (!is.null(phrRunString(input))) {
##'     cat(phrGetErrorString())
##' }
##' cat(phrGetDumpString())
##' 
##' 
##' phrLoadDatabaseString(phreeqc.dat.list)
##' phrSetDumpStringOn(TRUE)
##' 
##' input <-              "SOLUTION 1 Pure water     \n"
##' input <- paste(input, "EQUILIBRIUM_PHASES 1      \n")
##' input <- paste(input, "    Calcite 0 10          \n")
##' input <- paste(input, "SAVE solution 1           \n")
##' input <- paste(input, "SAVE equilibrium_phases 1 \n")
##' input <- paste(input, "DUMP                      \n")
##' input <- paste(input, "    -solution 1           \n")
##' input <- paste(input, "    -equilibrium_phases  1\n")
##' 
##' if (!is.null(phrRunString(input))) {
##'     cat(phrGetErrorString())
##' }
##' cat(phrGetDumpString())
##' 
##'
phrGetDumpString =
function()
{
  return(.Call("getDumpString", PACKAGE=.packageName))
}



##' Retrieves the string buffer containing phreeqc log output.
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetLogString()
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function () 
##' {
##'   }
##' 
phrGetLogString =
function()
{
  return(.Call("getLogString", PACKAGE=.packageName))
}



##' Retrieves the string buffer containing phreeqc output.
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetOutputString()
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function () 
##' {
##'   }
##' 
phrGetOutputString =
function()
{
  return(.Call("getOutputString", PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetSelectedOutputString()
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function () 
##' {
##'   }
##' 
phrGetSelectedOutputString =
function()
{
  return(.Call("getSelectedOutputString", PACKAGE=.packageName))
}



##' Retrieve warning messages.
##' 
##' Returns a character vector containing any warning messages that were
##' generated during the last invocation of the following methods:
##' \code{\link{phrAccumulateLine}}, \code{\link{phrLoadDatabase}},
##' \code{\link{phrLoadDatabaseString}}, \code{\link{phrRunAccumulatedeadString}},
##' \code{\link{phrRunFile}}, \code{\link{phrRunString}}
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetWarningString()
##' @return A character vector containing warning messages.
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' # This example loads the phreeqc.dat database and attempts to use the
##' # \code{DATABASE} keyword to set the database to wateq4f.dat.  A
##' # warning is displayed stating that the DATABASE keyword is ignored
##' # in the 'R' implementation.
##' phrLoadDatabaseString(phreeqc.dat.list)
##' phrAccumulateLine("DATABASE wateq4f.dat")
##' phrAccumulateLine("SOLUTION 1")
##' phrRunAccumulated()
##' if (is.null(phrGetWarningString())) {
##'   cat(phrGetWarningString())
##' }
##' 
phrGetWarningString =
function()
{
  return(.Call("getWarningString", PACKAGE=.packageName))
}


##' Load a phreeqc database file
##' 
##' Loads the given phreeqc database into phreeqc.  Returns NULL if successful.
##' 
##' 
##' @usage phrLoadDatabase(filename)
##' @param filename The file name of the database.
##' @return This function returns NULL.
##' @seealso \code{\link{phrLoadDatabaseString}}
##' @references Literature references and web URLs can go here.
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##'
##' # create temporary database file
##' tf <- tempfile()
##' writeLines(phreeqc.dat.list, tf)
##' 
##' if (is.null(phrLoadDatabase(tf))) {
##'   cat("database ok\n")
##' } else {
##'   cat("database contains errors\n")
##' }
##'
##' # delete temporary database file
##' unlink(tf)
##' 
phrLoadDatabase =
function(filename)
{
  invisible(.Call("loadDB", as.character(filename), PACKAGE=.packageName))
}


##' Load a phreeqc database as a string or a list of strings.
##' 
##' Load the specified string(s) as a database into phreeqc. Returns NULL if
##' successful.
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrLoadDatabaseString(input)
##' @param input String containing data to be used as the phreeqc database.
##' @return This function returns NULL.
##' @note All previous definitions are cleared.
##' @seealso \code{\link{phrLoadDatabase}}
##' @references \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##'   # this example loads the phreeqc.dat database, turns on the
##'   # output file and runs ex2 as a string
##'   phrLoadDatabaseString(phreeqc.dat.list)
##'   phrSetOutputFileOn(TRUE)
##'   if (is.null(phrRunString(ex2.list))) {
##'     cat(paste("see ", phrGetOutputFileName(), ".\n", sep=""))
##'   }
##' 
phrLoadDatabaseString =
function(input)
{
  invisible(.Call("loadDBLst", as.character(input), PACKAGE=.packageName))
}



##' Retrieve the accumulated input string.
##' 
##' Returns the accumulated text in the input buffer of the phreeqc object.
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrGetAccumulatedLines()
##' @return The input as a single string.
##' @note %% ~~further notes~~
##' @seealso \code{\link{phrAccumulateLine}}, \code{\link{phrRunAccumulated}}
##' @references \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' # This example loads some keyword input and displays the contents.
##' phrAccumulateLine("SOLUTION 1")
##' phrAccumulateLine("END")
##' cat(paste("The accumulated input is:", phrGetAccumulatedLines(), sep="\n"))
##' 
phrGetAccumulatedLines =
function()
{
  return(.Call("getAccumLines", PACKAGE=.packageName))
}



##' TODO
##' 
##' %% ~~ A concise (1-5 lines) description of what the function does. ~~
##' 
##' %% ~~ If necessary, more details than the description above ~~
##' 
##' @usage phrRunAccumulated()
##' @return %% ~Describe the value returned %% If it is a LIST, use %%
##' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
##' 'comp2'} %% ...
##' @note %% ~~further notes~~
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references %% ~put references to the literature/web site here ~
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##' 
##' ##---- Should be DIRECTLY executable !! ----
##' ##-- ==>  Define data, use random,
##' ##--	or do  help(data=index)  for the standard data sets.
##' 
##' ## The function is currently defined as
##' function () 
##' {
##'   }
##' 
phrRunAccumulated =
function()
{
  invisible(.Call("runAccum", PACKAGE=.packageName))
}



##' Run phreeqc input file
##' 
##' 
##' phrRunFile executes a phreeqc run
##' 
##' 
##' Any details about the operation of this function should go here.
##' 
##' @usage phrRunFile(filename)
##' @param filename The file name of the phreeqc input file.
##' @return This function returns NULL.
##' @seealso \code{\link{phrReadString}}, \code{\link{phrRun}},
##' \code{\link{phrGetSelectedOutput}}
##' @references Literature references and web URLs can go here.
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##'
##' # load the phreeqc.dat database
##' phrLoadDatabaseString(phreeqc.dat.list)
##'
##' # create ex2 if it doesn't exist
##' if (!file.exists("ex2")) writeLines(ex2.list, "ex2")
##' 
##' # run ex2
##' if (is.null(phrRunFile("ex2"))) {
##'   cat("use phrGetSelectedOutput() to see results.")
##' }
##' 
phrRunFile =
function(filename)
{
  invisible(.Call("runFile", as.character(filename), PACKAGE=.packageName))
}



##' Runs phreeqc using the given string as input.
##' 
##' Runs phreeqc using the given string as input. Returns the number of
##' errors encountered during the run.
##' 
##' The \code{RunString} method cannot be called until a database has
##' been successfully loaded by one of the following the LoadDatabase
##' methods \code{\link{phrLoadDatabase}}, \code{\link{phrLoadDatabaseString}}.
##' 
##' @usage phrRunString(input)
##' @param input character vector containing phreeqc input
##' @return The number of errors encountered during the run.
##' @note %% ~~further notes~~
##' @seealso \code{\link{phrRunAccumulated}}, \code{\link{phrRunFile}}
##' @references \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @keywords interface
##' @useDynLib phreeqc
##' @export
##' @examples
##'
##' #
##' # This example accumulates phreeqc input into a character vector
##' # and runs it.
##' #
##'
##' # load phreeqc.dat file
##' phrLoadDatabaseString(phreeqc.dat.list)
##'
##' # create input
##' input <- vector()
##' input <- c(input, "SOLUTION 1") 
##' input <- c(input, "  temp 25.0") 
##' input <- c(input, "  pH    7.0")
##'
##' # turn on output
##' phrSetOutputFileOn(TRUE)
##'
##' # run input
##' phrRunString(input)
##' cat(paste("see", phrGetOutputFileName(), ".")
##' 
phrRunString =
function(input)
{
  invisible(.Call("runStringLst", as.character(input), PACKAGE=.packageName))
}
