##' R interface to the phreeqc geochemical modeling program.
##' 
##' Provides an interface to PHREEQC (Version 3)--A Computer Program for
##' Speciation, Batch-Reaction, One-Dimensional Transport, and Inverse
##' Geochemical Calculations.
##' 
##' \tabular{ll}{ Package: \tab phreeqc\cr Type: \tab Package\cr Version: \tab
##' 1.0\cr Date: \tab 2014-02-05\cr License: \tab Unlimited\cr }
##' 
##' @name phreeqc-package
##' @aliases phreeqc-package phreeqc
##' @docType package
##' @author David L. Parkhurst \email{dlpark@@usgs.gov}\cr C.A.J. Appelo
##' \email{appt@@hydrochemistry.eu}\cr Maintainer: Scott R. Charlton
##' \email{charlton@@usgs.gov}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @keywords package
##' @examples
##'
##' #########################################################################
##' # Run ex2 and plot results
##' #########################################################################
##' 
##' # load the phreeqc.dat database
##' phrLoadDatabaseString(phreeqc.dat)
##' 
##' # run example 2
##' phrRunString(ex2)
##' 
##' # retrieve selected_output as a list of data.frame
##' so <- phrGetSelectedOutput()
##' 
##' # plot the results
##' attach(so$n1)
##' title  <- "Gypsum-Anhydrite Stability"
##' xlabel <- "Temperature, in degrees celcius"
##' ylabel <- "Saturation index"
##' plot(temp.C., si_gypsum, main = title, xlab = xlabel, ylab = ylabel,
##'      col = "darkred", xlim = c(25, 75), ylim = c(-0.4, 0.0))
##' points(temp.C., si_anhydrite, col = "darkgreen")
##' legend("bottomright", c("Gypsum", "Anhydrite"),
##'        col = c("darkred", "darkgreen"), pch = c(1, 1))
##'
##' 
##' #########################################################################
##' # Load data from .csv and calculate CO2
##' #########################################################################
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
##' solns <- data.frame("soln" = CO2.df[,1], "Temp" = CO2.df[,2],
##'                     "pH" = CO2.df[,3], "alkalinity" = CO2.df[,21],
##'                     "syringe_CO2" = CO2.df[,7])
##' 
##' # create input (as a character vector)
##' input <- vector()
##' for (i in 1:length(CO2.df[,1])) {
##'   input <- fSoln(input, solns[i, 1], solns[i, 2], solns[i, 3], solns[i, 4])
##' }
##' 
##' # add selected_output definition
##' input <- fSelOut(input)
##' 
##' # load database
##' phrLoadDatabaseString(phreeqc.dat)
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



##' Accumulate line(s) for input to phreeqc.
##' 
##' Appends a line of text to the input buffer in order to be run using
##' \code{\link{phrRunAccumulated}}.
##' 
##' @export phrAccumulateLine
##' @useDynLib phreeqc
##' @param line the line(s) to add for input to phreeqc.
##' @return NULL
##' @seealso \code{\link{phrClearAccumulatedLines}}, \code{\link{phrGetAccumulatedLines}}, \code{\link{phrRunAccumulated}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # this example loads the phreeqc.dat database, accumulates input, and
##' # runs it
##' phrLoadDatabaseString(phreeqc.dat)
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
##'   cat(paste("see ", phrGetOutputFileName(), ".\n", sep = ""))
##' }
##' 
phrAccumulateLine <-
function(line) {
  invisible(.Call("accumLineLst", as.character(line), PACKAGE = .packageName))
}



##' Clear the accumulated input buffer.
##' 
##' Clears the accumulated input buffer. The input buffer is accumulated from
##' calls to the \code{\link{phrAccumulateLine}} method.
##' 
##' @export phrClearAccumulatedLines
##' @useDynLib phreeqc
##' @return NULL
##' @seealso \code{\link{phrAccumulateLine}}, \code{\link{phrGetAccumulatedLines}}, \code{\link{phrRunAccumulated}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example loads some keyword input, clears the input, and displays
##' # the results.
##' phrAccumulateLine("SOLUTION 1")
##' phrAccumulateLine("END")
##' cat("The accumulated input is:", phrGetAccumulatedLines(), sep = "\n")
##' phrClearAccumulatedLines()
##' cat("The accumulated input now is:\n", phrGetAccumulatedLines(), sep = "\n")
##' 
phrClearAccumulatedLines <-
function() {
  invisible(.Call("clearAccum", PACKAGE = .packageName))
}



##' Retrieve the accumulated input.
##' 
##' Returns the accumulated input as a character vector.
##' 
##' @export phrGetAccumulatedLines
##' @useDynLib phreeqc
##' @return A character vector containing the accumulated input.
##' @seealso \code{\link{phrAccumulateLine}}, \code{\link{phrRunAccumulated}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example loads some keyword input and displays the contents.
##' phrAccumulateLine("SOLUTION 1")
##' phrAccumulateLine("END")
##' cat("The accumulated input is:", phrGetAccumulatedLines(), sep = "\n")
##' 
phrGetAccumulatedLines <-
function() {
  return(.Call("getAccumLines", PACKAGE = .packageName))
}



##' Retrieve a list containing the current list of components.
##' 
##' @export phrGetComponentList
##' @useDynLib phreeqc
##' @return A list containing the names of the components defined in the current system.
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example runs the ex2 input file and echos the list of components.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrRunString(ex2)
##' cat("components:\n")
##' for (comp in phrGetComponentList()) {
##'   cat(comp, "\n")
##' }
##' 
phrGetComponentList <-
function() {
  return(.Call("listComps", PACKAGE = .packageName))
}



##' Retrieve the name of the dump file.
##' 
##' Retrieves the name of the dump file. This file name is used if not
##' specified within DUMP input. The default value is dump.0.out.
##' 
##' @export phrGetDumpFileName
##' @useDynLib phreeqc
##' @return The name of the dump file as a string.
##' @seealso \code{\link{phrGetDumpFileOn}}, \code{\link{phrGetDumpStrings}}, \code{\link{phrGetDumpStringsOn}}, \code{\link{phrSetDumpFileName}}, \code{\link{phrSetDumpFileOn}}, \code{\link{phrSetDumpStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' phrLoadDatabaseString(phreeqc.dat)
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
##'   cat(phrGetErrorStrings())
##' }
##' cat(paste("see ", phrGetDumpFileName(), "."))
##' 
##' 
phrGetDumpFileName <-
function() {
  return(.Call("getDumpFileName", PACKAGE = .packageName))
}



##' Retrieve DUMP strings.
##'
##' Retrieves the string buffer containing DUMP output as a character
##' vector.
##' 
##' @export phrGetDumpStrings
##' @useDynLib phreeqc
##' @return The dump output as a character vector.
##' @seealso \code{\link{phrGetDumpFileName}}, \code{\link{phrGetDumpFileOn}}, \code{\link{phrGetDumpStringsOn}}, \code{\link{phrSetDumpFileName}}, \code{\link{phrSetDumpFileOn}}, \code{\link{phrSetDumpStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetDumpStringsOn(TRUE)
##' 
##' input <-              "SOLUTION 1 Pure water     \n"
##' input <- paste(input, "EQUILIBRIUM_PHASES 1      \n")
##' input <- paste(input, "    Calcite 0 10          \n")
##' input <- paste(input, "SAVE solution 1           \n")
##' input <- paste(input, "SAVE equilibrium_phases 1 \n")
##' input <- paste(input, "DUMP                      \n")
##' input <- paste(input, "    -solution 1           \n")
##' input <- paste(input, "    -equilibrium_phases 1 \n")
##' 
##' if (!is.null(phrRunString(input))) {
##'   cat(phrGetErrorStrings(), sep = "\n")
##' }
##' cat(phrGetDumpStrings(), sep = "\n")
##'
phrGetDumpStrings <-
function() {
  return(.Call("getDumpStrings", PACKAGE = .packageName))
}



##' Retrieve the name of the error file.
##' 
##' Retrieves the name of the error file. The default value is phreeqc.0.err.
##'
##' The error file switch must be set using the \code{\link{phrSetErrorFileOn}} function.
##' 
##' @export phrGetErrorFileName
##' @useDynLib phreeqc
##' @return The name of the error file as a string.
##' @seealso \code{\link{phrGetErrorFileOn}}, \code{\link{phrGetErrorStrings}}, \code{\link{phrGetErrorStringsOn}}, \code{\link{phrSetErrorFileName}}, \code{\link{phrSetErrorFileOn}}, \code{\link{phrSetErrorStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' 
phrGetErrorFileName <-
function() {
  return(.Call("getErrorFileName", PACKAGE = .packageName))
}



##' Retrieve the current value of the dump file switch.
##' 
##' @export phrGetDumpFileOn
##' @useDynLib phreeqc
##' @return TRUE if errors are currently being written to file.
##' @seealso \code{\link{phrGetDumpFileName}}, \code{\link{phrGetDumpStrings}}, \code{\link{phrGetDumpStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' 
phrGetDumpFileOn <-
function() {
  return(.Call("getDumpFileOn", PACKAGE = .packageName))
}



##' Retrieve the current value of the dump strings switch.
##' 
##' @export phrGetDumpStringsOn
##' @useDynLib phreeqc
##' @return TRUE if errors are currently being written to file.
##' @seealso \code{\link{phrGetDumpFileName}}, \code{\link{phrGetDumpFileOn}}, \code{\link{phrGetDumpStrings}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' 
phrGetDumpStringsOn <-
function() {
  return(.Call("getDumpStringOn", PACKAGE = .packageName))
}



##' Retrieve the current value of the error file switch.
##' 
##' @export phrGetErrorFileOn
##' @useDynLib phreeqc
##' @return TRUE if errors are currently being written to file.
##' @seealso \code{\link{phrGetErrorFileName}}, \code{\link{phrGetErrorFileOn}}, \code{\link{phrGetErrorStrings}},
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' 
phrGetErrorFileOn <-
function() {
  return(.Call("getErrorFileOn", PACKAGE = .packageName))
}



##' Retrieve the current value of the error strings switch.
##' 
##' @export phrGetErrorStringsOn
##' @useDynLib phreeqc
##' @return TRUE if errors are currently being written to file.
##' @seealso \code{\link{phrGetErrorFileName}}, \code{\link{phrGetErrorFileOn}}, \code{\link{phrGetErrorStrings}},
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' 
phrGetErrorStringsOn <-
function() {
  return(.Call("getErrorStringOn", PACKAGE = .packageName))
}



##' Retrieve the current value of the log file switch.
##' 
##' @export phrGetLogFileOn
##' @useDynLib phreeqc
##' @return TRUE if errors are currently being written to file.
##' @seealso \code{\link{phrGetLogFileName}}, \code{\link{phrGetLogFileOn}}, \code{\link{phrGetLogStrings}},
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' 
phrGetLogFileOn <-
function() {
  return(.Call("getLogFileOn", PACKAGE = .packageName))
}



##' Retrieve the current value of the log strings switch.
##' 
##' @export phrGetLogStringsOn
##' @useDynLib phreeqc
##' @return TRUE if errors are currently being written to file.
##' @seealso \code{\link{phrGetLogFileName}}, \code{\link{phrGetLogFileOn}}, \code{\link{phrGetLogStrings}},
##' @references \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' 
phrGetLogStringsOn <-
function() {
  return(.Call("getLogStringOn", PACKAGE = .packageName))
}



##' Retrieve the current value of the output file switch.
##' 
##' @export phrGetOutputFileOn
##' @useDynLib phreeqc
##' @return TRUE if errors are currently being written to file.
##' @seealso \code{\link{phrGetOutputFileName}}, \code{\link{phrGetOutputStringsOn}}, \code{\link{phrGetOutputStrings}},
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' 
phrGetOutputFileOn <-
function() {
  return(.Call("getOutputFileOn", PACKAGE = .packageName))
}



##' Retrieve the current value of the output strings switch.
##' 
##' @export phrGetOutputStringsOn
##' @useDynLib phreeqc
##' @return TRUE if errors are currently being written to file.
##' @seealso \code{\link{phrGetOutputFileName}}, \code{\link{phrGetOutputFileOn}}, \code{\link{phrGetOutputStrings}},
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' 
phrGetOutputStringsOn <-
function() {
  return(.Call("getOutputStringOn", PACKAGE = .packageName))
}



##' Retrieve error string messages.
##' 
##' Retrieves a character vector containing any error messages that were generated
##' during the last invocation of the following methods:
##' \code{\link{phrAccumulateLine}}, \code{\link{phrLoadDatabase}},
##' \code{\link{phrLoadDatabaseString}}, \code{\link{phrRunAccumulated}},
##' \code{\link{phrRunFile}}, \code{\link{phrRunString}}
##' 
##' This routine is rarely needed when running interactively since the error
##' string is displayed when it occurs.
##' 
##' @export phrGetErrorStrings
##' @useDynLib phreeqc
##' @return The error messages as a character vector.
##' @seealso \code{\link{phrGetErrorFileName}}, \code{\link{phrGetErrorFileOn}}, \code{\link{phrGetErrorStringsOn}}, \code{\link{phrSetErrorFileName}}, \code{\link{phrSetErrorFileOn}}, \code{\link{phrSetErrorStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # loaddatabase should fail
##' n <- try(phrLoadDatabase("missing.dat"), silent = TRUE)
##' # if n is non-NULL display error string
##' if (!is.null(n)) phrGetErrorStrings()
##' 
phrGetErrorStrings <-
function() {
  return(.Call("getErrorStrings", PACKAGE = .packageName))
}



##' Retrieve the name of the log file.
##' 
##' Retrieves the name of the log file. The default name is phreeqc.0.log.
##' 
##' @export phrGetLogFileName
##' @useDynLib phreeqc
##' @return The name of the log file as a string.
##' @seealso \code{\link{phrGetLogFileOn}}, \code{\link{phrGetLogStrings}}, \code{\link{phrGetLogStringsOn}}, \code{\link{phrSetLogFileName}}, \code{\link{phrSetLogFileOn}}, \code{\link{phrSetLogStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##'
##' # This example checks to see if the log file is turned on
##' # and prints the appropriate message
##' if (phrGetLogFileOn()) {
##'   cat("The name of the log file (is/will be):", phrGetLogFileName(), "\n")
##' } else {
##'   cat("The log file is not turned on\n")
##' }
##' 
phrGetLogFileName <-
function() {
  return(.Call("getLogFileName", PACKAGE = .packageName))
}



##' Retrieve log output.
##' 
##' Retrieves the string buffer containing phreeqc log output.
##' 
##' @export phrGetLogStrings
##' @useDynLib phreeqc
##' @return A character vector containing phreeqc log output.
##' @seealso \code{\link{phrGetLogFileName}}, \code{\link{phrGetLogFileOn}}, \code{\link{phrGetLogStringsOn}}, \code{\link{phrSetLogFileName}}, \code{\link{phrSetLogFileOn}}, \code{\link{phrSetLogStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example equilibrates pure water with gypsum with the output file on.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetOutputFileOn(TRUE)
##'
##' input <- vector(mode="character")
##' input <- c(input, "SOLUTION 1 Pure water ")
##' input <- c(input, "EQUILIBRIUM_PHASES 1  ")
##' input <- c(input, "  Gypsum 0 10         ")
##' input <- c(input, "KNOBS                 ")
##' input <- c(input, "  -logfile TRUE       ")
##'
##' if (is.null(phrRunString(input))) {
##'   log <- phrGetLogStrings()
##' }
##' 
phrGetLogStrings <-
function() {
  return(.Call("getLogStrings", PACKAGE = .packageName))
}



##' Retrieve the name of the output file.
##' 
##' Retrieves the name of the output file. The default name is phreeqc.0.out.
##' 
##' @export
##' @useDynLib phreeqc
##' @return The name of the outputfile as a string.
##' @seealso \code{\link{phrGetOutputFileOn}}, \code{\link{phrGetOutputStrings}}, \code{\link{phrGetOutputStringsOn}}, \code{\link{phrSetOutputFileName}}, \code{\link{phrSetOutputFileOn}}, \code{\link{phrSetOutputStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example equilibrates pure water with gypsum with the output file on.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetOutputFileOn(TRUE)
##'
##' input <- vector(mode="character")
##' input <- c(input, "SOLUTION 1 Pure water ")
##' input <- c(input, "EQUILIBRIUM_PHASES 1  ")
##' input <- c(input, "  Gypsum 0 10         ")
##'
##' if (is.null(phrRunString(input))) {
##'   output <- readLines(phrGetOutputFileName())
##'   unlink(phrGetOutputFileName())  # tidy up
##' }
##' 
phrGetOutputFileName <-
function() {
  return(.Call("getOutputFileName", PACKAGE = .packageName))
}



##' Retrieve standard phreeqc output.
##' 
##' Retrieves the phreeqc output as a character vector.
##' 
##' A NULL value is returned when there is no selected-output.
##' 
##' @export phrGetOutputStrings
##' @useDynLib phreeqc
##' @return A character vector containing phreeqc output.
##' @seealso \code{\link{phrGetOutputFileName}}, \code{\link{phrGetOutputFileOn}}, \code{\link{phrGetOutputStringsOn}}, \code{\link{phrSetOutputFileOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example equilibrates pure water with calcite and displays
##' # the results
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetOutputStringsOn(TRUE)
##'
##' input <- vector(mode="character")
##' input <- c(input, "SOLUTION 1 Pure water ")
##' input <- c(input, "EQUILIBRIUM_PHASES 1  ")
##' input <- c(input, "  Gypsum 0 10         ")
##'
##' if (is.null(phrRunString(input))) {
##'   cat(phrGetOutputStrings(), sep = "\n")
##' }
##' 
phrGetOutputStrings <-
function() {
  return(.Call("getOutputStrings", PACKAGE = .packageName))
}



##' Returns the contents of the selected output as a list of data frames.
##' 
##' phrGetSelectedOutput returns a named list containing the resultant
##' selected output blocks.  The names of each data frame are creating by
##' concatenating the letter 'n' and the user number of the selected output
##' block.
##'
##' phrGetSelectedOutput uses the \code{\link{make.names}} function to create
##' valid column names. The allow_ variable is passed to
##' \code{\link{make.names}} and is used for backward compatibility.
##' 
##' @export phrGetSelectedOutput
##' @useDynLib phreeqc
##' @param allow_ used for compatibility with R prior to 1.9.0 (default is TRUE).
##' @return Returns a named list of data frames containing the selected_output from the previous run.
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # Load database and run ex2
##' phrLoadDatabaseString(phreeqc.dat)
##' phrRunString(ex2)
##'
##' # display a summary of the results
##' df <- phrGetSelectedOutput()
##' summary(df$n1)
##' 
phrGetSelectedOutput <-
function(allow_ = TRUE) {
  sel_outs <- .Call("getSelOutLst", PACKAGE = .packageName)
  if (!is.null(sel_outs)) {
    for (t in names(sel_outs)) {
      if (!is.null(sel_outs[[t]])) {
        names(sel_outs[[t]]) <- make.names(names(sel_outs[[t]]), unique = TRUE,
                                           allow_ = allow_)
      }
    }
  }
  return(sel_outs)
}



##' Retrieve warning messages.
##' 
##' Returns a character vector containing any warning messages that were
##' generated during the last invocation of the following methods:
##' \code{\link{phrAccumulateLine}}, \code{\link{phrLoadDatabase}},
##' \code{\link{phrLoadDatabaseString}}, \code{\link{phrRunAccumulated}},
##' \code{\link{phrRunFile}}, \code{\link{phrRunString}}.
##' 
##' A NULL value is returned if there are no warnings.
##' 
##' @export phrGetWarningStrings
##' @useDynLib phreeqc
##' @return A character vector containing warning messages or NULL.
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example loads the phreeqc.dat database and attempts to use the
##' # DATABASE keyword to set the database to wateq4f.dat.  A warning is
##' # displayed stating that the DATABASE keyword is ignored in the 'R'
##' # implementation.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrAccumulateLine("DATABASE wateq4f.dat")
##' phrAccumulateLine("SOLUTION 1")
##' phrRunAccumulated()
##' if (!is.null(phrGetWarningStrings())) {
##'   cat(phrGetWarningStrings(), sep = "\n")
##' }
##' 
phrGetWarningStrings <-
function() {
  return(.Call("getWarningStrings", PACKAGE = .packageName))
}



##' Load a phreeqc database file
##' 
##' Loads the given phreeqc database into phreeqc.  Returns NULL if successful.
##' 
##' 
##' @export phrLoadDatabase
##' @useDynLib phreeqc
##' @param filename The name of the database file.
##' @return This function returns NULL.
##' @seealso \code{\link{phrLoadDatabaseString}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##'
##' # create temporary database file
##' tf <- tempfile()
##' writeLines(phreeqc.dat, tf)
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
phrLoadDatabase <-
function(filename) {
  invisible(.Call("loadDB", as.character(filename), PACKAGE = .packageName))
}



##' Load a phreeqc database as a string or a list of strings.
##' 
##' Load the specified string(s) as a database into phreeqc. Returns NULL if
##' successful.
##' 
##' All previous definitions are cleared.
##' 
##' @export phrLoadDatabaseString
##' @useDynLib phreeqc
##' @param input String containing data to be used as the phreeqc database.
##' @return This function returns NULL.
##' @seealso \code{\link{phrLoadDatabase}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @keywords interface
##' @examples
##' 
##' # this example loads the phreeqc.dat database, turns on the
##' # output file and runs ex2 as a string
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetOutputFileOn(TRUE)
##' if (is.null(phrRunString(ex2))) {
##'   cat(paste("see ", phrGetOutputFileName(), ".\n", sep = ""))
##' }
##' 
phrLoadDatabaseString <-
function(input) {
  invisible(.Call("loadDBLst", as.character(input), PACKAGE = .packageName))
}



##' Runs the accumulated input.
##' 
##' Runs the input buffer as defined by calls to \code{\link{phrAccumulateLine}}.
##' 
##' After calling \code{phrRunAccumulated} \code{\link{phrGetAccumulatedLines}} can
##' be used in case of errors. The accumulated lines will be cleared on the next call
##' to \code{\link{phrAccumulateLine}}.
##'
##' The \code{phrAccumulateLine} method cannot be called until a database
##' has been successfully loaded by calls to either
##' \code{\link{phrLoadDatabase}} or \code{\link{phrLoadDatabaseString}}.
##' 
##' @export phrRunAccumulated
##' @useDynLib phreeqc
##' @return This function returns NULL on success.
##' @seealso \code{\link{phrAccumulateLine}}, \code{\link{phrClearAccumulatedLines}}, \code{\link{phrGetAccumulatedLines}}, \code{\link{phrRunFile}}, \code{\link{phrRunString}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # turn on the output file
##' phrSetOutputFileOn(TRUE)
##' 
##' # load the phreeqc.dat database
##' phrLoadDatabaseString(phreeqc.dat)
##' 
##' # accumulate the input
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
##' 
##' # run it and echo the name of the output file
##' if (is.null(phrRunAccumulated())) {
##'   cat(paste("see ", phrGetOutputFileName(), ".\n", sep = ""))
##' }
phrRunAccumulated <-
function() {
  invisible(.Call("runAccum", PACKAGE = .packageName))
}



##' Run phreeqc input file
##' 
##' phrRunFile executes a phreeqc run using a file as input
##' 
##' @export phrRunFile
##' @useDynLib phreeqc
##' @param filename The file name of the phreeqc input file.
##' @return This function returns NULL on success.
##' @seealso \code{\link{phrAccumulateLine}}, \code{\link{phrRunAccumulated}}, \code{\link{phrGetSelectedOutput}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##'
##' # load the phreeqc.dat database
##' phrLoadDatabaseString(phreeqc.dat)
##'
##' # create ex2 if it doesn't exist
##' if (!file.exists("ex2")) writeLines(ex2, "ex2")
##' 
##' # run ex2
##' if (is.null(phrRunFile("ex2"))) {
##'   cat("use phrGetSelectedOutput() to see results.")
##' }
##'
##' unlink("ex2")  # tidy up
##' 
phrRunFile <-
function(filename) {
  invisible(.Call("runFile", as.character(filename), PACKAGE = .packageName))
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
##' @export phrRunString
##' @useDynLib phreeqc
##' @param input character vector containing phreeqc input
##' @return This function returns NULL on success.
##' @seealso \code{\link{phrRunAccumulated}}, \code{\link{phrRunFile}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @keywords interface
##' @examples
##'
##' #
##' # This example accumulates phreeqc input into a character vector
##' # and runs it.
##' #
##'
##' # load phreeqc.dat file
##' phrLoadDatabaseString(phreeqc.dat)
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
##' cat(paste("see", phrGetOutputFileName(), "."))
##' 
phrRunString <-
function(input) {
  invisible(.Call("runStringLst", as.character(input), PACKAGE = .packageName))
}



##' Set the name of the dump file.
##' 
##' Sets the name of the dump file. This file name is used if not specified
##' within the DUMP keyword block. The default value is dump.0.out.
##' 
##' @export phrSetDumpFileName
##' @useDynLib phreeqc
##' @param filename the name of the file.
##' @return NULL
##' @seealso \code{\link{phrGetDumpFileName}}, \code{\link{phrGetDumpFileOn}}, \code{\link{phrGetDumpStrings}}, \code{\link{phrGetDumpStringsOn}}, \code{\link{phrSetDumpFileOn}}, \code{\link{phrSetDumpStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##'
##' # This example equilibrates pure water with calcite and writes the 
##' # dump results to file.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetDumpFileOn(TRUE)
##' phrSetDumpFileName("phreeqc.dump")
##' input <- c( 
##'   'SOLUTION 1 Pure water     ',
##'   'EQUILIBRIUM_PHASES 1      ',
##'   '    Calcite 0 10          ',
##'   'SAVE solution 1           ',
##'   'SAVE equilibrium_phases 1 ',
##'   'DUMP                      ',
##'   '    -solution 1           ',
##'   '    -equilibrium_phases 1 '
##'   )
##' 
##' if (is.null(phrRunString(input))) {
##'   cat("see", phrGetDumpFileName(), "\n")
##' }
##' 
phrSetDumpFileName <-
function(filename) {
  invisible(.Call("setDumpFileName", as.character(filename), PACKAGE = .packageName))
}



##' Set the dump file on/off.
##' 
##' Sets the dump file switch on or off. This switch controls whether or
##' not phreeqc writes to the dump file. The initial setting is off.
##' 
##' @export phrSetDumpFileOn
##' @useDynLib phreeqc
##' @param value if TRUE, captures output normally sent to the dump file into a buffer.
##' @return NULL
##' @seealso \code{\link{phrGetDumpFileName}}, \code{\link{phrGetDumpFileOn}}, \code{\link{phrGetDumpStrings}}, \code{\link{phrGetDumpStringsOn}}, \code{\link{phrSetDumpFileOn}}, \code{\link{phrSetDumpStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example equilibrates pure water with calcite and writes the 
##' # dump results to file.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetDumpFileOn(TRUE)
##' input <- c( 
##'   'SOLUTION 1 Pure water     ',
##'   'EQUILIBRIUM_PHASES 1      ',
##'   '    Calcite 0 10          ',
##'   'SAVE solution 1           ',
##'   'SAVE equilibrium_phases 1 ',
##'   'DUMP                      ',
##'   '    -solution 1           ',
##'   '    -equilibrium_phases 1 '
##'   )
##' 
##' if (is.null(phrRunString(input))) {
##'   cat("see", phrGetDumpFileName(), "\n")
##' }
##' 
phrSetDumpFileOn <-
function(value) {
  invisible(.Call("setDumpFileOn", as.logical(value), PACKAGE = .packageName))
}



##' Set dump strings on/off.
##' 
##' Sets the dump strings switch on or off. This switch controls whether or
##' not the data normally sent to the dump file are stored in a buffer for
##' retrieval. The initial setting is off.
##'
##' @export phrSetDumpStringsOn
##' @useDynLib phreeqc
##' @param value if TRUE, captures output normally sent to the error file into a buffer.
##' @return NULL
##' @seealso \code{\link{phrGetDumpFileOn}}, \code{\link{phrGetDumpStrings}}, \code{\link{phrGetDumpStringsOn}}, \code{\link{phrSetDumpFileOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##'
##' # This example equilibrates pure water with calcite and echos the 
##' # dump strings.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetDumpStringsOn(TRUE)
##' input <- c( 
##'   'SOLUTION 1 Pure water     ',
##'   'EQUILIBRIUM_PHASES 1      ',
##'   '    Calcite 0 10          ',
##'   'SAVE solution 1           ',
##'   'SAVE equilibrium_phases 1 ',
##'   'DUMP                      ',
##'   '    -solution 1           ',
##'   '    -equilibrium_phases 1 '
##'   )
##' 
##' if (is.null(phrRunString(input))) {
##'   cat("Dump:\n")
##'   cat(phrGetDumpStrings(), sep = "\n")
##' }
##' 
phrSetDumpStringsOn <-
function(value) {
  invisible(.Call("setDumpStringOn", as.logical(value), PACKAGE = .packageName))
}



##' Set the name of the error file.
##' 
##' Sets the name of the error file. The default value is phreeqc.0.err.
##' 
##' @export phrSetErrorFileName
##' @useDynLib phreeqc
##' @param filename the name of the file.
##' @return NULL
##' @seealso \code{\link{phrGetErrorFileName}}, \code{\link{phrGetErrorFileOn}}, \code{\link{phrGetErrorStrings}}, \code{\link{phrGetErrorStringsOn}}, \code{\link{phrSetErrorFileOn}}, \code{\link{phrSetErrorStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##'
##' # This example equilibrates pure water with calcite and displays
##' # the log file name.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetLogFileOn(TRUE)
##' phrSetLogFileName("phreeqc.log")
##' input <- c( 
##'   'SOLUTION 1 Pure water ',
##'   'EQUILIBRIUM_PHASES 1  ',
##'   '    Calcite 0 10      '
##'   )
##' 
##' if (is.null(phrRunString(input))) {
##'   cat("see", phrGetErrorFileName(), "\n")
##' }
##' 
phrSetErrorFileName <-
function(filename) {
  invisible(.Call("setErrorFileName", as.character(filename), PACKAGE = .packageName))
}



##' Set error file on/off.
##' 
##' Sets the error file switch on or off. This switch controls whether
##' or not phreeqc writes to the error file. The initial setting is off.
##' 
##' The try is necessary to keep the error message from displaying immediately.
##'
##' @export phrSetErrorFileOn
##' @useDynLib phreeqc
##' @param value if TRUE, writes output to the the error file.
##' @return NULL
##' @seealso \code{\link{phrGetErrorFileName}}, \code{\link{phrGetErrorFileOn}}, \code{\link{phrGetErrorStringsOn}}, \code{\link{phrGetErrorStrings}}, \code{\link{phrSetErrorStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##'
##' # This example attempts to run ex1, fails, and writes the error
##' # message to the error file (no database is loaded).
##' phrSetErrorFileOn(TRUE)
##' phrSetErrorFileName("phreeqc.errors")
##' if (!is.null(try(phrRunString(ex1), silent=TRUE))) {
##'   cat("see", phrGetErrorFileName(), "\n")
##' }
##' 
phrSetErrorFileOn <-
function(value) {
  invisible(.Call("setErrorFileOn", as.logical(value), PACKAGE = .packageName))
}



##' Set error strings on/off.
##' 
##' Sets the error strings switch on or off.  This switch controls whether or
##' not the data normally sent to the error file are stored in a buffer for
##' retrieval. The initial setting is on.
##' 
##' The try is necessary to keep the error message from displaying immediately.
##' 
##' @export phrSetErrorStringsOn
##' @useDynLib phreeqc
##' @param value if TRUE, captures output normally sent to the error file into a buffer.
##' @return NULL
##' @seealso \code{\link{phrGetErrorFileOn}}, \code{\link{phrGetErrorStringsOn}}, \code{\link{phrSetErrorFileOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##'
##' # This example attempts to run ex1, fails, and displays the error message
##' # (no database is loaded).
##' phrSetErrorStringsOn(TRUE)
##' if (!is.null(try(phrRunString(ex1), silent=TRUE))) {
##'   cat(phrGetErrorStrings(), "\n")
##' }
##' 
phrSetErrorStringsOn <-
function(value) {
  invisible(.Call("setErrorStringOn", as.logical(value), PACKAGE = .packageName))
}



##' Set the name of the log file.
##' 
##' Sets the name of the log file. The default value is phreeqc.0.log
##' 
##' Logging must be enabled through the use of the KNOBS -logfile
##' option in order to receive an log messages.
##' 
##' @export phrSetLogFileName
##' @useDynLib phreeqc
##' @param filename the name of the file.
##' @return NULL
##' @seealso \code{\link{phrGetLogFileName}}, \code{\link{phrGetLogFileOn}}, \code{\link{phrGetLogStrings}}, \code{\link{phrGetLogStringsOn}}, \code{\link{phrSetLogFileOn}}, \code{\link{phrSetLogStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example equilibrates pure water with calcite and displays
##' # the log file name.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetLogFileOn(TRUE)
##' phrSetLogFileName("phreeqc.log")
##' input <- c( 
##'   'SOLUTION 1 Pure water ',
##'   'EQUILIBRIUM_PHASES 1  ',
##'   '    Calcite 0 10      ',
##'   'KNOBS                 ',
##'   '    -logfile true     '
##'   )
##' 
##' if (is.null(phrRunString(input))) {
##'   cat("see", phrGetLogFileName(), "\n")
##' }
##' 
phrSetLogFileName <-
function(filename) {
  invisible(.Call("setLogFileName", as.character(filename), PACKAGE = .packageName))
}



##' Set log file on/off.
##' 
##' Sets the log file switch on or off. This switch controls whether
##' or not phreeqc writes log messages to the log file. The initial
##' setting is off.
##'
##' Logging must be enabled through the use of the KNOBS -logfile
##' option in order to receive an log messages.
##' 
##' @export phrSetLogFileOn
##' @useDynLib phreeqc
##' @param value if TRUE, writes output to the the log file.
##' @return NULL
##' @seealso \code{\link{phrGetLogFileName}}, \code{\link{phrGetLogFileOn}}, \code{\link{phrGetLogStringsOn}}, \code{\link{phrGetLogStrings}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example runs ex2 with the log file turned on.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetLogStringsOn(TRUE)
##'
##' # turn logging on
##' phrAccumulateLine("KNOBS; -logfile true")
##' phrRunAccumulated()
##' 
##' if (is.null(phrRunString(ex2))) {
##'   cat("see", phrGetLogFileName(), "\n")
##' }
##' 
phrSetLogFileOn <-
function(value) {
  invisible(.Call("setLogFileOn", as.logical(value), PACKAGE = .packageName))
}



##' Set log strings on/off.
##' 
##' Sets the log strings switch on or off.  This switch controls whether or
##' not the data normally sent to the log file are stored in a buffer for
##' retrieval. The initial setting is off.
##' 
##' @export phrSetLogStringsOn
##' @useDynLib phreeqc
##' @param value if TRUE, captures output normally sent to the log file into a buffer.
##' @return NULL
##' @seealso \code{\link{phrGetLogFileOn}}, \code{\link{phrGetLogStringsOn}}, \code{\link{phrSetLogFileOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example runs ex2 with log strings turned on.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetLogStringsOn(TRUE)
##'
##' # turn logging on
##' phrAccumulateLine("KNOBS; -logfile true")
##' phrRunAccumulated()
##' 
##' if (is.null(phrRunString(ex2))) {
##'   cat(phrGetLogStrings(), sep = "\n")
##' }
##' 
phrSetLogStringsOn <-
function(value) {
  invisible(.Call("setLogStringOn", as.logical(value), PACKAGE = .packageName))
}



##' Set the name of the output file.
##' 
##' Sets the name of the output file. The default value is phreeqc.0.out.
##'
##' The output file must be turned on using the \code{\link{phrSetOutputFileOn}} function.
##' 
##' @export phrSetOutputFileName
##' @useDynLib phreeqc
##' @param filename the name of the file.
##' @return NULL
##' @seealso \code{\link{phrGetOutputFileName}}, \code{\link{phrGetOutputFileOn}}, \code{\link{phrGetOutputStringsOn}}, \code{\link{phrGetOutputStrings}}, \code{\link{phrSetOutputFileOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example equilibrates pure water with calcite and displays
##' # the resulting file name.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetOutputFileOn(TRUE)
##' phrSetOutputFileName("phreeqc.output")
##' input <- c( 
##'   'SOLUTION 1 Pure water ',
##'   'EQUILIBRIUM_PHASES 1  ',
##'   '    Calcite 0 10      '
##'   )
##' 
##' if (is.null(phrRunString(input))) {
##'   cat("see", phrGetOutputFileName(), "\n")
##' }
##' 
phrSetOutputFileName <-
function(filename) {
  invisible(.Call("setOutputFileName", as.character(filename), PACKAGE = .packageName))
}



##' Set output file on/off.
##' 
##' Sets the output file switch on or off. This switch controls whether
##' or not phreeqc writes to the output file. This is the output normally
##' generated when phreeqc is run. The initial setting is off.
##' 
##' @export phrSetOutputFileOn
##' @useDynLib phreeqc
##' @param value if TRUE, writes output to the the output file.
##' @return NULL
##' @seealso \code{\link{phrGetOutputFileName}}, \code{\link{phrGetOutputFileOn}}, \code{\link{phrGetOutputStringsOn}}, \code{\link{phrGetOutputStrings}}, \code{\link{phrSetOutputStringsOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example runs ex2 with the output file turned on.
##'
##' # write temporary input file
##' tf <- tempfile()
##' writeLines(ex2, tf)
##'
##' # load database and run input file
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetOutputFileOn(TRUE)
##' if (is.null(phrRunFile(tf))) {
##'   cat("see", phrGetOutputFileName(), "\n")
##' }
##'
##' # delete temporary input file
##' unlink(tf)
##' 
phrSetOutputFileOn <-
function(value) {
  invisible(.Call("setOutputFileOn", as.logical(value), PACKAGE = .packageName))
}



##' Set output strings on/off.
##' 
##' Set the output string switch on or off.  This switch controls whether or
##' not the data normally sent to the output file are stored in a buffer for
##' retrieval. The initial setting is off.
##' 
##' The output strings setting is only used by the Run methods:
##' \code{\link{phrRunAccumulated}}, \code{\link{phrRunFile}},
##' \code{\link{phrRunString}}.
##'
##' @export phrSetOutputStringsOn
##' @useDynLib phreeqc
##' @param value if TRUE, captures output normally sent to the output file into a buffer.
##' @return NULL
##' @seealso \code{\link{phrGetOutputFileOn}}, \code{\link{phrGetOutputStringsOn}}, \code{\link{phrGetOutputStrings}}, \code{\link{phrSetOutputFileOn}}
##' @references \url{ftp://brrftp.cr.usgs.gov/pub/charlton/iphreeqc/IPhreeqc.pdf}
##' @examples
##' 
##' # This example equilibrates pure water with calcite and displays
##' # the results.
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetOutputStringsOn(TRUE)
##' input <- c( 
##'   'SOLUTION 1 Pure water ',
##'   'EQUILIBRIUM_PHASES 1  ',
##'   '    Calcite 0 10      '
##'   )
##' 
##' if (is.null(phrRunString(input))) {
##'   cat(phrGetOutputStrings(), sep = "\n")
##' }
##' 
phrSetOutputStringsOn <-
function(value) {
  invisible(.Call("setOutputStringOn", as.logical(value), PACKAGE = .packageName))
}



##' @name phreeqc.dat
##' @title The phreeqc.dat database
##' @description phreeqc.dat is a phreeqc database file derived from PHREEQE,
##' which is consistent with wateq4f.dat, but has a smaller set of elements and
##' aqueous species. The database has been reformatted for use by
##' \code{\link{phrLoadDatabaseString}}.
##' @docType data
##' @references \url{http://pubs.usgs.gov/tm/06/a43/pdf/tm6-A43.pdf}
##' @source \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @usage phreeqc.dat  # phrLoadDatabaseString(phreeqc.dat)
##' @keywords dataset 
NULL



##' @name Amm.dat
##' @title The Amm.dat database.
##' @description Amm.dat is the same as phreeqc.dat, except that ammmonia redox
##' state has been decoupled from the rest of the nitrogen system; that is,
##' ammonia has been defined as a separate component. The database has been
##' reformatted for use by \code{\link{phrLoadDatabaseString}}.
##' @docType data
##' @references \url{http://pubs.usgs.gov/tm/06/a43/pdf/tm6-A43.pdf}
##' @source \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @usage Amm.dat  # phrLoadDatabaseString(Amm.dat)
##' @keywords dataset 
NULL



##' @name wateq4f.dat
##' @title The wateq4f.dat database.
##' @description wateq4f.dat is a database derived from WATEQ4F. The database
##' has been reformatted for use by \code{\link{phrLoadDatabaseString}}.
##' @docType data
##' @references \url{http://pubs.usgs.gov/tm/06/a43/pdf/tm6-A43.pdf}
##' @source \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @usage wateq4f.dat  # phrLoadDatabaseString(wateq4f.dat)
##' @keywords dataset 
NULL



##' @name llnl.dat
##' @title The llnl.dat database.
##' @description llnl.dat is a database derived from databases for EQ3/6 and
##' Geochemist's Workbench that uses thermodynamic data compiled by the
##' Lawrence Livermore National Laboratory. The database has been reformatted
##' for use by \code{\link{phrLoadDatabaseString}}.
##' @docType data
##' @references \url{http://pubs.usgs.gov/tm/06/a43/pdf/tm6-A43.pdf}
##' @usage llnl.dat  # phrLoadDatabaseString(llnl.dat)
##' @keywords dataset 
NULL



##' @name minteq.dat
##' @title The minteq.dat database.
##' @description minteq.dat is a database derived from the databases for the
##' program MINTEQA2. The database has been reformatted for use by
##' \code{\link{phrLoadDatabaseString}}.
##' @docType data
##' @references \url{http://pubs.usgs.gov/tm/06/a43/pdf/tm6-A43.pdf}
##' @source \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @usage minteq.dat  # phrLoadDatabaseString(minteq.dat)
##' @keywords dataset 
NULL



##' @name minteq.v4.dat
##' @title The minteq.v4.dat database.
##' @description minteq.v4.dat is a database derived from MINTEQA2 version 4.
##' The database has been reformatted for use by
##' \code{\link{phrLoadDatabaseString}}.
##' @docType data
##' @references \url{http://pubs.usgs.gov/tm/06/a43/pdf/tm6-A43.pdf}
##' @source \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @usage minteq.v4.dat  # phrLoadDatabaseString(minteq.v4.dat)
##' @keywords dataset 
NULL



##' @name pitzer.dat
##' @title The pitzer.dat database.
##' @description pitzer.dat is a database for the specific-ion-interaction model
##' of Pitzer as implemented in PHRQPITZ. The database has been reformatted for
##' use by \code{\link{phrLoadDatabaseString}}.
##' @docType data
##' @references \url{http://pubs.usgs.gov/tm/06/a43/pdf/tm6-A43.pdf}
##' @source \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @usage pitzer  # phrLoadDatabaseString(pitzer.dat)
##' @keywords dataset 
NULL



##' @name sit.dat
##' @title The sit.dat database.
##' @description sit.dat is a database derived from databases for EQ3/6 and
##' Geochemist's Workbench that uses thermodynamic data compiled by the
##' Lawrence Livermore National Laboratory. The database has been reformatted
##' for use by \code{\link{phrLoadDatabaseString}}.
##' @docType data
##' @references \url{http://pubs.usgs.gov/tm/06/a43/pdf/tm6-A43.pdf}
##' @source \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @usage sit.dat  # phrLoadDatabaseString(sit.dat)
##' @keywords dataset 
NULL



##' @name iso.dat
##' @title The iso.dat database.
##' @description iso.dat is a partial implementation of the individual component
##' approach to isotope calculations as described by Thorstenson and Parkhurst.
##' The database has been reformatted for use by
##' \code{\link{phrLoadDatabaseString}}.
##' @docType data
##' @references \url{http://pubs.usgs.gov/tm/06/a43/pdf/tm6-A43.pdf}
##' @source \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @usage iso.dat  # phrLoadDatabaseString(iso.dat)
##' @keywords dataset 
NULL



##' @name ex1
##' @title Example 1--Speciation Calculation
##' @description This example calculates the distribution of aqueous species in
##' seawater and the saturation state of seawater relative to a set of minerals.
##' To demonstrate how to expand the model to new elements, uranium is added to
##' the aqueous model defined by \code{\link{phreeqc.dat}}.  The example can be
##' run using the \code{\link{phrRunString}} routine.
##' @docType data
##' @references \url{http://pubs.usgs.gov/tm/06/a43/pdf/tm6-A43.pdf}
##' @source \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
##' @keywords dataset 
##' @examples
##' 
##' phrLoadDatabaseString(phreeqc.dat)
##' phrSetOutputStringsOn(TRUE)
##' phrRunString(ex1)
##' phrGetOutputStrings()
##' 
NULL

