\name{phrAccumulateLine}
\alias{phrAccumulateLine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Accumlulate line(s) for input to phreeqc. 
}
\description{
Appends a line of text to the input buffer in order to be run using \code{\link{phrRunAccumulated}}.
}
\usage{
phrAccumulateLine(line)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{line}{
The line(s) to add for input to phreeqc.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns NULL.
}
\references{
    \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
}
\author{
    David Parkhurst \email{dlpark@usgs.gov}\cr
    Maintainer: Scott Charlton \email{charlton@usgs.gov}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{phrClearAccumulatedLines}}, \code{\link{phrGetAccumulatedLines}},  \code{\link{phrRunAccumulated}}
}
\examples{
# this example loads the phreeqc.dat database, accumulates input, and
# runs it
phrLoadDatabase("phreeqc/phreeqc.dat")
phrAccumulateLine("TITLE Example 2.--Temperature dependence of solubility")
phrAccumulateLine("                  of gypsum and anhydrite")
phrAccumulateLine("SOLUTION 1 Pure water")
phrAccumulateLine("        pH      7.0")
phrAccumulateLine("        temp    25.0")
phrAccumulateLine("EQUILIBRIUM_PHASES 1")
phrAccumulateLine("        Gypsum          0.0     1.0")
phrAccumulateLine("        Anhydrite       0.0     1.0")
phrAccumulateLine("REACTION_TEMPERATURE 1")
phrAccumulateLine("        25.0 75.0 in 51 steps")
phrAccumulateLine("SELECTED_OUTPUT")
phrAccumulateLine("        -file   ex2.sel")
phrAccumulateLine("        -temperature")
phrAccumulateLine("        -si     anhydrite  gypsum")
phrAccumulateLine("END")
phrSetOutputFileOn(TRUE)
if (is.null(phrRunAccumulated())) {
  cat(paste("see ", phrGetOutputFileName(), ".\n", sep=""))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
