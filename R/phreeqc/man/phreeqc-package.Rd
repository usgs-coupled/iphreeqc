\name{phreeqc-package}
\alias{phreeqc-package}
\alias{phreeqc}
\docType{package}
\title{
R interface to the phreeqc geochemical modeling program.
}
\description{
Provides an interface to PHREEQC (Version 3)--A Computer Program for Speciation, Batch-Reaction, One-Dimensional Transport, and Inverse Geochemical Calculations.
}
\details{
\tabular{ll}{
Package: \tab phreeqc\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-02-05\cr
License: \tab What license is it under?\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
    David Parkhurst \email{dlpark@usgs.gov}\cr
    Maintainer: Scott Charlton \email{charlton@usgs.gov}
}
\references{
    \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc/index.html}
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
\seealso{
}
\examples{
phrLoadDatabase("phreeqc/phreeqc.dat")
if (is.null(phrRunFile("phreeqc/ex2"))) {
  phrGetSelectedOutput()
}
}
