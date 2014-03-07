\name{phrLoadDatabaseString}
\alias{phrLoadDatabaseString}
\title{Load a phreeqc database as a string.}
\description{
  Load the specified string as a database into phreeqc. Returns NULL if successful.
}
\usage{
  phrLoadDatabaseString(input)
}
\arguments{
  \item{input}{String containing data to be used as the phreeqc database.}
} %END arguments
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
  All previous definitions are cleared.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{phrLoadDatabase}}
}

\examples{
  # this example loads the phreeqc.dat database, turns on the
  # output file and runs ex2 as a string
  phrLoadDatabaseString(phreeqc.dat.string)
  phrSetOutputFileOn(TRUE)
  if (is.null(phrRunString(ex2))) {
    cat(paste("see ", phrGetOutputFileName(), ".\n", sep=""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
