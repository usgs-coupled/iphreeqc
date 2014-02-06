\name{phrLoadDatabase}
\alias{phrLoadDatabase}
\title{Load a phreeqc database file}
\description{
  Loads the given phreeqc database into phreeqc.  Returns NULL if successful.
}
\usage{
  phrLoadDatabase(filename)
}
\arguments{
  \item{filename}{The file name of the database.} 
} %END arguments
\value{
    This function returns NULL.
}
\references{
    Literature references and web URLs can go here.
}
\author{
  Scott Charlton \email{charlton@usgs.gov}
} 
\seealso{
    \code{\link{phrLoadDatabaseString}}
}

\examples{

# This will only work if phreeqc/phreeqc.dat is in the current directory
if (is.null(phrLoadDatabase("phreeqc/phreeqc.dat"))) {
  cat("database ok\n")
} else {
  cat("database contains errors\n")
}

}
\keyword{interface}
