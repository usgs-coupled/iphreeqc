\name{phrLoadDatabase}
\alias{phrLoadDatabase}
\title{
    Read phreeqc database file
}
\description{

    phrLoadDatabase loads a phreeqc database file into ...
}
\synopsis{
    phrLoadDatabase(filename)
}
\usage{
    phrLoadDatabase(filename)
}
\arguments{
\item{filename}{
    The file name of the database.
} 
} %END arguments
\details{

    Any details about the operation of this function should go here.

} %END details
\value{
    This function returns NULL.
}
\references{
    Literature references and web URLs can go here.
}
\author{
    Scott Charlton <charlton@usgs.gov>

} 
\seealso{
    \code{\link{phrReadString}},
    \code{\link{phrRun}},
    \code{\link{phrGetSelectedOutput}}
}

\examples{

    # This will only work if phreeqc.dat is in the current directory
    phrLoadDatabase("phreeqc/phreeqc.dat")
}
\keyword{interface}
