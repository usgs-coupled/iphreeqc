\name{phrRunFile}
\alias{phrRunFile}
\title{
    Run phreeqc input file
}
\description{

    phrRunFile executes a phreeqc run
}
\synopsis{
    phrRunFile(filename)
}
\usage{
    phrRunFile(filename)
}
\arguments{
\item{filename}{
    The file name of the phreeqc input file.
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

    Lopaka(Rob) Lee <rclee@usgs.gov>
} 
\seealso{
    \code{\link{phrReadString}},
    \code{\link{phrRun}},
    \code{\link{phrGetSelectedOutput}}
}

\examples{

    # This will only work if ex1 is in the current directory
    phrRunFile("phreeqc/ex1")
}
\keyword{interface}
