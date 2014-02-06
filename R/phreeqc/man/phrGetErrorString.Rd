\name{phrGetErrorString}
\alias{phrGetErrorString}
\title{
    Return error string messages.
}
\description{

    phrGetErrorString executes a phreeqc run
}
\synopsis{
    phrGetErrorString()
}
\usage{
    phrGetErrorString()
}
\arguments{
} %END arguments
\details{

    Any details about the operation of this function should go here.
} %END details
\value{
    This function returns the errors that occured during the previous
    phrReadDB, phrRun, phrRunFile call.
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
    \code{\link{phrRunFile}},
    \code{\link{phrGetSelectedOutput}}
}

\examples{
    # loaddatabase should fail
    n <- try(phrLoadDatabase("missing.dat"), silent = TRUE)
    # if n is non-NULL display error string
    if (!is.null(n)) phrGetErrorString()
}
\keyword{interface}
