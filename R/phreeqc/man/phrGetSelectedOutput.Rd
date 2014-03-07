\name{phrGetSelectedOutput}
\alias{phrGetSelectedOutput}
\title{
    Returns the contents of the selected output as a data frame.
}
\description{
    phrGetSelectedOutput return the results of a phreeqc run specified
    in the selected output block of a phreeqc input file.

}
\synopsis{
    phrGetSelectedOutput()
}
\usage{
    phrGetSelectedOutput()
}
\arguments{
} %END arguments
\details{
    Accessing selected outputs having a user number other than 1 requires
    setting the current selected output user number with \code{\link{phrSetCurrentSelectedOutputUserNumber}}.

    *** Should we return a list of data.frames??? ***
} %END details
\value{
    Returns a data frame containing the selected_output from the previous run.
}
\references{
    \url{http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phreeqc}
}
\author{
    David Parkhurst \email{dlpark@usgs.gov}\cr
    Maintainer: Scott Charlton \email{charlton@usgs.gov}
} 
\seealso{
    \code{\link{phrGetCurrentSelectedOutputUserNumber}},
    \code{\link{phrGetSelectedOutputFileOn}},
    \code{\link{phrSetCurrentSelectedOutputUserNumber}},
}
\examples{

    # Load database and run ex2
    phrLoadDatabaseString(phreeqc.dat)
    phrRunString(ex2)
    
    # display a summary of the results
    df <- phrGetSelectedOutput()
    summary(df)
}
\keyword{interface}
