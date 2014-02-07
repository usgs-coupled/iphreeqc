\name{phrClearAccumulatedLines}
\alias{phrClearAccumulatedLines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clears the accumulated input buffer.
}
\description{
Clears the accumulated input buffer. The input buffer is accumulated from calls to the \code{\link{phrAccumulateLine}} method.
}
\usage{
phrClearAccumulatedLines()
}
%- maybe also 'usage' for other objects documented here.
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
\code{\link{phrAccumulateLine}}, \code{\link{phrGetAccumulatedLines}},  \code{\link{phrRunAccumulated}}
}
\examples{
# This example loads some keyword input, clears the input, and displays the results.
phrAccumulateLine("SOLUTION 1")
phrAccumulateLine("END")
cat(paste("The accumulated input is:\n", phrGetAccumulatedLines(), sep=""))
phrClearAccumulatedLines()
WScript.Echo("The accumulated input now is:\n" + phrLines)
cat(paste("The accumulated input now is:\n", phrGetAccumulatedLines(), sep=""))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
