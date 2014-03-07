\name{phrGetAccumulatedLines}
\alias{phrGetAccumulatedLines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the accumulated input string.
}
\description{
Returns the accumulated text in the input buffer of the phreeqc object.
}
\usage{
phrGetAccumulatedLines()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  The input as a single string.
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
  \code{\link{phrAccumulateLine}}, \code{\link{phrRunAccumulated}}
}
\examples{
# This example loads some keyword input and displays the contents.
phrAccumulateLine("SOLUTION 1")
phrAccumulateLine("END")
cat(paste("The accumulated input is:", phrGetAccumulatedLines(), sep="\n"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
