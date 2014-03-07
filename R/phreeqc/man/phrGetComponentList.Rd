\name{phrGetComponentList}
\alias{phrGetComponentList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieves a list containing the current list of components.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
phrGetComponentList()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list containing the names of the components defined in the current system.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# This example runs the ex2 input file and echos the list of components.
phrLoadDatabaseString(phreeqc.dat)
phrRunString(ex2)
cat("components:\n")
for (c in phrGetComponentList()) {
  cat(c, "\n")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
