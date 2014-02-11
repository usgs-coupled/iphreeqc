\name{phrGetDumpString}
\alias{phrGetDumpString}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieves the string buffer containing DUMP output.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
phrGetDumpString()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The dump output as a string.
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
\code{\link{phrGetDumpFileName}}, \code{\link{phrGetDumpFileOn}},  \code{\link{phrGetDumpStringOn}}, \code{\link{phrSetDumpFileName}}, \code{\link{phrSetDumpFileOn}}, \code{\link{phrSetDumpStringOn}}
}
\examples{

phrLoadDatabase("phreeqc/phreeqc.dat")
phrSetDumpStringOn(TRUE)

input <-              "SOLUTION 1 Pure water     \n"
input <- paste(input, "EQUILIBRIUM_PHASES 1      \n")
input <- paste(input, "    Calcite 0 10          \n")
input <- paste(input, "SAVE solution 1           \n")
input <- paste(input, "SAVE equilibrium_phases 1 \n")
input <- paste(input, "DUMP                      \n")
input <- paste(input, "    -solution 1           \n")
input <- paste(input, "    -equilibrium_phases  1\n")

if (!is.null(phrRunString(input))) {
    cat(phrGetErrorString())
}
cat(phrGetDumpString())

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
