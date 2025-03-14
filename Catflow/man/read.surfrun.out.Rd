\name{read.surfrun.out}
\alias{read.surfrun.out}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read CATFLOW surface runoff output
}
\description{
Reads a surface runoff output file from a CATFLOW simulation.
}
\usage{
read.surfrun.out(filename,
                 GMT.off = -3600, timzon = "GMT", plotting = TRUE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Name (and path) of an surface runoff output file to read 
}
  \item{GMT.off}{
Offset to convert time zone of precipitation record to reference time zone GMT
(e.g., CET -> GMT: -3600)
}
  
  \item{plotting}{
Logical indicating if plotting should be done
}
}
\details{
To avoid possible problems with missing daylight saving time in the precipitation
record, the index vector of the resulting \code{zoo} object is converted to a reference time zone without daylight saving, namely,
'GMT' (=UTC), and shifted by an appropriate offset. 

For example, if the time specification in
the record originally is Central European Time (strictly without daylight saving time), the
time is offset by -3600 s and reported as GMT. 
}
\value{
A list with the following items:
 \item{orig }{A \code{zoo} series with simulated surface runoff in original format: Time in s, rain in m/s, runoff in m³/s and m³, respectively}
 \item{converted }{A \code{zoo} series with simulated surface runoff multiplied with the conversion factors specified in header: Time in d, rain in mm/h, runoff in mm/h and mm, respectively} 
}

\references{
Maurer, T. and E. Zehe (2007) \emph{CATFLOW User Guide and Program Documentation (Version Catstat)}
}
\author{
Jan \enc{Wienhöfer}{Wienhoefer}
}

\seealso{
\code{\link{read.catf.results}}
}

\examples{
ofa <- read.surfrun.out(system.file("Catflow-TEST/out/qoben.out", package = "Catflow"))
}

\keyword{utilities}

