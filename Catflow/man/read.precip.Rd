\name{read.precip}
\alias{read.precip}

\title{
Read a CATFLOW precipitation record
}
\description{
Reads a precipitation record in CATFLOW-specific format, and returns a data frame or a
\code{zoo} object with the record. Optionally, the precipitation record is plotted.
}
\usage{
read.precip(file.nam = "./in/Klima/P_DB_knick_04.dat", 
            GMT.off = -3600, timzon = "GMT", plotting = TRUE)
}

\arguments{
  \item{filename}{
Name (and path) of the precipitation file
}
  \item{GMT.off}{
Offset to convert time zone of precipitation record to reference time zone
(e.g., CET -> GMT: -3600)
}
  \item{timzon}{
Reference time zone (e.g., GMT)
}
  \item{plotting}{
Logical indicating if plotting should be done
}
}
\details{
To avoid possible problems with missing daylight saving time in the precipitation
record, the index vector of the resulting \code{zoo} object is converted to a reference time zone without daylight saving, e.g.,
'GMT' (=UTC), and shifted by an appropriate offset. 

For example, if the time specification in
the record originally is Central European Time (strictly without daylight saving time), the
time is offset by -3600 s and treated as UTC. 
 
}
\value{
A \code{zoo} object with the precipitation in original units and an index in POSIX
format (see details).
}
\references{
Maurer, T. and E. Zehe (2007) \emph{CATFLOW User Guide and Program Documentation (Version Catstat)}
}
\author{
Jan \enc{Wienhöfer}{Wienhoefer}
}


\seealso{
 \code{\link{write.precip}}
}
\examples{

 rainfile <- system.file("Catflow-TEST/in/TEST.rain.dat", package = "Catflow")
 rain <- read.precip(rainfile, plotting=T)
 }
}

\keyword{ utilities }

