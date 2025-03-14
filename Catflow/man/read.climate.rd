\name{read.climate}
\alias{read.climate}

\title{
Read a CATFLOW climate record
}
\description{
Reads a climate record in CATFLOW-specific format, and returns a data frame or a \code{zoo}
object with the record. Optionally, the climate record is plotted.
}
\usage{
read.climate( file.nam, 
              GMT.off = -3600, timzon = "GMT", plotting = TRUE, ...)
}

\arguments{
  \item{file.nam}{
Name (and path) of the climate file
}
  \item{GMT.off}{
Offset to convert time zone of climate record to reference time zone (e.g., CET -> GMT: -3600)
}
  \item{timzon}{
Reference time zone (e.g., GMT)
}
  \item{plotting}{
Logical indicating if plotting should be done
}
  \item{\dots}{
Arguments to be passed to plot methods, such as graphical parameters (see \code{par})
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
A \code{zoo} object with the climate record and an index in POSIX format (see details).
The climate record holds:
 \tabular{ll}{
\code{GlobRad}: \tab Global radiation [W/m²] \cr                                   
  \code{NetRad}: \tab Net radiation [W/m²]  \cr  
  \code{Temp}: \tab Temperature [°C] \cr  
  \code{RelHum}: \tab Relative humidity [\%] \cr  
  \code{vWind}: \tab Wind velocity [m/s] \cr  
  \code{dirWind}: \tab Wind direction [°, clockwise from North]\cr
}
}
\references{
Maurer, T. and E. Zehe (2007) \emph{CATFLOW User Guide and Program Documentation (Version Catstat)}
}
\author{
Jan \enc{Wienhöfer}{Wienhoefer}
}

\seealso{
\code{\link{write.climate}}
}
\examples{

 # exmple climate record
climfile <- system.file("Catflow-TEST/in/TEST.clima.dat", package = "Catflow")              
 # ... and read it
 clima <- read.climate(climfile)
 
 \dontrun{  
 plot(clima)                                   
 } 
}

\keyword{ utilities }
