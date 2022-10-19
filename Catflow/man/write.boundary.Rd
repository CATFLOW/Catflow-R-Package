\name{write.boundary}
\alias{write.boundary}

\title{
Write a file with time-variable boundary conditions for CATFLOW
}
\description{
Writes a file specifying time-variable boundary conditions input for a CATFLOW simulation.
}
\usage{
write.boundary(timeser, 
             irbtyp, 
             output.file, 
             start.time = "01.01.2004 00:00:00", 
             time.unit = "h")
}

\arguments{
  \item{timeser}{
Time-variable boundary conditions: either a \code{zoo} object with a POSIX index which holds the boundary condition values;
or a data frame with two columns: time since start date, and boundary condition values
}
  \item{irbtyp}{
ID for type of boundary condition
}
  \item{output.file}{
Name of file to produce
}
  \item{start.time}{
Start date for the simulation ("\%d.\%m.\%Y \%H:\%M:\%S")
}
  \item{time.unit}{
Time units of the time series 
}

}
\details{
The function 'write.boundary' is used for its side-effect: writing a input file with time-variable boundary conditions in CATFLOW-specific format.  
}
\value{
No value is returned. A short message indicates that a file has been produced.
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
 \dontrun{
 # some inflow data in m³/s
   inflow <- data.frame("hours" = seq(0,48, by=0.5),
                        "inflow" = c(rep(0,30), 1, rep(3,4), rep(2,3), 
                                      rep(0,25), rep(1,4), rep(0,30))*1e-3 ) 
                      
 write.boundary(inflow, "TEST.inflow.dat", irbtyp = 1,
                        start.time= "01.01.2004 00:00:00" )
 
 file.show("TEST.inflow.dat")
 
 ## maybe you like to delete the produced file
 unlink("TEST.inflow.dat")
 }       
}

\keyword{ utilities}
