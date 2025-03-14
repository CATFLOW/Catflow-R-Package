\name{get.realworld.coords}
\alias{get.realworld.coords}

\title{
Compute real-world coordinates of computational nodes.
}
\description{
Compute real-world coordinates (eastings and northings) for the nodes in a CATFLOW
simulation grid, which is read from a geometry file.
}
\usage{
get.realworld.coords(geofile, tol = 1e-3)
}

\arguments{
  \item{geofile}{
Name of the CATFLOW geofile
}
  \item{tol}{
Tolerance for agreement of local coordinates and (transformed) real-world coordinates [m].
}
}
\details{
An error occurs if the calculated real-world coordinates, transformed back to local
coordinates, do not match the original local coordinates within the specified tolerance
(default: 1e-3 m).
}
\value{A list of the real-world coordinates of the computational grid with two components:
 \item{Re}{matrix of real-world x-coordinates (easting)}
  \item{Ho}{matrix of real-world y-coordinate (northing)}
}

\author{
Jan \enc{Wienhöfer}{Wienhoefer}
}

\seealso{
\code{\link{make.geometry}}, \code{\link{read.geofile}}
}
\examples{
  # example slope
  geof <- system.file("Catflow-TEST/in/test.geo", package = "Catflow")
  
  # Read the geo file and get real world coordinates
   test.rw.coords <- get.realworld.coords(geof)   
  # Plot profile line (map view)
    matplot(test.rw.coords$Re, test.rw.coords$Ho, 
            type="p", 
            pch="+", 
            col = 1,
            xlab="easting", 
            ylab="northing", 
            main="Profile line ('simple slope')")
}

\keyword{ utilities }

