\name{read.geofile}
\alias{read.geofile}
\title{
Reads a CATFLOW geometry file
}
\description{
Reads a CATFLOW geometry from a text file, e.g. produced with 'make.geometry'.
}
\usage{
read.geofile(geofile)
}

\arguments{
  \item{geofile}{
Name of the CATFLOW geometry file
}
}


\value{A list with the following components:
 \item{hko}{matrix of local vertical coordinates of the computational grid}
 \item{sko}{matrix of local horizontal coordinates of the computational grid}
 \item{xsi}{lateral discretization of slope profile}
 \item{eta}{vertical discretization of slope profile}
 \item{slope.width}{slope width along lateral discretization}
 \item{realworld.coords}{real-world coordinates of surface nodes; a data frame with two columns:
                        \enumerate{
                          \item Re: real-world x coordinate (easting)
                          \item Ho: real-world y coordinate (northing)
                         }
                        }
}

\references{
Maurer, T. and E. Zehe (2007) \emph{CATFLOW User Guide and Program Documentation (Version Catstat)}
}
\author{
Jan \enc{Wienhöfer}{Wienhoefer}
}



\seealso{
 \code{\link{make.geometry}}  ,    \code{\link{plot.catf.grid}} 
}
\examples{
    # Read geo file of example slope
    test.geom <- read.geofile(system.file("Catflow-TEST/in/test.geo", package = "Catflow"))
    
  \dontrun{
    # Plot the geometry
    with(test.geom, plot(sko, hko, t="p"))
}      
}

\keyword{ utilities }
