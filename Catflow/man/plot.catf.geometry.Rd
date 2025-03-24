
\name{plot.catf.geometry}
\alias{plot.catf.geometry}
\title{Plot the geometry of a catflow object}
\usage{
plot.catf.geometry}(geom, zooming = TRUE, ...)
}
\arguments{
\item{geom}{A catflow geometry object as returned by \code{\link{make.geometry}}}

\item{zooming}{Logical. If TRUE, the plot is interactive and zooming is enabled. If FALSE, the plot is static.}

\item{...}{Additional arguments passed to \code{\link{matplot}} and \code{\link{matlines}}}
}
\description{
Plot the geometry of a catflow object
}
\value{
No value is returned.
}

\author{
Jan \enc{Wienhöfer}{Wienhoefer}
}
\examples{
\dontrun{        
 geom <- make.geometry(make.output = FALSE)     
 plot.catf.geometry(geom) 
}
}
