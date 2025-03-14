\name{read.soil.mat}
\alias{read.soil.mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read soil ID assignment
}
\description{
Reads a file with the soil type identifiers for a CATFLOW simulation. This file is 
automatically produced within the simulation directory during each CATFLOW simulation.
}
\usage{
read.soil.mat(input.file = "boden.dat")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.file}{
Name (and path) of the file with the soil type ID's
}
}
\details{
The function \code{read.soil.mat} is designed for reading a soil type assignment
of a CATFLOW simulation. 
}
\value{
A matrix with soil type identifiers (and same dimensions as the simulation grid of the
CATFLOW run).
}
\references{
Maurer, T. and E. Zehe (2007) \emph{CATFLOW User Guide and Program Documentation (Version Catstat)}
}
\author{
Jan \enc{Wienhöfer}{Wienhoefer}
}
\seealso{
\code{\link{read.catf.results}} , \code{\link{plot.catf.grid}}
}

\examples{

 soilgrid <- read.soil.mat( system.file("Catflow-TEST/in/soils.bod", package = "Catflow"))
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}