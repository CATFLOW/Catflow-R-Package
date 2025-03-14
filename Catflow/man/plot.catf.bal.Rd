\name{plot.catf.bal}
\alias{plot.catf.bal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a CATFLOW simulation balance
}
\description{
Plots the water balance of a CATFLOW simulation, and calculates the surface and total mass balances.  
}
\usage{
plot.catf.bal(bilanz, ylim, unit = "m3", on.top = FALSE, stay = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bilanz}{
Data frame with balance file results from CATFLOW, or filename (and path) of a balance file 
}
  \item{ylim}{
Limits for the y-axis (optional)
}  

  \item{unit}{
Units for the water balance. Defaults to "m3" (cubic metre). Other option: unit ="mm" gives the balance in mm, calculated from the precipitation info in the Catflow results 
}
\item{on.top}{
Logical to indicate if the plotting window should be brought to the front
(NOTE: setting this to "TRUE" might not work in RStudio)
}
  \item{stay}{
Logical to indicate if the graphics window should stay at the front (optional)
}
  \item{\dots}{
Arguments to be passed to plot methods, such as graphical parameters (see \code{par}).  
For example, \code{main = "Title"} to specify a title for the plot.
}
}
\details{
The function \code{plot.catf.bal} is mainly used for its side-effect of plotting a
CATFLOW balance file. The input may be given either as a data frame, e.g., from 
\code{read.catf.balance}, or a character string with the filename of a CATFLOW balance file.

The columns selected for plotting are: Precipitation, surface runoff, 
right and lower boundary fluxes, soil moisture storage, interception, and sinks (evapotranspiration).


If assigned, a data frame with simulation time and the surface and total mass balances is returned. The surface balance is calculated as:

\eqn{Surf.bal = - Precipitation + Infiltration + Surface Runoff + Interception 
}  
   
All variables are positive; the surface balance is thus positive when the output > input.

 
The internal balance that is reported in the Catflow output can be calculated as:

\eqn{Intern.bal = - Infiltration + Soil Storage Change - Sinks - Lower Boundary Flux - Right Boundary Flux
}

Infiltration is positive, while sinks and boundary fluxes are negative, Soil storage change can be either. The internal balance is positive when the output > input. 
 The total mass balance is the sum of surface and internal mass balance. A positive value indicates a surplus of water in the model.
}


\value{
A data frame with simulation time, surface and total mass balance in chosen units.
}

\references{
Maurer, T. and E. Zehe (2007) \emph{CATFLOW User Guide and Program Documentation (Version Catstat)}
}

\author{
Jan \enc{Wienhöfer}{Wienhoefer}
}

\seealso{
\code{\link{read.catf.balance}}
}

\examples{
 \dontrun{
 balancefile <- system.file("Catflow-TEST/out/bilanz.csv", package = "Catflow")

balance <- plot.catf.bal(balancefile, unit="mm") 
}
}

\keyword{utilities}

